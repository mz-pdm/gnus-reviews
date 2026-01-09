;;; gnus-reviews.el --- Email-based code review management for Gnus  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Red Hat, Inc.

;; Author: Claude Code <noreply@anthropic.com>
;; Author: Milan Zamazal <mzamazal@redhat.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (gnus "5.13"))
;; Keywords: mail, gnus, code-review, development
;; URL: https://github.com/mz-pdm/gnus-reviews
;; Homepage: https://github.com/mz-pdm/gnus-reviews

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides tools for managing email-based code reviews in Gnus.

;;; Code:

(require 'gnus)
(require 'gnus-sum)
(require 'gnus-art)
(require 'gnus-score)
(require 'gnus-group)
(require 'message)
(require 'cl-lib)
(require 'org)
(require 'org-id)

;;; Constants and Variables

(defgroup gnus-reviews nil
  "Email-based code review management for Gnus."
  :group 'gnus
  :prefix "gnus-reviews-")

(defcustom gnus-reviews-base-group "nnml:reviews"
  "Base group name for review-related messages."
  :type 'string
  :group 'gnus-reviews)

(defcustom gnus-reviews-own-patches-group (concat gnus-reviews-base-group ".patches")
  "Group for reviews of your own patches."
  :type 'string
  :group 'gnus-reviews)

(defcustom gnus-reviews-to-review-group (concat gnus-reviews-base-group ".reviews")
  "Group for patches you intend to review."
  :type 'string
  :group 'gnus-reviews)

(defcustom gnus-reviews-watching-group (concat gnus-reviews-base-group ".watching")
  "Group for patches you want to watch without reviewing."
  :type 'string
  :group 'gnus-reviews)

(defcustom gnus-reviews-score-increase 1000
  "Score increase for review-relevant articles."
  :type 'integer
  :group 'gnus-reviews)

(defcustom gnus-reviews-org-file "~/gnus-reviews.org"
  "Org file to store review data in hierarchical format.
The file is organized hierarchically by patch series, versions,
and individual patches with comments stored as Org blocks."
  :type 'file
  :group 'gnus-reviews)

(defcustom gnus-reviews-auto-create-groups t
  "Whether to automatically create review groups if they don't exist."
  :type 'boolean
  :group 'gnus-reviews)

(defcustom gnus-reviews-group-comments-by-article t
  "Whether to group series comments by article titles when displaying.
When non-nil, series comments are grouped by article with section headers.
When nil, comments are displayed in a flat list with article titles shown
above each comment."
  :type 'boolean
  :group 'gnus-reviews)

;;; Org File Management Functions

(defun gnus-reviews--ensure-org-file ()
  "Ensure the Org file exists and has basic structure."
  (unless (file-exists-p gnus-reviews-org-file)
    (with-temp-file gnus-reviews-org-file
      (insert "#+TITLE: Gnus Reviews Database\n")
      (insert "#+DESCRIPTION: Hierarchical review data for email-based code reviews\n")
      (insert "#+STARTUP: overview\n\n")
      (insert "This file is managed by gnus-reviews.el - you can edit it manually,\n")
      (insert "but be careful with the structure and properties.\n\n")))
  (unless (get-file-buffer gnus-reviews-org-file)
    (find-file-noselect gnus-reviews-org-file)))

(defmacro gnus-reviews--with-org-buffer (&rest body)
  "Execute BODY with the gnus-reviews Org file buffer current."
  `(progn
     (gnus-reviews--ensure-org-file)
     (with-current-buffer (find-file-noselect gnus-reviews-org-file)
       (org-mode)
       ,@body)))

(defun gnus-reviews--find-org-node-by-id (custom-id)
  "Find Org node with CUSTOM-ID in the reviews file.
Returns the position of the heading or nil if not found."
  (gnus-reviews--with-org-buffer
    (save-excursion
      (ignore-errors
        (org-link-search (concat "#" custom-id))
        (org-back-to-heading t)
        (point)))))

(defun gnus-reviews--create-series-node (series-subject)
  "Create a new series node for SERIES-SUBJECT.
Returns the position of the created series heading."
  (gnus-reviews--with-org-buffer
    (let* ((clean-subject (replace-regexp-in-string "[^a-zA-Z0-9-]" "_" series-subject))
           (series-id (format "series-%s" clean-subject)))
      ;; Check if series already exists
      (if-let ((existing-pos (gnus-reviews--find-org-node-by-id series-id)))
          existing-pos
        ;; Create new series node at end of file
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "\n* %s\n" series-subject))
        (insert "  :PROPERTIES:\n")
        (insert (format "  :CUSTOM_ID: %s\n" series-id))
        (insert "  :STATUS: active\n")
        (insert "  :END:\n")
        (save-buffer)
        (org-back-to-heading t)
        (point)))))

(defun gnus-reviews--add-comment-to-patch (patch-pos comment-data)
  "Add a comment under patch at PATCH-POS.
Returns the position after the comment."
  (gnus-reviews--with-org-buffer
    (goto-char patch-pos)
    (org-end-of-subtree t nil)
    (let ((status (plist-get comment-data :status))
          (content (plist-get comment-data :content))
          (context (plist-get comment-data :context))
          (author-name (plist-get comment-data :author-name))
          (review-email-id (plist-get comment-data :review-email-id))
          (review-newsgroup-name (plist-get comment-data :review-newsgroup-name)))
      ;; Add context as quote block if present
      (when context
        (insert "\n#+BEGIN_QUOTE\n")
        (insert context)
        (insert "\n#+END_QUOTE\n"))
      ;; Add comment with status and author indicators
      (insert (format "\n#+STATUS: %s\n" (symbol-name status)))
      ;; Add author information with link to review email
      (when (and author-name review-email-id)
        (let ((author-link (if review-newsgroup-name
                               (format "[[gnus:%s#%s][%s]]" review-newsgroup-name review-email-id author-name)
                             author-name)))
          (insert (format "#+AUTHOR: %s\n" author-link))))
      (insert (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE\n"
                      (or content "")))
      (save-buffer)
      (point))))

(defun gnus-reviews--create-version-node (series-pos version thread-id)
  "Create a version node under series at SERIES-POS.
Returns the position of the version heading."
  (gnus-reviews--with-org-buffer
    (goto-char series-pos)
    (let* ((version-title (format "v%s" version))
           (version-id (format "version-%s-%s" thread-id version)))
      ;; Look for existing version under this series
      (if-let ((existing-pos (save-excursion
                               (let ((series-end (save-excursion (org-end-of-subtree t t) (point))))
                                 (when (re-search-forward
                                        (format "^\\*\\* %s$" (regexp-quote version-title))
                                        series-end t)
                                   (org-back-to-heading t)
                                   (point))))))
          existing-pos
        ;; Create new version node
        (org-end-of-subtree t nil)
        (insert (format "\n** %s\n" version-title))
        (insert "   :PROPERTIES:\n")
        (insert (format "   :CUSTOM_ID: %s\n" version-id))
        (insert (format "   :VERSION_NUMBER: %s\n" version))
        (insert "   :END:\n")
        (save-buffer)
        (org-back-to-heading t)
        (point)))))

(defun gnus-reviews--create-gnus-link (article-id)
  (if gnus-newsgroup-name
      (format "[[gnus:%s#%s][View in Gnus]]" gnus-newsgroup-name article-id)
    (error "No newsgroup active")))

(defun gnus-reviews--create-patch-node (version-pos article-id article-title)
  "Create a patch node under version at VERSION-POS.
Returns the position of the patch heading."
  (gnus-reviews--with-org-buffer
    (goto-char version-pos)
    (let ((gnus-link (gnus-reviews--create-gnus-link article-id)))
      ;; Create patch node under version
      (org-end-of-subtree t nil)
      (insert (format "\n*** %s\n" article-title))
      (insert "    :PROPERTIES:\n")
      (insert (format "    :CUSTOM_ID: %s\n" article-id))
      (insert (format "    :ARTICLE_ID: %s\n" article-id))
      (insert (format "    :ARTICLE_TITLE: %s\n" article-title))
      (when gnus-link
        (insert (format "    :GNUS_LINK: %s\n" gnus-link)))
      (insert "    :END:\n")
      (save-buffer)
      (org-back-to-heading t)
      (point))))

(defun gnus-reviews--store-comment-in-org (patch-email-id comment-data)
  (gnus-reviews--with-org-buffer
    (let ((patch-pos (gnus-reviews--find-org-node-by-id patch-email-id)))
      (unless patch-pos
        (let* ((thread-id (plist-get comment-data :thread-id))
               (series-info (gnus-reviews-extract-patch-info-from thread-id))
               (patch-info (gnus-reviews-extract-patch-info-from patch-email-id))
               (patch-title (if patch-info
                                (let ((series-num (plist-get patch-info :series-num))
                                      (series-total (plist-get patch-info :series-total))
                                      (clean-subject (plist-get patch-info :subject)))
                                  (if (and series-num series-total (> series-total 1))
                                      (format "[#%d/%d] %s" series-num series-total clean-subject)
                                    clean-subject))
                              "Unknown Patch"))
               (series-subject (or (plist-get series-info :subject) patch-title))
               (version (or (plist-get patch-info :version) "1"))
               (series-pos (gnus-reviews--create-series-node series-subject))
               (version-pos (gnus-reviews--create-version-node series-pos version thread-id)))
          (setq patch-pos (gnus-reviews--create-patch-node version-pos patch-email-id patch-title))))
      (gnus-reviews--add-comment-to-patch patch-pos comment-data))))

;;; Group Management Functions

(defun gnus-reviews--group-exists-p (group)
  "Check if GROUP exists in Gnus."
  (and group (gnus-group-entry group)))

(defun gnus-reviews--create-group (group)
  "Create GROUP if it doesn't exist and auto-creation is enabled."
  (when (and gnus-reviews-auto-create-groups
             group
             (not (gnus-reviews--group-exists-p group)))
    (gnus-group-make-group (gnus-group-short-name group) "nnml" "")
    (message "Created review group: %s" group)))

(defun gnus-reviews--ensure-groups ()
  "Ensure all review groups exist, creating them if necessary."
  (unless (and (boundp 'gnus-group-buffer)
               gnus-group-buffer
               (get-buffer gnus-group-buffer))
    (error "Gnus group buffer is not available - ensure Gnus is running"))
  (with-current-buffer gnus-group-buffer
    (mapc #'gnus-reviews--create-group
          (list gnus-reviews-base-group
                gnus-reviews-own-patches-group
                gnus-reviews-to-review-group
                gnus-reviews-watching-group))))

;;; Article and thread utilities

(defmacro gnus-reviews--in-summary-buffer (&rest body)
  `(if (buffer-live-p gnus-summary-buffer)
       (with-current-buffer gnus-summary-buffer
         ,@body)
     (error "No Gnus summary buffer")))

(defmacro gnus-reviews--with-article (message-id &rest body)
  (let (($current-id (gensym)))
    `(gnus-reviews--in-summary-buffer
       (let ((,$current-id (gnus-reviews--current-article-id)))
         (when (and ,$current-id
                    (gnus-summary-refer-article ,message-id))
           (prog1 (progn ,@body)
             (gnus-summary-refer-article ,$current-id)))))))

(defun gnus-reviews--article-header (func field)
  (cond
   ;; Try to get from article buffer context first (most reliable)
   ((and (boundp 'gnus-current-headers) gnus-current-headers)
    (funcall func gnus-current-headers))
   ;; Try getting from article buffer if we're in one
   ((gnus-buffer-live-p gnus-article-buffer)
    (with-current-buffer gnus-article-buffer
      (when (and (boundp 'gnus-current-headers) gnus-current-headers)
        (funcall func gnus-current-headers))))
   ;; Fall back to summary buffer approach
   ((and (gnus-summary-article-number)
         (gnus-summary-article-header))
    (funcall func (gnus-summary-article-header)))
   ;; Last resort: try to extract from raw article headers
   (t
    (gnus-with-article-headers
      (gnus-fetch-field field)))))

(defun gnus-reviews--current-article-id ()
  (gnus-reviews--article-header #'mail-header-id "Message-ID"))

(defun gnus-reviews--current-thread-id ()
  (or (car (gnus-reviews--current-article-references))
      (gnus-reviews--current-article-id)))

(defun gnus-reviews--current-article-references ()
  (mapcar (lambda (s)
            (replace-regexp-in-string "^<\\|>$" "" s))
          (split-string (or (gnus-reviews--article-header #'mail-header-references "References")
                            "")
                        "[ \t\n]+" t)))

(defun gnus-reviews--find-patch-email-id ()
  (if (gnus-reviews-is-patch-email-p)
      (gnus-reviews--current-article-id)
    (cl-find-if (lambda (message-id)
                  (gnus-reviews--with-article message-id
                    (gnus-reviews-is-patch-email-p)))
                (nreverse (gnus-reviews--current-article-references)))))

(defun gnus-reviews--current-article-title ()
  "Get the title/subject of the current article.
Strips reply prefixes (Re:, Fwd:) and patch type prefixes ([PATCH], [RFC])
while preserving series information (e.g., 1/3) from any email that has it."
  (gnus-with-article-buffer
    (let ((subject (gnus-fetch-field "Subject")))
      (when subject
        (setq subject (string-trim subject))
        ;; First, extract any patch series information that might be present
        (let ((series-info nil)
              (clean-subject subject))
          ;; Look for series numbers in various formats
          (cond
           ;; Format: [PATCH v2 1/3] or [RFC PATCH v1 2/5] etc.
           ((string-match "\\[\\(?:PATCH\\|RFC\\)\\(?:[^]]*?\\)\\s-+\\([0-9]+\\)/\\([0-9]+\\)\\]\\s-*\\(.*\\)" subject)
            (setq series-info (format "[#%s/%s]" (match-string 1 subject) (match-string 2 subject)))
            (setq clean-subject (match-string 3 subject)))
           ;; Format: Re: [PATCH v2 1/3] (reply to patch with series info)
           ((string-match "\\(?:Re:\\s-*\\|Fwd:\\s-*\\)+.*?\\[\\(?:PATCH\\|RFC\\)\\(?:[^]]*?\\)\\s-+\\([0-9]+\\)/\\([0-9]+\\)\\]\\s-*\\(.*\\)" subject)
            (setq series-info (format "[#%s/%s]" (match-string 1 subject) (match-string 2 subject)))
            (setq clean-subject (match-string 3 subject))))

          ;; Remove reply prefixes
          (setq clean-subject
                (replace-regexp-in-string "^\\(Re:\\s-*\\|Fwd:\\s-*\\)+" "" clean-subject))

          ;; Remove any remaining patch/RFC prefixes
          (setq clean-subject
                (replace-regexp-in-string "^\\s-*\\[\\(PATCH\\|RFC\\)[^]]*\\]\\s-*" "" clean-subject))

          ;; Trim and combine series info with clean subject
          (setq clean-subject (string-trim clean-subject))
          (if series-info
              (format "%s %s" series-info clean-subject)
            clean-subject))))))

;;; Message Classification

(defcustom gnus-reviews-patch-patterns
  '("^\\[PATCH[^]]*\\]" "^\\[RFC[^]]*\\]" "^diff --git" "^---.*\\+\\+\\+" "^Index: ")
  "Patterns that indicate a message contains a patch."
  :type '(repeat string)
  :group 'gnus-reviews)

(defcustom gnus-reviews-review-patterns
  '("^Re:.*\\[PATCH" "^Re:.*\\[RFC" "Reviewed-by:" "Acked-by:" "Tested-by:"
    "^On .* wrote:" "> .*" "inline comment")
  "Patterns that indicate a message is a review comment."
  :type '(repeat string)
  :group 'gnus-reviews)

(defun gnus-reviews--match-patterns (content patterns)
  "Check if CONTENT matches any of the PATTERNS."
  (cl-some (lambda (pattern)
             (string-match-p pattern content))
           patterns))

(defun gnus-reviews-is-patch-email-p ()
  "Return non-nil if current article is a patch email."
  (gnus-with-article-buffer
    (let ((subject (gnus-fetch-field "Subject"))
          (content (buffer-string)))
      (or (and subject (gnus-reviews--match-patterns subject gnus-reviews-patch-patterns))
          (gnus-reviews--match-patterns content gnus-reviews-patch-patterns)))))

(defun gnus-reviews-is-own-patch-email-p ()
  "Return non-nil if current article is a patch by the user."
  (gnus-with-article-buffer
    (when-let ((from (gnus-fetch-field "From")))
      (or (and user-mail-address (string-match (regexp-quote user-mail-address) from))
          (and user-full-name (string-match (regexp-quote user-full-name) from)))
      (gnus-reviews-is-patch-email-p))))

(defun gnus-reviews-is-review-email-p ()
  "Return non-nil if current article is a review email."
  (gnus-with-article-buffer
    (let ((subject (gnus-fetch-field "Subject"))
          (content (buffer-string))
          (in-reply-to (gnus-fetch-field "In-Reply-To")))
      (and subject
           (not (gnus-reviews-is-patch-email-p))
           (or in-reply-to (string-match "^Re:" subject))
           (or (gnus-reviews--match-patterns subject gnus-reviews-review-patterns)
               (gnus-reviews--match-patterns content gnus-reviews-review-patterns))))))

(defun gnus-reviews-extract-patch-info ()
  "Extract patch information from the current article.
Returns a plist with :series-num, :series-total, :version, :subject."
  (gnus-with-article-buffer
    (let ((subject (gnus-fetch-field "Subject")))
      (when subject
        (cond
         ;; PATCH series with version and series numbers
         ((string-match (rx "["
                            "PATCH"
                            (optional (seq (+ whitespace) "v" (group (+ digit))))
                            (+ whitespace)
                            (group (+ digit))
                            "/"
                            (group (+ digit))
                            "]"
                            (* whitespace)
                            (group (* anything)))
                        subject)
          (list :version (match-string 1 subject)
                :series-num (string-to-number (match-string 2 subject))
                :series-total (string-to-number (match-string 3 subject))
                :subject (string-trim (match-string 4 subject))))
         ;; RFC series with version and series numbers
         ((string-match (rx "["
                            "RFC"
                            (optional (seq (+ whitespace) "PATCH"))
                            (optional (seq (+ whitespace) "v" (group (+ digit))))
                            (+ whitespace)
                            (group (+ digit))
                            "/"
                            (group (+ digit))
                            "]"
                            (* whitespace)
                            (group (* anything)))
                        subject)
          (list :version (match-string 1 subject)
                :series-num (string-to-number (match-string 2 subject))
                :series-total (string-to-number (match-string 3 subject))
                :subject (string-trim (match-string 4 subject))
                :rfc t))
         ;; Single PATCH with version
         ((string-match (rx "["
                            "PATCH"
                            (optional (seq (+ whitespace) "v" (group (+ digit))))
                            "]"
                            (* whitespace)
                            (group (* anything)))
                        subject)
          (list :version (match-string 1 subject)
                :series-num 1
                :series-total 1
                :subject (string-trim (match-string 2 subject))))
         ;; Single RFC with version
         ((string-match (rx "["
                            "RFC"
                            (optional (seq (+ whitespace) "PATCH"))
                            (optional (seq (+ whitespace) "v" (group (+ digit))))
                            "]"
                            (* whitespace)
                            (group (* anything)))
                        subject)
          (list :version (match-string 1 subject)
                :series-num 1
                :series-total 1
                :subject (string-trim (match-string 2 subject))
                :rfc t)))))))

(defun gnus-reviews-extract-patch-info-from (message-id)
  (gnus-reviews--with-article message-id
    (gnus-reviews-extract-patch-info)))

;;; Comment Tracking System

(defun gnus-reviews--generate-comment-id (article-id comment-order)
  "Generate a deterministic unique comment ID.
ARTICLE-ID identifies the article, COMMENT-ORDER is the sequential order
of this comment within the article (1-based)."
  (format "%s#%d" article-id comment-order))

(defun gnus-reviews--parse-individual-comments ()
  "Parse individual review comments from current article.
Returns a list of (content start-pos end-pos context) for each comment."
  (gnus-with-article-buffer
    (let ((content (buffer-string))
          (comments '())
          (body-start 0))
      ;; Skip headers
      (when (string-match "\n\n" content)
        (setq body-start (match-end 0)))

      (with-temp-buffer
        (insert (substring content body-start))
        (goto-char (point-min))

        (let ((current-context nil)
              (comment-lines '())
              (comment-start-pos nil))

          (while (not (eobp))
            (cond
             ;; Found quoted line - save any accumulated comment block first
             ((looking-at "^> \\(.+\\)$")
              (when comment-lines
                (let ((comment-text (string-join (nreverse comment-lines) "\n")))
                  (when (and (> (length comment-text) 0)
                             (string-match "\\w" comment-text))
                    (push (list comment-text
                                (+ body-start comment-start-pos)
                                (+ body-start (line-end-position 0))
                                current-context)
                          comments)))
                (setq comment-lines nil
                      comment-start-pos nil))
              ;; Update context for future comments
              (setq current-context (match-string-no-properties 1)))

             ;; Found non-quoted, non-empty line
             ((looking-at "^\\([^>\n].*\\)$")
              (let ((line-text (save-match-data (string-trim (match-string-no-properties 1)))))
                ;; Exclude signature lines, headers, email reply introductions, and greetings
                (when (and (> (length line-text) 0)
                           (string-match "\\w" line-text)
                           (not (or
                                 ;; Email reply introductions
                                 (string-match-p "^On " line-text)
                                 (string-match-p " wrote:[ \t]*$" line-text)
                                 (string-match-p " writes:[ \t]*$" line-text)
                                 ;; Signature lines
                                 (string-match-p "^--" line-text)
                                 (string-match-p "^___" line-text)
                                 ;; Headers
                                 (string-match-p "^\\(From\\|Subject\\|Date\\|To\\|Cc\\):" line-text)
                                 ;; Common greetings and closings (case insensitive)
                                 (string-match-p "^[ \t]*\\(hi\\|hello\\|hey\\|dear\\)\\($\\|[ \t,]\\)"
                                                 (downcase line-text))
                                 (string-match-p "^[ \t]*\\(regards\\|best\\|thanks\\|thank you\\|cheers\\|sincerely\\|yours\\)\\( regards\\| wishes\\)?[ \t,]*$"
                                                 (downcase line-text)))))
                  (when (null comment-start-pos)
                    (setq comment-start-pos (line-beginning-position)))
                  (push line-text comment-lines))))

             ;; Empty line or other - save accumulated comment block if any
             (t
              (when comment-lines
                (let ((comment-text (string-join (nreverse comment-lines) "\n")))
                  (when (and (> (length comment-text) 0)
                             (string-match "\\w" comment-text))
                    (push (list comment-text
                                (+ body-start comment-start-pos)
                                (+ body-start (line-end-position 0))
                                current-context)
                          comments)))
                (setq comment-lines nil
                      comment-start-pos nil))))

            (forward-line 1))

          ;; Handle any remaining comment block at end of buffer
          (when comment-lines
            (let ((comment-text (string-join (nreverse comment-lines) "\n")))
              (when (and (> (length comment-text) 0)
                         (string-match "\\w" comment-text))
                (push (list comment-text
                            (+ body-start comment-start-pos)
                            (+ body-start (point-max))
                            current-context)
                      comments))))))

      (nreverse comments))))

(defun gnus-reviews-track-individual-comment (comment-text status comment-order
                                                       &optional context author-name author-email review-newsgroup-name)
  "Track an individual review comment.
COMMENT-TEXT is the actual comment content.
STATUS should be one of: `pending', `addressed', `dismissed'.
COMMENT-ORDER is the sequential order of this comment within the
article (1-based).
CONTEXT is optional code context the comment refers to.
AUTHOR-NAME, AUTHOR-EMAIL, REVIEW-NEWSGROUP-NAME are author information from the review email."
  (let* ((review-email-id (gnus-reviews--current-article-id))
         ;; Note: Context information should be passed in from caller to avoid
         ;; issues when processing multiple comments from the same email
         (patch-email-id (gnus-reviews--find-patch-email-id))
         (thread-id (gnus-reviews--current-thread-id))
         (comment-id (gnus-reviews--generate-comment-id patch-email-id comment-order))
         (comment-data (list :status status
                             :content comment-text
                             :thread-id thread-id
                             :timestamp (current-time)
                             :context context
                             :author-name author-name
                             :author-email author-email
                             :review-email-id review-email-id
                             :review-newsgroup-name review-newsgroup-name)))
    (unless review-email-id
      (error "No review email ID available - ensure there is a Gnus article buffer"))
    (unless patch-email-id
      (error "Could not find patch email ID for this review"))
    ;; Store comment in Org file using patch email ID as the article node
    (gnus-reviews--store-comment-in-org patch-email-id comment-data)
    comment-id))

(defun gnus-reviews--get-status-choices (comment-order)
  "Get available status choices for a comment based on its order.
COMMENT-ORDER is the sequential position of the comment (1-based).
Returns a list of status strings, including `merge' only if comment-order > 1."
  (let ((base-choices '("pending" "addressed" "dismissed" "skip")))
    (if (> comment-order 1)
        (append '("merge") base-choices)
      base-choices)))

;;; Core Functions

(defun gnus-reviews-classify-message ()
  "Classify the current message and return its type.
Returns one of: `own-patch', `review-comment', `patch', `other'."
  (cond
   ((and (gnus-reviews-is-patch-email-p) (gnus-reviews-is-own-patch-email-p))
    'own-patch)
   ((gnus-reviews-is-review-email-p)
    'review-comment)
   ((gnus-reviews-is-patch-email-p)
    'patch)
   (t 'other)))

;;; Public Interface

(defun gnus-reviews--collect-thread-articles ()
  "Collect all article numbers in the current thread including siblings.
Returns a list of article numbers in the complete thread."
  (let ((thread-articles '())
        (current-article (gnus-summary-article-number)))
    (when current-article
      (save-excursion
        ;; Move to the thread root to ensure we get the whole thread
        (gnus-summary-refer-thread)
        (gnus-summary-top-thread)
        ;; Collect all articles in the entire thread (including siblings)
        (let ((thread-root (gnus-summary-article-number))
              (visited (make-hash-table :test 'equal)))
          ;; Add the root article
          (push thread-root thread-articles)
          (puthash thread-root t visited)
          ;; Navigate through the entire thread structure
          (gnus-summary-goto-article thread-root)
          (let ((start-pos (point)))
            ;; Move to next thread to find the boundary
            (if (= (gnus-summary-next-thread 1) 0)
                (let ((end-pos (line-end-position 0)))
                  ;; Go back to start and collect all articles until next thread
                  (goto-char start-pos)
                  (while (< (point) end-pos)
                    (when-let ((article-num (gnus-summary-article-number)))
                      (unless (gethash article-num visited)
                        (push article-num thread-articles)
                        (puthash article-num t visited)))
                    (forward-line 1)))
              ;; If no next thread, go to end of buffer
              (goto-char start-pos)
              (while (not (eobp))
                (when-let ((article-num (gnus-summary-article-number)))
                  (unless (gethash article-num visited)
                    (push article-num thread-articles)
                    (puthash article-num t visited)))
                (forward-line 1)))))))
    (nreverse thread-articles)))

(defun gnus-reviews--copy-and-tick-articles (articles target-group)
  "Copy and tick ARTICLES to TARGET-GROUP.
Returns the number of articles successfully copied."
  (let ((copied-count 0))
    (when articles
      (dolist (article-num articles)
        (when (gnus-summary-goto-article article-num)
          ;; Tick the article before copying to preserve the tick status
          (gnus-summary-mark-article nil gnus-ticked-mark)
          (gnus-summary-copy-article nil target-group)
          (cl-incf copied-count))))
    copied-count))

(defun gnus-reviews--tick-processed-articles ()
  "Tick all articles in current buffer that have process marks (#).
Process marks indicate articles marked for batch processing."
  (let ((process-articles (gnus-summary-work-articles nil))
        (ticked-count 0)
        (current-article (gnus-summary-article-number)))
    (when process-articles
      (dolist (article-num process-articles)
        (when (gnus-summary-goto-article article-num)
          ;; Tick the article with process mark
          (gnus-summary-mark-article article-num gnus-ticked-mark)
          (cl-incf ticked-count)))
      ;; Return to original article
      (when current-article
        (gnus-summary-goto-article current-article))
      (when (> ticked-count 0)
        (message "Ticked %d articles with process marks" ticked-count)))))

(defun gnus-reviews--boost-thread-score (thread-root-article)
  "Boost score for the entire thread starting from THREAD-ROOT-ARTICLE."
  (save-excursion
    (gnus-summary-goto-article thread-root-article)
    (gnus-reviews-increase-score)))

(defun gnus-reviews--process-thread-action (target-group message-format)
  "Perform a standard action on the current thread.
This involves ensuring groups exist, collecting thread articles,
copying them to TARGET-GROUP, boosting the thread score,
and displaying a formatted message."
  (interactive)
  (gnus-reviews--ensure-groups)
  (let* ((current-article (gnus-summary-article-number))
         (thread-articles (gnus-reviews--collect-thread-articles))
         (thread-root (car thread-articles)))
    (when thread-articles
      (let ((copied-count (gnus-reviews--copy-and-tick-articles thread-articles target-group)))
        ;; Boost score for thread and return to original article
        (when thread-root
          (gnus-reviews--boost-thread-score thread-root))
        (when current-article
          (gnus-summary-goto-article current-article))
        (message message-format copied-count target-group)))))

;;;###autoload
(defun gnus-reviews-add-reviewed-by-tag ()
  "Insert a Reviewed-by tag with user's name and email at point."
  (interactive)
  (if (and user-full-name user-mail-address)
      (insert (format "Reviewed-by: %s <%s>\n" user-full-name user-mail-address))
    (error "User name or email not configured.")))

;;;###autoload
(defun gnus-reviews-copy-to-group (&optional group)
  "Copy current article to GROUP.
When called interactively, automatically suggests an appropriate group
based on message classification but always asks for confirmation.

If called with a prefix argument, tick the copied article first and if there
are articles with processed marks, tick them all."
  (interactive
   (let* ((type (gnus-reviews-classify-message))
          (default-group (pcase type
                          ('own-patch gnus-reviews-own-patches-group)
                          ('review-comment gnus-reviews-to-review-group)
                          ('patch gnus-reviews-to-review-group)
                          (_ gnus-reviews-to-review-group)))
          (all-groups (list gnus-reviews-own-patches-group
                            gnus-reviews-to-review-group
                            gnus-reviews-watching-group)))
     (list (completing-read
            (format "Copy to group (default %s): " default-group)
            all-groups nil t nil nil default-group))))
  (gnus-reviews--ensure-groups)

  ;; Handle prefix argument: tick current article and processed articles
  (when current-prefix-arg
    (gnus-summary-mark-article nil gnus-ticked-mark)
    (gnus-reviews--tick-processed-articles))

  (gnus-summary-copy-article nil group)
  (message "Copied article to %s%s"
           group
           (if current-prefix-arg " (ticked)" "")))

;;;###autoload
(defun gnus-reviews-watch-thread ()
  "Watch the current thread by copying all thread articles to watching group.
Also increases the score for the thread to boost visibility."
  (interactive)
  (gnus-reviews--process-thread-action
   gnus-reviews-watching-group
   "Watched thread: copied %d articles to %s"))

(defun gnus-reviews--process-patch-review-helper (target-group)
  "Helper function to process a patch review and copy to TARGET-GROUP.
Extracts and tracks individual comments, ticks the article if there are
pending comments, and copies it to the specified target group for follow-up."
  (unless (gnus-reviews-is-review-email-p)
    (error "Current article is not a review email"))
  (gnus-reviews--ensure-groups)
  ;; Extract and track comments from the review
  (gnus-reviews-extract-and-track-comments)
  ;; Increase score for the subthread to boost visibility
  (gnus-reviews-increase-score)
  ;; Check for pending comments and tick only if found
  (let* ((article-id (gnus-reviews--current-article-id))
         (tracked-comments (gnus-reviews-get-comments-for-article article-id))
         (pending-count (cl-count-if (lambda (comment)
                                      (eq (plist-get (cdr comment) :status) 'pending))
                                    tracked-comments))
         (total-count (length tracked-comments)))
    ;; Tick the article only if there are pending comments
    (when (> pending-count 0)
      (gnus-summary-mark-article nil gnus-ticked-mark))
    ;; Copy to target group
    (gnus-summary-copy-article nil target-group)
    ;; Show feedback about what was processed
    (cond
     ((> pending-count 0)
      (message "Processed patch review: %d pending comments (of %d total), ticked and copied to %s"
               pending-count total-count target-group))
     ((> total-count 0)
      (message "Processed patch review: %d comments tracked (none pending), copied to %s"
               total-count target-group))
     (t
      (message "Processed patch review: no comments found, copied to %s"
               target-group)))))

;;;###autoload
(defun gnus-reviews-process-my-patch-review ()
  "Process a review of your own patch.
Extracts and tracks individual comments, ticks the article if there are
pending comments, and copies it to the own patches group for follow-up."
  (interactive)
  (gnus-reviews--process-patch-review-helper gnus-reviews-own-patches-group))

;;;###autoload
(defun gnus-reviews-process-others-patch-review ()
  "Process a review of someone else's patch.
Extracts and tracks individual comments, ticks the article if there are
pending comments, and copies it to the reviews group for follow-up."
  (interactive)
  (gnus-reviews--process-patch-review-helper gnus-reviews-to-review-group))

;;;###autoload
(defun gnus-reviews-copy-my-patch-series ()
  "Copy the entire current patch series to own patches group.
Ticks all articles in the series, copies them to own patches group,
and increases score for better visibility. Use this when you want to
track your own patch series without processing review comments."
  (interactive)
  (gnus-reviews--process-thread-action
   gnus-reviews-own-patches-group
   "Copied patch series: %d articles ticked and copied to %s"))

;;;###autoload
(defun gnus-reviews-queue-series-for-review ()
  "Queue the entire current patch series for review.
Ticks all articles in the series, copies them to review group,
and increases score for better visibility. Use this when you want to
review someone else's patch series."
  (interactive)
  (gnus-reviews--process-thread-action
   gnus-reviews-to-review-group
   "Queued patch series for review: %d articles ticked and copied to %s"))

;;;###autoload
(defun gnus-reviews-increase-score ()
  "Increase score for the current review-related article subthread and subject.
Temporarily boosts the score of all articles in the subthread starting from
the current article and all articles with the same core subject
(prefixes stripped)."
  (interactive)
  (when (or (gnus-reviews-is-patch-email-p)
            (gnus-reviews-is-review-email-p))
    (let ((article-id (gnus-reviews--current-article-id))
          (subject (gnus-with-article-buffer (gnus-fetch-field "Subject")))
          (score gnus-reviews-score-increase)
          (parts '()))
      ;; Score the subthread starting with current article
      (when article-id
        (gnus-summary-score-entry "thread" article-id 's score (current-time-string))
        (push "subthread" parts))
      ;; Score by cleaned subject (strip common prefixes)
      (when subject
        (let ((clean-subject (replace-regexp-in-string
                              "^\\(\\(Re: \\|Fwd: \\)*\\[\\(PATCH\\|RFC\\)[^]]*\\]\\s-*\\|\\(Re: \\|Fwd: \\)+\\)"
                              "" subject)))
          (when (> (length clean-subject) 0)
            (gnus-summary-score-entry "subject" clean-subject 's score (current-time-string))
            (push (format "subject '%s'" clean-subject) parts))))
      ;; Refresh the summary and show single message
      (gnus-summary-rescore)
      (when parts
        (message "Boosted %s score by %d" (string-join (nreverse parts) " and ") score)))))

;;;###autoload
(defun gnus-reviews-extract-and-track-comments ()
  "Extract individual comments from current article and assign status to each."
  (interactive)
  (when (gnus-reviews-is-review-email-p)
    (let* (;; Extract author information FIRST before any context changes
           (author-from (gnus-reviews--article-header #'mail-header-from "From"))
           (author-name (when author-from
                          (if (string-match "^\\([^<]+\\)\\s-*<" author-from)
                              (string-trim (match-string 1 author-from))
                            (string-trim author-from))))
           (author-email (when author-from
                           (if (string-match "<\\([^>]+\\)>" author-from)
                               (match-string 1 author-from)
                             author-from)))
           (review-newsgroup-name (and (boundp 'gnus-newsgroup-name) gnus-newsgroup-name))
           ;; Now extract other information
           (comments (gnus-reviews--parse-individual-comments))
           (tracked-count 0)
           (article-id (gnus-reviews--current-article-id))
           (existing-comments (gnus-reviews-get-comments-for-article article-id)))
      (if comments
          (progn
            (message "Found %d individual comments to process..." (length comments))
            (let ((comment-order 1))
              (dolist (comment comments)
                (let* ((text (nth 0 comment))
                       (context (nth 3 comment))
                       (display-text (if context
                                         (format "Context: %s\nComment: %s"
                                                 context
                                                 (substring text 0 (min 100 (length text))))
                                       (substring text 0 (min 100 (length text)))))
                       ;; Check if this comment already exists in the database
                       (existing-comment (cl-find-if
                                          (lambda (c) (string= (plist-get (cdr c) :content) text))
                                          existing-comments))
                       (existing-status (when existing-comment
                                          (plist-get (cdr existing-comment) :status)))
                       (default-status (when existing-status
                                         (symbol-name existing-status)))
                       ;; Get dynamic status choices based on comment order
                       (status-choices (gnus-reviews--get-status-choices comment-order))
                       (prompt-text (if existing-status
                                        (format "Status for comment [EXISTING: %s]: %s\n> "
                                                existing-status display-text)
                                      (format "Status for comment: %s\n> " display-text)))
                       (status (completing-read prompt-text status-choices nil t nil nil default-status)))
                  (cond
                   ((string= status "skip")
                    ;; Skip this comment entirely
                    nil)
                   ((string= status "merge")
                    ;; Merge with preceding comment
                    (if (> comment-order 1)
                        (let* ((preceding-comment-id (gnus-reviews--generate-comment-id article-id (1- comment-order)))
                               (preceding-comment (cl-find-if
                                                   (lambda (c) (string= (car c) preceding-comment-id))
                                                   existing-comments)))
                          (if preceding-comment
                              (let* ((preceding-content (plist-get (cdr preceding-comment) :content))
                                     (merged-content (concat preceding-content "\n\n" text)))
                                (gnus-reviews-update-comment-content article-id preceding-comment-id merged-content)
                                (message "Merged comment with preceding comment %s" preceding-comment-id)
                                (cl-incf tracked-count))
                            (message "No preceding comment found to merge with, tracking as new comment")
                            (gnus-reviews-track-individual-comment text 'pending comment-order context author-name author-email review-newsgroup-name)
                            (cl-incf tracked-count)))
                      (message "No preceding comment to merge with (this is the first comment), tracking as new comment")
                      (gnus-reviews-track-individual-comment text 'pending comment-order context author-name author-email review-newsgroup-name)
                      (cl-incf tracked-count)))
                   (existing-comment
                    ;; Update existing comment status if it changed
                    (let ((new-status (intern status)))
                      (unless (eq existing-status new-status)
                        (gnus-reviews-update-comment-status article-id (car existing-comment) new-status)
                        (cl-incf tracked-count))))
                   (t
                    ;; Track new comment with specified status
                    (gnus-reviews-track-individual-comment text (intern status) comment-order context author-name author-email review-newsgroup-name)
                    (cl-incf tracked-count)))
                  (cl-incf comment-order))))
            (gnus-reviews-increase-score)
            (message "Tracked %d individual comments" tracked-count))
        (message "No individual comments found in this article")))))

;;;###autoload
(defun gnus-reviews-mark-region-as-comment (start end status)
  "Mark the selected region as an individual comment.
START and END define the region.
STATUS should be one of: pending, addressed, dismissed."
  (interactive (list (region-beginning)
                     (region-end)
                     (completing-read "Comment status: "
                                      '("pending" "addressed" "dismissed")
                                      nil t)))
  (let* ((status-symbol (intern status))
         (comment-text (buffer-substring-no-properties start end))
         (all-comments (gnus-reviews--parse-individual-comments))
         (comment-order (1+ (cl-position-if (lambda (comment)
                                              (>= start (nth 1 comment)))
                                            all-comments
                                            :from-end t)))
         (comment-id (gnus-reviews-track-individual-comment
                      comment-text status-symbol comment-order nil)))
    (message "Tracked comment %s as %s: %s"
             comment-id status
             (substring comment-text 0 (min 50 (length comment-text))))))

;; Provide the package
(provide 'gnus-reviews)

;;; gnus-reviews.el ends here
