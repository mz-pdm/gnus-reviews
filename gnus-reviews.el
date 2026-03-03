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

(defcustom gnus-reviews-comment-exclusion-patterns
  '(;; Email reply introductions
    "^On " " wrote:[ \t]*$" " writes:[ \t]*$"
    ;; Signature lines
    "^--" "^___"
    ;; Common greetings and closings
    "^[ \t]*\\([Hh]i\\|[Hh]ello\\|[Hh]ey\\|[Dd]ear\\)\\($\\|[ \t,]\\)"
    "^[ \t]*\\([Rr]egards\\|[Bb]est\\|[Tt]hanks\\|[Tt]hank you\\|[Cc]heers\\|[Ss]incerely\\|[Yy]ours\\)\\( regards\\| wishes\\)?[ \t,]*$")
  "Patterns for lines to exclude when parsing individual comments.
Lines matching any of these patterns will not be considered as review comments.
All patterns are matched case-sensitively."
  :type '(repeat string)
  :group 'gnus-reviews)

(defcustom gnus-reviews-greeting-template "Hi %s,\n\nthank you for the patch.\n\n"
  "Template for greeting message when replying to patches.
%s will be replaced with the recipient's first name."
  :type 'string
  :group 'gnus-reviews)

;;; Org File Management Functions

(defun gnus-reviews--ensure-org-file ()
  "Ensure the Org file exists and has basic structure."
  (unless (file-exists-p gnus-reviews-org-file)
    (with-temp-file gnus-reviews-org-file
      (insert "#+TITLE: Gnus Reviews Database\n")
      (insert "#+DESCRIPTION: Hierarchical review data for email-based code reviews\n")
      (insert "#+TODO: PENDING | DONE REJECTED\n")
      (insert "#+STARTUP: overview\n\n")
      (insert "This file is managed by gnus-reviews.el - you can edit it manually,\n")
      (insert "but be careful with the structure and properties.\n\n")))
  (unless (get-file-buffer gnus-reviews-org-file)
    (find-file-noselect gnus-reviews-org-file)))

(defmacro gnus-reviews--with-org-buffer (&rest body)
  "Execute BODY with the gnus-reviews Org file buffer current."
  (declare (indent 0))
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

(defun gnus-reviews--insert-on-new-line (text)
  "Insert TEXT, ensuring it starts on a new line."
  (unless (bolp) (insert "\n"))
  (insert text))

(defun gnus-reviews--insert-properties-block (properties)
  (insert "  :PROPERTIES:\n")
  (dolist (p properties)
    (cl-destructuring-bind (prop . val) p
      (when val
        (insert (format "  :%s: %s\n" prop val)))))
  (insert "  :END:\n"))

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
        (insert (format "\n* PENDING %s\n" series-subject))
        (gnus-reviews--insert-properties-block `(("CUSTOM_ID" . ,series-id)
                                                 ("STATUS" . "active")))
        (save-buffer)
        (org-back-to-heading t)
        (point)))))

(defun gnus-reviews--add-comment-to-patch (patch-pos comment-data)
  (gnus-reviews--with-org-buffer
    (goto-char patch-pos)
    (org-end-of-subtree t nil)
    (let* ((status (plist-get comment-data :status))
           (content (or (plist-get comment-data :content) ""))
           (content-start (substring content 0 (min 40 (length content))))
           (content-intro (car (split-string content-start "\n")))
           (context (plist-get comment-data :context))
           (author (or (plist-get comment-data :author-name)
                       (plist-get comment-data :author-email)))
           (review-email-id (plist-get comment-data :review-email-id)))
      (gnus-reviews--insert-on-new-line (format "**** %s %s\n" status content-intro))
      (when review-email-id
        (gnus-reviews--insert-properties-block
         `(("AUTHOR" . ,(gnus-reviews--gnus-link review-email-id author)))))
      (when context
        (insert "\n#+BEGIN_QUOTE\n")
        (dolist (line (split-string context "\n"))
          (insert "> " line "\n"))
        (insert "#+END_QUOTE\n"))
      ;; Add author information with link to review email
      (insert "\n#+BEGIN_RESPONSE\n")
      (dolist (line (split-string content "\n"))
        (insert "  " line "\n"))
      (insert "#+END_RESPONSE\n"))
    (save-buffer)
    (point)))

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
                                        (format "^\\*\\*\\( [A-Z]+\\)? +%s$" (regexp-quote version-title))
                                        series-end t)
                                   (org-back-to-heading t)
                                   (point))))))
          existing-pos
        ;; Create new version node
        (org-end-of-subtree t nil)
        (gnus-reviews--insert-on-new-line (format "** PENDING %s\n" version-title))
        (gnus-reviews--insert-properties-block `(("CUSTOM_ID" . ,version-id)
                                                 ("VERSION_NUMBER" . ,version)))
        (save-buffer)
        (org-back-to-heading t)
        (point)))))

(defun gnus-reviews--gnus-link (article-id title)
  (if gnus-newsgroup-name
      (format "[[gnus:%s#%s][%s]]" gnus-newsgroup-name article-id (or title "View in Gnus"))
    (error "No newsgroup active")))

(defun gnus-reviews--create-patch-node (version-pos article-id article-title)
  "Create a patch node under version at VERSION-POS.
Returns the position of the patch heading."
  (gnus-reviews--with-org-buffer
    (goto-char version-pos)
    (let ((gnus-link (gnus-reviews--gnus-link article-id "Patch e-mail")))
      ;; Create patch node under version
      (org-end-of-subtree t nil)
      (gnus-reviews--insert-on-new-line (format "*** PENDING %s\n" article-title))
      (gnus-reviews--insert-properties-block `(("CUSTOM_ID" . ,article-id)
                                               ("ARTICLE_TITLE" . ,article-title)
                                               ("GNUS_LINK" . ,gnus-link)))
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
  (declare (indent 0))
  `(if (buffer-live-p gnus-summary-buffer)
       (with-current-buffer gnus-summary-buffer
         ,@body)
     (error "No Gnus summary buffer")))

(defmacro gnus-reviews--with-article (message-id &rest body)
  (declare (indent 1))
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
                ;; Exclude lines matching configured exclusion patterns
                (when (and (> (length line-text) 0)
                           (string-match "\\w" line-text)
                           (not (gnus-reviews--match-patterns line-text gnus-reviews-comment-exclusion-patterns)))
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

(defun gnus-reviews-track-individual-comment (comment-text status context author-name author-email)
  "Track an individual review comment.
COMMENT-TEXT is the actual comment content.
STATUS should be one of: \"PENDING\", \"REJECTED\", \"DONE\".
CONTEXT is optional code context the comment refers to.
AUTHOR-NAME and AUTHOR-EMAIL are author information from the review email."
  (let* ((review-email-id (gnus-reviews--current-article-id))
         ;; Note: Context information should be passed in from caller to avoid
         ;; issues when processing multiple comments from the same email
         (patch-email-id (gnus-reviews--find-patch-email-id))
         (thread-id (gnus-reviews--current-thread-id))
         (comment-data (list :status status
                             :content comment-text
                             :thread-id thread-id
                             :timestamp (current-time)
                             :context context
                             :author-name author-name
                             :author-email author-email
                             :review-email-id review-email-id)))
    (unless review-email-id
      (error "No review email ID available - ensure there is a Gnus article buffer"))
    (unless patch-email-id
      (error "Could not find patch email ID for this review"))
    ;; Store comment in Org file using patch email ID as the article node
    (gnus-reviews--store-comment-in-org patch-email-id comment-data)))

(defun gnus-reviews--find-patch-comment-sections (patch-email-id)
  "Find all comment sections for a given patch.
Returns a list of (section-title section-position) pairs."
  (gnus-reviews--with-org-buffer
   (let ((patch-pos (gnus-reviews--find-org-node-by-id patch-email-id))
         (comment-sections '()))
     (when patch-pos
       (goto-char patch-pos)
       (org-end-of-subtree t nil)
       (let ((patch-end (point)))
         (goto-char patch-pos)
         (while (re-search-forward "^\\*\\*\\*\\*\\s-+\\(\\S-+\\)\\s-+\\(.+\\)" patch-end t)
           (let* ((status (match-string 1))
                  (comment-intro (match-string 2))
                  (section-title (format "%s: %s" status comment-intro))
                  (section-pos (match-beginning 0)))
             (push (list section-title section-pos) comment-sections)))))
     (nreverse comment-sections))))

(defun gnus-reviews--select-comment-section (patch-email-id)
  "Let user select a comment section for rebuttal.
Returns the position of the selected section, or nil if cancelled."
  (let ((sections (gnus-reviews--find-patch-comment-sections patch-email-id)))
    (if sections
        (let* ((section-titles (mapcar #'car sections))
               (selected-title (completing-read "Select comment section for rebuttal: "
                                                section-titles nil t)))
          (cadr (assoc selected-title sections)))
      (error "No existing comment sections found for this patch"))))

(defun gnus-reviews--add-rebuttal-to-section (section-pos rebuttal-text author-name)
  "Add a rebuttal comment to an existing comment section."
  (gnus-reviews--with-org-buffer
   (goto-char section-pos)
   ;; Move to end of current line to get past the **** heading
   (end-of-line)
   ;; Find the end of this comment section (before next **** or end of subtree)
   (let ((section-end (save-excursion
                        (if (re-search-forward "^\\*\\*\\*\\*" nil t)
                            (match-beginning 0)
                          (org-end-of-subtree t nil)
                          (point)))))
     (goto-char section-end)
     (gnus-reviews--insert-on-new-line (format "Rebuttal by %s:\n" author-name))
     (insert "#+BEGIN_REBUTTAL\n")
     (dolist (line (split-string rebuttal-text "\n"))
       (insert "  " line "\n"))
     (insert "#+END_REBUTTAL\n")
     (save-buffer))))

(defun gnus-reviews--get-status-choices (initial)
  (let ((base-choices '("PENDING" "REJECTED" "DONE" "skip")))
    (when (gnus-reviews--find-patch-comment-sections
           (gnus-reviews--find-patch-email-id))
      (setq base-choices (append base-choices '("rebuttal"))))
    (if initial
        base-choices
      (cons "merge" base-choices))))

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
Extract and track individual comments, tick the article if there are
pending comments, and copy it to the specified target group for follow-up."
  (unless (gnus-reviews-is-review-email-p)
    (error "Current article is not a review email"))
  (gnus-reviews--ensure-groups)
  (when (> (gnus-reviews-extract-and-track-comments) 0)
    (gnus-summary-mark-article nil gnus-ticked-mark))
  (unless (string= gnus-newsgroup-name target-group)
    (gnus-reviews-increase-score)
    (gnus-summary-copy-article nil target-group)))

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

(defun gnus-reviews--author ()
  (let* ((author-from (gnus-reviews--article-header #'mail-header-from "From"))
         (author-name (when author-from
                        (string-trim
                         (if (string-match "^\\([^<]+\\)\\s-*<" author-from)
                             (match-string 1 author-from)
                           author-from))))
         (author-email (when author-from
                         (if (string-match "<\\([^>]+\\)>" author-from)
                             (match-string 1 author-from)
                           author-from))))
    (cons author-name author-email)))

(defun gnus-reviews--extract-first-name (header)
  "Extract the first name from a mail HEADER like To or From.
Handles formats like \"First Last <email@domain>\" or \"email@domain\"."
  (when header
    (let ((name-part
           (cond
            ;; Format: "Full Name <email@domain>"
            ((string-match "^\\s-*\\([^<]+\\)\\s-*<" header)
             (string-trim (match-string 1 header)))
            ;; Format: just "email@domain", use part before @
            ((string-match "^\\s-*\\([^@]+\\)" header)
             (string-trim (match-string 1 header)))
            ;; Fallback
            (t (string-trim header)))))
      ;; Extract first word as first name
      (when (and name-part (> (length name-part) 0))
        (car (split-string name-part "\\s-+"))))))

;;;###autoload
(defun gnus-reviews-extract-and-track-comments ()
  "Extract individual comments from current article and assign status to each."
  (interactive)
  (unless (gnus-reviews-is-review-email-p)
    (error "Not a review e-mail"))
  (let ((comments (gnus-reviews--parse-individual-comments))
        (n-pending-comments 0))
    (if comments
        (progn
          (let ((preceding-comment nil)
                (processed-comments '()))
            (dolist (comment comments)
              (let* ((text (nth 0 comment))
                     (start-pos (nth 1 comment))
                     (end-pos (nth 2 comment))
                     (context (nth 3 comment))
                     (display-text (if context
                                       (format "Context: %s\nComment: %s"
                                               context
                                               (substring text 0 (min 100 (length text))))
                                     (substring text 0 (min 100 (length text)))))
                     (status-choices (gnus-reviews--get-status-choices (not preceding-comment)))
                     (prompt-text (format "Status for comment \"%s\": " display-text)))
                ;; Scroll article to show the comment location
                (when-let ((article-window (get-buffer-window gnus-article-buffer)))
                  (with-selected-window article-window
                    (goto-char start-pos)
                    (recenter)
                    ;; Highlight the comment region briefly
                    (when (fboundp 'pulse-momentary-highlight-region)
                      (pulse-momentary-highlight-region start-pos end-pos))))
                (let ((status (completing-read prompt-text status-choices nil t)))
                  (cond
                   ((string= status "skip")
                    nil)
                   ((string= status "merge")
                    (unless preceding-comment
                      (error "No preceding comment to merge with"))
                    (setcar preceding-comment (concat (car preceding-comment) "\n\n" text)))
                   ((string= status "rebuttal")
                    (let* ((patch-email-id (gnus-reviews--find-patch-email-id))
                           (section-pos (gnus-reviews--select-comment-section patch-email-id))
                           (processed (list text context status section-pos)))
                      (push processed processed-comments)
                      (setq preceding-comment processed)))
                   (t
                    (let ((processed (list text context status nil)))
                      (push processed processed-comments)
                      (setq preceding-comment processed)))))))
            (let* ((author (gnus-reviews--author))
                   (author-name (car author))
                   (author-email (cdr author)))
              (dolist (comment (nreverse processed-comments))
		(cl-destructuring-bind (text context status section-pos) comment
                  (if section-pos
                      ;; Handle rebuttal comment
                      (gnus-reviews--add-rebuttal-to-section section-pos text author-name)
                    ;; Handle regular comment
                    (gnus-reviews-track-individual-comment text status context author-name author-email)
                    (when (string= status "PENDING")
                      (cl-incf n-pending-comments)))))))
          (gnus-reviews-increase-score))
      (message "No comments found in this article"))
    n-pending-comments))

;;;###autoload
(defun gnus-reviews-mark-region-as-comment (start end status)
  "Mark the selected region as an individual comment.
START and END define the region.
STATUS should be one of: \"PENDING\", \"REJECTED\", \"DONE\"."
  (interactive (list (region-beginning)
                     (region-end)
                     (completing-read "Comment status: "
                                      '("PENDING" "REJECTED" "DONE")
                                      nil t)))
  (let* ((text (buffer-substring-no-properties start end))
         (author (gnus-reviews--author))
         (author-name (car author))
         (author-email (cdr author)))
    (gnus-reviews-track-individual-comment text status nil author-name author-email)))

;;;###autoload
(defun gnus-reviews-greet ()
  "Insert a greeting at the beginning of the mail being composed.
The greeting uses the first name of the recipient and follows
the template defined in `gnus-reviews-greeting-template'."
  (interactive)
  (unless (derived-mode-p 'message-mode)
    (error "Must be in a message composition buffer"))
  (let* ((to-header (message-fetch-field "To"))
         (first-name (gnus-reviews--extract-first-name to-header)))
    (unless first-name
      (error "Could not extract recipient name from To field"))
    (save-excursion
      ;; Go to the beginning of the message body (after headers)
      (message-goto-body)
      ;; Insert the greeting
      (insert (format gnus-reviews-greeting-template first-name)))))

;; Provide the package
(provide 'gnus-reviews)

;;; gnus-reviews.el ends here
