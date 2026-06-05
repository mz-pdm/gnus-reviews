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

(defcustom gnus-reviews-auto-create-groups t
  "Whether to automatically create review groups if they don't exist."
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

(defcustom gnus-reviews-greeting-template "Hi %s,\n\nthank you for the patch.\n\n"
  "Template for greeting message when replying to patches.
%s will be replaced with the recipient's first name."
  :type 'string
  :group 'gnus-reviews)

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
(defun gnus-reviews-insert-reviewed-by ()
  "Insert a Reviewed-by tag with user's name and email at point."
  (interactive)
  (if (and user-full-name user-mail-address)
      (insert (format "Reviewed-by: %s <%s>\n" user-full-name user-mail-address))
    (error "User name or email not configured.")))

;;;###autoload
(defun gnus-reviews-copy (&optional group)
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
(defun gnus-reviews-watch ()
  "Watch the current thread by copying all thread articles to watching group.
Also increases the score for the thread to boost visibility."
  (interactive)
  (gnus-reviews--process-thread-action
   gnus-reviews-watching-group
   "Watched thread: copied %d articles to %s"))

(defun gnus-reviews--process-patch-review-helper (target-group)
  "Helper function to process a patch review and copy to TARGET-GROUP.
Increase score and copy the article to the specified target group for
follow-up."
  (unless (gnus-reviews-is-review-email-p)
    (error "Current article is not a review email"))
  (gnus-reviews--ensure-groups)
  (unless (string= gnus-newsgroup-name target-group)
    (gnus-reviews-increase-score)
    (gnus-summary-copy-article nil target-group)))

;;;###autoload
(defun gnus-reviews-process-own-review ()
  "Process a review of your own patch.
Increases score and copies the article to the own patches group for follow-up."
  (interactive)
  (gnus-reviews--process-patch-review-helper gnus-reviews-own-patches-group))

;;;###autoload
(defun gnus-reviews-process-review ()
  "Process a review of someone else's patch.
Increases score and copies the article to the reviews group for follow-up."
  (interactive)
  (gnus-reviews--process-patch-review-helper gnus-reviews-to-review-group))

;;;###autoload
(defun gnus-reviews-store-own-patches ()
  "Copy the entire current patch series to own patches group.
Ticks all articles in the series, copies them to own patches group,
and increases score for better visibility. Use this when you want to
track your own patch series without processing review comments."
  (interactive)
  (gnus-reviews--process-thread-action
   gnus-reviews-own-patches-group
   "Copied patch series: %d articles ticked and copied to %s"))

;;;###autoload
(defun gnus-reviews-store-for-review ()
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
(defun gnus-reviews-insert-greeting ()
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
