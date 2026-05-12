# Gnus Reviews

An Emacs Lisp package for managing email-based code reviews in Gnus.

This is an AI experiment, the project is of AI-quality.

## Overview

Gnus Reviews helps you organize and track email-based code review workflows. While designed with libcamera development in mind, it's generic and can be used with any project that uses email for code reviews.

## Installation

1. Place `gnus-reviews.el` in your Emacs load path
2. Add to your init file:
   ```elisp
   (require 'gnus-reviews)
   (gnus-reviews-initialize)
   ```

## Configuration

Customize the package with `M-x customize-group RET gnus-reviews RET` or add to your init file:

```elisp
;; Group names (adjust to your mail backend)
(setq gnus-reviews-own-patches-group "nnml:reviews.own-patches"
      gnus-reviews-to-review-group "nnml:reviews.to-review"
      gnus-reviews-watching-group "nnml:reviews.watching"
      gnus-reviews-finished-group "nnml:reviews.finished")

;; User identification for own patches
(setq gnus-reviews-user-email "your.email@example.com"
      gnus-reviews-user-name "Your Name")

;; Score increase for review articles
(setq gnus-reviews-score-increase 1000)
```

## Usage

### Copying Articles

- `M-x gnus-reviews-copy-to-group` - Copy current article to a review group (suggests group based on message type; prefix arg ticks the article)
- `M-x gnus-reviews-copy-own-series` - Copy entire current patch series to own-patches group and boost its score
- `M-x gnus-reviews-queue-series` - Copy entire current patch series to reviews group and boost its score
- `M-x gnus-reviews-watch-thread` - Copy entire current thread to watching group and boost its score

### Processing Reviews

- `M-x gnus-reviews-process-own-review` - Process a review comment on your own patch (boost score and copy to own-patches group)
- `M-x gnus-reviews-process-review` - Process a review comment on someone else's patch (boost score and copy to reviews group)

### Scoring

- `M-x gnus-reviews-increase-score` - Boost score for the current article's subthread and subject

### Composing

- `M-x gnus-reviews-insert-reviewed-by` - Insert a `Reviewed-by:` tag with your name and email at point
- `M-x gnus-reviews-greet` - Insert a greeting addressed to the recipient at the start of the message body

## License

GPL 3 or later
