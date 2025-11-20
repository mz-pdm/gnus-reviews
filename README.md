# Gnus Reviews

An Emacs Lisp package for managing email-based code reviews in Gnus.

## Overview

Gnus Reviews helps you organize and track email-based code review workflows. While designed with libcamera development in mind, it's generic and can be used with any project that uses email for code reviews.

## Features

- **Message Classification**: Automatically identifies patch emails vs. review comments
- **Group Organization**: Copies messages to purpose-specific Gnus groups
- **Individual Comment Tracking**: Track specific comments within review emails with statuses (pending/addressed/dismissed)
- **Patch Series Awareness**: Understands patch series and tracks comments across the entire series
- **Scoring Integration**: Automatically boosts scores for review-relevant articles
- **Review Lifecycle Management**: Prune old completed reviews while preserving pending ones
- **Persistent Storage**: Automatically saves and loads comment tracking data

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

;; Auto-expire completed reviews after N days
(setq gnus-reviews-auto-expire-days 30)
```

## Usage

### Basic Commands

- `M-x gnus-reviews-process-article` - Automatically classify and copy current article to appropriate group
- `M-x gnus-reviews-copy-to-group` - Manually copy current article to a review group
- `M-x gnus-reviews-increase-score` - Boost score for review-relevant articles

### Comment Tracking

- `M-x gnus-reviews-extract-and-track-comments` - Parse individual comments from a review email and assign statuses
- `M-x gnus-reviews-mark-region-as-comment` - Mark selected text as an individual comment
- `M-x gnus-reviews-mark-comment-addressed` - Mark a pending comment as addressed

### Viewing Comments

- `M-x gnus-reviews-show-article-comments` - Show all tracked comments for current article
- `M-x gnus-reviews-show-series-comments` - Show all comments for current patch series

### Maintenance

- `M-x gnus-reviews-prune-old-reviews` - Remove old completed/dismissed reviews (keeps pending ones)

## Workflow Example

1. **Incoming Review**: Use `gnus-reviews-process-article` to automatically classify and organize
2. **Track Comments**: For review emails, use `gnus-reviews-extract-and-track-comments` to parse and track individual feedback items
3. **Monitor Progress**: Use `gnus-reviews-show-series-comments` to see the status of all comments for a patch series
4. **Mark Resolution**: Use `gnus-reviews-mark-comment-addressed` when you've addressed feedback
5. **Cleanup**: Periodically run `gnus-reviews-prune-old-reviews` to remove old completed reviews

## Comment Status Types

- **pending**: Comment needs to be addressed
- **addressed**: Comment has been resolved/fixed
- **dismissed**: Comment was acknowledged but won't be addressed

## License

GPL 3 or later

Copyright (C) 2025 Red Hat, Inc.