;;; read-later-bookmarks.el --- Bookmarks list functions-*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Internal utilities for managing bookmarks

;;; Code:

(require 'tabulated-list)
(require 'json)
(require 'cl-lib)

(defvar read-later--bookmarks-data nil
  "List of bookmark plists for the current buffer.")

;; ========================================= FORMATER FUNCTIONS =========================================
(defun read-later--format-progress (progress)
  "Format PROGRESS as a percentage string."
  (if progress
      (format "%.0f%%" (* progress 100))
    "0%"))

(defun read-later--format-tags (tags)
  "Format TAGS (list of tag plists) as a comma-separated colorized string."
  (if (and tags (listp tags) (> (length tags) 0))
      (mapconcat (lambda (tag)
                   (let ((tag-name (plist-get tag :name)))
                     (propertize tag-name
                                 'face '(:foreground "#8be9fd" :weight bold))))
                 tags "  ")
    ""))

(defun read-later--create-bookmark-entries (bookmarks)
  "Format BOOKMARKS data."
  (mapcar (lambda (bookmark)
            (list (plist-get bookmark :bookmark_id)
                  (vector (or (plist-get bookmark :title) "")
                          (read-later--format-progress (plist-get bookmark :progress))
                          (read-later--format-tags (plist-get bookmark :tags))
                          (or (plist-get bookmark :description) ""))))
          bookmarks))

(provide 'read-later-bookmarks)

;;; read-later-bookmarks.el ends here
