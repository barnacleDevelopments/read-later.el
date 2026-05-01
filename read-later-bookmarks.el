;;; read-later-bookmarks.el --- Bookmarks list functions-*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Internal utilities for managing bookmarks

;;; Code:
(require 'read-later-globals)
(require 'tabulated-list)
(require 'json)
(require 'cl-lib)
(require 'read-later-api)
(require 'read-later-utils)
(require 'shr)

;; ========================================= FORMATER FUNCTIONS =========================================
(defun read-later--format-progress (progress)
  "Format PROGRESS as a percentage string."
  (if progress
      (format "%.0f%%" (* progress 100))
    "0%"))

;; TODO globalize tag color
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
            (let ((face (if (< (plist-get bookmark :progress) 1) 'read-later-unread-tabulated-row-face 'read-later-read-tabulated-row-face)))
              (list (plist-get bookmark :bookmark_id)
                    (vector (or (propertize (plist-get bookmark :title) 'face face) "")
                            (read-later--format-tags (plist-get bookmark :tags))
                            (propertize (or (plist-get bookmark :description) "") 'face face))
                    )))
          bookmarks))

;; ========================================= ACTION FUNCTIONS =========================================
(defun read-later-view-bookmark (id)
  "Open the bookmark content with ID in buffer."
  (read-later-check-bookmarks-buffer
   (lambda (buffer)
     (let ((params `(("bookmark_id" . ,(number-to-string id)))))
       (read-later-api-full-request 'bookmarks-text
                                    :params params
                                    :type "html"
                                    :callback (lambda (text)
                                                (with-current-buffer buffer
                                                  (let* ((view-bookmark-buffer (get-buffer-create (format "*Read Later Bookmark %S*" id))))
                                                    (with-current-buffer view-bookmark-buffer
                                                      (progn
                                                        (goto-char (point-max))
                                                        (insert text)
                                                        (shr-render-buffer view-bookmark-buffer)))))))))))

(defun read-later--update-bookmark-read-progress (id progress)
  "Update the bookmarks with ID to read PROGRESS."
  (read-later-check-bookmarks-buffer
   (lambda (buffer)
     (read-later-api-full-request 'bookmarks-update-progress
                                  :params `(("bookmark_id" . ,(number-to-string id))
                                            ("progress" . ,(number-to-string progress))
                                            ("progress_timestamp" . ,(number-to-string (floor (float-time)))))
                                  :type "bookmark"
                                  :callback (lambda (bookmarks)
                                              (with-current-buffer buffer
                                                (progn
                                                  (read-later--update-bookmark (car bookmarks))
                                                  (message "Bookmark read progress updated"))))))))

(defun read-later--archive-bookmark (id)
  "Archive bookmark with ID."
  (read-later-check-bookmarks-buffer
   (lambda (buffer)
     (read-later-api-full-request 'bookmarks-archive
                                  :params `(("bookmark_id" . ,(number-to-string id)))
                                  :type "bookmark"
                                  :callback (lambda (bookmarks)
                                              (with-current-buffer buffer
                                                (progn
                                                  (read-later--update-bookmark (car bookmarks))
                                                  (read-later--remove-bookmarks (list id))
                                                  (message "Bookmark archived"))))))))

(defun read-later-delete-bookmark (id)
  "Delete bookmark with ID."
  (read-later-check-bookmarks-buffer
   (lambda (buffer)
     (read-later-api-full-request 'bookmarks-delete
                                  :params `(("bookmark_id" . ,(number-to-string id)))
                                  :type "bookmark"
                                  :callback (lambda (&rest _)
                                              (with-current-buffer buffer
                                                (progn
                                                  (read-later--remove-bookmarks (list id))
                                                  (message "Bookmark deleted: %s" id))))))))

;; ========================================= TABLE UPDATE FUNCTIONS =========================================
(defun read-later--unarchive-bookmark (id)
  "Archive bookmark with ID."
  (read-later-api-full-request 'bookmarks-unarchive
                               :params `(("bookmark_id" . ,(number-to-string id))))
  :type "bookmark"
  :callback (lambda (bookmarks)
              (progn
                (read-later--update-bookmark (car bookmarks))
                (message "Bookmark unarchived"))))

(defun read-later--display-bookmarks (bookmarks)
  "Display BOOKMARKS in the buffer."
  (with-current-buffer "*Read-Later Bookmarks*"
    (setq read-later--bookmarks-data bookmarks)
    (setq tabulated-list-entries (read-later--create-bookmark-entries bookmarks))
    (tabulated-list-print t)))

(defun read-later--append-bookmarks (bookmarks)
  "Append BOOKMARKS to existing data."
  (with-current-buffer "*Read-Later Bookmarks*"
    (setq read-later--bookmarks-data
          (append read-later--bookmarks-data bookmarks))
    (read-later--display-bookmarks read-later--bookmarks-data)))

(defun read-later--remove-bookmarks (bookmark_ids)
  "Remove BOOKMARK_IDS list."
  (setq read-later--bookmarks-data
        (cl-remove-if (lambda (b) (member (plist-get b :bookmark_id) bookmark_ids))
                      read-later--bookmarks-data))
  (read-later--display-bookmarks read-later--bookmarks-data))

(defun read-later--update-bookmark (bookmark)
  "Update BOOKMARK fields inside tabulated list. The bookmark must have an ID."
  (let ((bookmark_id (plist-get bookmark :bookmark_id)))
    (progn
      (read-later--remove-bookmarks (list bookmark_id))
      (setq read-later--bookmarks-data (cons bookmark read-later--bookmarks-data))
      (read-later--display-bookmarks read-later--bookmarks-data))))

(provide 'read-later-bookmarks)

;;; read-later-bookmarks.el ends here
