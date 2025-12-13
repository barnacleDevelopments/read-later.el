;;; read-later-bookmarks.el --- Bookmarks list functions-*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Internal utilities for managing bookmarks

;;; Code:

(require 'tabulated-list)
(require 'json)
(require 'cl-lib)

;;; Variables

(defvar read-later--bookmarks-data nil
  "List of bookmark plists for the current buffer.")

;;; Helper Functions

(defun read-later--handle-request-body (result &rest args)
  "Parse the JSON body from RESULT and extract items of TYPE.
ARGS should contain :type keyword with value like \"bookmark\" or \"highlight\"."
  (let ((type (plist-get args :type))
        (body (plist-get result :body)))
    (if (plist-get result :success)
        (let* ((json-object-type 'plist)
               (json-key-type 'keyword)
               (json-array-type 'list)
               (parsed (json-read-from-string body))
               ;; Filter the flat array for items matching the type
               (items (cl-remove-if-not
                       (lambda (item)
                         (string= (plist-get item :type) type))
                       parsed)))
          items)
      (error "Failed to fetch resource: %s" (plist-get result :message)))))

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

;;; Bookmarks Mode

(defvar read-later-bookmarks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'read-later-update)
    (define-key map (kbd "RET") 'read-later-open-bookmark-at-point)
    map)
  "Keymap for `read-later-bookmarks-mode'.")

(define-derived-mode read-later-bookmarks-mode tabulated-list-mode "Instapaper Bookmarks"
  "Major mode for viewing Instapaper bookmarks."
  (setq tabulated-list-format [("Title" 50 t)
                               ("Progress" 10 t)
                               ("Tags" 30 t)
                               ("Description" 100 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header))

(defun read-later--create-bookmarks-buffer (bookmarks)
  "Create the bookmarks buffer with BOOKMARKS data."
  (with-current-buffer (get-buffer-create "*Instapaper Bookmarks*")
    (read-later-bookmarks-mode)
    (setq read-later--bookmarks-data bookmarks)
    (setq tabulated-list-entries (read-later--format-bookmarks bookmarks))
    (tabulated-list-print t)
    (current-buffer)))

(defun read-later--format-bookmarks (bookmarks)
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
