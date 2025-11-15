;;; read-later.el --- add urls to instapaper with a zing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Devin Davis
;;
;; Author: Devin Davis <devindavis@pop-os>
;; Maintainer: Devin Davis <devindavis@pop-os>
;; Created: October 25, 2025
;; Modified: November 06, 2025
;; Version: 2.0.0
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/barnacleDevelopments/read-later.el
;; Instapaper API Docs: https://www.instapaper.com/api/simple
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; read-later.el was made to provide a comprehensive url adding experience
;;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:
(require 'read-later-api)
(require 'vtable)
(require 'json)

(setq url-debug t)

;;; Internal variables

(defvar-local read-later--bookmarks-data nil
  "Cached bookmarks data for the current buffer.")

(defvar-local read-later--vtable nil
  "The vtable object for the bookmarks list.")

;;;###autoload
(defun read-later-add-url (url)
  "Add URL to Instapaper account asynchronously."
  (message "Adding to Instapaper...")
  (read-later-api-simple-request 'simple-add
                                 :params `((url . ,url))
                                 :callback (lambda (result)
                                             (if (plist-get result :success)
                                                 (message "✓ Added to Instapaper: %s" url)
                                               (message "✗ %s" (plist-get result :message))))))

;;;###autoload
(defun read-later-test-auth ()
  "Test Instapaper authentication asynchronously."
  (interactive)
  (message "Testing authentication...")
  (read-later-api-simple-request 'simple-authenticate
                                 :callback (lambda (result)
                                             (if (plist-get result :success)
                                                 (message "✓ Authentication successful!")
                                               (message "✗ %s" (plist-get result :message))))))

;;;###autoload
(defun read-later-add-url-at-point()
  "Add the URL at point to Instapaper."
  (interactive)
  (let ((url (thing-at-point 'url)))
    (read-later-add-url url)))

;;;###autoload
(defun read-later-add-elfeed-entry-at-point()
  "Add the elfeed entry at point in show buffer."
  (interactive)
  (let ((url (or (elfeed-entry-link (elfeed-search-selected :ignore-region)))))
    (read-later-add-url url)))

;;;###autoload
(defun read-later-interactively-add-url(url)
  "Add URL interactively."
  (interactive "sArticle URL:")
  (read-later-add-url url))

;;; Bookmarks List Functions

(defun read-later--parse-bookmark (bookmark-data)
  "Parse BOOKMARK-DATA from JSON into a plist for display."
  (list :bookmark-id (alist-get 'bookmark_id bookmark-data)
        :title (or (alist-get 'title bookmark-data) "Untitled")
        :url (alist-get 'url bookmark-data)
        :description (or (alist-get 'description bookmark-data) "")
        :progress (or (alist-get 'progress bookmark-data) 0.0)
        :time (alist-get 'time bookmark-data)))

(defun read-later--format-progress (progress)
  "Format PROGRESS (0.0-1.0) as a percentage string."
  (if progress
      (format "%d%%" (round (* 100 progress)))
    "0%"))

(defun read-later--create-bookmarks-buffer (bookmarks-json)
  "Create and populate the bookmarks list buffer with BOOKMARKS-JSON."
  (message "DEBUG: Received JSON: %s" (substring bookmarks-json 0 (min 200 (length bookmarks-json))))
  (let* ((data (json-read-from-string bookmarks-json))
         ;; Data is a vector of objects, filter for type="bookmark"
         (bookmarks (cl-remove-if-not
                     (lambda (item)
                       (equal (alist-get 'type item) "bookmark"))
                     (append data nil)))
         (parsed-bookmarks (mapcar #'read-later--parse-bookmark bookmarks)))

    (message "DEBUG: Parsed %d bookmarks" (length parsed-bookmarks))
    (message "DEBUG: First bookmark: %S" (car parsed-bookmarks))

    (with-current-buffer (get-buffer-create "*Instapaper Bookmarks*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (read-later-bookmarks-mode)

        (setq read-later--bookmarks-data parsed-bookmarks)

        ;; Create vtable
        (setq read-later--vtable
              (make-vtable
               :columns '((:name "Title" :width 50)
                          (:name "Progress" :width 10 :align right)
                          (:name "Description" :width 60))
               :objects parsed-bookmarks
               :getter (lambda (bookmark column vtable)
                         (pcase (vtable-column vtable column)
                           ("Title" (plist-get bookmark :title))
                           ("Progress" (read-later--format-progress
                                        (plist-get bookmark :progress)))
                           ("Description" (plist-get bookmark :description))))))

        (message "DEBUG: Buffer contents length: %d" (buffer-size))
        (goto-char (point-min)))
      (switch-to-buffer (current-buffer)))))

(defun read-later--open-bookmark-at-point ()
  "Open the bookmark URL at point in browser."
  (interactive)
  (when-let* ((bookmark (vtable-current-object))
              (url (plist-get bookmark :url)))
    (browse-url url)
    (message "Opening: %s" url)))

(defvar read-later-bookmarks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'read-later--open-bookmark-at-point)
    (define-key map (kbd "g") #'read-later-list-bookmarks)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `read-later-bookmarks-mode'.")

(define-derived-mode read-later-bookmarks-mode special-mode "Read-Later-Bookmarks"
  "Major mode for displaying Instapaper bookmarks.

\\{read-later-bookmarks-mode-map}"
  (setq buffer-read-only t))

;;;###autoload
(defun read-later-view-bookmarks ()
  "Fetch and display Instapaper bookmarks in a vtable."
  (interactive)
  (message "Fetching bookmarks...")
  (read-later-api-full-request 'bookmarks-list
                               :callback
                               (lambda (result)
                                 (if (plist-get result :success)
                                     (progn
                                       (message "✓ Bookmarks retrieved")
                                       (read-later--create-bookmarks-buffer
                                        (plist-get result :body)))
                                   (message "✗ Failed to fetch bookmarks: %s"
                                            (plist-get result :message))))))

(provide 'read-later)

;;; read-later.el ends here











