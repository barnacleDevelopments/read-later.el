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

(defvar-local read-later--refresh-prompted nil
  "Tracks if refresh is required.")

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

(defun read-later--filter-api-data-type (json &rest args)
  "Parse data type specfified in ARGS from JSON."
  (let* ((data (json-read-from-string json))
         (type (plist-get args :type)))
    (cl-remove-if-not
     (lambda (item)
       (equal (alist-get 'type item) type))
     (append data nil))))

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

(defun read-later--create-bookmarks-buffer (&optional default-bookmarks)
  "Create and populate the bookmarks list buffer with DEFAULT-BOOKMARKS plist."
  (with-current-buffer (get-buffer-create "*Instapaper Bookmarks*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (read-later-bookmarks-mode)

      (setq read-later--bookmarks-data default-bookmarks)

      (setq read-later--vtable
            (make-vtable
             :columns '((:name "Title" :width 100)
                        (:name "Progress" :width 10 :align right)
                        (:name "Description" :width 60))
             :objects-function #'read-later--refresh-bookmarks ; TODO you are here trying to figure out how to refresh table with data
             :getter (lambda (bookmark column vtable)
                       (pcase (vtable-column vtable column)
                         ("Title" (plist-get bookmark :title))
                         ("Progress" (read-later--format-progress
                                      (plist-get bookmark :progress)))
                         ("Description" (plist-get bookmark :description))))
             :ellipsis t))
      (goto-char (point-min)))))

(defun read-later-open-bookmark-at-point ()
  "Open the bookmark URL at point in browser."
  (interactive)
  (when-let* ((bookmark (vtable-current-object))
              (url (plist-get bookmark :url)))
    (browse-url url)
    (message "Opening: %s" url)))

(defvar read-later-bookmarks-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'read-later-open-bookmark-at-point)
    (define-key map (kbd "g") #'read-later-view-bookmarks)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `read-later-bookmarks-mode'.")

(define-derived-mode read-later-bookmarks-mode special-mode "Read-Later-Bookmarks"
  "Major mode for displaying Instapaper bookmarks.

\\{read-later-bookmarks-mode-map}"
  (setq buffer-read-only t))

(defun read-later--handle-request-body(result &rest args)
  "Handles request RESULT from Instapaper takes ARGS of :type (i.e \='bookmarks')."
  (if (plist-get result :success)
      (progn
        
        (let* ((type (plist-get args :type))
               (body (plist-get result :body))
               (json (read-later--filter-api-data-type body :type type)))
          (message "✓ %S retrieved" type)
          (mapcar #'read-later--parse-bookmark json)))
    (message "✗ Failed to fetch resource: %s"
             (plist-get result :message))))

;;;###autoload
(defun read-later-view-bookmarks ()
  "Fetch and display Instapaper bookmarks in a vtable."
  (interactive)
  (let* ((bookmarks-buffer (get-buffer "*Instapaper Bookmarks*")))
    (unless bookmarks-buffer (read-later--create-bookmarks-buffer nil))
    (switch-to-buffer "*Instapaper Bookmarks*")))

(defun read-later--refresh-bookmarks ()
  "Refreshes the bookmark buffer."
  (interactive)
  (read-later-api-full-request 'bookmarks-list
                               :callback
                               (lambda (result)
                                 (setq read-later--bookmarks-data (read-later--handle-request-body result :type "bookmark")))))


(provide 'read-later)

;;; read-later.el ends here











