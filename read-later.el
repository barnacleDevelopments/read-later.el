;;; read-later.el --- add urls to instapaper with a zing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Devin Davis
;;
;; Author: Devin Davis <devindavis@pop-os>
;; Maintainer: Devin Davis <devindavis@pop-os>
;; Created: October 25, 2025
;; Modified: November 06, 2025
;; Version: 2.1.0
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
(require 'read-later-bookmarks)
(require 'json)

(setq url-debug t)

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
(defun read-later--get-resource-ids (resource-list)
  "Return list of ids of provided RESOURCE-LIST."
  (mapconcat 'number-to-string (mapcar (lambda (resource)
                                         (plist-get resource :bookmark_id)) resource-list) ","))

;;;###autoload
(defun read-later-update ()
  "Refreshes the bookmark buffer."
  (interactive)
  (message "Refreshing bookmarks...")
  (with-current-buffer "*Instapaper Bookmarks*"
    (read-later-api-full-request 'bookmarks-list
                                 :params `(("have" . ,(read-later--get-resource-ids read-later--bookmarks-data)))
                                 :callback
                                 (lambda (result)
                                   (let ((bookmarks (read-later--handle-request-body result :type "bookmark")))
                                     (setq read-later--bookmarks-data bookmarks)
                                     (setq tabulated-list-entries
                                           (mapcar (lambda (bookmark)
                                                     (list (plist-get bookmark :bookmark_id)
                                                           (vector (or (plist-get bookmark :title) "")
                                                                   (read-later--format-progress (plist-get bookmark :progress))
                                                                   (read-later--format-tags (plist-get bookmark :tags))
                                                                   (or (plist-get bookmark :description) ""))))
                                                   bookmarks))
                                     (tabulated-list-print t)
                                     (message "✓ Bookmarks refreshed"))))))

;;;###autoload
(defun read-later ()
  "Enter read-later."
  (interactive)
  (let* ((bookmarks-buffer (get-buffer "*Instapaper Bookmarks*")))
    (unless bookmarks-buffer (read-later--create-bookmarks-buffer read-later--bookmarks-data))
    (switch-to-buffer "*Instapaper Bookmarks*")))

(provide 'read-later)

;;; read-later.el ends here











