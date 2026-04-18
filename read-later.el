;;; read-later.el --- add urls to instapaper with a zing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Devin Davis
;;
;; Author: Devin Davis <devindavis@pop-os>
;; Maintainer: Devin Davis <devindavis@pop-os>
;; Created: October 25, 2025
;; Modified: November 06, 2025
;; Version: 2.1.2
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/barnacleDevelopments/read-later.el
;; Instapaper API Docs: https://www.instapaper.com/api/simple
;; Package-Requires: ((emacs "27.1"))
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
(require 'read-later-globals)
(require 'read-later-mode)
(require 'read-later-api)
(require 'read-later-bookmarks)
(require 'read-later-tags)
(require 'read-later-folders)
(require 'read-later-utils)
(require 'json)

;; ========================================= AUTH TEST FUNCTIONS =========================================
;;;###autoload
(defun read-later-test-auth ()
  "Test Instapaper simple authentication."
  (interactive)
  (message "Testing authentication...")
  (read-later-api-simple-request 'simple-authenticate
                                 :callback (lambda (result)
                                             (if (plist-get result :success)
                                                 (message "✓ Authentication successful!")
                                               (message "✗ %s" (plist-get result :message))))))

;;;###autoload
(defun read-later-test-full-auth ()
  "Test Instapaper Full API Authentication."
  (interactive)
  (message "Testing full api authentication...")
  (read-later-api-full-request 'account-verify
                               :callback (lambda(result)
                                           (if result
                                               (message "✓ Authentication successful!")
                                             (message "Authentication failed.")))))

;; ========================================= ANY BUFFER FUNCTIONS =========================================
;;;###autoload
(defun read-later-interactively-add-url(url)
  "Add URL interactively."
  (interactive "sURL:")
  (read-later-add-url url))

;; ========================================= BOOKMARK ACTION FUNCTIONS =========================================
;;;###autoload
(defun read-later-add-url-at-point()
  "Add the URL at point to Instapaper."
  (interactive)
  (let ((url (thing-at-point 'url)))
    (read-later-add-url url)))

;;;###autoload
(defun read-later ()
  "Enter read-later. Open the bookmarks table buffer."
  (interactive)
  (let* ((bookmarks-buffer (get-buffer "*Read-Later Bookmarks*")))
    (unless bookmarks-buffer (read-later--create-bookmarks-buffer read-later--bookmarks-data))
    (switch-to-buffer "*Read-Later Bookmarks*")))

(defun read-later-update ()
  "Load bookmarks into the current bookmark buffer."
  (read-later-check-bookmarks-buffer
   (lambda (buffer)
     (let ((params `(("limit" . ,(number-to-string read-later-append-limit))
                     ("have" . ,(mapconcat 'number-to-string (mapcar (lambda (resource)
                                                                       (plist-get resource :bookmark_id)) read-later--bookmarks-data) ","))
                     ("tag". ,read-later-tag)
                     ("folder_id" . ,read-later-folder))))
       (read-later-api-full-request 'bookmarks-list
                                    :params params
                                    :type "bookmark"
                                    :callback (lambda (bookmarks)
                                                (with-current-buffer buffer
                                                  (read-later--display-bookmarks bookmarks)
                                                  (message (format "  %S More Bookmarks loaded" read-later-append-limit)))))))))

;;;###autoload
(defun read-later-load-more ()
  "Load extra bookmarks into the current bookmark buffer."
  (interactive)
  (read-later-check-bookmarks-buffer
   (lambda (buffer)
     (let ((params `(("limit" . ,(number-to-string (+ read-later-append-limit (or (length read-later--bookmarks-data) 0))))
                     ("have" . ,(mapconcat 'number-to-string (mapcar (lambda (resource)
                                                                       (plist-get resource :bookmark_id)) read-later--bookmarks-data) ","))
                     ("tag". ,read-later-tag)
                     ("folder_id" . ,read-later-folder))))
       (read-later-api-full-request 'bookmarks-list
                                    :params params
                                    :type "bookmark"
                                    :callback (lambda (bookmarks)
                                                (with-current-buffer buffer
                                                  (read-later--append-bookmarks bookmarks)
                                                  (message (format "  %S More Bookmarks loaded" read-later-append-limit)))))))))

;;;###autoload
(defun read-later-delete-bookmark-at-point ()
  "Delete bookmark at point."
  (interactive)
  (let* ((id (tabulated-list-get-id)))
    (if(yes-or-no-p "Are you sure you want to delete this bookmark?")
        (read-later-delete-bookmark id))))

;;;###autoload
(defun read-later-mark-read-at-point ()
  "Mark the bookmark at point as read."
  (interactive)
  (let((id (tabulated-list-get-id)))
    (message (format "Updating read progress for: %S" id))
    (read-later--update-bookmark-read-progress id 1.0)))

;;;###autoload
(defun read-later-mark-unread-at-point ()
  "Mark the bookmark at point as unread."
  (interactive)
  (let((id (tabulated-list-get-id)))
    (message (format "Updating read progress for: %S" id))
    (read-later--update-bookmark-read-progress id 0)))

;;;###autoload
(defun read-later-clear-filters ()
  "Clear the current tag filter."
  (setq read-later-folder nil)
  (setq read-later-tag nil))

;;;###autoload
(defun read-later-clear-table ()
  "Clear bookmarks from table."
  (interactive)
  (read-later-check-bookmarks-buffer
   (lambda (_)
     (setq read-later--bookmarks-data nil)
     (read-later-clear-filters)
     (read-later--display-bookmarks nil))))

;;;###autoload
(defun read-later-archive-bookmark-at-point ()
  "Arhive bookmark at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (progn
      (read-later--archive-bookmark id))))

;;;###autoload
(defun read-later-open-bookmark-at-point ()
  "Open the bookmark URL at point in the bookmark buffer in browser."
  (interactive)
  (read-later-check-bookmarks-buffer
   (lambda (_)
     (let* ((bookmark-id (tabulated-list-get-id))
            (bookmark (seq-find (lambda (b) (equal (plist-get b :bookmark_id) bookmark-id))
                                read-later--bookmarks-data))
            (url (plist-get bookmark :url)))
       (if url
           (browse-url url)
         (message "Warning: No URL found for this bookmark"))))))

;; ========================================= FILTER FUNCTIONS =========================================
;;;###autoload
(defun read-later-set-folder-filter ()
  "Set the current folder filter."
  (interactive)
  (read-later-check-bookmarks-buffer
   (lambda (buffer)
     (read-later-api-full-request 'folders-list
                                  :type "folder"
                                  :callback (lambda (folders)
                                              (with-current-buffer buffer
                                                (let ((selected-folder-id (read-later--prompt-folder-select folders)))
                                                  (read-later-clear-table)
                                                  (setq read-later-folder (or selected-folder-id "unread"))
                                                  (read-later-update))))))))

;;;###autoload
(defun read-later-search-tag ()
  "Seach by tag."
  (interactive)
  (read-later-check-bookmarks-buffer
   (lambda (_)
     (let* ((tag (read-later--prompt-tag-search)))
       (read-later-clear-table)
       (setq read-later-tag tag)
       (read-later-update)))))

;; ========================================= LINK HINT FUNCTIONS =========================================
;;;###autoload
(defun read-later-link-hint-add-url ()
  "Add url using link-hint."
  (interactive)
  (link-hint-copy-link)
  (read-later-add-url (car kill-ring)))

;; ========================================= ELFEED ACTION FUNCTIONS =========================================
;;;###autoload
(defun read-later-add-elfeed-entry-at-point()
  "Add the elfeed entry at point in show buffer."
  (interactive)
  (let ((url (or (elfeed-entry-link (elfeed-search-selected :ignore-region)))))
    (read-later-add-url url)))

(provide 'read-later)

;;; read-later.el ends here
