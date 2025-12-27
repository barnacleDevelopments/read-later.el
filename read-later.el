;;; read-later.el --- add urls to instapaper with a zing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Devin Davis
;;
;; Author: Devin Davis <devindavis@pop-os>
;; Maintainer: Devin Davis <devindavis@pop-os>
;; Created: October 25, 2025
;; Modified: November 06, 2025
;; Version: 2.1.2
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
  (read-later-api-full-request 'verify-credentials
                               :callback (lambda(result)
                                           (if (plist-get result :success)
                                               (message "✓ Authentication successful!")
                                             (message "Authentication failed.")))))

;; ========================================= ANY BUFFER FUNCTIONS =========================================
;;;###autoload
(defun read-later-add-url-at-point()
  "Add the URL at point to Instapaper."
  (interactive)
  (let ((url (thing-at-point 'url)))
    (read-later-add-url url)))

;;;###autoload
(defun read-later-interactively-add-url(url)
  "Add URL interactively."
  (interactive "sArticle URL:")
  (read-later-add-url url))

;; ========================================= ELFEED FUNCTIONS =========================================
;;;###autoload
(defun read-later-add-elfeed-entry-at-point()
  "Add the elfeed entry at point in show buffer."
  (interactive)
  (let ((url (or (elfeed-entry-link (elfeed-search-selected :ignore-region)))))
    (read-later-add-url url)))

;;; Bookmarks List Functions

;; ========================================= BOOKMARK ACTION FUNCTIONS =========================================
;;;###autoload
(defun read-later ()
  "Enter read-later. Open the bookmarks table buffer."
  (interactive)
  (let* ((bookmarks-buffer (get-buffer "*Instapaper Bookmarks*")))
    (unless bookmarks-buffer (read-later--create-bookmarks-buffer read-later--bookmarks-data))
    (switch-to-buffer "*Instapaper Bookmarks*")))

;;;###autoload
(defun read-later-update ()
  "Refreshes the bookmark buffer with the latest 25 bookmarks."
  (interactive)
  (if(read-later-check-bookmarks-buffer)
      (with-current-buffer "*Instapaper Bookmarks*"
        (message "Refreshing bookmarks...")
        (let ((params `(("limit" . "25"))))
          (read-later-api-full-request 'bookmarks-list
                                       :params params
                                       :callback (lambda (result)
                                                   (let ((bookmarks (read-later--handle-request-body result :type "bookmark")))
                                                     (read-later--display-bookmarks bookmarks))))))))

;;;###autoload
(defun read-later-load-more ()
  "Load extra bookmarks into the current bookmark buffer."
  (interactive)
  (if(read-later-check-bookmarks-buffer)
      (with-current-buffer "*Instapaper Bookmarks*"
        (let ((params `(("limit" . ,(number-to-string (+ 25 (or (length read-later--bookmarks-data) 0))))
                        ("have" . ,(read-later--get-resource-ids read-later--bookmarks-data)))))
          (read-later-api-full-request 'bookmarks-list
                                       :params params
                                       :callback (lambda (result)
                                                   (let ((bookmarks (read-later--handle-request-body result :type "bookmark")))
                                                     (read-later--append-bookmarks bookmarks)
                                                     (read-later--remove-bookmarks (mapcar (lambda (b) (plist-get b :bookmark_id)) bookmarks))
                                                     (message "  25 More Bookmarks loaded"))))))))

;;;###autoload
(defun read-later-delete-bookmark-at-point ()
  "Delete bookmark at point."
  (interactive)
  (if(read-later-check-bookmarks-buffer)
      (with-current-buffer "*Instapaper Bookmarks*"
        (let ((id (tabulated-list-get-id)))
          (if(yes-or-no-p "Are you sure you want to delete this bookmark?")
              (read-later-api-full-request 'bookmarks-delete
                                           :params `(("bookmark_id" . ,(number-to-string id)))
                                           :callback (lambda (result)
                                                       (let ((success (plist-get result :success)))
                                                         (if success
                                                             (progn
                                                               (read-later--remove-bookmarks (list id))
                                                               (message "Bookmark deleted: %s" id))
                                                           (message "Failed to delete bookmark: %s" id))))))))))


;;;###autoload
(defun read-later-open-bookmark-at-point ()
  "Open the bookmark URL at point in the bookmark buffer in browser."
  (interactive)
  (if(read-later-check-bookmarks-buffer)
      (let* ((bookmark-id (tabulated-list-get-id))
             (bookmark (cl-find-if (lambda (b) (equal (plist-get b :bookmark_id) bookmark-id))
                                   read-later--bookmarks-data))
             (url (plist-get bookmark :url)))
        (if url
            (browse-url url)
          (message "Warning: No URL found for this bookmark")))))

(defun read-later--display-bookmarks (bookmarks)
  "Display BOOKMARKS in the buffer."
  (with-current-buffer "*Instapaper Bookmarks*"
    (setq read-later--bookmarks-data bookmarks)
    (setq tabulated-list-entries (read-later--create-bookmark-entries bookmarks))
    (tabulated-list-print t)))

(defun read-later--append-bookmarks (bookmarks)
  "Append BOOKMARKS to existing data."
  (setq read-later--bookmarks-data
        (append read-later--bookmarks-data bookmarks))
  (read-later--display-bookmarks read-later--bookmarks-data))

(defun read-later--remove-bookmarks (bookmarks)
  "Remove BOOKMARKS list."
  (setq read-later--bookmarks-data
        (cl-remove-if (lambda (b) (member (plist-get b :bookmark_id) bookmarks))
                      read-later--bookmarks-data))
  (read-later--display-bookmarks read-later--bookmarks-data))

;; ========================================= UTILITY FUNCTIONS =========================================

(defun read-later--get-resource-ids (resource-list)
  "Return list of ids of provided RESOURCE-LIST."
  (mapconcat 'number-to-string (mapcar (lambda (resource)
                                         (plist-get resource :bookmark_id)) resource-list) ","))

(defun read-later-add-url (url)
  "Add URL to Instapaper account asynchronously."
  (message "Adding to Instapaper...")
  (read-later-api-simple-request 'simple-add
                                 :params `((url . ,url))
                                 :callback (lambda (result)
                                             (if (plist-get result :success)
                                                 (message "✓ Added to Instapaper: %s" url)
                                               (message "✗ %s" (plist-get result :message))))))

(defun read-later-check-bookmarks-buffer ()
  "Check if the *Instapaper Bookmarks* buffer exists."
  (if (get-buffer "*Instapaper Bookmarks*")
      t
    (message "Please run M-x read-later first to create the bookmarks buffer")
    nil))

(provide 'read-later)

;;; read-later.el ends here







