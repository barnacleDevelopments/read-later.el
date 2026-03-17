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

;; Keymap
(defvar read-later-map (make-sparse-keymap)
  "Read later keymap.")

;; Customize Variables
(defcustom read-later-update-limit 25
  "The Instapaper API limit query parameter value."
  :type 'integer
  :group 'read-later)

(defcustom read-later-append-limit 50
  "The Instapaper API limit query parameter value when loading more."
  :type 'integer
  :group 'read-later)

(defcustom read-later-default-tag nil
  "The default tags applied to BOOKMARKS query.
Only filters if folder is not specified."
  :type 'string
  :group 'read-later)

(defcustom read-later-default-folder "unread"
  "The default folders applied to BOOKMARKS query."
  :type 'string
  :group 'read-later)

;; Variables
(defvar read-later-folder read-later-default-folder
  "Current active folder for filtering.")

(defvar read-later-tag read-later-default-tag
  "Current active tags for filtering.")

(defun read-later-reset-folders ()
  "Reset active folders to defaults."
  (setq read-later-folder read-later-default-folder))

(defun read-later-reset-tags ()
  "Reset active tags to defaults."
  (setq read-later-tag read-later-default-tag))

;; ========================================= READ LATER MODE =========================================
(defvar read-later-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" 'read-later-update)
    (define-key map "d" 'read-later-delete-bookmark-at-point)
    (define-key map "l" 'read-later-load-more)
    map)
  "Keymap for read-later-mode.")

(define-derived-mode read-later-mode tabulated-list-mode "Instapaper Bookmarks"
  "Major mode for viewing Instapaper bookmarks."
  (setq tabulated-list-format [("Title" 50 t)
                               ("Progress" 10 t)
                               ("Tags" 30 t)
                               ("Description" 100 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun read-later--create-bookmarks-buffer (bookmarks)
  "Create the bookmarks buffer with BOOKMARKS data."
  (with-current-buffer (get-buffer-create "*Instapaper Bookmarks*")
    (read-later-mode)
    (setq read-later--bookmarks-data bookmarks)
    (setq tabulated-list-entries (read-later--create-bookmark-entries bookmarks))
    (tabulated-list-print t)
    (current-buffer)))

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
(defun read-later-interactively-add-url(url)
  "Add URL interactively."
  (interactive "sArticle URL:")
  (read-later-add-url url))

;; ========================================= ELFEED ACTION FUNCTIONS =========================================
;;;###autoload
(defun read-later-add-elfeed-entry-at-point()
  "Add the elfeed entry at point in show buffer."
  (interactive)
  (let ((url (or (elfeed-entry-link (elfeed-search-selected :ignore-region)))))
    (read-later-add-url url)))

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
  (let* ((bookmarks-buffer (get-buffer "*Instapaper Bookmarks*")))
    (unless bookmarks-buffer (read-later--create-bookmarks-buffer read-later--bookmarks-data))
    (switch-to-buffer "*Instapaper Bookmarks*")))

;;;###autoload
(defun read-later-clear-table ()
  "Clear bookmarks from table."
  (interactive)
  (read-later--display-bookmarks nil))

;;;###autoload
(defun read-later-load-more ()
  "Load extra bookmarks into the current bookmark buffer."
  (interactive)
  (if(read-later-check-bookmarks-buffer)
      (with-current-buffer "*Instapaper Bookmarks*"
        (let ((params `(("limit" . ,(number-to-string (+ read-later-append-limit (or (length read-later--bookmarks-data) 0))))
                        ("have" . ,(read-later--get-resource-ids read-later--bookmarks-data))
                        ("tag". ,read-later-tag)
                        ("folder_id" . ,read-later-folder))))
          (read-later-api-full-request 'bookmarks-list
                                       :params params
                                       :type "bookmark"
                                       :callback (lambda (bookmarks)
                                                   (read-later--append-bookmarks bookmarks)
                                                   (message (format "  %S More Bookmarks loaded" read-later-append-limit))))))))

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
                                           :type "bookmark"
                                           :callback (lambda (&rest _)
                                                       (progn
                                                         (read-later--remove-bookmarks (list id))
                                                         (message "Bookmark deleted: %s" id)))))))))

(defun read-later-mark-read-at-point ()
  "Mark the bookmark at point as read."
  (interactive)
  (let((id (tabulated-list-get-id)))
    (message (format "Updating read progress for: %S" id))
    (read-later--update-bookmark-read-progress id 1.0)))

(defun read-later-mark-unread-at-point ()
  "Mark the bookmark at point as unread."
  (interactive)
  (let((id (tabulated-list-get-id)))
    (message (format "Updating read progress for: %S" id))
    (read-later--update-bookmark-read-progress id 0)))

;;;###autoload
(defun read-later-archive-bookmark-at-point ()
  "Arhive bookmark at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (progn
      (read-later--archive-bookmark id)
      (read-later--remove-bookmarks (list id)))))

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







