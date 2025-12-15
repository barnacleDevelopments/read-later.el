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

;;;###autoload
(defun read-later-api-oauth-setup ()
  "Set up OAuth authentication for the Instapaper Full API.
Retrieves consumer credentials from 'instapaper-oauth' and user credentials
from 'www.instapaper.com' in auth-source, then uses xAuth to obtain an
access token. The token is cached in `read-later-api--oauth-access-token'.

Returns the oauth-access-token object on success, or nil on failure."
  (interactive)
  (let* ((consumer-creds (read-later-api--get-oauth-consumer-credentials))
         (user-creds (read-later-api--get-credentials))
         (consumer-key (car consumer-creds))
         (consumer-secret (cdr consumer-creds))
         (username (car user-creds))
         (password (cdr user-creds)))

    (unless consumer-creds
      (error "OAuth consumer credentials not found. Please add 'instapaper-oauth' to your authinfo file"))

    (unless user-creds
      (error "User credentials not found. Please add 'www.instapaper.com' to your authinfo file"))

    ;; Create OAuth request for xAuth access token
    (let* ((access-token-url "https://www.instapaper.com/api/1/oauth/access_token")
           (req (oauth-make-request access-token-url consumer-key))
           (xauth-params `(("x_auth_mode" . "client_auth")
                           ("x_auth_username" . ,username)
                           ("x_auth_password" . ,password))))

      ;; Set HTTP method to POST (required by Instapaper)
      (setf (oauth-request-http-method req) "POST")

      ;; Add xAuth parameters to the request params for signature calculation
      (setf (oauth-request-params req)
            (append (oauth-request-params req) xauth-params))

      ;; Sign the request with HMAC-SHA1
      (oauth-sign-request-hmac-sha1 req consumer-secret)

      ;; Fetch the access token with xAuth params in POST body
      (let ((oauth-post-vars-alist xauth-params))
        (condition-case err
            (let ((token (oauth-fetch-token req)))
              (setq read-later-api--oauth-access-token
                    (make-oauth-access-token
                     :consumer-key consumer-key
                     :consumer-secret consumer-secret
                     :auth-t token))
              read-later-api--oauth-access-token)
          (error
           (error "Failed to obtain OAuth access token: %s" (error-message-string err))))))))
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

;; ========================================= BOOKMARK FUNCTIONS =========================================
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
  (message "Refreshing bookmarks...")
  (if (not (get-buffer "*Instapaper Bookmarks*"))
      (message "✗ Please run M-x read-later first to create the bookmarks buffer")
    (with-current-buffer "*Instapaper Bookmarks*"
      (let ((params `(("limit" . "25"))))
        (read-later-api-full-request 'bookmarks-list
                                     :params params
                                     :callback
                                     (lambda (result)
                                       (let ((bookmarks (read-later--handle-request-body result :type "bookmark")))
                                         (with-current-buffer "*Instapaper Bookmarks*"
                                           (setq read-later--bookmarks-data bookmarks)
                                           (setq tabulated-list-entries (read-later--format-bookmarks bookmarks))
                                           (tabulated-list-print t)
                                           (message "✓ Bookmarks refreshed")))))))))

;;;###autoload
(defun read-later-load-more ()
  "Load extra bookmarks into the current bookmark buffer."
  (interactive)
  (with-current-buffer "*Instapaper Bookmarks*"
    (let ((params `(("limit" . ,(number-to-string (+ 25 (or (length read-later--bookmarks-data) 0))))
                    ("have" . ,(read-later--get-resource-ids read-later--bookmarks-data)))))
      (read-later-api-full-request 'bookmarks-list
                                   :params params
                                   :callback
                                   (lambda (result)
                                     (let ((bookmarks (read-later--handle-request-body result :type "bookmark")))
                                       (with-current-buffer "*Instapaper Bookmarks*"
                                         (setq read-later--bookmarks-data (append read-later--bookmarks-data bookmarks))
                                         (setq tabulated-list-entries (read-later--format-bookmarks read-later--bookmarks-data))
                                         (tabulated-list-print t)
                                         (message "✓ 25 More Bookmarks loaded"))))))))

(defun read-later-open-bookmark-at-point ()
  "Open the bookmark URL at point in the bookmark buffer in browser."
  (interactive)
  (let* ((bookmark-id (tabulated-list-get-id))
         (bookmark (cl-find-if (lambda (b) (equal (plist-get b :bookmark_id) bookmark-id))
                               read-later--bookmarks-data))
         (url (plist-get bookmark :url)))
    (if url
        (browse-url url)
      (message "Warning: No URL found for this bookmark"))))

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
(provide 'read-later)

;;; read-later.el ends here











