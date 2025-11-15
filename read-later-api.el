;;; read-later-api.el --- Instapaper API endpoint mappings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Devin Davis
;;
;; Author: Devin Davis
;; Maintainer: Devin Davis
;; Created: November 11, 2025
;; Version: 2.0.0
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; API endpoint mapping functions for both Instapaper Simple API and Full API v1
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

(require 'url)
(require 'auth-source)
(require 'oauth)

;;; Internal constants

(defconst read-later-api--host "www.instapaper.com"
  "The official Instapaper host.")

(defconst read-later-api--base-url "https://www.instapaper.com"
  "The official Instapaper URL.")

;;; Internal variables

(defvar read-later-api--oauth-access-token nil
  "Cached OAuth access token for the current session.
Set by `read-later-api-oauth-setup'.")

;;; Endpoint registry

(defconst read-later-api--endpoints
  '(;; Simple API
    (simple-add . (:path "/api/add" :method "GET"))
    (simple-authenticate . (:path "/api/authenticate" :method "GET"))

    ;; OAuth endpoints
    (oauth-access-token . (:path "/api/1/oauth/access_token" :method "POST"))

    ;; Account endpoints
    (account-verify . (:path "/api/1/account/verify_credentials" :method "POST"))

    ;; Bookmark endpoints
    (bookmarks-list . (:path "/api/1/bookmarks/list" :method "POST"))
    (bookmarks-update-progress . (:path "/api/1/bookmarks/update_read_progress" :method "POST"))
    (bookmarks-add . (:path "/api/1/bookmarks/add" :method "POST"))
    (bookmarks-delete . (:path "/api/1/bookmarks/delete" :method "POST"))
    (bookmarks-star . (:path "/api/1/bookmarks/star" :method "POST"))
    (bookmarks-unstar . (:path "/api/1/bookmarks/unstar" :method "POST"))
    (bookmarks-archive . (:path "/api/1/bookmarks/archive" :method "POST"))
    (bookmarks-unarchive . (:path "/api/1/bookmarks/unarchive" :method "POST"))
    (bookmarks-move . (:path "/api/1/bookmarks/move" :method "POST"))
    (bookmarks-text . (:path "/api/1/bookmarks/get_text" :method "POST"))

    ;; Folder endpoints
    (folders-list . (:path "/api/1/folders/list" :method "POST"))
    (folders-add . (:path "/api/1/folders/add" :method "POST"))
    (folders-delete . (:path "/api/1/folders/delete" :method "POST"))
    (folders-set-order . (:path "/api/1/folders/set_order" :method "POST"))

    ;; Highlight endpoints (API v1.1)
    (highlights-list . (:path "/api/1.1/bookmarks/%s/highlights" :method "POST" :id-required t))
    (highlights-add . (:path "/api/1.1/bookmarks/%s/highlight" :method "POST" :id-required t))
    (highlights-delete . (:path "/api/1.1/highlights/%s/delete" :method "POST" :id-required t)))
  "Registry of all Instapaper API endpoints.")

;;; Helper functions

(defun read-later-api--get-credentials ()
  "Retrieve Instapaper credentials from auth-source.
Returns a cons cell (USERNAME . PASSWORD) or nil if not found."
  (let ((auth-info (car (auth-source-search :host read-later-api--host
                                            :require '(:user :secret)))))
    (message "result is: %S" auth-info)
    (when auth-info
      (cons (plist-get auth-info :user)
            (funcall (plist-get auth-info :secret))))))

(defun read-later-api--get-oauth-consumer-credentials ()
  "Retrieve OAuth consumer key and secret from auth-source.
Returns a cons cell (CONSUMER-KEY . CONSUMER-SECRET) or nil if not found.
Looks for host 'instapaper-oauth'."
  (let ((auth-info (car (auth-source-search :host "instapaper-oauth"
                                            :require '(:user :secret)))))

    (message "result is: %S" auth-info)
    (when auth-info
      (cons (plist-get auth-info :user)
            (funcall (plist-get auth-info :secret))))))

(defun read-later-api--make-auth-header (credentials)
  "Create Basic Auth header from CREDENTIALS cons cell."
  (let ((username (car credentials))
        (password (cdr credentials)))
    (concat "Basic "
            (base64-encode-string
             (concat username ":" password)
             t))))

(defun read-later-api--build-url (endpoint-key params &optional id)
  "Build full URL for ENDPOINT-KEY with PARAMS alist and optional ID."
  (let* ((endpoint-info (alist-get endpoint-key read-later-api--endpoints))
         (path (plist-get endpoint-info :path))
         (id-required (plist-get endpoint-info :id-required))
         (full-path (if id-required
                        (format path id)
                      path)))
    (if params
        (concat read-later-api--base-url
                full-path
                "?"
                (mapconcat (lambda (param)
                             (when (cdr param)  ; Only include non-nil values
                               (format "%s=%s"
                                       (url-hexify-string (if (symbolp (car param))
                                                              (symbol-name (car param))
                                                            (car param)))
                                       (url-hexify-string (format "%s" (cdr param))))))
                           params
                           "&"))
      (concat read-later-api--base-url full-path))))

(defun read-later-api--parse-response (status)
  "Parse HTTP response from STATUS buffer.
Returns a plist with :success, :status-code, :message, and :body keys."
  (let ((error-status (plist-get status :error)))
    (if error-status
        (list :success nil
              :status-code nil
              :message (format "Network error: %s" error-status)
              :body nil)
      (goto-char (point-min))
      (if (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
          (let ((http-status (string-to-number (match-string 1)))
                (body (progn
                        (re-search-forward "\n\n" nil t)
                        (buffer-substring-no-properties (point) (point-max)))))
            (list :success (or (= http-status 200) (= http-status 201))
                  :status-code http-status
                  :message (pcase http-status
                             (200 "Success")
                             (201 "Created successfully")
                             (400 "Bad request - check parameters")
                             (403 "Authentication failed")
                             (500 "Server error")
                             (_ (format "HTTP status %d" http-status)))
                  :body body))
        (list :success nil
              :status-code nil
              :message "Could not parse response"
              :body nil)))))

;;; Public API

(defun read-later-api-simple-request (endpoint &rest args)
  "Make a Simple API request to ENDPOINT with ARGS.
Uses Basic Authentication (username/password).

ENDPOINT is a symbol like 'simple-add, 'simple-authenticate.

ARGS is a plist that can contain:
  :params - alist of parameters to send (e.g., '((url . \"...\") (title . \"...\")))
  :callback - function called with response plist (:success :status-code :message :body)

Example usage:
  (read-later-api-simple-request 'simple-add
                                 :params '((url . \"https://example.com\"))
                                 :callback #'my-handler)

  (read-later-api-simple-request 'simple-authenticate
                                 :callback #'my-handler)"
  (let* ((params (plist-get args :params))
         (callback (plist-get args :callback))
         (credentials (read-later-api--get-credentials))
         (endpoint-info (alist-get endpoint read-later-api--endpoints))
         (method (plist-get endpoint-info :method))
         (url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(read-later-api--make-auth-header credentials))))
         (api-url (read-later-api--build-url endpoint params nil)))
    (url-retrieve api-url
                  (lambda (status)
                    (let ((result (read-later-api--parse-response status)))
                      (kill-buffer (current-buffer))
                      (when callback
                        (funcall callback result)))))))

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

    (message "Obtaining OAuth access token for %s..." username)

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
              (message "OAuth access token obtained successfully")
              read-later-api--oauth-access-token)
          (error
           (message "Failed to obtain OAuth access token: %s" (error-message-string err))
           nil))))))

(defun read-later-api-full-request (endpoint &rest args)
  "Make a Full API request to ENDPOINT with ARGS.
Uses OAuth 1.0 authentication with HMAC-SHA1 signing.

ENDPOINT is a symbol like 'bookmarks-list, 'folders-add, 'highlights-list, etc.

ARGS is a plist that can contain:
  :params - alist of parameters to send (e.g., '((folder_id . \"123\") (limit . \"25\")))
  :id - optional ID for endpoints that require it (e.g., bookmark ID for highlights)
  :callback - function called with response plist (:success :status-code :message :body)

The function automatically uses the cached OAuth token from `read-later-api-oauth-setup'.
If no token is cached, it will automatically call `read-later-api-oauth-setup' first.

Example usage:
  ;; First time: setup OAuth (or call interactively: M-x read-later-api-oauth-setup)
  (read-later-api-oauth-setup)

  ;; Make requests (OAuth setup happens automatically if needed)
  (read-later-api-full-request 'bookmarks-list
                               :params '((limit . \"25\"))
                               :callback #'my-handler)

  (read-later-api-full-request 'highlights-list
                               :id \"bookmark-id\"
                               :callback #'my-handler)"
  (let* ((params (plist-get args :params))
         (id (plist-get args :id))
         (callback (plist-get args :callback))
         (endpoint-info (alist-get endpoint read-later-api--endpoints))
         (method (plist-get endpoint-info :method)))

    ;; Ensure we have an OAuth access token
    (unless read-later-api--oauth-access-token
      (message "No OAuth token found. Setting up OAuth...")
      (unless (read-later-api-oauth-setup)
        (error "Failed to obtain OAuth access token. Cannot proceed with Full API request")))

    ;; Build the URL (without query parameters - they go in POST body)
    (let* ((api-url (read-later-api--build-url endpoint nil id))
           (oauth-post-vars-alist params)
           (url-request-method method))

      ;; Use oauth.el's url-retrieve wrapper which handles signing
      (oauth-url-retrieve
       read-later-api--oauth-access-token
       api-url
       (when callback
         (lambda (status)
           (let ((result (read-later-api--parse-response status)))
             (kill-buffer (current-buffer))
             (funcall callback result))))
       nil))))

(provide 'read-later-api)

;;; read-later-api.el ends here
