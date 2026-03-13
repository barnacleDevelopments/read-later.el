;;; oauth.el --- Oauth library.  -*- lexical-binding: t; -*-

;; Copyright (C) 2009 Peter Sanford

;; Author: Peter Sanford <peter AT petersdanceparty.com>
;; Keywords: comm
;; Contributors:
;;     Anthony Garcia <lagg@lavabit.com>
;;     Leo Shidai Liu <github.com/leoliu>
;;     Neil Roberts <bpeeluk@yahoo.co.uk>
;;     Devin Davis <devdeveloper.ca>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is oauth client library implementation in elisp. It is
;; capable of authenticating (receiving an access token) and signing
;; requests. Currently it only supports HMAC-SHA1, although adding
;; additional signature methods should be relatively straight forward.

;; Visit http://oauth.net/core/1.0a for the complete oauth spec.

;; Fixes Applied:
;; - Fixed POST request parameter encoding: POST parameters are now properly
;;   URL-encoded in the request body (both for curl and url-retrieve methods).
;;   Previously, parameters were only included in the OAuth signature but not
;;   actually sent in the request body, causing POST requests to fail.

;; Oauth requires the client application to receive user authorization in order
;; to access restricted content on behalf of the user. This allows for
;; authenticated communication without jeopardizing the user's password.
;; In order for an application to use oauth it needs a key and secret
;; issued by the service provider.

;; Usage:

;; Obtain access token:

;; The easiest way to obtain an access token is to call (oauth-authorize-app)
;; This will authorize the application and return an oauth-access-token.
;; You will use this token for all subsequent requests. In many cases
;; it will make sense to serialize this token and reuse it for future sessions.
;; At this time, that functionality is left to the application developers to
;; implement (see yammer.el for an example of token serialization).

;; Two helper functions are provided to handle authenticated requests:
;; (oauth-fetch-url) and (oauth-post-url)
;; Both take the access-token and a url.
;; Post takes an additional parameter post-vars-alist which is a
;; list of key val pairs to be used in a x-www-form-urlencoded message.

;; yammer.el:
;; http://github.com/psanford/emacs-yammer/tree/master is an example
;; mode that uses oauth.el

;; Dependencies:

;; The default behavior of oauth.el is to dispatch to curl for http
;; communication. It is strongly recommended that you use curl.
;; If curl is unavailable you can set oauth-use-curl to nil and oauth.el
;; will try to use the emacs internal http functions (url-request).
;; Note: if you plan on doing https and have oauth-use-curl set to nil,
;; make sure you have gnutls-bin installed.

;; oauth.el uses hmac-sha1 library for generating signatures. An implementation
;; by Derek Upham is included for convenience.

;; This library assumes that you are using the oauth_verifier method
;; described in the 1.0a spec.

;;; Code:

(require 'url)
(require 'url-util)
(require 'read-later-hmac-sha1)
(require 'hex-util)
(require 'cl-lib)

(defvar read-later-oauth-nonce-function nil
  "Fuction used to generate nonce.

Use (sasl-unique-id) if available otherwise read-later-oauth-internal-make-nonce")

(defvar read-later-oauth-hmac-sha1-param-reverse nil)

;; Initialize at load time, not just compile time
(require 'cl-lib)

;; Sad hack: There are two different implementations of hmac-sha1
;; One by Derek Upham (included with oauth),
;; and one by Shuhei KOBAYASHI (in the FLIM package).
;; Both functions work but they have different parameter orderings.
;; To deal with this we have this nice test to figure out which one
;; is actually available to us. Hopefully things will *just work*.
(when (equal
       (encode-hex-string (read-later-hmac-sha1 "Hi There" (make-string 20 ?\x0b)))
       "b617318655057264e28bc0b6fb378c8ef146be00")
  (setq read-later-oauth-hmac-sha1-param-reverse t))

;; Use sasl if available, otherwise make the nonce ourselves
(if (require 'sasl nil t)
    (setq read-later-oauth-nonce-function #'sasl-unique-id)
  (setq read-later-oauth-nonce-function #'read-later-oauth-internal-make-nonce))

(cl-defstruct read-later-oauth-request
  "Container for request information.

This includes both oauth header parameters as well as general
request information (url and http-method)."
  params ; alist
  token ; read-later-oauth-t
  url (http-method "GET"))

(cl-defstruct read-later-oauth-t
  "Token used for both Unauth Request Token (6.1.2) and Access Token (6.3.2)."
  token token-secret)

(cl-defstruct read-later-oauth-access-token
  consumer-key consumer-secret auth-t)

(defvar read-later-oauth-enable-browse-url t
  "Specifies whether or not to use call \='browse-url\' for authorizing apps.

Disabling is useful for remote machines.
Most of the time you will want this set to t.")

(defvar read-later-oauth-use-curl t
  "Specifies whether to use curl (external) or url-request (Emacs internal) for requests.

It is generally recomended that you use curl for your requests.")

(defvar read-later-oauth-curl-insecure t
  "Use the curl insecure flag (-k) which ignores ssl certificate errors.")

(defvar read-later-oauth-post-vars-alist nil
  "Alist containing key/vals for POSTing (x-www-form-urlencoded) requests.")

(defvar read-later-oauth-callback-url "oob"
  "Callback url for the server to redirect the client after the client authorizes the application.

This is mainly intended for web apps. Most client side apps will use 'oob' instead of a url.")

(defun read-later-oauth-authorize-app (consumer-key consumer-secret request-url access-url authorize-url)
  "Authorize application.

CONSUMER-KEY and CONSUMER-SECRET are the key and secret issued by the
service provider.

REQUEST-URL is the url to request an unauthorized token.
ACCESS-URL is the url to request an access token.
AUTHORIZE-URL is the url that oauth.el should redirect the user to once
it has recieved an unauthorized token.

This will fetch an unauthorized token, prompt the user to authorize this
application and the fetch the authorized token.

Returns an read-later-oauth-access-token if everything was successful."
  (let ((auth-t) (auth-req) (unauth-t) (auth-url) (access-token)
        (unauth-req (read-later-oauth-sign-request-hmac-sha1
                     (read-later-oauth-make-request request-url consumer-key)
                     consumer-secret)))
    (setq unauth-t (read-later-oauth-fetch-token unauth-req))
    (setq auth-url (format "%s?oauth_token=%s"
                           authorize-url (read-later-oauth-t-token unauth-t)))
    (if read-later-oauth-enable-browse-url (browse-url auth-url))
    (read-string (concat
                  "Please authorize this application by visiting: " auth-url
                  " \nPress enter once you have done so: "))
    (setq access-token (read-string
                        "Please enter the provided code: "))
    (setq auth-req
          (read-later-oauth-sign-request-hmac-sha1
           (read-later-oauth-make-request
            (concat access-url "?oauth_verifier=" access-token)
            consumer-key unauth-t)
           consumer-secret))
    (setq auth-t (read-later-oauth-fetch-token auth-req))
    (make-read-later-oauth-access-token :consumer-key consumer-key
                                        :consumer-secret consumer-secret
                                        :auth-t auth-t)))

(defun read-later-oauth-url-retrieve (access-token url &optional async-callback cb-data)
  "Like url retrieve, with url-request-extra-headers set to the necessary
oauth headers."
  (let ((req (read-later-oauth-make-request
              url
              (read-later-oauth-access-token-consumer-key access-token)
              (read-later-oauth-access-token-auth-t access-token))))
    (setf (read-later-oauth-request-http-method req) (or url-request-method "GET"))
    (when read-later-oauth-post-vars-alist
      (setf (read-later-oauth-request-params req)
            (append (read-later-oauth-request-params req) read-later-oauth-post-vars-alist)))
    (read-later-oauth-sign-request-hmac-sha1
     req (read-later-oauth-access-token-consumer-secret access-token))
    (let* ((url-request-extra-headers (if url-request-extra-headers
                                          (append url-request-extra-headers
                                                  (read-later-oauth-request-to-header req))
                                        (read-later-oauth-request-to-header req)))
           (url-request-method (read-later-oauth-request-http-method req))
           (url-request-data (when read-later-oauth-post-vars-alist
                               (mapconcat (lambda (pair)
                                            (concat (url-hexify-string (car pair)) "="
                                                    (url-hexify-string (cdr pair))))
                                          read-later-oauth-post-vars-alist
                                          "&"))))
      (cond
       (async-callback (url-retrieve (read-later-oauth-request-url req)
                                     async-callback cb-data))
       (read-later-oauth-use-curl (read-later-oauth-curl-retrieve (read-later-oauth-request-url req)))
       (t (url-retrieve-synchronously (read-later-oauth-request-url req)))))))

(defun read-later-oauth-fetch-url (access-token url)
  "Wrapper around url-retrieve-synchronously using the the authorized-token
to authenticate.

This is intended for simple get reqests.
Returns a buffer of the xresponse."
  (read-later-oauth-url-retrieve access-token url))

(defun read-later-oauth-post-url (access-token url post-vars-alist)
  "Wrapper around url-retrieve-synchronously using the the authorized-token
to authenticate.

This is intended for simple post reqests.
Returns a buffer of the response."
  (let ((url-request-method "POST")
        (read-later-oauth-post-vars-alist post-vars-alist))
    (read-later-oauth-url-retrieve access-token url)))

(defun read-later-oauth-epoch-string ()
  "Returns a unix epoch timestamp string"
  (format "%d" (ftruncate (float-time (current-time)))))

(defun read-later-oauth-make-nonce ()
  (funcall read-later-oauth-nonce-function))

(defun read-later-oauth-internal-make-nonce ()
  (number-to-string (random t)))

(defun read-later-oauth-make-request (url consumer-key &optional token)
  "Generates a read-later-oauth-request object with default values

Most consumers should call this function instead of creating
read-later-oauth-request objects directly"
  (make-read-later-oauth-request :url url
                                 :token token
                                 :params `(("oauth_consumer_key" . ,consumer-key)
                                           ("oauth_timestamp" . ,(read-later-oauth-epoch-string))
                                           ("oauth_nonce" . ,(read-later-oauth-make-nonce))
                                           ("oauth_callback" . ,read-later-oauth-callback-url)
                                           ("oauth_version" . "1.0"))))

;; HMAC-SHA1 specific code
(defun read-later-oauth-sign-request-hmac-sha1 (req secret)
  "Adds signature and signature_method to req.

This function is destructive"
  (let ((token (read-later-oauth-request-token req)))
    (push '("oauth_signature_method" . "HMAC-SHA1")
          (read-later-oauth-request-params req))
    (when token
      (push `("oauth_token" . ,(read-later-oauth-t-token token))
            (read-later-oauth-request-params req)))
    (push `("oauth_signature" . ,(read-later-oauth-build-signature-hmac-sha1 req secret))
          (read-later-oauth-request-params req)))
  req)

(defun read-later-oauth-build-signature-hmac-sha1 (req secret)
  "Returns the signature for the given request object"
  (let* ((token (read-later-oauth-request-token req))
         (key-parts (list secret "&" (when token (read-later-oauth-t-token-secret token))))
         ;; Ensure each part is unibyte before concatenating
         (key (apply 'concat
                     (mapcar (lambda (s)
                               (if (and s (multibyte-string-p s))
                                   (encode-coding-string s 'utf-8 t)
                                 (or s "")))
                             key-parts)))
         ;; Ensure key is unibyte
         (key-unibyte (if (multibyte-string-p key)
                          (string-as-unibyte (encode-coding-string key 'utf-8))
                        key))
         ;; Ensure message is unibyte
         (message-unibyte (string-as-unibyte
                           (encode-coding-string
                            (read-later-oauth-build-signature-basestring-hmac-sha1 req) 'utf-8)))
         (hmac-params (list key-unibyte message-unibyte)))
    (if read-later-oauth-hmac-sha1-param-reverse (setq hmac-params (reverse hmac-params)))
    (base64-encode-string (apply 'read-later-hmac-sha1 hmac-params))))

(defun read-later-oauth-build-signature-basestring-hmac-sha1 (req)
  "Returns the base string for the hmac-sha1 signing function"
  (let ((base-url (read-later-oauth-extract-base-url req))
        (params (append
                 (read-later-oauth-extract-url-params req)
                 (copy-sequence (read-later-oauth-request-params req)))))
    (concat
     (read-later-oauth-request-http-method req) "&"
     (read-later-oauth-hexify-string base-url) "&"
     (read-later-oauth-hexify-string
      (mapconcat
       (lambda (pair)
         (concat (car pair) "=" (read-later-oauth-hexify-string (cdr pair))))
       (sort params
             (lambda (a b) (string< (car a) (car b))))
       "&")))))

(defun read-later-oauth-extract-base-url (req)
  "Returns just the base url.

For example: http://example.com?param=1 returns http://example.com"
  (let ((url (read-later-oauth-request-url req)))
    (if (string-match "\\([^?]+\\)" url)
        (match-string 1 url)
      url)))

(defun read-later-oauth-extract-url-params (req)
  "Returns an alist of param name . param value from the url"
  (let ((url (read-later-oauth-request-url req)))
    (when (string-match (regexp-quote "?") url)
      (mapcar (lambda (pair)
                `(,(car pair) . ,(cadr pair)))
              (url-parse-query-string (substring url (match-end 0)))))))

(defun read-later-oauth-fetch-token (req)
  "Fetches a token based on the given request object"
  (let ((token (make-read-later-oauth-t)))
    (set-buffer (read-later-oauth-do-request req))
    (goto-char (point-min))
    (let ((linebreak (search-forward "\n\n" nil t nil)))
      (when linebreak
        (delete-region (point-min) linebreak)))
    (goto-char (point-max))
    (let ((line-end (search-backward "\r\n" nil t nil)))
      (when line-end
        (delete-region (point-min) (+ line-end 2))))
    (cl-loop for pair in (mapcar (lambda (str) (split-string str "="))
                                 (split-string
                                  (buffer-substring (point-min) (point-max)) "&"))
             do
             (cond
              ((equal (car pair) "oauth_token_secret")
               (setf (read-later-oauth-t-token-secret token) (cadr pair)))
              ((equal (car pair) "oauth_token")
               (setf (read-later-oauth-t-token token) (cadr pair)))))
    token))

(defun read-later-oauth-do-request (req)
  "Make an http request to url using the request object to generate the oauth
headers. Returns the http response buffer."
  (if read-later-oauth-use-curl (read-later-oauth-do-request-curl req)
    (read-later-oauth-do-request-emacs req)))

(defun read-later-oauth-do-request-emacs (req)
  "Make an http request to url using the request object to generate the oauth
headers. Returns the http response buffer.

This function uses the emacs function `url-retrieve' for the http connection."
  (let ((url-request-extra-headers (read-later-oauth-request-to-header req))
        (url-request-method (read-later-oauth-request-http-method req)))
    (url-retrieve-synchronously (read-later-oauth-request-url req))))

(defun read-later-oauth-do-request-curl (req)
  "Make an http request to url using the request object to generate the oauth
headers. Returns the http response buffer.

This function dispatches to an external curl process"

  (let ((url-request-extra-headers (read-later-oauth-request-to-header req))
        (url-request-method (read-later-oauth-request-http-method req)))
    (read-later-oauth-curl-retrieve (read-later-oauth-request-url req))))

(defun read-later-oauth-headers-to-curl (headers)
  "Converts header alist (like `url-request-extra-headers') to a string that
can be fed to curl"
  (apply
   'append
   (mapcar
    (lambda (header) `("--header"
                       ,(concat (car header) ": " (cdr header)))) headers)))

(defun read-later-oauth-curl-retrieve (url)
  "Retrieve via curl"
  (url-gc-dead-buffers)
  (set-buffer (generate-new-buffer " *read-later-oauth-request*"))
  (let ((curl-args `("-s" ,(when read-later-oauth-curl-insecure "-k")
                     "-X" ,url-request-method
                     "-i" ,url
                     ,@(when read-later-oauth-post-vars-alist
                         (apply
                          'append
                          (mapcar
                           (lambda (pair)
                             (list
                              "-d"
                              (concat (read-later-oauth-hexify-string (car pair)) "="
                                      (read-later-oauth-hexify-string (cdr pair)))))
                           read-later-oauth-post-vars-alist)))
                     ,@(read-later-oauth-headers-to-curl url-request-extra-headers))))
    (apply 'call-process "curl" nil t nil curl-args))
  (url-mark-buffer-as-dead (current-buffer))
  (current-buffer))

(defun read-later-oauth-request-to-header (req)
  "Given a requst will return a alist of header pairs. This can
be consumed by `url-request-extra-headers'."
  (let* ((params (copy-sequence (read-later-oauth-request-params req)))
         ;; Filter to only include oauth_* parameters in Authorization header
         (read-later-oauth-params (cl-remove-if-not
                                   (lambda (pair)
                                     (string-prefix-p "oauth_" (car pair)))
                                   params)))
    (cons
     (cons
      "Authorization"
      (apply 'concat "OAuth realm=\"\""
             (mapcar
              (lambda (pair)
                (format ", %s=\"%s\""
                        (car pair)
                        (read-later-oauth-hexify-string (cdr pair))))
              (sort read-later-oauth-params
                    (lambda (a b) (string< (car a) (car b))))))) '())))

(defconst read-later-oauth-unreserved-chars
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
    ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
    ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
    ?- ?_ ?. ?~ )
  "A list of characters that are _NOT_ reserved for oauth.")

(defun read-later-oauth-hexify-string (string)
  "Similar to hexify-string from `url-utils.el' except the hex
characters are upper case and the reserved char set is slightly different."
  (mapconcat (lambda (byte)
               (if (memq byte read-later-oauth-unreserved-chars)
                   (char-to-string byte)
                 (format "%%%02X" byte)))
             (if (multibyte-string-p string)
                 (encode-coding-string string 'utf-8)
               string)
             ""))

(provide 'read-later-oauth)

;;; read-later-oauth.el ends here
