;;; read-later-utils.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Devin Davis
;;
;; Author: Devin Davis <devin@devdeveloper.ca>
;; Maintainer: Devin Davis <devin@devdeveloper.ca>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'read-later-api)

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

(provide 'read-later-utils)
;;; read-later-utils.el ends here
