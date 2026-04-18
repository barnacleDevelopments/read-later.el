;;; read-later-globals.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Devin Davis
;;
;; Author: Devin Davis <devindavis@pop-os>
;; Maintainer: Devin Davis <devindavis@pop-os>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(defvar-local read-later-mode-buffer-name "*Read-Later Bookmarks*")

(defvar-local read-later--bookmarks-data nil
  "List of bookmark plists for the current buffer.")

;; Customize Variables
(defcustom read-later-update-limit 25
  "The Instapaper API limit query parameter value."
  :type 'integer
  :group 'read-later)

(defcustom read-later-append-limit 50
  "The Instapaper API limit query parameter value when loading more."
  :type 'integer
  :group 'read-later)

(defcustom read-later-default-folder "unread"
  "The default folders applied to BOOKMARKS query."
  :type 'string
  :group 'read-later)

(defconst read-later-default-folders '(unread archive starred) "Default folders of Instapaper.")

(defvar-local read-later-folder read-later-default-folder
  "Current active folder for filtering.")

(defcustom read-later-default-tag nil
  "The default tags applied to BOOKMARKS query.
Only filters if folder is not specified."
  :type 'string
  :group 'read-later)

(defvar-local read-later-tag read-later-default-tag
  "Current active tags for filtering.")

(provide 'read-later-globals)
;;; read-later-globals.el ends here
