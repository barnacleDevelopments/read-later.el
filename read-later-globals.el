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

;; Keymap

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

(defvar read-later-folder read-later-default-folder
  "Current active folder for filtering.")

(defun read-later-reset-folders ()
  "Reset active folders to defaults."
  (setq read-later-folder read-later-default-folder))

(defcustom read-later-default-tag nil
  "The default tags applied to BOOKMARKS query.
Only filters if folder is not specified."
  :type 'string
  :group 'read-later)

(defvar read-later-tag read-later-default-tag
  "Current active tags for filtering.")

(defun read-later-reset-tags ()
  "Reset active tags to defaults."
  (setq read-later-tag read-later-default-tag))

(provide 'read-later-globals)
;;; read-later-globals.el ends here
