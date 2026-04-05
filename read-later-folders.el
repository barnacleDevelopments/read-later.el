;;; read-later-folders.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Devin Davis
;;
;; Author: Devin Davis <devin@devdeveloper.ca>
;; Maintainer: Devin Davis <devin@devdeveloper.ca>

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'read-later-globals)

(defun read-later--prompt-folder-select (folders)
  "Prompt folder select from list of FOLDERS."
  (let* ((folder-titles (append (mapcar (lambda (item) (plist-get item :title)) folders) read-later-default-folders))
         (selected-title (completing-read "Choose a folder (default unread): " folder-titles nil t nil nil "Unread"))
         (is-default-folder (seq-some (lambda (title) (string= title selected-title)) read-later-default-folders)))
    (cond ((equal selected-title "Unread")
           (message "No folder selected, defaulting to unread.")
           nil)
          (is-default-folder selected-title)
          (t (let* ((selected-folder (seq-find (lambda (item) (equal (plist-get item :title) selected-title)) folders))
                    (selected-folder-id (number-to-string (plist-get selected-folder :folder_id))))
               selected-folder-id)))))


(provide 'read-later-folders)
;;; read-later-folders.el ends here
