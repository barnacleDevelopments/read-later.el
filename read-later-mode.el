;;; read-later-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Devin Davis
;;
;; Author: Devin Davis <devindavis@pop-os>
;; Maintainer: Devin Davis <devindavis@pop-os>
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar read-later-map (make-sparse-keymap)
  "Read later keymap.")

(defvar read-later-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" 'read-later-update)
    (define-key map "d" 'read-later-delete-bookmark-at-point)
    (define-key map "l" 'read-later-load-more)
    map)
  "Keymap for read-later-mode.")

(define-derived-mode read-later-mode tabulated-list-mode "Read-Later Bookmarks"
  "Major mode for viewing Instapaper bookmarks."
  (setq tabulated-list-format [("Title" 50 t)
                               ("Progress" 10 t)
                               ("Tags" 30 t)
                               ("Description" 100 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun read-later--create-bookmarks-buffer (bookmarks)
  "Create the bookmarks buffer with BOOKMARKS data."
  (with-current-buffer (get-buffer-create "*Read-Later Bookmarks*")
    (read-later-mode)
    (setq read-later--bookmarks-data bookmarks)
    (setq tabulated-list-entries (read-later--create-bookmark-entries bookmarks))
    (tabulated-list-print t)
    (current-buffer)))

(provide 'read-later-mode)
;;; read-later-mode.el ends here
