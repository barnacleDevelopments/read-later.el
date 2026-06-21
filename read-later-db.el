;;; read-later-db.el --- SQLite database for read-later.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Devin Davis
;;
;; Author: Devin Davis <devindavis@pop-os>
;; Maintainer: Devin Davis <devindavis@pop-os>
;; Created: November 06, 2025
;; Version: 2.2.0
;; Keywords: database tools
;; Homepage: https://github.com/barnacleDevelopments/read-later.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module handles all SQLite database operations for read-later.el.
;;
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
;;
;;; Code:

;;; read-later-db.el --- Database layer for read-later -*- lexical-binding: t; -*-

(require 'emacsql)
(require 'emacsql-sqlite-builtin)
(require 'json)
(require 'read-later-globals)

(defvar read-later-db--connection nil
  "The database connection for read-later.")

(defun read-later-db--connect ()
  "Establish connection to the read-later database, initializing it if needed."
  (unless (and read-later-db--connection (emacsql-live-p read-later-db--connection))
    (setq read-later-db--connection (emacsql-sqlite-builtin read-later-db-file))
    (read-later-db--init-schema)))

(defun read-later-db--init-schema ()
  "Create the bookmarks table if it doesn't already exist.
Called automatically by `read-later-db--connect' on a fresh connection."
  (emacsql read-later-db--connection
           [:create-table :if-not-exists bookmarks
                          ([(bookmark_id integer :primary-key) (data text)])]))

;;;###autoload
(defun read-later-db-init ()
  "Initialize the database and create tables if they don't exist.
Other functions in this package call `read-later-db--connect' themselves,
so calling this explicitly first is optional."
  (interactive)
  (read-later-db--connect)
  (message "Read-later database initialized."))

(defun read-later-db-upsert-bookmarks (bookmarks)
  "Upsert a list of BOOKMARKS into the database.
BOOKMARKS is a list of plists."
  (when bookmarks
    (read-later-db--connect)
    (emacsql-with-transaction read-later-db--connection
      (dolist (bookmark bookmarks)
        (emacsql read-later-db--connection
                 [:insert-or-replace-into bookmarks
                  [bookmark_id data]
                  :values $v1]
                 (vector (plist-get bookmark :bookmark_id)
                         (json-encode bookmark)))))))

(defun read-later-db-get-all-bookmarks ()
  "Get all bookmarks from the database."
  (condition-case nil
      (progn
        (read-later-db--connect)
        (let ((results (emacsql read-later-db--connection
                                [:select data :from bookmarks])))
          (mapcar (lambda (row)
                    (let ((json-object-type 'plist))
                      (json-read-from-string (car row))))
                  results)))
    (error nil)))

(defun read-later-db-delete-bookmark (bookmark-id)
  "Delete bookmark with BOOKMARK-ID from database."
  (read-later-db--connect)
  (emacsql read-later-db--connection
           [:delete-from bookmarks :where (= bookmark_id $s1)]
           bookmark-id))

(defun read-later-db-clear-bookmarks ()
  "Delete all bookmarks from the database."
  (read-later-db--connect)
  (emacsql read-later-db--connection
           [:delete-from bookmarks]))

(provide 'read-later-db)
;;; read-later-db.el ends here
