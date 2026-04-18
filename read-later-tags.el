;;; read-later-tags.el --- Description -*- lexical-binding: t; -*-
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
(require 'read-later-globals)

(defun read-later-reset-tags ()
  "Reset active tags to defaults."
  (setq read-later-tag read-later-default-tag))

(defun read-later-get-tags ()
  "Use the current buffer to collect available tags.
This is not an exaustive list just what is visible in
the buffer because instapaper API does not have a tag listing endpoint."
  (seq-filter (lambda (tag-list) tag-list)
              (apply #'append (seq-map (lambda (bookmark)
                                         (message "Bookmark: %S" bookmark)
                                         (plist-get bookmark :tags)) read-later--bookmarks-data))))

(defun read-later--prompt-tag-search ()
  "Prompt for tag."
  (let* ((tags (read-later-get-tags))
         (tag-titles (seq-map (lambda(tag) (plist-get tag :name)) tags)))
    (completing-read "Search tag: " tag-titles)))

(provide 'read-later-tags)
;;; read-later-tags.el ends here
