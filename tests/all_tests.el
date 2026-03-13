;;; all-tests.el --- Load all test files -*- lexical-binding: t; -*-

(dolist (file (directory-files
               (file-name-directory load-file-name) t "_test\\.el\\'"))
  (load file nil t))

;;; all-tests.el ends here
