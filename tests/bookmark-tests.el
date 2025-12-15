;;; bookmark-tests.el --- Bookmark tests -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Test cases for read-later.el
;;
;;; Code:
(ert-deftest read-later-test-format-progress ()
  "Tests that floating point number is converted to percentage."
  (should (equal (read-later--format-progress 0.5) "50%")))

(ert-deftest read-later-test-format-tags ()
  "Tests that provided alist of plist is converted to comma seperated list."
  (should (equal (read-later--format-tags '((:name "web")(:name "programming"))) "web  programming")))

(ert-deftest read-later-test-format-bookmarks ()
  (should (equal (read-later--format-bookmarks 
                  '((:bookmark_id 1945647911
                     :title "Exploring the depths of the ocean"
                     :progress 0.5
                     :tags ((:name "web") (:name "programming"))
                     :description "What does it take to explore the depths of the ocean.")))
                 '((1945647911
                    ["Exploring the depths of the ocean" 
                     "50%" 
                     "web  programming" 
                     "What does it take to explore the depths of the ocean."])))))

(provide 'bookmark-tests)
;;; bookmark-tests.el ends here
