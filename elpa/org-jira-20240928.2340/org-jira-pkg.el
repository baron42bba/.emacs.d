;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-jira" "20240928.2340"
  "Syncing between Jira and Org-mode."
  '((emacs   "24.5")
    (cl-lib  "0.5")
    (request "0.2.0")
    (dash    "2.14.1"))
  :url "https://github.com/ahungry/org-jira"
  :commit "5f591f5f4abd0ef12c64676e38d8ec3b13eba280"
  :revdesc "5f591f5f4abd"
  :keywords '("ahungry" "jira" "org" "bug" "tracker")
  :maintainers '(("Matthew Carter" . "m@ahungry.com")))
