;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "ox-pandoc" "20240710.1424"
  "An Org-mode exporter using pandoc."
  '((org   "8.2")
    (emacs "24.4")
    (dash  "2.8")
    (ht    "2.0"))
  :url "https://github.com/emacsorphanage/ox-pandoc"
  :commit "34e6ea97b586e20529d07158a73af3cf33cdd1d5"
  :revdesc "34e6ea97b586"
  :keywords '("tools")
  :authors '(("Taichi" . "kawabata.taichi@gmail.com")
             ("Alex" . "a-fent@github"))
  :maintainers '(("Alex" . "a-fent@github")))
