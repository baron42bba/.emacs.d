;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "elfeed-org" "20231009.1125"
  "Configure elfeed with one or more org-mode files."
  '((elfeed "1.1.1")
    (org    "8.2.7")
    (cl-lib "0.5"))
  :url "https://github.com/remyhonig/elfeed-org"
  :commit "fe59a96969bd321f5f9ec7317a4bc80943b94c86"
  :revdesc "fe59a96969bd"
  :keywords '("news")
  :authors '(("Remy Honig" . "remyhonig@gmail.com"))
  :maintainers '(("Remy Honig" . "remyhonig@gmail.com")))
