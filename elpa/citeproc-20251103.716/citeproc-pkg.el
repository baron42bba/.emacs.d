;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "citeproc" "20251103.716"
  "A CSL 1.0.2 Citation Processor."
  '((emacs             "26")
    (dash              "2.13.0")
    (s                 "1.12.0")
    (f                 "0.18.0")
    (queue             "0.2")
    (string-inflection "1.0")
    (org               "9")
    (parsebib          "2.4")
    (compat            "28.1"))
  :url "https://github.com/andras-simonyi/citeproc-el"
  :commit "a3d62ab8e40a75fcfc6e4c0c107e3137b4db6db8"
  :revdesc "a3d62ab8e40a"
  :keywords '("bib")
  :authors '(("András Simonyi" . "andras.simonyi@gmail.com"))
  :maintainers '(("András Simonyi" . "andras.simonyi@gmail.com")))
