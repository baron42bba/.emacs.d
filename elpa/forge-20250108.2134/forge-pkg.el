;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20250108.2134"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.0.1.0")
    (closql        "2.1.0")
    (dash          "2.19.1")
    (emacsql       "4.1.0")
    (ghub          "4.2.0")
    (let-alist     "1.0.6")
    (magit         "4.2.1")
    (markdown-mode "2.6")
    (seq           "2.24")
    (transient     "0.8.2")
    (yaml          "0.5.5"))
  :url "https://github.com/magit/forge"
  :commit "96fe98120c492f698088126b5340edc1976e7508"
  :revdesc "96fe98120c49"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
