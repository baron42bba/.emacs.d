;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20241218.2036"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.0.0.0")
    (closql        "2.1.0")
    (dash          "2.19.1")
    (emacsql       "4.1.0")
    (ghub          "4.2.0")
    (let-alist     "1.0.6")
    (magit         "4.1.3")
    (markdown-mode "2.6")
    (seq           "2.24")
    (transient     "0.8.0")
    (yaml          "0.5.5"))
  :url "https://github.com/magit/forge"
  :commit "3b80ace32c23c030d8c12dd9bf266962718545cf"
  :revdesc "3b80ace32c23"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
