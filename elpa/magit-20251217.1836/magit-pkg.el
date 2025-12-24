;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20251217.1836"
  "A Git porcelain inside Emacs."
  '((emacs         "28.1")
    (compat        "30.1")
    (cond-let      "0.1")
    (llama         "1.0")
    (magit-section "4.4")
    (seq           "2.24")
    (transient     "0.10")
    (with-editor   "3.4"))
  :url "https://github.com/magit/magit"
  :commit "655bc502a3bdd7f07928524515a736e4b8101eaf"
  :revdesc "655bc502a3bd"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
