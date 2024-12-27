;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "cider" "20241223.1320"
  "Clojure Interactive Development Environment that Rocks."
  '((emacs        "26")
    (clojure-mode "5.19")
    (parseedn     "1.2.1")
    (queue        "0.2")
    (spinner      "1.7")
    (seq          "2.22")
    (sesman       "0.3.2")
    (transient    "0.4.1"))
  :url "https://github.com/clojure-emacs/cider"
  :commit "430cfd40c57ad3ba4ef39262acf411531dc734f5"
  :revdesc "430cfd40c57a"
  :keywords '("languages" "clojure" "cider")
  :authors '(("Tim King" . "kingtim@gmail.com")
             ("Phil Hagelberg" . "technomancy@gmail.com")
             ("Bozhidar Batsov" . "bozhidar@batsov.dev")
             ("Artur Malabarba" . "bruce.connor.am@gmail.com")
             ("Hugo Duncan" . "hugo@hugoduncan.org")
             ("Steve Purcell" . "steve@sanityinc.com"))
  :maintainers '(("Bozhidar Batsov" . "bozhidar@batsov.dev")))
