;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "kubel" "20250507.1654"
  "Control Kubernetes with limited permissions."
  '((transient "0.1.0")
    (emacs     "25.3")
    (dash      "2.12.0")
    (s         "1.2.0")
    (yaml-mode "0.0.14"))
  :url "https://github.com/abrochard/kubel"
  :commit "61ec610b817c9ef59b2f25a242b71dee3f2e068a"
  :revdesc "61ec610b817c"
  :keywords '("kubernetes" "k8s" "tools" "processes"))
