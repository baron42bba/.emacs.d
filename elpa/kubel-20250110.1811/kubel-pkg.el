;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "kubel" "20250110.1811"
  "Control Kubernetes with limited permissions."
  '((transient "0.1.0")
    (emacs     "25.3")
    (dash      "2.12.0")
    (s         "1.2.0")
    (yaml-mode "0.0.14"))
  :url "https://github.com/abrochard/kubel"
  :commit "d587d6a09faa4add084847643821ec1aa60882d5"
  :revdesc "d587d6a09faa"
  :keywords '("kubernetes" "k8s" "tools" "processes"))
