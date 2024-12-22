;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "sx" "20240126.2120"
  "StackExchange client. Ask and answer questions on Stack Overflow, Super User, and the likes."
  '((emacs         "24.1")
    (cl-lib        "0.5")
    (json          "1.3")
    (markdown-mode "2.0")
    (let-alist     "1.0.3"))
  :url "https://github.com/vermiculus/sx.el"
  :commit "8c1c28f33d714fc8869e49f5642e1a585c8c85af"
  :revdesc "8c1c28f33d71"
  :keywords '("help" "hypermedia" "tools")
  :authors '(("Sean Allred" . "code@seanallred.com"))
  :maintainers '(("Sean Allred" . "code@seanallred.com")))
