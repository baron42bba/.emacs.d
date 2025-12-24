;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "mastodon" "20251201.1553"
  "Client for fediverse services using the Mastodon API."
  '((emacs   "28.1")
    (persist "0.8")
    (tp      "0.7"))
  :url "https://codeberg.org/martianh/mastodon.el"
  :commit "3c00418bfbb13f450551c28a97f8870e8ce3fef9"
  :revdesc "3c00418bfbb1"
  :authors '(("Johnson Denen" . "johnson.denen@gmail.com")
             ("Marty Hiatt" . "mousebot@disroot.org"))
  :maintainers '(("Marty Hiatt" . "mousebot@disroot.org")))
