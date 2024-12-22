;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "mastodon" "20241202.1839"
  "Client for fediverse services using the Mastodon API."
  '((emacs   "28.1")
    (request "0.3.0")
    (persist "0.4")
    (tp      "0.6"))
  :url "https://codeberg.org/martianh/mastodon.el"
  :commit "10be1f9c2845a6012cee84b68dc566ea92df40a6"
  :revdesc "10be1f9c2845"
  :authors '(("Johnson Denen" . "johnson.denen@gmail.com")
             ("Marty Hiatt" . "mousebot@disroot.org"))
  :maintainers '(("Marty Hiatt" . "mousebot@disroot.org")))
