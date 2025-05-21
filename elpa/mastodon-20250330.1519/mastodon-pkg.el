;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "mastodon" "20250330.1519"
  "Client for fediverse services using the Mastodon API."
  '((emacs   "28.1")
    (persist "0.4")
    (tp      "0.7"))
  :url "https://codeberg.org/martianh/mastodon.el"
  :commit "163ba2b0b89a292b99bff0f574f5584ec888469a"
  :revdesc "163ba2b0b89a"
  :authors '(("Johnson Denen" . "johnson.denen@gmail.com")
             ("Marty Hiatt" . "mousebot@disroot.org"))
  :maintainers '(("Marty Hiatt" . "mousebot@disroot.org")))
