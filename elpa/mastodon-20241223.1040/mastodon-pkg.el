;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "mastodon" "20241223.1040"
  "Client for fediverse services using the Mastodon API."
  '((emacs   "28.1")
    (request "0.3.0")
    (persist "0.4")
    (tp      "0.6"))
  :url "https://codeberg.org/martianh/mastodon.el"
  :commit "e2443f1cd425b31228e87739d1fc5035640bba06"
  :revdesc "e2443f1cd425"
  :authors '(("Johnson Denen" . "johnson.denen@gmail.com")
             ("Marty Hiatt" . "mousebot@disroot.org"))
  :maintainers '(("Marty Hiatt" . "mousebot@disroot.org")))
