;;; ox-reveal-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ox-reveal" "ox-reveal.el" (0 0 0 0))
;;; Generated autoloads from ox-reveal.el

(autoload 'org-reveal-publish-to-reveal "ox-reveal" "\
Publish an org file to Html.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

(register-definition-prefixes "ox-reveal" '("frag-" "if-format" "org-reveal-"))

;;;***

;;;### (autoloads nil nil ("ox-reveal-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ox-reveal-autoloads.el ends here
