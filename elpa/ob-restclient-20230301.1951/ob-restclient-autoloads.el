;;; ob-restclient-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ob-restclient" "ob-restclient.el" (0 0 0 0))
;;; Generated autoloads from ob-restclient.el

(autoload 'org-babel-execute:restclient "ob-restclient" "\
Execute a block of Restclient code with org-babel.
This function is called by `org-babel-execute-src-block'

\(fn BODY PARAMS)" nil nil)

(autoload 'org-babel-variable-assignments:restclient "ob-restclient" "\
Return a list of restclient statements assigning the block's variables specified in PARAMS.

\(fn PARAMS)" nil nil)

(register-definition-prefixes "ob-restclient" '("org-babel-"))

;;;***

;;;### (autoloads nil nil ("ob-restclient-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ob-restclient-autoloads.el ends here
