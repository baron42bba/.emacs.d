;;; kubel-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kubel" "kubel.el" (0 0 0 0))
;;; Generated autoloads from kubel.el

(autoload 'kubel-vterm-setup "kubel" "\
Adds a vterm enty to the KUBEL-EXEC-POP." nil nil)

(autoload 'kubel-open "kubel" "\
Open kubel pointing to CONTEXT and NAMESPACE.

NAMEPACE is optional, will default to \"default\".
RESOURCE is optional, will default to pods.
DIRECTORY is optional for TRAMP support.

\(fn CONTEXT &optional NAMESPACE RESOURCE DIRECTORY)" nil nil)

(autoload 'kubel "kubel" "\
Invoke the kubel buffer.

DIRECTORY is optional for TRAMP support.

\(fn &optional DIRECTORY)" t nil)

(register-definition-prefixes "kubel" '("kubel-"))

;;;***

;;;### (autoloads nil nil ("kubel-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kubel-autoloads.el ends here
