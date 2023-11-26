;;; tldr-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tldr" "tldr.el" (0 0 0 0))
;;; Generated autoloads from tldr.el

(autoload 'tldr-update-docs "tldr" "\
Get or update the TLDR docs from source." t nil)

(autoload 'tldr "tldr" "\
Lookup TLDR docs.

\(fn &optional CMD)" t nil)

(autoload 'helm-tldr "tldr" "\
Helm interface for `tldr'." t nil)

(register-definition-prefixes "tldr" '("tldr-"))

;;;***

;;;### (autoloads nil nil ("tldr-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tldr-autoloads.el ends here
