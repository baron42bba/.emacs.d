;;; outline-minor-faces-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "outline-minor-faces" "outline-minor-faces.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from outline-minor-faces.el

(autoload 'outline-minor-faces-mode "outline-minor-faces" "\
Minor mode that adds heading faces for `outline-minor-mode'.

This is a minor mode.  If called interactively, toggle the
`Outline minor-Faces mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `outline-minor-faces-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "outline-minor-faces" '("outline-minor-faces"))

;;;***

;;;### (autoloads nil nil ("outline-minor-faces-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; outline-minor-faces-autoloads.el ends here
