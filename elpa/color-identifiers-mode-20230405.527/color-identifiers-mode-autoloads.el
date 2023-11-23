;;; color-identifiers-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "color-identifiers-mode" "color-identifiers-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from color-identifiers-mode.el

(autoload 'color-identifiers-mode "color-identifiers-mode" "\
Color the identifiers in the current buffer based on their names.

This is a minor mode.  If called interactively, toggle the
`Color-Identifiers mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `color-identifiers-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-color-identifiers-mode 'globalized-minor-mode t)

(defvar global-color-identifiers-mode nil "\
Non-nil if Global Color-Identifiers mode is enabled.
See the `global-color-identifiers-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-color-identifiers-mode'.")

(custom-autoload 'global-color-identifiers-mode "color-identifiers-mode" nil)

(autoload 'global-color-identifiers-mode "color-identifiers-mode" "\
Toggle Color-Identifiers mode in all buffers.
With prefix ARG, enable Global Color-Identifiers mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Color-Identifiers mode is enabled in all buffers where
`color-identifiers-mode-maybe' would do it.

See `color-identifiers-mode' for more information on Color-Identifiers
mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "color-identifiers-mode" '("color-identifiers"))

;;;***

;;;### (autoloads nil nil ("color-identifiers-mode-pkg.el") (0 0
;;;;;;  0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; color-identifiers-mode-autoloads.el ends here
