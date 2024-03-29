;;; helm-gtags-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "helm-gtags" "helm-gtags.el" (0 0 0 0))
;;; Generated autoloads from helm-gtags.el

(autoload 'helm-gtags-clear-all-cache "helm-gtags" "\
Not documented." t nil)

(autoload 'helm-gtags-clear-cache "helm-gtags" "\
Not documented." t nil)

(autoload 'helm-gtags-next-history "helm-gtags" "\
Jump to next position on context stack" t nil)

(autoload 'helm-gtags-previous-history "helm-gtags" "\
Jump to previous position on context stack" t nil)

(autoload 'helm-gtags-select "helm-gtags" "\
Not documented." t nil)

(autoload 'helm-gtags-select-path "helm-gtags" "\
Not documented." t nil)

(autoload 'helm-gtags-tags-in-this-function "helm-gtags" "\
Show tagnames which are referenced in this function and jump to it." t nil)

(autoload 'helm-gtags-create-tags "helm-gtags" "\
Not documented.

\(fn DIR LABEL)" t nil)

(autoload 'helm-gtags-delete-tags "helm-gtags" "\
Delete file GTAGS, GRTAGS, GPATH, ID etc. generated by gtags." t nil)

(autoload 'helm-gtags-find-tag "helm-gtags" "\
Jump to definition

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-tag-other-window "helm-gtags" "\
Jump to definition in other window.

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-rtag "helm-gtags" "\
Jump to referenced point

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-symbol "helm-gtags" "\
Jump to the symbol location

\(fn TAG)" t nil)

(autoload 'helm-gtags-find-pattern "helm-gtags" "\
Grep and jump by gtags tag files.

\(fn PATTERN)" t nil)

(autoload 'helm-gtags-find-files "helm-gtags" "\
Find file from tagged with gnu global.

\(fn FILE)" t nil)

(autoload 'helm-gtags-find-tag-from-here "helm-gtags" "\
Jump point by current point information.
Jump to definition point if cursor is on its reference.
Jump to reference point if curosr is on its definition" t nil)

(autoload 'helm-gtags-dwim "helm-gtags" "\
Find by context. Here is
- on include statement then jump to included file
- on symbol definition then jump to its references
- on reference point then jump to its definition." t nil)

(autoload 'helm-gtags-parse-file "helm-gtags" "\
Parse current file with gnu global. This is similar to `imenu'.
You can jump definitions of functions, symbols in this file." t nil)

(autoload 'helm-gtags-push-stack "helm-gtags" "\
Push current location to the stack." t nil)

(autoload 'helm-gtags-pop-stack "helm-gtags" "\
Jump to previous point on the context stack and pop it from stack." t nil)

(autoload 'helm-gtags-show-stack "helm-gtags" "\
Show current context stack." t nil)

(autoload 'helm-gtags-clear-stack "helm-gtags" "\
Clear current context stack." t nil)

(autoload 'helm-gtags-clear-all-stacks "helm-gtags" "\
Clear all context stacks." t nil)

(autoload 'helm-gtags-update-tags "helm-gtags" "\
Update TAG file. Update All files with `C-u' prefix.
Generate new TAG file in selected directory with `C-u C-u'" t nil)

(autoload 'helm-gtags-resume "helm-gtags" "\
Resurrect previously invoked `helm-gtags` command." t nil)

(autoload 'helm-gtags-mode "helm-gtags" "\
Toggle Helm-Gtags mode on or off.

This is a minor mode.  If called interactively, toggle the
`Helm-Gtags mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `helm-gtags-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{helm-gtags-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "helm-gtags" '("helm-"))

;;;***

;;;### (autoloads nil nil ("helm-gtags-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-gtags-autoloads.el ends here
