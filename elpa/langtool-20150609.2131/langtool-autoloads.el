;;; langtool-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "langtool" "langtool.el" (21908 14228 0 0))
;;; Generated autoloads from langtool.el

(defalias 'langtool-check 'langtool-check-buffer)

(autoload 'langtool-check-buffer "langtool" "\
Check context current buffer and light up errors.
Optional \\[universal-argument] read LANG name.

You can change the `langtool-default-language' to apply all session.
Restrict to selection when region is activated.

\(fn &optional LANG)" t nil)

(autoload 'langtool-switch-default-language "langtool" "\
Switch `langtool-read-lang-name' to LANG

\(fn LANG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; langtool-autoloads.el ends here
