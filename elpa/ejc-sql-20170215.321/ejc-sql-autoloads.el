;;; ejc-sql-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "ejc-autocomplete" "ejc-autocomplete.el" (22692
;;;;;;  30293 0 0))
;;; Generated autoloads from ejc-autocomplete.el

(autoload 'ejc-candidates "ejc-autocomplete" "\
Possible completions list according to already typed prefixes.

\(fn)" nil nil)

(autoload 'ejc-ac-setup "ejc-autocomplete" "\
Add the completion sources to the front of `ac-sources'.
This affects only the current buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ejc-sql" "ejc-sql.el" (22692 30293 0 0))
;;; Generated autoloads from ejc-sql.el

(autoload 'ejc-sql-mode "ejc-sql" "\
Toggle ejc-sql mode.

\(fn &optional ARG)" t nil)

(autoload 'ejc-create-menu "ejc-sql" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("ejc-format.el" "ejc-interaction.el" "ejc-lib.el"
;;;;;;  "ejc-result-mode.el" "ejc-sql-pkg.el") (22692 30293 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ejc-sql-autoloads.el ends here
