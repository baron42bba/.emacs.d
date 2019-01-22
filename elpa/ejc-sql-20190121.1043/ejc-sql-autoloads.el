;;; ejc-sql-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ejc-autocomplete" "ejc-autocomplete.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ejc-autocomplete.el

(autoload 'ejc-owners-candidates "ejc-autocomplete" "\


\(fn)" nil nil)

(autoload 'ejc-tables-candidates "ejc-autocomplete" "\


\(fn)" nil nil)

(autoload 'ejc-colomns-candidates "ejc-autocomplete" "\


\(fn)" nil nil)

(autoload 'ejc-ac-setup "ejc-autocomplete" "\
Add the completion sources to the front of `ac-sources'.
This affects only the current buffer.

Check against following cases:
prefix-2.prefix-1.#
prefix-1.#
something#

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ejc-autocomplete" '("ejc-" "ac-")))

;;;***

;;;### (autoloads nil "ejc-direx" "ejc-direx.el" (0 0 0 0))
;;; Generated autoloads from ejc-direx.el

(autoload 'ejc-direx:pop-to-buffer "ejc-direx" "\


\(fn)" t nil)

(autoload 'ejc-direx:switch-to-buffer "ejc-direx" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ejc-direx" '("ejc-direx:" "direx")))

;;;***

;;;### (autoloads nil "ejc-doc" "ejc-doc.el" (0 0 0 0))
;;; Generated autoloads from ejc-doc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ejc-doc" '("ejc-")))

;;;***

;;;### (autoloads nil "ejc-flx" "ejc-flx.el" (0 0 0 0))
;;; Generated autoloads from ejc-flx.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ejc-flx" '("ejc-")))

;;;***

;;;### (autoloads nil "ejc-format" "ejc-format.el" (0 0 0 0))
;;; Generated autoloads from ejc-format.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ejc-format" '("ejc-")))

;;;***

;;;### (autoloads nil "ejc-interaction" "ejc-interaction.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ejc-interaction.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ejc-interaction" '("ejc-")))

;;;***

;;;### (autoloads nil "ejc-lib" "ejc-lib.el" (0 0 0 0))
;;; Generated autoloads from ejc-lib.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ejc-lib" '("ejc-")))

;;;***

;;;### (autoloads nil "ejc-result-mode" "ejc-result-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ejc-result-mode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ejc-result-mode" '("ejc-result-")))

;;;***

;;;### (autoloads nil "ejc-sql" "ejc-sql.el" (0 0 0 0))
;;; Generated autoloads from ejc-sql.el

(autoload 'ejc-sql-mode "ejc-sql" "\
Toggle ejc-sql mode.

\(fn &optional ARG)" t nil)

(autoload 'ejc-create-menu "ejc-sql" "\


\(fn)" nil nil)

(autoload 'ejc-connect "ejc-sql" "\
Connect to selected db.

\(fn CONNECTION-NAME)" t nil)

(autoload 'ejc-connect-existing-repl "ejc-sql" "\
Connect to existing ejc-sql nREPL running process.
You can `cd` to your ejc-sql project folder (typically
'~/.emacs.d/elpa/ejc-sql-<version>') and launch nREPL via `lein repl`.
Then run in Emacs `ejc-connect-existing-repl', type HOST and PORT
from your `lein run` console output. Finally, use `ejc-connect' from
any SQL buffer to connect to exact database, as always. 

\(fn)" t nil)

(autoload 'ejc-version "ejc-sql" "\
Get the ejc-sql version as string.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ejc-sql" '("ejc-")))

;;;***

;;;### (autoloads nil nil ("ejc-sql-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ejc-sql-autoloads.el ends here
