;;; ejc-sql-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ejc-autocomplete" "ejc-autocomplete.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ejc-autocomplete.el

(autoload 'ejc-ac-setup "ejc-autocomplete" "\
Add the completion sources to the front of `ac-sources'.
This affects only the current buffer.

Check against following cases:
prefix-2.prefix-1.#
prefix-1.#
something#" t nil)

(register-definition-prefixes "ejc-autocomplete" '("ac-source-ejc-" "ejc-ac-hook"))

;;;***

;;;### (autoloads nil "ejc-company" "ejc-company.el" (0 0 0 0))
;;; Generated autoloads from ejc-company.el

(register-definition-prefixes "ejc-company" '("ejc-company-"))

;;;***

;;;### (autoloads nil "ejc-completion-common" "ejc-completion-common.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ejc-completion-common.el

(autoload 'ejc-owners-candidates "ejc-completion-common" nil nil nil)

(autoload 'ejc-tables-candidates "ejc-completion-common" nil nil nil)

(autoload 'ejc-views-candidates "ejc-completion-common" nil nil nil)

(autoload 'ejc-packages-candidates "ejc-completion-common" nil nil nil)

(autoload 'ejc-colomns-candidates "ejc-completion-common" nil nil nil)

(register-definition-prefixes "ejc-completion-common" '("ac-ejc-documentation" "ejc-"))

;;;***

;;;### (autoloads nil "ejc-doc" "ejc-doc.el" (0 0 0 0))
;;; Generated autoloads from ejc-doc.el

(register-definition-prefixes "ejc-doc" '("ejc-"))

;;;***

;;;### (autoloads nil "ejc-eldoc" "ejc-eldoc.el" (0 0 0 0))
;;; Generated autoloads from ejc-eldoc.el

(autoload 'ejc-eldoc-setup "ejc-eldoc" "\
Set up eldoc function and enable eldoc-mode." t nil)

(register-definition-prefixes "ejc-eldoc" '("ejc-"))

;;;***

;;;### (autoloads nil "ejc-flx" "ejc-flx.el" (0 0 0 0))
;;; Generated autoloads from ejc-flx.el

(register-definition-prefixes "ejc-flx" '("ejc-"))

;;;***

;;;### (autoloads nil "ejc-format" "ejc-format.el" (0 0 0 0))
;;; Generated autoloads from ejc-format.el

(register-definition-prefixes "ejc-format" '("ejc-"))

;;;***

;;;### (autoloads nil "ejc-interaction" "ejc-interaction.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ejc-interaction.el

(register-definition-prefixes "ejc-interaction" '("ejc-"))

;;;***

;;;### (autoloads nil "ejc-lib" "ejc-lib.el" (0 0 0 0))
;;; Generated autoloads from ejc-lib.el

(register-definition-prefixes "ejc-lib" '("ejc-"))

;;;***

;;;### (autoloads nil "ejc-result-buffer" "ejc-result-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ejc-result-buffer.el

(autoload 'ejc-show-last-result "ejc-result-buffer" "\
Popup buffer with last SQL execution result output.

\(fn &key RESULT MODE CONNECTION-NAME DB GOTO-SYMBOL)" t nil)

(autoload 'ejc-show-prev-result "ejc-result-buffer" "\
Change `ejc-results-buffer' contents: show previous SQL evaluation result." t nil)

(autoload 'ejc-show-next-result "ejc-result-buffer" "\
Change `ejc-results-buffer' contents: show next SQL evaluation result." t nil)

(register-definition-prefixes "ejc-result-buffer" '("ejc-"))

;;;***

;;;### (autoloads nil "ejc-result-mode" "ejc-result-mode.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ejc-result-mode.el

(register-definition-prefixes "ejc-result-mode" '("ejc-result-"))

;;;***

;;;### (autoloads nil "ejc-sql" "ejc-sql.el" (0 0 0 0))
;;; Generated autoloads from ejc-sql.el

(autoload 'ejc-sql-mode "ejc-sql" "\
Toggle ejc-sql mode.

This is a minor mode.  If called interactively, toggle the
`ejc-Sql mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `ejc-sql-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'ejc-create-menu "ejc-sql" nil nil nil)

(autoload 'ejc-insert-connection-data "ejc-sql" "\
Insert configured connection data to keep it between Emacs restarts.
Assume to be evaluated somewhere in .emacs or any file, loaded as Emacs
configuration.

\(fn CONNECTION-NAME)" t nil)

(autoload 'ejc-connect-interactive "ejc-sql" "\
Create new connection interactively and connect to it.

\(fn CONNECTION-NAME)" t nil)

(autoload 'ejc-connect "ejc-sql" "\
Connect to selected db.

\(fn CONNECTION-NAME)" t nil)

(autoload 'ejc-connect-existing-repl "ejc-sql" "\
Connect to existing ejc-sql nREPL running process.
You can `cd` to your ejc-sql project folder (typically
'~/.emacs.d/elpa/ejc-sql-<version>') and launch nREPL via `lein repl`.
Then run in Emacs `ejc-connect-existing-repl', type HOST and PORT
from your `lein run` console output. Finally, use `ejc-connect' from
any SQL buffer to connect to exact database, as always. " t nil)

(autoload 'ejc-eval-user-sql-region "ejc-sql" "\
Evaluate SQL bounded by the selection area.

\(fn BEG END)" t nil)

(autoload 'ejc-eval-user-sql-at-point "ejc-sql" "\
Evaluate SQL bounded by the `ejc-sql-separator' or/and buffer
boundaries.

\(fn &key SYNC BEG END (DISPLAY-RESULT t))" t nil)

(autoload 'ejc-show-tables-list "ejc-sql" "\
Output tables list." t nil)

(autoload 'ejc-show-views-list "ejc-sql" "\
Output views list." t nil)

(autoload 'ejc-show-user-types-list "ejc-sql" "\
Output user types list.

\(fn &optional OWNER)" t nil)

(autoload 'ejc-show-constraints-list "ejc-sql" "\
Output constraints list.

\(fn &optional OWNER TABLE)" t nil)

(autoload 'ejc-show-procedures-list "ejc-sql" "\
Output procedures list." t nil)

(autoload 'ejc-get-temp-editor-buffer "ejc-sql" "\


\(fn &optional NUM)" t nil)

(autoload 'ejc-version "ejc-sql" "\
Get the ejc-sql version as string." t nil)

(register-definition-prefixes "ejc-sql" '("ejc-"))

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
