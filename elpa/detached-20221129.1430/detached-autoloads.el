;;; detached-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "detached" "detached.el" (0 0 0 0))
;;; Generated autoloads from detached.el

(autoload 'detached-shell-command "detached" "\
Execute COMMAND with `detached'.

Optionally SUPPRESS-OUTPUT if prefix-argument is provided.

\(fn COMMAND &optional SUPPRESS-OUTPUT)" t nil)

(autoload 'detached-open-session "detached" "\
Open a `detached' SESSION.

\(fn SESSION)" t nil)

(autoload 'detached-compile-session "detached" "\
Compile SESSION.

The session is compiled by opening its output and enabling
`compilation-minor-mode'.

\(fn SESSION)" t nil)

(autoload 'detached-edit-session-annotation "detached" "\
Edit SESSION's annotation.

\(fn SESSION)" t nil)

(autoload 'detached-edit-and-run-session "detached" "\
Edit and re-run SESSION at point.

Optionally TOGGLE-SESSION-MODE.

\(fn SESSION &optional TOGGLE-SESSION-MODE)" t nil)

(autoload 'detached-rerun-session "detached" "\
Re-run SESSION at point.

Optionally TOGGLE-SESSION-MODE.

\(fn SESSION &optional TOGGLE-SESSION-MODE)" t nil)

(autoload 'detached-describe-session "detached" "\
Describe current session." t nil)

(autoload 'detached-describe-duration "detached" "\
Describe the SESSION's duration statistics.

\(fn SESSION)" t nil)

(autoload 'detached-attach-session "detached" "\
Attach to SESSION.

\(fn SESSION)" t nil)

(autoload 'detached-copy-session-output "detached" "\
Copy SESSION's output.

\(fn SESSION)" t nil)

(autoload 'detached-copy-session-command "detached" "\
Copy SESSION's command.

\(fn SESSION)" t nil)

(autoload 'detached-insert-session-command "detached" "\
Insert SESSION's command.

\(fn SESSION)" t nil)

(autoload 'detached-delete-session "detached" "\
Delete SESSION.

\(fn SESSION)" t nil)

(autoload 'detached-kill-session "detached" "\
Send a TERM signal to SESSION.

Optionally DELETE the session if prefix-argument is provided.

\(fn SESSION &optional DELETE)" t nil)

(autoload 'detached-view-session "detached" "\
View the SESSION.

\(fn SESSION)" t nil)

(autoload 'detached-diff-session "detached" "\
Diff SESSION1 with SESSION2.

\(fn SESSION1 SESSION2)" t nil)

(autoload 'detached-open-session-directory "detached" "\
Open SESSION's log directory.

\(fn SESSION)" t nil)

(autoload 'detached-detach-session "detached" "\
Detach from session in current buffer.

This command is only activated if `detached-buffer-session' is an
active session.  For sessions created with `detached-compile' or
`detached-shell-command', the command will also kill the window." t nil)

(autoload 'detached-delete-sessions "detached" "\
Delete `detached' sessions which belong to the current host, unless ALL-HOSTS.

\(fn &optional ALL-HOSTS)" t nil)

(autoload 'detached-initialize-sessions "detached" "\
Initialize `detached' sessions from the database." nil nil)

(autoload 'detached-shell-mode "detached" "\
Integrate `detached' in `shell-mode'.

This is a minor mode.  If called interactively, toggle the
`detached-shell mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `detached-shell-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'detached-log-mode "detached" "\
Major mode for `detached' logs.

\(fn)" t nil)

(register-definition-prefixes "detached" '("detached-"))

;;;***

;;;### (autoloads nil "detached-compile" "detached-compile.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from detached-compile.el

(autoload 'detached-compile "detached-compile" "\
Run COMMAND through `compile' but in a `detached' session.
Optionally enable COMINT if prefix-argument is provided.

\(fn COMMAND &optional COMINT)" t nil)

(autoload 'detached-compile-recompile "detached-compile" "\
Re-compile by running `compile' but in a `detached' session.
Optionally EDIT-COMMAND.

\(fn &optional EDIT-COMMAND)" t nil)

(autoload 'detached-compile-attach "detached-compile" "\
Attach to SESSION with `compile'.

\(fn SESSION)" nil nil)

(autoload 'detached-compile-start-session "detached-compile" "\
Start SESSION with `detached-compile'.

\(fn SESSION)" nil nil)

(autoload 'detached-compile--start "detached-compile" "\
Run in `compilation-start-hook' if `detached-enabled'.

\(fn _)" nil nil)

(autoload 'detached-compilation-mode "detached-compile" "\
Major mode for `detached' compilation.

\(fn)" t nil)

(register-definition-prefixes "detached-compile" '("detached-compil"))

;;;***

;;;### (autoloads nil "detached-consult" "detached-consult.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from detached-consult.el

(autoload 'detached-consult-session "detached-consult" "\
Enhanced `detached-open-session' command." t nil)

(register-definition-prefixes "detached-consult" '("detached-consult-"))

;;;***

;;;### (autoloads nil "detached-dired" "detached-dired.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from detached-dired.el

(autoload 'detached-dired-do-shell-command "detached-dired" "\
Ensure `detached' is used before running DIRED-DO-SHELL-COMMAND with ARGS.

\(fn DIRED-DO-SHELL-COMMAND &rest ARGS)" nil nil)

;;;***

;;;### (autoloads nil "detached-eshell" "detached-eshell.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from detached-eshell.el

(autoload 'detached-eshell-external-command "detached-eshell" "\
Advice ORIG-FUN to optionally use `detached' on ARGS.

\(fn ORIG-FUN &rest ARGS)" nil nil)

(autoload 'detached-eshell-mode "detached-eshell" "\
Integrate `detached' in `eshell-mode'.

This is a minor mode.  If called interactively, toggle the
`detached-eshell mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `detached-eshell-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "detached-eshell" '("detached-eshell-"))

;;;***

;;;### (autoloads nil "detached-extra" "detached-extra.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from detached-extra.el

(autoload 'detached-extra-projectile-run-compilation "detached-extra" "\
If CMD is a string execute it with `detached-compile'.

Optionally USE-COMINT-MODE

\(fn CMD &optional USE-COMINT-MODE)" nil nil)

(autoload 'detached-extra-dired-rsync "detached-extra" "\
Run COMMAND with `detached'.

\(fn COMMAND DETAILS)" nil nil)

(autoload 'detached-extra-alert-notification "detached-extra" "\
Send an `alert' notification when SESSION becomes inactive.

\(fn SESSION)" nil nil)

;;;***

;;;### (autoloads nil "detached-init" "detached-init.el" (0 0 0 0))
;;; Generated autoloads from detached-init.el

(autoload 'detached-init "detached-init" "\
Initialize `detached' integration with all packages." nil nil)

(register-definition-prefixes "detached-init" '("detached-"))

;;;***

;;;### (autoloads nil "detached-list" "detached-list.el" (0 0 0 0))
;;; Generated autoloads from detached-list.el

(autoload 'detached-list-sessions "detached-list" "\
Open list of `detached'." t nil)

(register-definition-prefixes "detached-list" '("detached-"))

;;;***

;;;### (autoloads nil "detached-org" "detached-org.el" (0 0 0 0))
;;; Generated autoloads from detached-org.el

(autoload 'detached-org-babel-sh "detached-org" "\
Modify ARGS before calling ORG-BABEL-SH-EVALUATE-FUN.

This function modifies the full-body in ARGS and replaces it with a
`detached' command.  The functionality is enabled by setting a header
property of :detached t in the org babel src block.

\(fn ORG-BABEL-SH-EVALUATE-FUN &rest ARGS)" nil nil)

(register-definition-prefixes "detached-org" '("detached-org-session-action"))

;;;***

;;;### (autoloads nil "detached-shell" "detached-shell.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from detached-shell.el

(autoload 'detached-shell-override-history "detached-shell" "\
Override history in ORIG-FUN with ARGS.

This function also makes sure that the HISTFILE is disabled for local shells.

\(fn ORIG-FUN &rest ARGS)" nil nil)

(autoload 'detached-shell-save-history-on-kill "detached-shell" "\
Add hook to save history when killing `shell' buffer." nil nil)

(register-definition-prefixes "detached-shell" '("detached-shell-"))

;;;***

;;;### (autoloads nil "detached-vterm" "detached-vterm.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from detached-vterm.el

(autoload 'detached-vterm-mode "detached-vterm" "\
Integrate `detached' in `vterm'.

This is a minor mode.  If called interactively, toggle the
`detached-vterm mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `detached-vterm-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "detached-vterm" '("detached-vterm-"))

;;;***

;;;### (autoloads nil nil ("detached-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; detached-autoloads.el ends here
