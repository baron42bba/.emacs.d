;;; helm-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (helm-projectile helm-projectile-ag helm-projectile-ack
;;;;;;  helm-projectile-grep helm-projectile-off helm-projectile-on
;;;;;;  helm-projectile-find-other-file helm-projectile-find-file-dwim
;;;;;;  helm-projectile-fuzzy-match) "helm-projectile" "helm-projectile.el"
;;;;;;  (21906 14465 0 0))
;;; Generated autoloads from helm-projectile.el

(defvar helm-projectile-fuzzy-match t "\
Enable fuzzy matching for Helm Projectile commands.
This needs to be set before loading helm-projectile.")

(custom-autoload 'helm-projectile-fuzzy-match "helm-projectile" t)

(autoload 'helm-projectile-find-file-dwim "helm-projectile" "\
Find file at point based on context.

\(fn)" t nil)

(autoload 'helm-projectile-find-other-file "helm-projectile" "\
Switch between files with the same name but different extensions using Helm.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

\(fn &optional FLEX-MATCHING)" t nil)

(autoload 'helm-projectile-on "helm-projectile" "\
Turn on helm-projectile key bindings.

\(fn)" t nil)

(autoload 'helm-projectile-off "helm-projectile" "\
Turn off helm-projectile key bindings.

\(fn)" t nil)

(autoload 'helm-projectile-grep "helm-projectile" "\
Helm version of projectile-grep.

\(fn)" t nil)

(autoload 'helm-projectile-ack "helm-projectile" "\
Helm version of projectile-ack.

\(fn)" t nil)

(autoload 'helm-projectile-ag "helm-projectile" "\
Helm version of projectile-ag.

\(fn &optional OPTIONS)" t nil)

(autoload 'helm-projectile "helm-projectile" "\
Use projectile with Helm instead of ido.

With a prefix ARG invalidates the cache first.
If invoked outside of a project, displays a list of known projects to jump.

\(fn &optional ARG)" t nil)

(eval-after-load 'projectile '(progn (define-key projectile-command-map (kbd "h") 'helm-projectile)))

;;;***

;;;### (autoloads nil nil ("helm-projectile-pkg.el") (21906 14465
;;;;;;  971196 0))

;;;***

(provide 'helm-projectile-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-projectile-autoloads.el ends here
