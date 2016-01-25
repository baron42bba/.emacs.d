;;; demo-it-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "demo-it" "demo-it.el" (22173 59614 0 0))
;;; Generated autoloads from demo-it.el

(autoload 'demo-it-start "demo-it" "\
Start the current demonstration and kick off the first step.
STEPS is a list of functions to execute.  If non-nil, the
optional ADVANCED-MODE turns on keybindings where <F6> advances
the steps instead of space.  This mode is better for more
interactive demonstrations.

\(fn STEPS &optional ADVANCED-MODE)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; demo-it-autoloads.el ends here
