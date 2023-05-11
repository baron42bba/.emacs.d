;;; fast-scroll-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fast-scroll" "fast-scroll.el" (0 0 0 0))
;;; Generated autoloads from fast-scroll.el

(autoload 'fast-scroll-config "fast-scroll" "\
Load some config defaults / binds.

\(fn)" t nil)

(autoload 'fast-scroll-advice-scroll-functions "fast-scroll" "\
Wrap as many scrolling functions that we know of in this advice.

\(fn)" t nil)

(defvar fast-scroll-mode nil "\
Non-nil if Fast-Scroll mode is enabled.
See the `fast-scroll-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `fast-scroll-mode'.")

(custom-autoload 'fast-scroll-mode "fast-scroll" nil)

(autoload 'fast-scroll-mode "fast-scroll" "\
Minor mode to speed up scrolling.

When fast-scroll-mode is on, certain features/modes of Emacs will be
shut off or minimized during the scrolling activity, to ensure
the user experience the least amount of scroll-lag as possible.

By default, enabling this global minor mode will advice the following
scrolling built-ins (or commonly installed scroll functions): `scroll-up-command',
`scroll-down-command', `evil-scroll-up', `evil-scroll-down'.

Disabling this mode will unload the advice that was added when enabling.

The mode-line format will also be set to a minimal mode-line
during scrolling activity.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fast-scroll" '("fast-scroll-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fast-scroll-autoloads.el ends here
