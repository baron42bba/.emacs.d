;;; demo-it-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "demo-it" "demo-it.el" (0 0 0 0))
;;; Generated autoloads from demo-it.el

(autoload 'demo-it-start "demo-it" "\
Start the current demonstration and kick off the first step.
STEPS is a list of functions or keystrokes to execute.
If nil, the STEPS must be specified by a call to `demo-it-create'.

The optional ADVANCED-MODE turns on keybindings where <F12>
advances the steps instead of Space.  This mode is better for
more interactive demonstrations.

\(fn &optional STEPS ADVANCED-MODE)" t nil)

(autoload 'demo-it-create "demo-it" "\
Create and store an ordered list of steps and configuration
values. The FORMS can be either function names, expressions or
keywords, like `:advanced-mode' and `:variable-width'.

\(fn &rest FORMS)" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "demo-it" '("demo-it-")))

;;;***

;;;### (autoloads nil "demo-it-custom" "demo-it-custom.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from demo-it-custom.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "demo-it-custom" '("demo-it--")))

;;;***

;;;### (autoloads nil "demo-it-extras" "demo-it-extras.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from demo-it-extras.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "demo-it-extras" '("demo-it-")))

;;;***

;;;### (autoloads nil "demo-it-present" "demo-it-present.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from demo-it-present.el

(autoload 'demo-it-single-presentation "demo-it-present" "\
Demonstration that presents an `org-mode' FILE as a
full-screen presentation. SIZE is the text scaling size, and STYLE is the presentation 

\(fn FILE &optional SIZE STYLE SECTION)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "demo-it-present" '("demo-it-")))

;;;***

;;;### (autoloads nil nil ("demo-it-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; demo-it-autoloads.el ends here
