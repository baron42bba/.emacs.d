;;; tt-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tt-mode" "tt-mode.el" (0 0 0 0))
;;; Generated autoloads from tt-mode.el

(autoload 'tt-mode "tt-mode" "\
Major mode for editing Template Toolkit files

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.tt\\'" . tt-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tt-mode" '("tt-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tt-mode-autoloads.el ends here
