;;; vertica-snippets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vertica-snippets" "vertica-snippets.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from vertica-snippets.el

(autoload 'vertica-snippets-initialize "vertica-snippets" "\
Add snippet dir to yas-snippet-dirs and load it.

\(fn)" nil nil)

(eval-after-load 'yasnippet '(vertica-snippets-initialize))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vertica-snippets" '("vertica-snippets-dir")))

;;;***

;;;### (autoloads nil nil ("vertica-snippets-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vertica-snippets-autoloads.el ends here
