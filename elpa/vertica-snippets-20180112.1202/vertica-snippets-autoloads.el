;;; vertica-snippets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "vertica-snippets" "vertica-snippets.el" (23128
;;;;;;  38466 776630 477000))
;;; Generated autoloads from vertica-snippets.el

(autoload 'vertica-snippets-initialize "vertica-snippets" "\
Add snippet dir to yas-snippet-dirs and load it.

\(fn)" nil nil)

(eval-after-load 'yasnippet '(vertica-snippets-initialize))

;;;***

;;;### (autoloads nil nil ("vertica-snippets-pkg.el") (23128 38466
;;;;;;  775088 24000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; vertica-snippets-autoloads.el ends here
