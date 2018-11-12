;;; aws-snippets-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "aws-snippets" "aws-snippets.el" (0 0 0 0))
;;; Generated autoloads from aws-snippets.el

(autoload 'aws-snippets-initialize "aws-snippets" "\
Initialize package.

\(fn)" nil nil)

(eval-after-load 'yasnippet '(aws-snippets-initialize))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "aws-snippets" '("aws-snippets-")))

;;;***

;;;### (autoloads nil nil ("aws-snippets-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; aws-snippets-autoloads.el ends here
