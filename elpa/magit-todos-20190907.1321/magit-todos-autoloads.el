;;; magit-todos-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "magit-todos" "magit-todos.el" (23994 46458
;;;;;;  782116 843000))
;;; Generated autoloads from magit-todos.el

(defvar magit-todos-mode nil "\
Non-nil if Magit-Todos mode is enabled.
See the `magit-todos-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `magit-todos-mode'.")

(custom-autoload 'magit-todos-mode "magit-todos" nil)

(autoload 'magit-todos-mode "magit-todos" "\
Show list of to-do items in Magit status buffer for tracked files in repo.

\(fn &optional ARG)" t nil)

(autoload 'magit-todos-list "magit-todos" "\
Show to-do list of the current Git repository in a buffer.
With prefix, prompt for repository.

\(fn &optional DIRECTORY)" t nil)

(autoload 'magit-todos-list-internal "magit-todos" "\
Open buffer showing to-do list of repository at DIRECTORY.

\(fn DIRECTORY)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; magit-todos-autoloads.el ends here
