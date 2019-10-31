(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(history-length 1000)
 '(magit-commit-signoff t)
 '(magit-git-global-arguments (quote ("--no-pager")))
 '(magit-repo-dirs (quote ("~/git")))
 '(package-user-dir "~/.emacs.d/elpa")
 '(paradox-github-token t)
 '(safe-local-variable-values (quote ((bba-keep-whitespaces . 1))))
 '(template-default-directories (quote ("~/.emacs.d/templates/")))
 '(toolbar-visible-p nil)
 '(vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Mtn Arch))))
(defcustom sql-mysql-program "/usr/local/mysql/bin/mysql"
"*Command to start mysql by mysqlDB."
:type 'file
:group 'SQL)

(setq minibuffer-max-depth nil)
