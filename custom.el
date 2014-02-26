(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(history-length 1000)
 '(magit-commit-signoff t)
 '(org-agenda-files (quote ("~/org/emacs.org" "~/org/work.org" "~/org/private.org" "~/org/it.org")))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages"))))
 '(package-user-dir "/home/baron/.xemacs/elpa")
 '(recentf-max-saved-items 42)
 '(savehist-mode t nil (savehist))
 '(toolbar-visible-p nil)
 '(vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Mtn Arch))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:size "14pt" :family "Lucidatypewriter"))))
 '(toolbar ((t (:background "Gray80" :size "8"))) t))
(defcustom sql-mysql-program "/usr/local/mysql/bin/mysql"
"*Command to start mysql by mysqlDB."
:type 'file
:group 'SQL)

(setq minibuffer-max-depth nil)
