(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(history-length 1000)
 '(magit-commit-signoff t)
 '(magit-repo-dirs (quote ("~/git")))
 '(org-agenda-files (quote ("~/org/emacs.org" "~/org/work.org" "~/org/private.org" "~/org/it.org")))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-user-dir "~/.xemacs/elpa")
 '(recentf-max-saved-items 42)
 '(savehist-mode t nil (savehist))
 '(template-default-directories (quote ("~/.xemacs/templates/")))
 '(toolbar-visible-p nil)
 '(vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Mtn Arch))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:size "14pt" :family "Lucidatypewriter"))))
 '(cursor ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-1-face ((((background dark)) (:foreground "gray90"))))
 '(rainbow-delimiters-depth-2-face ((((background dark)) (:foreground "#ff82ab"))))
 '(rainbow-delimiters-depth-3-face ((((background dark)) (:foreground "#66ffff"))))
 '(rainbow-delimiters-depth-4-face ((((background dark)) (:foreground "#ffa500"))))
 '(rainbow-delimiters-depth-5-face ((((background dark)) (:foreground "white"))))
 '(rainbow-delimiters-depth-6-face ((((background dark)) (:foreground "#12bdbd"))))
 '(rainbow-delimiters-depth-7-face ((((background dark)) (:foreground "#ff826b"))))
 '(rainbow-delimiters-depth-8-face ((((background dark)) (:foreground "#004c00"))))
 '(rainbow-delimiters-depth-9-face ((((background dark)) (:foreground "blue"))))
 '(tool-bar ((t (:background "Gray80" :size "8")))))
(defcustom sql-mysql-program "/usr/local/mysql/bin/mysql"
"*Command to start mysql by mysqlDB."
:type 'file
:group 'SQL)

(setq minibuffer-max-depth nil)
