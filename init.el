(if (string-equal system-type "darwin")
    (delete '"/Applications/Emacs.app/Contents/Resources/lisp/org" load-path))
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq org-use-extra-keys t)
(require 'ob-core)
(require 'ob-core)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/README.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (editorconfig aws-snippets yasnippet ob-async org smartparens helm guide-key avy magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
