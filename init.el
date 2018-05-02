(setq load-path
      (append
       (list (expand-file-name "~/.emacs.d/lisp") (expand-file-name "~/.emacs.d/elpa/org-20171030"))
       load-path))
(if (string-equal system-type "darwin")
    (delete ' "/Applications/Emacs.app/Contents/Resources/lisp/org" load-path))
(setq org-use-extra-keys t)
(require 'org "~/.emacs.d/elpa/org-20171030/org.el")
(require 'ob-core)
(require 'ob-tangle)
(require 'ob-sql)
(require 'ob-async)
(org-babel-load-file (expand-file-name "~/.emacs.d/bba.org"))
