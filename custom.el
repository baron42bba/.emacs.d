;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "red3" "green3" "yellow3" "LightSkyBlue1" "magenta3" "cyan3"
    "gray90"])
 '(aws-snippets-ec2-list-instances-query
   '("Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime]"
     "Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, IamInstanceProfile.Arn]"
     "Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, KeyName]"
     "Reservations[].Instances[?IamInstanceProfile.Arn==null].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Type`].Value[] | [0],Tags[?Key==`Project`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, IamInstanceProfile.Arn]"))
 '(aws-snippets-profiles '("test" "prod" "cn-prod"))
 '(aws-snippets-regions '("us-east-1" "eu-west-1" "ap-southeast-1" "cn-north-1"))
 '(bmkp-last-as-first-bookmark-file "~/workorg/bookmarks")
 '(cfengine-parameters-indent '(promise pname 2))
 '(custom-safe-themes
   '("8c6bc3959bb049983f4c76b1a0b78a3a28629370558e1d7bd45d7b54bf671a88"
     "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26"
     default))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(forge-topic-list-limit '(60 . -5))
 '(frame-background-mode 'dark)
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(grep-template "grep -i <X> <C> -nH -e <R> <F>")
 '(helm-comint-mode-list '(comint-mode slime-repl-mode sql-interactive-mode))
 '(helm-move-to-line-cycle-in-source nil)
 '(history-length 1000)
 '(kubel-use-namespace-list 'on)
 '(magit-commit-arguments '("--signoff" "--gpg-sign=43BA01102D205F88"))
 '(magit-commit-signoff t)
 '(magit-git-global-arguments '("--no-pager"))
 '(magit-log-arguments '("--graph" "--decorate" "-n256"))
 '(magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
 '(magit-pull-arguments nil)
 '(magit-repo-dirs '("~/git"))
 '(magit-repository-directories '(("~/git" . 2)))
 '(org-mobile-directory
   "~/Library/Mobile Documents/iCloud~com~mobileorg~mobileorg/Documents/")
 '(org-mobile-files
   '("~/org/notes.org" "~/org/motorcycles.org" "~/org/milling.org"
     "~/org/kitchen.org" "~/org/todo.org"))
 '(org-startup-folded t)
 '(package-selected-packages
   '(a ac-helm alert annotate ansible apache-mode apples-mode
       artbollocks-mode ascii async atomic-chrome auto-complete
       auto-complete-pcmp avy aws-snippets beacon biblio biblio-core
       bibtex-completion bicycle bug-hunter centered-cursor-mode cider
       citeproc clojure-mode clomacs closql color-identifiers-mode
       command-log-mode company csv-mode csv-nav dash dash-functional
       debian-changelog-mode demo-it detached dired+ dired-git
       dired-hacks-utils dired-narrow direx docker-cli docker-tramp
       dockerfile-mode dwim-shell-command eat edit-indirect
       editorconfig eglot ejc-sql eldoc elfeed elfeed-org emacsql
       emacsql-sqlite engine-mode epoch-view erlang eshell-bookmark
       eshell-vterm ess expand-region f fast-scroll flycheck
       flycheck-mmark flymake flymake-easy flymake-json
       flymake-perlcritic flymake-puppet flymake-yaml forge fullframe
       fullscreen-mode german-holidays ggtags ghub git-commit git-link
       git-timemachine gnuplot go-mode go-snippets go-translate
       google-translate graphql graphviz-dot-mode groovy-mode
       guide-key hcl-mode helm helm-aws helm-bibtex helm-c-yasnippet
       helm-core helm-descbinds helm-git helm-gtags helm-org
       helm-projectile helm-safari helm-swoop highlight-symbol hl-todo
       ht htmlize hydra impatient-mode ivy jenkinsfile-mode json-mode
       json-reformat json-snatcher julia-mode key-chord keyfreq kubel
       langtool log4e lsp-mode lsp-ui lv magit magit-gitflow
       magit-popup magit-section magit-todos markdown-mode mastodon
       mew multiple-cursors mustache mwe-log-commands neotree noflet
       ob-async ob-cfengine3 ob-graphql ob-restclient ob-tmux org
       org-ac org-chef org-jira org-mac-link org-mime org-plus-contrib
       org-ref org-sticky-header org-tree-slide outline-minor-faces
       ox-clip ox-jira ox-pandoc ox-reveal package-lint pacmacs
       paradox parsebib parseclj parseedn pcre2el pdf-tools pinentry
       popup popwin projectile puppet-mode rainbow-delimiters
       rainbow-mode request restclient restclient-helm rhtml-mode s
       s3ed seq sesman sicp simple-httpd smartparens sokoban
       solarized-theme spinner sqlite3 sqlup-mode string-edit
       string-inflection sx symbol-overlay tablist tabulated-list
       terraform-doc terraform-mode tldr transient treepy tt-mode
       twittering-mode undo-tree vertica-snippets visual-ascii-mode
       visual-regexp vterm websocket with-editor x-path-walker
       xah-lookup yaml yaml-mode yasnippet yaxception))
 '(package-user-dir "~/.emacs.d/elpa")
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((user-mail-address . "baron@bundesbrandschatzamt.de")
     (user-mail-address . baron@bundesbrandschatzamt.de)
     (bba-keep-whitespaces . 1)))
 '(template-default-directories '("~/.emacs.d/templates/"))
 '(toolbar-visible-p nil)
 '(tramp-allow-unsafe-temporary-files t)
 '(undo-tree-auto-save-history nil)
 '(vc-handled-backends '(RCS CVS SVN SCCS Bzr Hg Arch))
 '(warning-suppress-types
   '((comp) (comp) (comp) (comp) (comp) (comp) (comp) (comp) (comp)
     (comp))))
(defcustom sql-mysql-program "~/bin/mysql"
"*Command to start mysql by mysqlDB."
:type 'file
:group 'SQL)

(setq minibuffer-max-depth nil)
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :extend nil :stipple nil :background "#282828" :foreground "#07e30d" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Menlo"))))
;;  '(org-block ((t (:inherit shadow :background "#29759c" :foreground "#84cbef" :distant-forground "#131313" :extend t))))
;;  '(org-block-begin-line ((t (:background "#07538a" :foreground "#84cbef" :extend t))))
;;  '(org-block-end-line ((t (:inherit org-block-begin-line))))
;;  '(org-document-title ((t (:inherit default :foreground "#62a9cd" :font "Arial" :height 1.5 :underline nil))))
;;  '(org-level-1 ((t (:inherit default :foreground "#62a9cd" :font "Arial" :height 1.6))))
;;  '(org-level-2 ((t (:inherit default :foreground "#62a9cd" :font "Arial" :height 1.4))))
;;  '(org-level-3 ((t (:inherit default :foreground "#62a9cd" :font "Arial" :height 1.25))))
;;  '(org-level-4 ((t (:inherit default :foreground "#62a9cd" :font "Arial" :height 1.1))))
;;  '(org-level-5 ((t (:inherit default :foreground "#62a9cd" :font "Arial"))))
;;  '(org-level-6 ((t (:inherit default :foreground "#62a9cd" :font "Arial"))))
;;  '(org-level-7 ((t (:inherit default :foreground "#62a9cd" :font "Arial"))))
;;  '(org-level-8 ((t (:inherit default :foreground "#62a9cd" :font "Arial"))))
;;  '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "yellow"))))
;;  '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "magenta1"))))
;;  '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan"))))
;;  '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "SteelBlue1"))))
;;  '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "pink"))))
;;  '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "OrangeRed1"))))
;;  '(show-paren-match ((t (:underline (:color "#07e30d" :style line)))))
;;  '(show-paren-mismatch ((t (:underline (:color "red" :style wave)))))
;;  '(stripes-face ((t :background "gray0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eat-term-font-0 ((t (:foreground "systemBlueColor"))))
 '(org-block ((t (:inherit shadow :background "#29759c" :foreground "#84cbef" :distant-forground "#131313" :extend t))))
 '(org-block-begin-line ((t (:background "#07538a" :foreground "#84cbef" :extend t))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))))
 '(org-document-title ((t (:inherit default :foreground "#62a9cd" :font "Arial" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :foreground "#62a9cd" :font "Arial" :height 1.6))))
 '(org-level-2 ((t (:inherit default :foreground "#62a9cd" :font "Arial" :height 1.4))))
 '(org-level-3 ((t (:inherit default :foreground "#62a9cd" :font "Arial" :height 1.25))))
 '(org-level-4 ((t (:inherit default :foreground "#62a9cd" :font "Arial" :height 1.1))))
 '(org-level-5 ((t (:inherit default :foreground "#62a9cd" :font "Arial"))))
 '(org-level-6 ((t (:inherit default :foreground "#62a9cd" :font "Arial"))))
 '(org-level-7 ((t (:inherit default :foreground "#62a9cd" :font "Arial"))))
 '(org-level-8 ((t (:inherit default :foreground "#62a9cd" :font "Arial"))))
 '(show-paren-match ((t (:underline (:color "#07e30d" :style line)))))
 '(show-paren-mismatch ((t (:underline (:color "red" :style wave)))))
 '(stripes-face ((t :background "gray0")))
 '(vterm-color-blue ((t (:foreground "LightSkyBlue1")))))
