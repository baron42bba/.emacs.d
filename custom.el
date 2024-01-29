(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "red3" "green3" "yellow3" "LightSkyBlue1" "magenta3" "cyan3" "gray90"])
 '(aws-snippets-ec2-list-instances-query
   '("Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime]" "Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, IamInstanceProfile.Arn]" "Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, KeyName]" "Reservations[].Instances[?IamInstanceProfile.Arn==null].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Type`].Value[] | [0],Tags[?Key==`Project`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, IamInstanceProfile.Arn]"))
 '(aws-snippets-profiles '("test" "prod" "cn-prod"))
 '(aws-snippets-regions '("us-east-1" "eu-west-1" "ap-southeast-1" "cn-north-1"))
 '(bmkp-last-as-first-bookmark-file "~/workorg/bookmarks")
 '(cfengine-parameters-indent '(promise pname 2))
 '(custom-safe-themes
   '("8c6bc3959bb049983f4c76b1a0b78a3a28629370558e1d7bd45d7b54bf671a88" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(forge-topic-list-limit '(60 . -5))
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(grep-template "grep -i <X> <C> -nH -e <R> <F>")
 '(helm-comint-mode-list '(comint-mode slime-repl-mode sql-interactive-mode))
 '(history-length 1000)
 '(kubel-use-namespace-list 'on)
 '(magit-commit-arguments '("--signoff" "--gpg-sign=43BA01102D205F88"))
 '(magit-commit-signoff t)
 '(magit-git-global-arguments '("--no-pager"))
 '(magit-log-arguments '("--graph" "--decorate" "-n256"))
 '(magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
 '(magit-pull-arguments nil)
 '(magit-repo-dirs '("~/git"))
 '(org-mobile-directory
   "~/Library/Mobile Documents/iCloud~com~mobileorg~mobileorg/Documents/")
 '(org-mobile-files
   '("~/org/notes.org" "~/org/motorcycles.org" "~/org/milling.org" "~/org/kitchen.org" "~/org/todo.org"))
 '(org-startup-folded t)
 '(package-selected-packages
   '(pinentry eshell-bookmark eshell-vterm websocket tablist sesman projectile popup pdf-tools parseclj julia-mode ivy eldoc s restclient popwin ox-pandoc key-chord json-snatcher json-reformat ht helm-bibtex go-translate flymake-easy erlang dwim-shell-command direx dired-hacks-utils closql biblio-core auto-complete-pcmp auto-complete flycheck-mmark terraform-doc hcl-mode yaxception yaml ob-graphql emacsql-sqlite docker-cli citeproc spinner symbol-overlay sqlite3 outline-minor-faces forge bibtex-completion detached atomic-chrome org-mac-link noflet pcre2el vterm german-holidays jenkinsfile-mode cider company seq flymake log4e annotate a magit-section bicycle beacon alert graphviz-dot-mode helm-core apache-mode groovy-mode kubel solarized-theme lsp-ui docker-tramp dockerfile-mode yasnippet with-editor treepy transient simple-httpd request markdown-mode magit-popup hydra htmlize hl-todo helm git-commit f elfeed dash-functional dash clomacs clojure-mode async magit org lsp-mode git-link eglot helm-org biblio parsebib parseedn lv yaml-mode xah-lookup x-path-walker visual-regexp visual-ascii-mode vertica-snippets undo-tree twittering-mode tt-mode tldr terraform-mode tabulated-list sx string-inflection string-edit sqlup-mode sokoban smartparens sicp s3ed rhtml-mode restclient-helm rainbow-mode rainbow-delimiters puppet-mode paradox pacmacs package-lint ox-reveal ox-jira ox-clip org-tree-slide org-sticky-header org-ref org-plus-contrib org-mime org-jira org-chef org-ac ob-tmux ob-restclient ob-cfengine3 ob-async neotree mwe-log-commands mustache multiple-cursors mew magit-todos magit-gitflow langtool keyfreq json-mode impatient-mode highlight-symbol helm-swoop helm-safari helm-projectile helm-gtags helm-git helm-descbinds helm-c-yasnippet helm-aws guide-key graphql go-snippets go-mode gnuplot git-timemachine ghub ggtags fullscreen-mode fullframe flymake-yaml flymake-puppet flymake-perlcritic flymake-json flycheck fast-scroll expand-region ess epoch-view engine-mode elfeed-org ejc-sql editorconfig edit-indirect dired-narrow dired+ demo-it debian-changelog-mode csv-nav csv-mode command-log-mode color-identifiers-mode centered-cursor-mode bug-hunter aws-snippets avy ascii artbollocks-mode apples-mode ac-helm))
 '(package-user-dir "~/.emacs.d/elpa")
 '(paradox-github-token t)
 '(safe-local-variable-values
   '((user-mail-address . "baron@bundesbrandschatzamt.de")
     (user-mail-address . baron@bundesbrandschatzamt.de)
     (bba-keep-whitespaces . 1)))
 '(template-default-directories '("~/.emacs.d/templates/"))
 '(toolbar-visible-p nil)
 '(undo-tree-auto-save-history nil)
 '(vc-handled-backends '(RCS CVS SVN SCCS Bzr Hg Arch))
 '(warning-suppress-types
   '((comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
     (comp)
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
