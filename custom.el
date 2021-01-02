(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "red3" "green3" "yellow3" "LightSkyBlue1" "magenta3" "cyan3" "gray90"])
 '(aws-snippets-ec2-list-instances-query
   (quote
    ("Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime]" "Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, IamInstanceProfile.Arn]" "Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, KeyName]" "Reservations[].Instances[?IamInstanceProfile.Arn==null].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Type`].Value[] | [0],Tags[?Key==`Project`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, IamInstanceProfile.Arn]")))
 '(aws-snippets-profiles (quote ("test" "prod" "cn")))
 '(aws-snippets-regions
   (quote
    ("us-east-1" "eu-west-1" "ap-southeast-1" "cn-north-1")))
 '(blink-cursor-mode t)
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(grep-template "grep -i <X> <C> -nH -e <R> <F>")
 '(helm-comint-mode-list (quote (comint-mode slime-repl-mode sql-interactive-mode)))
 '(history-length 1000)
 '(magit-commit-arguments (quote ("--signoff" "--gpg-sign=43BA01102D205F88")))
 '(magit-commit-signoff t)
 '(magit-git-global-arguments (quote ("--no-pager")))
 '(magit-log-arguments (quote ("--graph" "--decorate" "-n256")))
 '(magit-log-margin (quote (t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))
 '(magit-pull-arguments nil)
 '(magit-repo-dirs (quote ("~/git")))
 '(org-agenda-files
   (quote
    ("~/workorg/work.org" "~/org/notes.org" "~/org/emacs.org" "~/org/it.org" "~/org/refile.org" "~/org/workhours.org")))
 '(package-selected-packages
   (quote
    (yasnippet with-editor treepy transient simple-httpd request markdown-mode magit-popup hydra htmlize hl-todo helm git-commit f elfeed dash-functional dash clomacs clojure-mode async magit org lsp-mode git-link eglot helm-org biblio parsebib parseedn lv yaml-mode xah-lookup x-path-walker visual-regexp visual-ascii-mode vertica-snippets undo-tree twittering-mode tt-mode tldr terraform-mode tabulated-list sx string-inflection string-edit sqlup-mode sokoban smartparens sicp s3ed rhtml-mode restclient-helm rainbow-mode rainbow-delimiters puppet-mode paradox pacmacs package-lint ox-reveal ox-jira ox-clip org-tree-slide org-sticky-header org-ref org-plus-contrib org-mime org-jira org-chef org-ac ob-tmux ob-restclient ob-cfengine3 ob-async neotree mwe-log-commands mustache multiple-cursors mew magit-todos magit-gitflow langtool keyfreq json-mode impatient-mode highlight-symbol helm-swoop helm-safari helm-projectile helm-gtags helm-git helm-descbinds helm-c-yasnippet helm-aws guide-key graphql go-snippets go-mode gnuplot git-timemachine ghub ggtags fullscreen-mode fullframe flymake-yaml flymake-puppet flymake-perlcritic flymake-json flycheck fast-scroll expand-region ess epoch-view engine-mode elfeed-org ejc-sql editorconfig edit-indirect dired-narrow dired+ demo-it debian-changelog-mode csv-nav csv-mode command-log-mode color-identifiers-mode centered-cursor-mode bug-hunter aws-snippets avy ascii artbollocks-mode apples-mode ac-helm)))
 '(package-user-dir "~/.emacs.d/elpa")
 '(paradox-github-token t)
 '(safe-local-variable-values (quote ((bba-keep-whitespaces . 1))))
 '(template-default-directories (quote ("~/.emacs.d/templates/")))
 '(toolbar-visible-p nil)
 '(vc-handled-backends (quote (RCS CVS SVN SCCS Bzr Hg Arch))))
(defcustom sql-mysql-program "~/bin/mysql"
"*Command to start mysql by mysqlDB."
:type 'file
:group 'SQL)

(setq minibuffer-max-depth nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(stripes-face ((t :background "gray0"))))
