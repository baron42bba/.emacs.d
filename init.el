;; seting the load-path for load-library:
(setq load-path
(append
(list (expand-file-name "/usr/local/lib/xemacs/xemacs-packages/lisp/"))
(list (expand-file-name "~/.xemacs/"))
;;	     (expand-file-name "/usr/share/emacs/lisp/")
load-path))

;; Some general links regarding these configs:
;;
;; http://www.emacswiki.org/emacs/EmacsCrashCode
;; http://www.emacswiki.org/emacs/EmacsCrashTips
;; http://www.emacswiki.org/emacs/EmacsNiftyTricks

;;; * Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (shell-command "git --work-tree ~/org/ --git-dir ~/org/.git commit -a -m 'autocommit'")
  (shell-command "git --work-tree ~/org/ --git-dir ~/org/.git push origin")
  (setq twittering-cert-file nil )

  (kill-emacs)
  )

;;; * http://www.emacswiki.org/emacs/ELPA
(require 'package)

;;; (package-initialize)

(setq tex-dvi-view-command "(f=*; pdflatex \"${f%.dvi}.tex\" && open \"${f%.dvi}.pdf\")")
;;(require 'rainbow-delimiters)
;;(global-rainbow-delimiters-mode)

(require 'centered-cursor-mode)

;;; * load template support
(require 'template)
(template-initialize)

;; The file names are absolute, not relative, locations
;;     - e.g. /foobar/mthesaur.txt.cache, not mthesaur.txt.cache
(setq synonyms-file        "~/.xemacs/mthesaur.txt")
(setq synonyms-cache-file  "~/.emacs.d/mthesaur.txt.cache")
(require 'synonyms)
(define-key global-map (kbd "C-c ?") 'synonyms)

;; load yaml-mode
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'htmlize )

;;; * vc-git
(require 'vc-git)
  (when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))

;;; * ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c C-@") 'ace-jump-mode)

;;; * ido-mode

(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max- 10)
;; (require 'ido-vertical-mode)
;; (ido-vertical-mode)

;; (defun sd/ido-define-keys() ;; C-n/p is more intuitive in vertical layout
;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
;;   (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
;; )

;;; * visual-regexp

(require 'visual-regexp)
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

;;; *  load org mode
;; See http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html for details

(add-to-list 'load-path "~/.xemacs/xemacs-packages/lisp/org-8.2.3c")
(require 'org)

;; run these commands to make org-protocol work in gnome:
;; gconftool-2 -s /desktop/gnome/url-handlers/org-protocol/command '/usr/bin/emacsclient %s' --type String
;; gconftool-2 -s /desktop/gnome/url-handlers/org-protocol/enabled --type Boolean true
;;
;; this is how your firefox bookmark must look like:
;; javascript:location.href='org-protocol://capture:/l/'%20+%20encodeURIComponent(location.href)+'/'%20+%20encodeURIComponent(document.title)+%20'/'%20+%20encodeURIComponent(window.getSelection()%20)

(require 'org-protocol)

;; (require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-id-method (quote uuidgen))
(setq org-agenda-files (list "~/org/emacs.org"
			     "~/org/work.org"
			     "~/org/private.org"
			     "~/org/it.org"
			     "~/org/refile.org"
			     "~/org/workhours.org"
			     ))

; Some initial languages we want org-babel to support
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (sqlite . t)
   (perl . t)
   ))

;;(setq org-html-preamble nil
;;     org-html-postamble nil
;;      org-html-head "")

;; (setq org-html-preamble nil
;;       org-html-postamble nil
;;       org-html-include-default-style nil
;;       org-html-head ""
;;       org-export-html-with-timestamp nil
;;       org-export-html-style "body-only"
;; )
;; body-only option ?

;; (setq html (org-export-as-html 3 nil nil 1))

(global-set-key (kbd "C-c <f5>") '(lambda () (interactive) (find-file "~/org/notes.org")))

(global-set-key (kbd "C-c <f6>") '(lambda () (interactive) (find-file "~/org/work.org")))
(global-set-key (kbd "C-c <f7>") '(lambda () (interactive) (find-file "~/org/private.org")))
(global-set-key (kbd "C-c <f8>") '(lambda () (interactive) (find-file "~/org/workhours.org")))
(global-set-key (kbd "C-c <f9>") '(lambda () (interactive) (find-file "~/org/emacs.org")))

;; (setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-default-notes-file (concat org-directory "/refile.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates

'(("t" "todo" entry (file+headline "~/org/refile.org" "Tasks")
"* TODO %?\n%U\n

%i\n
%a")

("n" "note" entry (file+headline "~/org/refile.org" "Note")
"* NOTE %?\n%U\n

%i\n
%a")

("m" "Meeting" entry (file+headline "~/org/refile.org" "Meeting")
"* MEETING %? :MEETING:\n%U\n

%i\n
%a" :clock-in t :clock-resume t)

("j" "Journal" entry (file+datetree "~/git/org/diary.org")
 "* %?\n%U\n" :clock-in t :clock-resume t)

("l" "Links (it)" entry (file+headline "~/org/refile.org" "Links")
"** %c\n\n  %u\n  %i"
         :empty-lines 1)

))

(setq org-link-abbrev-alist '(
("bing" . "http://www.bing.com/search?q=%sform=OSDSRC")
("cpan" . "http://search.cpan.org/search?query=%s&mode=all")
("google" . "http://www.google.com/search?q=")
("gmap" . "http://maps.google.com/maps?q=%s")
("omap" . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
("bmap" . "http://www.bing.com/maps/default.aspx?q=%s&mkt=en&FORM=HDRSC4")
("wiki" . "http://en.wikipedia.org/wiki/")
("rfc" . "http://tools.ietf.org/rfc/rfc%s.txt")
("ads" . "http://adsabs.harvard.edu/cgi-bin/nph-abs_connect?author=%s&db_key=AST")
))
;; example: [[bmap:space needle]]
;; load git support
; (require 'egg)
;; (add-to-list 'load-path "~/.xemacs/xemacs-packages/lisp/egg")
;; (load-library "egg")




;; taken from org-mode.org:

;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

;; (defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")
(defvar bh/organization-task-id "20140625-424242-424242")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)


(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)
(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)



;;



;;; * cfengine
(load-library "cfengine")
;;; * enable mouse-wheel
(load-library "mwheel")
(mwheel-install)
;;   (load-library "todo-mode")

;;; * tramp
(load-library "tramp")
(setq default-tramp-method "sftp")

;; with this you can do /sudo:ssh-host:file-on-ssh-host
(add-to-list 'tramp-default-proxies-alist '(".*" "\`root\'" "/ssh:%h:"))

;;; * twitter http://www.twmode.sourceforge.net/
(add-to-list 'load-path "~/.xemacs/xemacs-packages/lisp/twittering-mode-3.0.0")
(require 'twittering-mode)
(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (setq twittering-cert-file "/etc/ssl/certs/ca-bundle.crt") )
  )
)

(setq twittering-use-master-password t)

;;; * Big Brother Database

;; (require 'bbdb)
;; (bbdb-initialize)

;;; * Malyon
;; http://www.emacswiki.org/emacs/MalyonMode

(require 'malyon)

;;; * TemplateToolkit

(add-to-list 'auto-mode-alist '("\\.tt2$" . html-mode))

;;; * AucTex:

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;;; * EPG/GPG:

;; Do not use gpg agent when runing in terminal
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" nil))
    ad-do-it
    (when (not (display-graphic-p))
      (setenv "GPG_AGENT_INFO" agent))))

;; (defadvice epg--start (around advice-epg-disable-agent disable)
;;   "Don't allow epg--start to use gpg-agent in plain text terminals."
;;   (if (display-graphic-p)
;;       ad-do-it
;;     (let ((agent (getenv "GPG_AGENT_INFO")))
;;       (setenv "GPG_AGENT_INFO" nil) ; give us a usable text password prompt
;;       ad-do-it
;;       (setenv "GPG_AGENT_INFO" agent))))
;; (ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
;; (ad-activate 'epg--start)

;;; * Perl

;; load cperl-mode for perl files
(require 'cperl-mode)
(fset 'perl-mode 'cperl-mode)

(eval-after-load "cperl-mode"
    '(add-hook 'cperl-mode-hook (lambda() (cperl-set-style "GNU"))))

;;; * auto-completion

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(add-to-list 'ac-dictionary "~/.dict")
(ac-config-default)
(add-to-list 'ac-modes 'cfengine3-mode)
(add-to-list 'ac-modes 'dns-mode)


;;; * syntax-highlighting
(font-lock-mode)
(global-font-lock-mode 1)

;;; * Display Line Number and Col Number in mode-line
(column-number-mode t)
(line-number-mode t)

;;; * Display time / email in mode-line
(setq display-time-24hr-format t)
(display-time)

;;; * No menubar
(menu-bar-mode -1)

;;; * No toolbar
(if window-system
    (tool-bar-mode -1)
)

;;; * Specify printing format
(setq ps-paper-type 'a4)

;;; * Set ispell dictionary
(setq ispell-dictionary "english")

;;; * Set Shell for M-| command
(setq shell-file-name "/bin/bash")

;;; * Set Shell used by TeX
(setq tex-shell-file-name "/bin/bash")

;;; * Set grep command options
(setq grep-command "grep -i -nH -e ")

;;; * Confirm quit
(setq confirm-kill-emacs 'yes-or-no-p)

;;; * Quick file access with bar
;; (speedbar t)

;;; * Ignore case when completing file names
(setq read-file-name-completion-ignore-case t)

;;; * Highlight parenthesis pairs
(show-paren-mode 1)

;;; * Blinking parenthesis
(setq blink-matching-paren-distance nil)

;;; * Highlight text between parens
(setq show-paren-style 'expression)

;;; * Use buffer nane as frame title
(setq frame-title-format "%b - emacs")

;;; * Completion in mini-buffer
(icomplete-mode t)

;;; * Stack minibuffers
(setq enable-recursive-minibuffers t)

;;; * RecentFiles http://www.emacswiki.org/emacs/RecentFiles
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 42)
(global-set-key "\C-cr" 'recentf-open-files)

;;; * Some nice functions
;;(blink-matching-paren 1)
;;(paren-activate)
(defun insert-date ()
"Insert the current date"
(interactive)
(insert-string (format-time-string "%B %e, %Y")))
(defun insert-timestamp ()
"Insert the current timestamp"
(interactive)
(insert-string (format-time-string "%a %b %e %Y") " " (or (and (boundp 'user-full-name) user-full-name) (user-full-name))" <" (getenv "EMAIL") ">" ))

;; eshell-here: Thanks to Howard Abrahams:
;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
;;
;; modified because current version lacks function have window-total-height.

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
     ;;    (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
;;    (split-window-vertically (- height))
    (split-window-vertically '-10)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(define-key global-map "\C-c!" 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

;; post-commit and post-merge hook for git:
;; #!/bin/bash
;; rm .git/etags
;; find ${PWD} -type f -regex ".*\(\.cf\|_pl\.dat\|_conf.dat\)" | xargs etags --append --output=.git/etags
;; set link for emacs:
;; ln -s ~/.cfagent/inputs/../.git/etags ~/.cfengine_tags

(defun load-git-cfengine ()
  "Load config and tags file of git cfengine repo"
(interactive) (visit-tags-table "~/.cfengine_tags")
(interactive) (find-file "~/.cfagent/inputs/config.cf")
)

;; cfe-config-adduser-ldap runs ldapsearch with cn=user to fill some values.

(defun cfe-config-adduser-ldap ( user )
  "Insert usertemplate based on ldap information for config.cf"
  (interactive "sUser: ")
  (insert "      \"users[" user "][login]\" string => \"" user "\";
      \"users[" user "][fullname]\" string => \"" (substring ( shell-command-to-string (concat "ldapse " user " givenName ")) 0 -1) " " (substring ( shell-command-to-string (concat "ldapse " user " sn ")) 0 -1) "\";
      \"users[" user "][uid]\" string => \"" (substring ( shell-command-to-string (concat "ldapse " user " uidNumber")) 0 -1) "\";
      \"users[" user "][gid]\" string => \"" (substring ( shell-command-to-string (concat "ldapse " user " uidNumber")) 0 -1)"\";
      \"users[" user "][group]\" string => \"" user "\";
      \"users[" user "][groups]\" string => \"adm,apache,games\";
      \"users[" user "][home]\" string => \"/home/" user "\";
      \"users[" user "][shell]\" string => \"/bin/bash\";
      \"users[" user "][flags]\" string => \"-m\";
      \"users[" user "][authorized_keys][0]\" string => \"\";" )

)

(defun cfe-config-adduser ( user )
  "Insert usertemplate for config.cf"
  (interactive "sUser: ")
  (insert "      \"users[" user "][login]\" string => \"" user "\";
      \"users[" user "][fullname]\" string => \"\";
      \"users[" user "][uid]\" string => \"\";
      \"users[" user "][gid]\" string => \"\";
      \"users[" user "][group]\" string => \"" user "\";
      \"users[" user "][groups]\" string => \"" user "\";
      \"users[" user "][home]\" string => \"/home/" user "\";
      \"users[" user "][shell]\" string => \"/bin/bash\";
      \"users[" user "][flags]\" string => \"-m\";
      \"users[" user "][authorized_keys][0]\" string => \"\";" )

)

(defun cfe-insert-bundle ( name )
  "Insert bundletemplate"
  (interactive "sBundle: ")
  (insert "#=head2 bundle " name "
#
#
#
#=cut
#

bundle " name "
{
  vars:

  files:

  methods:

  classes:

}")
)

(add-hook 'cfengine3-mode-hook
  (lambda ()
    (define-key cfengine3-mode-map "\C-cb" 'cfe-insert-bundle)
    (define-key cfengine3-mode-map "\C-cu" 'cfe-config-adduser-ldap)
    (define-key cfengine3-mode-map "\C-c\C-c" 'compile)
    ))

(add-hook 'org-mode-hook
  (lambda ()
    (auto-fill-mode)
    ))

(add-hook 'latex-mode-hook
  (lambda ()
    (auto-fill-mode)
    ))


(defun dns-rndc ()
"Do rndc reload of current buffers filename."
(interactive)
(string-match "/\\([^/]*\\)$" buffer-file-name)
(let* ((zonefile (match-string 1 buffer-file-name))
       )
  (if (y-or-n-p (format "rndc reload %s?" zonefile))
      (shell-command (concat "rndc reload " zonefile ) ) )
  )

)

(add-hook 'dns-mode-hook
  (lambda ()
    (define-key dns-mode-map "\C-c\C-r" 'dns-rndc)
    ))

;; (defun cfe-lookup-docs ()
;;  "Search current word from buffer in online docs."
;;  (interactive)
;;  (save-excursion
;;    (skip-syntax-backward "w_")
;;    (w3m-browse-url (lambda ()
;; 		     (skip-syntax-forward "w_")
;; 		     (point)
;; 		     )
;; 		   )))



(defun eshell/ssh (&rest args)
"Secure shell"
(let ((cmd (eshell-flatten-and-stringify
(cons "ssh" args)))
(display-type (framep (selected-frame))))
(cond
((and
(eq display-type 't)
(getenv "STY"))
(send-string-to-terminal (format "\033]83;screen %s\007" cmd)))
((eq display-type 'x)
(eshell-do-eval
(eshell-parse-command
(format "rxvt -e %s &" cmd)))
nil)
(t
(apply 'eshell-exec-visual (cons "ssh" args))))))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'goto-match-paren)

;;(move-overlay hl-line-overlay
;;	      (line-beginning-position) (1+ (line-end-position))
;;	      (current-buffer)))))

;; (set-face-background-pixmap 'default "~/.xemacs/xemacs-bg.xpm")
(set-foreground-color "green")
(set-background-color "black")

;; pos1: goto start of line, start of screen, start of buffer
;; end: goto end of line, end of screen, end of buffer

(global-set-key '[(home)] 'chb-home)
(global-set-key '[(end)] 'chb-end)
;;
(defun chb-home ()
(interactive)
(setq zmacs-region-stays t)
(if (not (bolp))
(beginning-of-line)
(if (eq this-command last-command)
(cond
 ((not (= (point) (window-start)))
  (move-to-window-line 0)
  (beginning-of-line))
 (t
  (goto-char (point-min)))))))

(defun chb-end ()
(interactive)
(setq zmacs-region-stays t)
(if (not (eolp))
(end-of-line)
(if (eq this-command last-command)
(cond
 ((not (= (point) (save-excursion
		    (move-to-window-line -1)
			    (end-of-line)
			    (point))))
	  (move-to-window-line -1)
	  (end-of-line))
	 (t
	  (goto-char (point-max)))))))




;; safe files with #! in first line as user executable

(add-hook `after-safe-hook
	  #'(lambda ()
	     (and (save-excursion
		    (save-restriction
		      (widen)
		      (goto-char (point-min))
		      (save-match-data
			(looking-at "^#!"))))
		  (not (file-executable-p buffer-file-name))
		  (shell-command (concat "chmod u+x " buffer-file-name))
		  (message
		   (concat "Saved as script: " buffer-file-name)))))

;;
;; list of recently opened files
;;

;; (load "recent-files")
;; (setq recent-files-dont-include
;;      '("~$" "tmp/." "INBOX" ".bbdb" ".newsrc." ))

;; (setq recent-files-non-permanent-submenu t)
;; (setq recent-files-commands-submenu t)
;; (setq recent-files-number-of-entries 30)
;; (recent-files-initialize)

;;  Make the <ctrl> c F12 key toggle Whitespace mode on and off.  Whitespace mode causes 
;; all hard tabs to be highlighted.  You can also configure it to highlight space characters 
;; in a different color.  There is also an untabify function to convert hard tabs to the 
;; appropriate number of spaces, and a tabify function to convert groups of spaces to 
;; hard tabs. 
(global-set-key (kbd "C-c <f12>") 'whitespace-mode)


;; (add-hook 'find-file-hooks 'fume-setup-buffer)
;; (add-hook 'Manual-mode-hook 'turn-on-fume-mode)

;; (function-menu USE-MENUBAR RETURN-ONLY MENU-ITEM-FUNCTION)
  
;;====================================================================
;;The Following Code Will Enable Me To Use The "Fume" Package Which
;;Creates, On The Menubar, A "Functions" Menu Containing The List Of
;;All The Functions In The Buffer Being Currently Displayed.
;;====================================================================
;;
;;Setq-Default Set The Default Value Of A Var.  This Def. Val. Is Seen
;;In Buffers That *Don'T* Have Their Own Values For The Variable.

;(require function-menu)
;(Define-Key Global-Map 'F8 'Function-Menu)
;(Add-Hook 'Find-File-Hooks 'Fume-Add-Menubar-Entry)
;(Define-Key Global-Map "\C-Cl" 'Fume-List-Functions)
;(Define-Key Global-Map "\C-Cg" 'Fume-Prompt-Function-Goto)
;(Define-Key Global-Map '(Shift Button3) 'Mouse-Function-Menu)
;(Define-Key Global-Map '(Meta  Button1) 'Fume-Mouse-Function-Goto)

;(Add-Hook
; 'Find-File-Hooks
; (Function
;  (Lambda()
;    (If (And (String-Match "Xemacs" Emacs-Version)
;             (Boundp 'Emacs-Major-Version)
;            (Or (= Emacs-Major-Version 20)
;                 (And
;                  (= Emacs-Major-Version 19)
;                 (>= Emacs-Minor-Version 13)))
;             (Not (Eq Major-Mode 'Latex-Mode)))
;        (Fume-Add-Menubar-Entry))
;    ))) 


(define-key global-map "\C-ct" 'visit-tags-table)
(define-key global-map "\C-cf" 'tags-search)

(define-key global-map "\C-c\C-t" 'insert-timestamp)
(define-key global-map "\C-c\M-c" 'centered-cursor-mode)

(define-key global-map "\C-cf" 'load-git-cfengine)

(define-key global-map "\C-c\C-w" 'fixup-whitespace)


(define-key global-map "\M-g\M-d" 'magit-diff-unstaged)
(define-key global-map "\M-g\M-b" 'magit-branch-manager)
(define-key global-map "\C-cm" 'magit-status)

(define-key global-map "\C-cw" (lambda ()
				 (interactive)
				 (let ((woman-use-topic-at-point t))
				   (woman))))
(define-key global-map "\C-c\M-d" 'diff-buffer-with-file)

;;; ** Use C-+ and C-- to adjust font size

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; NUMBERIC KEYPAD. nice number pad conveniences as extra function keys

;; (global-set-key (kbd "<kp-subtract>") 'ergoemacs-close-current-buffer)
;; (global-set-key (kbd "<kp-divide>") 'ergoemacs-previous-user-buffer)
;; (global-set-key (kbd "<kp-multiply>") 'ergoemacs-next-user-buffer)

;; (global-set-key (kbd "<C-kp-divide>") 'ergoemacs-previous-emacs-buffer)
;; (global-set-key (kbd "<C-kp-multiply>") 'ergoemacs-next-emacs-buffer)

;; (global-set-key (kbd "<kp-decimal>") 'other-window)
;; (global-set-key (kbd "<kp-0>") 'delete-window)
;; (global-set-key (kbd "<kp-1>") 'delete-other-windows)
;; (global-set-key (kbd "<kp-2>") 'split-window-vertically)
;; (global-set-key (kbd "<kp-3>") 'xah-open-file-at-cursor)

;; (global-set-key (kbd "<kp-9>") 'isearch-forward)

;; Local variables: ***
;; eval: (orgstruct-mode 1) ***
;; eval: (setq orgstruct-heading-prefix-regexp ";;; ") ***
;; End: ***

