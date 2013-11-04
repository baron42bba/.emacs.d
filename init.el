;; seting the load-path for load-library:
(setq load-path
(append
(list (expand-file-name "/usr/local/lib/xemacs/xemacs-packages/lisp/"))
(list (expand-file-name "/home/baron/.xemacs/"))
;;	     (expand-file-name "/usr/share/emacs/lisp/")
load-path))

;; Some general links regarding these configs:
;;
;; http://www.emacswiki.org/emacs/EmacsCrashCode
;; http://www.emacswiki.org/emacs/EmacsCrashTips
;; http://www.emacswiki.org/emacs/EmacsNiftyTricks


(require 'centered-cursor-mode)

;; load template support
(require 'template)
(template-initialize)

;; load org mode
;; See http://orgmode.org/worg/org-tutorials/orgtutorial_dto.html for details

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/emacs.org"
			     "~/org/work.org"
			     "~/org/private.org"
			     "~/org/it.org"
			     ))

(define-key global-map "\C-ce" '(lambda () (interactive) (find-file "~/org/emacs.org")))

;; load git support
; (require 'egg)
(load-library "egg")

(load-library "cfengine")
;; to enable mouse-wheel
(load-library "mwheel")
;;   (load-library "todo-mode")
(load-library "tramp")
(setq default-tramp-method "sftp")
(mwheel-install)

;; twitter http://www.twmode.sourceforge.net/
(add-to-list 'load-path "~/.xemacs/xemacs-packages/lisp/twittering-mode-3.0.0")
(require 'twittering-mode)

;; For Big Brother Database

;; (require 'bbdb)
;; (bbdb-initialize)


;; For AucTex:

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; For Perl:

;; load cperl-mode for perl files
(fset 'perl-mode 'cperl-mode)

;; for auto-completion

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(add-to-list 'ac-dictionary "~/.dict")
(ac-config-default)
(add-to-list 'ac-modes 'cfengine3-mode)
(add-to-list 'ac-modes 'dns-mode)


;; syntax-highlighting
(font-lock-mode)
(global-font-lock-mode 1)

;; Display Line Number and Col Number in mode-line
(column-number-mode t)
(line-number-mode t)

;; Display time / email in mode-line
(setq display-time-24hr-format t)
(display-time)

;; No menubar
(menu-bar-mode -1)

;; Specify printing format
(setq ps-paper-type 'a4)

;; Set ispell dictionary
(setq ispell-dictionary "english")

;; Set Shell for M-| command
(setq shell-file-name "/bin/bash")

;; Set Shell used by TeX
(setq tex-shell-file-name "/bin/bash")

;; Set grep command options
(setq grep-command "grep -i -nH -e ")

;; Confirm quit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Quick file access with bar
;; (speedbar t)

;; Ignore case when completing file names
(setq read-file-name-completion-ignore-case t)

;; Highlight parenthesis pairs
(show-paren-mode 1)

;; Blinking parenthesis
(setq blink-matching-paren-distance nil)

;; Highlight text between parens
(setq show-paren-style 'expression)

;; Use buffer nanme as frame title
(setq frame-title-format "%b - emacs")

;; Completion in mini-buffer
(icomplete-mode t)

;; Stack minibuffers
(setq enable-recursive-minibuffers t)

;; RecentFiles http://www.emacswiki.org/emacs/RecentFiles
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 42)
(global-set-key "\C-c\ \C-r" 'recentf-open-files)

;; Some nice functions
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

(defun load-git-cfengine ()
  "Load config and tags file of git cfengine repo"
(interactive) (visit-tags-table "~/.cfengine_tags")
(interactive) (find-file "~/.cfagent/inputs/config.cf")
)

;;(defun scpload (&rest args)
;;"scp copy from pottwal"
;;   (shell-command ( scp andreas@pottwal: emacstmp/))
;;)

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

;;(move-overlay hl-line-overlay
;;	      (line-beginning-position) (1+ (line-end-position))
;;	      (current-buffer)))))

(set-face-background-pixmap 'default "/home/baron/.xemacs/xemacs-bg.xpm")


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

;;  Make the F12 key toggle Whitespace mode on and off.  Whitespace mode causes 
;; all hard tabs to be highlighted.  You can also configure it to highlight space characters 
;; in a different color.  There is also an untabify function to convert hard tabs to the 
;; appropriate number of spaces, and a tabify function to convert groups of spaces to 
;; hard tabs. 
(global-set-key (kbd "<f12>") 'whitespace-mode)


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
(define-key global-map "\C-c\C-c" 'centered-cursor-mode)

(define-key global-map "\C-cc" 'load-git-cfengine)

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
