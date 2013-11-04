;;; after-save-commands.el --- Run a shell command or lisp form after saving a file

;; Copyright (C) 1997,98,99 by  Karl M. Hegbloom

;; $Id: after-save-commands.el,v 1.8 2002/10/29 20:58:06 scop Exp $
;; Author: Karl M. Hegbloom <karlheg@cathcart.sysc.pdx.edu>
;; Keywords: processes,unix

;; 25-Oct-2002 vladimir@worklogic.com: 
;; - the predicate can also be a lisp form instead of a regexp string
;; - the command can also be a lisp form instead of a shell command  
;; - documented After-save-alist better

;; This file is part of XEmacs.

;;; This might be rolled into `files.el' at some point in the near
;;; future, pending bug fixes, functionality/feature froze, and the
;;; approval of the XEmacs development team (and perhaps RMS... who
;;; will need to find someone to port it some for GNU Emacs if he
;;; would like to.)


;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Set up a list of file-name matching regular expressions associated
;;; with shell commands or lisp forms to run after saving the file.

;;; This is good for things like running `newaliases(1)' on
;;; "/etc/aliases", `xrdb(1)' on "~/.Xresources", installing a new
;;; "~/.crontab", as well as for sending signals to daemons whos
;;; configuration files you've just finished editing.
;;; 
;;; It is much safer and more powerful than using exec statements in
;;; "Local Variables" sections, and can safely be used by root for
;;; system administration tasks.  The shell command can run about
;;; anything you can think of.
;;;
;;; Who knows?  Maybe Homekey Symsun will use this feature, and it
;;; will allow per to quickly run a daemon HUP after reconfiguration
;;; of the nuclear plant control file, thus saving an entire duchy
;;; from immanent destruction!
;;;
;;; See variable `After-save-alist' for more information.

;;; Devel Notes:
;;;
;;;  I would like to perhaps... pull out the ~/ and ~name/ expansion
;;;  from `expand-file-name', and make a `tilde-expand'
;;;  function... and also a `expand-environment-in-string' that
;;;  doesn't try to do any tilde expanditsomes.  The regexp ought to
;;;  be split (by my code in this file?) on \\|, etc., and in the
;;;  relevant locations, expansion should be done, so that the regexp
;;;  can contain ~'s, and have it do what I mean there, like to match
;;;  files in a user's home directory...  ? maybe.

;;; Code:
;;;-----------------------------------------------------
(require 'env)
(require 'advice)

(defmacro After-save--with-modeline-process-extent-ext (&rest body)
  `(and modeline-process
	(consp modeline-process)
	(let ((ext (car modeline-process)))
	  (and (extentp ext) 
	       ,@body))))

(defun After-save--set-help-flyover (str)
  (After-save--with-modeline-process-extent-ext
   (let ((state (After-save--ascmd-property)))
     (unless (stringp str) (setq str (format "%s" str)))
     (cond
      ((eq :on state)
       (setq str (concat "Run: " str)))
      ((eq :off state)
       (setq str (concat "DON'T run: " str)))))
   (set-extent-property ext 'help-echo str)))

(defun After-save--get-help-flyover ()
  (After-save--with-modeline-process-extent-ext
   (let ((str (extent-property ext 'help-echo)))
     (string-match "^[^:]+:\\(.*\\)\\'" str)
     (match-string 1 str))))

(defun After-save--ascmd-property ()
  "Return the 'ascmd property of our extent in `modeline-process' if it
exists."
  (After-save--with-modeline-process-extent-ext
   (extent-property ext 'ascmd)))


(defun After-save--entry-lookup (buf-fn)
  "Lookup BUF-FN in `After-save-alist', and return that record."
  (if buf-fn
      (catch 'return
	(mapc #'(lambda (elt)
                  (if (cond ((stringp (car elt))
                             (string-match (car elt) buf-fn))
                            ((eval (car elt))))
                      (throw 'return elt)))
	      After-save-alist)
	(throw 'return nil))))

;; Q: Is `modeline-process' ok?  Or is there a more standard place for
;; this?  At some point I'll be able to answer my own question, but
;; now have lots else I ought to be doing. (like reading for
;; instance.)  Please advise.  Yeah or Nay?

;;;###autoload
(defun After-save--find-file-hook ()
  "Look up the name of this file in `After-save-alist', and if it has
an entry, turn on the modeline button and indicator."
  (let ((file->cmd-entry (After-save--entry-lookup (buffer-file-name))))
    ;;; (declarelike (special file->cmd-entry)) ; need dynamic scope,
    ;;; this time.  Put up with one byte compiler warning.
    (if file->cmd-entry
	(After-save--install-ascmd))))

;; `autoload' these just in case then get stuck on the hook before the
;; setting of `After-save-alist' brings this program in with its
;; :require.  I think that could happen if the `find-file-hooks' gets
;; saved in `options.el' after this has been installed on it.  That
;; variable might come before the `defcustom'ed variables at the top
;; of this program.  Of course, once this feature is rolled into
;; "files.el", there's no need for it to be in the hooks anymore;
;; it'll be more inline... right?

;;;###autoload
(defun After-save--after-save-hook ()
  "An `after-save-hook' to run a shell-command.
This gets hung on the `after-save-hook'.
See: `After-save-alist'."
  (let ((file->cmd-entry (After-save--entry-lookup (buffer-file-name))))
    (if (and file->cmd-entry
	     (eq :on (After-save--ascmd-property)))
        (let ((confirm-exec-flag (third  file->cmd-entry))
              (command           (fourth file->cmd-entry)))
          (cond ((and confirm-exec-flag
                      (not (y-or-n-p (format "Run: %s ? " command))))
                 ;; user declined: do nothing
                 )
                ((not (stringp command)) ;; command is lisp sexp
                 (message "%s ->%s" command (eval command)))
                (t                      ;; shell command
                 ;; The `copy-sequence' is important, since `setenv' mutates
                 ;; `process-environment' in place. (uses `setcar'...)  We want
                 ;; a copy of `process-environment' to get bound here, so the
                 ;; more global one, outside of this `let*' block, doesn't get
                 ;; its elements modified like that.
                 (let ((process-environment (copy-sequence process-environment))
                       (environ-alist (second file->cmd-entry)))
                   ;; Here, the bound `process-environment' is modified, not
                   ;; the more global one.  The current dynamic value of it
                   ;; will get passed to the command's shell.
                   (setenv "f" (buffer-file-name))
                   (setenv "d" (default-directory))
                   (if environ-alist
                       (mapc #'(lambda (env-pair)
                                 (setenv (car env-pair)
                                         (if (cdr env-pair)
                                             ;; expand $vars
                                             ;; does not expand tilde's...
                                             ;; there is no `expand-environ-in-string'
                                             ;; or `tilde-expand' AFAIK
                                             (substitute-in-file-name (cdr env-pair))
                                           nil)))
                             environ-alist))
                   (shell-command command))))))))

;; I'd just use `minor-mode-alist', but I want the fly-over to show
;; the after save command rather than "button2 will...".
(defun After-save--install-ascmd ()
  "Install a modeline indicator."
  (let ((ext (make-extent nil nil))
	(km (make-sparse-keymap)))
    (set-extent-property ext 'ascmd :on)
    (set-extent-face ext 'modeline-mousable-minor-mode)
    ;;; ref to free var file->cmd-entry : rely on dynamic scope.
    (define-key km [(button2)] #'(lambda ()
				   (interactive)
				   (After-save--toggle-ascmd)))
    (set-extent-keymap ext km)
    (setq modeline-process (cons ext " AScmd"))
    (After-save--set-help-flyover (fourth file->cmd-entry))
    (redraw-modeline)))


(copy-face 'modeline-mousable-minor-mode 'After-save--strikethu-face)
;;; Wish: (set-face-strikethru-p 'After-save--strikethu-face t :strikethru-spaces nil)
(set-face-strikethru-p 'After-save--strikethu-face t)

(defun After-save--toggle-ascmd ()
  "Turn AScmd off if on, on if off, but not on if not installed in this
buffer yet."
  (interactive)
  (let* ((ext (and modeline-process
		   (consp modeline-process)
		   (car modeline-process)))
	 (state (After-save--ascmd-property)))
    (and (extentp ext)
	 (cond
	  ((eq state :off)
	   (set-extent-property ext 'ascmd :on)
	   (set-extent-face ext 'modeline-mousable-minor-mode))
	  ((eq state :on)
	   (set-extent-property ext 'ascmd :off)
	   (set-extent-face ext 'After-save--strikethu-face))))
    (After-save--set-help-flyover (After-save--get-help-flyover))
    (redraw-modeline)))

;; Will of course be unnecessary once this is part of "files.el"
;; someday when it grows up and is ready to join the core of
;; xemacs/lisp/*.el society as a full fledged member.
(defadvice write-file (before After-save activate)
  (if (After-save--ascmd-property)
      (setq modeline-process nil)))

 ;; At least in XEmacs-21.0 Pyrenean63, `write-file' calls
 ;; `set-visited-file-name' which uses `kill-local-variable' to clear
 ;; both `write-file-hooks' and `after-save-hook', amoung others...

(defadvice write-file (after After-save activate)
  (After-save--find-file-hook))

;; I shouldn't have to use `after-init-hook' like this...  Or should
;; I?  I think maybe I ought to be able to use just a straight out
;; `add-hook' here.  But when `find-file-hooks' has been customized,
;; it's value can get set to an arbitrary list after this `add-hook'
;; is run, thus wiping it out. So I have to install it on the
;; `after-init-hook' like this.  It might be nice if the hook type
;; would be initialized by custom with an add-hook... Then again,
;; maybe sticking an add-hook onto the `after-init-hook' like this is
;; really just the standard way of getting a function onto the list?
;; Perhaps by makeing `custom-set-variables' set the hook to an
;; absolute value, we make it possible to know for certain what it's
;; startup time value will be...  minus additions by packages like
;; this one.  YTMAWBK OTOH, perhaps `custom-set-variables' ought to
;; use `add-hook', so that whenif things like this are installed
;; earlier than when the options.el file is run, they won't get wiped
;; out.
(add-hook 'after-init-hook
	  #'(lambda ()
	      (add-hook 'find-file-hooks 'After-save--find-file-hook)
	      (add-hook 'after-save-hook 'After-save--after-save-hook)))

;; And once for when we load this, in case that's sometime after the
;; `after-init-hook' has already been run, like the first time a new
;; user customizes `After-save-alist'.  `add-hook' will ensure it's
;; only in there once.
(add-hook 'find-file-hooks 'After-save--find-file-hook)
(add-hook 'after-save-hook 'After-save--after-save-hook)

;;; "... or should I just put ;;;###autoload cookies in front of those
;;; add-hook's?"  No, again, I'm afraid that somebody might customize
;;; the hooks and they'll overwrite what the `autoload' brings in. And
;;; besides, there's a :require statement in the `defcustom' for
;;; `After-save-alist', which is easily accessed from the [ Options |
;;; Customize | Emacs | Files ] menu.

(defcustom After-save-alist
  '(("/etc/X11/Xresources/\\|/\\.Xresources" nil t "xrdb $f")
    ("/\\.crontab\\'" nil t "crontab $f")
    ("/etc/inetd.conf" nil t "echo /etc/init.d/netbase reload")
    ("\\.mailrc" nil t (build-mail-aliases))
    ((eq major-mode 'sh-mode) nil t "chmod a+x $f")
    ("#  __JUST_FOR_EXAMPLE__  #"
     (("Set_ME" . "to some value")
      ("UN_Set_ME"))
     nil "echo 'rm -rf / && Bwahahahha!'"))
  "*List associating file regexp or condition to shell command or lisp form.
Each element is of the form
  (PREDICATE ((VAR1 . VAL1) (VAR2 . VAL2) (VAR3)...) PROMPT-P COMMAND)
where
  PREDICATE    is a string regexp to match against the file name,
               or a lisp form to evaluate.
  (VAR . VAL)  are environment variable assignments.
               Omit VAL to unset VAR.
  PROMPT-P     whether to ask for confirmation each time the file is saved 
               and prior to running the command.
  COMMAND      Shell command (string) to run or lisp form to eval,
               after the file is saved.

While you are visiting a file that has an `after-save-command' associated with
it, the modeline will display \"AScmd\" in the minor mode list, and moving the
mouse over that indicator will cause the buffer's associated shell command to
be displayed in the minibuffer. Clicking button 2 there will toggle whether
the command will be run or not.

This facility can be very handy for doing things like:
- running `newaliases(1)' after you've edited the `sendmail(8)' daemon's
  \"/etc/aliases\" file,
- running `xrdb\(1)' after you've hand-tweaked your \".Xresource\" settings
- installing a \".crontab\"
- sending a signal to a system daemon whose configuration file you've edited.
- running (build-mail-aliases) after you've edited .mailrc

You may create or change these settings while you are visiting a file, since
it works by installing a function in the global `after-save-hook', and a
lookup in `After-save-alist' for your command spec happens then. You may also
change the settings for a file that's already got an after-save entry, prior
to saving it.

The command you specify will be run in a subshell, out of the
`after-save-hook', using the lisp function `shell-command'. You can cause it
to background by suffixing the command with the usual \"&\". It will inherit
the `process-environment' of your XEmacs session, along with the specified
environment modifications, as well as the following automatically defined
variables:

   $f -- The full path and filename of the file, `buffer-file-name'
   $d -- The directory, with a trailing \"/\" where the file was saved.

The `Var=\"Value\" pair' environment variables will be defined in the context
the shell command will be run in. You may reference previously defined
environment variables within the `Value' fields, since they are expanded
sequentially, from top to bottom, using `substitute-in-file-name', just before
the command is run. $f and $d are set first, and so may be used for expansion
within your environment specifications, as well as in the commandline.

Note that no shell processing will be done until the commandline is fed to
your shell. That is, globbing or brace expansions and things don't happen
until the command is run.

If you use `write-file' (`C-x C-w') to write the visited buffer to a different
filename, the `after-save-command' will not be run, and the after save command
property will be removed from the buffer, unless the new file name matches one
of your `After-save-alist' specifications."

  :require 'after-save-commands
  :set #'(lambda (var val)
	   (set-default var val)
	   (mapc #'(lambda (b)
		     (with-current-buffer b
		       (if (After-save--ascmd-property)
			   (setq modeline-process nil))
		       (After-save--find-file-hook)))
		 (buffer-list))
	   (redraw-modeline t))
  :type '(repeat
	  (list :tag
		"------------------------------------------------------------"
		:indent 2
                (choice (regexp :tag "File name regexp" "")
                        (sexp :tag "ELisp form (predicate)" ""))
		(repeat :tag "Environment"
			:indent 1
                        (cons :tag "Var=\"Value\" pair"
                              (string :tag "Variable" "")
                              (choice (string :tag "Value" "")
				      (const :tag "unset" nil))))
		(boolean :tag "Confirm before execution? " t)
                (choice (string :tag "Shell Command line (use $f and $d)" "")
                        (sexp :tag "ELisp form to evaluate " ""))))
  :group 'files)

(provide 'after-save-commands)
;;; after-save-commands.el ends here


;; vladimir@worklogic.com: this below has some nicer output capabilities,
;; consider merging into the code above.

'(defun my-after-save-action (predicate action)
  (if (cond ((and (stringp predicate)
                  (eq ?$ (aref predicate (1- (length predicate)))))
             (string-match predicate buffer-file-name))
            ((stringp predicate)
             (string= buffer-file-name (expand-file-name predicate)))
            (t (eval predicate)))
      (let (async descr result time)
        (if (setq async (eq (car action) :async))
            (setq action (cdr action)))
        (when (stringp (car action))
          (setq descr (concat "`" (mapconcat 'identity
            (append action (list (file-name-nondirectory buffer-file-name)))
            " ") "'"))
          (setq action (append action (list buffer-file-name))))
        (cond
         (async
          (apply 'start-process descr
                 (setq buf (concat "*output " descr "*"))
                 action)
          (message "%s started, see buffer %s." descr buf))
         (descr
          (setq time (second (current-time)))
          (message "%s started, please wait..." descr)
          (setq result (apply 'call-process
                              (car action) nil nil nil (cdr action)))
          (message "%s returned %s." descr result)
          (if (< 3 (- (second (current-time)) time))
              (ding)))
         (t (message "%s returned %s." action (eval action)))))))
