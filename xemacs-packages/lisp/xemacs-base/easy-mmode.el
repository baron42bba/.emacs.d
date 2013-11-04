;;; easy-mmode.el --- easy definition for major and minor modes

;; Copyright (C) 1997, 2000, 2001 Free Software Foundation, Inc.

;; Author: Georges Brun-Cottan <Georges.Brun-Cottan@inria.fr>
;; Maintainer: Stefan Monnier <monnier@gnu.org>

;; Keywords: extensions lisp

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: GNU Emacs 21.3.

;;; Commentary:

;; Minor modes are useful and common.  This package makes defining a
;; minor mode easy, by focusing on the writing of the minor mode
;; functionalities themselves.  Moreover, this package enforces a
;; conventional naming of user interface primitives, making things
;; natural for the minor-mode end-users.

;; For each mode, easy-mmode defines the following:
;; <mode>      : The minor mode predicate. A buffer-local variable.
;; <mode>-map  : The keymap possibly associated to <mode>.
;; <mode>-hook,<mode>-on-hook,<mode>-off-hook and <mode>-mode:
;;       see `define-minor-mode' documentation
;;
;; eval
;;  (pp (macroexpand '(define-minor-mode <your-mode> <doc>)))
;; to check the result before using it.

;; The order in which minor modes are installed is important.  Keymap
;; lookup proceeds down minor-mode-map-alist, and the order there
;; tends to be the reverse of the order in which the modes were
;; installed.  Perhaps there should be a feature to let you specify
;; orderings.

;; Additionally to `define-minor-mode', the package provides convenient
;; ways to define keymaps, and other helper functions for major and minor
;; modes.

;;; Code:

(eval-when-compile (require 'cl))

;;; This file uses two functions that did not exist in some versions of
;;; XEmacs: propertize and replace-regexp-in-string.  We provide these
;;; functions here for such XEmacsen.
;;;
;;; FIXME: These function definitions should go into the future or
;;; forward-compat package, once that package exists.

;; XEmacs <= 21.4 does not have propertize, but XEmacs >= 21.5 dumps it (it is
;; defined in subr.el).  Therefore, it is either defined regardless of what
;; has been loaded already, or it won't be defined regardless of what is
;; loaded.
(if (not (fboundp 'propertize))
    (defun propertize (string &rest properties)
      "Return a copy of STRING with text properties added.
First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result."
      (let ((str (copy-sequence string)))
	(add-text-properties 0 (length str)
			     properties
			     str)
	str)))

;; XEmacs <= 21.4 does not have replace-regexp-in-string, but XEmacs >= 21.5
;; dumps it (it is defined in subr.el).  Therefore, it is either defined
;; regardless of what has been loaded already, or it won't be defined
;; regardless of what is loaded.
(if (not (fboundp 'replace-regexp-in-string))
    (defun replace-regexp-in-string (regexp rep string &optional
				     fixedcase literal subexp start)
      "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"
      (let ((l (length string))
	    (start (or start 0))
	    matches str mb me)
	(save-match-data
	  (while (and (< start l) (string-match regexp string start))
	    (setq mb (match-beginning 0)
		  me (match-end 0))
	    ;; If we matched the empty string, make sure we advance by one char
	    (when (= me mb) (setq me (min l (1+ mb))))
	    ;; Generate a replacement for the matched substring.
	    ;; Operate only on the substring to minimize string consing.
	    ;; Set up match data for the substring for replacement;
	    ;; presumably this is likely to be faster than munging the
	    ;; match data directly in Lisp.
	    (string-match regexp (setq str (substring string mb me)))
	    (setq matches
		  (cons (replace-match (if (stringp rep)
					   rep
					 (funcall rep (match-string 0 str)))
				       fixedcase literal str subexp)
			(cons (substring string start mb) ; unmatched prefix
			      matches)))
	    (setq start me))
	  ;; Reconstruct a string from the pieces.
	  (setq matches (cons (substring string start l) matches)) ; leftover
	  (apply #'concat (nreverse matches))))))


(defun easy-mmode-pretty-mode-name (mode &optional lighter)
  "Turn the symbol MODE into a string intended for the user.
If provided LIGHTER will be used to help choose capitalization."
  (let* ((case-fold-search t)
	 (name (concat (replace-regexp-in-string
			"-Minor" " minor"
			(capitalize (replace-regexp-in-string
				     "-mode\\'" "" (symbol-name mode))))
		       " mode")))
    (if (not (stringp lighter)) name
      (setq lighter
	    (replace-regexp-in-string "\\`\\s-+\\|\\-s+\\'" "" lighter))
      (replace-regexp-in-string lighter lighter name t t))))

;; XEmacs change: add -on-hook, -off-hook, and macro parameter documentation.
;;;###autoload
(defalias 'easy-mmode-define-minor-mode 'define-minor-mode)
;;;###autoload
(defmacro define-minor-mode (mode doc &optional init-value lighter keymap &rest body)
  "Define a new minor mode MODE.
This function defines the associated control variable MODE, keymap MODE-map,
toggle command MODE, and hook MODE-hook.

DOC is the documentation for the mode toggle command.
Optional INIT-VALUE is the initial value of the mode's variable.
Optional LIGHTER is displayed in the modeline when the mode is on.
Optional KEYMAP is the default (defvar) keymap bound to the mode keymap.
  If it is a list, it is passed to `easy-mmode-define-keymap'
  in order to build a valid keymap.  It's generally better to use
  a separate MODE-map variable than to use this argument.
The above three arguments can be skipped if keyword arguments are
used (see below).

BODY contains code that will be executed each time the mode is (de)activated.
  It will be executed after any toggling but before running the hooks.
  Before the actual body code, you can write
  keyword arguments (alternating keywords and values).
  These following keyword arguments are supported:
:group GROUP	Custom group name to use in all generated `defcustom' forms.
:global GLOBAL	If non-nil specifies that the minor mode is not meant to be
              	buffer-local, so don't make the variable MODE buffer-local.
		By default, the mode is buffer-local.
:init-value VAL	Same as the INIT-VALUE argument.
:lighter SPEC	Same as the LIGHTER argument.
:require SYM	Same as in `defcustom'.

For backwards compatibility, these hooks are run each time the mode is
\(de)activated.  When the mode is toggled, MODE-hook is always run before the
other hook.
MODE-hook: run if the mode is toggled.
MODE-on-hook: run if the mode is activated.
MODE-off-hook: run if the mode is deactivated.
 
\(defmacro easy-mmode-define-minor-mode
  (MODE DOC &optional INIT-VALUE LIGHTER KEYMAP &rest BODY)...\)

For example, you could write
  (define-minor-mode foo-mode \"If enabled, foo on you!\"
    nil \"Foo \" foo-keymap
    :require 'foo :global t :group 'inconvenience
    ...BODY CODE...)"

  ;; Allow skipping the first three args.
  (cond
   ((keywordp init-value)
    (setq body (list* init-value lighter keymap body)
	  init-value nil lighter nil keymap nil))
   ((keywordp lighter)
    (setq body (list* lighter keymap body) lighter nil keymap nil))
   ((keywordp keymap) (push keymap body) (setq keymap nil)))

  (let* ((mode-name (symbol-name mode))
	 (pretty-name (easy-mmode-pretty-mode-name mode lighter))
	 (globalp nil)
	 (group nil)
	 (extra-args nil)
	 (require t)
	 (keymap-sym (if (and keymap (symbolp keymap)) keymap
		       (intern (concat mode-name "-map"))))
	 (hook (intern (concat mode-name "-hook")))
	 (hook-on (intern (concat mode-name "-on-hook")))
	 (hook-off (intern (concat mode-name "-off-hook"))))

    ;; Check keys.
    (while (keywordp (car body))
      (case (pop body)
	(:init-value (setq init-value (pop body)))
	(:lighter (setq lighter (pop body)))
	(:global (setq globalp (pop body)))
	(:extra-args (setq extra-args (pop body)))
	(:group (setq group (nconc group (list :group (pop body)))))
	(:require (setq require (pop body)))
	(t (pop body))))

    (unless group
      ;; We might as well provide a best-guess default group.
      (setq group
	    ;; XEmacs: the commented-out functions are going to be in
	    ;; 21.5.  easy-mmode will be moved to the core in any case,
	    ;; so don't worry for now.
	    `(:group ',(or ;(custom-current-group)
			   (intern (replace-regexp-in-string
				    "-mode\\'" "" mode-name))))))
    ;; Add default properties to LIGHTER.
;; #### FSF comments this out in 21.3.
;     (unless (or (not (stringp lighter))
; 		(get-text-property 0 'local-map lighter)
; 		(get-text-property 0 'keymap lighter))
;       (setq lighter
; 	    (propertize lighter
; 			'local-map modeline-minor-mode-map ; XEmacs change
; 			'help-echo "mouse-3: minor mode menu")))

    `(progn
       ;; Define the variable to enable or disable the mode.
       ,(if (not globalp)
	    `(progn
	       (defvar ,mode ,init-value ,(format "Non-nil if %s is enabled.
Use the command `%s' to change this variable." pretty-name mode))
	       (make-variable-buffer-local ',mode))

	  (let ((curfile (or (and (boundp 'byte-compile-current-file)
				  byte-compile-current-file)
			     load-file-name)))
	    `(defcustom ,mode ,init-value
	       ,(format "Non-nil if %s is enabled.
See the command `%s' for a description of this minor-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `%s'."
			pretty-name mode mode)
	       :set (lambda (symbol value) (funcall symbol (or value 0)))
	       :initialize 'custom-initialize-default
	       ,@group
	       :type 'boolean
	       ,@(cond
		  ((not (and curfile require)) nil)
		  ((not (eq require t)) `(:require ,require))
		  (t `(:require
		       ',(intern (file-name-nondirectory
				  (file-name-sans-extension curfile)))))))))

       ;; The actual function.
       (defun ,mode (&optional arg ,@extra-args)
	 ,(or doc
	      (format (concat "Toggle %s on or off.
Interactively, with no prefix argument, toggle the mode.
With universal prefix ARG turn mode on.
With zero or negative ARG turn mode off.
\\{%s}") pretty-name keymap-sym))
	 ;; Use `toggle' rather than (if ,mode 0 1) so that using
	 ;; repeat-command still does the toggling correctly.
	 (interactive (list (or current-prefix-arg 'toggle)))
	 ;; XEmacs addition: save the old mode
	 (let ((old-mode ,mode))
	   (setq ,mode
		 (cond
		  ((eq arg 'toggle) (not ,mode))
		  (arg (or (listp arg);; XEmacs addition: C-u alone
			   (> (prefix-numeric-value arg) 0)))
		  (t
		   (if (null ,mode) t
		     (message
		      "Toggling %s off; better pass an explicit argument."
		      ',mode)
		     nil))))
	   ,@body
	   ;; The on/off hooks are here for backward compatibility only.
	   ;; The on/off hooks are here for backward compatibility only.
	   ;; XEmacs change: check mode before running hooks
	   (and ,hook
		(not (equal old-mode ,mode))
		(run-hooks ',hook))
	   (and ,hook-on
		,mode
		(run-hooks ',hook-on))
	   (and ,hook-off
		(not ,mode)
		(run-hooks ',hook-off)))
	 (if (interactive-p)
	     (progn
	       ;; see comment up at custom-current-group.
	       ;; ,(if globalp `(customize-mark-as-set ',mode))
	       (message ,(format "%s %%sabled" pretty-name)
			(if ,mode "en" "dis"))))
	 (force-mode-line-update)
	 ;; Return the new setting.
	 ,mode)

       ;; Autoloading an easy-mmode-define-minor-mode autoloads
       ;; everything up-to-here.
       ;;
       ;; XEmacs change: XEmacs does not support :autoload-end.  On the other
       ;; hand, I don't see why we need to support it.  An autoload cookie
       ;; just before a (define-minor-mode foo) form will generate an autoload
       ;; form for the file with name foo.  But that's exactly right, since
       ;; the defun created just above here has the name foo.  There are no
       ;; other top-level forms created above here by the macro, so we're done.
       ;;
       ;;:autoload-end

       ;; The toggle's hook.
       (defcustom ,hook nil
	 ,(format "Hook run at the end of function `%s'." mode-name)
	 ,@group
	 :type 'hook)

       ;; XEmacs addition: declare the on and off hooks also
       (defcustom ,hook-on nil
	 ,(format "Hook to run when entering %s." mode-name)
	 :group ,(cadr group)
	 :type 'hook)

       (defcustom ,hook-off nil
	 ,(format "Hook to run when exiting %s." mode-name)
	 :group ,(cadr group)
	 :type 'hook)

       ;; Define the minor-mode keymap.
       ,(unless (symbolp keymap)	;nil is also a symbol.
	  `(defvar ,keymap-sym
	     (let ((m ,keymap))
	       (cond ((keymapp m) m)
		     ((listp m) (easy-mmode-define-keymap m))
		     (t (error "Invalid keymap %S" ,keymap))))
	     ,(format "Keymap for `%s'." mode-name)))

       (add-minor-mode ',mode ',lighter
		       ,(if keymap keymap-sym
			  `(if (boundp ',keymap-sym)
			       (symbol-value ',keymap-sym)))
		       ;; XEmacs change: supply the AFTER and TOGGLE-FUN args
		       t ',mode)

       ;; If the mode is global, call the function according to the default.
       ,(if globalp
	    `(if (and load-file-name (not (equal ,init-value ,mode))
		      ;; XEmacs addition:
		      (not purify-flag))
		 (eval-after-load load-file-name '(,mode (if ,mode 1 -1))))))))

;;;
;;; make global minor mode
;;;

;;;###autoload
(defmacro easy-mmode-define-global-mode (global-mode mode turn-on
						     &rest keys)
  "Make GLOBAL-MODE out of the buffer-local minor MODE.
TURN-ON is a function that will be called with no args in every buffer
  and that should try to turn MODE on if applicable for that buffer.
KEYS is a list of CL-style keyword arguments:
:group to specify the custom group."
  (let* ((global-mode-name (symbol-name global-mode))
	 (pretty-name (easy-mmode-pretty-mode-name mode))
	 (pretty-global-name (easy-mmode-pretty-mode-name global-mode))
	 (group nil)
	 (extra-args nil)
	 (buffers (intern (concat global-mode-name "-buffers")))
	 (cmmh (intern (concat global-mode-name "-cmmh"))))

    ;; Check keys.
    (while (keywordp (car keys))
      (case (pop keys)
	(:extra-args (setq extra-args (pop keys)))
	(:group (setq group (nconc group (list :group (pop keys)))))
	(t (setq keys (cdr keys)))))

    (unless group
      ;; We might as well provide a best-guess default group.
      (setq group
	    `(:group ',(or ;(custom-current-group)
			   (intern (replace-regexp-in-string
				    "-mode\\'" "" (symbol-name mode)))))))

    `(progn
       ;; The actual global minor-mode
       (define-minor-mode ,global-mode
	 ,(format "Toggle %s in every buffer.
With prefix ARG, turn %s on if and only if ARG is positive.
%s is actually not turned on in every buffer but only in those
in which `%s' turns it on."
		  pretty-name pretty-global-name pretty-name turn-on)
	 :global t :extra-args ,extra-args ,@group

	 ;; Setup hook to handle future mode changes and new buffers.
	 (if ,global-mode
	     ;; XEmacs: find-file-hooks not find-file-hook
	     (progn
	       (add-hook 'find-file-hooks ',buffers)
	       (add-hook 'change-major-mode-hook ',cmmh))
	   (remove-hook 'find-file-hooks ',buffers)
	   (remove-hook 'change-major-mode-hook ',cmmh))

	 ;; Go through existing buffers.
	 (dolist (buf (buffer-list))
	   (with-current-buffer buf
	     (if ,global-mode (,turn-on) (when ,mode (,mode -1))))))

       ;; TODO: XEmacs does not support :autoload-end
       ;; Autoloading easy-mmode-define-global-mode
       ;; autoloads everything up-to-here.
       :autoload-end

       ;; List of buffers left to process.
       (defvar ,buffers nil)

       ;; The function that calls TURN-ON in each buffer.
       (defun ,buffers ()
	 (remove-hook 'post-command-hook ',buffers)
	 (while ,buffers
	   (let ((buf (pop ,buffers)))
	     (when (buffer-live-p buf)
	       (with-current-buffer buf (,turn-on))))))
       (put ',buffers 'definition-name ',global-mode)

       ;; The function that catches kill-all-local-variables.
       (defun ,cmmh ()
	 (add-to-list ',buffers (current-buffer))
	 (add-hook 'post-command-hook ',buffers))
       (put ',cmmh 'definition-name ',global-mode))))

;;;
;;; easy-mmode-defmap
;;;

(if (fboundp 'set-keymap-parents)
    (defalias 'easy-mmode-set-keymap-parents 'set-keymap-parents)
  (defun easy-mmode-set-keymap-parents (m parents)
    (set-keymap-parent
     m
     (cond
      ((not (consp parents)) parents)
      ((not (cdr parents)) (car parents))
      (t (let ((m (copy-keymap (pop parents))))
	   (easy-mmode-set-keymap-parents m parents)
	   m))))))

;;;###autoload
(defun easy-mmode-define-keymap (bs &optional name m args)
  "Return a keymap built from bindings BS.
BS must be a list of (KEY . BINDING) where
KEY and BINDINGS are suitable for `define-key'.
Optional NAME is passed to `make-sparse-keymap'.
Optional map M can be used to modify an existing map.
ARGS is a list of additional keyword arguments."
  (let (inherit dense ;suppress
		)
    (while args
      (let ((key (pop args))
	    (val (pop args)))
	(case key
	 (:name (setq name val))
	 (:dense (setq dense val))
	 (:inherit (setq inherit val))
	 (:group)
	 ;;((eq key :suppress) (setq suppress val))
	 (t (message "Unknown argument %s in defmap" key)))))
    (unless (keymapp m)
      (setq bs (append m bs))
      (setq m (if dense (make-keymap name) (make-sparse-keymap name))))
    (dolist (b bs)
      (let ((keys (car b))
	    (binding (cdr b)))
	(dolist (key (if (consp keys) keys (list keys)))
	  (cond
	   ((symbolp key)
	    (substitute-key-definition key binding m global-map))
	   ((null binding)
	    (unless (keymapp (lookup-key m key)) (define-key m key binding)))
	   ((let ((o (lookup-key m key)))
	      (or (null o) (numberp o) (eq o 'undefined)))
	    (define-key m key binding))))))
    (cond
     ((keymapp inherit) (set-keymap-parent m inherit))
     ((consp inherit) (easy-mmode-set-keymap-parents m inherit)))
    m))

;;;###autoload
(defmacro easy-mmode-defmap (m bs doc &rest args)
  `(defconst ,m
     (easy-mmode-define-keymap ,bs nil (if (boundp ',m) ,m) ,(cons 'list args))
     ,doc))


;;;
;;; easy-mmode-defsyntax
;;;

(defun easy-mmode-define-syntax (css args)
  (let ((st (make-syntax-table (plist-get args :copy)))
	(parent (plist-get args :inherit)))
    (dolist (cs css)
      (let ((char (car cs))
	    (syntax (cdr cs)))
	(if (sequencep char)
	    (mapcar (lambda (c) (modify-syntax-entry c syntax st)) char)
	  (modify-syntax-entry char syntax st))))
    ;; XEmacs change: we do not have set-char-table-parent
    (if parent (derived-mode-merge-syntax-tables
		(if (symbolp parent) (symbol-value parent) parent) st))
    st))

;;;###autoload
(defmacro easy-mmode-defsyntax (st css doc &rest args)
  "Define variable ST as a syntax-table.
CSS contains a list of syntax specifications of the form (CHAR . SYNTAX)."
  `(progn
     (autoload 'easy-mmode-define-syntax "easy-mmode")
     (defconst ,st (easy-mmode-define-syntax ,css ,(cons 'list args)) ,doc)))



;;;
;;; easy-mmode-define-navigation
;;;

;; XEmacs change: autoload
;;;###autoload
(defmacro easy-mmode-define-navigation (base re &optional name endfun)
  "Define BASE-next and BASE-prev to navigate in the buffer.
RE determines the places the commands should move point to.
NAME should describe the entities matched by RE.  It is used to build
  the docstrings of the two functions.
BASE-next also tries to make sure that the whole entry is visible by
  searching for its end (by calling ENDFUN if provided or by looking for
  the next entry) and recentering if necessary.
ENDFUN should return the end position (with or without moving point)."
  (let* ((base-name (symbol-name base))
	 (prev-sym (intern (concat base-name "-prev")))
	 (next-sym (intern (concat base-name "-next"))))
    (unless name (setq name (symbol-name base-name)))
    `(progn
       (add-to-list 'debug-ignored-errors
		    ,(concat "^No \\(previous\\|next\\) " (regexp-quote name)))
       (defun ,next-sym (&optional count)
	 ,(format "Go to the next COUNT'th %s." name)
	 (interactive)
	 (unless count (setq count 1))
	 (if (< count 0) (,prev-sym (- count))
	   (if (looking-at ,re) (incf count))
	   (if (not (re-search-forward ,re nil t count))
	       (if (looking-at ,re)
		   (goto-char (or ,(if endfun `(,endfun)) (point-max)))
		 (error ,(format "No next %s" name)))
	     (goto-char (match-beginning 0))
	     (when (and (eq (current-buffer) (window-buffer (selected-window)))
			(interactive-p))
	       (let ((endpt (or (save-excursion
				  ,(if endfun `(,endfun)
				     `(re-search-forward ,re nil t 2)))
				(point-max))))
		 ;; XEmacs change: versions < 21.5.16 have a
		 ;; pos-visible-in-window-p that takes only 2 parameters
		 (unless
		     (if (eq (function-max-args #'pos-visible-in-window-p) 2)
			 (pos-visible-in-window-p endpt nil)
		       (pos-visible-in-window-p endpt nil t))
		   (recenter '(0))))))))
       (defun ,prev-sym (&optional count)
	 ,(format "Go to the previous COUNT'th %s" (or name base-name))
	 (interactive)
	 (unless count (setq count 1))
	 (if (< count 0) (,next-sym (- count))
	   (unless (re-search-backward ,re nil t count)
	     (error ,(format "No previous %s" name))))))))

(provide 'easy-mmode)

;;; easy-mmode.el ends here
