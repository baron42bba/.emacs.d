;; LCD-ENTRY:    pydoc.el|Bob Weiner|bob@deepware.com|Interface to pydoc Python documentation viewer|Date|04/23/2001|01.02|www.deepware.com/pub/python
;;
;; FILE:         pydoc.el
;; SUMMARY:      Interface to pydoc Python documentation viewer
;; USAGE:        Autoloaded XEmacs Lisp Library, use {C-c M-h} to invoke
;; KEYWORDS:     help, languages, oop, tools
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Deepware
;; E-MAIL:       bob@deepware.com
;;
;; ORIG-DATE:    18-Apr-01 at 19:11:27
;; LAST-MOD:     23-Apr-01 at 23:59:46 by Bob Weiner
;;
;; Copyright (C) 2001  Bob Weiner
;; Licensed under the Python license version 2.0 or higher.
;;
;; This file is part of InfoDock, available from:
;;   www.sourceforge.net/projects/infodock
;;
;; DESCRIPTION:
;;
;;   Globally adds key binding {C-c M-h} (pydoc-commands) which
;;   displays a menu of commands for interacting with the pydoc Python
;;   documentation viewer library.  See the documentation of
;;   `pydoc-menu-help' for details on each available command.
;;
;;   NOTE: You *must* install the following for pydoc.el to work:
;;
;;     python-mode.el 4.1.1 or higher as your Python editing mode (included
;;     in the pydoc.el distribution)
;;
;;     pydoc_lisp.py in your Python site-packages directory (unless
;;       these functions are already integrated into pydoc.py)
;;
;;     set the environment variable PYTHONDOCS to the root directory
;;       of your Python html documentation tree, for example:
;;       export PYTHONDOCS=$HOME/Python-2.1/Doc/html/
;;     and then startup your editor (otherwise, the environment variable
;;     setting won't be inherited by the editor and some commands will not
;;     work).
;;
;;   Install and byte-compile pydoc.el in one of your load-path directories
;;   and then either set it to autoload or do a (require 'pydoc) in your
;;   editor initialization file.  Then invoke it via the menu key binding.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(require 'python-mode)

;; Ensure that the user has a python-mode version which supports pydoc.el.
(if (string-lessp py-version "$Revision: 1.1 $")
    (error "(pydoc): you must upgrade from python-mode.el %s to 4.1.1 (or higher)"
	   (if (string-match "[0-9.]+" py-version)
	       (match-string 0 py-version) py-version)))

(provide 'pydoc)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar pydoc-version "01.01"
  "Release version of the Emacs Lisp interface to pydoc.")

;; Provide easy access to (pydoc-commands) everywhere since the user may want
;; to reference Python doc when in a Python help buffer, a documentation file,
;; etc.  Use {C-c M-h} instead of {C-c C-h} for this next binding since
;; {C-c C-h} is an important key in InfoDock's outline-minor-mode which is
;; often used under python-mode, so this avoids conflicts.

;;;###autoload
(global-set-key "\C-c\M-h" 'pydoc-commands)

(defvar pydoc-buffer-prefix "Py: "
  "Prefix string attached to all pydoc display buffers.")

(defvar pydoc-xrefs-prefix "^Related help topics:"
  "Prefix regexp output before comma separated pydoc help section names.")

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar pydoc-cmd-alist
  '(("A)propos"         . pydoc-apropos)
    ("H)elp"            . pydoc-help)
    ("K)eyword"         . pydoc-keywords)
    ("M)odule"          . pydoc-modules)
    ("P)ackage"         . pydoc-packages)
    ("T)opic"           . pydoc-topics)
    ("X)ref"            . pydoc-xrefs))
  "Association list of (HELP-CMD-PROMPT . CMD) elements for pydoc commands.")

(defvar pydoc-alist nil
  "Association list of (list-name . alist) elements used to build prompt completion tables.")

(defvar pydoc-menu-mode-map nil
  "Keymap containing pydoc-menu commands.")

(defvar pydoc-menu-special-keys '(?\C-g ?? ?\C-i ?\C-j ?\C-m ?\ ))

(if pydoc-menu-mode-map
    nil
  (setq pydoc-menu-mode-map (make-keymap))
  (suppress-keymap pydoc-menu-mode-map)
  (define-key pydoc-menu-mode-map
    (car (where-is-internal 'keyboard-quit)) 'pydoc-menu-enter)
  (mapcar #'(lambda (key)
	      (define-key pydoc-menu-mode-map (char-to-string key) 'pydoc-menu-enter))
	  pydoc-menu-special-keys)
  (let ((i 33))
    (while (<= i 127)
      (define-key pydoc-menu-mode-map (char-to-string i) 'pydoc-menu-enter)
      (setq i (1+ i)))))

;;; ************************************************************************
;;; Commands
;;; ************************************************************************

;;;###autoload
(defun pydoc-commands (&optional init-flag)
  "Display a menu of commands for interacting with the pydoc Python documentation viewer library.
With optional prefix arg INIT-FLAG, reinitializes the pydoc completion
tables which includes the list of available Python modules.
See the documentation for `pydoc-menu-help' for a description of the
available commands."
  (interactive "P")
  (if (or init-flag (null pydoc-alist))
      (pydoc-initialize))
  (call-interactively (pydoc-select-command)))

;;;###autoload
(defun pydoc-apropos (&optional argument)
  "Call the Python module apropos function with optional ARGUMENT and display the output.
ARGUMENT defaults to an identifier near point.
The apropos function finds matches for ARGUMENT within the first line of
module or package doc strings."
  (interactive)
  (if (not argument)
      (setq argument (pydoc-read-argument "Python module/package apropos: ")))
  (pydoc-call "apropos" (format "'%s'" argument) (concat "apropos " argument)))

;;;###autoload
(defun pydoc-help (&optional argument)
  "Call the Python help function with optional ARGUMENT and display the output.
ARGUMENT defaults to an identifier near point.
ARGUMENT is resolved as a name in any present Python namespace; use
single 'quotes' to send ARGUMENT as a Python literal string."
  (interactive)
  (if (not argument)
      (setq argument (pydoc-read-argument "Python help (name or 'string'): ")))
  (pydoc-call "help" argument argument))

;;;###autoload
(defun pydoc-keywords (argument)
  "Prompt with completion for a Python keyword ARGUMENT and display its documentation."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Python keyword help (RET for all): "
		       (cdr (assoc "keywords" pydoc-alist))
		       nil t))))
  (if (member argument '(nil ""))
      (setq argument "keywords"))
  (pydoc-call "help" (format "'%s'" argument) argument))

;;;###autoload
(defun pydoc-modules (argument)
  "Prompt with completion for a Python module/package ARGUMENT and display its documentation."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Python module/package help (RET for all): "
		       (cdr (assoc "modules" pydoc-alist))
		       nil t))))
  (if (member argument '(nil ""))
      (setq argument "modules"))
  (if (string-match "\\`Pkg-" argument)
      (setq argument (substring argument (match-end 0))))
  (pydoc-call "help" (format "'%s'" argument) argument))

;;;###autoload
(defun pydoc-packages (argument)
  "Prompt with completion for a Python package ARGUMENT and display its documentation."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Python package help (RET for all): "
		       (delq nil
			     (mapcar
			      #'(lambda (entry)
				  (if (string-match "\\`Pkg-" entry)
				      (list (substring entry (match-end 0)))))
			      (mapcar 'car (cdr (assoc "modules" pydoc-alist)))))
		       nil t))))
  (if (member argument '(nil ""))
      (setq argument "modules"))
  (if (string-match "\\`Pkg-" argument)
      (setq argument (substring argument (match-end 0))))
  (pydoc-call "help" (format "'%s'" argument) argument))

;;;###autoload
(defun pydoc-topics (argument)
  "Prompt with completion for a Python topic ARGUMENT and display its documentation."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (completing-read "Python topic help (RET for all): "
		       (cdr (assoc "topics" pydoc-alist))
		       nil t))))
  (if (member argument '(nil ""))
      (setq argument "topics")
    (setq argument (upcase argument)))
  (pydoc-call "help" (format "'%s'" argument) argument))

;;;###autoload
(defun pydoc-xrefs ()
  "Display xref at point or prompt user with completion and display chosen xref.
Xrefs are terms which follow the `pydoc-xrefs-prefix' regular expression."
  (interactive)
  (cond ((save-excursion
	   (beginning-of-line)
	   (looking-at pydoc-xrefs-prefix))
	 ;; On an xref line
	 (if (>= (point) (match-end 0))
	     ;; After prefix, within an xref
	     (pydoc-display-xref)
	   ;; Prompt for with completion and display xref
	   (pydoc-display-xref
	    (save-excursion
	      (goto-char (match-end 0))
	      (pydoc-choose-xref "Display xref: ")))))

	;; Move to last xref line if any in buffer
	((pydoc-xrefs-p)
	 ;; Prompt for with completion and display xref
	 (pydoc-display-xref
	  (save-excursion
	    (goto-char (match-end 0))
	    (pydoc-choose-xref "Display xref: "))))

	(t (error "(pydoc): No cross-references in this buffer"))))

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun pydoc-call (func-name argument buf-name-suffix)
  "Call the pydoc function FUNC-NAME with ARGUMENT (a string) and display in Py: BUF-NAME-SUFFIX."
  (let ((wind-config (current-window-configuration))
	(output-buf-name
	 (py-execute-string
	  (format
	   "if not vars().has_key('%s'):\n    from pydoc import %s\n\n%s(%s)\n"
	   func-name func-name func-name argument)))
	(async-process (pydoc-async-output-p))
	input-buf-name)
    (setq input-buf-name (if async-process
			     (buffer-name (process-buffer async-process))
			   " *Python Command*"))
    (if (buffer-live-p (get-buffer input-buf-name))
	(bury-buffer input-buf-name))

    ;; current vintages of python-mode.el (4.6 at least)
    ;; no longer return a buffer [name].  We get t from the
    ;; final kill-buffer instead.  If we see t we use try to
    ;; guess a good buffer name.
    (if (eq output-buf-name t)
	(setq output-buf-name (if async-process
				  (buffer-name (process-buffer async-process))
				"*Python Output*")))

    (if (or (null output-buf-name)
	    ;; In earlier versions of python-mode.el, py-execute-string does
	    ;; not return a buffer name.
	    (not (stringp output-buf-name))
	    (not (buffer-live-p (get-buffer output-buf-name)))
	    (and async-process (not (pydoc-wait-for-output input-buf-name 10.0))))
	(message "(\"%s(%s)\" finished with no output)" func-name argument)
      (set-window-configuration wind-config)
      (bury-buffer output-buf-name)
      (set-buffer output-buf-name)
      (goto-char (point-max)) ;; User may have moved point elsewhere.
      (let ((output-end 
	     ;; Skip any trailing Python prompt
	     (save-excursion (beginning-of-line) (point)))
	    (output-start (pydoc-output-start))
	    (display-buf))
	(if (save-excursion
	      (goto-char (point-max))
	      (forward-line -1)
	      (looking-at "^NameError:"))
	    (progn (switch-to-buffer (car (buffer-list)))
		   (error "(pydoc): %s(%s) failed to find any matching term"
			  func-name argument))
	  (setq display-buf (get-buffer-create
			     (concat pydoc-buffer-prefix buf-name-suffix)))
	  (set-buffer display-buf)
	  (toggle-read-only 0)
	  (erase-buffer)
	  (insert-buffer-substring output-buf-name output-start output-end)
	  (goto-char (point-min))
	  (set-buffer-modified-p nil)
	  (message "")
	  (help-mode)
	  (pydoc-kill-async-output output-buf-name async-process)
	  (pop-to-buffer display-buf))))))

(defun pydoc-display-apropos-entry ()
  "Detect module/package names in pydoc apropos buffer entries and display their code.
Each module/package name must be at the beginning of the line
and must be followed by a space, dash and then another space."
  (interactive)
  (if (string-match (concat "\\`" (regexp-quote pydoc-buffer-prefix)
			    "apropos ")
		    (buffer-name))
      (save-excursion
	(beginning-of-line)
	(if (looking-at "\\([^][-(){}<>'`\"/*+&^%$#@!=|?,~ \t\n\r]+\\) - ")
	    (let* ((entry (buffer-substring
			   (match-beginning 1) (match-end 1)))
		   (path (pydoc-pathname entry)))
	      (if (and path (file-exists-p path))
		  (find-file-other-window path)))))))

(defun pydoc-pathname (module-identifier)
  "Return the filename or package directory where MODULE-IDENTIFIER is defined, else nil."
  (setq module-identifier
	(replace-in-string module-identifier "\\."
			   (char-to-string directory-sep-char) t))
  (or (locate-file module-identifier (pydoc-paths-list) ".py:.pyc:.pyo:.pyd")
      ;; May be a package directory
      (locate-data-directory module-identifier (pydoc-paths-list))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun pydoc-commands-dialog-box (dialog-box)
  "Prompt user with DIALOG-BOX and return selected value.
Assumes caller has checked that `dialog-box' function exists."
  (let ((echo-keystrokes 0)
	event-obj
	event)	 
    ;; Add cmd-help and cancel buttons to dialog box.
    (setq dialog-box (append dialog-box
			     (list nil '["Cmd-Help" (pydoc-menu-help) t]
				   '["Cancel" 'keyboard-quit t])))
    (popup-dialog-box dialog-box)
    (catch 'pydoc-done
      (while t
	(setq event (next-command-event event)
	      event-obj (event-object event))
	(cond ((and (menu-event-p event)
		    (memq event-obj '(abort menu-no-selection-hook)))
	       (signal 'quit nil))
	      ((button-release-event-p event) ;; don't beep twice
	       nil)
	      ((menu-event-p event)
	       (throw 'pydoc-done (eval event-obj)))
	      (t
	       (beep)
	       (message "Please answer the dialog box.")))))))

(defun pydoc-default-argument ()
  "Return a default identifier argument near point."
  (require 'etags)
  ;; Include periods as symbol constituents but remove any trailing period.
  (let* ((period-syntax (char-to-string (char-syntax ?\.)))
	 (default (unwind-protect
		      (progn
			(modify-syntax-entry ?\. "_")
			(find-tag-default))
		    (modify-syntax-entry ?\. period-syntax))))
    (if (and (stringp default) (string-match "\\.+\\'" default))
	(setq default (substring default 0 (match-beginning 0))))
    default))

(defun pydoc-initialize()
  (message "Please wait a moment while the Python help system is initialized...")
  ;; XEmacs change: help python find pydoc_lisp.py OOTB.
  (let ((output-buf)
	(pydir (locate-data-directory "python-modes")))
    (save-window-excursion
      ;; Start a Python interpreter if not already running.
      (py-shell)
      (pydoc-wait-for-output (current-buffer) 3.0)
      (setq output-buf
	    (py-execute-string
	     (format
	      "if not vars().has_key('pydoc_lisp'):
    import sys
    if not '%s' in sys.path:
        sys.path.append('%s')
    import pydoc_lisp

pydoc_lisp.pydoc_output_lisp()
" pydir pydir)))
      ;; current vintages of python-mode.el (4.6 at least)
      ;; no longer return a buffer [name].  We get t from the
      ;; final kill-buffer instead.  If we see t we use the
      ;; python shell's buffer.
      (if (eq output-buf t)
	  (setq output-buf 
		(buffer-name (process-buffer (pydoc-async-output-p)))))

      (setq pydoc-alist (pydoc-lisp-read-result output-buf)))
    (if (member pydoc-alist '(nil None Traceback))
	(progn 
	  (setq pydoc-alist nil)
	  (pop-to-buffer output-buf)
	  (error "(pydoc): Initialization failed, Python did not output Lisp lists"))
      (pydoc-kill-async-output output-buf (pydoc-async-output-p))
      ;; Normalize Python search paths and make a regular list, not an alist
      (let ((paths (assoc "paths" pydoc-alist)))
	(setcdr paths (mapcar 'file-name-as-directory
			      (mapcar 'car (cdr paths)))))
      (message ""))))

(defun pydoc-lisp-read-result (result-buf)
  "Read and return the most recent python output in RESULT-BUF as a Lisp expression.
If a timeout occurs before the expression is read, then return nil."
  (save-excursion
    (if (pydoc-wait-for-output result-buf 5.0)
	(progn (goto-char (pydoc-output-start))
	       (read (current-buffer)))
      nil)))

(defun pydoc-output-start ()
  "Return the character position start of the most recent Python output."
  (save-excursion
    ;; Skip any trailing Python prompt
    (forward-line 0) ;; to bol
    (if (re-search-backward "^>>> " nil t)
	(if (not (zerop (forward-line 1)))
	    (goto-char (point-max)))
      (goto-char (point-min)))
    (point)))

(defun pydoc-read-argument (prompt)
  "Read and return a string argument using PROMPT."
  (let* ((default (pydoc-default-argument))
	 (argument
	  (read-string
	   (if default
	       (format "%s(default %s) " prompt default)
	     prompt))))
    (if (member argument '(nil ""))
	default
      argument)))

(defun pydoc-select-command ()
  "Interactively select and return a pydoc command to run."
  (let ((cmd-prompt)
	(cmd)
	;; Use dialog box if last user event involved the mouse.
	(use-dialog-box (and (fboundp 'popup-dialog-box)
			     (fboundp 'button-press-event-p)
			     (or (button-press-event-p last-command-event)
				 (button-release-event-p last-command-event)
				 (menu-event-p last-command-event)))))
    ;; Create a prompt numbering each command available.
    (setq cmd-prompt
	  (if use-dialog-box
	      (mapcar
	       #'(lambda (name-and-cmd)
		   (vector (car name-and-cmd)
			   (list 'quote (cdr name-and-cmd))
			   't))
	       pydoc-cmd-alist)
	    (concat
	     "Pydoc>  "
	     (mapconcat 'identity (mapcar 'car pydoc-cmd-alist) "  ")
	     ": ")))
    ;; Prompt user.
    (if use-dialog-box
	(setq cmd (pydoc-commands-dialog-box
		   (cons "Choose pydoc command (or choose Cmd-Help for help on commands): " cmd-prompt)))
      ;; Otherwise, prompt in the minibuffer.
      (let ((item-keys (mapcar #'(lambda (item) (aref item 0))
			       (mapcar 'car pydoc-cmd-alist)))
	    key)
	(while (and (not (memq (setq key (upcase
					  (string-to-char
					   (read-from-minibuffer
					    "" cmd-prompt pydoc-menu-mode-map))))
			       item-keys))
		    (not (memq key pydoc-menu-special-keys)))
	  (beep)
	  (discard-input))
	(if (eq key ?\C-g)
	    ;; abort
	    (keyboard-quit))
	(if (memq key pydoc-menu-special-keys)
	    (setq cmd (pydoc-menu-help))
	  (setq cmd (cdr (nth (- (length pydoc-cmd-alist)
				 (length (memq key item-keys)))
			      pydoc-cmd-alist))))))
    cmd))

;;; Asynchronous handling
(defun pydoc-async-output-p ()
  "Return the running asynchronous process for Python code evaluation or nil if none."
  (let ((proc (and (stringp py-which-bufname)
		   (get-process py-which-bufname))))
    (and proc (eq (process-status proc) 'run) proc)))

(defun pydoc-kill-async-output (output-buf async-process)
  (if async-process
      (progn
	(set-buffer output-buf)
	;; Remove output so it doesn't clog up the interpreter buffer.
	(comint-kill-output))))

(defun pydoc-wait-for-output (buffer timeout)
  "Move to BUFFER and wait a maximum of TIMEOUT seconds or until Python command execution ends.
Python command execution ends when Python returns a top-level prompt.
Return t if waited less than TIMEOUT time (and thus received full output)."
  (set-buffer buffer)
  (goto-char (point-max)) ;; User may have moved point elsewhere.
  (let ((time-waited 0.0)
	(wait-time 0.1))
    (while (and (< time-waited timeout)
		(not (save-excursion
		       (forward-line 0) ;; to bol
		       (looking-at ">>> \\'"))))
      (sleep-for wait-time)
      (setq time-waited (+ time-waited wait-time)))
    (< time-waited timeout)))


;;; Cmd menu handling
(defun pydoc-menu-enter (&optional char-str)
  "Uses CHAR-STR or last input character as minibuffer argument."
  (interactive)
  (let ((input (or char-str (aref (recent-keys) (1- (length (recent-keys))))))
	(case-fold-search t))
    (cond
     ;; GNU Emacs 19 or above
     ((and (not (string-match "XEmacs" emacs-version))
	   ;; Version 19 and above.
	   (string-lessp "19" emacs-version))
      (and (not (integerp input))
	   (eventp input)
	   (setq input (event-basic-type input))))
     ((string-match "XEmacs" emacs-version)
      (if (eventp input)
	  (setq input (event-to-character input)))))
    (erase-buffer)
    (insert input))
  (exit-minibuffer))

(defun pydoc-menu-help ()
  "Type one of the following characters:

a   - A)propos <term>    - list modules/packages with <term> in their first line doc strings
h   - H)elp <term>       - display doc for name <term> or string literal '<term>'
k   - K)eyword <keyword> - with completion, display doc for a Python <keyword>
m   - M)odule <name>     - with completion, display doc for a Python module <name>
p   - P)ackage <name>    - with completion, display doc for a Python package <name>
t   - T)opic <topic>     - with completion, display Python reference doc for <topic>
x   - X)ref <term>       - with completion, display doc for a pydoc cross-reference

?   - show this help
C-g - abort from menu"
  (interactive)
  (save-window-excursion
    (switch-to-buffer "*Help*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (documentation 'pydoc-menu-help))
    (goto-char (point-min))
    (pydoc-select-command)))

;;; Pathname handling
(defun pydoc-paths-list ()
  (cdr (assoc "paths" pydoc-alist)))


;;; Xref handling
(defun pydoc-choose-xref (prompt &optional xrefs-alist)
  (or xrefs-alist (setq xrefs-alist (pydoc-xrefs-alist)))
  (if (null xrefs-alist)
      (error "(pydoc): No cross-references in this buffer")
    (let ((completion-ignore-case t)
	  (default (pydoc-default-argument))
	  (result))
      (if (not (assoc default xrefs-alist))
	  (setq default nil))
      (while (not result)
	(setq result (completing-read
		      (if default
			  (format "%s(default %s) " prompt default)
			prompt)
		      xrefs-alist nil t))
	(if (string-equal result "")
	    (if default
		(setq result default)
	      (beep)
	      (setq result nil))))
      result)))

(defun pydoc-delete-space (string)
  "Delete any leading and trailing space from STRING and return the STRING."
  (if (string-match "\\`[ \t\n\r\f]+" string)
      (setq string (substring string (match-end 0))))
  (if (string-match "[ \t\n\r\f]+\\'" string)
      (setq string (substring string 0 (match-beginning 0))))
  string)

(defun pydoc-display-xref (&optional xref)
  "Displays optional XREF (or prompts for and then displays it).
Signals an error when there are no xrefs within the current buffer."
  (interactive)
  (if (member xref '(nil ""))
      ;; Triggers an error if there are no xrefs.
      (setq xref (pydoc-choose-xref "Display xref: ")))
  (let ((case-fold-search nil))
    (if (string-match "\\`[0-9A-Z_]+\\'" xref)
	;; all uppercase, so is a topic
	(pydoc-topics xref)
      ;; assume is a module
      (pydoc-modules xref))))

(defun pydoc-xrefs-alist ()
  ;; Assumes all xrefs are on a single line following point.
  (mapcar #'(lambda (str) (list (pydoc-delete-space str)))
	  (split-string (buffer-substring
			 (point) (save-excursion (end-of-line) (point)))
			",")))

(defun pydoc-xrefs-p ()
  "Return t if current buffer contains xrefs or nil otherwise.
A call to \(match-end 0) returns the end of the xrefs-prefix."
  (save-excursion
    (goto-char (point-max))
    (re-search-backward pydoc-xrefs-prefix nil t)))


