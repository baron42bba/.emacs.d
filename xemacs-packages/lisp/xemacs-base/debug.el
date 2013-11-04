;;; debug.el --- debuggers and related commands for XEmacs

;; Copyright (C) 1985, 1986, 1994, 2001 Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team
;; Keyword: lisp, tools

;; This file is part of XEmacs.

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

;;; Synched up with: FSF 21.3.

;;; Commentary:

;; NB: There are lots of formatting changes in the XEmacs version. -slb

;; 06/11/1997 - Converted to use char-after instead of broken
;;  following-char. -slb

;; This is a major mode documented in the Emacs manual.

;;; Code:

;; #### not yet implemented (require 'button)

(defgroup debugger nil
  "Debuggers and related commands for Emacs."
  :prefix "debugger-"
  :group 'debug)

(defcustom debugger-mode-hook nil
  "*Hooks run when `debugger-mode' is turned on."
  :type 'hook
  :group 'debugger
  :version "20.3")

(defcustom debugger-batch-max-lines 40
  "*Maximum lines to show in debugger buffer in a noninteractive Emacs.
When the debugger is entered and Emacs is running in batch mode,
if the backtrace text has more than this many lines,
the middle is discarded, and just the beginning and end are displayed."
  :type 'integer
  :group 'debugger
  :version "21.1")

(defcustom debug-function-list nil
  "List of functions currently set for debug on entry."
  :type '(repeat function)
  :group 'debugger)

(defcustom debugger-step-after-exit nil
  "Non-nil means \"single-step\" after the debugger exits."
  :type 'boolean
  :group 'debugger)

(defvar debugger-value nil
  "This is the value for the debugger to return, when it returns.")

(defvar debugger-old-buffer nil
  "This is the buffer that was current when the debugger was entered.")

(defvar debugger-previous-backtrace nil
  "The contents of the previous backtrace (including text properties).
This is to optimize `debugger-make-xrefs'.")

;;#### This is terminally random and nigh-unmaintainable.
;;####  (need progv in elisp...)
(defvar debugger-outer-match-data)
(defvar debugger-outer-load-read-function)
(defvar debugger-outer-overriding-local-map)
(defvar debugger-outer-overriding-terminal-local-map)
;; FSFmacs (defvar debugger-outer-track-mouse)
(defvar debugger-outer-last-command)
(defvar debugger-outer-this-command)
(defvar debugger-outer-unread-command-event)
;; but we still save and restore it
;; in case some user program still tries to set it.
;; FSF: (defvar debugger-outer-unread-command-char)
(defvar debugger-outer-unread-command-events)
;; FSF (defvar debugger-outer-unread-post-input-method-events)
(defvar debugger-outer-last-input-event)
(defvar debugger-outer-last-input-char)
(defvar debugger-outer-last-input-time)
(defvar debugger-outer-last-command-event)
;; FSF (defvar debugger-outer-last-nonmenu-event)
;; FSF (defvar debugger-outer-last-event-frame)
(defvar debugger-outer-last-command-char)
(defvar debugger-outer-standard-input)
(defvar debugger-outer-standard-output)
;; FSF (defvar debugger-outer-inhibit-redisplay)
(defvar debugger-outer-cursor-in-echo-area)

;;;don't ###autoload, loadup.el does something smarter.
(setq debugger 'debug)

(defvar debugger-step-after-exit)
(defvar debugger-old-buffer)
(defvar debugger-value)

;;;###autoload
(defun debug (&rest debugger-args)
  "Enter debugger.  To return, type \\<debugger-mode-map>`\\[debugger-continue]'.
Arguments are mainly for use when this is called from the internals
of the evaluator.

You may call with no args, or you may pass nil as the first arg and
any other args you like.  In that case, the list of args after the
first will be printed into the backtrace buffer."
  (interactive)
  ;; FSF: (we handle this at the C level)
;   (if inhibit-redisplay
;       ;; Don't really try to enter debugger within an eval from redisplay.
;       debugger-value
  ;; XEmacs: it doesn't work to enter the debugger non-interactively
  ;; so just print out a backtrace and exit.
  (if (noninteractive) (apply 'early-error-handler debugger-args))
  (message "Entering debugger...")
  (let (debugger-value
	(debug-on-error nil)
	(debug-on-quit nil)
	(debug-on-signal nil)	; XEmacs
	(debugger-buffer (let ((default-major-mode 'fundamental-mode))
			   (get-buffer-create "*Backtrace*")))
	;; #### I18N3 set the debugger-buffer to output-translating
	(debugger-old-buffer (current-buffer))
	(debugger-step-after-exit nil)
	;; Don't keep reading from an executing kbd macro!
	(executing-kbd-macro nil)
	;; Save the outer values of these vars for the `e' command
	;; before we replace the values.
	(debugger-outer-match-data (match-data))
	(debugger-outer-load-read-function load-read-function)
	(debugger-outer-overriding-local-map overriding-local-map)
 	(debugger-outer-overriding-terminal-local-map
 	 overriding-terminal-local-map)
	;; FSFmacs (debugger-outer-track-mouse track-mouse)
	(debugger-outer-last-command last-command)
	(debugger-outer-this-command this-command)
	(debugger-outer-unread-command-event unread-command-event)
	(debugger-outer-unread-command-events unread-command-events)
; 	(debugger-outer-unread-post-input-method-events
; 	 unread-post-input-method-events)
	(debugger-outer-last-input-event last-input-event)
	(debugger-outer-last-input-char last-input-char)
	(debugger-outer-last-input-time last-input-time)
	(debugger-outer-last-command-event last-command-event)
	;; (debugger-outer-last-nonmenu-event last-nonmenu-event)
	;; (debugger-outer-last-event-frame last-event-frame)
	(debugger-outer-last-command-char last-command-char)
	(debugger-outer-standard-input standard-input)
	(debugger-outer-standard-output standard-output)
	;; (debugger-outer-inhibit-redisplay inhibit-redisplay)
	(debugger-outer-cursor-in-echo-area cursor-in-echo-area))
    ;; Set this instead of binding it, so that `q'
    ;; will not restore it.
    (setq overriding-terminal-local-map nil) 
    ;; Don't let these magic variables affect the debugger itself.
    (unwind-protect                     ;XEmacs change
	(let ((last-command nil)
	      (this-command nil)
	      (unread-command-event nil)
	      (unread-command-events nil)
	      ; (unread-post-input-method-events nil)
	      (last-input-event (make-event))
	      (last-input-char -1)
	      (last-input-time nil)
	      (last-command-event (make-event))
	      (last-command-char -1)
	      overriding-local-map
	      load-read-function
	      ;; If we are inside a minibuffer, allow nesting
	      ;; so that we don't get an error from the `e' command.
	      (enable-recursive-minibuffers
	       (or enable-recursive-minibuffers (> (minibuffer-depth) 0)))
	      (standard-input t)
	      (standard-output t)
	      ;; inhibit-redisplay
	      (cursor-in-echo-area nil))
	  (save-excursion
	    (save-window-excursion
	      (pop-to-buffer debugger-buffer)
	      (debugger-mode)
	      (debugger-setup-buffer debugger-args)
	      (when noninteractive
		;; If the backtrace is long, save the beginning
		;; and the end, but discard the middle.
		(when (> (count-lines (point-min) (point-max))
			 debugger-batch-max-lines)
		  (goto-char (point-min))
		  (forward-line (/ 2 debugger-batch-max-lines))
		  (let ((middlestart (point)))
		    (goto-char (point-max))
		    (forward-line (- (/ 2 debugger-batch-max-lines)
				     debugger-batch-max-lines))
		    (delete-region middlestart (point)))
		  (insert "...\n"))
		(goto-char (point-min))
		(message (buffer-string))
		(kill-emacs))
	      (if (eq (car debugger-args) 'debug)
		  ;; Skip the frames for backtrace-debug, byte-code, and debug.
		  (backtrace-debug 3 t))
	      (debugger-reenable)
	      (message "")
	      (let (;(inhibit-trace t)
		    (standard-output nil)
		    (buffer-read-only t))
		(message "")
 		;; Make sure we unbind buffer-read-only in the right buffer.
 		(save-excursion
 		  (recursive-edit))))
	    ;; XEmacs change
	    debugger-value))
      ;; Kill or at least neuter the backtrace buffer, so that users
      ;; don't try to execute debugger commands in an invalid context.
      (if (get-buffer-window debugger-buffer 'visible)
	    ;; Still visible despite the save-window-excursion?  Maybe it
	    ;; it's in a pop-up frame.  It would be annoying to delete and
	    ;; recreate it every time the debugger stops, so instead we'll
	    ;; erase it but leave it visible.
	  (save-excursion
	    (set-buffer debugger-buffer)
	    (erase-buffer)
	    (fundamental-mode))
	  (kill-buffer debugger-buffer))
      (set-match-data debugger-outer-match-data)
      ;; Put into effect the modified values of these variables
      ;; in case the user set them with the `e' command.
      (setq load-read-function debugger-outer-load-read-function)
      (setq overriding-local-map debugger-outer-overriding-local-map)
      (setq overriding-terminal-local-map
	    debugger-outer-overriding-terminal-local-map)
      ;; FSFmacs (setq track-mouse debugger-outer-track-mouse)
      (setq last-command debugger-outer-last-command)
      (setq this-command debugger-outer-this-command)
      (setq unread-command-event debugger-outer-unread-command-event)
      (setq unread-command-events debugger-outer-unread-command-events)
;       (setq unread-post-input-method-events
;            debugger-outer-unread-post-input-method-events)
      (setq last-input-event debugger-outer-last-input-event)
      (setq last-input-char debugger-outer-last-input-char)
      (setq last-input-time debugger-outer-last-input-time)
      (setq last-command-event debugger-outer-last-command-event)
      ;; (setq last-nonmenu-event debugger-outer-last-nonmenu-event)
      ;; (setq last-event-frame debugger-outer-last-event-frame)
      (setq last-command-char debugger-outer-last-command-char)
      (setq standard-input debugger-outer-standard-input)
      (setq standard-output debugger-outer-standard-output)
      ; FSF (setq inhibit-redisplay debugger-outer-inhibit-redisplay)
      (setq cursor-in-echo-area debugger-outer-cursor-in-echo-area)
      (setq debug-on-next-call debugger-step-after-exit) ;do this last!
      )))

;; XEmacs
(defun debugger-exit ()
  (condition-case nil
      (let ((debug-on-error nil)
	    (debug-on-signal nil))
        ;; Tell signal to keep searching for handlers
        (throw 'debugger t))
    ;; Called from an old version of Emacs, perhaps?
    (no-catch (exit-recursive-edit))))


(defun debugger-setup-buffer (debugger-args)
  "Initialize the `*Backtrace*' buffer for entry to the debugger.
That buffer should be current already."
  (setq buffer-read-only nil)
  (erase-buffer)
  ;; FSF (set-buffer-multibyte nil)
  (let ((standard-output (current-buffer))
	(print-escape-newlines t)
	(print-level 8)
	(print-length 50))
    (backtrace))
  (goto-char (point-min))
  (delete-region (point)
		 (progn
		   ;; XEmacs change
		   (re-search-forward "\n[* ] debug(")
		   (forward-line 1)
		   (point)))
  (insert "Debugger entered")
  ;; lambda is for debug-on-call when a function call is next.
  ;; debug is for debug-on-entry function called.
  (cond ((memq (car debugger-args) '(lambda debug))
	 (insert "--entering a function:\n")
	 (if (eq (car debugger-args) 'debug)
	     (progn
	       (delete-char 1)
	       (insert ?*)
	       (beginning-of-line))))
	;; Exiting a function.
	((eq (car debugger-args) 'exit)
	 (insert "--returning value: ")
	 (setq debugger-value (nth 1 debugger-args))
	 (prin1 debugger-value (current-buffer))
	 (insert ?\n)
	 (delete-char 1)
	 (insert ? )
	 (beginning-of-line))
	;; Debugger entered for an error.
	((eq (car debugger-args) 'error)
	 (insert "--Lisp error: ")
	 (prin1 (nth 1 debugger-args) (current-buffer))
	 (insert ?\n))
	;; debug-on-call, when the next thing is an eval.
	((eq (car debugger-args) t)
	 (insert "--beginning evaluation of function call form:\n"))
	;; User calls debug directly.
	(t
	 (insert ": ")
	 (prin1 (if (eq (car debugger-args) 'nil)
		    (cdr debugger-args) debugger-args)
		(current-buffer))
	 (insert ?\n)))
  ;; After any frame that uses eval-buffer,
  ;; insert a line that states the buffer position it's reading at.
  (save-excursion
    (while (re-search-forward "^  eval-buffer(" nil t)
      (end-of-line)
      (insert (format "\n  ;;; Reading at buffer position %d"
		      (with-current-buffer (nth 2 (backtrace-frame (debugger-frame-number)))
			(point))))))
  (debugger-make-xrefs))

(defun debugger-make-xrefs (&optional buffer)
  "Attach cross-references to symbol names in the `*Backtrace*' buffer."
  (interactive "b")
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (setq buffer (current-buffer))
    (let ((buffer-read-only nil)
	  (old-end (point-min)) (new-end (point-min)))
      ;; If we saved an old backtrace, find the common part
      ;; between the new and the old.
      ;; Compare line by line, starting from the end,
      ;; because that's the part that is likely to be unchanged.
      (if debugger-previous-backtrace
	  (let (old-start new-start (all-match t))
	    (goto-char (point-max))
	    (with-temp-buffer
	      (insert debugger-previous-backtrace)
	      (while (and all-match (not (bobp)))
		(setq old-end (point))
		(forward-line -1)
		(setq old-start (point))
		(with-current-buffer buffer
		  (setq new-end (point))
		  (forward-line -1)
		  (setq new-start (point)))
		(if (not (zerop
			  (compare-buffer-substrings
			   (current-buffer) old-start old-end
			   buffer new-start new-end)))
		    (setq all-match nil))))
	    ;; Now new-end is the position of the start of the
	    ;; unchanged part in the current buffer, and old-end is
	    ;; the position of that same text in the saved old
	    ;; backtrace.  But we must subtract (point-min) since strings are
	    ;; indexed in origin 0.

	    ;; Replace the unchanged part of the backtrace
	    ;; with the text from debugger-previous-backtrace,
	    ;; since that already has the proper xrefs.
	    ;; With this optimization, we only need to scan
	    ;; the changed part of the backtrace.
	    (delete-region new-end (point-max))
	    (goto-char (point-max))
	    (insert (substring debugger-previous-backtrace
			       (- old-end (point-min))))
	    ;; Make the unchanged part of the backtrace inaccessible
	    ;; so it won't be scanned.
	    (narrow-to-region (point-min) new-end)))

      ;; Scan the new part of the backtrace, inserting xrefs.
      (goto-char (point-min))
      (while (progn
	       (skip-syntax-forward "^w_")
	       (not (eobp)))
	(let* ((beg (point))
	       (end (progn (skip-syntax-forward "w_") (point)))
	       (sym (intern-soft (buffer-substring-no-properties
				  beg end)))
	       (file (and sym (symbol-file sym))))
	  (when file
	    (goto-char beg)
	    ;; help-xref-button needs to operate on something matched
	    ;; by a regexp, so set that up for it.
	    (re-search-forward "\\(\\(\\sw\\|\\s_\\)+\\)")
	    ;; #### (help-xref-button 1 'help-function-def sym file)
	    ))
	(forward-line 1))
      (widen))
    (setq debugger-previous-backtrace (buffer-string))))

(defun debugger-step-through ()
  "Proceed, stepping through subexpressions of this expression.
Enter another debugger on next entry to eval, apply or funcall."
  (interactive)
  (setq debugger-step-after-exit t)
  (message "Proceeding, will debug on next eval or call.")
  ;; XEmacs
  (debugger-exit))

(defun debugger-continue ()
  "Continue, evaluating this expression without stopping."
  (interactive)
  ;; FSF
;   (unless debugger-may-continue
;     (error "Cannot continue"))
  (message "Continuing.")
  ;; XEmacs
  (debugger-exit))

(defun debugger-return-value (val)
  "Continue, specifying value to return.
This is only useful when the value returned from the debugger
will be used, such as in a debug on exit from a frame."
  (interactive "XReturn value (evaluated): ")
  (setq debugger-value val)
  (princ "Returning " t)
  (prin1 debugger-value)
  (exit-recursive-edit))

;; XEmacs: [Moved block]
;; Chosen empirically to account for all the frames
;; that will exist when debugger-frame is called
;; within the first one that appears in the backtrace buffer.
;; Assumes debugger-frame is called from a key;
;; will be wrong if it is called with Meta-x.
(defconst debugger-frame-offset 8 "")

(defun debugger-jump ()
  "Continue to exit from this frame, with all debug-on-entry suspended."
  (interactive)
  (debugger-frame)
  ;; Turn off all debug-on-entry functions
  ;; but leave them in the list.
  (let ((list debug-function-list))
    (while list
      (fset (car list)
	    (debug-on-entry-1 (car list) (symbol-function (car list)) nil))
      (setq list (cdr list))))
  (message "Continuing through this frame")
  (debugger-exit))

(defun debugger-reenable ()
  "Turn all debug-on-entry functions back on."
  (let ((list debug-function-list))
    (while list
      (or (consp (symbol-function (car list)))
	  (debug-convert-byte-code (car list)))
      (fset (car list)
	    (debug-on-entry-1 (car list) (symbol-function (car list)) t))
      (setq list (cdr list)))))

(defun debugger-frame-number ()
  "Return number of frames in backtrace before the one point points at."
  (save-excursion
    (beginning-of-line)
    (let ((opoint (point))
	  (count 0))
      (while (not (eq (cadr (backtrace-frame count)) 'debug))
	(setq count (1+ count)))
      (goto-char (point-min))
      ;; XEmacs:#### I18N3 will not localize properly!
      (if (or (equal (buffer-substring (point) (+ (point) 6))
		     (gettext "Signal"))
	      (equal (buffer-substring (point) (+ (point) 6))
		     (gettext "Return")))
	  (progn
	    (search-forward ":")
	    (forward-sexp 1)))
      (forward-line 1)
      (while (progn
	       (forward-char 2)
	       (if (eq (char-after (point)) ?\()
		   (forward-sexp 1)
		 (forward-sexp 2))
	       (forward-line 1)
	       (<= (point) opoint))
	(if (looking-at " *;;;")
	    (forward-line 1))
	(setq count (1+ count)))
      count)))

(defun debugger-frame ()
  "Request entry to debugger when this frame exits.
Applies to the frame whose line point is on in the backtrace."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at " *;;;\\|[a-z]")
	(error "This line is not a function call")))
  (beginning-of-line)
  (backtrace-debug (debugger-frame-number) t)
  (if (eq (char-after (point)) ? )
      (let ((buffer-read-only nil))
	(delete-char 1)
	(insert ?*)))
  (beginning-of-line))

(defun debugger-frame-clear ()
  "Do not enter debugger when this frame exits.
Applies to the frame whose line point is on in the backtrace."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at " *;;;\\|[a-z]")
	(error "This line is not a function call")))
  (beginning-of-line)
  (backtrace-debug (debugger-frame-number) nil)
  (if (eq (char-after (point)) ?*)
      (let ((buffer-read-only nil))
	(delete-char 1)
	(insert ? )))
  (beginning-of-line))

(put 'debugger-env-macro 'lisp-indent-function 0)
(defmacro debugger-env-macro (&rest body)
  "Run BODY in original environment."
  `(save-excursion
    (if (null (buffer-name debugger-old-buffer))
	;; old buffer deleted
	(setq debugger-old-buffer (current-buffer)))
    (set-buffer debugger-old-buffer)
    (let ((load-read-function debugger-outer-load-read-function)
          (overriding-terminal-local-map
           debugger-outer-overriding-terminal-local-map)
          (overriding-local-map debugger-outer-overriding-local-map)
	  ;; (track-mouse debugger-outer-track-mouse)
	  (last-command debugger-outer-last-command)
	  (this-command debugger-outer-this-command)
	  (unread-command-event debugger-outer-unread-command-event)
	  (unread-command-events debugger-outer-unread-command-events)
;           (unread-post-input-method-events
; 	   debugger-outer-unread-post-input-method-events)
	  (last-input-event debugger-outer-last-input-event)
	  (last-input-char debugger-outer-last-input-char)
	  (last-input-time debugger-outer-last-input-time)
	  (last-command-event debugger-outer-last-command-event)
	  ;; (last-nonmenu-event debugger-outer-last-nonmenu-event)
	  ;; (last-event-frame debugger-outer-last-event-frame)
	  (last-command-char debugger-outer-last-command-char)
	  (standard-input debugger-outer-standard-input)
	  (standard-output debugger-outer-standard-output)
      ; (inhibit-redisplay debugger-outer-inhibit-redisplay)
	  (cursor-in-echo-area debugger-outer-cursor-in-echo-area))
      (set-match-data debugger-outer-match-data)
      (prog1 (progn ,@body)
	(setq debugger-outer-match-data (match-data)
	      debugger-outer-load-read-function load-read-function
	      debugger-outer-overriding-local-map overriding-local-map
	      debugger-outer-overriding-terminal-local-map
	      overriding-terminal-local-map
	      ;; debugger-outer-track-mouse track-mouse
	      debugger-outer-last-command last-command
              debugger-outer-this-command this-command
              debugger-outer-unread-command-event unread-command-event
              debugger-outer-unread-command-events unread-command-events
	      ; debugger-outer-unread-post-input-method-events
              ; unread-post-input-method-events
              debugger-outer-last-input-event last-input-event
              debugger-outer-last-input-char last-input-char
              debugger-outer-last-input-time last-input-time
              debugger-outer-last-command-event last-command-event
	      ;; debugger-outer-last-nonmenu-event last-nonmenu-event
	      ;; debugger-outer-last-event-frame last-event-frame
              debugger-outer-last-command-char last-command-char
              debugger-outer-standard-input standard-input
              debugger-outer-standard-output standard-output
	      ; debugger-outer-inhibit-redisplay inhibit-redisplay
              debugger-outer-cursor-in-echo-area cursor-in-echo-area)))))

(defun debugger-eval-expression (exp)
  "Eval an expression, in an environment like that outside the debugger."
  (interactive
   (list (read-from-minibuffer "Eval: "
			       nil read-expression-map t
			       'read-expression-history)))
  (debugger-env-macro (eval-expression exp)))

(defvar debugger-mode-map nil)
(unless debugger-mode-map
  (let (;(loop ? )
	)
    (setq debugger-mode-map (make-keymap))
    ;; #### (set-keymap-parent debugger-mode-map button-buffer-map)
    (suppress-keymap debugger-mode-map)
    (define-key debugger-mode-map "-" 'negative-argument)
    (define-key debugger-mode-map "b" 'debugger-frame)
    (define-key debugger-mode-map "c" 'debugger-continue)
    (define-key debugger-mode-map "j" 'debugger-jump)
    (define-key debugger-mode-map "r" 'debugger-return-value)
    (define-key debugger-mode-map "u" 'debugger-frame-clear)
    (define-key debugger-mode-map "d" 'debugger-step-through)
    (define-key debugger-mode-map "l" 'debugger-list-functions)
    (define-key debugger-mode-map "h" 'describe-mode)
    (define-key debugger-mode-map "q" 'top-level)
    (define-key debugger-mode-map "e" 'debugger-eval-expression)
    (define-key debugger-mode-map " " 'next-line)
    (define-key debugger-mode-map "R" 'debugger-record-expression)
    (define-key debugger-mode-map "\C-m" 'help-follow)
    ;; #### (define-key debugger-mode-map 'button2 'push-button)
    ))


(defcustom debugger-record-buffer "*Debugger-record*"
  "*Buffer name for expression values, for \\[debugger-record-expression]."
  :type 'string
  :group 'debugger
  :version "20.3")

(defun debugger-record-expression  (exp)
  "Display a variable's value and record it in `*Backtrace-record*' buffer."
  (interactive
   (list (read-from-minibuffer
	  "Record Eval: "
	  nil
	  read-expression-map t
	  'read-expression-history)))
  (let* ((buffer (get-buffer-create debugger-record-buffer))
	 (standard-output buffer))
    (princ (format "Debugger Eval (%s): " exp))
    (princ (debugger-eval-expression exp))
    (terpri))

  (with-current-buffer (get-buffer debugger-record-buffer)
    (save-excursion
      (forward-line -1)
      (message
       (buffer-substring (point) (progn (end-of-line) (point)))))))

(put 'debugger-mode 'mode-class 'special)

(defun debugger-mode ()
  "Mode for backtrace buffers, selected in debugger.
\\<debugger-mode-map>
A line starts with `*' if exiting that frame will call the debugger.
Type \\[debugger-frame] or \\[debugger-frame-clear] to set or remove the `*'.

When in debugger due to frame being exited,
use the \\[debugger-return-value] command to override the value
being returned from that frame.

Use \\[debug-on-entry] and \\[cancel-debug-on-entry] to control
which functions will enter the debugger when called.

Complete list of commands:
\\{debugger-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'debugger-mode)
  (setq mode-name (gettext "Debugger"))	; XEmacs
  (setq truncate-lines t)
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (use-local-map debugger-mode-map)
  (run-hooks 'debugger-mode-hook))

;;;###autoload
(defun debug-on-entry (function)
  "Request FUNCTION to invoke debugger each time it is called.
If you tell the debugger to continue, FUNCTION's execution proceeds.
This works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined.
Use \\[cancel-debug-on-entry] to cancel the effect of this command.
Redefining FUNCTION also cancels it."
  (interactive "aDebug on entry (to function): ")
  (debugger-reenable)
  ;; Handle a function that has been aliased to some other function.
  ;; #### We have no way of determining if a subr is unevalled
;   (if (and (subrp (symbol-function function))
; 	   (eq (cdr (subr-arity (symbol-function function))) 'unevalled))
;       (error "Function %s is a special form" function))
  (if (or (symbolp (symbol-function function))
	  (subrp (symbol-function function)))
      ;; Create a wrapper in which we can then add the necessary debug call.
      (fset function `(lambda (&rest debug-on-entry-args)
			,(interactive-form (symbol-function function))
			(apply ',(symbol-function function)
			       debug-on-entry-args))))
  (or (consp (symbol-function function))
      (debug-convert-byte-code function))
  (or (consp (symbol-function function))
      (error "Definition of %s is not a list" function))
  (fset function (debug-on-entry-1 function (symbol-function function) t))
  (or (memq function debug-function-list)
      (push function debug-function-list))
  function)

;;;###autoload
(defun cancel-debug-on-entry (&optional function)
  "Undo effect of \\[debug-on-entry] on FUNCTION.
If argument is nil or an empty string, cancel for all functions."
  (interactive
   (list (let ((name
		(completing-read "Cancel debug on entry (to function): "
				 (mapcar (lambda (sym) 
                                           (cons (symbol-name sym) sym)) 
                                         debug-function-list)
				 nil t nil)))
	   (if name (intern name)))))
  (debugger-reenable)
  (if (and function (not (string= function "")))
      (progn
	(let ((f (debug-on-entry-1 function (symbol-function function) nil)))
	  (condition-case nil
	      (if (and (equal (nth 1 f) '(&rest debug-on-entry-args))
		       (eq (car (nth 3 f)) 'apply))
		  ;; `f' is a wrapper introduced in debug-on-entry.
		  ;; Get rid of it since we don't need it any more.
		  (setq f (nth 1 (nth 1 (nth 3 f)))))
	    (error nil))
	  (fset function f))
	(setq debug-function-list (delq function debug-function-list))
	function)
    (message "Cancelling debug-on-entry for all functions")
    (mapcar 'cancel-debug-on-entry debug-function-list)))

(defun debug-convert-byte-code (function)
  (let ((defn (symbol-function function)))
    (if (not (consp defn))
	;; Assume a compiled-function object.
	(let* ((body
		(list (list 'byte-code
			    (compiled-function-instructions defn)
			    (compiled-function-constants defn)
			    (compiled-function-stack-depth defn)))))
	  (if (compiled-function-interactive defn)
	      (setq body (cons (compiled-function-interactive defn) body)))
	  (if (compiled-function-doc-string defn)
	      ;; Use `documentation' here, to get the actual string,
	      ;; in case the compiled function has a reference
	      ;; to the .elc file.
	      (setq body (cons (documentation function) body)))
	  (fset function (cons 'lambda (cons
					(compiled-function-arglist defn)
					body)))))))

(defun debug-on-entry-1 (function defn flag)
  (if (subrp defn)
      (error "%s is a built-in function" function)
    (if (eq (car defn) 'macro)
	(debug-on-entry-1 function (cdr defn) flag)
      (or (eq (car defn) 'lambda)
	  (error "%s not user-defined Lisp function" function))
      (let ((tail (cddr defn)))
	;; Skip the docstring.
	(if (stringp (car tail)) (setq tail (cdr tail)))
	;; Skip the interactive form.
	(if (eq 'interactive (car-safe (car tail))) (setq tail (cdr tail)))
	(unless (eq flag (equal (car tail) '(debug 'debug)))
	  ;; Add/remove debug statement as needed.
	  (if (not flag)
	      (progn (setcar tail (cadr tail))
		     (setcdr tail (cddr tail)))
	    (setcdr tail (cons (car tail) (cdr tail)))
	    (setcar tail '(debug 'debug))))
	defn))))

(defun debugger-list-functions ()
  "Display a list of all the functions now set to debug on entry."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (if (null debug-function-list)
	(princ "No debug-on-entry functions now\n")
      (princ "Functions set to debug on entry:\n\n")
      (let ((list debug-function-list))
	(while list
	  (prin1 (car list))
	  (terpri)
	  (setq list (cdr list))))
      (princ "Note: if you have redefined a function, then it may no longer\n")
      (princ "be set to debug on entry, even if it is in the list."))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))))

;;; convenience functions for turning on and off debug-on varibles

;;;###autoload
(defun toggle-debug-on-error (&optional arg)
  "Toggle the status of debug-on-error.
With arg, set debug-on-error iff arg is positive."
  (interactive "_P")
  (setq debug-on-error
        (if (null arg)
            (not debug-on-error)
	  (> (prefix-numeric-value arg) 0))))

;;;###autoload
(defun toggle-debug-on-signal (&optional arg)
  "Toggle the status of debug-on-signal.
With arg, set debug-on-signal iff arg is positive."
  (interactive "_P")
  (setq debug-on-signal
        (if (null arg)
            (not debug-on-signal)
	  (> (prefix-numeric-value arg) 0))))

;;;###autoload
(defun toggle-debug-on-quit (&optional arg)
  "Toggle the status of debug-on-quit.
With arg, set debug-on-quit iff arg is positive."
  (interactive "_P")
  (setq debug-on-quit
        (if (null arg)
            (not debug-on-quit)
	  (> (prefix-numeric-value arg) 0))))

(provide 'debug)

;;; debug.el ends here
