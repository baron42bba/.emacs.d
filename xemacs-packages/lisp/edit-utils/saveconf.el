;;; Save Emacs buffer and window configuration between editing sessions.
;;; Copyright (C) 1987, 1988, 1989 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from the
;;; program's author (send electronic mail to kyle@cs.odu.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle@cs.odu.edu.

;;; Synched up with: Not in FSF.

;; This package of functions gives Emacs the ability to remember which
;; files were being visited, the windows that were on them, and the
;; value of point in their buffers the last Emacs session in the same
;; directory.  This is an emulation of an old Gosling Emacs feature.
;;
;; The relevant commands are save-context and recover-context.
;;
;; Most of the time you'll want an Emacs session's context saved even if
;; you choose not to recover it later.  To avoid having to manually
;; M-x save-context at each emacs exit, put the line:
;;    (setq auto-save-and-recover-context t)
;; in your .emacs or in default.el in the lisp directory of the Emacs
;; distribution.  The context will then automatically be saved when
;; Emacs exits.
;;
;; By default only the contexts of visible buffers (buffers with windows
;; on them) are saved.  Setting the variable save-buffer-context to t
;; causes the contexts of all buffers to be saved.
;;
;; Put this file in the "lisp" directory of the emacs distribution in a
;; file called saveconf.el.  Byte-compile it.
;;
;; There are two ways to use this package.
;;   1) Put the line
;;       (require 'saveconf)
;;      in the file site-init.el in the lisp directory of the Emacs
;;      directory and rebuild Emacs.  If you get the "Pure Lisp storage
;;      exhausted" error message when rebuilding Emacs, increase PURESIZE
;;      in src/config.h by about 30000 bytes and try again.  It's almost
;;      certain that this will happen to you so you might as well increase
;;      PURESIZE beforehand.
;;
;;      This is the preferred mode of operation because it allows the
;;      package to become part of Emacs' startup sequence and automatically
;;      restore context in a directory if Emacs is invoked without any
;;      command line arguments.
;;
;;   2) Put these lines
;;       (require 'saveconf)
;;       (if (null (cdr command-line-args))
;;           (setq inihibit-startup-message (recover-context)))
;;      at the end of your .emacs file or the default.el file in the
;;      lisp directory of the Emacs distribution.  This causes the
;;      context saved in the current directory to be recovered whenever
;;      Emacs is invoked without any arguments.

(provide 'saveconf)

(defconst save-context-version "Norma Jean"
  "A unique string which is placed at the beginning of every saved context
file.  If the string at the beginning of the context file doesn't match the
value of this variable the `recover-context' command will ignore the file's
contents.")

(defvar auto-save-and-recover-context nil
  "*If non-nil the `save-context' command will always be run before Emacs is
exited.  Also upon Emacs startup, if this variable is non-nil and Emacs is
passed no command line arguments, `recover-context' will be run.")

(defvar save-buffer-context nil
  "*If non-nil the `save-context' command will save the context
of buffers that are visiting files, as well as the contexts of buffers
that have windows.")

(defvar save-context-predicate
  (function (lambda (w)
	      (and (buffer-file-name (window-buffer w))
		   (not (string-match "^\\(/usr\\)?/tmp/"
				      (buffer-file-name (window-buffer w)))))))
  "*Value is a predicate function which determines which windows' contexts
are saved.  When the `save-context' command is invoked, this function will
be called once for each existing Emacs window.  The function should accept
one argument which will be a window object, and should return non-nil if
the window's context should be saved.")


;; kill-emacs' function definition must be saved
(if (not (fboundp 'just-kill-emacs))
    (fset 'just-kill-emacs (symbol-function 'kill-emacs)))

;; Make Emacs call recover-context at startup if appropriate.
(setq top-level
      (list 'let '((starting-up (not command-line-processed)))
	    (list 'prog1
		  top-level
		  '(and starting-up auto-save-and-recover-context
			(null (cdr command-line-args)) (recover-context)))))

(defun kill-emacs (&optional query)
  "End this Emacs session.
Prefix ARG or optional first ARG non-nil means exit with no questions asked,
even if there are unsaved buffers.  If Emacs is running non-interactively
and ARG is an integer, then Emacs exits with ARG as its exit code.

If the variable `auto-save-and-restore-context' is non-nil,
the function save-context will be called first."
  (interactive "P")
  ;; check the purify flag.  try to save only if this is a dumped Emacs.
  ;; saving context from a undumped Emacs caused a NULL pointer to be
  ;; referenced through.  I'm not sure why.
  (if (and auto-save-and-recover-context (null purify-flag))
      (save-context))
  (just-kill-emacs query))

(defun save-context ()
  "Save context of all Emacs windows (files visited and position of point).
The information goes into a file called .emacs_<username> in the directory
where the Emacs session was started.  The context can be recovered with the
`recover-context' command, provided you are in the same directory where
the context was saved.

If the variable `save-buffer-context' is non-nil, the context of all buffers
visiting files will be saved as well.

Window sizes and shapes are not saved, since these may not be recoverable
on terminals with a different number of rows and columns."
  (interactive)
  (condition-case error-data
      (let (context-buffer mark save-file-name)
	(setq save-file-name (concat (original-working-directory)
				     ".emacs_" (user-login-name)))
	(if (not (file-writable-p save-file-name))
	    (if (file-writable-p (original-working-directory))
		(error "context is write-protected, %s" save-file-name)
	      (error "can't access directory, %s"
		     (original-working-directory))))
	;;
	;; set up a buffer for the saved context information
	;; Note that we can't set the visited file yet, because by
	;; giving the buffer a file to visit we are making it
	;; eligible to have it's context saved.
	;;
	(setq context-buffer (get-buffer-create " *Context Info*"))
	(set-buffer context-buffer)
	(erase-buffer)
	(set-buffer-modified-p nil)
	;;
	;; record the context information
	;;
	(mapcar
	 (function
	  (lambda (w)
	    (cond ((funcall save-context-predicate w)
		   (prin1 (buffer-file-name (window-buffer w)) context-buffer)
		   (princ " " context-buffer)
		   (prin1 (window-point w) context-buffer)
		   (princ "\n" context-buffer)))))
	 (window-list))
	
	;;
	;; nil is the data sentinel.  We will insert it later if we
	;; need it but for now just remember where the last line of
	;; window context ended.
	;;
	(setq mark (point))

	;;
	;; If `save-buffer-context' is non-nil we save buffer contexts.
	;;
	(if save-buffer-context
	    (mapcar
	     (function
	      (lambda (b)
		(set-buffer b)
		(cond (buffer-file-name
		       (prin1 buffer-file-name context-buffer)
		       (princ " " context-buffer)
		       (prin1 (point) context-buffer)
		       (princ "\n" context-buffer)))))
	     (buffer-list)))

	;;
	;; If the context-buffer contains information, we add the version
	;;   string and sentinels, and write out the saved context.
	;; If the context-buffer is empty, we don't create a file at all.
	;; If there's an old saved context in this directory we attempt
	;;   to delete it.
	;;
	(cond ((buffer-modified-p context-buffer)
	       (set-buffer context-buffer)
	       (setq buffer-offer-save nil)
	       ;; sentinel for EOF
	       (insert "nil\n")
	       ;; sentinel for end of window contexts
	       (goto-char mark)
	       (insert "nil\n")
	       ;; version string
	       (goto-char (point-min))
	       (prin1 save-context-version context-buffer)
	       (insert "\n\n")
	       ;; so kill-buffer won't need confirmation later
	       (set-buffer-modified-p nil)
	       ;; save it
	       (write-region (point-min) (point-max) save-file-name
			     nil 'quiet))
	      (t (condition-case data
		     (delete-file save-file-name) (error nil))))

	(kill-buffer context-buffer))
    (error nil)))

(defun recover-context ()
  "Recover an Emacs context saved by `save-context' command.
Files that were visible in windows when the context was saved are visited and
point is set in each window to what is was when the context was saved."
  (interactive)
  (condition-case error-data
      ;;
      ;; Set up some local variables.
      ;;
      (let (sexpr context-buffer recover-file-name)
	(setq recover-file-name (concat (original-working-directory)
					".emacs_" (user-login-name)))
	(if (not (file-readable-p recover-file-name))
	    (error "can't access context, %s" recover-file-name))
	;;
	;; create a temp buffer and copy the saved context into it.
	;;
	(setq context-buffer (get-buffer-create " *Recovered Context*"))
	(set-buffer context-buffer)
	(erase-buffer)
	(insert-file-contents recover-file-name nil)
	;; so kill-buffer won't need confirmation later
	(set-buffer-modified-p nil)
	;;
	;; If it's empty forget it.
	;;
	(if (zerop (buffer-size))
	    (error "context file is empty, %s" recover-file-name))
	;;
	;; check the version and make sure it matches ours
	;;
	(setq sexpr (read context-buffer))
	(if (not (equal sexpr save-context-version))
	    (error "version string incorrect, %s" sexpr))
	;;
	;; Recover the window contexts
	;;
	(while (setq sexpr (read context-buffer))
	  (select-window (get-largest-window))
	  (if (buffer-file-name)
	      (split-window))
	  (other-window 1)
	  (find-file sexpr)
	  (goto-char (read context-buffer)))
	;;
	;; Recover buffer contexts, if any.
	;;
	(while (setq sexpr (read context-buffer)
                     point (read context-buffer))
	  (set-buffer (find-file-noselect sexpr t))
	  (if (zerop (buffer-size))
	      (kill-buffer (current-buffer))
	    (goto-char point)))
	(bury-buffer "*scratch*")
	(kill-buffer context-buffer)
	t )
    (error nil)))
	 
(defun original-working-directory ()
  (save-excursion
    (set-buffer (get-buffer-create "*scratch*"))
    default-directory))
