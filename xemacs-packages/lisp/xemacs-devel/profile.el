;;; profile.el --- basic profiling commands for XEmacs

;; Copyright (C) 1996, 2002, 2003, 2004 Ben Wing.
;; Copyright (C) 1997 Free Software Foundation.

;; Author: Ben Wing <ben@xemacs.org>
;; Maintainer: XEmacs Development Team
;; Keywords: internal

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; In addition to Lisp-based `elp', XEmacs provides a set of
;; primitives able to profile evaluation of Lisp functions, created by
;; the illustrious Ben Wing.  The functions in this file can be used
;; to gain easy access to the internal profiling functions.

;; The profiler works by catching "ticks" (actually SIGPROF signals),
;; and looking at the current Lisp function, at the time of each tick.
;; In addition, call counts for each function called are recorded.
;; `profile-results' pretty-prints this information.

;; Note (ELP users should read this):
;;
;; 1) Both function and function + descendants time are reported.
;; 2) Number of times called is recorded for each function, as well as
;;    Lisp allocation for the functions and for function + descendants.
;; 3) The profiling entries include not only functions but also certain
;;    common internal operations, such as redisplay, garbage collection,
;;    byte-char conversion, internal-external conversion, etc.  Profiling
;;    overhead is also recorded.
;; 4) Each tick is equivalent to 1ms (which can be changed), but this
;;    is CPU time (user+kernel), not the real time.
;; 5) Only the actual funcalls are profiled.  If, in the C code, a subr
;;    Ffoo calls Fbar directly, using Fbar(), only Ffoo will appear in
;;    the profile.
;; 6) When profiling complex forms, more meaningful results are achieved
;;    by byte-compiling, e.g. with `compile-and-profile'.
;; 7) Results will be skewed if your XEmacs is compiled with error-
;;    checking.  The best thing is to compile with optimization and without
;;    error-checking, before profiling.
;; 8) If you call `profile-key-sequence' and then hit a sequence starting
;;    with `M-x ...', the profiling will start from when you hit M-x,
;;    and will include the reading of the rest of the command.

;; There are three levels of functions:
;;
;; -- High-level interactive commands for profiling key sequences, commands,
;;    and expressions (`profile-key-sequence', `profile-command',
;;    `profile-expression'), or for interactively profiling any sequence of
;;    commands (`toggle-profiling').
;;
;; -- Functions for profiling forms, optionally byte-compiled (`profile',
;;    `continue-profile', `compile-and-profile',
;;    `compile-and-continue-profile'), which can be conveniently executed
;;    from the `Eval:' prompt or *scratch* buffer.  Use the `compile-and-'
;;    versions with complex forms, for more meaningful results.
;;
;; -- The basic API, for complete control over the profiling process
;;    (`clear-profiling-info', `start-profiling', `stop-profiling',
;;     `profile-results', `get-profiling-info', `combine-profiling-info',
;;     `save-profiling-info').
;;
;; For instance, to see where Gnus spends time when generating
;; Summary buffer, go to the group buffer and press
;; `M-x profile-key-sequence RET SPC'; or just do
;; `M-x toggle-profiling', hit SPC, and do `M-x toggle-profiling' again.
;; (This is especially convenient if you bind `toggle-profiling' to a
;; keystroke.)


;;; Code:

;;; FIXME: This is (almost) a direct copy of `with-displaying-help-buffer'
;; that exists only in 21.5 core.  When 21.5 is stable I'd suggest
;; that this macro be removed and the call to it in `profile-results'
;; (see fixme comment in that function) below be changed to use the
;; macro in core.  This is here because the packages are built with
;; 21.4 so when this file is byte-compiled it doesn't include the
;; definition of the `with-displaying-temp-buffer'.  Which was OK for
;; 21.4 users, but 21.5 users would lose. --SY.
(defmacro profile-displaying-temp-buffer (name &rest body)
  "Form which makes a help buffer with given NAME and evaluates BODY there.

Use this function for displaying information in temporary buffers, where the
user will typically view the information and then exit using
\\<temp-buffer-mode-map>\\[help-mode-quit].

The buffer is put into the mode specified in `mode-for-temp-buffer'."
  `(let* ((winconfig (current-window-configuration))
	  (was-one-window (one-window-p))
	  (buffer-name ,name)
	  (help-not-visible
	   (not (and (windows-of-buffer buffer-name) ;shortcut
		     (memq (selected-frame)
			   (mapcar 'window-frame
				   (windows-of-buffer buffer-name)))))))
    (help-register-and-maybe-prune-excess buffer-name)
    ;; if help-sticky-window is bogus or deleted, get rid of it.
    (if (and help-sticky-window (or (not (windowp help-sticky-window))
				    (not (window-live-p help-sticky-window))))
	(setq help-sticky-window nil))
    (prog1
	(let ((temp-buffer-show-function
	       (if help-sticky-window
		   #'(lambda (buffer)
		       (set-window-buffer help-sticky-window buffer))
		 temp-buffer-show-function)))
	  (with-output-to-temp-buffer buffer-name
	    (prog1 (progn ,@body)
	      (save-excursion
		(set-buffer standard-output)
		(funcall mode-for-help)))))
      (let ((helpwin (get-buffer-window buffer-name)))
	(when helpwin
	  ;; If the temp buffer is already displayed on this
	  ;; frame, don't override the previous configuration
	  (when help-not-visible
	    (with-current-buffer (window-buffer helpwin)
	      (setq help-window-config winconfig)))
	  (when help-selects-help-window
	    (select-window helpwin))
	  (cond ((eq helpwin (selected-window))
		 (display-message 'command
		   (substitute-command-keys "Type \\[help-mode-quit] to remove window, \\[scroll-up] to scroll the text.")))
		(was-one-window
		 (display-message 'command
		   (substitute-command-keys "Type \\[delete-other-windows] to remove window, \\[scroll-other-window] to scroll the text.")))
		(t
		 (display-message 'command
		   (substitute-command-keys "Type \\[switch-to-buffer-other-window] to restore the other window, \\[scroll-other-window] to scroll the text.")))))))))

(put 'profile-displaying-temp-buffer 'lisp-indent-function 1)

;;;###autoload
(defun profile-results (&optional info stream sort-by)
  "Print profiling info INFO to STREAM in a pretty format.
If INFO is omitted, the current profiling info is retrieved using
 `get-profiling-info'.
If STREAM is omitted, the results will be displayed in a temp buffer
 using `with-output-to-temp-buffer'; otherwise, they will simply be
 printed into STREAM.  Use `standard-output' explicitly if you
 want standard output.
If SORT-BY is `call-count' (interactively, the prefix arg), display items
 sorted by call count rather than timing.  If `gc-usage' (interactively,
 use C-u C-u), sort by GC usage."
  (interactive (list nil nil (cond ((equal current-prefix-arg '(16))
				    'gc-usage)
				   (current-prefix-arg 'call-count))))
  (or info (setq info (get-profiling-info)))
  (if (not stream)
      ;; FIXME: change this to `with-displaying-temp-buffer' when that
      ;; exists in stable XEmacs. --SY.
      (profile-displaying-temp-buffer "*Profiling Results*"
	(profile-results info standard-output sort-by))
    (let* ((standard-output stream)
	   ;; #### Support old profile format for the moment
	   (timing (if (consp (car info)) (copy-alist info)
		     (loop for x being the hash-key in (getf info 'timing)
		       using (hash-value y)
		       collect (cons x y))))
	   (total-timing (if (boundp 'call-count-profile-table)
			       (make-hash-table)
			     (getf info 'total-timing)))
	   (call-count (if (boundp 'call-count-profile-table)
			   call-count-profile-table
			 (getf info 'call-count)))
	   (gc-usage (if (boundp 'call-count-profile-table)
			 (make-hash-table)
		       (getf info 'gc-usage)))
	   (total-gc-usage (if (boundp 'call-count-profile-table)
			       (make-hash-table)
			     (getf info 'total-gc-usage)))
	   (spaces-for-data 41)
	   (spaces-for-fun (- 79 spaces-for-data))
	   maxfunlen)
      (loop for table in (list total-timing call-count gc-usage total-gc-usage)
	do
	;; Add entries for any functions in other tables but no ticks
	(loop for x being the hash-key in table using (hash-value y) do
	  (if (not (assoc x timing))
	      (push (cons x 0) timing))))
      ;; Calculate the longest function
      (setq maxfunlen
	    (apply #'max
		   (length "Function Name")
		   (mapcar
		    (lambda (el)
		      (let ((l (length (format "%s" (car el)))))
			(if (<= l spaces-for-fun)
			    l 0)))
		    timing)))
      (princ (format "%-*sTicks/Total %%Usage Calls GC-Usage/  Total\n"
		     maxfunlen "Function Name"))
      (princ (make-string maxfunlen ?=))
      (princ "=====/===== ====== ===== ========/=======\n")
      (let ((timing-sum (float (apply #'+ (mapcar #'cdr timing))))
	    (calls-sum 0)
	    (gc-sum 0))
	(dolist (entry
		 (nreverse
		  (sort timing
			(cond ((eq sort-by 'call-count)
			       #'(lambda (a b)
				   (< (or (gethash (car a) call-count) 0)
				      (or (gethash (car b) call-count) 0))))
			      ((eq sort-by 'gc-usage)
			       #'(lambda (a b)
				   (< (or (gethash (car a) gc-usage) 0)
				      (or (gethash (car b) gc-usage) 0))))
			      (t #'cdr-less-than-cdr)))))
	  (princ (format "%-*s%5d/%5d %6.3f %s %s\n"
			 maxfunlen
			 ;; if function too long (often lambdas or compiled
			 ;; funs), put in a newline to keep the alignment
			 (let ((str (format "%s" (car entry))))
			   (if (<= (length str) maxfunlen) str
			     (concat str "\n" (make-string maxfunlen ? ))))
			 (cdr entry)
			 (or (gethash (car entry) total-timing) 0)
			 (if (zerop timing-sum)
			     0
			   (* 100 (/ (cdr entry) timing-sum)))
			 (let ((count (gethash (car entry) call-count)))
			   (if count (format "%5d" count) "     "))
			 (let ((gcval (or (gethash (car entry) gc-usage) 0))
			       (total-gcval
				(or (gethash (car entry) total-gc-usage) 0)))
			   (if (or (/= gcval 0) (/= total-gcval 0))
			       (format "%8d/%7d" gcval total-gcval)
			     "                "))
			 ))
	  (incf calls-sum (or (gethash (car entry) call-count 0)))
	  (incf gc-sum (or (gethash (car entry) gc-usage 0)))
	  )
	(princ (make-string (+ maxfunlen spaces-for-data) ?-))
	(princ (format "\n%-*s%7d      %7.3f %5d %8d\n"
		       (- maxfunlen 2) "Total" timing-sum 100.0 calls-sum
		       gc-sum))
	(princ (format "\n
Ticks/Total     = Ticks this function/this function and descendants
Calls           = Number of calls to this function
GC-Usage/Total  = Lisp allocation this function/this function and descendants
One tick        = %g ms\n"
		       (/ default-profiling-interval 1000.0)))
	(and (boundp 'internal-error-checking)
	     (delq 'quick-build internal-error-checking)
	     (princ "
WARNING: Error checking is turned on in this XEmacs.  This might make
         the measurements very unreliable.\n"))))))

;;;###autoload
(defun combine-profiling-info (&rest info)
  "Add up the profiling results accumulated during many profiling sessions.
See `profile'."
  (if (boundp 'call-count-profile-table)
      ;; #### old format
      (let ((hash (make-hash-table :test 'equal)))
	(loop for i in info do
	  (loop for (x . y) in i do
	    (puthash x (+ y (or (gethash x hash) 0)) hash)))
	(loop for x being the hash-key in hash using (hash-value y)
	  collect (cons x y)))
    (let ((ninfo (list 'timing (make-hash-table :test 'equal)
		       'total-timing (make-hash-table :test 'equal)
		       'call-count (make-hash-table :test 'equal)
		       'gc-usage (make-hash-table :test 'equal)
		       'total-gc-usage (make-hash-table :test 'equal)
		       )))
      (loop
	for i in info do
	(loop for (key hash) on i by #'cddr
	  for reshash = (getf ninfo key) do
	  (loop for x being the hash-key in hash using (hash-value y) do
	    (puthash x (+ (or y 0) (or (gethash x reshash) 0)) reshash))))
      ninfo)))

;;;###autoload
(defmacro save-profiling-info (&rest body)
  "Execute BODY, preserving the profiling info and profiling on-ness."
  (let ((old-profiling-info (gensym "spi"))
	(old-was-profiling (gensym "spi")))
  `(let ((,old-profiling-info (get-profiling-info))
	 (,old-was-profiling (profiling-active-p)))
     (unwind-protect
	 (progn ,@body)
       (if (not (eq ,old-was-profiling (profiling-active-p)))
	   (if ,old-was-profiling (start-profiling) (stop-profiling)))
       (set-profiling-info ,old-profiling-info)))))

;;;###autoload
(defmacro profile (&rest forms)
  "Profile FORMS and display results in a temporary buffer.
This clears out existing profiling info, turns on profiling, executes
the forms, turns off profiling, and displays the results.

If you want to accumulate the results of multiple profiling runs, you can
use `continue-profile', which does not clear out existing profiling info.

If you are looking for high-level interactive commands for profiling key
sequences, commands, and expressions, see `profile-key-sequence',
`profile-command', and `profile-expression'.

See also `toggle-profiling', which lets you easily profile any sequence
of commands.

If you need more control over what is profiled and what isn't, use the more
basic functions `clear-profiling-info', `start-profiling',
`stop-profiling', `profile-results', `get-profiling-info',
`combine-profiling-info' and `save-profiling-info'."
  `(progn
    (clear-profiling-info)
     (unwind-protect
	 (progn
	   (start-profiling)
	   ,@forms)
       (stop-profiling))
    (profile-results)))

;;;###autoload
(defmacro continue-profile (&rest forms)
  "Profile FORMS, combining the results with previous profile runs.
Display results in a temporary buffer.  Unlike `profile', this does
not clear out existing profile information first, and will leave profiling
on if it was already on when this macro was invoked."
  `(let ((was-profiling (profiling-active-p)))
     (unwind-protect
	 (progn
	   (start-profiling)
	   ,@forms)
       (unless was-profiling
	 (stop-profiling)))
     (profile-results)))

(put 'profile 'lisp-indent-function 0)
(put 'continue-profile 'lisp-indent-function 0)

;;;###autoload
(defun profile-expression (expr &optional arg)
  "Eval EXPR, profiling the execution and displaying the results.
With prefix, combine results with results from a previous run."
  (interactive (list (read (read-string "Expression to profile: "))
		     current-prefix-arg))
  (if arg (continue-profile (eval expr))
    (profile (eval expr))))

;;;###autoload
(defun profile-command (command &optional arg)
  "Run COMMAND, profiling the execution and displaying the results.
With prefix, combine results with results from a previous run."
  (interactive "CCommand to profile: \nP")
  (if arg (continue-profile (call-interactively command))
    (profile (call-interactively command))))

;;;###autoload
(defun profile-key-sequence (keys &optional arg)
  "Dispatch the key sequence KEYS, profile the execution and show the results.
KEYS can be a vector of keypress events, a keypress event, or a character.
With prefix, combine results with results from a previous run."
  (interactive "kProfile keystroke: \nP")
  (and (characterp keys)
       (setq keys (character-to-event keys)))
  (or (vectorp keys)
      (setq keys (vector keys)))
  (if arg (continue-profile (mapc 'dispatch-event keys))
    (profile (mapc 'dispatch-event keys))))

;;;###autoload
(defun toggle-profiling ()
  "Start profiling, or stop it and print results.
This lets you figure out where time is being spent when executing Lisp code."
  (interactive)  
  (if (profiling-active-p) 
      (progn  
	(stop-profiling) 
	(message "...Finished profiling")
	(profile-results))
    (message "Profiling...") 
    (clear-profiling-info) 
    (start-profiling)))

;;;###autoload
(defmacro compile-and-profile (&rest forms)
  "Byte compile FORMS, profile the execution, and pretty-print the results."
  `(progn
     (flet ((compiled-code-being-profiled () ,@forms))
       (byte-compile 'compiled-code-being-profiled)
       (profile (compiled-code-being-profiled)))))

;;;###autoload
(defmacro compile-and-continue-profile (&rest forms)
  "Like `compile-and-profile' but combine results with previous profile runs."
  `(progn
     (flet ((compiled-code-being-profiled () ,@forms))
       (byte-compile 'compiled-code-being-profiled)
       (continue-profile (compiled-code-being-profiled)))))

;;; profile.el ends here
