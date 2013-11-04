;;; timer-funcs.el --- timer implementation-independent functions

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Maintainer: FSF

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains functions from Emacs timer.el that do not take timer
;; objects as parameters.  This lets us use itimers as the implementation,
;; instead of emulated Emacs timers.  Note that a few of these functions
;; RETURN itimers, however.

;;; Code:

(require 'itimer)

(defun timer-next-integral-multiple-of-time (time secs)
  "Yield the next value after TIME that is an integral multiple of SECS.
More precisely, the next value, after TIME, that is an integral multiple
of SECS seconds since the epoch.  SECS may be a fraction."
  (let* ((time-base (ash 1 16))
	 (float-time-base (float time-base))
	 (million 1000000.0)
	 (time-usec (+ (* million (+ (* float-time-base (nth 0 time))
				     (nth 1 time)))
		       (nth 2 time)))
	 (secs-usec (* million secs))
	 (mod-usec (mod time-usec secs-usec))
	 (next-usec (+ (- time-usec mod-usec) secs-usec))
	 (time-base-million (* float-time-base million)))
    (list (floor next-usec time-base-million)
	  (floor (mod next-usec time-base-million) million)
	  (floor (mod next-usec million)))))

(defun timer-relative-time (time secs &optional usecs)
  "Advance TIME by SECS seconds and optionally USECS microseconds.
SECS may be a fraction."
  (let ((high (car time))
	(low (if (consp (cdr time)) (nth 1 time) (cdr time)))
	(micro (if (numberp (car-safe (cdr-safe (cdr time))))
		   (nth 2 time)
		 0)))
    ;; Add
    (if usecs (setq micro (+ micro usecs)))
    (if (floatp secs)
	(setq micro (+ micro (floor (* 1000000 (- secs (floor secs)))))))
    (setq low (+ low (floor secs)))

    ;; Normalize
    ;; `/' rounds towards zero while `mod' returns a positive number,
    ;; so we can't rely on (= a (+ (* 100 (/ a 100)) (mod a 100))).
    (setq low (+ low (/ micro 1000000) (if (< micro 0) -1 0)))
    (setq micro (mod micro 1000000))
    (setq high (+ high (/ low 65536) (if (< low 0) -1 0)))
    (setq low (logand low 65535))

    (list high low (and (/= micro 0) micro))))

(defun cancel-function-timers (function)
  "Cancel all itimers scheduled by `run-at-time' which would run FUNCTION."
  (interactive "aCancel itimers of function: ")
  (dolist (itimer itimer-list)
    (if (eq (itimer-function itimer) function)
	(delete-itimer itimer))))

(define-error 'invalid-repetition-interval "Invalid repetition interval")
(define-error 'invalid-time "Invalid time format")

;;;###autoload
(defun run-at-time (time repeat function &rest args)
  "Perform an action at time TIME.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
TIME should be a string recognized by `timer-parse-time', like \"11:23pm\",
nil meaning now, a number of seconds from now, a value from `current-time',
or t (with non-nil REPEAT) meaning the next integral multiple of REPEAT.
REPEAT may be an integer or floating point number.
The action is to call FUNCTION with arguments ARGS.

This function returns an itimer object which you can use in `delete-itimer'."
  (interactive "sRun at time: \nNRepeat interval: \naFunction: ")

  (or (null repeat)
      (and (numberp repeat) (< 0 repeat))
      (error 'invalid-repetition-interval repeat))

  ;; Special case: nil means "now" and is useful when repeating.
  (if (null time)
      (setq time 0))

  ;; Special case: t means the next integral multiple of REPEAT.
  (if (and (eq time t) repeat)
      (let ((now (current-time)))
	(setq time
	      (itimer-time-difference
	       (timer-next-integral-multiple-of-time now repeat)
	       now))))

  ;; Handle relative times like "2 hours and 35 minutes"
  (if (stringp time)
      (let ((spec-time (timer-duration time)))
	(if spec-time
	    (setq time spec-time))))

  ;; Handle "11:23pm" and the like.  Interpret it as meaning today
  ;; which admittedly is rather stupid if we have passed that time
  ;; already.  (Though only Emacs hackers hack Emacs at that time.)
  (if (stringp time)
      (let ((spec-time (timer-parse-time time)))
	(if spec-time
	    (setq time
		  (ceiling
		   (itimer-time-difference spec-time (current-time)))))))

  (or (numberp time)
      (error 'invalid-time time))

  (apply #'start-itimer "run-at-time" function time repeat nil t args))

;;;###autoload
(defun run-with-timer (secs repeat function &rest args)
  "Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
SECS and REPEAT may be integers or floating point numbers.
The action is to call FUNCTION with arguments ARGS.

This function returns an itimer object which you can use in `delete-itimer'."
  (interactive "sRun after delay (seconds): \nNRepeat interval: \naFunction: ")
  (apply #'start-itimer "run-with-timer" function secs repeat nil t args))

;;;###autoload
(defun run-with-idle-timer (secs repeat function &rest args)
  "Perform an action the next time XEmacs is idle for SECS seconds.
The action is to call FUNCTION with arguments ARGS.
SECS may be an integer or a floating point number.

If REPEAT is non-nil, do the action each time XEmacs has been idle for
exactly SECS seconds (that is, only once for each time XEmacs becomes idle).

This function returns an itimer object which you can use in `delete-itimer'."
  (interactive
   (list (read-from-minibuffer "Run after idle (seconds): " nil nil t)
	 (y-or-n-p "Repeat each time XEmacs is idle? ")
	 (intern (completing-read "Function: " obarray 'fboundp t))))
  (apply #'start-itimer "run-with-idle-timer" function secs
	 (if repeat secs nil) t t args))

(defun with-timeout-handler (tag)
  (throw tag 'timeout))

;;;###autoload (put 'with-timeout 'lisp-indent-function 1)

;;;###autoload
(defmacro with-timeout (list &rest body)
  "Run BODY, but if it doesn't finish in SECONDS seconds, give up.
If we give up, we run the TIMEOUT-FORMS and return the value of the last one.
The call should look like:
 (with-timeout (SECONDS TIMEOUT-FORMS...) BODY...)
The timeout is checked whenever XEmacs waits for some kind of external
event \(such as keyboard input, input from subprocesses, or a certain time);
if the program loops without waiting in any way, the timeout will not
be detected."
  (let ((seconds (car list))
	(timeout-forms (cdr list)))
    `(let ((with-timeout-tag (cons nil nil))
	   with-timeout-value with-timeout-timer)
       (unwind-protect
	   (when (catch with-timeout-tag
		   (progn
		     (setq with-timeout-timer
			   (start-itimer "with-timeout" #'with-timeout-handler
					 ,seconds nil nil t with-timeout-tag))
		     (setq with-timeout-value (progn ,@body))
		     nil))
	     ,@timeout-forms
	     with-timeout-value)
	 (delete-itimer with-timeout-timer)))))


;;;###autoload
(defun y-or-n-p-with-timeout (prompt seconds default-value)
  "Like (y-or-n-p PROMPT), with a timeout.
If the user does not answer after SECONDS seconds, return DEFAULT-VALUE."
  (with-timeout (seconds default-value)
    (y-or-n-p prompt)))

(defvar timer-duration-words
  (list (cons "microsec" 0.000001)
	(cons "microsecond" 0.000001)
        (cons "millisec" 0.001)
	(cons "millisecond" 0.001)
        (cons "sec" 1)
	(cons "second" 1)
	(cons "min" 60)
	(cons "minute" 60)
	(cons "hour" (* 60 60))
	(cons "day" (* 24 60 60))
	(cons "week" (* 7 24 60 60))
	(cons "fortnight" (* 14 24 60 60))
	(cons "month" (* 30 24 60 60))	  ; Approximation
	(cons "year" (* 365.25 24 60 60)) ; Approximation
	)
  "Alist mapping temporal words to durations in seconds")

(defun timer-duration (string)
  "Return number of seconds specified by STRING, or nil if parsing fails."
  (let ((secs 0)
	(start 0)
	(case-fold-search t))
    (while (string-match
	    "[ \t]*\\([0-9.]+\\)?[ \t]*\\([a-z]+[a-rt-z]\\)s?[ \t]*"
	    string start)
      (let ((count (if (match-beginning 1)
		       (string-to-number (match-string 1 string))
		     1))
	    (itemsize (cdr (assoc (match-string 2 string)
				  timer-duration-words))))
	(if itemsize
	    (setq start (match-end 0)
		  secs (+ secs (* count itemsize)))
	  (setq secs nil
		start (length string)))))
    (if (= start (length string))
	secs
      (if (string-match "\\`[0-9.]+\\'" string)
	  (string-to-number string)))))

(defun timer-parse-time (s)
  "Return an encoded time represesnting the time specifed by S.
This is a time in the same format as returned by `current-time'.  This
function assumes that the specified time is during the current day.
S can be of the form XXXX, X:XX, XX:XX, XXXXXX, X:XX:XX, or XX:XX:XX (military
time), or any of the above followed by \"am\", \"AM\", \"pm\", or \"PM\".  If
S does not describe a recognizable time, nil is returned."
  (let ((case-fold-search t)
	(now (decode-time))
	hour minute second)
    (cond ((string-match;; Hour only  XXam or XXpm
	    "\\`[ \t\n\\^M]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>" s)
	   (setq hour (+ (% (string-to-number (match-string 1 s)) 12)
			 (if (char-equal ?a (aref s (match-beginning 2)))
			     0 12))
		 minute 0
		 second 0))
	  ((string-match;; Hour and minute  XX:XXam or XX:XXpm
	    "^[ \t]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>" s)
	   (setq hour (+ (% (string-to-number (match-string 1 s)) 12)
			 (if (char-equal ?a (aref s (match-beginning 3)))
			     0 12))
		 minute (string-to-number (match-string 2 s))
		 second 0))
	  ((string-match;; Hour, minute, and seconds  XX:XX:XXam or XX:XX:XXpm
	    "^[ \t]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>" s)
	   (setq hour (+ (% (string-to-number (match-string 1 s)) 12)
			 (if (char-equal ?a (aref s (match-beginning 4)))
			     0 12))
		 minute (string-to-number (match-string 2 s))
		 second (string-to-number (match-string 3 s))))
	  ((string-match;; Military time  
	    "\\`[ \t\n\\^M]*\\([0-9]?[0-9]\\)[:.]?\\([0-9][0-9]\\)[:.]?\\([0-9][0-9]\\)?\\(\\>\\|[^ap]\\)" s)
	   (setq hour (string-to-number (match-string 1 s))
		 minute (string-to-number (match-string 2 s))
		 second (if (match-beginning 3)
			    (string-to-number (match-string 3 s))
			  0))))
    (if hour
	(nconc
	 (encode-time
	  second minute hour (nth 3 now) (nth 4 now) (nth 5 now) (nth 8 now))
	 '(0)))))

(provide 'timer-funcs)

;;; timer-funcs.el ends here
