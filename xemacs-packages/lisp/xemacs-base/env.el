;;; env.el --- functions to manipulate environment variables.

;; Copyright 1991, 1994, 2001 Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team
;; Keywords: processes, unix

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

;;; Synched up with: FSF 21.1.50

;;; Commentary:

;; UNIX processes inherit a list of name-to-string associations from their
;; parents called their `environment'; these are commonly used to control
;; program options.  This package permits you to set environment variables
;; to be passed to any sub-process run under XEmacs.

;;; Code:

;; History list for environment variable names.
(defvar read-envvar-name-history nil)

(defun read-envvar-name (prompt &optional mustmatch)
  "Read environment variable name, prompting with PROMPT.
Optional second arg MUSTMATCH, if non-nil, means require existing envvar name.
If it is also not t, RET does not exit if it does non-null completion."
  (completing-read prompt
		   (mapcar (function
			    (lambda (enventry)
			      (list (substring enventry 0
					       (string-match "=" enventry)))))
			   process-environment)
		   nil mustmatch nil 'read-envvar-name-history))

;; History list for VALUE argument to setenv.
(defvar setenv-history nil)

;;;###autoload
(defun substitute-env-vars (string)
  "Substitute environment variables referred to in STRING.
`$FOO' where FOO is an environment variable name means to substitute
the value of that variable.  The variable name should be terminated
with a character not a letter, digit or underscore; otherwise, enclose
the entire variable name in braces.  Use `$$' to insert a single
dollar sign."
  (let ((start 0))
    (while (string-match
	    ;; XEmacs change - FSF use their rx macro to generate this regexp
	    "\\(?:\\$\\(\\(?:[a-zA-Z0-9_]\\)+\\)\\)\\|\\(?:\\${\\(\\(?:.\\|\n\\)*?\\)}\\)\\|\\$\\$"
	    string start)
      (cond ((match-beginning 1)
	     (let ((value (getenv (match-string 1 string))))
	       (setq string (replace-match (or value "") t t string)
		     start (+ (match-beginning 0) (length value)))))
	    ((match-beginning 2)
	     (let ((value (getenv (match-string 2 string))))
	       (setq string (replace-match (or value "") t t string)
		     start (+ (match-beginning 0) (length value)))))
	    (t
	     (setq string (replace-match "$" t t string)
		   start (+ (match-beginning 0) 1)))))
    string))

;;;###autoload
(defun setenv (variable &optional value unset substitute-env-vars)
  "Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or is
`nil', the environment variable VARIABLE will be removed.

UNSET, if non-nil, means to remove VARIABLE from the environment.
SUBSTITUTE-ENV-VARS, if non-nil, means to substitute environment
variables in VALUE using `substitute-env-vars'.

Interactively, a prefix argument means to unset the variable.
Interactively, the current value (if any) of the variable
appears at the front of the history list when you type in the new value.

This function works by modifying `process-environment'."
  (interactive
   (if current-prefix-arg
       (list (read-envvar-name "Clear environment variable: " 'exact) nil t)
     (let* ((var (read-envvar-name "Set environment variable: " nil))
	    (oldval (getenv var))
	    newval
	    oldhist)
       ;; Don't put the current value on the history
       ;; if it is already there.
       (if (equal oldval (car setenv-history))
	   (setq oldval nil))
       ;; Now if OLDVAL is non-nil, we should add it to the history.
       (if oldval
	   (setq setenv-history (cons oldval setenv-history)))
       (setq oldhist setenv-history)
       (setq newval (read-from-minibuffer (format "Set %s to value: " var)
					  nil nil nil 'setenv-history))
       ;; If we added the current value to the history, remove it.
       ;; Note that read-from-minibuffer may have added the new value.
       ;; Don't remove that!
       (if oldval
	   (if (eq oldhist setenv-history)
	       (setq setenv-history (cdr setenv-history))
	     (setcdr setenv-history (cdr (cdr setenv-history)))))
       ;; Here finally we specify the args to call setenv with.
       (list var newval))))
  (if unset 
      (setq value nil)
    (if substitute-env-vars
	(setq value (substitute-env-vars value))))
  (if (string-match "=" variable)
      (error "Environment variable name `%s' contains `='" variable)
    (let ((pattern (concat "\\`" (regexp-quote (concat variable "="))))
	  (case-fold-search nil)
	  (scan process-environment)
	  found)
      (if (string-equal "TZ" variable)
	  (set-time-zone-rule value))
      (while scan
	(cond ((string-match pattern (car scan))
	       (setq found t)
	       (if (eq nil value)
		   (setq process-environment (delq (car scan) process-environment))
		 (setcar scan (concat variable "=" value)))
	       (setq scan nil)))
	(setq scan (cdr scan)))
      (or found
	  (if value
	      (setq process-environment
		    (cons (concat variable "=" value)
			  process-environment))))))) ;FSF 21.1.50 returns value

(provide 'env)

;;; env.el ends here
