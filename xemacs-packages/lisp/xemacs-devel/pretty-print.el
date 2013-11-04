;;; pretty-print.el --- Emacs Lisp pretty printer and macro expander

;; Copyright (C) 1992,1993 Guido Bosch <Guido.Bosch@loria.fr>

;; Author: Guido Bosch
;; Maintainer: None
;; Keywords: lisp, internal

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

;;; Synched up with:  Not in FSF.

;;; Commentary:

;; Please send bugs and comments to the author.
;;
;; <DISCLAIMER>
;; This program is still under development.  Neither the author nor
;; CRIN-INRIA accepts responsibility to anyone for the consequences of
;; using it or for whether it serves any particular purpose or works
;; at all.
;; 
;; The package has been developed under Lucid Emacs 19, but also runs
;; on Emacs 18, if it is compiled with the version 19 byte compiler
;; (function `compiled-function-p' lacking).
;;

;; Installation and Usage
;; ----------------------
;;
;; This package provides an Emacs Lisp sexpression pretty printer and
;; macroexpander.  To install it, put the following line in your .emacs,
;; default.el or site-init.el/site-run.el (for Lucid Emacs): 
;; (require 'pp)
;; 
;; The package can also be made autoloadable, with the following entry 
;; points: 
;; (autoload 'pp-function "pp" nil t)
;; (autoload 'pp-variable "pp" nil t)
;; (autoload 'pp-plist     "pp" nil t)
;; (autoload 'macroexpand-sexp "pp" nil t)
;; (autoload 'macroexpand-all-sexp "pp" nil t)
;; (autoload 'prettyexpand-sexp "pp" nil t)
;; (autoload 'prettyexpand-all-sexp "pp" nil t)
;;
;;(define-key emacs-lisp-mode-map '(control meta m) 'macroexpand-sexp)
;;(define-key emacs-lisp-mode-map '(control meta M) 'macroexpand-all-sexp)
;;(define-key emacs-lisp-mode-map '(control symbol m) 'prettyexpand-sexp)
;;(define-key emacs-lisp-mode-map '(control symbol M) 'prettyexpand-all-sexp)
;;
;;(define-key lisp-interaction-mode-map '(control meta m) 'macroexpand-sexp)
;;(define-key lisp-interaction-mode-map '(control meta M) 'macroexpand-all-sexp)
;;(define-key lisp-interaction-mode-map '(control symbol m) 'prettyexpand-sexp)
;;(define-key lisp-interaction-mode-map '(control symbol M) 'prettyexpand-all-sexp)
;;

;; Pretty printing of the different cells of a symbol is done with the
;; commands:
;;
;; 		M-x pp-function
;; 		M-x pp-variable
;;		M-x pp-plist
;;
;; They print a symbol's function definition, variable value and
;; property list, respectively.  These commands pop up a separate
;; window in which the pretty printed lisp object is displayed.
;; Completion for function and variable symbols is provided. If a
;; function is byte compiled, `pp-function' proposes to call the Emacs
;; Lisp disassembler (this feature only works for Emacs 19, as it
;; needs the `compiled-function-p' predicate).
;;
;; To use the macro expander, put the cursor at the beginning of the
;; form to be expanded, then type
;;
;; 	        C-M-m 		(macroexpand-sexp)
;; or		C-M-Sh-M  	(macroexpand-all-sexp)
;; 
;; Both commands will pop up a temporary window containing the
;; macroexpanded code. The only difference is that the second command
;; expands recursively all containing macro calls, while the first one
;; does it only for the uppermost sexpression.  
;; 	With a prefix argument, the macro expansion isn't displayed in a
;; separate buffer but replaces the original code in the current
;; buffer. Be aware: Comments will be lost.
;; 	You can get back the original sexpression using the `undo'
;; 	command on `C-x u'.
;;
;; There is also a prettyfied version of the macroexpander:
;;
;;		C-Sym-m		(prettyexpand-sexp)
;; or		C-Sym-M		(prettyexpand-all-sexp)
;; 
;; The only difference with the corresponding macroexpand commands is 
;; that calls to macros specified in the variable
;; `pp-shadow-expansion-list' are not expanded, in order to make the
;; code look nicer. This is only useful for Lucid Emacs or code that
;; uses Dave Gillespies cl package, as it inhibits expansion of the
;; following macros: block, eval-when, defun*, defmacro*, function*,
;; setf.

; Change History
; 
; $Log: pretty-print.el,v $
; Revision 1.3  2000/10/06 09:35:05  youngs
; Martin's Monster Mega typo patch
;
; Revision 1.2  1998/02/10 16:23:33  steveb
; pretty-print fixes
;
; Revision 1.4  1993/03/25  14:09:52  bosch
; Commands `prettyexpand-sexp' and `prettyexpand-all-sexp' and
; corresponding key bindings added.  Commands pp-{function, variable}
; rewritten. `pp-plist' added. Function `pp-internal-loop' (for Dave
; Gillespies CL loop macro) added.
;
; Revision 1.3  1993/03/03  12:24:13  bosch
; Macroexpander rewritten. Function `pp-macroexpand-all' added (snarfed
; from Dave Gillespies cl-extra.el). Pretty printing for top level
; defining forms added (`pp-internal-def'). Key bindings for
; `emacs-lisp-mode-map' and `lisp-interaction-mode-map' added.  Built-in
; variable `print-gensym' set for printinng uninterned symbols. Started
; adding support for cl-dg (defun*, defmacro*, ...).  Minor bug fixes.
;
; Revision 1.2  1993/02/25  17:35:02  bosch
; Comments about Emacs 18 compatibility added.
;
; Revision 1.1  1993/02/25  16:55:01  bosch
; Initial revision
;
;
;;; Code:

;; TO DO LIST
;; ----------
;; Provide full Emacs 18 compatibility.

;; Popper support
(defvar pp-buffer-name "*Pretty Print*")
(defvar pp-macroexpand-buffer-name "*Macro Expansion*")
(if (featurep 'popper)
    (or (eq popper-pop-buffers 't)
	(setq popper-pop-buffers 
	      (cons pp-buffer-name 
		    (cons pp-macroexpand-buffer-name 
			  popper-pop-buffers)))))

;; User level functions
;;;###autoload
(defun pp-function (symbol)
  "Pretty print the function definition of SYMBOL in a separate buffer"
  (interactive 
   (list (pp-read-symbol 'fboundp "Pretty print function definition of: ")))
  (if (compiled-function-p (symbol-function symbol))
      (if (y-or-n-p 
	   (format "Function %s is byte compiled. Disassemble? " symbol))
	  (disassemble (symbol-function symbol))
	(pp-symbol-cell symbol 'symbol-function))
    (pp-symbol-cell symbol 'symbol-function)))

;;;###autoload
(defun pp-variable (symbol)
  "Pretty print the variable value of SYMBOL in a separate buffer"
  (interactive
   (list (pp-read-symbol 'boundp "Pretty print variable value of: ")))
  (pp-symbol-cell symbol 'symbol-value))

;;;###autoload
(defun pp-plist (symbol)
  "Pretty print the property list of SYMBOL in a separate buffer"
  (interactive
   (list (pp-read-symbol 'symbol-plist "Pretty print property list of: ")))
  (pp-symbol-cell symbol 'symbol-plist))

(defun pp-read-symbol (predicate prompt)
  "Read a symbol for which  PREDICATE is true, promptiong with PROMPT."
  (let (symbol)
    (while (or (not symbol) (not (funcall predicate symbol)))
      (setq symbol 
	    (intern-soft 
	     (completing-read
	      prompt
	      obarray
	      predicate
	      t
	      (and symbol (symbol-name symbol))))))
    symbol))

(defun pp-symbol-cell (symbol accessor)  
  "Pretty print the contents of the cell of SYMBOL that can be reached
with the function ACCESSOR."
  (with-output-to-temp-buffer pp-buffer-name
    (set-buffer pp-buffer-name)
    (emacs-lisp-mode)
    (erase-buffer)
    (pp-internal 
     (funcall accessor symbol) 
     (format "%s's %s is:\n" symbol accessor))
    (terpri)))


  
;; Macro expansion (user level)

;;;###autoload
(defun macroexpand-sexp (&optional replace)
  "Macro expand the sexpression following point. Pretty print expansion in a
temporary buffer. With prefix argument, replace the original
sexpression by its expansion in the current buffer."
  (interactive "P")
  (pp-macroexpand-internal 'macroexpand replace t))

;;;###autoload
(defun macroexpand-all-sexp (&optional replace)
  "Macro expand recursively the sexpression following point. Pretty print
expansion in a temporary buffer. With prefix argument, replace the
original sexpression by its expansion in the current buffer."
  (interactive "P")
  (pp-macroexpand-internal 'pp-macroexpand-all replace t))

;;;###autoload
(defun prettyexpand-sexp (&optional replace)
  "Macro expand the sexpression following point. Pretty print expansion
in a temporary buffer. With prefix argument, replace the original
sexpression by its expansion in the current buffer.  
	However, calls to macros specified in the variable
`pp-shadow-expansion-list' are not expanded, in order to make the code
look nicer."

  (interactive "P")
  (pp-macroexpand-internal 'macroexpand replace))

;;;###autoload
(defun prettyexpand-all-sexp (&optional replace)
  "Macro expand recursively the sexpression following point. Pretty print
expansion in a temporary buffer. With prefix argument, replace the
original sexpression by its expansion in the current buffer.
	However, calls to macros specified in the variable
`pp-shadow-expansion-list' are not expanded, in order to make the code
look nicer."
  (interactive "P")
  (pp-macroexpand-internal 'pp-macroexpand-all replace))

;; XEmacs: don't do this at load time.
;;(define-key emacs-lisp-mode-map '(control meta m) 'macroexpand-sexp)
;;(define-key emacs-lisp-mode-map '(control meta M) 'macroexpand-all-sexp)
;;(define-key emacs-lisp-mode-map '(control symbol m) 'prettyexpand-sexp)
;;(define-key emacs-lisp-mode-map '(control symbol M) 'prettyexpand-all-sexp)

;;(define-key lisp-interaction-mode-map '(control meta m) 'macroexpand-sexp)
;;(define-key lisp-interaction-mode-map '(control meta M) 'macroexpand-all-sexp)
;;(define-key lisp-interaction-mode-map '(control symbol m) 'prettyexpand-sexp)
;;(define-key lisp-interaction-mode-map '(control symbol M) 'prettyexpand-all-sexp)


;; Macro expansion (internals)

(defvar pp-shadow-expansion-list
  (mapcar 'list '(block eval-when defun* defmacro* function* setf))
  "The value of this variable is given as the optional environment
argument of the macroexpand functions. Forms specified in this list are
not expanded.")

(defun pp-macroexpand-internal 
  (macroexpand-function replace &optional dont-shadow)
  "Macro expand the sexp that starts at point, using
MACROEXPAND-FUNCTION.  If REPLACE is non-nil, replace the original
text by its expansion, otherwise pretty print the expansion in a
temporary buffer. With optional argument DONT-SHADOW non-nil, do not
use the `pp-shadow-expansion-list' to inhibit expansion of some
forms."

  (interactive)
  (let ((expansion
	 (funcall 
	  macroexpand-function
	  (let ((stab (syntax-table)))
	    (unwind-protect
		(save-excursion
		  (set-syntax-table emacs-lisp-mode-syntax-table)
		  ;; (forward-sexp 1)
		  (read (current-buffer)))
	      (set-syntax-table stab)))
	  (if dont-shadow 
	      nil
	    pp-shadow-expansion-list))))
    (save-excursion
      (if replace 
	  (let ((start (point))
		(end (progn (forward-sexp 1) (point))))
	    (delete-region start end)
	    (pp-internal expansion))
	(with-output-to-temp-buffer pp-macroexpand-buffer-name
	  (set-buffer pp-macroexpand-buffer-name)
	  (erase-buffer)
	  (emacs-lisp-mode)
	  (pp-internal expansion))))))

;; Internal pretty print functions

;;;###autoload
(defun pp-internal (form &optional title)
  "Pretty print FORM in in the current buffer.
Optional string TITLE is inserted before the pretty print."
  (let (start)
    (if title (princ title))
    (setq start (point))
    ;; print-escape-newlines must be t, otherwise we cannot use
    ;; (current-column) to detect good line breaks
    (let ((print-escape-newlines t)
	  (print-gensym t)
	  )
      (prin1 form (current-buffer))
      (goto-char start)
      (pp-internal-sexp))))

(defun pp-internal-sexp ()
  "Pretty print the following sexp. 
Point must be on or before the first character."

  (skip-chars-forward " \n\t")
  (let* ((char (following-char))
	 (ch-class (char-syntax char))
	 (start (point)))

    (cond
     ;; open paren
     ((eq char ?\()
      (down-list 1)
      (if (memq  (char-syntax (following-char)) '(?_ ?w))
	  (let ((symbol (read (current-buffer))))
	    (cond ((and (symbolp symbol)
			(fboundp symbol))
		   (goto-char start)
		   (pp-internal-function symbol))
		  ((memq symbol '(lambda macro))
		   (pp-internal-lambda))
		  (t
		   (goto-char start)
		   (pp-internal-list))))
	(up-list -1)
	(pp-internal-list)))
     
     ;;symbols & strings
     ((memq  ch-class '(?_		; symbol
			?w		; word
			?\"		; string
			?\\		; escape
			?\'		; quote (for uninterned symbols)
			)) (forward-sexp 1))
	
     ;; vector
     ((eq char ?\[) (pp-internal-list))
     
     ;; error otherwise
     (t (error "pp-internal-sexp: character class not treated yet: `%c'" 
	       ch-class)))))

(defun pp-internal-function (func)
  "Pretty print a functuion call.
Point must be on the open paren. the function symbol may be passed as an 
optional argument."
  (let ((start (point))
	(too-large (>= (save-excursion
			 (forward-sexp 1)
			 (current-column))
		       fill-column))
	(indent-info (get func lisp-indent-function)))
    (down-list 1)
    ;; skip over function name
    (forward-sexp 1)
    (cond
     ((memq func '(let let*)) (pp-internal-let))

     ((eq func 'cond) (pp-internal-cond))

     ((memq func '(if while with-output-to-temp-buffer catch block))
      (pp-internal-sexp)
      (pp-internal-body 't))

     ((eq func 'quote) (pp-internal-quote))

     ((memq func '(progn 
		    prog1 prog2
		    save-window-excursion 
		    save-excursion 
		    save-restriction))
      (pp-internal-body 't))

     ((memq func '(defun defmacro defsubst defun* defmacro*))
      (pp-internal-def))
     
     ((eq func 'loop) (pp-internal-loop))

     ('t (pp-internal-body too-large)))))

(defun pp-internal-def ()
  (forward-sexp 1)			; skip name
  (if (looking-at " nil")		; replace nil by () 
      (replace-match " ()")
    (forward-sexp 1))
  (if (looking-at " \"")
      ;; comment string. Replace all escaped linefeeds by real ones
      (let ((limit (save-excursion (forward-sexp 1) (point-marker))))
	(newline-and-indent)
	(while (re-search-forward "\\\\n" limit t)
	  (replace-match "\n" nil nil))
	(goto-char limit)))
  (pp-internal-body 't))

(defun pp-internal-list ()
  "Pretty print a list  or a vector.
Point must be on the open paren."
  (let ((too-large (>= (save-excursion
			(forward-sexp 1)
			(current-column))
		      fill-column)))
    (down-list 1)
    (pp-internal-sexp)
    (pp-internal-body too-large)))

(defun pp-internal-body (&optional force-indent)
  "Prety print a body of sexp. Stop after reaching a `)'.  If argument
FORCE-INDENT is non-nil, break line after each sexpression of the
body."
  (skip-chars-forward " \n\t")
  (let (ch-class)
    ;; while not closing paren
    (while (/= (setq ch-class (char-syntax (following-char))) ?\)) 
      (if  force-indent (newline-and-indent))
      (pp-internal-sexp))
    (up-list 1)))

(defun pp-internal-loop ()
  "Prety print a loop body. Stop after reaching a `)'. 
Line breaks are done before the following keywords: "
  (forward-sexp 1)
  (skip-chars-forward " \n\t")
  (let (ch-class)
    ;; while not closing paren
    (while (/= (setq ch-class (char-syntax (following-char))) ?\))
      (if (not (looking-at "for\\|repeat\\|with\\|while\\|until\\|always\\|never\\|thereis\\|collect\\|append\\|nconc\\|sum\\|count\\|maximize\\|minimize\\|if\\|when\\|else\\|unless\\|do\\W\\|initially\\|finally\\|return\\|named"))
	  (pp-internal-sexp)
	(newline-and-indent)
	(forward-sexp 1))
      (skip-chars-forward " \n\t"))
    (up-list 1)))

(defun pp-internal-body-list ()
  (let ((too-large (>= (save-excursion
			(forward-sexp 1)
			(current-column))
		      fill-column))
	ch-class)
    (down-list 1)
    (pp-internal-sexp)
    (while (/= (setq ch-class (char-syntax (following-char))) ?\)) 
      (if  too-large (newline-and-indent))
      (pp-internal-sexp))
    (up-list 1)))
    
(defun pp-internal-lambda ()
  (forward-sexp 1) ; arguments
  (pp-internal-body 't))

(defun pp-internal-let ()
  "Pretty print a let-like  form.
Cursor is behind function symbol."
  (down-list 1)
  (while (not (= (following-char) ?\)))
    (if (= (following-char) ?\()
	(pp-internal-body-list)
      (forward-sexp 1))
    (if (not (= (following-char) ?\)))
        (newline-and-indent)))
  (up-list 1)
  (pp-internal-body 't))

(defun pp-internal-cond ()
  "Pretty print a cond-like  form.
Cursor is behind function symbol."
  (skip-chars-forward " \n\t")
  (while (not (= (following-char) ?\)))
    (pp-internal-body-list)
    (if (not (= (following-char) ?\)))
	(newline-and-indent)))
  (up-list 1))

      
(defun pp-internal-quote ()
  "Pretty print a quoted list.
Cursor is behind the symbol quote."
  (skip-chars-forward " \n\t")
  (let ((end (point)))
    (backward-sexp 1)
    (delete-region (point) end)
    (up-list -1)
    (setq end (point))
    (forward-sexp 1)
    (delete-char -1)
    (goto-char end)
    (delete-char 1)
    (insert "'")
    (if (= (char-syntax (following-char)) ?\()
	;; don't print it as sexp, because it could be (let ... ) or
	;; (cond ... ) or whatever. 
	(pp-internal-list)
      (pp-internal-sexp))))


;; Stolen form Dave Gillespies cl-extra.el
(defun pp-macroexpand-all (form &optional env)
  "Expand all macro calls through a Lisp FORM.
This also does some trivial optimizations to make the form prettier."
  (setq form (macroexpand form env))
  (cond 
   ((not (consp form)) form)
   ((memq (car form) '(let let*))
    (if (null (nth 1 form))
	(pp-macroexpand-all (cons 'progn (cdr (cdr form))) env)
      (cons (car form) 
	    (cons (pp-macroexpand-lets (nth 1 form) env)
		  (pp-macroexpand-body (cdr (cdr form)) env)))))
   ((eq (car form) 'cond)
    (cons (car form)
	  (mapcar (function (lambda (x) (pp-macroexpand-body x env)))
		  (cdr form))))
   ((eq (car form) 'condition-case)
    (cons (car form)
	  (cons (nth 1 form)
		(cons (pp-macroexpand-all (nth 2 form) env)
		      (pp-macroexpand-lets 
		       (cdr (cdr (cdr form))) env)))))
   ((memq (car form) '(quote function))
    (if (eq (car-safe (nth 1 form)) 'lambda)
	(list (car form) 
	      (cons 'lambda
		    (cons (car (cdr (car (cdr form))))
			  (pp-macroexpand-body 
			   (cdr (cdr (car (cdr form)))) env))))
      form))
   ((memq (car form) '(defun defmacro))
    (cons (car form)
	  (cons (nth 1 form)
		(pp-macroexpand-body (cdr (cdr form)) env))))
   ((and (eq (car form) 'progn) (not (cdr (cdr form))))
    (pp-macroexpand-all (nth 1 form) env))
   (t 
    (cons (car form) (pp-macroexpand-body (cdr form) env)))))

(defun pp-macroexpand-body (body &optional env)
  (mapcar (function (lambda (x) (pp-macroexpand-all x env))) body))

(defun pp-macroexpand-lets (list &optional env)
  (mapcar (function
	   (lambda (x)
	     (if (consp x) (cons (car x) (pp-macroexpand-body (cdr x) env))
	       x))) list))

(run-hooks 'pp-load-hook)
(provide 'pp)

;;; pretty-print.el ends here
