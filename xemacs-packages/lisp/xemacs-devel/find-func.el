;;; find-func.el --- find the definition of the Emacs Lisp function near point

;; Copyright (C) 1997, 1999, 2001, 2004  Free Software Foundation, Inc.

;; Author: Jens Petersen <petersen@kurims.kyoto-u.ac.jp>
;; Maintainer: petersen@kurims.kyoto-u.ac.jp
;; Keywords: emacs-lisp, functions, variables
;; Created: 97/07/25

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

;;; Synched up with: FSF 21.3.

;;; Commentary:
;;
;; The funniest thing about this is that I can't imagine why a package
;; so obviously useful as this hasn't been written before!!
;; 
;; The default keybindings are the ones protected by autoload cookies at
;; the bottom of this file.  It does pretty much what you would expect,
;; putting the cursor at the definition of the function or variable at
;; point.
;;
;; In XEmacs the source filename of every dumped or loaded Lisp symbol
;; definition is now recorded in `load-history'.  So in XEmacs all
;; non-primitive functions and variables can be found in principle.  (I
;; say "in principle" since the system is not perfect.  For example
;; although it is unusual it can happen that say a variable and function
;; of the same name are defined in different files, which may lead to
;; incorrect results.)  Unfortunately in Emacs, only symbols loaded from
;; startup can be found.  It would be nice if the location of primitive
;; functions in the C code was also recorded!

;; The code started out from `describe-function', `describe-key'
;; ("help.el") and `fff-find-loaded-emacs-lisp-function' (Noah Friedman's
;; "fff.el").

;;; Problems:
;;
;; o `find-function-other-frame' is not quite right when the
;; function is in the current buffer.
;;

;;;; Code:

(require 'loadhist)

;;; User variables:

(defgroup find-function nil
  "Finds the definition of the Emacs Lisp symbol near point."
;;   :prefix "find-function"
  :group 'lisp)

(defconst find-function-space-re "\\(?:\\s-\\|\n\\|;.*\n\\)+")

(defcustom find-function-regexp
  ;; Match things like (defun foo ...), (defmacro foo ...),
  ;; (define-skeleton foo ...), (define-generic-mode 'foo ...),
  ;;  (define-derived-mode foo ...), (define-minor-mode foo)
  (concat
   "^\\s-*(\\(def\\(ine-skeleton\\|ine-generic-mode\\|ine-derived-mode\\|\
ine-function\\|ine-obsolete-function-alias\\|ine-compatible-function-alias\\|\
\[^cgv\W]\\w+\\*?\\)\\|define-minor-mode\
\\|easy-mmode-define-global-mode\\)" find-function-space-re
   "\\('\\|\(quote \\)?%s\\(\\s-\\|$\\|\(\\|\)\\)")
  "The regexp used by `find-function' to search for a function definition.
Note it must contain a `%s' at the place where `format'
should insert the function name.  The default value avoids `defconst',
`defgroup', `defvar'.

Please send improvements and fixes to the maintainer."
  :type 'regexp
  :group 'find-function)

(defcustom find-variable-regexp
  (concat "^\\s-*(\\(def[^umag\W]\\(\\w\\|\\s_\\)+\\*?\\|define-obsolete-variable-alias\\|define-compatible-variable-alias\\)"
	  find-function-space-re "%s\\(\\s-\\|$\\)")
  "The regexp used by `find-variable' to search for a variable definition.
It should match right up to the variable name.  The default value
avoids `defun', `defmacro', `defalias', `defadvice', `defgroup'.

Please send improvements and fixes to the maintainer."
  :type 'regexp
  :group 'find-function)

(defcustom find-function-source-path nil
  "The default list of directories where `find-function' searches.

If this variable is nil then `find-function' searches `load-path' by
default."
  :type '(repeat directory)
  :group 'find-function)

(defcustom find-function-recenter-line 1
  "The window line-number from which to start displaying a symbol definition.
A value of nil implies center the beginning of the definition.
See the function `center-to-window-line' for more information, and
`find-function' and `find-variable'."
  :type '(choice (const :tag "Center" nil)
		 integer)
  :group 'find-function)

(defcustom find-function-after-hook nil
  "Hook run after finding symbol definition.

See the functions `find-function' and `find-variable'."
  :group 'find-function)

;;; Functions:

;; XEmacs omission: Emacs has find-library and supporting friends here.
;; We have the equivalent in lib-complete.el in core.

;;;###autoload
(defun find-function-search-for-symbol (symbol variable-p library)
  "Search for SYMBOL in LIBRARY.
If VARIABLE-P is nil, `find-function-regexp' is used, otherwise
`find-variable-regexp' is used."
  (if (null library)
      (error "Don't know where `%s' is defined" symbol))
  ;; We deviate significantly from Emacs here, due to our distinct
  ;; find-library implementations.
  (if (string-match "\\.el\\(c\\)\\'" library)
      (setq library (substring library 0 (match-beginning 1))))
  (let* ((path find-function-source-path)
	 (filename (if (file-exists-p library)
		       library
		     ;; use `file-name-sans-extension' here? (if it gets fixed)
		     (if (string-match "\\(\\.el\\)\\'" library)
			 (setq library (substring library 0
						  (match-beginning 1))))
		     (or (locate-library (concat library ".el") t path)
			 (locate-library library t path)))))
    (if (not filename)
	(error "The library \"%s\" is not in the path." library))
    (with-current-buffer (find-file-noselect filename)
      (save-match-data
	(let ((regexp (format (if variable-p
				  find-variable-regexp
				find-function-regexp)
			      (regexp-quote (symbol-name symbol))))
	      (case-fold-search))
	  (with-syntax-table emacs-lisp-mode-syntax-table
	    (goto-char (point-min))
	    (if (or (re-search-forward regexp nil t)
		    (re-search-forward
		     (concat "^([^ ]+" find-function-space-re "['(]"
			     (regexp-quote (symbol-name symbol))
			     "\\>")
		     nil t))
		(progn
		  (beginning-of-line)
		  (cons (current-buffer) (point)))
	      (error "Cannot find definition of `%s' in library `%s'"
		     symbol library))))))))

;;;###autoload
(defun find-function-noselect (function)
  "Return a pair (BUFFER . POINT) pointing to the definition of FUNCTION.

Finds the Emacs Lisp library containing the definition of FUNCTION
in a buffer and the point of the definition.  The buffer is
not selected.

If the file where FUNCTION is defined is not known, then it is
searched for in `find-function-source-path' if non nil, otherwise
in `load-path'."
  (if (not function)
      (error "You didn't specify a function"))
  (let ((def (symbol-function function))
	aliases)
    (while (symbolp def)
      (or (eq def function)
	  (if aliases
	      (setq aliases (concat aliases
				    (format ", which is an alias for `%s'"
					    (symbol-name def))))
	    (setq aliases (format "`%s' an alias for `%s'"
				  function (symbol-name def)))))
      (setq function (symbol-function function)
	    def (symbol-function function)))
    ;; XEmacs difference: check for primitive functions a bit differently
    (and (subrp (symbol-function function))
         (if aliases
             (error "%s which is a primitive function" aliases)
       (error "%s is a primitive function" function)))
    (if aliases
	(message aliases))
    (let ((library
	   (cond ((eq (car-safe def) 'autoload)
		  (nth 1 def))
		 ((symbol-file function))
		 ;; XEmacs addition: function annotations
		 ((fboundp 'compiled-function-annotation)
		  (cond ((compiled-function-p def)
			 (file-name-sans-extension
			  (compiled-function-annotation def)))
			((eq 'macro (car-safe def))
			 (and (compiled-function-p (cdr def))
			      (file-name-sans-extension
			       (compiled-function-annotation (cdr def))))))))))
      (find-function-search-for-symbol function nil library))))

;; XEmacs change: these functions are defined in help.el
;(defalias 'function-at-point 'function-called-at-point)

(defun find-function-read (&optional variable-p)
  "Read and return an interned symbol, defaulting to the one near point.

If the optional VARIABLE-P is nil, then a function is gotten
defaulting to the value of the function `function-at-point', otherwise
a variable is asked for, with the default coming from
`variable-at-point'."
  (let ((symb (funcall (if variable-p
			   'variable-at-point
			 'function-at-point)))
	(enable-recursive-minibuffers t)
	val)
    (if (equal symb 0)
	(setq symb nil))
    (setq val (if variable-p
		  (completing-read
		   (concat "Find variable"
			   (if symb
			       (format " (default %s)" symb))
			   ": ")
		   obarray 'boundp t nil 'variable-history)
		(completing-read
		 (concat "Find function"
			 (if symb
			     (format " (default %s)" symb))
			 ": ")
		 obarray 'fboundp t nil 'function-history)))
    (list (if (equal val "")
	      symb
	    (intern val)))))

(defun find-function-do-it (symbol variable-p switch-fn)
  "Find Emacs Lisp SYMBOL in a buffer and display it with SWITCH-FN.
If VARIABLE-P is nil, a function definition is searched for, otherwise 
a variable definition is searched for.  The start of a definition is
centered according to the variable `find-function-recenter-line'.
See also `find-function-after-hook'.

Point is saved in the buffer if it is one of the current buffers."
  (let* ((orig-point (point))
	 (orig-buffers (buffer-list))
	 (buffer-point (save-excursion
			 (funcall (if variable-p
				      'find-variable-noselect
				    'find-function-noselect)
				  symbol)))
	 (new-buf (car buffer-point))
	 (new-point (cdr buffer-point)))
    (when buffer-point
      (when (memq new-buf orig-buffers)
	(push-mark orig-point))
      (funcall switch-fn new-buf)
      (goto-char new-point)
      (recenter find-function-recenter-line)
      (run-hooks 'find-function-after-hook))))

;;;###autoload
(defun find-function (function)
  "Find the definition of the function near point in the current window.

Finds the Emacs Lisp library containing the definition of the function
near point (selected by `function-at-point') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (find-function-read))
  (find-function-do-it function nil 'switch-to-buffer))

;;;###autoload
(defun find-function-other-window (function)
  "Find the definition of the function near point in another window.

See `find-function' for more details."
  (interactive (find-function-read))
  (find-function-do-it function nil 'switch-to-buffer-other-window))

;;;###autoload
(defun find-function-other-frame (function)
  "Find the definition of the function near point in another frame.

See `find-function' for more details."
  (interactive (find-function-read))
  (find-function-do-it function nil 'switch-to-buffer-other-frame))

;;;###autoload
(defun find-variable-noselect (variable &optional file)
  "Return a pair `(BUFFER . POINT)' pointing to the definition of SYMBOL.

Finds the Emacs Lisp library containing the definition of SYMBOL
in a buffer and the point of the definition.  The buffer is
not selected.

The library where VARIABLE is defined is searched for in FILE or
`find-function-source-path', if non nil, otherwise in `load-path'."
  (if (not variable)
      (error "You didn't specify a variable"))
  (and (built-in-variable-type variable)
       (error "%s is a primitive variable" variable))
  (let ((library (or file (symbol-file variable))))
    (find-function-search-for-symbol variable 'variable library)))

;;;###autoload
(defun find-variable (variable)
  "Find the definition of the variable near point in the current window.

Finds the Emacs Lisp library containing the definition of the variable
near point (selected by `variable-at-point') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where VARIABLE is defined is searched for in
`find-function-source-path', if non nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'."
  (interactive (find-function-read 'variable))
  (find-function-do-it variable t 'switch-to-buffer))

;;;###autoload
(defun find-variable-other-window (variable)
  "Find the definition of the variable near point in another window.

See `find-variable' for more details."
  (interactive (find-function-read 'variable))
  (find-function-do-it variable t 'switch-to-buffer-other-window))

;;;###autoload
(defun find-variable-other-frame (variable)
  "Find the definition of the variable near point in another frame.

See `find-variable' for more details."
  (interactive (find-function-read 'variable))
  (find-function-do-it variable t 'switch-to-buffer-other-frame))

;;;###autoload
(defun find-function-on-key (key)
  "Find the function that KEY invokes.  KEY is a string.
Point is saved if FUNCTION is in the current buffer."
  (interactive "kFind function on key: ")
  ;; XEmacs change: Avoid the complex menu code with key-or-menu-binding
  (let ((defn (key-or-menu-binding key))
	(key-desc (key-description key)))
    (if (or (null defn) (integerp defn))
        (message "%s is unbound" key-desc)
      (if (consp defn)
	  (message "%s runs %s" key-desc (prin1-to-string defn))
	(find-function-other-window defn)))))

;;;###autoload
(defun find-function-at-point ()
  "Find directly the function at point in the other window."
  (interactive)
  (let ((symb (function-at-point)))
    (when symb
      (find-function-other-window symb))))

;;;###autoload
(defun find-variable-at-point ()
  "Find directly the function at point in the other window."
  (interactive)
  (let ((symb (variable-at-point)))
    (when (and symb (not (equal symb 0)))
      (find-variable-other-window symb))))

;; XEmacs change: autoload instead of defining find-function-setup-keys
;; FIXME: We do not have a default keybinding for find-function
;; (define-key ctl-x-map "F" 'find-function) ; conflicts with `facemenu-keymap'
;;;###autoload(define-key ctl-x-4-map "F" 'find-function-other-window)
;;;###autoload(define-key ctl-x-5-map "F" 'find-function-other-frame)
;;;###autoload(define-key ctl-x-map "K" 'find-function-on-key)
;;;###autoload(define-key ctl-x-map "V" 'find-variable)
;;;###autoload(define-key ctl-x-4-map "V" 'find-variable-other-window)
;;;###autoload(define-key ctl-x-5-map "V" 'find-variable-other-frame)
;;;###autoload(define-key help-mode-map "F" 'find-function-at-point)
;;;###autoload(define-key help-mode-map "V" 'find-variable-at-point)

(provide 'find-func)

;;; arch-tag: 43ecd81c-74dc-4d9a-8f63-a61e55670d64
;;; find-func.el ends here
