;;; hippie-exp.el --- expand text trying various ways to find its expansion.

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Anders Holst <aho@sans.kth.se>
;; Last change: 6 August 1995
;; Version: 1.4
;; Keywords: abbrev

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Emacs/Mule zeta

;;; Commentary:

;;  `hippie-expand' is a single function for a lot of different kinds
;;  of completions and expansions.  Called repeatedly it tries all
;;  possible completions in succession. 
;;  Which kinds of completions to try, and in which order, is
;;  determined by the contents of `hippie-expand-try-functions-list'.
;;  Much customization of `hippie-expand' can be made by changing the
;;  order of, removing, or inserting new functions in this list.
;;  Given a positive numeric argument, `hippie-expand' jumps directly
;;  ARG functions forward in this list.  Given some other argument
;;  (a negative argument or just Ctrl-U) it undoes the tried
;;  completion.
;;
;;  If the variable `hippie-expand-verbose' is non-nil, `hippie-expand'
;;  outputs in a message which try-function in the list that is used
;;  currently (ie. was used currently and will be tried first the next
;;  time).
;;  The variable `hippie-expand-max-buffers' determines in how many
;;  buffers, apart from the current, to search for expansions in.  It
;;  is used by the try-functions named "-all-buffers".
;;  The variable `hippie-expand-ignore-buffers' is a list of regexps
;;  matching buffer names (as strings) or major modes (as atoms) of
;;  buffers that should not be searched by the try-functions named
;;  "-all-buffers".
;;  See also the macro `make-hippie-expand-function' below.
;;  
;;  A short description of the current try-functions in this file:
;;    `try-complete-file-name' : very convenient to have in any buffer,
;;      and not just in the minibuffer or (some) shell-mode.  It goes
;;      through all possible completions instead of just completing as
;;      much as is unique.
;;    `try-complete-file-name-partially' : To insert in the list just
;;      before `try-complete-file-name' for those who want first to get
;;      a file name completed only as many characters as is unique.
;;    `try-expand-all-abbrevs' : can be removed if you don't use abbrevs.
;;      Otherwise it looks through all abbrev-tables, starting with
;;      the local followed by the global. 
;;    `try-expand-line' : Searches the buffer for an entire line that 
;;      begins exactly as the current line.  Convenient sometimes, for 
;;      example as a substitute for (or complement to) the history
;;      list in shell-like buffers.  At other times, only confusing.
;;    `try-expand-line-all-buffers' : Like `try-expand-line' but searches
;;      in all buffers (except the current).  (This may be a little
;;      slow, don't use it unless you are really fond of `hippie-expand'.)
;;    `try-expand-list' : Tries to expand the text back to the nearest
;;      open delimiter, to a whole list from the buffer. Convenient for
;;      example when writing lisp or TeX.
;;    `try-expand-list-all-buffers' : Like `try-expand-list' but searches 
;;      in all buffers (except the current).  
;;    `try-expand-dabbrev' : works exactly as dabbrev-expand (but of
;;      course in a way compatible with the other try-functions).
;;    `try-expand-dabbrev-all-buffers' : perhaps the most useful of them,
;;      like `dabbrev-expand' but searches all Emacs buffers (except the
;;      current) for matching words.  (No, I don't find this one
;;      particularly slow.) 
;;    `try-expand-dabbrev-visible': Searches the currently visible parts of
;;      all windows.  Can be put before `try-expand-dabbrev-all-buffers' to
;;      first try the expansions you can see.
;;    `try-expand-dabbrev-from-kill': Searches the kill ring for a suitable
;;      completion of the word.  Good to have, just in case the word was not
;;      found elsewhere.
;;    `try-expand-whole-kill' : Tries to complete text with a whole entry
;;      from the kill ring.  May be good if you don't know how far up in
;;      the kill-ring the required entry is, and don't want to mess with
;;      "Choose Next Paste".
;;    `try-complete-lisp-symbol' : like `lisp-complete-symbol', but goes
;;      through all possibilities instead of completing what is unique.
;;      Might be tedious (usually a lot of possible completions) and
;;      since its function is much like `lisp-complete-symbol', which
;;      already has a key of its own, you might want to remove this.
;;    `try-complete-lisp-symbol-partially' : To insert in the list just
;;      before `try-complete-lisp-symbol' for those who first want to get
;;      completion of what is unique in the name.  
;;
;;  Not all of the above functions are by default in
;;  `hippie-expand-try-functions-list'.  This variable is better set
;;  in ".emacs" to make `hippie-expand' behave maximally convenient
;;  according to personal taste.  Also, instead of loading the
;;  variable with all kinds of try-functions above, it might be an
;;  idea to use `make-hippie-expand-function' to construct different
;;  `hippie-expand'-like functions, with different try-lists and bound
;;  to different keys. It is also possible to make
;;  `hippie-expand-try-functions-list' a buffer local variable, and
;;  let it depend on the mode (by setting it in the mode-hooks).
;;
;;  To write new try-functions, consider the following:
;;  Each try-function takes one argument OLD which is nil the first
;;  time the function is called and true in succeeding calls for the
;;  same string to complete.  The first time the function has to
;;  extract the string before point to complete, and substitute the
;;  first completion alternative for it.  On following calls it has to
;;  substitute the next possible completion for the last tried string.
;;  The try-function is to return t as long as it finds new
;;  possible completions.  When there are no more alternatives it has
;;  to restore the text before point to its original contents, and
;;  return nil (don't beep or message or anything).
;;  The try-function can (should) use the following functions:
;;    `he-init-string' : Initializes the text to substitute to the
;;      contents of the region BEGIN to END.  Also sets the variable
;;      `he-search-string' to the text to expand.
;;    `he-substitute-string' : substitutes STR into the region
;;      initialized with `he-init-string'.  (An optional second argument
;;      TRANS-CASE non-nil, means transfer of case from the abbreviation
;;      to the expansion is ok if that is enabled in the buffer.)
;;    `he-reset-string' : Resets the initialized region to its original
;;      contents.
;;  There is also a variable: `he-tried-table' which is meant to contain
;;  all tried expansions so far.  The try-function can check this 
;;  variable to see whether an expansion has already been tried
;;  (hint: `he-string-member').
;;
;;  Known bugs
;;
;;  It may happen that some completion suggestion occurs twice, in
;;  spite of the use of `he-tried-table' to prevent that.  This is 
;;  because different try-functions may try to complete different
;;  lengths of text, and thus put different amounts of the
;;  text in `he-tried-table'.  Anyway this seems to occur seldom enough
;;  not to be too disturbing.  Also it should NOT be possible for the
;;  opposite situation to occur, that `hippie-expand' misses some
;;  suggestion because it thinks it has already tried it.
;;
;;  Acknowledgement
;;
;;  I want to thank Mikael Djurfeldt in discussions with whom the idea
;;  of this function took form.
;;  I am also grateful to all those who have given me suggestions on
;;  how to improve it, and all those who helped to find and remove bugs.
;;

;;; Code:

(defvar he-num -1)

(defvar he-string-beg (make-marker))

(defvar he-string-end (make-marker))

(defvar he-search-string ())

(defvar he-expand-list ())

(defvar he-tried-table ())

(defvar he-search-loc (make-marker))

(defvar he-search-loc2 ())

(defvar he-search-bw ())

(defvar he-search-bufs ())

(defvar he-searched-n-bufs ())

(defvar he-search-window ())

;;;###autoload
(defvar hippie-expand-try-functions-list '(try-complete-file-name-partially
					   try-complete-file-name
					   try-expand-all-abbrevs
					   try-expand-list
					   try-expand-line
					   try-expand-dabbrev
					   try-expand-dabbrev-all-buffers
					   try-expand-dabbrev-from-kill
					   try-complete-lisp-symbol-partially
					   try-complete-lisp-symbol)
  "The list of expansion functions tried in order by `hippie-expand'.
To change the behavior of `hippie-expand', remove, change the order of,
or insert functions in this list.")

;;;###autoload
(defvar hippie-expand-verbose t
  "*Non-nil makes `hippie-expand' output which function it is trying.")

;;;###autoload
(defvar hippie-expand-max-buffers ()
  "*The maximum number of buffers (apart from the current) searched.
If nil, all buffers are searched.")

;;;###autoload
(defvar hippie-expand-ignore-buffers '("^ \\*.*\\*$" dired-mode)
  "*A list specifying which buffers not to search (if not current).
Can contain both regexps matching buffer names (as strings) and major modes
\(as atoms)")

;;;###autoload
(defun hippie-expand (arg)
  "Try to expand text before point, using multiple methods.
The expansion functions in `hippie-expand-try-functions-list' are
tried in order, until a possible expansion is found.  Repeated
application of `hippie-expand' inserts successively possible
expansions.  
With a positive numeric argument, jumps directly to the ARG next
function in this list.  With a negative argument or just \\[universal-argument], 
undoes the expansion." 
  (interactive "P")
  (if (or (not arg) 
	  (and (integerp arg) (> arg 0)))
      (let ((first (or (= he-num -1)
		       (not (equal this-command last-command)))))
	(if first
	    (progn
	      (setq he-num -1)
	      (setq he-tried-table nil)))
	(if arg
	    (if (not first) (he-reset-string))
	    (setq arg 0))
	(let ((i (max (+ he-num arg) 0)))
	  (while (not (or (>= i (length hippie-expand-try-functions-list))
			  (apply (nth i hippie-expand-try-functions-list) 
				 (list (= he-num i)))))
	    (setq i (1+ i)))
	  (setq he-num i))
	(if (>= he-num (length hippie-expand-try-functions-list))
	    (progn
	      (setq he-num -1)
	      (if first
		  (message "No expansion found")
		  (message "No further expansions found"))
	      (ding))
	    (if (and hippie-expand-verbose
		     (not (window-minibuffer-p (selected-window))))
		(message "Using %s"
			 (prin1-to-string (nth he-num 
				   hippie-expand-try-functions-list))))))
      (if (and (>= he-num 0)
	       (eq (marker-buffer he-string-beg) (current-buffer)))
	  (progn
	    (setq he-num -1)
	    (he-reset-string)
	    (if (and hippie-expand-verbose
		     (not (window-minibuffer-p (selected-window))))
		(message "Undoing expansions"))))))

;; Initializes the region to expand (to between BEG and END).
(defun he-init-string (beg end)
  (set-marker he-string-beg beg)
  (set-marker he-string-end end)
  (setq he-search-string (buffer-substring beg end)))

;; Resets the expanded region to its original contents.
(defun he-reset-string ()
  (let ((newpos (point-marker)))
    (goto-char he-string-beg)
    (insert he-search-string)
    (delete-region (point) he-string-end)
    (goto-char newpos)))

;; Substitutes an expansion STR into the correct region (the region
;; initialized with `he-init-string'). 
;; An optional argument TRANS-CASE means that it is ok to transfer case
;; from the abbreviation to the expansion if that is possible, and is
;; enabled in the buffer.
(defun he-substitute-string (str &optional trans-case)
  (let ((trans-case (and trans-case
			 case-replace
			 case-fold-search))
	(newpos (point-marker))
	(subst ()))
    (goto-char he-string-beg)
    (setq subst (if trans-case (he-transfer-case he-search-string str) str))
    (setq he-tried-table (cons subst he-tried-table))
    (insert subst)
    (delete-region (point) he-string-end)
    (goto-char newpos)))

(defun he-capitalize-first (str)
  (save-match-data
    (if (string-match "\\Sw*\\(\\sw\\).*" str)
	(let ((res (downcase str))
	      (no (match-beginning 1)))
	  (aset res no (upcase (aref str no)))
	  res)
      str)))

(defun he-ordinary-case-p (str)
  (or (string= str (downcase str))
      (string= str (upcase str))
      (string= str (capitalize str))
      (string= str (he-capitalize-first str))))

(defun he-transfer-case (from-str to-str)
  (cond ((string= from-str (substring to-str 0 (min (length from-str)
						    (length to-str))))
	 to-str)
	((not (he-ordinary-case-p to-str))
	 to-str)
	((string= from-str (downcase from-str))
	 (downcase to-str))
	((string= from-str (upcase from-str))
	 (upcase to-str))
	((string= from-str (he-capitalize-first from-str))
	 (he-capitalize-first to-str))
	((string= from-str (capitalize from-str))
	 (capitalize to-str))
	(t
	 to-str)))


;; Check if STR is a member of LST.
;; Transform to the final case if optional TRANS-CASE is non-NIL.
(defun he-string-member (str lst &optional trans-case)
  (if str
      (member (if (and trans-case
		       case-replace
		       case-fold-search)
		  (he-transfer-case he-search-string str)
		str)
	      lst)))

;; Check if STR matches any regexp in LST.
;; Ignore possible non-strings in LST.
(defun he-regexp-member (str lst)
  (while (and lst
	      (or (not (stringp (car lst)))
		  (not (string-match (car lst) str))))
    (setq lst (cdr lst)))
  lst)

;;  For the real hippie-expand enthusiast: A macro that makes it
;;  possible to use many functions like hippie-expand, but with
;;  different try-functions-lists.
;;  Usage is for example:
;;    (fset 'my-complete-file (make-hippie-expand-function
;;                             '(try-complete-file-name-partially
;;                               try-complete-file-name)))
;;    (fset 'my-complete-line (make-hippie-expand-function
;;                             '(try-expand-line
;;                               try-expand-line-all-buffers)))
;;  
;;;###autoload
(defmacro make-hippie-expand-function (try-list &optional verbose)
  "Construct a function similar to `hippie-expand'.
Make it use the expansion functions in TRY-LIST.  An optional second
argument VERBOSE non-nil makes the function verbose."
  (` (function (lambda (arg)
       (, (concat 
	   "Try to expand text before point, using the following functions: \n"
	   (mapconcat 'prin1-to-string (eval try-list) ", ")))
       (interactive "P")
       (let ((hippie-expand-try-functions-list (, try-list))
	     (hippie-expand-verbose (, verbose)))
	 (hippie-expand arg))))))


;;;  Here follows the try-functions and their requisites:


(defun try-complete-file-name (old)
  "Try to complete text as a file name.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (if (not old)
      (progn 
	(he-init-string (he-file-name-beg) (point))
	(let ((name-part (he-file-name-nondirectory he-search-string))
	      (dir-part (expand-file-name (or (he-file-name-directory
					       he-search-string) ""))))
	  (if (not (he-string-member name-part he-tried-table))
	      (setq he-tried-table (cons name-part he-tried-table)))
	  (if (and (not (equal he-search-string ""))
		   (he-file-directory-p dir-part))
	      (setq he-expand-list (sort (file-name-all-completions 
					  name-part
					  dir-part)
					 'string-lessp))
	      (setq he-expand-list ())))))

  (while (and he-expand-list
	      (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
      (let ((filename (he-concat-directory-file-name
		       (he-file-name-directory he-search-string)
		       (car he-expand-list))))
	(he-substitute-string filename)
	(setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
	(setq he-expand-list (cdr he-expand-list))
	t)))

(defun try-complete-file-name-partially (old)
  "Try to complete text as a file name, as many characters as unique.
The argument OLD has to be nil the first call of this function.  It
returns t if a unique, possibly partial, completion is found, nil 
otherwise."
  (let ((expansion ()))
    (if (not old)
	(progn 
	  (he-init-string (he-file-name-beg) (point))
	  (let ((name-part (he-file-name-nondirectory he-search-string))
		(dir-part (expand-file-name (or (he-file-name-directory
						 he-search-string) ""))))
	    (if (and (not (equal he-search-string ""))
		     (he-file-directory-p dir-part))
		(setq expansion (file-name-completion name-part
						      dir-part)))
	    (if (or (eq expansion t)
		    (string= expansion name-part)
		    (he-string-member expansion he-tried-table))
		(setq expansion ())))))

    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
	(let ((filename (he-concat-directory-file-name
			 (he-file-name-directory he-search-string)
			 expansion)))
	  (he-substitute-string filename)
	  (setq he-tried-table (cons expansion (cdr he-tried-table)))
	  t))))

(defvar he-file-name-chars
  (cond ((memq system-type '(vax-vms axp-vms))
	 "-a-zA-Z0-9_/.,~^#$+=:\\[\\]")
	((memq system-type '(ms-dos windows-nt))
	 "-a-zA-Z0-9_/.,~^#$+=:\\\\")
	(t			    ;; More strange file formats ?
	 "-a-zA-Z0-9_/.,~^#$+="))
  "Characters that are considered part of the file name to expand.")

(defun he-file-name-beg ()
  (save-excursion
    (skip-chars-backward he-file-name-chars)
    (point)))

;; Thanks go to Richard Levitte <levitte@e.kth.se> who helped to make these
;; work under VMS, and to David Hughes <ukchugd@ukpmr.cs.philips.nl> who 
;; helped to make it work on PC.
(defun he-file-name-nondirectory (file)
  "Fix to make `file-name-nondirectory' work for hippie-expand under VMS."
  (if (memq system-type '(axp-vms vax-vms))
      (let ((n (file-name-nondirectory file)))
	(if (string-match "^\\(\\[.*\\)\\.\\([^\\.]*\\)$" n)
	    (concat "[." (substring n (match-beginning 2) (match-end 2)))
	  n))
    (file-name-nondirectory file)))

(defun he-file-name-directory (file)
  "Fix to make `file-name-directory' work for hippie-expand under VMS."
  (if (memq system-type '(axp-vms vax-vms))
      (let ((n (file-name-nondirectory file))
	    (d (file-name-directory file)))
	(if (string-match "^\\(\\[.*\\)\\.\\([^\\.]*\\)$" n)
	    (concat d (substring n (match-beginning 1) (match-end 1)) "]")
	  d))
    (file-name-directory file)))

(defun he-file-directory-p (file)
  "Fix to make `file-directory-p' work for hippie-expand under VMS."
  (if (memq system-type '(vax-vms axp-vms))
      (or (file-directory-p file)
	  (file-directory-p (concat file "[000000]")))
    (file-directory-p file)))
  
(defun he-concat-directory-file-name (dir-part name-part)
  "Try to slam together two parts of a file specification, system dependently."
  (cond ((null dir-part) name-part)
	((memq system-type '(axp-vms vax-vms))
	 (if (and (string= (substring dir-part -1) "]")
		  (string= (substring name-part 0 2) "[."))
	     (concat (substring dir-part 0 -1) (substring name-part 1))
	   (concat dir-part name-part)))
	((memq system-type '(ms-dos mswindows))
	 (if (and (string-match "\\\\" dir-part)
		  (not (string-match "/" dir-part))
		  (= (aref name-part (1- (length name-part))) ?/))
	     (aset name-part (1- (length name-part)) ?\\))
	 (concat dir-part name-part))
	(t 
	 (concat dir-part name-part))))
			
(defun try-complete-lisp-symbol (old)
  "Try to complete word as an Emacs Lisp symbol.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (if (not old)
      (progn 
	(he-init-string (he-lisp-symbol-beg) (point))
	(if (not (he-string-member he-search-string he-tried-table))
	    (setq he-tried-table (cons he-search-string he-tried-table)))
	(setq he-expand-list 
	      (and (not (equal he-search-string ""))
		   (sort (all-completions he-search-string obarray
					  (function (lambda (sym)
					    (or (boundp sym)
						(fboundp sym)
						(symbol-plist sym)))))
			 'string-lessp)))))
  (while (and he-expand-list
	      (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))

(defun try-complete-lisp-symbol-partially (old)
  "Try to complete as an Emacs Lisp symbol, as many characters as unique.
The argument OLD has to be nil the first call of this function.  It
returns t if a unique, possibly partial, completion is found, nil 
otherwise."
  (let ((expansion ()))
    (if (not old)
	(progn 
	  (he-init-string (he-lisp-symbol-beg) (point))
	  (if (not (string= he-search-string ""))
	      (setq expansion 
		    (try-completion he-search-string obarray
				    (function (lambda (sym)
				      (or (boundp sym)
					  (fboundp sym)
					  (symbol-plist sym)))))))
	  (if (or (eq expansion t)
		  (string= expansion he-search-string)
		  (he-string-member expansion he-tried-table))
	      (setq expansion ()))))

  (if (not expansion)
      (progn
	(if old (he-reset-string))
	())
      (progn
	(he-substitute-string expansion)
	t))))

(defun he-lisp-symbol-beg ()
  (let ((skips "-a-zA-Z0-9_."))
    (save-excursion
      (skip-chars-backward skips)
      (point))))

(defun try-expand-line (old)
  "Try to complete the current line to an entire line in the buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ())
	(strip-prompt (and (get-buffer-process (current-buffer))
			   comint-prompt-regexp)))
    (if (not old)
	(progn
	  (he-init-string (he-line-beg strip-prompt) (point))
	  (set-marker he-search-loc he-string-beg)
	  (setq he-search-bw t)))

    (if (not (equal he-search-string ""))
	(save-excursion
	  ;; Try looking backward unless inhibited.
	  (if he-search-bw
	      (progn 
		(goto-char he-search-loc)
		(setq expansion (he-line-search he-search-string
						strip-prompt t))
		(set-marker he-search-loc (point))
		(if (not expansion)
		    (progn
		      (set-marker he-search-loc he-string-end)
		      (setq he-search-bw ())))))
	  
	  (if (not expansion) ; Then look forward.
	      (progn 
		(goto-char he-search-loc)
		(setq expansion (he-line-search he-search-string 
						strip-prompt nil))
		(set-marker he-search-loc (point))))))

    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
	(progn
	  (he-substitute-string expansion t)
	  t))))

(defun try-expand-line-all-buffers (old)
  "Try to complete the current line, searching all other buffers.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ())
	(strip-prompt (and (get-buffer-process (current-buffer))
			   comint-prompt-regexp))
	(buf (current-buffer))
	(orig-case-fold-search case-fold-search))
    (if (not old)
	(progn
	  (he-init-string (he-line-beg strip-prompt) (point))
	  (setq he-search-bufs (buffer-list))
	  (setq he-searched-n-bufs 0)
	  (set-marker he-search-loc 1 (car he-search-bufs))))

    (if (not (equal he-search-string ""))
	(while (and he-search-bufs 
		    (not expansion)
		    (or (not hippie-expand-max-buffers)
			(< he-searched-n-bufs hippie-expand-max-buffers)))
	  (set-buffer (car he-search-bufs))
	  (if (and (not (eq (current-buffer) buf))
		   (not (memq major-mode hippie-expand-ignore-buffers))
		   (not (he-regexp-member (buffer-name)
					  hippie-expand-ignore-buffers)))
	      (save-excursion
		(goto-char he-search-loc)
		(setq strip-prompt (and (get-buffer-process (current-buffer))
					comint-prompt-regexp))
		(setq expansion (let ((case-fold-search orig-case-fold-search))
				  (he-line-search he-search-string
						  strip-prompt nil)))
		(set-marker he-search-loc (point))
		(if (not expansion)
		    (progn
		      (setq he-search-bufs (cdr he-search-bufs))
		      (setq he-searched-n-bufs (1+ he-searched-n-bufs))
		      (set-marker he-search-loc 1 (car he-search-bufs)))))
	    (setq he-search-bufs (cdr he-search-bufs))
	    (set-marker he-search-loc 1 (car he-search-bufs)))))

    (set-buffer buf)
    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
	(progn
	  (he-substitute-string expansion t)
	  t))))

(defun he-line-search (str strip-prompt reverse) 
  (let ((result ()))
    (while (and (not result)
		(if reverse
		    (re-search-backward 
		     (he-line-search-regexp str strip-prompt)
		     nil t)
		    (re-search-forward
		     (he-line-search-regexp str strip-prompt)
		     nil t)))
      (setq result (buffer-substring (match-beginning 2) (match-end 2)))
      (if (he-string-member result he-tried-table t)
	  (setq result nil)))		    ; if already in table, ignore
    result))

(defun he-line-beg (strip-prompt)
  (save-excursion
    (if (re-search-backward (he-line-search-regexp "" strip-prompt) 
			    (save-excursion (beginning-of-line)
					    (point)) t)
	(match-beginning 2)
      (point))))

(defun he-line-search-regexp (pat strip-prompt)
  (if strip-prompt
      (concat "\\(" comint-prompt-regexp "\\|^\\s-*\\)\\("
	      (regexp-quote pat)
	      "[^\n]*[^ \t\n]\\)")
      (concat "^\\(\\s-*\\)\\(" 
	      (regexp-quote pat)
	      "[^\n]*[^ \t\n]\\)")))

(defun try-expand-list (old)
  "Try to complete the current beginning of a list.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ()))
    (if (not old)
	(progn
	  (he-init-string (he-list-beg) (point))
	  (set-marker he-search-loc he-string-beg)
	  (setq he-search-bw t)))

    (if (not (equal he-search-string ""))
	(save-excursion
	  ;; Try looking backward unless inhibited.
	  (if he-search-bw
	      (progn 
		(goto-char he-search-loc)
		(setq expansion (he-list-search he-search-string t))
		(set-marker he-search-loc (point))
		(if (not expansion)
		    (progn
		      (set-marker he-search-loc he-string-end)
		      (setq he-search-bw ())))))
	  
	  (if (not expansion) ; Then look forward.
	      (progn 
		(goto-char he-search-loc)
		(setq expansion (he-list-search he-search-string nil))
		(set-marker he-search-loc (point))))))

    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
	(progn
	  (he-substitute-string expansion t)
	  t))))

(defun try-expand-list-all-buffers (old)
  "Try to complete the current list, searching all other buffers.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ())
	(buf (current-buffer))
	(orig-case-fold-search case-fold-search))
    (if (not old)
	(progn
	  (he-init-string (he-list-beg) (point))
	  (setq he-search-bufs (buffer-list))
	  (setq he-searched-n-bufs 0)
	  (set-marker he-search-loc 1 (car he-search-bufs))))

    (if (not (equal he-search-string ""))
	(while (and he-search-bufs 
		    (not expansion)
		    (or (not hippie-expand-max-buffers)
			(< he-searched-n-bufs hippie-expand-max-buffers)))
	  (set-buffer (car he-search-bufs))
	  (if (and (not (eq (current-buffer) buf))
		   (not (memq major-mode hippie-expand-ignore-buffers))
		   (not (he-regexp-member (buffer-name)
					  hippie-expand-ignore-buffers)))
	      (save-excursion
		(goto-char he-search-loc)
		(setq expansion (let ((case-fold-search orig-case-fold-search))
				  (he-list-search he-search-string nil)))
		(set-marker he-search-loc (point))
		(if (not expansion)
		    (progn
		      (setq he-search-bufs (cdr he-search-bufs))
		      (setq he-searched-n-bufs (1+ he-searched-n-bufs))
		      (set-marker he-search-loc 1 (car he-search-bufs)))))
	    (setq he-search-bufs (cdr he-search-bufs))
	    (set-marker he-search-loc 1 (car he-search-bufs)))))

    (set-buffer buf)
    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
	(progn
	  (he-substitute-string expansion t)
	  t))))

(defun he-list-search (str reverse) 
  (let ((result ())
	beg pos err)
    (while (and (not result)
		(if reverse
		    (search-backward str nil t)
		    (search-forward str nil t)))
      (setq pos (point))
      (setq beg (match-beginning 0))
      (goto-char beg)
      (setq err ())
      (condition-case ()
	  (forward-list 1)
	(error (setq err t)))
      (if (and reverse 
	       (> (point) he-string-beg))
	  (setq err t))
      (if (not err)
	  (progn
	    (setq result (buffer-substring beg (point)))
	    (if (he-string-member result he-tried-table t)
		(setq result nil))))	       ; if already in table, ignore
      (goto-char pos))
    result))

(defun he-list-beg ()
  (save-excursion
    (condition-case ()
	(backward-up-list 1)
      (error ()))
    (point)))

(defun try-expand-all-abbrevs (old)
  "Try to expand word before point according to all abbrev tables.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (if (not old)
      (progn
	(he-init-string (he-dabbrev-beg) (point))
	(setq he-expand-list 
	      (and (not (equal he-search-string ""))
		   (mapcar (function (lambda (sym)
			     (if (and (boundp sym) (vectorp (eval sym)))
				 (abbrev-expansion (downcase he-search-string)
						   (eval sym)))))
			   (append '(local-abbrev-table 
				     global-abbrev-table)
				   abbrev-table-name-list))))))
  (while (and he-expand-list
	      (or (not (car he-expand-list))
		  (he-string-member (car he-expand-list) he-tried-table t)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
      (progn
	(he-substitute-string (car he-expand-list) t)
	(setq he-expand-list (cdr he-expand-list))
	t)))

(defun try-expand-dabbrev (old)
  "Try to expand word \"dynamically\", searching the current buffer.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (let ((expansion ()))
    (if (not old)
	(progn
	  (he-init-string (he-dabbrev-beg) (point))
	  (set-marker he-search-loc he-string-beg)
	  (setq he-search-bw t)))

    (if (not (equal he-search-string ""))
	(save-excursion
	  ;; Try looking backward unless inhibited.
	  (if he-search-bw
	      (progn 
		(goto-char he-search-loc)
		(setq expansion (he-dabbrev-search he-search-string t))
		(set-marker he-search-loc (point))
		(if (not expansion)
		    (progn
		      (set-marker he-search-loc he-string-end)
		      (setq he-search-bw ())))))
	  
	  (if (not expansion) ; Then look forward.
	      (progn 
		(goto-char he-search-loc)
		(setq expansion (he-dabbrev-search he-search-string nil))
		(set-marker he-search-loc (point))))))
	
    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
	(progn
	  (he-substitute-string expansion t)
	  t))))

(defun try-expand-dabbrev-all-buffers (old)
  "Tries to expand word \"dynamically\", searching all other buffers.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (let ((expansion ())
	(buf (current-buffer))
	(orig-case-fold-search case-fold-search))
    (if (not old)
	(progn
	  (he-init-string (he-dabbrev-beg) (point))
	  (setq he-search-bufs (buffer-list))
	  (setq he-searched-n-bufs 0)
	  (set-marker he-search-loc 1 (car he-search-bufs))))

    (if (not (equal he-search-string ""))
	(while (and he-search-bufs 
		    (not expansion)
		    (or (not hippie-expand-max-buffers)
			(< he-searched-n-bufs hippie-expand-max-buffers)))
	  (set-buffer (car he-search-bufs))
	  (if (and (not (eq (current-buffer) buf))
		   (not (memq major-mode hippie-expand-ignore-buffers))
		   (not (he-regexp-member (buffer-name)
					  hippie-expand-ignore-buffers)))
	      (save-excursion
		(goto-char he-search-loc)
		(setq expansion (let ((case-fold-search orig-case-fold-search))
				  (he-dabbrev-search he-search-string nil)))
		(set-marker he-search-loc (point))
		(if (not expansion)
		    (progn
		      (setq he-search-bufs (cdr he-search-bufs))
		      (setq he-searched-n-bufs (1+ he-searched-n-bufs))
		      (set-marker he-search-loc 1 (car he-search-bufs)))))
	    (setq he-search-bufs (cdr he-search-bufs))
	    (set-marker he-search-loc 1 (car he-search-bufs)))))

    (set-buffer buf)
    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
	(progn
	  (he-substitute-string expansion t)
	  t))))

;; Thanks go to Jeff Dairiki <dairiki@faraday.apl.washington.edu> who
;; suggested this one.
(defun try-expand-dabbrev-visible (old)
  "Try to expand word \"dynamically\", searching visible window parts.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible expansions of the same
string).  It returns t if a new expansion is found, nil otherwise."
  (let ((expansion ())
	(buf (current-buffer))
	(flag (if (frame-visible-p (window-frame (selected-window)))
		  'visible t)))
    (if (not old)
	(progn
	   (he-init-string (he-dabbrev-beg) (point))
	   (setq he-search-window (selected-window))
	   (set-marker he-search-loc
		       (window-start he-search-window)
		       (window-buffer he-search-window))))

    (while (and (not (equal he-search-string ""))
		(marker-position he-search-loc)
		(not expansion))
      (save-excursion
	(set-buffer (marker-buffer he-search-loc))
	(goto-char he-search-loc)
	(setq expansion (he-dabbrev-search he-search-string ()
					   (window-end he-search-window)))
	(if (and expansion
		 (eq (marker-buffer he-string-beg) (current-buffer))
		 (eq (marker-position he-string-beg) (match-beginning 0)))
	    (setq expansion (he-dabbrev-search he-search-string ()
					       (window-end he-search-window))))
	(set-marker he-search-loc (point) (current-buffer)))
      (if (not expansion)
	  (progn 
	    (setq he-search-window (next-window he-search-window nil flag))
	    (if (eq he-search-window (selected-window))
		(set-marker he-search-loc nil)
	      (set-marker he-search-loc (window-start he-search-window)
			  (window-buffer he-search-window))))))
    
    (set-buffer buf)
    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
	(progn
	  (he-substitute-string expansion t)
	  t))))

(defun he-dabbrev-search (pattern &optional reverse limit)
  (let ((result ())
	(regpat (if (eq (char-syntax (aref pattern 0)) ?_)
		    (concat (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+")
		  (concat "\\<" (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+"))))
    (while (and (not result) 
		(if reverse
		     (re-search-backward regpat limit t)
		     (re-search-forward regpat limit t)))
      (setq result (buffer-substring (match-beginning 0) (match-end 0)))
      (if (or (and (> (match-beginning 0) (point-min))
		   (memq (char-syntax (char-after (1- (match-beginning 0))))
			 '(?_ ?w)))
	      (he-string-member result he-tried-table t))
	  (setq result nil)))     ; ignore if bad prefix or already in table
    result))

(defvar he-dabbrev-skip-space ()
  "Non-NIL means tolerate trailing spaces in the abbreviation to expand.")

(defun he-dabbrev-beg ()
  (let ((op (point)))
    (save-excursion
      (if he-dabbrev-skip-space
	  (skip-syntax-backward ". "))
      (if (= (skip-syntax-backward "w_") 0)
	  op
	(point)))))

(defun try-expand-dabbrev-from-kill (old)
  "Try to expand word \"dynamically\", searching the kill ring.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ()))
    (if (not old)
	(progn 
	  (he-init-string (he-dabbrev-beg) (point))
	  (setq he-expand-list
		(if (not (equal he-search-string ""))
		    kill-ring))
	  (setq he-search-loc2 0)))
    (if (not (equal he-search-string ""))
	(setq expansion (he-dabbrev-kill-search he-search-string)))
    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
	(progn
	  (he-substitute-string expansion t)
	  t))))

(defun he-dabbrev-kill-search (pattern)
  (let ((result ())
	(regpat (if (eq (char-syntax (aref pattern 0)) ?_)
		    (concat (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+")
		  (concat "\\<" (regexp-quote pattern) "\\(\\sw\\|\\s_\\)+")))
	(killstr (car he-expand-list)))
    (while (and (not result) 
		he-expand-list)
      (while (and (not result)
		  (string-match regpat killstr he-search-loc2))
	(setq result (substring killstr (match-beginning 0) (match-end 0)))
	(setq he-search-loc2 (1+ (match-beginning 0)))
	(if (or (and (> (match-beginning 0) 0)
		     (memq (char-syntax (aref killstr (1- (match-beginning 0))))
			   '(?_ ?w)))
		(he-string-member result he-tried-table t))
	    (setq result nil)))     ; ignore if bad prefix or already in table
      (if (and (not result) 
		he-expand-list)
	  (progn
	    (setq he-expand-list (cdr he-expand-list)) 
	    (setq killstr (car he-expand-list))
	    (setq he-search-loc2 0))))
    result))

(defun try-expand-whole-kill (old)
  "Try to complete text with something from the kill ring.
The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (let ((expansion ()))
    (if (not old)
	(progn 
	  (he-init-string (he-kill-beg) (point))
	  (if (not (he-string-member he-search-string he-tried-table))
	      (setq he-tried-table (cons he-search-string he-tried-table)))
	  (setq he-expand-list 
		(if (not (equal he-search-string ""))
		    kill-ring))
	  (setq he-search-loc2 ())))
    (if (not (equal he-search-string ""))
	(setq expansion (he-whole-kill-search he-search-string)))
    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
	(progn
	  (he-substitute-string expansion)
	  t))))

(defun he-whole-kill-search (str)
  (let ((case-fold-search ())
	(result ())
	(str (regexp-quote str))
	(killstr (car he-expand-list))
	(pos -1))
    (while (and (not result)
		he-expand-list)
      (if (not he-search-loc2)
	  (while (setq pos (string-match str killstr (1+ pos)))
	    (setq he-search-loc2 (cons pos he-search-loc2))))
      (while (and (not result)
		  he-search-loc2)
	(setq pos (car he-search-loc2))
	(setq he-search-loc2 (cdr he-search-loc2))
	(save-excursion
	  (goto-char he-string-beg)
	  (if (and (>= (- (point) pos) (point-min))   ; avoid some string GC
		   (eq (char-after (- (point) pos)) (aref killstr 0))
		   (search-backward (substring killstr 0 pos)
				    (- (point) pos) t))
	      (setq result (substring killstr pos))))
	(if (and result
		 (he-string-member result he-tried-table))
	    (setq result nil)))     ; ignore if already in table
      (if (and (not result)
		he-expand-list)
	  (progn
	    (setq he-expand-list (cdr he-expand-list)) 
	    (setq killstr (car he-expand-list))
	    (setq pos -1))))
    result))

(defun he-kill-beg ()
  (let ((op (point)))
    (save-excursion
      (skip-syntax-backward "^w_")
      (if (= (skip-syntax-backward "w_") 0)
	  op
	(point)))))


(provide 'hippie-exp)

;;; hippie-exp.el ends here
