;;; rexx-mode.el --- major mode for editing REXX program files
;; Keywords: languages

;; Copyright (C) 1993 by Anders Lindgren.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; AUTHOR
;;;	Anders Lindgren, d91ali@csd.uu.se
;;;
;;;         Abbreviation table due to:
;;;	Johan Bergkvist, nv91-jbe@nada.kth.se
;;;
;;; USAGE
;;;	This file contains code for a GNU Emacs major mode for
;;;	editing REXX program files.
;;;
;;;     Type C-h m in Emacs for information on how to configurate
;;;	the rexx-mode, or see rexx-mode.doc.
;;;
;;;	Put the following lines into your .emacs and rexx-mode
;;;	will be automatically loaded when editing a REXX program.
;;;	If rexx-mode shall be used for files with other extensions
;;;	you can create more (cons ...) lines with these extensions.
;;;
;;;	(autoload 'rexx-mode "rexx-mode" "REXX mode" nil t)
;;;	(setq auto-mode-alist
;;;	      (append
;;;	       (list (cons "\\.rexx$"  'rexx-mode)
;;;		     (cons "\\.elx$"   'rexx-mode)
;;;		     (cons "\\.ncomm$" 'rexx-mode)
;;;		     (cons "\\.cpr$"   'rexx-mode)
;;;	       	     )
;;;	       auto-mode-alist))
;;;
;;; HISTORY
;;;	93-01-07 V0.1 ALi	Works for the first time.
;;;	92-01-11 V0.2 ALi	rexx-calc-indent totally rewritten.
;;;     93-03-08 V0.3 JB        rexx-indent-and-newline-and-indent added.
;;;                             Abbrev-table containing 173 entries created.
;;;                             rexx-check-expansion added.
;;;                             rexx-mode enables use of abbrev-table.
;;;	93-03-15 V0.4 ALi	abbrev-mode removed, better to call
;;;				 (abbrev-mode 1) from the hook.
;;;				case-fold-search set to t to recognize capital
;;;				 letters in keywords.
;;;				Old (setq case-fold-search nil) removed which
;;;				 prevented the recognition of END.
;;;				rexx-indent-and-newline-and-indent renamed to
;;;				 rexx-indent-newline-indent.
;;;				rexx-i-n-i now only expands abbrevs when
;;;				 buffer is in abbrev-mode.
;;;				New rexx-newline-and-indent added.
;;;	93-03-20      ALi	 A serious bug in the routine for checking
;;;				strings and comments found and fixed.
;;;			   V1.0 Relesed!	
;;;


(provide 'rexx-mode)

(defgroup rexx nil
  "Major mode for editing REXX program files"
  :group 'languages)


(defcustom rexx-indent 8
  "*This variable contains the indentation in rexx-mode."
  :type 'integer
  :group 'rexx)

(defcustom rexx-end-indent 0
  "*This variable indicates the relative position of the \"end\" in REXX mode."
  :type 'integer
  :group 'rexx)

(defcustom rexx-cont-indent 8
  "*This variable indicates how far a continued line shall be intended."
  :type 'integer
  :group 'rexx)

(defcustom rexx-comment-col 32
  "*This variable gives the desired comment column 
for comments to the right of text."
  :type 'integer
  :group 'rexx)

(defcustom rexx-tab-always-indent t
  "*Non-nil means TAB in REXX mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used."
  :type 'boolean
  :group 'rexx)

(defcustom rexx-special-regexp 
  ".*\\(,\\|then\\|else\\)[ \t]*\\(/\\*.*\\*/\\)?[ \t]*$"
  "*Regular expression for parsing lines which shall be followed by
a extra indention"
  :type 'regexp
  :group 'rexx)

(defconst rexx-font-lock-keywords
  (purecopy
   (list
    (cons (concat "\\<\\("
     (mapconcat 'identity
     '("address" "arg" "break" "call" "do" "drop" "echo" "else" "end"
      "exit" "if" "interpret" "iterate" "leave" "nop" "numeric"
      "options" "otherwise" "parse" "procedure" "pull" "push" "queue"
      "return" "say" "select" "shell" "signal" "then" "trace" "upper"
      "when" "value" "to" "by" "for" "forever" "while" "until" "form"
      "digits" "fuzz" "scientific" "engineering" "failat" "prompt"
      "results" "upper" "external" "source" "with" "command"
      "function" "var" "version" "expose" "on" "off")
     "\\|") "\\)\\>") 'font-lock-keyword-face)
   '("\\(\\sw+\\):" 1 font-lock-function-name-face)))
  "Additional expressions to highlight in Rexx mode.")
(put 'rexx-mode 'font-lock-defaults '(rexx-font-lock-keywords))

(defvar rexx-mode-map nil
  "Keymap for rexx-mode.")
(if rexx-mode-map
    nil
  (setq rexx-mode-map (make-sparse-keymap))
  (define-key rexx-mode-map "\t"   'rexx-indent-command)
  (define-key rexx-mode-map "\C-m"  'rexx-indent-and-newline)
  (define-key rexx-mode-map 'backspace 'backward-delete-char-untabify)
  (define-key rexx-mode-map "\C-c\C-p" 'rexx-find-matching-do)
  (define-key rexx-mode-map "\C-c\C-c" 'rexx-debug)
  )

(defvar rexx-mode-syntax-table nil
  "Syntax table in use in REXX-mode buffers.")

(if rexx-mode-syntax-table
    ()
  (setq rexx-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" rexx-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" rexx-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" rexx-mode-syntax-table)
  (modify-syntax-entry ?+ "." rexx-mode-syntax-table)
  (modify-syntax-entry ?- "." rexx-mode-syntax-table)
  (modify-syntax-entry ?= "." rexx-mode-syntax-table)
  (modify-syntax-entry ?% "." rexx-mode-syntax-table)
  (modify-syntax-entry ?< "." rexx-mode-syntax-table)
  (modify-syntax-entry ?> "." rexx-mode-syntax-table)
  (modify-syntax-entry ?& "." rexx-mode-syntax-table)
  (modify-syntax-entry ?| "." rexx-mode-syntax-table)
  (modify-syntax-entry ?. "_" rexx-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" rexx-mode-syntax-table))

(defvar rexx-mode-abbrev-table nil
  "*Abbrev table in use in rexx-mode buffers.")

(if rexx-mode-abbrev-table
    nil
  (define-abbrev-table 'rexx-mode-abbrev-table '(
     ("address" "ADDRESS" rexx-check-expansion 0)
     ("arg" "ARG" rexx-check-expansion 0)
     ("break" "BREAK" rexx-check-expansion 0)
     ("call" "CALL" rexx-check-expansion 0)
     ("do" "DO" rexx-check-expansion 0)
     ("drop" "DROP" rexx-check-expansion 0)
     ("echo" "ECHO" rexx-check-expansion 0)
     ("else" "ELSE" rexx-check-expansion 0)
     ("end" "END" rexx-check-expansion 0)
     ("exit" "EXIT" rexx-check-expansion 0)
     ("if" "IF" rexx-check-expansion 0)
     ("interpret" "INTERPRET" rexx-check-expansion 0)
     ("iterate" "ITERATE" rexx-check-expansion 0)
     ("leave" "LEAVE" rexx-check-expansion 0)
     ("nop" "NOP" rexx-check-expansion 0)
     ("numeric" "NUMERIC" rexx-check-expansion 0)
     ("options" "OPTIONS" rexx-check-expansion 0)
     ("otherwise" "OTHERWISE" rexx-check-expansion 0)
     ("parse" "PARSE" rexx-check-expansion 0)
     ("procedure" "PROCEDURE" rexx-check-expansion 0)
     ("pull" "PULL" rexx-check-expansion 0)
     ("push" "PUSH" rexx-check-expansion 0)
     ("queue" "QUEUE" rexx-check-expansion 0)
     ("return" "RETURN" rexx-check-expansion 0)
     ("say" "SAY" rexx-check-expansion 0)
     ("select" "SELECT" rexx-check-expansion 0)
     ("shell" "SHELL" rexx-check-expansion 0)
     ("signal" "SIGNAL" rexx-check-expansion 0)
     ("then" "THEN" rexx-check-expansion 0)
     ("trace" "TRACE" rexx-check-expansion 0)
     ("upper" "UPPER" rexx-check-expansion 0)
     ("when" "WHEN" rexx-check-expansion 0)
     ("value" "VALUE" rexx-check-expansion 0)
     ("to" "TO" rexx-check-expansion 0)
     ("by" "BY" rexx-check-expansion 0)
     ("for" "FOR" rexx-check-expansion 0)
     ("forever" "FOREVER" rexx-check-expansion 0)
     ("while" "WHILE" rexx-check-expansion 0)
     ("until" "UNTIL" rexx-check-expansion 0)
     ("form" "FORM" rexx-check-expansion 0)
     ("digits" "DIGITS" rexx-check-expansion 0)
     ("fuzz" "FUZZ" rexx-check-expansion 0)
     ("scientific" "SCIENTIFIC" rexx-check-expansion 0)
     ("engineering" "ENGINEERING" rexx-check-expansion 0)
     ("failat" "FAILAT" rexx-check-expansion 0)
     ("prompt" "PROMPT" rexx-check-expansion 0)
     ("results" "RESULTS" rexx-check-expansion 0)
     ("upper" "UPPER" rexx-check-expansion 0)
     ("external" "EXTERNAL" rexx-check-expansion 0)
     ("source" "SOURCE" rexx-check-expansion 0)
     ("with" "WITH" rexx-check-expansion 0)
     ("command" "COMMAND" rexx-check-expansion 0)
     ("function" "FUNCTION" rexx-check-expansion 0)
     ("var" "VAR" rexx-check-expansion 0)
     ("version" "VERSION" rexx-check-expansion 0)
     ("expose" "EXPOSE" rexx-check-expansion 0)
     ("on" "ON" rexx-check-expansion 0)
     ("off" "OFF" rexx-check-expansion 0)
     ("abbrev" "ABBREV" rexx-check-expansion 0)
     ("abs" "ABS" rexx-check-expansion 0)
     ("addlib" "ADDLIB" rexx-check-expansion 0)
     ("b2c" "B2C" rexx-check-expansion 0)
     ("bitand" "BITAND" rexx-check-expansion 0)
     ("bitchg" "BITCHG" rexx-check-expansion 0)
     ("bitclr" "BITCLR" rexx-check-expansion 0)
     ("bitcomp" "BITCOMP" rexx-check-expansion 0)
     ("bitor" "BITOR" rexx-check-expansion 0)
     ("bittst" "BITTST" rexx-check-expansion 0)
     ("bitset" "BITSET" rexx-check-expansion 0)
     ("c2b" "C2B" rexx-check-expansion 0)
     ("c2d" "C2D" rexx-check-expansion 0)
     ("c2x" "C2X" rexx-check-expansion 0)
     ("center" "CENTER" rexx-check-expansion 0)
     ("centre" "CENTRE" rexx-check-expansion 0)
     ("close" "CLOSE" rexx-check-expansion 0)
     ("compress" "COMPRESS" rexx-check-expansion 0)
     ("compare" "COMPARE" rexx-check-expansion 0)
     ("copies" "COPIES" rexx-check-expansion 0)
     ("d2c" "D2C" rexx-check-expansion 0)
     ("datatype" "DATATYPE" rexx-check-expansion 0)
     ("delstr" "DELSTR" rexx-check-expansion 0)
     ("delword" "DELWORD" rexx-check-expansion 0)
     ("eof" "EOF" rexx-check-expansion 0)
     ("errortext" "ERRORTEXT" rexx-check-expansion 0)
     ("exists" "EXISTS" rexx-check-expansion 0)
     ("export" "EXPORT" rexx-check-expansion 0)
     ("freespace" "FREESPACE" rexx-check-expansion 0)
     ("getclip" "GETCLIP" rexx-check-expansion 0)
     ("getspace" "GETSPACE" rexx-check-expansion 0)
     ("hash" "HASH" rexx-check-expansion 0)
     ("import" "IMPORT" rexx-check-expansion 0)
     ("index" "INDEX" rexx-check-expansion 0)
     ("insert" "INSERT" rexx-check-expansion 0)
     ("lastpos" "LASTPOS" rexx-check-expansion 0)
     ("left" "LEFT" rexx-check-expansion 0)
     ("length" "LENGTH" rexx-check-expansion 0)
     ("max" "MAX" rexx-check-expansion 0)
     ("min" "MIN" rexx-check-expansion 0)
     ("open" "OPEN" rexx-check-expansion 0)
     ("overlay" "OVERLAY" rexx-check-expansion 0)
     ("pos" "POS" rexx-check-expansion 0)
     ("pragma" "PRAGMA" rexx-check-expansion 0)
     ("random" "RANDOM" rexx-check-expansion 0)
     ("randu" "RANDU" rexx-check-expansion 0)
     ("readch" "READCH" rexx-check-expansion 0)
     ("readln" "READLN" rexx-check-expansion 0)
     ("remlib" "REMLIB" rexx-check-expansion 0)
     ("reverse" "REVERSE" rexx-check-expansion 0)
     ("right" "RIGHT" rexx-check-expansion 0)
     ("seek" "SEEK" rexx-check-expansion 0)
     ("setclip" "SETCLIP" rexx-check-expansion 0)
     ("show" "SHOW" rexx-check-expansion 0)
     ("sign" "SIGN" rexx-check-expansion 0)
     ("space" "SPACE" rexx-check-expansion 0)
     ("storage" "STORAGE" rexx-check-expansion 0)
     ("strip" "STRIP" rexx-check-expansion 0)
     ("substr" "SUBSTR" rexx-check-expansion 0)
     ("subword" "SUBWORD" rexx-check-expansion 0)
     ("symbol" "SYMBOL" rexx-check-expansion 0)
     ("time" "TIME" rexx-check-expansion 0)
     ("trace" "TRACE" rexx-check-expansion 0)
     ("translate" "TRANSLATE" rexx-check-expansion 0)
     ("trim" "TRIM" rexx-check-expansion 0)
     ("verify" "VERIFY" rexx-check-expansion 0)
     ("word" "WORD" rexx-check-expansion 0)
     ("wordindex" "WORDINDEX" rexx-check-expansion 0)
     ("wordlength" "WORDLENGTH" rexx-check-expansion 0)
     ("words" "WORDS" rexx-check-expansion 0)
     ("writech" "WRITECH" rexx-check-expansion 0)
     ("writeln" "WRITELN" rexx-check-expansion 0)
     ("x2c" "X2C" rexx-check-expansion 0)
     ("xrange" "XRANGE" rexx-check-expansion 0)
     ("allocmem" "ALLOCMEM" rexx-check-expansion 0)
     ("baddr" "BADDR" rexx-check-expansion 0)
     ("bitxor" "BITXOR" rexx-check-expansion 0)
     ("break_c" "BREAK_C" rexx-check-expansion 0)
     ("break_d" "BREAK_D" rexx-check-expansion 0)
     ("break_e" "BREAK_E" rexx-check-expansion 0)
     ("break_f" "BREAK_F" rexx-check-expansion 0)
     ("cache" "CACHE" rexx-check-expansion 0)
     ("closeport" "CLOSEPORT" rexx-check-expansion 0)
     ("d2x" "D2X" rexx-check-expansion 0)
     ("date" "DATA" rexx-check-expansion 0)
     ("delay" "DELAY" rexx-check-expansion 0)
     ("delete" "DELETE" rexx-check-expansion 0)
     ("error" "ERROR" rexx-check-expansion 0)
     ("failure" "FAILURE" rexx-check-expansion 0)
     ("find" "FIND" rexx-check-expansion 0)
     ("forbid" "FORBID" rexx-check-expansion 0)
     ("freemem" "FREEMEM" rexx-check-expansion 0)
     ("getarg" "GETARG" rexx-check-expansion 0)
     ("getpkt" "GETPKT" rexx-check-expansion 0)
     ("halt" "HALT" rexx-check-expansion 0)
     ("ioerr" "IOERR" rexx-check-expansion 0)
     ("lines" "LINES" rexx-check-expansion 0)
     ("makedir" "MAKEDIR" rexx-check-expansion 0)
     ("next" "NEXT" rexx-check-expansion 0)
     ("novalue" "NOVALUE" rexx-check-expansion 0)
     ("null" "NULL" rexx-check-expansion 0)
     ("offset" "OFFSET" rexx-check-expansion 0)
     ("openport" "OPENPORT" rexx-check-expansion 0)
     ("permit" "PERMIT" rexx-check-expansion 0)
     ("rename" "RENAME" rexx-check-expansion 0)
     ("reply" "REPLY" rexx-check-expansion 0)
     ("showdir" "SHOWDIR" rexx-check-expansion 0)
     ("showlist" "SHOWLIST" rexx-check-expansion 0)
     ("sourceline" "SOURCELINE" rexx-check-expansion 0)
     ("statef" "STATEF" rexx-check-expansion 0)
     ("syntax" "SYNTAX" rexx-check-expansion 0)
     ("trunc" "TRUNC" rexx-check-expansion 0)
     ("typepkt" "TYPEPKT" rexx-check-expansion 0)
     ("waitpkt" "WAITPKT" rexx-check-expansion 0)
     ("x2d" "X2D" rexx-check-expansion 0))))

;;;###autoload
(defun rexx-mode ()
"Major mode for editing REXX code.
\\{rexx-mode-map}

Variables controlling indentation style:
 rexx-indent
	The basic indentation for do-blocks.
 rexx-end-indent
	The relative offset of the \"end\" statement. 0 places it in the
	same column as the statements of the block. Setting it to the same
	value as rexx-indent places the \"end\" under the do-line.
 rexx-cont-indent
	The indention for lines following \"then\", \"else\" and \",\"
	(continued) lines.
 rexx-tab-always-indent
	Non-nil means TAB in REXX mode should always reindent the current 
	line, regardless of where in the line the point is when the TAB
	command is used.

If you have set rexx-end-indent to a nonzero value, you probably want to
remap RETURN to rexx-indent-newline-indent. It makes sure that lines
indents correctly when you press RETURN.

An extensive abbreviation table consisting of all the keywords of REXX are
supplied. Expanded keywords are converted into upper case making it
easier to distinguish them. To use this feature the buffer must be in
abbrev-mode. (See example below.)

Turning on REXX mode calls the value of the variable rexx-mode-hook with
no args, if that value is non-nil.

For example:
(setq rexx-mode-hook '(lambda ()
			(setq rexx-indent 4)
			(setq rexx-end-indent 4)
			(setq rexx-cont-indent 4)
			(local-set-key \"\\C-m\" 'rexx-indent-newline-indent)
			(abbrev-mode 1)
			))

will make the END aligned with the DO/SELECT. It will indent blocks and
IF-statements four steps and make sure that the END jumps into the
correct position when RETURN is pressed. Finally it will use the abbrev
table to convert all REXX keywords into upper case."
  (interactive)
  (kill-all-local-variables)
  (use-local-map rexx-mode-map)
  (set-syntax-table rexx-mode-syntax-table)
  (setq major-mode 'rexx-mode)
  (setq mode-name "REXX")
  (setq local-abbrev-table rexx-mode-abbrev-table)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'rexx-indent-command)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'rexx-comment-indent)
  (run-hooks 'rexx-mode-hook))


(defun rexx-indent-command (&optional whole-exp)
  "Indent the current line as REXX code."
  (interactive "P")
  (if whole-exp
      (let ((shift-amt (rexx-indent-line))
	    beg
	    end)
	(save-excursion
	  (if rexx-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt)))
    (if (and (not rexx-tab-always-indent)
	     (save-excursion
	      (skip-chars-backward " \t")
	      (not (bolp))))
	(insert-tab)
      (rexx-indent-line))))

(defun rexx-indent-line ()
  "Indent the current line as REXX code.
Return the amount the indentation changed by."
  (let ((indent (rexx-calc-indent))
	beg
	shift-amt
        (pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil) (setq indent (current-indentation)))
	  ((eq indent t) (setq indent (rexx-calculate-indent-within-comment)))
	  ((looking-at "[ \t]*#") (setq indent 0))
	  (t (skip-chars-forward " \t")
	     (if (listp indent) (setq indent (car indent)))
	      ;; /* Sprekspecifik kod! */
	     (if (looking-at "end") (setq indent (- indent rexx-end-indent)))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun rexx-calc-indent ()
  "Return the appropriate indentation for this line as an int."
  (save-excursion
    (beginning-of-line)
    (let ((block (rexx-find-environment))
	  beg
	  state
	  indent)
      (save-excursion (setq state (rexx-inside-comment-or-string)))
      (cond ((or (nth 3 state) (nth 4 state))
	     (nth 4 state))	;; Inside a comment or string
	    (t	
	     ;; Find line to indent current line after.
	     (rexx-backup-to-noncomment 1)
	     (beginning-of-line)
	     (setq beg (rexx-find-environment))
	     (while (> beg block)
	       (goto-char beg)
	       (beginning-of-line)
	       (setq beg (rexx-find-environment)))

	     (if (> (point) block)		 
		 ;; Check to see if we shall make a special indention
		 (if (looking-at rexx-special-regexp)
		     (+ (current-indentation) rexx-cont-indent)
		   ;; If not, find the basic indention by stepping
		   ;; by all special indented lines.
		   (progn
		     (setq indent (current-indentation))
		     (rexx-backup-to-noncomment 1)
		     (beginning-of-line)
		     (while (looking-at rexx-special-regexp)
		       (setq indent (current-indentation))
		       (rexx-backup-to-noncomment 1)
		       (beginning-of-line))
		     indent))
	       (if (= 1 block)
		   0
		 ;; Indent after the do-line.
		 (progn
		   (goto-char block)
		   (+ (current-indentation) rexx-indent)))))))))

(defun rexx-backup-to-noncomment (lim)
  "Backup the point to the previous noncomment REXX line."
  (let (stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (if (and (>= (point) (+ 2 lim))
	       (save-excursion
		 (forward-char -2)
		 (looking-at "\\*/")))
	  (search-backward "/*" lim 'move)
	(setq stop t)))
    (>= (point) lim)))

(defun rexx-find-environment ()
  "Return the position of the corresponding \"do\" or \"select\".
If none found, return the beginning of buffer."
  (save-excursion
    (let ((do-level 1)
	  (cont t)
	  state)
      (while (and cont (not (zerop do-level)))
	(setq cont (re-search-backward "\\b\\(do\\|select\\|end\\)\\b" 1 t))
	(save-excursion (setq state (rexx-inside-comment-or-string)))
	(setq do-level (+ do-level
			  (cond ((or (nth 3 state) (nth 4 state)) 0)
				((looking-at "do") -1)
				((looking-at "select") -1)
				((looking-at "end") +1)
				(t 0)))))

      (if cont (point) 1))))

(defun rexx-calculate-indent-within-comment ()
  "Return the indentation amount for line, assuming that
the current line is to be regarded as part of a block comment."
  (let (end star-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq star-start (= (following-char) ?\*))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (and (re-search-forward "/\\*[ \t]*" end t)
	   star-start
	   (goto-char (1+ (match-beginning 0))))
      (current-column))))

(defun rexx-comment-indent ()
  (if (looking-at "^/\\*")
      0				;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	;Else indent at comment column
	   comment-column))))	; except leave at least one space.

(defun rexx-find-matching-do ()
  "Set mark, look for the \"do\" or \"select\" for the present block."
  (interactive)
  (set-mark-command nil)
  (beginning-of-line)
  (goto-char (rexx-find-environment)))

(defun rexx-check-expansion ()
  "If abbrev was made within a comment or a string, de-abbrev!"
  (let ((state (rexx-inside-comment-or-string)))
    (if (or (nth 3 state) (nth 4 state))
	(unexpand-abbrev))))

(defun rexx-inside-comment-or-string ()
  "Check if the point is inside a comment or a string.
It returns the state from parse-partial-sexp for the search that
terminated on the points position"
  (let ((origpoint (point))
	state)
    (save-excursion 
      (goto-char 1)
      (while (> origpoint (point))
	(setq state (parse-partial-sexp (point) origpoint 0))))
    state))

(defun rexx-indent-and-newline ()
  "New newline-and-indent which expands abbrevs before running
a regular newline-and-indent."
  (interactive)
  (if abbrev-mode 
      (expand-abbrev))
  (newline-and-indent))

(defun rexx-indent-newline-indent ()
  "New newline-and-indent which expands abbrevs and indent the line
before running a regular newline-and-indent."
  (interactive)
  (rexx-indent-command)
  (if abbrev-mode 
      (expand-abbrev))
  (newline-and-indent))

;; XEmacs addition
;;;###autoload(add-to-list 'interpreter-mode-alist '("rexx" . rexx-mode))
