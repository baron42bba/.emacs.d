;;; teco.el --- Teco interpreter for Gnu Emacs, version 1.

;; Author: Dale R. Worley.
;; Keywords: emulations

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

;;; Synched up with: Not in FSF

;;; Commentary:

;; This code has been tested some, but no doubt contains a zillion bugs.
;; You have been warned.

;; Written by Dale R. Worley based on a C implementation by Matt Fichtenbaum.
;; Please send comments, bug fixes, enhancements, etc. to drw@math.mit.edu.

;; Emacs Lisp version copyright (C) 1991 by Dale R. Worley.
;; Do what you will with it.

;; Since much of this code is translated from the C version by 
;; Matt Fichtenbaum, I include his copyright notice:
;; TECO for Ultrix.   Copyright 1986 Matt Fichtenbaum.
;; This program and its components belong to GenRad Inc, Concord MA 01742.
;; They may be copied if this copyright notice is included.

;; To invoke directly, do:
;; (global-set-key ?\C-z 'teco-command)
;; (autoload teco-command "teco"
;;   "Read and execute a Teco command string."
;;   t nil)

;; Differences from other Tecos:
;; Character positions in the buffer are numbered in the Emacs way:  The first
;; character is numbered 1 (or (point-min) if narrowing is in effect).  The
;; B command returns that number.
;; Ends of lines are represented by a single character (newline), so C and R
;; skip over them, rather than 2C and 2R.
;; All file I/O is left to the underlying Emacs.  Thus, almost all Ex commands
;; are omitted.

;; Command set:
;;	NUL	Not a command.
;;	^A	Output message to terminal (argument ends with ^A)
;;	^C	Exit macro
;;	^C^C	Stop execution
;;	^D	Set radix to decimal
;;	^EA	(match char) Match alphabetics
;;	^EC	(match char) Match symbol constituents
;;	^ED	(match char) Match numerics
;;	^EGq	(match char) Match any char in q-reg
;;	^EL	(match char) Match line terminators
;;	^EQq	(string char) Use contents of q-reg
;;	^ER	(match char) Match alphanumerics
;;	^ES	(match char) Match non-null space/tab
;;	^EV	(match char) Match lower case alphabetic
;;	^EW	(match char) Match upper case alphabetic
;;	^EX	(match char) Match any char
;;	^G^G	(type-in) Kill command string
;;	^G<sp>	(type-in) Retype current command line
;;	^G*	(type-in) Retype current command input
;;	TAB	Insert tab and text
;;	LF	Line terminator; Ignored in commands
;;	VT	Ignored in commands
;;	FF	Ignored in commands
;;	CR	Ignored in commands
;;	^Nx	(match char) Match all but x
;;	^O	Set radix to octal
;;	^P	Find matching parenthesis
;;	^Q	Convert line argument into character argument
;;	^Qx	(string char) Use x literally
;;	n^R	Set radix to n
;;	:^R	Enter recursive edit
;;	^S	-(length of last referenced string)
;;	^S	(match char) match separator char
;;	^T	Ascii value of next character typed
;;	n^T	Output Ascii character with value n
;;	^U	(type-in) Kill command line
;;	^Uq	Put text argument into q-reg
;;	n^Uq	Put Ascii character 'n' into q-reg
;;	:^Uq	Append text argument to q-reg
;;	n:^Uq	Append character 'n' to q-reg
;;	^X	Set/get search mode flag
;;	^X	(match char) Match any character
;;	^Y	Equivalent to '.+^S,.'
;;	^Z	Not a Teco command
;;	ESC	String terminator; absorbs arguments
;;	ESC ESC	(type-in) End command
;;	^\	Not a Teco command
;;	^]	Not a Teco command
;;	^^x	Ascii value of the character x
;;	^_	One's complement (logical NOT)
;;	!	Define label (argument ends with !)
;;	"	Start conditional
;;	n"<	Test for less than zero
;;	n">	Test for greater than zero
;;	n"=	Test for equal to zero
;;	n"A	Test for alphabetic
;;	n"C	Test for symbol constituent
;;	n"D	Test for numeric
;;	n"E	Test for equal to zero
;;	n"F	Test for false
;;	n"G	Test for greater than zero
;;	n"L	Test for less than zero
;;	n"N	Test for not equal to zero
;;	n"R	Test for alphanumeric
;;	n"S	Test for successful
;;	n"T	Test for true
;;	n"U	Test for unsuccessful
;;	n"V	Test for lower case
;;	n"W	Test for upper case
;;	#	Logical OR
;;	$	Not a Teco command
;;	n%q	Add n to q-reg and return result
;;	&	Logical AND
;;	'	End conditional
;;	(	Expression grouping
;;	)	Expression grouping
;;	*	Multiplication
;;	+	Addition
;;	,	Argument separator
;;	-	Subtraction or negation
;;	.	Current pointer position
;;	/	Division
;;	0-9	Digit
;;	n<	Iterate n times
;;	=	Type in decimal
;;	:=	Type in decimal, no newline
;;	=	Type in octal
;;	:=	Type in octal, no newline
;;	=	Type in hexadecimal
;;	:=	Type in hexadecimal, no newline
;;	::	Make next search a compare
;;	>	End iteration
;;	n:A	Get Ascii code of character at relative position n
;;	B	Character position of beginning of buffer
;;	nC	Advance n characters
;;	nD	Delete n characters
;;	n,mD	Delete characters between n and m
;;	Gq	Get string from q-reg into buffer
;;	:Gq	Type out q-reg
;;	H	Equivalent to 'B,Z'
;;	I	Insert text argument
;;	nJ	Move pointer to character n
;;	nK	Kill n lines
;;	n,mK	Kill characters between n and m
;;	nL	Advance n lines
;;	Mq	Execute string in q-reg
;;	O	Goto label
;;	nO	Go to n-th label in list (0-origin)
;;	Qq	Number in q-reg
;;	nQq	Ascii value of n-th character in q-reg
;;	:Qq	Size of text in q-reg
;;	nR	Back up n characters
;;	nS	Search
;;	nT	Type n lines
;;	n,mT	Type chars from n to m
;;	nUq	Put number n into q-reg
;;	nV	Type n lines around pointer
;;	nXq	Put n lines into q-reg
;;	n,mXq	Put characters from n to m into q-reg
;;	n:Xq	Append n lines to q-reg q
;;	n,m:Xq	Append characters from n to m into q-reg
;;	Z 	Pointer position at end of buffer
;;	[q	Put q-reg on stack
;;	\	Value of digit string in buffer
;;	n\	Convert n to digits and insert in buffer
;;	]q	Pop q-reg from stack
;;	:]q	Test whether stack is empty and return value
;;	`	Not a Teco command
;;	a-z	Treated the same as A-Z
;;	{	Not a Teco command
;;	|	Conditional 'else'
;;	}	Not a Teco command
;;	~	Not a Teco command
;;	DEL	Delete last character typed in


;;; Code:
(require 'backquote)

;; set a range of elements of an array to a value
(defun teco-set-elements (array start end value)
  (let ((i start))
    (while (<= i end)
      (aset array i value)
      (setq i (1+ i)))))

;; set a range of elements of an array to their indexes plus an offset
(defun teco-set-elements-index (array start end offset)
  (let ((i start))
    (while (<= i end)
      (aset array i (+ i offset))
      (setq i (1+ i)))))

(defvar teco-command-string ""
  "The current command string being executed.")

(defvar teco-command-pointer nil
  "Pointer into teco-command-string showing next character to be executed.")

(defvar teco-ctrl-r 10
  "Current number radix.")

(defvar teco-digit-switch nil
  "Set if we have just executed a digit.")

(defvar teco-exp-exp nil
  "Expression value preceding operator.")

(defvar teco-exp-val1 nil
  "Current argument value.")

(defvar teco-exp-val2 nil
  "Argument before comma.")

(defvar teco-exp-flag1 nil
  "t if argument is present.")

(defvar teco-exp-flag2 nil
  "t if argument before comma is present.")

(defvar teco-exp-op nil
  "Pending arithmetic operation on argument.")

(defvar teco-exp-stack nil
  "Stack for parenthesized expressions.")

(defvar teco-macro-stack nil
  "Stack for macro invocations.")

(defvar teco-mapch-l nil
  "Translation table to lower-case letters.")

    (setq teco-mapch-l (make-vector 256 0))
    (teco-set-elements-index teco-mapch-l 0 255 0)
    (teco-set-elements-index teco-mapch-l ?A ?Z (- ?a ?A))

(defvar teco-trace nil
  "t if tracing is on.")

(defvar teco-at-flag nil
  "t if an @ flag is pending.")

(defvar teco-colon-flag nil
  "1 if a : flag is pending, 2 if a :: flag is pending.")

(defvar teco-qspec-valid nil
  "Flags describing whether a character is a vaid q-register name.
3 means yes, 2 means yes but only for file and search operations.")

    (setq teco-qspec-valid (make-vector 256 0))
    (teco-set-elements teco-qspec-valid ?a ?z 3)
    (teco-set-elements teco-qspec-valid ?0 ?9 3)
    (aset teco-qspec-valid ?_ 2)
    (aset teco-qspec-valid ?* 2)
    (aset teco-qspec-valid ?% 2)
    (aset teco-qspec-valid ?# 2)

(defvar teco-exec-flags 0
  "Flags for iteration in process, ei macro, etc.")

(defvar teco-iteration-stack nil
  "Iteration list.")

(defvar teco-cond-stack nil
  "Conditional stack.")

(defvar teco-qreg-text (make-vector 256 "")
  "The text contents of the q-registers.")

(defvar teco-qreg-number (make-vector 256 0)
  "The number contents of the q-registers.")

(defvar teco-qreg-stack nil
  "The stack of saved q-registers.")

(defconst teco-prompt "*"
  "*Prompt to be used when inputting Teco command.")

(defconst teco-exec-1 (make-vector 256 nil)
  "Names of routines handling type 1 characters (characters that are
part of expression processing).")

(defconst teco-exec-2 (make-vector 256 nil)
  "Names of routines handling type 2 characters (characters that are
not part of expression processing).")

(defvar teco-last-search-string ""
  "Last string searched for.")

(defvar teco-last-search-regexp ""
  "Regexp version of teco-last-search-string.")

(defmacro teco-define-type-1 (char &rest body)
  "Define the code to process a type 1 character.
Transforms
	(teco-define-type-1 ?x
	  code ...)
into
        (defun teco-type-1-x ()
	  code ...)
and does
	(aset teco-exec-1 ?x 'teco-type-1-x)"
  (let ((s (intern (concat "teco-type-1-" (char-to-string char)))))
    (` (progn
	 (defun (, s) ()
	   (,@ body))
	 (aset teco-exec-1 (, char) '(, s))))))

(defmacro teco-define-type-2 (char &rest body)
  "Define the code to process a type 2 character.
Transforms
	(teco-define-type-2 ?x
	  code ...)
into
        (defun teco-type-2-x ()
	  code ...)
and does
	(aset teco-exec-2 ?x 'teco-type-2-x)"
  (let ((s (intern (concat "teco-type-2-" (char-to-string char)))))
    (` (progn
	 (defun (, s) ()
	   (,@ body))
	 (aset teco-exec-2 (, char) '(, s))))))

(defconst teco-char-types (make-vector 256 0)
  "Define the characteristics of characters, as tested by \":
	1	alphabetic
	2	alphabetic, $, or .
	4	digit
	8	alphabetic or digit
	16	lower-case alphabetic
	32	upper-case alphabetic")

    (teco-set-elements teco-char-types ?0 ?9 (+ 4 8))
    (teco-set-elements teco-char-types ?A ?Z (+ 1 2 8 32))
    (teco-set-elements teco-char-types ?a ?z (+ 1 2 8 16))
    (aset teco-char-types ?$ 2)
    (aset teco-char-types ?. 2)

(defconst teco-error-texts '(("BNI" . "> not in iteration")
			     ("CPQ" . "Can't pop Q register")
			     ("COF" . "Can't open output file ")
			     ("FNF" . "File not found ")
			     ("IEC" . "Invalid E character")
			     ("IFC" . "Invalid F character")
			     ("IIA" . "Invalid insert arg")
			     ("ILL" . "Invalid command")
			     ("ILN" . "Invalid number")
			     ("IPA" . "Invalid P arg")
			     ("IQC" . "Invalid \" character")
			     ("IQN" . "Invalid Q-reg name")
			     ("IRA" . "Invalid radix arg")
			     ("ISA" . "Invalid search arg")
			     ("ISS" . "Invalid search string")
			     ("IUC" . "Invalid ^ character")
			     ("LNF" . "Label not found")
			     ("MEM" . "Insufficient memory available")
			     ("MRP" . "Missing )")
			     ("NAB" . "No arg before ^_")
			     ("NAC" . "No arg before ,")
			     ("NAE" . "No arg before =")
			     ("NAP" . "No arg before )")
			     ("NAQ" . "No arg before \"")
			     ("NAS" . "No arg before ;")
			     ("NAU" . "No arg before U")
			     ("NFI" . "No file for input")
			     ("NFO" . "No file for output")
			     ("NYA" . "Numeric arg with Y")
			     ("OFO" . "Output file already open")
			     ("PDO" . "Pushdown list overflow")
			     ("POP" . "Pointer off page")
			     ("SNI" . "; not in iteration")
			     ("SRH" . "Search failure ")
			     ("STL" . "String too long")
			     ("UTC" . "Unterminated command")
			     ("UTM" . "Unterminated macro")
			     ("XAB" . "Execution interrupted")
			     ("YCA" . "Y command suppressed")
			     ("IWA" . "Invalid W arg")
			     ("NFR" . "Numeric arg with FR")
			     ("INT" . "Internal error")
			     ("EFI" . "EOF read from std input")
			     ("IAA" . "Invalid A arg")
			     ))

(defconst teco-spec-chars 
  [
   0          1          0          0	; ^@ ^A ^B ^C
   0          64         0          0	; ^D ^E ^F ^G
   0          2          128        128	; ^H ^I ^J ^K
   128        0          64         0	; ^L ^M ^N ^O
   0          64         64         64	; ^P ^Q ^R ^S
   0          34         0          0	; ^T ^U ^V ^W
   64         0          0          0	; ^X ^Y ^Z ^\[
   0          0          1          0	; ^\ ^\] ^^ ^_
   0          1          16         0	;    !  \"  # 
   0          0          0          16	; $  %  &  ' 
   0          0          0          0	; \(  \)  *  + 
   0          0          0          0	; ,  -  .  / 
   0          0          0          0	; 0  1  2  3 
   0          0          0          0	; 4  5  6  7 
   0          0          0          0	; 8  9  :  ; 
   16         0          16         0	; <  =  >  ? 
   1          0          12         0	; @  A  B  C 
   0          1          1          32	; D  E  F  G 
   0          6          0          0	; H  I  J  K 
   0          32         10         2	; L  M  N  O 
   0          32         4          10	; P  Q  R  S 
   0          32         0          4	; T  U  V  W 
   32         0          0          32	; X  Y  Z  \[ 
   0          32         1          6	; \  \]  ^  _ 
   0          0          12         0	; `  a  b  c 
   0          1          1          32	; d  e  f  g 
   0          6          0          0	; h  i  j  k 
   0          32         10         2	; l  m  n  o 
   0          32         4          10	; p  q  r  s 
   0          32         0          4	; t  u  v  w 
   32         0          0          0	; x  y  z  { 
   16         0          0          0	; |  }  ~  DEL
   ]
  "The special properties of characters:
	1	skipto() special character
	2	command with std text argument
	4	E<char> takes a text argument
	8	F<char> takes a text argument
	16	char causes skipto() to exit
	32	command with q-register argument
	64	special char in search string
	128	character is a line separator")


(defun teco-execute-command (string)
  "Execute teco command string."
  ;; Initialize everything
  (let ((teco-command-string string)
	(teco-command-pointer 0)
	(teco-digit-switch nil)
	(teco-exp-exp nil)
	(teco-exp-val1 nil)
	(teco-exp-val2 nil)
	(teco-exp-flag1 nil)
	(teco-exp-flag2 nil)
	(teco-exp-op 'start)
	(teco-trace nil)
	(teco-at-flag nil)
	(teco-colon-flag nil)
	(teco-exec-flags 0)
	(teco-iteration-stack nil)
	(teco-cond-stack nil)
	(teco-exp-stack nil)
	(teco-macro-stack nil)
	(teco-qreg-stack nil))
    ;; initialize output
    (teco-out-init)
    ;; execute commands
    (catch 'teco-exit
      (while t
	;; get next command character
	(let ((cmdc (teco-get-command0 teco-trace)))
	  ;; if it's ^, interpret the next character as a control character
	  (if (eq cmdc ?^)
	      (setq cmdc (logand (teco-get-command teco-trace) 31)))
	  (if (and (<= ?0 cmdc) (<= cmdc ?9))
	      ;; process a number
	      (progn
		(setq cmdc (- cmdc ?0))
		;; check for invalid digit
		(if (>= cmdc teco-ctrl-r)
		    (teco-error "ILN"))
		(if teco-digit-switch
		    ;; later digits
		    (setq teco-exp-val1 (+ (* teco-exp-val1 teco-ctrl-r) cmdc))
		  ;; first digit
		  (setq teco-exp-val1 cmdc)
		  (setq teco-digit-switch t))
		;; indicate a value was read in
		(setq teco-exp-flag1 t))
	    ;; not a digit
	    (setq teco-digit-switch nil)
	    ;; cannonicalize the case
	    (setq cmdc (aref teco-mapch-l cmdc))
	    ;; dispatch on the character, if it is a type 1 character
	    (let ((r (aref teco-exec-1 cmdc)))
	      (if r
		  (funcall r)
		;; if a value has been entered, process any pending operation
		(if teco-exp-flag1
		    (cond ((eq teco-exp-op 'start)
			   nil)
			  ((eq teco-exp-op 'add)
			   (setq teco-exp-val1 (+ teco-exp-exp teco-exp-val1))
			   (setq teco-exp-op 'start))
			  ((eq teco-exp-op 'sub)
			   (setq teco-exp-val1 (- teco-exp-exp teco-exp-val1))
			   (setq teco-exp-op 'start))
			  ((eq teco-exp-op 'mult)
			   (setq teco-exp-val1 (* teco-exp-exp teco-exp-val1))
			   (setq teco-exp-op 'start))
			  ((eq teco-exp-op 'div)
			   (setq teco-exp-val1
				 (if (/= teco-exp-val1 0)
				     (/ teco-exp-exp teco-exp-val1)
				   0))
			   (setq teco-exp-op 'start))
			  ((eq teco-exp-op 'and)
			   (setq teco-exp-val1
				 (logand teco-exp-exp teco-exp-val1))
			   (setq teco-exp-op 'start))
			  ((eq teco-exp-op 'or)
			   (setq teco-exp-val1
				 (logior teco-exp-exp teco-exp-val1))
			   (setq teco-exp-op 'start))))
		;; dispatch on a type 2 character
		(let ((r (aref teco-exec-2 cmdc)))
		  (if r
		      (funcall r)
		    (teco-error "ILL")))))))))))

;; Type 1 commands

(teco-define-type-1
 ?\m					; CR
 nil)

(teco-define-type-1
 ?\n					; LF
 nil)

(teco-define-type-1
 ?\^k					; VT
 nil)

(teco-define-type-1
 ?\^l					; FF
 nil)

(teco-define-type-1
 32					; SPC
 nil)

(teco-define-type-1
 ?\e					; ESC
 (if (teco-peek-command ?\e)
     ;; ESC ESC terminates macro or command
     (teco-pop-macro-stack)
   ;; otherwise, consume argument
   (setq teco-exp-flag1 nil)
   (setq teco-exp-op 'start)))

(teco-define-type-1
 ?!					; !
 (while (/= (teco-get-command teco-trace) ?!)
   nil))

(teco-define-type-1
 ?@					; @
 ;; set at-flag
 (setq teco-at-flag t))

(teco-define-type-1
 ?:					; :
 ;; is it '::'?
 (if (teco-peek-command ?:)
     (progn
       ;; skip second colon
       (teco-get-command teco-trace)
       ;; set flag to show two colons
       (setq teco-colon-flag 2))
   ;; set flag to show one colon
   (setq teco-colon-flag 1)))

(teco-define-type-1
 ??					; ?
 ;; toggle trace
 (setq teco-trace (not teco-trace)))

(teco-define-type-1
 ?.					; .
 ;; value is point
 (setq teco-exp-val1 (point)
       teco-exp-flag1 t))

(teco-define-type-1
 ?z					; z
 ;; value is point-max
 (setq teco-exp-val1 (point-max)
       teco-exp-flag1 t))

(teco-define-type-1
 ?b					; b
 ;; value is point-min
 (setq teco-exp-val1 (point-min)
       teco-exp-flag1 t))

(teco-define-type-1
 ?h					; h
 ;; value is b,z
 (setq teco-exp-val1 (point-max)
       teco-exp-val2 (point-min)
       teco-exp-flag1 t
       teco-exp-flag2 t
       teco-exp-op 'start))

(teco-define-type-1
 ?\^s					; ^s
 ;; value is - length of last insert, etc.
 (setq teco-exp-val1 teco-ctrl-s
       teco-exp-flag1 t))

(teco-define-type-1
 ?\^y					; ^y
 ;; value is .+^S,.
 (setq teco-exp-val1 (+ (point) teco-ctrl-s)
       teco-exp-val2 (point)
       teco-exp-flag1 t
       teco-exp-flag2 t
       teco-exp-op 'start))

(teco-define-type-1
 ?\(					; \(
 ;; push expression stack
 (teco-push-exp-stack)
 (setq teco-exp-flag1 nil
       teco-exp-flag2 nil
       teco-exp-op 'start))

(teco-define-type-1
 ?\^p					; ^p
 (teco-do-ctrl-p))

(teco-define-type-1
 ?\C-^					; ^^
 ;; get next command character
 (setq teco-exp-val1 (teco-get-command teco-trace)
       teco-exp-flag1 t))


;; Type 2 commands
(teco-define-type-2
 ?+					; +
 (setq teco-exp-exp (if teco-exp-flag1 teco-exp-val1 0)
       teco-exp-flag1 nil
       teco-exp-op 'add))

(teco-define-type-2
 ?-					; -
 (setq teco-exp-exp (if teco-exp-flag1 teco-exp-val1 0)
       teco-exp-flag1 nil
       teco-exp-op 'sub))

(teco-define-type-2
 ?*					; *
 (setq teco-exp-exp (if teco-exp-flag1 teco-exp-val1 0)
       teco-exp-flag1 nil
       teco-exp-op 'mult))

(teco-define-type-2
 ?/					; /
 (setq teco-exp-exp (if teco-exp-flag1 teco-exp-val1 0)
       teco-exp-flag1 nil
       teco-exp-op 'div))

(teco-define-type-2
 ?&					; &
 (setq teco-exp-exp (if teco-exp-flag1 teco-exp-val1 0)
       teco-exp-flag1 nil
       teco-exp-op 'and))

(teco-define-type-2
 ?#					; #
 (setq teco-exp-exp (if teco-exp-flag1 teco-exp-val1 0)
       teco-exp-flag1 nil
       teco-exp-op 'or))

(teco-define-type-2
 ?\)					; \)
 (if (or (not teco-exp-flag1) (not teco-exp-stack))
     (teco-error "NAP"))
 (let ((v teco-exp-val1))
   (teco-pop-exp-stack)
   (setq teco-exp-val1 v
	 teco-exp-flag1 t)))

(teco-define-type-2
 ?,					; ,
 (if (not teco-exp-flag1)
     (teco-error "NAC"))
 (setq teco-exp-val2 teco-exp-val1
       teco-exp-flag2 t
       teco-exp-flag1 nil))

(teco-define-type-2
 ?\^_					; ^_
 (if (not teco-exp-flag1)
     (teco-error "NAB")
   (setq teco-exp-val1 (lognot teco-exp-val1))))

(teco-define-type-2
 ?\^d					; ^d
 (setq teco-ctrl-r 10
       teco-exp-flag1 nil
       teco-exp-op 'start))

(teco-define-type-2
 ?\^o					; ^o
 (setq teco-ctrl-r 8
       teco-exp-flag1 nil
       teco-exp-op 'start))

(teco-define-type-2
 ?\^r					; ^r
 (if teco-colon-flag
     (progn
       (recursive-edit)
       (setq teco-colon-flag nil))
   (if teco-exp-flag1
       ;; set radix
       (progn
	 (if (and (/= teco-exp-val1 8)
		  (/= teco-exp-val1 10)
		  (/= teco-exp-val1 16))
	     (teco-error "IRA"))
	 (setq teco-ctrl-r teco-exp-val1
	       teco-exp-flag1 nil
	       teco-exp-op 'start))
     ;; get radix
     (setq teco-exp-val1 teco-ctrl-r
	   teco-exp-flag1 t))))

(teco-define-type-2
 ?\^c					; ^c
 (if (teco-peek-command ?\^c)
     ;; ^C^C stops execution
     (throw 'teco-exit nil)
   (if teco-macro-stack
       ;; ^C inside macro exits macro
       (teco-pop-macro-stack)
     ;; ^C in command stops execution
     (throw 'teco-exit nil))))

(teco-define-type-2
 ?\^x					; ^x
 ;; set/get search mode flag
 (teco-set-var 'teco-ctrl-x))

(teco-define-type-2
 ?m					; m
 (let ((macro-name (teco-get-qspec nil
				   (teco-get-command teco-trace))))
   (teco-push-macro-stack)
   (setq teco-command-string (aref teco-qreg-text macro-name)
	 teco-command-pointer 0)))

(teco-define-type-2
 ?<					; <
 ;; begin iteration
 (if (and teco-exp-flag1 (<= teco-exp-val1 0))
     ;; if this is not to be executed, just skip the
     ;; intervening stuff
     (teco-find-enditer)
   ;; push iteration stack
   (teco-push-iter-stack teco-command-pointer
			 teco-exp-flag1 teco-exp-val1)
   ;; consume the argument
   (setq teco-exp-flag1 nil)))

(teco-define-type-2
 ?>					; >
 ;; end iteration
 (if (not teco-iteration-stack)
     (teco-error "BNI"))
 ;; decrement count and pop conditionally
 (teco-pop-iter-stack nil)
 ;; consume arguments
 (setq teco-exp-flag1 nil
       teco-exp-flag2 nil
       teco-exp-op 'start))

(teco-define-type-2
 59					; ;
 ;; semicolon iteration exit
 (if (not teco-iteration-stack)
     (teco-error "SNI"))
 ;; if exit
 (if (if (>= (if teco-exp-flag1
		 teco-exp-val1
	       teco-search-result) 0)
	 (not teco-colon-flag)
       teco-colon-flag)
     (progn
       (teco-find-enditer)
       (teco-pop-iter-stack t)))
 ;; consume argument and colon
 (setq teco-exp-flag1 nil
       teco-colon-flag nil
       teco-exp-op 'start))

(teco-define-type-2
 ?\"					; \"
 ;; must be an argument
 (if (not teco-exp-flag1)
     (teco-error "NAQ"))
 ;; consume argument
 (setq teco-exp-flag1 nil
       teco-exp-op 'start)
 (let* (;; get the test specification
	(c (aref teco-mapch-l (teco-get-command teco-trace)))
	;; determine whether the test is true
	(test (cond ((eq c ?a)
		     (/= (logand (aref teco-char-types teco-exp-val1)
				 1) 0))
		    ((eq c ?c)
		     (/= (logand (aref teco-char-types teco-exp-val1)
				 2) 0))
		    ((eq c ?d)
		     (/= (logand (aref teco-char-types teco-exp-val1)
				 4) 0))
		    ((or (eq c ?e) (eq c ?f) (eq c ?u) (eq c ?=))
		     (= teco-exp-val1 0))
		    ((or (eq c ?g) (eq c ?>))
		     (> teco-exp-val1 0))
		    ((or (eq c ?l) (eq c ?s) (eq c ?t) (eq c ?<))
		     (< teco-exp-val1 0))
		    ((eq c ?n)
		     (/= teco-exp-val1 0))
		    ((eq c ?r)
		     (/= (logand (aref teco-char-types teco-exp-val1)
				 8) 0))
		    ((eq c ?v)
		     (/= (logand (aref teco-char-types teco-exp-val1)
				 16) 0))
		    ((eq c ?w)
		     (/= (logand (aref teco-char-types teco-exp-val1)
				 32) 0))
		    (t
		     (teco-error "IQC")))))
   (if (not test)
       ;; if the conditional isn't satisfied, read
       ;; to matching | or '
       (let ((ll 1)
	     c)
	 (while (> ll 0)
	   (while (progn (setq c (teco-skipto))
			 (and (/= c ?\")
			      (/= c ?|)
			      (/= c ?\')))
	     (if (= c ?\")
		 (setq ll (1+ ll))
	       (if (= c ?\')
		   (setq ll (1- ll))
		 (if (= ll 1)
		     (break))))))))))

(teco-define-type-2
 ?'					; '
 ;; ignore it if executing
 t)

(teco-define-type-2
 ?|					; |
 (let ((ll 1)
       c)
   (while (> ll 0)
     (while (progn (setq c (teco-skipto))
		   (and (/= c ?\")
			(/= c ?\')))
       nil)
     (if (= c ?\")
	 (setq ll (1+ ll))
       (setq ll (1- ll))))))

(teco-define-type-2
 ?u					; u
 (if (not teco-exp-flag1)
     (teco-error "NAU"))
 (aset teco-qreg-number
       (teco-get-qspec 0 (teco-get-command teco-trace))
       teco-exp-val1)
 (setq teco-exp-flag1 teco-exp-flag2	; command's value is second arg
       teco-exp-val1 teco-exp-val2
       teco-exp-flag2 nil
       teco-exp-op 'start))

(teco-define-type-2
 ?q					; q
 ;; Qn is numeric val, :Qn is # of chars, mQn is mth char
 (let ((mm (teco-get-qspec (or teco-colon-flag teco-exp-flag1)
			   (teco-get-command teco-trace))))
   (if (not teco-exp-flag1)
       (setq teco-exp-val1 (if teco-colon-flag
			       ;; :Qn
			       (length (aref teco-qreg-text mm))
			     ;; Qn
			     (aref teco-qreg-number mm))
	     teco-exp-flag1 t)
     ;; mQn
     (let ((v (aref teco-qreg-text mm)))
       (setq teco-exp-val1 (condition-case nil
			       (aref v teco-exp-val1)
			     (error -1))
	     teco-exp-op 'start)))
   (setq teco-colon-flag nil)))

(teco-define-type-2
 ?%					; %
 (let* ((mm (teco-get-qspec nil (teco-get-command teco-trace)))
	(v (+ (aref teco-qreg-number mm) (teco-get-value 1))))
   (aset teco-qreg-number mm v)
   (setq teco-exp-val1 v
	 teco-exp-flag1 t)))

(teco-define-type-2
 ?c					; c
 (let ((p (+ (point) (teco-get-value 1))))
   (if (or (< p (point-min)) (> p (point-max)))
       (teco-error "POP")
     (goto-char p)
     (setq teco-exp-flag2 nil))))

(teco-define-type-2
 ?r					; r
 (let ((p (- (point) (teco-get-value 1))))
   (if (or (< p (point-min)) (> p (point-max)))
       (teco-error "POP")
     (goto-char p)
     (setq teco-exp-flag2 nil))))

(teco-define-type-2
 ?j					; j
 (let ((p (teco-get-value (point-min))))
   (if (or (< p (point-min)) (> p (point-max)))
       (teco-error "POP")
     (goto-char p)
     (setq teco-exp-flag2 nil))))

(teco-define-type-2
 ?l					; l
 ;; move forward by lines
 (forward-char (teco-lines (teco-get-value 1))))

(teco-define-type-2
 ?\C-q					; ^q
 ;; number of characters until the nth line feed
 (setq teco-exp-val1 (teco-lines (teco-get-value 1))
       teco-exp-flag1 t))

(teco-define-type-2
 ?=					; =
 ;; print numeric value
 (if (not teco-exp-flag1)
     (teco-error "NAE"))
 (teco-output (format
	       (if (teco-peek-command ?=)
		   ;; at least one more =
		   (progn
		     ;; read past it
		     (teco-get-command teco-trace)
		     (if (teco-peek-command ?=)
			 ;; another?
			 (progn
			   ;; read it too
			   (teco-get-command teco-trace)
			   ;; print in hex
			   "%x")
		       ;; print in octal
		       "%o"))
		 ;; print in decimal
		 "%d")
	       teco-exp-val1))
 ;; add newline if no colon
 (if (not teco-colon-flag)
     (teco-output ?\n))
 ;; absorb argument, etc.
 (setq teco-exp-flag1 nil
       teco-exp-flag2 nil
       teco-colon-flag nil
       teco-exp-op 'start))

(teco-define-type-2
 ?\t					; TAB
 (if exp-flag1
     (teco-error "IIA"))
 (let ((text (teco-get-text-arg)))
   (insert ?\t text)
   (setq teco-ctrl-s (1+ (length text))))
 ;; clear arguments
 (setq teco-colon-flag nil
       teco-exp-flag1 nil
       teco-exp-flag2 nil))

(teco-define-type-2
 ?i					; i
 (let ((text (teco-get-text-arg)))
   (if teco-exp-flag1
       ;; if a nI$ command
       (progn
	 ;; text argument must be null
	 (or (string-equal text "") (teco-error "IIA"))
	 ;; insert the character
	 (insert teco-exp-val1)
	 (setq teco-ctrl-s 1)
	 ;; consume argument
	 (setq teco-exp-op 'start))
     ;; otherwise, insert the text
     (insert text)
     (setq teco-ctrl-s (length text)))
   ;; clear arguments
   (setq teco-colon-flag nil
	 teco-exp-flag1 nil
	 teco-exp-flag2 nil)))

(teco-define-type-2
 ?t					; t
 (let ((args (teco-line-args nil)))
   (teco-output (buffer-substring (car args) (cdr args)))))

(teco-define-type-2
 ?v					; v
 (let ((ll (teco-get-value 1)))
   (teco-output (buffer-substring (+ (point) (teco-lines (- 1 ll)))
				  (+ (point) (teco-lines ll))))))

(teco-define-type-2
 ?\C-a					; ^a
 (teco-output (teco-get-text-arg nil ?\C-a))
 (setq teco-at-flag nil
       teco-colon-flag nil
       teco-exp-flag1 nil
       teco-exp-flag2 nil
       teco-exp-op 'start))

(teco-define-type-2
 ?d					; d
 (if (not teco-exp-flag2)
     ;; if only one argument
     (delete-char (teco-get-value 1))
   ;; if two arguments, treat as n,mK
   (let ((ll (teco-line-args 1)))
     (delete-region (car ll) (cdr ll)))))

(teco-define-type-2
 ?k					; k
 (let ((ll (teco-line-args 1)))
   (delete-region (car ll) (cdr ll))))

(teco-define-type-2
 ?\C-u					; ^u
 (let* ((mm (teco-get-qspec nil (teco-get-command teco-trace)))
	(text-arg (teco-get-text-arg))
	(text (if (not teco-exp-flag1)
		  text-arg
		(if (string-equal text-arg "")
		    (char-to-string teco-exp-val1)
		  (teco-error "IIA")))))
   ;; if :, append to the register
   (aset teco-qreg-text mm (if teco-colon-flag
			       (concat (aref teco-qreg-text mm) text)
			     text))
   ;; clear various flags
   (setq teco-exp-flag1 nil
	 teco-at-flag nil
	 teco-colon-flag nil
	 teco-exp-flag1 nil)))

(teco-define-type-2
 ?x					; x
 (let* ((mm (teco-get-qspec nil (teco-get-command teco-trace)))
	(args (teco-line-args 0))
	(text (buffer-substring (car args) (cdr args))))
   ;; if :, append to the register
   (aset teco-qreg-text mm (if teco-colon-flag
			       (concat (aref teco-qreg-text mm) text)
			     text))
   ;; clear various flags
   (setq teco-exp-flag1 nil
	 teco-at-flag nil
	 teco-colon-flag nil
	 teco-exp-flag1 nil)))

(teco-define-type-2
 ?g					; g
 (let ((mm (teco-get-qspec t (teco-get-command teco-trace))))
   (if teco-colon-flag
       (teco-output (aref teco-qreg-text mm))
     (insert (aref teco-qreg-text mm)))
   (setq teco-colon-flag nil)))

(teco-define-type-2
 ?\[					; \[
 (let ((mm (teco-get-qspec t (teco-get-command teco-trace))))
   (setq teco-qreg-stack
	 (cons (cons (aref teco-qreg-text mm)
		     (aref teco-qreg-number mm))
	       teco-qreg-stack))))

(teco-define-type-2
 ?\]					; \]
 (let ((mm (teco-get-qspec t (teco-get-command teco-trace))))
   (if teco-colon-flag
       (setq teco-exp-flag1 t
	     teco-exp-val1 (if teco-qreg-stack -1 0))
     (if teco-qreg-stack
	 (let ((pop (car teco-qreg-stack)))
	   (aset teco-qreg-text mm (car pop))
	   (aset teco-qreg-number mm (cdr pop))
	   (setq teco-qreg-stack (cdr teco-qreg-stack)))
       (teco-error "CPQ")))
   (setq teco-colon-flag nil)))

(teco-define-type-2
 ?\\					; \
 (if (not teco-exp-flag1)
     ;; no argument; read number
     (let ((p (point))
	   (sign +1)
	   (n 0)
	   c)
       (setq c (char-after p))
       (if c
	   (if (= c ?+)
	       (setq p (1+ p))
	     (if (= c ?-)
		 (setq p (1+ p)
		       sign -1))))
       (cond
	((= teco-ctrl-r 8) 
	 (while (progn
		  (setq c (char-after p))
		  (and c (>= c ?0) (<= c ?7)))
	   (setq p (1+ p)
		 n (+ c -48 (* n 8)))))
	((= teco-ctrl-r 10) 
	 (while (progn
		  (setq c (char-after p))
		  (and c (>= c ?0) (<= c ?9)))
	   (setq p (1+ p)
		 n (+ c -48 (* n 10)))))
	(t
	 (while (progn
		  (setq c (char-after p))
		  (and c
		       (or
			(and (>= c ?0) (<= c ?9))
			(and (>= c ?a) (<= c ?f))
			(and (>= c ?A) (<= c ?F)))))
	   (setq p (1+ p)
		 n (+ c (if (> c ?F)
			    ;; convert 'a' to 10
			    -87 
			  (if (> c ?9)
			      ;; convert 'A' to 10
			      -55
			    ;; convert '0' to 0
			    -48))
		      (* n 16))))))
       (setq teco-exp-val1 (* n sign)
	     teco-exp-flag1 t
	     teco-ctrl-s (- (point) p)))
   ;; argument: insert it as a digit string
   (insert (format (cond 
		    ((= teco-ctrl-r 8) "%o")
		    ((= teco-ctrl-r 10) "%d")
		    (t "%x"))
		   teco-exp-val1))
   (setq teco-exp-flag1 nil
	 teco-exp-op 'start)))

(teco-define-type-2
 ?\C-t					; ^t
 (if teco-exp-flag1
     ;; type a character
     (progn
       (teco-output teco-exp-val1)
       (setq teco-exp-flag1 nil))
   ;; input a character
   (let* ((echo-keystrokes 0)
	  (c (read-char)))
     (teco-output c)
     (setq teco-exp-val1 c
	   teco-exp-flag1 t))))

(teco-define-type-2
 ?s					; s
 (let ((arg (teco-get-text-arg))
       (count (if teco-exp-flag1 teco-expr-val1 1))
       regexp)
   (if (not (string-equal arg ""))
       (setq regexp (teco-parse-search-string arg)
	     teco-last-search-string arg
	     teco-last-search-regexp regexp)
     (setq regexp (teco-last-search-regexp)
	   arg teco-last-search-string))
   (let ((p (point))
	 (result (cond
		  ((> count 0)
		   (re-search-forward regexp nil t count))
		  ((< count 0)
		   (re-search-backward regexp nil t count))
		  (t
		   ;; 0s always is successful
		   t))))
     ;; if ::s, restore point
     (if (eq teco-colon-flag 2)
	 (goto-char p))
     ;; if no real or implied colon, error if not found
     (if (and (not result)
	      (not teco-colon-flag)
	      (/= (teco-peekcmdc) 34))
	 (teco-error "SRH"))
     ;; set return results
     (setq teco-exp-flag2 nil
	   teco-colon-flag nil
	   teco-at-flag nil
	   teco-exp-op 'start)
     (if teco-colon-flag
	 (setq teco-exp-flag1 t
	       teco-exp-val1 (if result -1 0))
       (setq teco-exp-flag1 nil)))))

(defun teco-parse-search-string (s)
  (let ((i 0)
	(l (length s))
	(r "")
	c)
    (while (< i l)
      (setq r (concat r (teco-parse-search-string-1))))
    r))

(defun teco-parse-search-string-1 ()
  (if (>= i l)
      (teco-error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (cond
   ((eq c ?\C-e)			; ^E - special match characters
    (teco-parse-search-string-e))
   ((eq c ?\C-n)			; ^Nx - match all but x
    (teco-parse-search-string-n))
   ((eq c ?\C-q)			; ^Qx - use x literally
    (teco-parse-search-string-q))
   ((eq c ?\C-s)			; ^S - match separator chars
    "[^A-Za-z0-9]")
   ((eq c ?\C-x)			; ^X - match any character
    "[\000-\377]")
   (t					; ordinary character
    (teco-parse-search-string-char c))))

(defun teco-parse-search-string-char (c)
  (regexp-quote (char-to-string c)))

(defun teco-parse-search-string-q ()
  (if (>= i l)
      (teco-error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (teco-parse-search-string-char c))

(defun teco-parse-search-string-e ()
  (if (>= i l)
      (teco-error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (cond
   ((or (eq c ?a) (eq c ?A))		; ^EA - match alphabetics
    "[A-Za-z]")
   ((or (eq c ?c) (eq c ?C))		; ^EC - match symbol constituents
    "[A-Za-z.$]")
   ((or (eq c ?d) (eq c ?D))		; ^ED - match numerics
    "[0-9]")
   ((eq c ?g)				; ^EGq - match any char in q-reg
    (teco-parse-search-string-e-g))
   ((or (eq c ?l) (eq c ?L))		; ^EL - match line terminators
    "[\012\013\014]")
   ((eq c ?q)				; ^EQq - use contents of q-reg
    (teco-parse-search-string-e-q))
   ((eq c ?r)				; ^ER - match alphanumerics
    "[A-Za-z0-9]")
   ((eq c ?s)				; ^ES - match non-null space/tab seq
    "[ \t]+")
   ((eq c ?v)				; ^EV - match lower case alphabetic
    "[a-z]")
   ((eq c ?w)				; ^EW - match upper case alphabetic
    "[A-Z]")
   ((eq c ?x)				; ^EX - match any character
    "[\000-\377]")
   (t
    (teco-error "ISS"))))

(defun teco-parse-search-string-e-q ()
  (if (>= i l)
      (teco-error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (regexp-quote (aref reco:q-reg-text c)))

(defun teco-parse-search-string-e-g ()
  (if (>= i l)
      (teco-error "ISS"))
  (setq c (aref s i))
  (setq i (1+ i))
  (let* ((q (aref teco-qreg-text c))
	 (len (length q))
	 (null (= len 0))
	 (one-char (= len 1))
	 (dash-present (string-match "-" q))
	 (caret-present (string-match "\\^" q))
	 (outbracket-present (string-match "]" q))
	 p)
    (cond
     (null
      "[^\000-\377]")
     (one-char
      (teco-parse-search-string-char c))
     (t
      (while (setq p (string-match "^]\\^"))
	(setq q (concat (substring q 1 p) (substring q (1+ p)))))
      (concat
       "["
       (if outbracket-present "]" "")
       (if dash-present "---" "")
       q
       (if caret-present "^" ""))))))

(defun teco-parse-search-string-n ()
  (let ((p (teco-parse-search-string-1)))
    (cond
     ((= (aref p 0) ?\[)
      (if (= (aref p 1) ?^)
	  ;; complement character set
	  (if (= (length p) 4)
	      ;; complement of one character
	      (teco-parse-search-string-char (aref p 2))
	    ;; complement of more than one character
	    (concat "[" (substring p 2)))
	;; character set - invert it
      (concat "[^" (substring p 1))))
     ((= (aref p 0) ?\\)
      ;; single quoted character
      (concat "[^" (substring p 1) "]"))
     (t
      ;; single character
      (if (string-equal p "-")
	  "[^---]"
	(concat "[^" p "]"))))))

(teco-define-type-2
 ?o					; o
 (let ((label (teco-get-text-arg))
       (index (and teco-exp-flag1 teco-exp-val1)))
   (setq teco-exp-flag1 nil)
   ;; handle computed goto by extracting the proper label
   (if index
       (if (< index 0)
	   ;; argument < 0 is a noop
	   (setq label "")
	 ;; otherwise, find the n-th label (0-origin)
	 (setq label (concat label ","))
	 (let ((p 0))
	   (while (and (> index 0)
		       (setq p (string-match "," label p))
		       (setq p (1+ p)))
	     (setq index (1- index)))
	   (setq q (string-match "," label p))
	   (setq label (substring label p q)))))
   ;; if the label is non-null, find the correct label
   ;; start from beginning of iteration or macro, and look for tag
   (setq teco-command-pointer
	 (if teco-iteration-stack
	     ;; if in iteration, start at beginning of iteration
	     (aref (car teco-iteration-stack) 0)
	   ;; if not in iteration, start at beginning of command or macro
	   0))
   ;; search for tag
   (catch 'label
     (let ((level 0)
	   c p l)
       ;; look for interesting things, including !
       (while t
	 (setq c (teco-skipto t))
	 (cond
	  ((= c ?<)			; start of iteration
	   (setq level (1+ level)))
	  ((= c ?>)			; end of iteration
	   (if (= level 0)
	       (teco-pop-iter-stack t)
	     (setq level (1- level))))
	  ((= c ?!)			; start of tag
	   (setq p (string-match "!" teco-command-string teco-command-pointer))
	   (if (and p
		    (string-equal label (substring teco-command-string
						   teco-command-pointer
						   p)))
	       (progn
		 (setq teco-command-pointer (1+ p))
		 (throw 'label nil))))))))))

(teco-define-type-2
 ?a					; :a
 ;; 'a' must be used as ':a'
 (if (and teco-exp-flag1 teco-colon-flag)
     (let ((char (+ (point) teco-exp-val1)))
       (setq teco-exp-val1
	     (if (and (>= char (point-min))
		      (< char (point-max)))
		 (char-after char)
	       -1)
	     teco-colon-flag nil))
   (teco-error "ILL")))


;; Routines to get next character from command buffer
;; getcmdc0, when reading beyond command string, pops
;; macro stack and continues.
;; getcmdc, in similar circumstances, reports an error.
;; If pushcmdc() has returned any chars, read them first
;; routines type characters as read, if argument != 0.

(defun teco-get-command0 (trace)
  ;; get the next character
  (let (char)
    (while (not (condition-case nil
		    (setq char (aref teco-command-string teco-command-pointer))
		  ;; if we've exhausted the string, pop the macro stack
		  ;; if we exhaust the macro stack, exit
		  (error (teco-pop-macro-stack)
			 nil))))
    ;; bump the command pointer
    (setq teco-command-pointer (1+ teco-command-pointer))
    ;; trace, if requested
    (and trace (teco-trace-type char))
    ;; return the character
    char))

;; 	while (cptr.dot >= cptr.z)		/* if at end of this level, pop macro stack
;; 		{
;; 		if (--msp < &mstack[0])		/* pop stack; if top level
;; 			{
;; 			msp = &mstack[0];		/* restore stack pointer
;; 			cmdc = ESC;				/* return an ESC (ignored)
;; 			exitflag = 1;			/* set to terminate execution
;; 			return(cmdc);			/* exit "while" and return
;; 			}
;; 		}
;; 	cmdc = cptr.p->ch[cptr.c++];		/* get char
;; 	++cptr.dot;							/* increment character count
;; 	if (trace) type_char(cmdc);			/* trace
;; 	if (cptr.c > CELLSIZE-1)			/* and chain if need be
;; 		{
;; 		cptr.p = cptr.p->f;
;; 		cptr.c = 0;
;; 		}
;; 	return(cmdc);
;; 	}


(defun teco-get-command (trace)
  ;; get the next character
  (let ((char (condition-case nil
		  (aref teco-command-string teco-command-pointer)
		;; if we've exhausted the string, give error
		(error
		 (teco-error (if teco-macro-stack "UTM" "UTC"))))))
    ;; bump the command pointer
    (setq teco-command-pointer (1+ teco-command-pointer))
    ;; trace, if requested
    (and trace (teco-trace-type char))
    ;; return the character
    char))

;; char getcmdc(trace)
;; 	{
;; 	if (cptr.dot++ >= cptr.z) ERROR((msp <= &mstack[0]) ? E_UTC : E_UTM);
;; 	else
;; 		{
;; 		cmdc = cptr.p->ch[cptr.c++];	/* get char
;; 		if (trace) type_char(cmdc);		/* trace
;; 		if (cptr.c > CELLSIZE-1)		/* and chain if need be
;; 			{
;; 			cptr.p = cptr.p->f;
;; 			cptr.c = 0;
;; 			}
;; 		}
;; 	return(cmdc);
;; 	}


;; peek at next char in command string, return 1 if it is equal
;; (case independent) to argument

(defun teco-peek-command (arg)
  (condition-case nil
      (eq (aref teco-mapch-l (aref teco-command-string teco-command-pointer))
	  (aref teco-mapch-l arg))
    (error nil)))

;; int peekcmdc(arg)
;; 	char arg;
;; 	{
;; 	return(((cptr.dot < cptr.z) && (mapch_l[cptr.p->ch[cptr.c]] == mapch_l[arg])) ? 1 : 0);
;; 	}

(defun teco-get-text-arg (&optional term-char default-term-char)
  ;; figure out what the terminating character is
  (setq teco-term-char (or term-char
			   (if teco-at-flag
			       (teco-get-command teco-trace)
			     (or default-term-char
				 ?\e)))
	teco-at_flag nil)
  (let ((s "")
	c)
    (while (progn
	     (setq c (teco-get-command teco-trace))
	     (/= c teco-term-char))
      (setq s (concat s (char-to-string c))))
    s))


;; Routines to manipulate the stacks

;; Pop the macro stack.  Throw to 'teco-exit' if the stack is empty.
(defun teco-pop-macro-stack ()
  (if teco-macro-stack
      (let ((frame (car teco-macro-stack)))
	(setq teco-macro-stack (cdr teco-macro-stack)
	      teco-command-string (aref frame 0)
	      teco-command-pointer (aref frame 1)
	      teco-exec-flags (aref frame 2)
	      teco-iteration-stack (aref frame 3)
	      teco-cond-stack (aref frame 4)))
    (throw 'teco-exit nil)))

;; Push the macro stack.
(defun teco-push-macro-stack ()
  (setq teco-macro-stack
	(cons (vector teco-command-string
		      teco-command-pointer
		      teco-exec-flags
		      teco-iteration-stack
		      teco-cond-stack)
	      teco-macro-stack)))

;; Pop the expression stack.
(defun teco-pop-exp-stack ()
  (let ((frame (car teco-exp-stack)))
    (setq teco-exp-stack (cdr teco-exp-stack)
	  teco-exp-val1 (aref frame 0)
	  teco-exp-flag1 (aref frame 1)
	  teco-exp-val2 (aref frame 2)
	  teco-exp-flag2 (aref frame 3)
	  teco-exp-exp (aref frame 4)
	  teco-exp-op (aref frame 5))))

;; Push the expression stack.
(defun teco-push-exp-stack ()
  (setq teco-exp-stack
	(cons (vector teco-exp-val1
		      teco-exp-flag1
		      teco-exp-val2
		      teco-exp-flag2
		      teco-exp-exp
		      teco-exp-op)
	      teco-exp-stack)))

;; Pop the iteration stack
;; if arg t, exit unconditionally
;; else check exit conditions and exit or reiterate
(defun teco-pop-iter-stack (arg)
  (let ((frame (car teco-iteration-stack)))
    (if (or arg
	    (not (aref frame 1))
	    ;; test against 1, since one iteration has already been done
	    (<= (aref frame 2) 1))
	;; exit iteration
	(setq teco-iteration-stack (cdr teco-iteration-stack))
      ;; continue with iteration
      ;; decrement count
      (aset frame 2 (1- (aref frame 2)))
      ;; reset command pointer
      (setq teco-command-pointer (aref frame 0)))))

;; Push the iteration stack
(defun teco-push-iter-stack (pointer flag count)
  (setq teco-iteration-stack
	(cons (vector pointer
		      flag
		      count)
	      teco-iteration-stack)))	      

(defun teco-find-enditer ()
  (let ((icnt 1)
	c)
    (while (> icnt 0)
      (while (progn (setq c (teco-skipto))
		    (and (/= c ?<)
			 (/= c ?>)))
	(if (= c ?<)
	    (setq icnt (1+ icnt))
	  (setq icnt (1- icnt)))))))


;; I/O routines

(defvar teco-output-buffer (get-buffer-create "*Teco Output*")
  "The buffer into which Teco output is written.")

(defun teco-out-init ()
  ;; Recreate the teco output buffer, if necessary
  (setq teco-output-buffer (get-buffer-create "*Teco Output*"))
  (save-excursion
    (set-buffer teco-output-buffer)
    ;; get a fresh line in output buffer
    (goto-char (point-max))
    (insert ?\n)
    ;; remember where to start displaying
    (setq teco-output-start (point))
    ;; clear minibuffer, in case we have to display in it
    (save-window-excursion
      (select-window (minibuffer-window))
      (erase-buffer))
    ;; if output is visible, position it correctly
    (let ((w (get-buffer-window teco-output-buffer)))
      (if w
	  (progn
	    (set-window-start w teco-output-start)
	    (set-window-point w teco-output-start))))))

(defun teco-output (s)
  (let ((w (get-buffer-window teco-output-buffer))
	(b (current-buffer))
	(sw (selected-window)))
    ;; Put the text in the output buffer
    (set-buffer teco-output-buffer)
    (goto-char (point-max))
    (insert s)
    (let ((p (point)))
      (set-buffer b)
      (if w
	  ;; if output is visible, move the window point to the end
	  (set-window-point w p)
	;; Otherwise, we have to figure out how to display the text
	;; Has a newline followed by another character been added to the
	;; output buffer?  If so, we have to make the output buffer visible.
	(if (save-excursion
	      (set-buffer teco-output-buffer)
	      (backward-char 1)
	      (search-backward "\n" teco-output-start t))
	    ;; a newline has been seen, clear the minibuffer and make the
	    ;; output buffer visible
	    (progn
	      (save-window-excursion
		(select-window (minibuffer-window))
		(erase-buffer))
	      (let ((pop-up-windows t))
		(pop-to-buffer teco-output-buffer)
		(goto-char p)
		(set-window-start w teco-output-start)
		(set-window-point w p)
		(select-window sw)))
	  ;; a newline has not been seen, add output to minibuffer
	  (save-window-excursion
	    (select-window (minibuffer-window))
	    (goto-char (point-max))
	    (insert s)))))))

;; Output a character of tracing information
(defun teco-trace-type (c)
  (teco-output (if (= c ?\e)
		?$
	      c)))

;; Report an error
(defun teco-error (code)
  (let ((text (cdr (assoc code teco-error-texts))))
    (teco-output (concat (if (save-excursion (set-buffer teco-output-buffer)
					     (/= (point) teco-output-start))
			     "\n"
			   "")
			 "? " code " " text))
    (beep)
    (if debug-on-error (debug nil code text))
    (throw 'teco-exit nil)))


;; Utility routines

;; copy characters from command string to buffer
(defun teco-moveuntil (string pointer terminate trace)
  (let ((count 0))
    (condition-case nil
	(while (/= (aref string pointer) terminate)
	  (and teco-trace (teco-trace-type (aref string pointer)))
	  (insert (aref string pointer))
	  (setq pointer (1+ pointer))
	  (setq count (1+ count)))
      (error (teco-error (if teco-macro-stack "UTM" "UTC"))))
    count))

;; Convert character to q-register name
;; If file-or-search is t, allow _, *, %, #
(defun teco-get-qspec (file-or-search char)
  ;; lower-case char
  (setq char (aref teco-mapch-l char))
  ;; test that it's valid
  (if (= (logand (aref teco-qspec-valid char) (if file-or-search 2 1)) 0)
      (teco-error "IQN"))
  char)

;; Set or get value of a variable
(defun teco-set-var (var)
  (if teco-exp-flag1
      (progn
	(if teco-exp-flag2
	    ;; if two arguments, they they are <clear bits>, <set bits>
	    (set var (logior (logand (symbol-value var) (lognot teco-exp-val2))
			     teco-exp-val1))
	  ;; if one argument, it is the new value
	  (set var teco-exp-val1))
	;; consume argument(s)
	(setq teco-exp-flag2 nil
	      teco-exp-flag1 nil))
    ;; if no arguments, fetch the value
    (setq teco-exp-val1 (symbol-value var)
	  teco-exp-flag1 t)))

;; Get numeric argument
(defun teco-get-value (default)
  (prog1
      (if teco-exp-flag1
	  teco-exp-val1
	(if (eq teco-exp-op 'sub)
	    (- default)
	  default))
    ;; consume argument
    (setq teco-exp-flag1 nil
	  teco-exp-op 'start)))

;; Get argument measuring in lines
(defun teco-lines (r)
  (- (save-excursion
       (if (> r 0)
	   (if (search-forward "\n" nil t r)
	       (point)
	     (point-max))
	 (if (search-backward "\n" nil t (- 1 r))
	     (1+ (point))
	   (point-min))))
     (point)))

;; routine to handle args for K, T, X, etc.
;; if two args, 'char x' to 'char y'
;; if just one arg, then n lines (default 1)
(defun teco-line-args (arg)
  (if teco-exp-flag2
      (cons teco-exp-val1 teco-exp-val2)
    (cons (point) (+ (point) (teco-lines (if teco-exp-flag1
					     teco-exp-val1
					   1))))))

;; routine to skip to next ", ', |, <, or >
;; skips over these chars embedded in text strings
;; stops in ! if argument is t
;; returns character found
(defun teco-skipto (&optional arg)
  (catch 'teco-skip
    (let (;; "at" prefix
	  (atsw nil)
	  ;; temp attributes
	  ta
	  ;; terminator
	  term
	  skipc)
      (while t				; forever
	(while (progn
		 (setq skipc (teco-get-command nil)
		       ta (aref teco-spec-chars skipc))
		 ;; if char is ^, treat next char as control
		 (if (eq skipc ?^)
		     (setq skipc (logand 31 (teco-get-command nil))
			   ta (aref teco-spec-chars skipc)))
		 (= (logand ta 51) 0))	; read until something interesting
					; found
	  nil)
	(if (/= (logand ta 32) 0)
	    (teco-get-command nil))	; if command takes a Q spec,
					; skip the spec
	(if (/= (logand ta 16) 0)	; sought char found: quit 
	    (progn
	      (if (= skipc ?\")		; quote must skip next char
		  (teco-get-command nil))
	      (throw 'teco-skip skipc)))
	(if (/= (logand ta 1) 0)	; other special char
	    (cond
	     ((eq skipc ?@)		; use alternative text terminator
	      (setq atsw t))
	     ((eq skipc ?\C-^)		; ^^ is value of next char
					; skip that char
	      (teco-get-command nil))
	     ((eq skipc ?\C-a)		; type text
	      (setq term (if atsw (teco-get-command nil) ?\C-a)
		    atsw nil)
	      (while (/= (teco-get-command nil) term)
		nil))			; skip text
	     ((eq skipc ?!)		; tag 
	      (if arg
		  (throw 'teco-skip skipc))
	      (while (/= (teco-get-command nil) ?!)
		nil))			; skip until next !
	     ((or (eq skipc ?e)
		  (eq skipc ?f))	; first char of two-letter E or F
					; command
	      nil)))			; not implemented
	(if (/= (logand ta 2) 0)	; command with a text
					; argument
	    (progn
	      (setq term (if atsw (teco-get-command nil) ?\e)
		    atsw nil)
	      (while (/= (teco-get-command nil) term)
		nil)			; skip text
	      ))))))


(defvar teco-command-keymap
  ;; This is what used to be (make-vector 128 'teco-command-self-insert)
  ;; Oh well
  (let ((map (make-keymap)) (n 127))
    (while (>= n 0)
      (define-key map (if (< n 32) (list 'control (+ n 32)) n)
	'teco-command-self-insert)
      (setq n (1- n)))
    map)
  "Keymap used while reading teco commands.")

(define-key teco-command-keymap "\^g" 'teco-command-ctrl-g)
(define-key teco-command-keymap "\^m" 'teco-command-return)
(define-key teco-command-keymap "\^u" 'teco-command-ctrl-u)
(define-key teco-command-keymap "\e" 'teco-command-escape)
(define-key teco-command-keymap "\^?" 'teco-command-delete)

(defvar teco-command-escapes nil
  "Records where ESCs are, since they are represented in the command buffer
by $.")

;;;###autoload
(defun teco-command ()
  "Read and execute a Teco command string."
  (interactive)
  (let* ((teco-command-escapes nil)
	 (command (catch 'teco-command-quit
		    (read-from-minibuffer teco-prompt nil
					  teco-command-keymap))))
    (if command
	(progn
	  (while teco-command-escapes
	    (aset command (car teco-command-escapes) ?\e)
	    (setq teco-command-escapes (cdr teco-command-escapes)))
	  (setq teco-output-buffer (get-buffer-create "*Teco Output*"))
	  (save-excursion
	    (set-buffer teco-output-buffer)
	    (goto-char (point-max))
	    (insert teco-prompt command))
	  (teco-execute-command command)))))

(defun teco-read-command ()
  "Read a teco command string from the user."
  (let ((command (catch 'teco-command-quit
		   (read-from-minibuffer teco-prompt nil
					 teco-command-keymap)))
	teco-command-escapes)
    (if command
	(while teco-command-escapes
	  (aset command (car teco-command-escapes) ?\e)
	  (setq teco-command-escapes (cdr teco-command-escapes))))
    command))

(defun teco-command-self-insert ()
  (interactive)
  (insert last-command-char)
  (if (not (pos-visible-in-window-p))
      (enlarge-window 1)))

(defun teco-command-ctrl-g ()
  (interactive)
  (beep)
  (throw 'teco-command-quit nil))

(defun teco-command-return ()
  (interactive)
  (setq last-command-char ?\n)
  (teco-command-self-insert))

(defun teco-command-escape ()
  (interactive)
  ;; Two ESCs in a row terminate the command string
  (if (eq last-command 'teco-command-escape)
      (throw 'teco-command-quit (buffer-string)))
  (setq teco-command-escapes (cons (1- (point)) teco-command-escapes))
  (setq last-command-char ?$)
  (teco-command-self-insert))

(defun teco-command-ctrl-u ()
  (interactive)
  ;; delete the characters
  (kill-line 0)
  ;; forget that they were ESCs
  (while (and teco-command-escapes (<= (point) (car teco-command-escapes)))
      (setq teco-command-escapes (cdr teco-command-escapes)))
  ;; decide whether to shrink the window
  (while (let ((a (insert ?\n))
	       (b (pos-visible-in-window-p))
	       (c (backward-delete-char 1)))
	   b)
    (shrink-window 1)))

(defun teco-command-delete ()
  (interactive)
  ;; delete the character
  (backward-delete-char 1)
  ;; forget that it was an ESC
  (if (and teco-command-escapes (= (point) (car teco-command-escapes)))
      (setq teco-command-escapes (cdr teco-command-escapes)))
  ;; decide whether to shrink the window
  (insert ?\n)
  (if (prog1 (pos-visible-in-window-p)
	(backward-delete-char 1))
      (shrink-window 1)))

(provide 'teco)

;;; teco.el ends here
