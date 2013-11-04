;;; thing.el --- find language-specific contiguous pieces of text

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) International Computer Science Institute, 1991

;; Author: David Hughes <d.hughes@videonetworks.com>
;; Maintainer: XEmacs Development Team
;; Keywords: extensions, languages

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

;;; Synched up with: Not in FSF.

;;; Commentary:

;; #### FSF has thingatpt.el, which does the same thing.  Should merge
;; or toss this.

;; I wish.  The howls of pain every time I try are too overwhelming. -slb

;; Authors: David Hughes <djh@cis.prime.com>
;;              adapted from Martin Boyer's thing.el for imouse
;;          Martin Boyer, IREQ <mboyer@ireq-robot.hydro.qc.ca>
;;              adapted from Heinz Schmidt's thing.el for sky-mouse
;;          Heinz Schmidt, ICSI (hws@ICSI.Berkeley.EDU)
;;              adapted from Dan L. Pierson's epoch-thing.el
;;          Dan L. Pierson <pierson@encore.com>, 2/5/90
;;              adapted from Joshua Guttman's Thing.el
;;          Joshua Guttman, MITRE (guttman@mitre.org)
;;              adapted from sun-fns.el by Joshua Guttman, MITRE.
;;
;;

;;*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;* FUNCTION: Things are language objects contiguous pieces of text
;;*           whose boundaries can be defined by syntax or context.
;;*
;;* RELATED PACKAGES: various packages built on this.
;;*
;;* HISTORY:
;;* Last edited: David Hughes 21st December 1992
;;*  jul 21 21:00 1993 (tlp00): added a kludgy thing-filename
;;*  Feb 22 21:00 1993 (tlp00): better merge with lucid and imouse
;;*  Dec 21 11:11 1992 (djh): added thing-report-char-p
;;*  Nov 23 18:00 1992 (djh): merged in Guido Bosch's ideas
;;*  Sep 10 15:35 1992 (djh): adapted for Lucid emacs19-mouse.el
;;*  Nov 28 17:40 1991 (mb): Cleaned up, and added thing-bigger-alist.
;;*  May 24 00:33 1991 (hws): overworked and added syntax.
;;* Created: 2/5/90 Dan L. Pierson
;;*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;; Code:

(provide 'thing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  Customization and Entry Point  ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar thing-boundary-alist
  '((?w thing-word)
    (?_ thing-symbol)
    (?\( thing-sexp-start)
    (?\$ thing-sexp-start)
    (?' thing-sexp-start)
    (?\" thing-sexp-start)
    (?\) thing-sexp-end)
    (?  thing-whitespace)
    (?< thing-comment)
    (?. thing-next-sexp))
  "*List of pairs of the form (SYNTAX-CHAR FUNCTION) used by
the function `thing-boundaries'.")

(defvar thing-report-char-p t
  "*Non nil means return single char boundaries if all else fails")

(defvar thing-report-whitespace t
  "*Non nil means that whitespaces are considered as things, otherwise not.")

(defvar *last-thing*
  "The last thing found by thing-boundaries.  Used for chaining commands.")

;; The variable and function `thing-region' are to avoid the continual
;; construction of cons cells as result af the thing scanner functions.
;; This avoids unnecessary garbage collection. Guido Bosch <bosch@loria.fr>

(defvar thing-region (cons 'nil 'nil)
  "Cons cell that contains a region (<beginning> . <end>)
The function `thing-region' updates and returns it.")

(defun thing-region (beginning end)
  "Make BEGINNING the car and END the cdr of the cons cell in the
variable `thing-region'. Return the updated cons cell"
  (cond ((/= beginning end)
         (setcar thing-region beginning)
         (setcdr thing-region end)
         thing-region)))

(defvar thing-bigger-alist
  '((word-symbol thing-symbol)
    (symbol thing-sexp)
    (word-sexp thing-sexp)
    (sexp thing-up-sexp)
    (sexp-up thing-up-sexp)
    (line thing-paragraph)
    (paragraph thing-page)
    (char thing-word)
    (word-sentence thing-sentence)
    (sentence thing-paragraph))
  "List of pairs to go from one thing to a bigger thing.
See mouse-select-bigger-thing and mouse-delete-bigger-thing.")

(defvar thing-word-next nil
  "*The next bigger thing after a word.  A symbol.
Supported values are: word-symbol, word-sexp, and word-sentence.
Default value is word-sentence.
Automatically becomes local when set in any fashion.")
(make-variable-buffer-local 'thing-word-next)

(defun thing-boundaries (here)
  "Return start and end of text object at HERE using syntax table and
thing-boundary-alist.  Thing-boundary-alist is a list of pairs of the
form (SYNTAX-CHAR FUNCTION) where FUNCTION takes a single position
argument and returns a cons of places (start end) representing
boundaries of the thing at that position.

Typically:
 Left or right Paren syntax indicates an s-expression.
 The end of a line marks the line including a trailing newline.
 Word syntax indicates current word.
 Symbol syntax indicates symbol.
 If it doesn't recognize one of these it selects just the character HERE.

If an error occurs  during syntax scanning, the function just prints a
message and returns `nil'."
  (interactive "d")
  (setq *last-thing* nil)
  (if (save-excursion (goto-char here) (eolp))
      (thing-get-line here)
    (let* ((syntax (char-syntax (char-after here)))
           (pair (assq syntax thing-boundary-alist)))
      (cond ((and pair
		  (or thing-report-whitespace
		      (not (eq (car (cdr pair)) 'thing-whitespace))))
             (funcall (car (cdr pair)) here))
            (thing-report-char-p
             (setq *last-thing* 'char)
             (thing-region here (1+ here)))
            (t
             nil)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  Code Delimiters  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun thing-symbol (here)
  "Return start and end of symbol at HERE."
  (cond ((or (memq (char-syntax (char-after here)) '(?_ ?w))
             (and
              (memq (char-syntax (char-before here)) '(?_ ?w))
              ;; point is at the end of a symbol; look from inside the symbol
              (setq here (1- here))))
         (setq *last-thing* 'symbol)
         (let ((end (scan-sexps here 1)))
           (if end
	       (thing-region (min here (scan-sexps end -1)) end))))))

(defun thing-filename (here)
  "Return start and end of filename at HERE."
  (cond ((and (memq (char-syntax (char-after here)) '(?w ?_ ?.))
	      (< here (point-max)))
         (let (start end)
	   (save-excursion
	     (goto-char here)
	     (and (re-search-forward "\\s \\|:\\s\"\\|$" nil t)
		  (goto-char (setq end (match-beginning 0)))
		  (or
		   (and 
		    (re-search-backward "[^_a-zA-Z0-9---#$.~/@]+" nil t)
		    (setq start (+ (match-beginning 0)
				   (if (bolp)
				       0
				     1))))
		   (setq start (point-min)))
		  (thing-region (min start here) (max here end))))))))
;~/  
(defun thing-sexp-start (here)
  "Return start and end of sexp starting HERE."
  (setq *last-thing* 'sexp-start)
  (thing-region here (scan-sexps here 1)))

(defun thing-sexp-end (here)
  "Return start and end of sexp ending HERE."
  (setq *last-thing* 'sexp-end)
  (thing-region (scan-sexps (1+ here) -1) (1+ here)))

(defun thing-sexp (here)
  "Return start and end of the sexp at HERE."
  (setq *last-thing* 'sexp)
  (save-excursion
    (goto-char here)
    (thing-region (progn (backward-up-list 1) (point))
                  (progn (forward-list 1) (point)))))

(defun thing-up-sexp (here)
  "Return start and end of the sexp enclosing the selected area."
  (setq *last-thing* 'sexp-up)
  ;; Keep going up and backward in sexps.  This means that thing-up-sexp
  ;; can only be called after thing-sexp or after itself.
  (save-excursion
    (goto-char here)
    (thing-region (progn 
		    (condition-case ()
			(backward-up-list 1) (error nil))
		    (point))
                  (progn 
		    (condition-case () 
			(forward-list 1) (error nil))
		    (point)))))

;;; Allow punctuation marks not followed by white-space to include
;;; the subsequent sexp. Useful in foo.bar(x).baz and such.
(defun thing-next-sexp (here)
  "Return from HERE to the end of the sexp at HERE,
if the character at HERE is part of a sexp."
  (setq *last-thing* 'sexp-next)
  (if (= (char-syntax (char-after (1+ here))) ? )
      (thing-region here (1+ here))
    (thing-region here
                  (save-excursion (goto-char here) (forward-sexp) (point)))))

;;; Allow click to comment-char to extend to end of line
(defun thing-comment (here)
  "Return rest of line from HERE to newline."
  (setq *last-thing* 'comment)
  (save-excursion (goto-char here)
                  (while (= (char-syntax (preceding-char)) ?<)
                    (forward-char -1))
                  (thing-region (point) (progn (end-of-line) (point)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  Text Delimiters  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun thing-word (here)
  "Return start and end of word at HERE."
  (setq *last-thing* 
	(if thing-word-next
	    thing-word-next
	  (setq thing-word-next
		(cond 
		 ((memq major-mode '(emacs-lisp-mode c-mode c++-mode
			             fortran-mode latex-mode lisp-mode
				     perl-mode tex-mode))
		  'word-symbol)
		 (t 'word-sentence)))))
  (save-excursion
    (goto-char here)
    (forward-word 1)
    (let ((end (point)))
      (forward-word -1)
      (thing-region (point) end))))

(defun thing-sentence (here)
  "Return start and end of the sentence at HERE."
  (setq *last-thing* 'sentence)
  (save-excursion
    (goto-char here)
    (thing-region (progn (backward-sentence) (point))
                  (progn (forward-sentence) (point)))))

(defun thing-whitespace (here)
  "Return start to end of all of whitespace HERE."
  (setq *last-thing* 'whitespace)
  (save-excursion
    (goto-char here)
    (let ((start (progn (skip-chars-backward " \t") (1+ (point))))
          (end (progn (skip-chars-forward " \t") (point))))
      (if (= start end)
          (thing-region (1- start) end)
        (thing-region start end)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;  Physical Delimiters  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun thing-get-line (here)
  "Return whole of line HERE is in, with newline unless at eob."
  (setq *last-thing* 'line)
  (save-excursion
    (goto-char here)
    (let* ((start (progn (beginning-of-line 1) (point))))
      (thing-region start (point)))))

(defun thing-paragraph (here)
  "Return start and end of the paragraph at HERE."
  (setq *last-thing* 'paragraph)
  (save-excursion
    (goto-char here)
    (thing-region (progn (backward-paragraph) (point))
                  (progn (forward-paragraph) (point)))))

(defun thing-page (here)
  "Return start and end of the page at HERE."
  (setq *last-thing* 'page)
  (save-excursion
    (goto-char here)
    (thing-region (progn (backward-page) (point))
                  (progn (forward-page) (point)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;  Support functions  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun kill-thing-at-point (here)
  "Kill text object using syntax table.
See thing-boundaries for definition of text objects"
  (interactive "d")
  (let ((bounds (thing-boundaries here)))
    (kill-region (car bounds) (cdr bounds))))

(defun copy-thing-at-point (here)
  "Copy text object using syntax table.
See thing-boundaries for definition of text objects"
  (interactive "d")
  (let ((bounds (thing-boundaries here)))
    (copy-region-as-kill (car bounds) (cdr bounds))))

;;; thing.el ends here
