;;; paren.el --- highlight (un)matching parens and whole expressions

;; Copyright (C) 1993 Free Software Foundation, Inc.
;; Copyright (C) 1993, 1994, 1995 Tinker Systems
;;
;; Author: Jonathan Stigelman <Stig@hackvan.com>
;; Note:   (some code scammed from simple.el and blink-paren.el)
;; Maintainer: Jonathan Stigelman <Stig@hackvan.com>
;; Keywords: languages, faces

;;; This file is part of XEmacs.
;;; 
;;; XEmacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;; 
;;; XEmacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with XEmacs; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: Not synched with FSF.
;;; Way different from FSF.

;;; Commentary:

;; Purpose of this package:
;;
;;   This package highlights matching parens (or whole sexps) for easier
;;   editing of source code, particularly lisp source code.
;; 
;; The `paren-highlight' hook function runs after each command and
;; checks to see if the cursor is at a parenthesis.  If so, then it
;; highlights, in one of several ways, the matching parenthesis.
;; 
;; Priority is given to matching parentheses right before the cursor because
;; that's what makes sense when you're typing a lot of closed parentheses.
;; 
;; This is especially intuitive if you frequently use forward-sexp (M-C-f)
;; and backward-sexp (M-C-b) to maneuver around in source code.
;; 
;; Different faces are used for matching and mismatching parens so that it
;; is easier to see mistakes as you type them.  Audible feedback is optional.
;; 
;; If a (mis)matching paren is offscreen, then a message is sent to the modeline.
;; 
;; If paren-mode is `sexp', entire S-expressions are highlighted instead of
;; just matching parens.

;;; Code:

(defgroup paren-matching nil
  "Highlight (un)matching of parens and expressions."
  :prefix "paren-"
  :group 'matching)


;;;###autoload
(defcustom paren-mode nil
  "*Sets the style of parenthesis highlighting.
Valid values are nil, `blink-paren', `paren', and `sexp'.
  nil		no parenthesis highlighting.
  blink-paren	causes the matching paren to blink.
  paren		causes the matching paren to be highlighted but not to blink.
  sexp		whole expression enclosed by the local paren at its mate.
  sexp-surround whole surrounding expression, even if point is not at paren.
  nested	(not yet implemented) use variable shading to see the
		nesting of an expression.  Also groks regular expressions
		and shell quoting.

This variable is global by default, but you can make it buffer-local and
highlight parentheses differently in different major modes."
  :type '(radio (const :tag "None (default)" nil)
		(const :tag "Blinking Paren" blink-paren)
		(const :tag "Highlighted Paren" paren)
		(const :tag "Highlighted Expression" sexp)
		(const :tag "Highlighted Surrounding Expression" sexp-surround))
  :set (lambda (symbol value)
	 (paren-set-mode value))
  :initialize 'custom-initialize-default
  :require 'paren
  :group 'paren-matching)

(defcustom paren-message-offscreen t
  "*Non-nil means display a message if matching open paren is offscreen."
  :type 'boolean
  :group 'paren-matching)

(defcustom paren-ding-unmatched nil
  "*Make noise if the cursor is at an unmatched paren.

If t, then typing or passing over an unmatched paren will ring the bell
using the `paren' sound.  If nil, then the bell will not ring even if an
unmatched paren is typed.  If neither t nor nil, then the bell will not ring
when the cursor moves over unmatched parens but will ring if one is typed."
  :type '(choice (const :tag "off" nil)
		 (const :tag "on" t)
		 (const :tag "other" other))
  :group 'paren-matching)

(defcustom paren-backwards-message nil
  "*Control what context is reported when a matching paren is offscreen.

If nil, the matching paren and the following context is reported;
if non-nil, the matching paren and the previous context.

This variable should normally be set in a language major mode hook as
appropriate for the syntax of a program in that language."
  :type 'boolean
  :group 'paren-matching)
(make-variable-buffer-local 'paren-backwards-message)


(make-face 'paren-match)
(or (face-differs-from-default-p 'paren-match)
    (copy-face 'highlight 'paren-match))

(make-face 'paren-mismatch)
(cond ((face-differs-from-default-p 'paren-mismatch) nil)
      (t (let ((color-tag (list 'x 'color))
	       (mono-tag (list 'x 'mono))
	       (gray-tag (list 'x 'grayscale)))
	   (set-face-background 'paren-mismatch "DeepPink" 'global color-tag)
	   (set-face-reverse-p 'paren-mismatch t 'global 'tty)
	   (set-face-background 'paren-mismatch [modeline background] 'global
				mono-tag)
	   (set-face-foreground 'paren-mismatch [modeline foreground] 'global
				mono-tag)
	   (set-face-background 'paren-mismatch [modeline background] 'global
				gray-tag)
	   (set-face-foreground 'paren-mismatch [modeline foreground] 'global
				gray-tag))))

(make-face 'paren-blink-off)
(or (face-differs-from-default-p 'paren-blink-off)
    (set-face-foreground 'paren-blink-off (face-background 'default)))

;; this is either paren-match or paren-mismatch...
(defvar paren-blink-on-face nil)

(defcustom paren-blink-interval 0.2
  "*If the cursor is on a parenthesis, the matching parenthesis will blink.
This variable controls how long each phase of the blink lasts in seconds.
This should be a fractional part of a second (a float.)"
  :type 'number
  :group 'paren-matching)

(defcustom paren-max-blinks (* 5 60 5)	; 5 minutes is plenty...
  ;; idea from Eric Eide <eeide@jaguar.cs.utah.edu>
  "*Maximum number of times that a matching parenthesis will blink.
Set this to NIL if you want indefinite blinking."
  :type '(choice (const nil) number)
  :group 'paren-matching)

;; timeout to blink the face
(defvar paren-timeout-id nil)

;; Code:

(defvar paren-n-blinks)
(defvar paren-extent nil)

(defsubst pos-visible-in-window-safe (pos)
  "safe version of pos-visible-in-window-p"
  (condition-case nil
      ;; #### - is this needed in XEmacs???
      (pos-visible-in-window-p pos)
      (args-out-of-range nil)))

;; called before a new command is executed in the pre-command-hook
;; cleanup by removing the extent and the time-out
(defun paren-nuke-extent ()
  (condition-case c  ; don't ever signal an error in pre-command-hook!
      (let ((inhibit-quit t))
	(if paren-timeout-id
	    (disable-timeout (prog1 paren-timeout-id
			       (setq paren-timeout-id nil))))
	(if paren-extent
	    (delete-extent (prog1 paren-extent
			     (setq paren-extent nil)))))
    (error
     (message "paren-nuke-extent error! %s" c))))

;; callback for the timeout
;; swap the face of the extent on the matching paren
(defun paren-blink-timeout (arg)
  ;; The extent could have been deleted for some reason and not point to a
  ;; buffer anymore.  So catch any error to remove the timeout.
  (condition-case ()
      (if (and paren-max-blinks
	       (> (setq paren-n-blinks (1+ paren-n-blinks)) paren-max-blinks))
	  (paren-nuke-extent)
	(set-extent-face paren-extent 
			 (if (eq (extent-face paren-extent)
				 paren-blink-on-face)
			     'paren-blink-off
			   paren-blink-on-face)))
    (error (paren-nuke-extent))))

;; Show a match where the parenthesis is not on screen.
;;
;; In the forwards case (like lisp):
;;   Display from the matching paren through to the end of the line.  If only
;;   whitespace after the paren, then append the next line.
;;
;; Backwards case (like C):
;;   Display from the matching paren back to the start of its line.  If only
;;   whitespace before the paren, then prepend the previous line.

(defun paren-describe-match (pos mismatch)  
  (or (window-minibuffer-p (selected-window))
      (save-excursion
	(goto-char pos)
	(message "%s: %s"
		 (if mismatch "MISMATCH" "Matches")
                 (replace-in-string
                  (buffer-substring
                   (save-excursion
                     (if paren-backwards-message
                         (progn
                           (skip-syntax-backward " ")
                           (beginning-of-line (if (bolp) 0 1))
                           (skip-syntax-forward " "))
                       (forward-char 1)
                       (skip-syntax-forward " ")
                       (end-of-line (if (eolp) 2 1)))
                     (point)) 
                   (if paren-backwards-message (1+ (point)) (point)))
                  "[\n\t ]+" " " t)))))

(defun paren-maybe-ding ()
  (and (or (eq paren-ding-unmatched t)
	   (and paren-ding-unmatched
		(eq this-command 'self-insert-command)))
       (progn
	 (message "Unmatched parenthesis.")
	 (ding nil 'paren))))

;; Find the place to show, if there is one,
;; and show it until input arrives.
(defun paren-highlight ()
  "This highlights matching parentheses.

See the variables:
  paren-message-offscreen   use modeline when matching paren is offscreen?
  paren-ding-unmatched	    make noise when passing over mismatched parens?
  paren-mode		    'blink-paren, 'paren, 'sexp or 'sexp-surround
  blink-matching-paren-distance  maximum distance to search for parens.

and the following faces:
  paren-match, paren-mismatch, paren-blink-off"

  ;; I suppose I could check here to see if a keyboard macro is executing,
  ;; but I did a quick empirical check and couldn't tell that there was any
  ;; difference in performance

  (let ((oldpos (point))
	(pface nil)			; face for paren...nil kills the overlay
	(dir (and paren-mode
		  (not (input-pending-p))
		  (not executing-kbd-macro)
		  (cond ((eq (char-syntax (preceding-char)) ?\))
			 -1)
			((eq (char-syntax (following-char)) ?\()
			 1))))
	pos mismatch)

    (save-excursion
      (if (or (not dir)
	      (not (save-restriction
		     ;; Determine the range within which to look for a match.
		     (if blink-matching-paren-distance
			 (narrow-to-region
			  (max (point-min)
			       (- (point) blink-matching-paren-distance))
			  (min (point-max)
			       (+ (point) blink-matching-paren-distance))))

		     ;; Scan across one sexp within that range.
		     (condition-case nil
			 (setq pos (scan-sexps (point) dir))
		       ;; NOTE - if blink-matching-paren-distance is set,
		       ;; then we can have spurious unmatched parens.
		       (error (paren-maybe-ding)
			      nil)))))

	  ;; No paren at point, so possible highlight sexp around
	  (when (eq paren-mode 'sexp-surround)
	    (let ((pnt (save-excursion
			 (condition-case nil
			     (cons (progn (up-list -1) (point))
				   (or (scan-lists (point) 1 0)
				       (buffer-end 1)))
			   (error nil)))))
	      (when (and pnt (> (cdr pnt) (car pnt)))
		(setq paren-extent (make-extent (car pnt) (cdr pnt)))
		(set-extent-face paren-extent 'paren-match) 
		(set-extent-priority paren-extent 100))))
	    
	;; See if the "matching" paren is the right kind of paren
	;; to match the one we started at.
	(let ((beg (min pos oldpos)) (end (max pos oldpos)))
	  (setq mismatch
		(and (/= (char-syntax (char-after beg)) ?\\)
		     (/= (char-syntax (char-after beg)) ?\$)
		     ;; XEmacs change
		     (matching-paren (char-after beg))
		     (/= (char-after (1- end))
			 (matching-paren (char-after beg)))))
	  (if (or (eq paren-mode 'sexp) (eq paren-mode 'sexp-surround))
	      (setq paren-extent (make-extent beg end))))
	(and mismatch
	     (paren-maybe-ding))
 	(setq pface (if mismatch
			'paren-mismatch
		      'paren-match))
	(and (memq paren-mode '(blink-paren paren))
	     (setq paren-extent (make-extent (- pos dir) pos)))

	(if (and paren-message-offscreen
		 (eq dir -1)
                 (not (current-message))
		 (not (window-minibuffer-p (selected-window)))
		 (not (pos-visible-in-window-safe pos)))
            (paren-describe-match pos mismatch))
		 
	;; put the right face on the extent
	(cond (pface
	       (set-extent-face paren-extent pface) 
	       (set-extent-priority paren-extent 100) ; want this to be high
	       (and (eq paren-mode 'blink-paren)
		    (setq paren-blink-on-face pface
			  paren-n-blinks 0
			  paren-timeout-id
			  (and paren-blink-interval
			       (add-timeout paren-blink-interval
					    'paren-blink-timeout
					    nil
					    paren-blink-interval))))))
	))))


;;;###autoload
(defun paren-set-mode (arg &optional quiet)
  "Cycles through possible values for `paren-mode', force off with negative arg.
When called from lisp, a symbolic value for `paren-mode' can be passed directly.
See also `paren-mode' and `paren-highlight'."
  (interactive "P")
  ;; kill off the competition, er, uh, eliminate redundancy...
  (setq post-command-hook (delq 'show-paren-command-hook post-command-hook))
  (setq pre-command-hook (delq 'blink-paren-pre-command pre-command-hook))
  (setq post-command-hook (delq 'blink-paren-post-command post-command-hook))

  (let* ((paren-modes '(blink-paren paren sexp sexp-surround))
	 (paren-next-modes (cons nil (append paren-modes (list nil)))))
    (setq paren-mode (if (and (numberp arg) (< arg 0))
			 nil		; turn paren highlighting off
		       (cond ((and arg (symbolp arg)) arg)
			     ((and (numberp arg) (> arg 0))
			      (nth (1- arg) paren-modes))
			     ((numberp arg) nil)
			     (t (car (cdr (memq paren-mode
						paren-next-modes)))))
		       )))
  (cond (paren-mode
	 (add-hook 'post-command-hook 'paren-highlight)
	 (add-hook 'pre-command-hook 'paren-nuke-extent)
	 (setq blink-matching-paren nil))
	((not (local-variable-p 'paren-mode (current-buffer)))
	 (remove-hook 'post-command-hook 'paren-highlight)
	 (remove-hook 'pre-command-hook 'paren-nuke-extent)
	 (paren-nuke-extent)		; overkill
	 (setq blink-matching-paren t)
	 ))
  (or quiet (message "Paren mode is %s" (or paren-mode "OFF"))))

(eval-when-compile
  ;; suppress compiler warning.
  (defvar highlight-paren-expression))

;; No no no!
;(paren-set-mode (if (and (boundp 'highlight-paren-expression)
;			    ;; bletcherous blink-paren no-naming-convention
;			    highlight-paren-expression)
;		       'sexp
;		     (if (eq 'x (device-type (selected-device)))
;			 'blink-paren
;		       'paren))
;		t)

;;;###autoload
(make-obsolete 'blink-paren 'paren-set-mode)

;;;###autoload
(defun blink-paren (&optional arg)
  "Obsolete.  Use `paren-set-mode' instead."
  (interactive "P") 
  (paren-set-mode (if (and (numberp arg) (> arg 0))
		      'blink-paren -1) t))

(provide 'blink-paren)
(provide 'paren)

;; Local Variables:
;; byte-optimize: t
;; End:

;;; paren.el ends here
