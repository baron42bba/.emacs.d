;;; blink-paren.el --- blink the matching paren, just like Zmacs
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

;; Author: devin@lucid.com.
;; Keywords: faces

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

(defvar blink-paren-timeout 0.2
  "*If the cursor is on a parenthesis, the matching parenthesis will blink.
This variable controls how long each phase of the blink lasts in seconds.
This should be a fractional part of a second (a float.)")

(defvar highlight-paren-expression nil
  "*If true, highlight the whole expression of the paren under the cursor
instead of blinking (or highlighting) the matching paren.  This will highlight
the expression using the `highlight-expression' face.")

;;; The blinking paren alternates between the faces blink-paren-on and
;;; blink-paren-off.  The default is for -on to look just like default
;;; text, and -off to be invisible.  You can change this so that, for
;;; example, the blinking paren fluctuates between bold and italic...
;;;
;;; You can make the matching paren merely be highlighted (and not blink)
;;; by setting the blink-paren-on and blink-paren-off faces to have the same
;;; attributes; if you do this, then emacs will not consume as much CPU.
;;;
;;; If highlight-paren-expression is true, then the whole sexp between the
;;; parens will be displayed in the `highlight-expression' face instead.

(make-face 'blink-paren-on)
(make-face 'blink-paren-off)
(make-face 'highlight-expression)

;; extent used to change the face of the matching paren
(defvar blink-paren-extent nil)

;; timeout to blink the face
(defvar blink-paren-timeout-id nil)

;; find if we should look forward or backward to find the matching paren
(defun blink-paren-sexp-dir ()
  (cond ((and (< (point) (point-max))
	      (eq (char-syntax (char-after (point))) ?\())
	 1)
	((and (> (point) (point-min))
	      (eq (char-syntax (char-after (- (point) 1))) ?\)))
	 -1)
	(t ())))

;; make an extent on the matching paren if any.  return it.
(defun blink-paren-make-extent ()
  (let ((dir (blink-paren-sexp-dir)))
    (and dir
	 (condition-case ()
	     (let* ((parse-sexp-ignore-comments t)
		    (other-pos (let ((pmin (point-min))
				     (pmax (point-max))
				     (point (point)))
				 (unwind-protect
				     (progn
				       (narrow-to-region
					(max pmin (- point blink-matching-paren-distance))
					(min pmax (+ point blink-matching-paren-distance)))
				       (forward-sexp dir) (point))
				   (narrow-to-region pmin pmax)
				   (goto-char point))))
		    (extent (if (= dir 1)
				(make-extent (if highlight-paren-expression
						 (point)
					       (- other-pos 1))
					     other-pos)
			      (make-extent other-pos
					   (if highlight-paren-expression
					       (point)
					     (+ other-pos 1))))))
	       (set-extent-face extent (if highlight-paren-expression
					   'highlight-expression
					 'blink-paren-on))
	       extent)
	   (error nil)))))

;; callback for the timeout
;; swap the face of the extent on the matching paren
(defun blink-paren-timeout (arg)
  ;; The extent could have been deleted for some reason and not point to a
  ;; buffer anymore.  So catch any error to remove the timeout.
  (condition-case ()
      (set-extent-face blink-paren-extent 
		       (if (eq (extent-face blink-paren-extent)
			       'blink-paren-on)
			   'blink-paren-off
			 'blink-paren-on))
    (error (blink-paren-pre-command))))

;; called after each command is executed in the post-command-hook
;; add the extent and the time-out if we are on a paren.
(defun blink-paren-post-command ()
  (blink-paren-pre-command)
  (if (and (setq blink-paren-extent (blink-paren-make-extent))
	   (not highlight-paren-expression)
	   (not (and (face-equal 'blink-paren-on 'blink-paren-off)
		     (progn
		       (set-extent-face blink-paren-extent 'blink-paren-on)
		       t)))
	   (or (floatp blink-paren-timeout)
	       (integerp blink-paren-timeout)))
      (setq blink-paren-timeout-id
	    (add-timeout blink-paren-timeout 'blink-paren-timeout ()
			 blink-paren-timeout))))

;; called before a new command is executed in the pre-command-hook
;; cleanup by removing the extent and the time-out
(defun blink-paren-pre-command ()
  (condition-case c  ; don't ever signal an error in pre-command-hook!
      (let ((inhibit-quit t))
	(if blink-paren-timeout-id
	    (disable-timeout (prog1 blink-paren-timeout-id
			       (setq blink-paren-timeout-id nil))))
	(if blink-paren-extent
	    (delete-extent (prog1 blink-paren-extent
			     (setq blink-paren-extent nil)))))
    (error
     (message "blink paren error! %s" c))))


(defun blink-paren (&optional arg)
  "Toggles paren blinking on and off.
With a positive argument, turns it on.
With a non-positive argument, turns it off."
  (interactive "P")
  (let* ((was-on (not (not (memq 'blink-paren-pre-command pre-command-hook))))
	 (on-p (if (null arg)
		   (not was-on)
		(> (prefix-numeric-value arg) 0))))
    (cond (on-p

	   ;; in case blink paren was dumped, this needs to be setup
	   (or (face-differs-from-default-p 'blink-paren-off)
	       (progn
		 (set-face-background 'blink-paren-off (face-background 'default))
		 (set-face-foreground 'blink-paren-off (face-background 'default))))

	   (or (face-differs-from-default-p 'highlight-expression)
	       (set-face-underline-p 'highlight-expression t))
	   
	   (add-hook 'pre-command-hook 'blink-paren-pre-command)
	   (add-hook 'post-command-hook 'blink-paren-post-command)
	   (setq blink-matching-paren nil))
	  (t
	   (remove-hook 'pre-command-hook 'blink-paren-pre-command)
	   (remove-hook 'post-command-hook 'blink-paren-post-command)
	   (and blink-paren-extent (detach-extent blink-paren-extent))
	   (setq blink-matching-paren t)))
    on-p))

(defun blink-paren-init ()
  "obsolete - use `blink-paren' instead."
  (interactive)
  (blink-paren 1))

(provide 'blink-paren)

(blink-paren 1)
