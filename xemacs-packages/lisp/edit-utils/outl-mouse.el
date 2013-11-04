;;; outl-mouse.el --- outline mode mouse commands for Emacs

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright 1994 (C) Andy Piper <ajp@eng.cam.ac.uk>

;; Author: Andy Piper <ajp@eng.cam.ac.uk>
;; Maintainer: XEmacs Development Team
;; Keywords: outlines, mouse

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

;;; Synched up with: Not in FSF

;;; Commentary:

;; outl-mouse.el v1.3.8:

;; Defines button one to hide  blocks when clicked on outline-up-arrow
;; and expand blocks when clicked on outline-down-arrow.  Features are
;; activated   when   outline-minor-mode  or   outline-mode are turned
;; on. There is also a menu for each glyph on button 3. 

;; To use put:
;; 	(require 'outl-mouse)
;; in your .emacs file.

;; If you use func-menu all  the time and  want outl-mouse on all  the
;; time as well then put:
;;	(setq outline-sync-with-func-menu t)
;; outlining will then be turned on when func-menu is. Note that this
;; requires a patch to func-menu 2.16 (in 19.10) to work:

;RCS file: func-menu.el,v
;retrieving revision 1.1
;diff -r1.1 func-menu.el
;180a181,183
;> (defvar fume-found-function-hook nil
;>   "*Hook to call after every function match.")
;> 
;1137,1138c1140,1142
;<         (if (listp funcname)
;<             (setq funclist (cons funcname funclist)))
;---
;>         (cond ((listp funcname)
;>              (setq funclist (cons funcname funclist))
;>              (save-excursion (run-hooks 'fume-found-function-hook))))

;; If you  want mac-style outlining  then set  outline-mac-style to t.
;; If you   want    the  outline   arrows on    the    left then   set
;; outline-glyphs-on-left  to t. If you  have xpm then arrows are much
;; better defined.

;; This package uses func-menu to  define outline regexps if they  are
;; not already defined. You should no longer need to use out-xtra.

;; You can define the package to  do something other than outlining by
;; setting outline-fold-in-function and outline-fold-out-function.

;; You can define the color of outline arrows, but only in your .emacs.

;; Only works in XEmacs 19.10 and onwards. 

;;; Code:

;; User definable variables.

(defgroup outl-mouse nil
  "Outline mouse mode commands for Emacs"
  :prefix "outline-"
  :group 'outlines
  :group 'mouse)


(defcustom outline-mac-style nil
  "*If t then outline glyphs will be right and down arrows."
  :type 'boolean
  :group 'outl-mouse)

(defcustom outline-glyphs-on-left nil
  "*The position of outline glyphs on a line."
  :type 'boolean
  :group 'outl-mouse)

(defcustom outline-glyph-colour "Gray75"
  "*The colour of outlining arrows."
  :type 'color
  :group 'outl-mouse)

(defcustom outline-glyph-shade-colour "Gray40"
  "*The shadow colour of outlining arrows."
  :type 'color
  :group 'outl-mouse)

(defcustom outline-glyph-lit-colour "Gray90"
  "*The lit colour of outlining arrows."
  :type 'color
  :group 'outl-mouse)

(defvar outline-fold-in-function 'outline-fold-in
  "Function to call for folding in. 
The function should take an annotation argument.")
(make-variable-buffer-local 'outline-fold-in-function)

(defvar outline-fold-out-function 'outline-fold-out
  "Function to call for folding out. 
The function should take an annotation argument.")
(make-variable-buffer-local 'outline-fold-out-function)

(defcustom outline-sync-with-func-menu nil
  "*If t then outline glyphs are permanently added by func-menu scans.
If outline-minor-mode is turned off then turning it back on will have
no effect. Instead the buffer should be rescanned from the function
menu."
  :type 'boolean
  :group 'outl-mouse)

(defcustom outline-move-point-after-click t
  "*If t then point is moved to the current heading when clicked."
  :type 'boolean
  :group 'outl-mouse)

(defcustom outline-scanning-message "Adding glyphs... (%3d%%)"
  "*Progress message during the scanning of the buffer.
Set this to nil to inhibit progress messages."
  :type 'string
  :group 'outl-mouse)

;;
;; No user definable variables beyond this point.
;;

;; I'll bet there's a neat way to do this with specifiers -- a pity the
;; sucks so badly on it. -sb
(defconst outline-up-arrow ; XEmacs
  (make-glyph ; an up-arrow
   (cond ((featurep 'xpm) (vector 'xpm :data (concat "/* XPM */
static char * arrow[] = {
\"10 10 5 1\",
\" 	c none\",
\".	c " outline-glyph-lit-colour "\",
\"X	c " outline-glyph-shade-colour "\",
\"o	c " outline-glyph-colour "\",
\"O	c " outline-glyph-shade-colour "\",
\"    .X    \",
\"    .X    \",
\"   ..XX   \",
\"   ..XX   \",
\"  ..ooXX  \",
\"  ..ooXX  \",
\" ..ooooXX \",
\" ..ooooXX \",
\"..OOOOOOXX\",
\"OOOOOOOOOO\"};")))
	 ((featurep 'x)
	  (vector 'xbm
		  :data
		  (list 10 10
			(concat "\000\000\000\000\060\000\060\000\150\000"
				"\150\000\324\000\324\000\376\001\376\001"))))
	 (t "^")))
  "Bitmap object for outline up glyph.")

(defconst outline-up-arrow-mask ; XEmacs
  (make-glyph ; an up-arrow
   (cond ((featurep 'xpm) (vector 'xpm :data (concat "/* XPM */
static char * arrow[] = {
\"10 10 5 1\",
\" 	c none\",
\".	c " outline-glyph-shade-colour "\",
\"X	c " outline-glyph-lit-colour "\",
\"o	c " outline-glyph-colour "\",
\"O	c " outline-glyph-lit-colour "\",
\"    .X    \",
\"    .X    \",
\"   ..XX   \",
\"   ..XX   \",
\"  ..ooXX  \",
\"  ..ooXX  \",
\" ..ooooXX \",
\" ..ooooXX \",
\"..OOOOOOXX\",
\"OOOOOOOOOO\"};")))
	 ((featurep 'x)
	  (vector 'xbm
		  :data 
		  (list 10 10
			(concat "\000\000\000\000\060\000\060\000\130\000"
				"\130\000\254\000\274\000\006\001\376\001"))))
	 (t "+")))
  "Bitmap object for outline depressed up glyph.")

(defconst outline-down-arrow ; XEmacs
  (make-glyph	; a down-arrow
   (cond ((featurep 'xpm) (vector 'xpm :data (concat "/* XPM */
static char * down[] = {
\"10 10 5 1\",
\" 	c " outline-glyph-lit-colour "\",
\".	c " outline-glyph-lit-colour "\",
\"X	c " outline-glyph-shade-colour "\",
\"o	c none\",
\"O	c " outline-glyph-colour "\",
\"          \",
\"..      XX\",
\"o..OOOOXXo\",
\"o..OOOOXXo\",
\"oo..OOXXoo\",
\"oo..OOXXoo\",
\"ooo..XXooo\",
\"ooo..XXooo\",
\"oooo.Xoooo\",
\"oooo.Xoooo\"};")))
	 ((featurep 'x)
	  (vector 'xbm
		  :data 
		  (list 10 10
			(concat "\000\000\000\000\376\001\202\001\364\000"
				"\324\000\150\000\150\000\060\000\060\000"))))
	 (t "v")))
  "Bitmap object for outline down glyph.")

(defconst outline-down-arrow-mask ; XEmacs
  (make-glyph	; a down-arrow
   (cond ((featurep 'xpm) (vector 'xpm :data (concat "/* XPM */
static char * down[] = {
\"10 10 5 1\",
\" 	c " outline-glyph-shade-colour "\",
\".	c " outline-glyph-shade-colour "\",
\"X	c " outline-glyph-lit-colour "\",
\"o	c none\",
\"O	c " outline-glyph-colour "\",
\"          \",
\"..      XX\",
\"o..OOOOXXo\",
\"o..OOOOXXo\",
\"oo..OOXXoo\",
\"oo..OOXXoo\",
\"ooo..XXooo\",
\"ooo..XXooo\",
\"oooo.Xoooo\",
\"oooo.Xoooo\"};")))
	 ((featurep 'x)
	  (vector 'xbm
		  :data
		  (list 10 10
			(concat "\000\000\000\000\376\001\376\001\254\000"
				"\254\000\130\000\130\000\060\000\060\000"))))
	 (t "+")))
  "Bitmap object for outline depressed down glyph.")

(defconst outline-right-arrow
  (make-glyph	; a right-arrow
   (cond ((featurep 'xpm) (vector 'xpm :data (concat "/* XPM */
static char * right[] = {
\"10 10 5 1\",
\" 	c " outline-glyph-lit-colour "\",
\".	c " outline-glyph-lit-colour "\",
\"X	c none\",
\"o	c " outline-glyph-colour "\",
\"O	c " outline-glyph-shade-colour "\",
\" .XXXXXXXX\",
\" ...XXXXXX\",
\"  ....XXXX\",
\"  oo....XX\",
\"  oooo....\",
\"  ooooOOOO\",
\"  ooOOOOXX\",
\"  OOOOXXXX\",
\" OOOXXXXXX\",
\" OXXXXXXXX\"};")))
	 ((featurep 'x)
	  (vector 'xbm
		  :data
		  (list 10 10
			(concat "\000\000\006\000\032\000\142\000\232\001"
				"\352\001\172\000\036\000\006\000\000\000"))))
	 (t ">")))
  "Bitmap object for outline right glyph.")

(defconst outline-right-arrow-mask
  (make-glyph	; a right-arrow
   (cond ((featurep 'xpm) (vector 'xpm :data (concat "/* XPM */
static char * right[] = {
\"10 10 5 1\",
\" 	c " outline-glyph-shade-colour "\",
\".	c " outline-glyph-shade-colour "\",
\"X	c none\",
\"o	c " outline-glyph-colour "\",
\"O	c " outline-glyph-lit-colour "\",
\" .XXXXXXXX\",
\" ...XXXXXX\",
\"  ....XXXX\",
\"  oo....XX\",
\"  oooo....\",
\"  ooooOOOO\",
\"  ooOOOOXX\",
\"  OOOOXXXX\",
\" OOOXXXXXX\",
\" OXXXXXXXX\"};")))
	 ((featurep 'x)
	  (vector 'xbm
		  :data
		  (list 10 10
			(concat "\000\000\006\000\036\000\176\000\346\001"
				"\236\001\146\000\036\000\006\000\000\000"))))
	 (t "+")))
  "Bitmap object for outline depressed right glyph.")

(defvar outline-glyph-menu
  '("Outline Commands"
    ["Hide all"		hide-body			t]
    ["Hide all subtrees" hide-subtrees-same-level	t]
    ["Hide subtree"	hide-subtree                    t]
;   ["Hide body"        hide-body                       t]
    "---"
    ["Show all"		show-all			t]
    ["Show subtree"	show-subtree                    t]
    ["Show body"        show-entry                      t]
    "---"
    ["Update buffer"	outline-add-glyphs		t]
    ["Rescan buffer"	outline-rescan-buffer		t])
  "Menu of commands for outline glyphs.")

(set-glyph-contrib-p outline-down-arrow nil)
(set-glyph-contrib-p outline-up-arrow nil)
(set-glyph-contrib-p outline-down-arrow-mask nil)
(set-glyph-contrib-p outline-up-arrow-mask nil)
(set-glyph-contrib-p outline-right-arrow nil)
(set-glyph-contrib-p outline-right-arrow-mask nil)

(require 'annotations)
(require 'advice)			; help me doctor !
(require 'outline)
(require 'func-menu)			; for those most excellent regexps.

(add-hook 'outline-mode-hook 'outline-mouse-hooks)
(add-hook 'outline-minor-mode-hook 'outline-mouse-hooks)
;; I thought this was done already ...
(make-variable-buffer-local 'outline-regexp)
(make-variable-buffer-local 'outline-level)

(cond (outline-sync-with-func-menu
       (add-hook 'fume-found-function-hook 'outline-heading-add-glyph-1)
       (setq-default fume-rescan-buffer-hook '(lambda () 
						(outline-minor-mode 1)))))

(defadvice fume-set-defaults (after fume-set-defaults-ad activate)
  "Advise fume-set-defaults to setup outline regexps."
  (if (and (not (assq 'outline-regexp (buffer-local-variables)))
	   fume-function-name-regexp)
      (progn
	(setq outline-regexp (if (listp fume-function-name-regexp)
				 (car fume-function-name-regexp)
			       fume-function-name-regexp))
	(setq outline-level '(lambda () 1)))))

(defadvice outline-minor-mode (after outline-mode-mouse activate)
  "Advise outline-minor-mode to delete glyphs when switched off."
  (if (not outline-minor-mode)
      (progn 
	(outline-delete-glyphs)
	(show-all))))

;; advise all outline commands so that glyphs are synced after use
(defadvice show-all (after show-all-ad activate)
  "Advise show-all to sync headings."
  (outline-sync-visible-sub-headings-in-region (point-min) (point-max)))

(defadvice hide-subtree (after hide-subtree-ad activate)
  "Advise hide-subtree to sync headings."
  (outline-sync-visible-sub-headings))

(defadvice hide-entry (after hide-entry-ad activate)
  "Advise hide-entry to sync headings."
  (outline-sync-visible-sub-headings))

(defadvice hide-body (after hide-body-ad activate)
  "Advise hide-body to sync headings."
  (outline-sync-visible-sub-headings-in-region (point-min) (point-max)))

(defadvice show-subtree (after show-subtree-ad activate)
  "Advise show-subtree to sync headings."
  (outline-sync-visible-sub-headings))

(defadvice show-entry (after show-entry-ad activate)
  "Advise shown-entry to sync headings."
  (outline-sync-visible-sub-headings))

;;;###autoload
(defun outl-mouse-mode ()
  "Calls outline-mode, with outl-mouse extensions"
  (interactive)
  (outline-mode))
    
;;;###autoload
(defun outl-mouse-minor-mode (&optional arg)
  "Toggles outline-minor-mode, with outl-mouse extensions"
  (interactive "P")
  (outline-minor-mode arg))

(defun hide-subtrees-same-level ()
  "Hide all subtrees below the current level."
  (interactive)
  (save-excursion
    (while (progn
	     (hide-subtree)
      	     (condition-case nil
		 (progn
		   (outline-forward-same-level 1)
		   t)
	       (error nil))))))

(defun outline-mouse-hooks ()
  "Hook for installing outlining with the mouse."
  ;; use function menu regexps if not set
  (fume-set-defaults)
  ;; only add glyphs when we're not synced.
  (if (not outline-sync-with-func-menu) (outline-add-glyphs))
  ;; add C-a to local keymap
  (let ((outline (cond ((keymapp (lookup-key (current-local-map)
					     outline-minor-mode-prefix))
			(lookup-key (current-local-map)
				    outline-minor-mode-prefix))
		       (t
			(define-key (current-local-map)
			  outline-minor-mode-prefix (make-sparse-keymap))
			(lookup-key (current-local-map) 
				    outline-minor-mode-prefix)))))
    (define-key outline "\C-a" 'outline-heading-add-glyph)
    (define-key outline-mode-map "\C-c\C-a" 'outline-heading-add-glyph)))

(defun outline-add-glyphs ()
  "Add annotations and glyphs to all heading lines that don't have them."
  (interactive)
  (save-excursion
    (and outline-scanning-message (display-message
				   'progress
				   (format outline-scanning-message 0)))
    (goto-char (point-min))
    (if (not (outline-on-heading-p)) (outline-next-visible-heading-safe))
    (while 
	(progn
	  (outline-heading-add-glyph-1)
	  (and outline-scanning-message 
	       (display-message
		'progress
		(format outline-scanning-message (fume-relative-position))))
	  (outline-next-visible-heading-safe)))
    (and outline-scanning-message 
	 (display-message
	  'progress
	  (format "%s done" (format outline-scanning-message 100))))))

(defun outline-delete-glyphs ()
  "Remove annotations and glyphs from heading lines."
  (save-excursion
    (mapcar 'outline-heading-delete-glyph (annotation-list))))

(defun outline-rescan-buffer ()
  "Remove and insert all annotations."
  (interactive)
  (outline-delete-glyphs)
  (outline-add-glyphs)
  (save-excursion
    (outline-sync-visible-sub-headings-in-region (point-min) (point-max))))

(defun outline-heading-delete-glyph (ext)
  "Delete annotation and glyph from a heading with annotation EXT."
  (if (and 
       (progn
	 (goto-char (extent-start-position ext))
	 (beginning-of-line)
	 (outline-on-heading-p))
       (extent-property ext 'outline))
      (delete-annotation ext))
  nil)

(defun outline-heading-add-glyph ()
  "Interactive version of outline-heading-add-glyph-1."
  (interactive)
  (save-excursion
    (outline-heading-add-glyph-1)))

(defun outline-heading-add-glyph-1 ()
  "Add glyph to the end of heading line which point is on.
 Returns nil if point is not on a heading or glyph already exists."
  (if (or (not (outline-on-heading-p))
	  (outline-heading-has-glyph-p)
	  (save-excursion (forward-line) (outline-on-heading-p)))
      nil
    (outline-back-to-heading)
    (let ((anot2 
	   (make-annotation (if outline-mac-style 
				outline-right-arrow
			      outline-down-arrow)
			    (save-excursion (if outline-glyphs-on-left nil
					      (outline-end-of-heading))
					    (point))
			    'text nil t 
			    (if outline-mac-style
				outline-right-arrow-mask
			      outline-down-arrow-mask)))
	  (anot1 
	   (make-annotation (if outline-mac-style
				outline-down-arrow
			      outline-up-arrow)
			    (save-excursion (if outline-glyphs-on-left nil
					      (outline-end-of-heading))
					    (point))
			    'text nil t 
			    (if outline-mac-style
				outline-down-arrow-mask
			      outline-up-arrow-mask))))
      ;; we cunningly make the annotation data point to its twin.
      (set-annotation-data anot1 anot2)
      (set-extent-property anot1 'outline 'up)
      (set-annotation-action anot1 'outline-up-click)
      (set-annotation-menu anot1 outline-glyph-menu)
      (set-extent-priority anot1 1)
      (set-annotation-data anot2 anot1)
      (set-extent-property anot2 'outline 'down)
      (set-annotation-menu anot2 outline-glyph-menu)
      (set-annotation-action anot2 'outline-down-click)
      (annotation-hide anot2))
    t))

(defun outline-heading-has-glyph-p ()
  "Return t if heading has an outline glyph."
  (catch 'found
    (mapcar
     '(lambda(a)
	(if (extent-property a 'outline)
	    (throw 'found t)))
     (annotations-in-region (save-excursion (outline-back-to-heading) (point))
			    (save-excursion (outline-end-of-heading) 
					    (+ 1 (point)))
			    (current-buffer)))
    nil))

(defun outline-sync-visible-sub-headings-in-region (pmin pmax)
  "Make sure all annotations on headings in region PMIN PMAX are 
displayed correctly."
  (mapcar '(lambda (x) 
	     (goto-char (extent-start-position x))
	     (beginning-of-line)
	     (cond ((and (eq (extent-property x 'outline) 'down)
			 ;; skip things we can't see
			 (not (eq (preceding-char) ?\^M)))
		    (if (outline-more-to-hide)
			;; reveal my twin
			(annotation-reveal (annotation-data x))
		      (annotation-hide (annotation-data x)))
		    (if (not (outline-hidden-p))
			;; hide my self
			(annotation-hide x)
		      (annotation-reveal x)))))
	  (annotations-in-region pmin pmax (current-buffer))))

(defun outline-sync-visible-sub-headings ()
  "Make sure all annotations on sub-headings below the one point is on are 
displayed correctly."
  (outline-sync-visible-sub-headings-in-region 
   (point) 
   (progn (outline-end-of-subtree) (point))))

(defun outline-fold-out (annotation)
  "Fold out the current heading."
  (beginning-of-line)
;  (if (not (equal (condition-case nil
;		      (save-excursion (outline-next-visible-heading 1)
;				      (point))
;		    (error nil))
;		  (save-excursion (outline-next-heading) 
;				  (if (eobp) nil (point)))))
  (if (save-excursion (outline-next-heading) 
		      (eq (preceding-char) ?\^M))
      (progn 
	(save-excursion (show-children))
	(outline-sync-visible-sub-headings))
    ;; mess with single entry
    (if (outline-hidden-p) 
	(progn 
	  (save-excursion (show-entry))
	  ;; reveal my twin and hide me
	  (annotation-hide annotation)
	  (annotation-reveal (annotation-data annotation))))))

(defun outline-fold-in (annotation)
  "Fold in the current heading."
  (beginning-of-line)
  ;; mess with single entries
  (if (not (outline-hidden-p))
      (progn
	(save-excursion (hide-entry))
	(if (not (outline-more-to-hide))
	    (annotation-hide annotation))
	(annotation-reveal (annotation-data annotation)))
    ;; otherwise look for more leaves
    (save-excursion 
      (if (outline-more-to-hide t)
	  (hide-subtree)
	(hide-leaves)))
    ;; sync everything
    (outline-sync-visible-sub-headings)))

(defun outline-more-to-hide (&optional arg)
  "Return t if there are more visible sub-headings or text.
With ARG return t only if visible sub-headings have no visible text."
  (if (not (outline-hidden-p))
      (if arg nil t)
    (save-excursion
      (and (< (funcall outline-level) (condition-case nil
					  (progn 
					    (outline-next-visible-heading 1)
					    (funcall outline-level))
					(error 0)))
	   (if (and (not (outline-hidden-p)) arg)
	       nil t)))))

(defun outline-hidden-p ()
  "Return t if point is on the header of a hidden subtree."
  (save-excursion
    (let ((end-of-entry (save-excursion (outline-next-heading))))
      ;; Make sure that the end of the entry really exists.
      (if (not end-of-entry)
	  (setq end-of-entry (point-max)))
      (outline-back-to-heading)
      ;; If there are ANY ^M's, the entry is hidden.
      (search-forward "\^M" end-of-entry t))))

(defun outline-next-visible-heading-safe ()
  "Safely go to the next visible heading. 
nil is returned if there is none."
  (condition-case nil
      (progn
	(outline-next-visible-heading 1)
	t)
    (error nil)))

(defun outline-up-click (data ev)
  "Annotation action for clicking on an up arrow.
DATA is the annotation data. EV is the mouse click event."
  (save-excursion
    (goto-char (extent-end-position (event-glyph-extent ev)))
    (funcall outline-fold-in-function (event-glyph-extent ev)))
  (if outline-move-point-after-click
      (progn
	(goto-char (extent-end-position (event-glyph-extent ev)))
	(beginning-of-line))))
; This line demonstrates a bug in redisplay
(defun outline-down-click (data ev)
  "Annotation action for clicking on a down arrow.
DATA is the annotation data. EV is the mouse click event."
  (save-excursion
    (goto-char (extent-end-position (event-glyph-extent ev)))
    (funcall outline-fold-out-function (event-glyph-extent ev)))
  (if outline-move-point-after-click
      (progn
	(goto-char (extent-end-position (event-glyph-extent ev)))
	(beginning-of-line))))


(provide 'outl-mouse)
(provide 'outln-18)			; fool auctex - outline is ok now.

;; Local Variables:
;; outline-regexp: ";;; \\|(def.."
;; End:

;;; outl-mouse.el ends here
