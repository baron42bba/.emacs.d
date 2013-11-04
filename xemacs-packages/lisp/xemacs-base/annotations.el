;;; annotations.el --- interface to marginal annotations

;; Copyright (C) 1992-1994 Free Software Foundation, Inc.

;; Author: Chuck Thompson <cthomp@cs.uiuc.edu>
;; Maintainer: XEmacs Development Team
;; Created: 10-Oct-93
;; Keywords: extensions, hypermedia, outlines

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

;;; Commentary:

;; Enhanced by Andy Piper <ajp@eng.cam.ac.uk>: 6-may-94

;; Last modified:  12-May-95 by Chuck Thompson.

;; The annotations are implemented on top of extents.  The extent property
;; 'annotation of an extent being used as an annotation is vector of size 6:
;;	[<data> <action> <menu> <glyph> <down-glyph> <rightp>]
;;

;;; Code:

;;;###autoload
(defvar make-annotation-hook nil
  "*Function or functions to run immediately after creating an annotation.")

;;;###autoload
(defvar before-delete-annotation-hook nil
  "*Function or functions to run immediately before deleting an annotation.")

;;;###autoload
(defvar after-delete-annotation-hook nil
  "*Function or functions to run immediately after deleting an annotation.")

(defvar annotation-local-map-default
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'annotation-local-map)
    (define-key map 'button1 'annotation-activate-function-default)
    (define-key map 'button3 'annotation-popup-menu)
    map)
  "Keymap used to activate annotations with only annotation data passed.")

(defvar annotation-local-map-with-event
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'annotation-local-map)
    (define-key map 'button1 'annotation-activate-function-with-event)
    (define-key map 'button3 'annotation-popup-menu)
    map)
  "Keymap used to activate annotations with annotation data and event passed.")

;;
;; When the mouse is pressed and released over an annotation glyph
;; this will run the annotation action passing a single arg, the value
;; of the annotation data field.
;;
(defun annotation-activate-function-default (event)
  (interactive "e")
  (let ((extent (event-glyph-extent event))
	(mouse-down t)
	(up-glyph nil))
    ;; make the glyph look pressed
    (cond ((annotation-down-glyph extent)
	   (setq up-glyph (annotation-glyph extent))
	   (set-annotation-glyph extent (annotation-down-glyph extent))))
    (while mouse-down
      (setq event (next-event event))
      (if (button-release-event-p event)
	  (setq mouse-down nil)))
    ;; make the glyph look released
    (cond ((annotation-down-glyph extent)
	   (set-annotation-glyph extent up-glyph)))
    (if (eq extent (event-glyph-extent event))
	(if (annotation-action extent)
	    (funcall (annotation-action extent) (annotation-data extent))))))

;;
;; When the mouse is pressed and released over an annotation glyph
;; this will run the annotation action passing two args, the value
;; of the annotation data field and the event which triggered the
;; annotation.
;;
(defun annotation-activate-function-with-event (event)
  (interactive "e")
  (let ((extent (event-glyph-extent event))
	(mouse-down t)
	(up-glyph nil))
    ;; make the glyph look pressed
    (cond ((annotation-down-glyph extent)
	   (setq up-glyph (annotation-glyph extent))
	   (set-annotation-glyph extent (annotation-down-glyph extent))))
    (while mouse-down
      (setq event (next-event event))
      (if (button-release-event-p event)
	  (setq mouse-down nil)))
    ;; make the glyph look released
    (cond ((annotation-down-glyph extent)
	   (set-annotation-glyph extent up-glyph)))
    (if (eq extent (event-glyph-extent event))
	(if (annotation-action extent)
	    (funcall (annotation-action extent) (annotation-data extent)
		     event)))))

;; #### Glyphs should be glyphs should be glyphs
;;;###autoload
(defun make-annotation (glyph &optional pos layout buffer with-event d-glyph rightp)
  "Create a marginal annotation, displayed using GLYPH, at position POS.
GLYPH may be either a glyph object or a string.  Use layout policy
LAYOUT and place the annotation in buffer BUFFER.  If POS is nil, point is
used.  If LAYOUT is nil, `whitespace' is used.  If BUFFER is nil, the
current buffer is used.  If WITH-EVENT is non-nil, then when an annotation
is activated, the triggering event is passed as the second arg to the
annotation function.  If D-GLYPH is non-nil then it is used as the glyph 
that will be displayed when button1 is down.  If RIGHTP is non-nil then
the glyph will be displayed on the right side of the buffer instead of the
left."
  (let ((new-annotation))
    ;; get the buffer to add the annotation at
    (if (not buffer)
	(setq buffer (current-buffer))
      (setq buffer (get-buffer buffer)))
    ;; get the position to put it at
    (if (not pos)
	(save-excursion
	  (set-buffer buffer)
	  (setq pos (point))))
    ;; make sure it gets some layout policy
    (if (not layout)
	(setq layout 'whitespace))

    ;; make sure the glyph arguments are actually glyphs
    (if (and glyph (not (glyphp glyph)))
	(setq glyph (make-glyph glyph)))
    (if (and d-glyph (not (glyphp d-glyph)))
	(setq d-glyph (make-glyph d-glyph)))

    ;; create the actual annotation
    (setq new-annotation (make-extent pos pos buffer))
    (detach-extent new-annotation)
    (set-extent-endpoints new-annotation pos pos)
    (if rightp
	(set-extent-end-glyph new-annotation glyph layout)
      (set-extent-begin-glyph new-annotation glyph layout))
    (set-extent-property new-annotation 'annotation 
			 (vector nil nil nil glyph d-glyph rightp))
    (set-extent-property new-annotation 'end-closed t)
    (set-extent-property new-annotation 'start-open t)
    (set-extent-property new-annotation 'duplicable t)
    (if with-event
	(set-extent-property new-annotation 'keymap
			     annotation-local-map-with-event)
      (set-extent-property new-annotation 'keymap
			   annotation-local-map-default))
    (run-hook-with-args 'make-annotation-hook new-annotation)
    new-annotation))

(fset 'make-graphic-annotation 'make-annotation)
(make-obsolete 'make-graphic-annotation 'make-annotation)

;;;###autoload
(defun delete-annotation (annotation)
  "Remove ANNOTATION from its buffer.  This does not modify the buffer text."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (progn
      (run-hook-with-args 'before-delete-annotation-hook annotation)
      (delete-extent annotation)
      (run-hooks 'after-delete-annotation-hook))))

;;;###autoload
(defun annotationp (annotation)
  "T if OBJECT is an annotation."
  (and (extent-live-p annotation)
       (not (null (extent-property annotation 'annotation)))))

(defun annotation-visible (annotation)
  "T if there is enough available space to display ANNOTATION."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (not (extent-property annotation 'glyph-invisible))))

;;;###autoload
(defun annotation-at (&optional pos buffer)
  "Return the first annotation at POS in BUFFER.
BUFFER defaults to the current buffer.  POS defaults to point in BUFFER."
  (car (annotations-at pos buffer)))
(make-obsolete 'annotation-at 'annotations-at)

(defun annotation-layout (annotation)
  "Return the layout policy of annotation ANNOTATION.  The layout policy
is set using `set-annotation-layout'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (if (eq 'right (annotation-side annotation))
	(extent-end-glyph-layout annotation)
      (extent-begin-glyph-layout annotation))))


(defun annotation-side (annotation)
  "Return the side of the buffer the annotation is displayed on.
Return value is either 'left or 'right."
  (if (aref (extent-property annotation 'annotation) 5)
      'right
    'left))

(defun set-annotation-layout (annotation layout)
  "Set the layout policy of ANNOTATION to LAYOUT.  The function
`annotation-layout' returns the current layout policy."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (if (eq 'right (annotation-side annotation))
	(set-extent-end-glyph-layout annotation layout)
      (set-extent-begin-glyph-layout annotation layout))))

;; Now that annotations use glyphs this function has little value and
;; will actually not work as is.
;(defun annotation-type (annotation)
;  "Return the display type of the annotation ANNOTATION.  The type will
;be one of the following symbols:
;
;	pixmap
;	bitmap
;	string
;	nil	(the object is not an annotation)"
;  (if (not (annotationp annotation))
;      nil
;    (let ((glyph (annotation-glyph annotation)))
;      (if (stringp glyph)
;	  'stringp
;	(if (not (pixmapp glyph))
;	    (error "%s is a corrupt annotation" annotation)
;	  (if (> (pixmap-depth glyph) 0)
;	      'pixmap
;	    'bitmap))))))
(make-obsolete 'annotation-type "This function no longer has any meaning.")

(defun annotation-width (annotation)
  "Return the width of the annotation ANNOTATION in pixels."  
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (glyph-width (annotation-glyph annotation))))

(defun annotation-glyph (annotation)
  "If ANNOTATION is of type `string' return the string.  Otherwise, return
the glyph object used to display ANNOTATION.  The glyph is set using
`set-annotation-glyph'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (aref (extent-property annotation 'annotation) 3)))

(defun set-annotation-glyph (annotation glyph &optional layout side)
  "Set the representation of ANNOTATION to GLYPH.
GLYPH should be a glyph object.  If LAYOUT is non-nil, set the layout
policy of the annotation to LAYOUT.  If SIDE is equal to 'left or 'right
change the side of the annotation to that value.
The function `annotation-glyph' returns the current glyph."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    ;; else
    (or layout (setq layout (annotation-layout annotation)))
    (or side   (setq side   (annotation-side   annotation)))
    (cond ((eq side 'left)
	   (set-extent-begin-glyph annotation glyph layout)
	   (set-extent-end-glyph annotation nil)
	   (aset (extent-property annotation 'annotation) 5 nil))
	  ((eq side 'right)
	   (set-extent-end-glyph annotation glyph layout)
	   (set-extent-begin-glyph annotation nil)
	   (aset (extent-property annotation 'annotation) 5 nil)))
    (aset (extent-property annotation 'annotation) 3 glyph)
    (annotation-glyph annotation)))

(defun annotation-down-glyph (annotation)
  "If ANNOTATION is of type `string' return the down string.  Otherwise,
return the glyph object of the down-glyph representing ANNOTATION.
The down-glyph is set using `set-annotation-down-glyph'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (aref (extent-property annotation 'annotation) 4)))

(defun set-annotation-down-glyph (annotation glyph)
  "Set the depressed representation of ANNOTATION to GLYPH.  
GLYPH should be a glyph object. 
The function `annotation-down-glyph' returns the current down-glyph."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (aset (extent-property annotation 'annotation) 4 glyph)))

(define-obsolete-function-alias 'annotation-graphic 'annotation-glyph)
(define-obsolete-function-alias 'set-annotation-graphic 'set-annotation-glyph)
  
(defun annotation-data (annotation)
  "Return the data associated with annotation ANNOTATION.  The data is
set using `set-annotation-data'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (aref (extent-property annotation 'annotation) 0)))

(defun set-annotation-data (annotation data)
  "Set the data field of ANNOTATION to DATA.
The function `annotation-data' returns the current data."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (aset (extent-property annotation 'annotation) 0 data)))

(defun annotation-action (annotation)
  "Return the action associated with annotation ANNOTATION.  The action
is set using `set-annotation-action'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (aref (extent-property annotation 'annotation) 1)))

(defun set-annotation-action (annotation action)
  "Set the action field of ANNOTATION to ACTION.
The function `annotation-action' returns the current action."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (aset (extent-property annotation 'annotation) 1 action)))

(defun annotation-face (annotation)
  "Return the face associated with annotation ANNOTATION.
The face is set using `set-annotation-face'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (extent-face annotation)))

(defun set-annotation-face (annotation face)
  "Set the face associated with annotation ANNOTATION to FACE.
The function `annotation-face' returns the current face."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (set-extent-face annotation face)))

(defun hide-annotation (annotation)
  "Remove ANNOTATION's glyph so that it is invisible."
  (if (eq (annotation-side annotation) 'left)
      (set-extent-begin-glyph annotation nil)
    (set-extent-end-glyph annotation nil)))
(define-obsolete-function-alias 'annotation-hide 'hide-annotation)

(defun reveal-annotation (annotation)
  "Add ANNOTATION's glyph so that it is visible."
  (if (eq (annotation-side annotation) 'left)
      (set-extent-begin-glyph annotation (annotation-glyph annotation))
    (set-extent-end-glyph annotation (annotation-glyph annotation))))
(define-obsolete-function-alias 'annotation-reveal 'reveal-annotation)

;;;###autoload
(defun annotations-in-region (start end buffer)
  "Return all annotations in BUFFER between START and END inclusively."
  (save-excursion
    (set-buffer buffer)

    (if (< start (point-min))
      (error "<start> not in range of buffer"))
    (if (> end (point-max))
      (error "<end> not in range of buffer"))

    (let (note-list)
      (map-extents #'(lambda (extent dummy)
		       (progn
			 (setq note-list (cons extent note-list))
			 nil))
		   buffer start end nil 'end-closed 'annotation)
      note-list)))

;;;###autoload
(defun annotations-at (&optional pos buffer)
  "Return a list of all annotations at POS in BUFFER.
If BUFFER is nil, the current buffer is used.  If POS is nil, point is used."
  (if (not buffer)
      (setq buffer (current-buffer)))
  (if (not pos)
      (save-excursion
	(set-buffer buffer)
	(setq pos (point))))

  (annotations-in-region pos pos buffer)
)

;;;###autoload
(defun annotation-list (&optional buffer)
  "Return a list of all annotations in BUFFER.
If BUFFER is nil, the current buffer is used."
  (if (not buffer)
    (setq buffer (current-buffer)))

  (save-excursion
    (set-buffer buffer)
    (annotations-in-region (point-min) (point-max) buffer)))

;;;###autoload
(defun all-annotations ()
  "Return a list of all annotations in existence."
  (let ((b (buffer-list))
	result)
    (while b
      (setq result (nconc result (annotation-list (car b))))
      (setq b (cdr b)))
    result))

;;; #### really this menus junk should append to the prevailing menu
;;;      in the same way `popup-mode-menu' does.  --jwz

;; annotations menu stuff
(defun annotation-popup-menu (event)
  "Pop up a menu of annotations commands.
Point is temporarily moved to the click position."
  (interactive "e")
  (let ((extent (event-glyph-extent event)))
    (save-excursion
      (goto-char (extent-end-position extent))
      (if (annotation-menu extent)
	  (popup-menu (annotation-menu extent))
	(popup-mode-menu)))))

(defun set-annotation-menu (annotation menu)
  "Set the menu field of ANNOTATION to MENU.  The function
`annotation-menu' returns the current menu."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (aset (extent-property annotation 'annotation) 2 menu)))

(defun annotation-menu (annotation)
  "Return the menu associated with annotation ANNOTATION.  The menu
is set using `set-annotation-menu'."
  (if (not (annotationp annotation))
      (error "%s is not an annotation" annotation)
    (aref (extent-property annotation 'annotation) 2)))

(provide 'annotations)

;;; annotations.el ends here
