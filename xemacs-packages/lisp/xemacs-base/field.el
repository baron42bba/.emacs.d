;;; field.el --- Emacs text field support

;; Copyright (C) 2004 Jerry James

;; Maintainer: Jerry James <james@xemacs.org>
;; Keywords: field

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

;;; Synched up with: FSF 21.3 + 09072004 CVS editfns.c.

;;; Commentary:

;; Emacs has a notion of a text field, which is a place to enter data
;; (for communication with a subprocess, for example).  The Emacs
;; implementation is written in C.  This implementation in XEmacs is in
;; Lisp for now.  Report performance problems to the XEmacs Beta mailing
;; list <xemacs-beta@xemacs.org>.

;; This Lisp code is closely modeled after the C code in the Emacs
;; sources.  The author will gladly consider better Lisp approaches.

;; In Emacs, text fields are created by making overlays with the 'field
;; property, or by using the text-property interface to mark a region of
;; text with the 'field property.  However, the default "stickiness"
;; (Emacs) or "open/closedness" (XEmacs) of text is reversed in XEmacs
;; from Emacs.  This affects the determination of which of two adjacent
;; fields is the relevant field.  This code:
;;
;; (with-temp-buffer
;;   (insert "This is some text.\n")
;;   (put-text-property 1  9 'field 1)
;;   (put-text-property 9 13 'field 2)
;;   (goto-char 9)
;;   (insert-and-inherit "not ")
;;   (get-char-property 9 'field))
;;
;; evaluates to 1 in Emacs, and 2 in XEmacs.  This file provides a
;; function not available in Emacs, make-field, which produces
;; Emacs-compatible fields.  When writing code intended to be compatible
;; with Emacs, either use make-field, or explicitly set the stickiness
;; (open/closedness) of your fields.


;;; Code:

(defgroup field nil
  "Text fields."
  :group 'editing)

(defcustom inhibit-field-text-motion nil
  "*Non-nil means text motion commands do not notice fields."
  :type 'boolean
  :group 'field)

;;;###autoload
(defun make-field (from to value &optional buffer front-advance rear-advance)
  "Make a field with value VALUE over the range [FROM, TO) in BUFFER.
If omitted, BUFFER defaults to the current buffer.
FROM and TO may be integers or markers.
The fifth argument, FRONT-ADVANCE, if non-nil, makes the front delimiter
advance when text is inserted there.
The sixth argument, REAR-ADVANCE, if non-nil, makes the rear delimiter
advance when text is inserted there."
  (let ((extent (make-extent from to buffer)))
    (set-extent-property extent 'field value)
    (set-extent-property extent 'start-open (not front-advance))
    (set-extent-property extent 'end-open rear-advance)))

;; The workhorse function
(defun find-field (&optional pos merge-at-boundary beg-limit end-limit
		   skip-start skip-stop)
  "Return a pair (START . STOP) marking the field surrounding POS.
If POS is nil, the value of (point) is used instead.
If MERGE-AT-BOUNDARY is non-nil, then
 - if POS is at the very first position of a field, then the beginning of the
   previous field is returned instead of the beginning of POS's field (since
   the end of a field is actually also the beginning of the next input field,
   this behavior is sometimes useful); and
 - if two fields are separated by a field with the special value `boundary',
   and POS lies within it, then the two separated fields are considered to be
   adjacent, and POS between them, when finding the beginning and ending of
   the \"merged\" field.
If BEG-LIMIT or END-LIMIT are non-nil, they limit the range of the returned
results; they do not affect boundary behavior.
if SKIP-START is non-nil, do not compute the START point (it is nil).
If SKIP-STOP is non-nil, do not computer the STOP point (it is nil)."
  (if (null pos)
      (setq pos (point)))
  (let ((after-field (get-char-property pos 'field nil 'after))
	(before-field (get-char-property pos 'field nil 'before))
	at-field-start at-field-end start stop)

    ;; See if we need to handle the case where MERGE-AT-BOUNDARY is nil
    ;; and POS is at beginning of a field, which can also be interpreted
    ;; as the end of the previous field.  Note that the case where if
    ;; MERGE-AT-BOUNDARY is non-nil (see docstring) is actually the more
    ;; natural one; then we avoid treating the beginning of a field specially.
    (unless merge-at-boundary
      (let ((field
	     (map-extents #'(lambda (ext ign) (extent-property ext 'field))
			  nil pos pos nil nil 'field)))
	(unless (eq field after-field)
	  (setq at-field-end t))
	(unless (eq field before-field)
	  (setq at-field-start t))
	;; If an inserted char would have a nil field while the surrounding
	;; text is non-nil, we're probably not looking at a zero-length
	;; field, but instead at a non-nil field that is not intended for
	;; editing (such as comint's prompts).
	(if (and (null field) at-field-start at-field-end)
	    (setq at-field-start nil
		  at-field-end nil))))

    ;; Note about special `boundary' fields:
    ;;
    ;; Consider the case where the point (`.') is between the fields `x' and
    ;; `y':
    ;;
    ;;   xxxx.yyyy
    ;;
    ;; In this situation, if MERGE-AT-BOUNDARY is non-nil, we consider the
    ;; `x' and `y' fields as forming one big merged field, and so the end
    ;; of the field is the end of `y'.
    ;;
    ;; However, if `x' and `y' are separated by a special `boundary' field
    ;; (a field with a `field' char-property of 'boundary), then we ignore
    ;; this special field when merging adjacent fields.  Here's the same
    ;; situation, but with a `boundary' field between the `x' and `y' fields:
    ;;
    ;;   xxx.BBBByyyy
    ;;
    ;; Here, if point is at the end of `x', the beginning of `y', or
    ;; anywhere in-between (within the `boundary' field), we merge all
    ;; three fields and consider the beginning as being the beginning of
    ;; the `x' field, and the end as being the end of the `y' field.

    ;; Compute START
    (unless skip-start
      (if at-field-start
	  ;; POS is at the edge of a field, and we should consider it as
	  ;; the beginning of the following field.
	  (setq start pos)
	(let ((p pos))
	  (if (and merge-at-boundary (eq before-field 'boundary))
	      ;; Skip a `boundary' field
	      (setq p
		    (previous-single-property-change p 'field nil beg-limit)))
	  (setq p (previous-single-property-change p 'field nil beg-limit))
	  (setq start (if p p (point-min))))))

    ;; Compute STOP
    (unless skip-stop
      (if at-field-end
	  ;; POS is at the edge of a field, and we should consider it as
	  ;; the end of the previous field.
	  (setq stop pos)
	(if (and merge-at-boundary (eq after-field 'boundary))
	    ;; Skip a `boundary' field
	    (setq pos (next-single-property-change pos 'field nil end-limit)))
	(setq pos (next-single-property-change pos 'field nil end-limit))
	(setq stop (if pos pos (point-max)))))

    ;; Return (START . STOP)
    (cons start stop)))

;;;###autoload
(defun delete-field (&optional pos)
  "Delete the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS."
  (let* ((field (find-field pos))
	 (start (car field))
	 (end (cdr field)))
    (if (< start end)
	(delete-region start end))))

;;;###autoload
(defun field-string (&optional pos)
  "Return the contents of the field surrounding POS as a string.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS."
  (let ((field (find-field pos)))
    (buffer-substring (car field) (cdr field))))

;;;###autoload
(defun field-string-no-properties (&optional pos)
  "Return the contents of the field around POS, without text-properties.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS."
  (let ((field (find-field pos)))
    (buffer-substring-no-properties (car field) (cdr field))))

;;;###autoload
(defun field-beginning (&optional pos escape-from-edge limit)
  "Return the beginning of the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the beginning of its
field, then the beginning of the *previous* field is returned.
If LIMIT is non-nil, it is a buffer position; if the beginning of the field
is before LIMIT, then LIMIT will be returned instead."
  (car (find-field pos escape-from-edge limit nil nil t)))

;;;###autoload
(defun field-end (&optional pos escape-from-edge limit)
  "Return the end of the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the end of its field,
then the end of the *following* field is returned.
If LIMIT is non-nil, it is a buffer position; if the end of the field
is after LIMIT, then LIMIT will be returned instead."
  (cdr (find-field pos escape-from-edge nil limit t nil)))

;;;###autoload
(defun constrain-to-field (new-pos old-pos &optional escape-from-edge
			   only-in-line inhibit-capture-property)
  "Return the position closest to NEW-POS that is in the same field as OLD-POS.

A field is a region of text with the same `field' property.
If NEW-POS is nil, then the current point is used instead, and set to the
constrained position if that is different.

If OLD-POS is at the boundary of two fields, then the allowable
positions for NEW-POS depend on the value of the optional argument
ESCAPE-FROM-EDGE: If ESCAPE-FROM-EDGE is nil, then NEW-POS is
constrained to the field that has the same `field' char-property
as any new characters inserted at OLD-POS, whereas if ESCAPE-FROM-EDGE
is non-nil, NEW-POS is constrained to the union of the two adjacent
fields.  Additionally, if two fields are separated by another field with
the special value `boundary', then any point within this special field is
also considered to be `on the boundary'.

If the optional argument ONLY-IN-LINE is non-nil and constraining
NEW-POS would move it to a different line, NEW-POS is returned
unconstrained.  This useful for commands that move by line, like
\\[next-line] or \\[beginning-of-line], which should generally respect field
boundaries only in the case where they can still move to the right line.

If the optional argument INHIBIT-CAPTURE-PROPERTY is non-nil, and OLD-POS has
a non-nil property of that name, then any field boundaries are ignored.

Field boundaries are not noticed if `inhibit-field-text-motion' is non-nil."
  (let (orig-point)
    (unless new-pos
      (setq orig-point (point)
	    new-pos (point)))
    (when (and
	   (null inhibit-field-text-motion)
	   (not (eq new-pos old-pos))
	   (or (get-char-property new-pos 'field)
	       (get-char-property old-pos 'field))
	   (or (null inhibit-capture-property)
	       (null (get-char-property old-pos inhibit-capture-property))))
      ;; NEW-POS is not within the same field as OLD-POS; try to move NEW-POS
      ;; so that it is
      (let* ((fwd (> new-pos old-pos))
	     (field-bound
	      (if fwd
		  (field-end old-pos escape-from-edge new-pos)
		(field-beginning old-pos escape-from-edge new-pos))))
	;; See if ESCAPE-FROM-EDGE caused FIELD-BOUND to jump to the other
	;; side of NEW-POS, which would mean that NEW-POS is already
	;; acceptable, and it is not necessary to constrain it to FIELD-BOUND.
	(if (and (if (< field-bound new-pos) fwd (not fwd))
		 (or (null only-in-line)
		     ;; Check that NEW-POS and FIELD-BOUND are on the same line
		     (not
		      (save-excursion
			(goto-char new-pos)
			(if fwd
			    (search-forward "\n" field-bound t 1)
			  (search-backward "\n" field-bound t 1))))))
	    ;; Constrain NEW-POS to FIELD-BOUND
	    (setq new-pos field-bound))

	(if (and orig-point (/= new-pos orig-point))
	    ;; The NEW-POS argument was originally nil, so automatically set
	    ;; point.
	    (goto-char new-pos)))))
  new-pos)
