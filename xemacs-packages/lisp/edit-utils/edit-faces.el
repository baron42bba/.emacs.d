;;; edit-faces.el -- interactive face editing mode

;; Copyright (C) 1998 Free Software Foundation, Inc.
;; Copyright (C) 1994, 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 1996 Ben Wing.

;; Author: Stig <stig@hackvan.com>.
;; Maintainer: XEmacs Development Team <xemacs-beta@xemacs.org>
;; Keywords: faces

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This is the file that won't die.  Even though we have otherwise completely
;; converted to custom, there is still apparently a need to view a lot of
;; faces in one buffer.

;;; ChangeLog:

;; May 11, 1998:  use easymenu. -sb
;; Significantly fixed up: Ben Wing <ben@xemacs.org>.

;;; Code:

(defvar edit-faces-menu
  '("Edit-Faces"
    ["Copy other face..." ef-copy-other-face t]
    ["Copy this face..." ef-copy-this-face t]
    ["Make smaller"	ef-smaller	t]
    ["Make larger"	ef-larger	t]
    ["Toggle bold"	ef-bold		t]
    ["Toggle italic"	ef-italic	t]
    ["Toggle underline"	ef-underline	t]
    ["Query true font"	ef-truefont	t]
    ["Set font"		ef-font		t]
    ["Set foreground"	ef-foreground	t]
    ["Set background"	ef-background	t]
    ["Set doc string"	ef-doc-string	t]
    ["Quit"		ef-quit		t]
    ))

(or (find-face 'underline)
    (progn (make-face 'underline)
	   (set-face-underline-p 'underline t)))

(define-derived-mode edit-faces-mode list-mode
  "Edit-Faces"
  "Major mode for `edit-faces' buffers.

Editing commands:

\\{edit-faces-mode-map}"
  (setq truncate-lines t)
  ;; auto-show-mode is too confusing in this mode
  (setq auto-show-mode nil)
  (setq	mode-popup-menu edit-faces-menu)
  (when (and (featurep 'menubar)
	     current-menubar)
    (easy-menu-add edit-faces-menu)))

(let ((map edit-faces-mode-map))
  (define-key map "<" 'ef-smaller)
  (define-key map ">" 'ef-larger)
  (define-key map "c" 'ef-copy-other-face)
  (define-key map "C" 'ef-copy-this-face)
  (define-key map "s" 'ef-smaller)
  (define-key map "l" 'ef-larger)
  (define-key map "b" 'ef-bold)
  (define-key map "i" 'ef-italic)
  (define-key map "e" 'ef-font)
  (define-key map "f" 'ef-font)
  (define-key map "u" 'ef-underline)
  (define-key map "t" 'ef-truefont)
  (define-key map "F" 'ef-foreground)
  (define-key map "B" 'ef-background)
  (define-key map "D" 'ef-doc-string)
  (define-key map "d" 'ef-delete)
  (define-key map "n" 'ef-next)
  (define-key map "p" 'ef-prev)
  (define-key map " " 'ef-next)
  (define-key map "\C-?" 'ef-prev)
  (define-key map "g" 'edit-faces)	; refresh display
  (define-key map "q" 'ef-quit)
  (define-key map "\C-c\C-c" 'bury-buffer))

;;;###autoload
(defun edit-faces ()
  "Alter face characteristics by editing a list of defined faces.
Pops up a buffer containing a list of defined faces.

WARNING: the changes you may perform with this function are no longer
saved. The prefered way to modify faces is now to use `customize-face'. If you 
want to specify particular X font names for faces, please do so in your
.XDefaults file.

Editing commands:

\\{edit-faces-mode-map}"
  (interactive)
  (pop-to-buffer (get-buffer-create "*Edit Faces*"))
  (reset-buffer (current-buffer))

  ;; face-list returns faces in a random order so we sort
  ;; alphabetically by the name in order to insert some logic into
  ;; the ordering.
  (let ((flist (sort (face-list)
		     (function
		      (lambda (x y)
			(string-lessp (symbol-name x) (symbol-name y))))))
	face)
    (ef-update-face-description t)	; insert header line
    (while (setq face (car flist))
      (ef-update-face-description face)
      (setq flist (cdr flist))
      ))
  (edit-faces-mode)
)

(defun ef-foreground-callback (event extent user-data)
  (ef-foreground (ef-face-arg (extent-start-position extent)
			      (extent-object extent))))
  
(defun ef-background-callback (event extent user-data)
  (ef-background (ef-face-arg (extent-start-position extent)
			      (extent-object extent))))

(defun ef-font-callback (event extent user-data)
  (ef-font (ef-face-arg (extent-start-position extent)
			(extent-object extent))))

(defun ef-doc-string-callback (event extent user-data)
  (ef-doc-string (ef-face-arg (extent-start-position extent)
			      (extent-object extent))))

(defun ef-update-face-description (face &optional replace)
  "Given a face, inserts a description of that face into the current buffer.
Inserts a descriptive header if passed `t'."
  (let ((face-name-fmt "%-25s")
	(foreground-fmt "%-15s")
	(background-fmt "%-15s")
	(font-fmt "%s")
	(buffer-read-only nil)
	fg bg font)
    (if (eq face t)
	(insert-face (format (concat face-name-fmt " " foreground-fmt " "
				     background-fmt " " font-fmt "\n")
			     "Face" "Foreground" "Background" "Font Spec")
		     'underline)
      (or replace (setq replace face))
      (goto-char (point-min)) 
      (if (re-search-forward (concat "^" (symbol-name replace) " ") nil 0)
	  (progn
	    (beginning-of-line)
	    (delete-region (point) (progn (forward-line 2) (point)))
	    ))
      (setq fg (face-foreground-instance face)
	    bg (face-background-instance face)
	    font (face-font-instance face))
      (let ((st (point))
	    (fn #'(lambda (str callback)
		    (let ((st1 (point)))
		      (insert str)
		      (add-list-mode-item st1 (point) nil callback)))))
	(funcall fn (format face-name-fmt (symbol-name face)) nil)
	(insert " ")
	(funcall fn (format foreground-fmt (color-instance-name fg))
		 'ef-foreground-callback)
	(insert " ")
	(funcall fn (format background-fmt (color-instance-name bg))
		 'ef-background-callback)
	(insert " ")
	(funcall fn (format font-fmt (font-instance-name font))
		 'ef-font-callback)
	(insert "\n  (")
	(funcall fn (or (face-doc-string face) "")
		 'ef-doc-string-callback)
	(insert ")")
	(add-nonduplicable-text-properties st (point)
					   `(face ,face eface ,face
						  start-open t))
	(insert "\n")
	)
      (and replace (forward-line -1))
      ))
  )

(defun ef-face-arg (&optional pos buffer)
  (if (and (not pos) (not buffer))
      (and current-mouse-event
	   (mouse-event-p current-mouse-event)
	   (mouse-set-point current-mouse-event)))
  (or buffer (setq buffer (current-buffer)))
  (or pos (setq pos (point buffer)))
  (let ((face (or (get-char-property pos 'eface buffer)
		  (and (> pos (point-min buffer))
		       (get-char-property (1- pos) 'eface buffer)))))
    (or face (error "There is no face to edit on this line."))
    face))

(defun ef-delete (arg)
  "Delete the face on the current line from the *Edit Faces* buffer.
The face is not altered.  The buffer can be regenerated again with
M-x edit-faces."
  (interactive "p") 
  (and current-mouse-event (mouse-event-p current-mouse-event)
       (mouse-set-point current-mouse-event))
  (let ( ;; is this worth the bother? (fwd (> arg 0))
	(count (abs arg))
	(buffer-read-only nil)
	ex)
    (while (not (zerop (prog1 count (setq count (1- count)))))
      (setq ex (text-property-bounds (point) 'eface nil 'at))
      (or ex (error "There is no face to delete on this line."))
      (delete-region (car ex) (cdr ex))
      (delete-blank-lines))))
  
(defun ef-next (arg)
  "Move forward ARG entries in the face table."
  (interactive "p") 
  (let ((bounds (next-text-property-bounds arg (point) 'eface)))
    (if bounds (goto-char (car bounds))
      (goto-char (if (> arg 0) (point-max) (point-min))))))

(defun ef-prev (arg)
  "Move forward ARG entries in the face table."
  (interactive "p") 
  (ef-next (- arg)))

(defun ef-smaller (face)
  (interactive (list (ef-face-arg)))
  (make-face-smaller face)
  (ef-update-face-description face))

(defun ef-larger (face)
  (interactive (list (ef-face-arg)))
  (make-face-larger face)
  (ef-update-face-description face))

(defun ef-face-font-indirect (face)
  (let ((font (face-font-instance face)))
    (or font (face-font-instance 'default))))

(defun ef-face-bold-p (face)
  (let ((font (ef-face-font-indirect face)))
    (not (not (string-match "-bold-" (font-instance-name font))))))

(defun ef-face-italic-p (face)
  (let ((font (ef-face-font-indirect face)))
    (not (not (string-match "-[io]-" (font-instance-name font))))))

(defun ef-bold (face)
  (interactive (list (ef-face-arg)))
  (if (ef-face-bold-p face)
      (make-face-unbold face)
    (make-face-bold face))
  (ef-update-face-description face))

(defun ef-italic (face)
  (interactive (list (ef-face-arg)))
  (if (ef-face-italic-p face)
      (make-face-unitalic face)
    (make-face-italic face))
  (ef-update-face-description face))

(defun ef-underline (face)
  (interactive (list (ef-face-arg)))
  (set-face-underline-p face (not (face-underline-p face)))
  (ef-update-face-description face))

(defun ef-truefont (face)
  (interactive (list (ef-face-arg)))
  (let ((font (face-font-instance face))
	(name (symbol-name face)))
    (if font
	(message "True font for `%s': %s" name (font-instance-truename font))
      (message "The face `%s' does not have its own font." name))))

(defun ef-foreground (face)
  (interactive
   (list (ef-face-arg)))
  (set-face-foreground
   face
   (read-color (format "Foreground color for `%s': " (symbol-name face))
	       nil
	       (color-instance-name (face-foreground-instance face))))
  (ef-update-face-description face))

(defun ef-background (face)
  (interactive
   (list (ef-face-arg)))
  (set-face-background
   face
   (read-color (format "Background color for `%s': " (symbol-name face))
	       nil
	       (color-instance-name (face-background-instance face))))
  (ef-update-face-description face))

(defun ef-doc-string (face)
  (interactive
   (list (ef-face-arg)))
  (set-face-doc-string
   face
   (read-string (format "Doc string for `%s': " (symbol-name face))
		(face-doc-string face)))
  (ef-update-face-description face))

(defun ef-copy-other-face (src dst)
  (interactive
   (let* ((f (ef-face-arg))
	  (name (symbol-name f)))
     (list (read-face (format "Make `%s' a copy of what face?: " name) t) f)))
  (copy-face src dst)
  (ef-update-face-description dst dst))

(defun ef-copy-this-face (src dst)
  (interactive
   (let* ((f (ef-face-arg))
	  (name (symbol-name f)))
       (list f (read-face (format "Copy `%s' onto what face?: " name)))))
  (copy-face src dst)
  (ef-update-face-description dst dst))

(defun ef-font (face)
  (interactive
   (list (ef-face-arg)))
  (let* ((ofont (face-font-instance face))
	 (font (read-string (format "Font for `%s': " (symbol-name face))
			    (font-instance-name (face-font-instance face))))
	 others)
    ;; you might think that this could be moved into the loop below, but I
    ;; think that it's important to see the new font before asking if the
    ;; change should be global. 
    (set-face-font face (if (and (string= font "")
				 (not (eq face 'default)))
			    nil font))
    (ef-update-face-description face)
    (setq others (delq nil (mapcar (lambda (f)
				     (and (equal (face-font-instance f) ofont)
					  f))
			       (face-list))))
    (if (and others
	     (y-or-n-p "Make the same font change for other faces? "))
	(while others
	  (setq face (car others)
		others (cdr others))
	  (set-face-font face font)
	  (ef-update-face-description face)))
    ))

(defun ef-quit ()
  (interactive)
  (or (one-window-p t 0)
      (delete-window))
  (kill-buffer "*Edit Faces*"))

;;; edit-faces.el ends here
