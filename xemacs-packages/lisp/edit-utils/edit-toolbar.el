;;; edit-toolbar.el --- Interactive toolbar editing mode for XEmacs

;; Copyright (C) 1996 Peter D. Pezaris

;; Author: Peter D. Pezaris <pez@dwwc.com>
;; Keywords: tools

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

;; To use edit-toolbar.el, simply type M-x edit-toolbar RET

;; For help on the various commands you can type ? in a edit-toolbar
;; buffer.  To save a modified toolbar type C-x C-s in an edit-toolbar
;; buffer.  If you want to use a saved toolbar in your future XEmacs
;; sessions, add the following line of code to your .emacs file:

;;     (load "~/.xemacs/.toolbar")

;; Acknowledgements:

;; Many thanks to Stig <stig@hackvan.com> and Ben Wing <ben@xemacs.org>
;; for writing edit-faces.el, on which much of this code is based.

;; To do:

;; o The function edit-toolbar-quit should do something other than just
;;   bury the buffer.
;; o Dynamically add new items to edit-toolbar-button-alist as new buttons
;;   are added.
;; o Allow more than one toolbar to be saved in the ~/.xemacs/.toolbar file.
;; o Allow buttons to be copied from any toolbar.
;; o Allow multiple toolbars to be edited simultaneously.

;;; Change Log:

;; Modified by Mike Scheidler <c23mts@eng.delcoelect.com> 25 Jul 1997
;;  - Enabled editing of any toolbar (not just `default-toolbar').
;;  - Added context sensitivity to `edit-toolbar-menu'.
;;  - Added support for `nil' toolbar item (left/right divider).
;;  - Enabled editing of empty toolbars.
;; Modified by Jeff Miller <jmiller@smart.net> 17 Aug 1997
;;  - Modfied how added toolbar buttons are created and saved.  

;;; Code:

(defvar edit-toolbar-version "1.03"
  "Version of Edit Toolbar.")

(defvar edit-toolbar-temp-toolbar-name nil
  "Value of toolbar being edited.")

(defvar edit-toolbar-temp-toolbar nil
  "Working copy of toolbar being edited.")

(defvar edit-toolbar-fallback-toolbar nil
  "Toolbar definition to use when reverting.")

(defvar edit-toolbar-file-name
  (cond
   ((boundp 'emacs-user-extension-dir)
    (concat (file-name-as-directory (concat "~" emacs-user-extension-dir))
	    ".toolbar"))
   ;; if `load-user-init-file-p' exists, `user-init-directory' is absolute
   ((boundp 'load-user-init-file-p)
    (paths-construct-path (list user-init-directory ".toolbar")))
   (t
    (paths-construct-path "~" ".toolbar")))
  "File name to save toolbars to.  Defaults to \"~/.xemacs/.toolbar\"")

(defvar edit-toolbar-button-prefix "edit-toolbar-button"
  "Prefix to use when naming new buttons created by edit-toolbar.
The new buttons will be stored in the file named by edit-toolbar-file-name")

(defvar edit-toolbar-added-buttons-alist nil
  "Buttons added by edit-toolbar.
A list of cons cells.  The car is the variable which stores the glyph data.
The cdr is a list of filenames to be passed as arguments to 
toolbar-make-button-list when the toolbar file is read at startup.")

(defvar edit-toolbar-menu
  '("Edit Toolbar"
    ["Move This Item Up" edit-toolbar-up (>= (edit-toolbar-current-index) 0)]
    ["Move This Item Down" edit-toolbar-down (>= (edit-toolbar-current-index) 0)]
    ["Set Function" edit-toolbar-set-function (edit-toolbar-button-p)]
    ["Set Help String" edit-toolbar-set-help (edit-toolbar-button-p)]
    ["Copy This Button" edit-toolbar-copy (edit-toolbar-button-p)]
    ["Remove This Item" edit-toolbar-kill (>= (edit-toolbar-current-index) 0)]
    "----"
    ["Add Button..." edit-toolbar-add-button t]
    ("Add Separator"
     ["2D (narrow)" edit-toolbar-add-separator-2D-narrow t]
     ["3D (narrow)" edit-toolbar-add-separator-3D-narrow t]
     ["2D (wide)" edit-toolbar-add-separator-2D-wide t]
     ["3D (wide)" edit-toolbar-add-separator-3D-wide t]
     ["Right/left divider" edit-toolbar-add-separator-right-left t]
     )
    "----"
    ["Restore Default Toolbar      " edit-toolbar-restore (buffer-modified-p)]
    ["Save This Toolbar" edit-toolbar-save (buffer-modified-p)]
    "----"
    ["Help" describe-mode t]
    "----"
    ["Quit" edit-toolbar-quit t]
    )
  )

(defvar edit-toolbar-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'edit-toolbar-quit)
    (define-key map "n" 'edit-toolbar-next)
    (define-key map "p" 'edit-toolbar-previous)
    (define-key map " " 'edit-toolbar-next)
    (define-key map "?" 'describe-mode)
    (define-key map "f" 'edit-toolbar-set-function)
    (define-key map "h" 'edit-toolbar-set-help)
    (define-key map "a" 'edit-toolbar-add-button)
    (define-key map "2" 'edit-toolbar-add-separator-2D-narrow)
    (define-key map "@" 'edit-toolbar-add-separator-2D-wide)
    (define-key map "3" 'edit-toolbar-add-separator-3D-narrow)
    (define-key map "#" 'edit-toolbar-add-separator-3D-wide)
    (define-key map "R" 'edit-toolbar-add-separator-right-left)
    (define-key map "c" 'edit-toolbar-copy)
    (define-key map "d" 'edit-toolbar-down)
    (define-key map "u" 'edit-toolbar-up)
    (define-key map "k" 'edit-toolbar-kill)
    (define-key map "s" 'edit-toolbar-save)
    (define-key map "\C-x\C-s" 'edit-toolbar-save)
    (define-key map "r" 'edit-toolbar-restore)
    (define-key map 'return 'edit-toolbar-next)
    (define-key map 'delete 'edit-toolbar-previous)
    map
    ))

(defun edit-toolbar-create-toolbar-alist ()
  (setq edit-toolbar-toolbar-alist nil)
  (mapatoms
   (lambda (sym)
     (if (and (boundp sym)
              (toolbar-specifier-p (symbol-value sym))
              (not (string-match "^edit-toolbar" (symbol-name sym))))
         (setq edit-toolbar-toolbar-alist
               (cons (cons (symbol-name sym) sym)
                     edit-toolbar-toolbar-alist))))))
 
;;;###autoload
(defun edit-toolbar (&optional toolbar)
  "Alter toolbar characteristics by editing a buffer representing the specified toolbar.
Pops up a buffer containing a list of the toolbar matching TOOLBAR_NAME."
  (interactive)
  (edit-toolbar-create-toolbar-alist)
  (if (eq toolbar nil)
      (setq toolbar (intern-soft
                     (completing-read
                      "Toolbar: " edit-toolbar-toolbar-alist))))
  (if (not (toolbar-specifier-p (symbol-value toolbar)))
      (error (format "Toolbar named %s not found" (prin1 toolbar))))
  (pop-to-buffer (get-buffer-create "*Edit Toolbar*"))
  (setq edit-toolbar-temp-toolbar (symbol-value toolbar))
  (setq edit-toolbar-temp-toolbar-name (symbol-name toolbar))
  (setq edit-toolbar-fallback-toolbar
                 (specifier-instance (symbol-value toolbar)))
  (edit-toolbar-create-button-alist)
  (edit-toolbar-list)
  (set-buffer-modified-p nil)
  (edit-toolbar-mode)
  (set-face-foreground 'default "black" (current-buffer))
  (set-face-background 'default "grey75" (current-buffer))
  (set-face-background-pixmap 'default "nil" (current-buffer))
  (set-face-foreground 'list-mode-item-selected "yellow" (current-buffer))
  (set-face-background 'list-mode-item-selected "black" (current-buffer)))

(define-derived-mode edit-toolbar-mode list-mode "Edit-Toolbar"
  "Major mode for 'edit-toolbar' buffers.

Editing commands:

\\{edit-toolbar-map}"
  (setq mode-popup-menu edit-toolbar-menu)
  (if (and (featurep 'menubar))
      current-menubar
      (progn
	(set (make-local-variable 'current-menubar)
	     (copy-sequence current-menubar))
	(add-submenu nil edit-toolbar-menu)))
  (use-local-map edit-toolbar-map)
  (setq buffer-read-only nil)
  (message "Edit Toolbar Version %s.  Type \"?\" for help." edit-toolbar-version))

(defun edit-toolbar-list ()
  (erase-buffer)
  (edit-toolbar-insert-item 'header)
  (mapcar (function (lambda (item)
                      (edit-toolbar-insert-item item)))
          (specifier-instance edit-toolbar-temp-toolbar))
  (goto-char (point-min)))

(defun edit-toolbar-button-p ()
  "Returns t if the currently selected edit-toolbar item is a button."
  (let ((item (edit-toolbar-current-item)))
    (not (or (eq item nil)
             (eq (aref item 0) :style)
             (eq (aref item 0) :size)))))

(defun edit-toolbar-current-index ()
  "Returns the offset of the currently selected edit-toolbar item." 
  (- (count-lines (point-min) (point)) 2))

(defun edit-toolbar-current-item ()
  "Returns the value of the currently selected edit-toolbar item." 
  (let ((toolbar (specifier-instance edit-toolbar-temp-toolbar)))
    (nth (edit-toolbar-current-index) toolbar)))

(defun edit-toolbar-quit ()
  "Quit an Edit Toolbar session.  This simply buries the buffer."
  (interactive)
  ;;FIXME
  (bury-buffer))

(defun edit-toolbar-next ()
  "Move to the next line in the Edit Toolbar buffer."
  (interactive)
  (next-line 1))

(defun edit-toolbar-previous ()
  "Move to the previous line in the Edit Toolbar buffer."
  (interactive)
  (next-line -1))

(defun edit-toolbar-set-function (func)
  "Set the function for the selected toolbar button."
  (interactive "aNew Function: ")
  (let ((index (edit-toolbar-current-index)))
    (if (not (edit-toolbar-button-p))
        (error "Not a button")
      (setf (aref (edit-toolbar-current-item) 1) func)
      (edit-toolbar-list)
      (forward-line (+ index 2)))))

(defun edit-toolbar-set-help (help)
  "Set the help string for the selected toolbar button."
  (interactive "sNew Help String: ")
  (let ((index (edit-toolbar-current-index)))
    (if (not (edit-toolbar-button-p))
        (error "Not a button")
      (setf (aref (edit-toolbar-current-item) 3) help)
      (edit-toolbar-list)
      (forward-line (+ index 2)))))

(defun edit-toolbar-copy ()
  "Make a copy of the selected toolbar button."
  (interactive)
  (let ((index (edit-toolbar-current-index)))
    (if (not (edit-toolbar-button-p))
        (error "Not a button")
      (setcdr (nthcdr index (specifier-instance edit-toolbar-temp-toolbar))
              (cons (edit-toolbar-current-item)
                    (nthcdr (1+ index)
                            (specifier-instance edit-toolbar-temp-toolbar))))
      (edit-toolbar-list)
      (forward-line (+ index 3)))))

(defun edit-toolbar-down ()
  "Move the current toolbar button down (right) one position."
  (interactive)
  (let* ((toolbar (specifier-instance edit-toolbar-temp-toolbar))
	 (index (- (count-lines (point-min) (point)) 2))
         (item (nth index toolbar)))
    (if (eq (1+ index) (length toolbar))
	(error "Already at the bottom of the toolbar."))
    (if (eq index 0)
	(setq toolbar (cdr toolbar))
      (setcdr (nthcdr (1- index) toolbar)
	      (nthcdr (1+ index) toolbar)))
    (setcdr (nthcdr index toolbar)
	    (cons item (nthcdr (1+ index) toolbar)))
    (set-specifier
     (symbol-value (intern-soft edit-toolbar-temp-toolbar-name)) toolbar)
    (edit-toolbar-list)
    (forward-line (+ index 3))))

(defun edit-toolbar-up ()
  "Move the current toolbar button up (left) one position."
  (interactive)
  (let* ((toolbar (specifier-instance edit-toolbar-temp-toolbar))
	 (index (- (count-lines (point-min) (point)) 2))
	 (item (nth index toolbar)))
    (if (<= index 0)
	(error "Already at the top of the toolbar."))
    (setcdr (nthcdr (1- index) toolbar)
	    (nthcdr (1+ index) toolbar))
    (if (eq index 1)
	(setq toolbar (cons item toolbar))
      (setcdr (nthcdr (- index 2) toolbar)
	      (cons item (nthcdr (- index 1) toolbar))))
    (set-specifier
     (symbol-value (intern-soft edit-toolbar-temp-toolbar-name)) toolbar)
    (edit-toolbar-list)
    (forward-line (+ index 1))))

(defun edit-toolbar-kill ()
  "Remove the current toolbar button."
  (interactive)
  (let* ((toolbar (specifier-instance edit-toolbar-temp-toolbar))
	 (index (- (count-lines (point-min) (point)) 2))
	 (etk-scratch-list)
	 (button (elt (nth index toolbar) 0 )))
    
    (mapcar
     (lambda (cons)
       (if (not (memq button cons))
	   (setq etk-scratch-list (append etk-scratch-list cons)))
       )
     edit-toolbar-added-buttons-alist)
    (setq edit-toolbar-added-buttons-alist etk-scratch-list)
    (if (eq index 0)
	(setq toolbar (cdr toolbar))
      (setcdr (nthcdr (1- index) toolbar)
	      (nthcdr (1+ index) toolbar)))
    (set-specifier
     (symbol-value (intern-soft edit-toolbar-temp-toolbar-name)) toolbar)
    (edit-toolbar-list)
    (forward-line (+ index 2))))

(defun edit-toolbar-insert-item (item)
  (let ((line-format "%-30s %s\n")
	icon function help)
    (if (eq item 'header)
	(progn
	  (setq function "Function"
		help "Help String")
	  (insert-face "Icon\t" 'bold)
	  (insert-face (format line-format function help) 'bold))
      (cond ((eq item nil)
             (setq icon nil
                   function "-------------- Right/Left Divider --------------"
                   help ""))
	    ((or (eq (aref item 0) :style)
		 (eq (aref item 0) :size))
	     (setq icon nil
		   function "----------------------------------------"
		   help ""))
	    (t
	     (setq icon (if (listp (aref item 0))
			    (car (aref item 0))
			  (car (symbol-value (aref item 0))))
		   function (aref item 1)
		   help (aref item 3))))
      (let ((st (point))
	    (fn #'(lambda (str callback data)
		    (let ((st1 (point)))
		      (insert str)
		      (add-list-mode-item st1 (point) nil callback data)))))
	(insert "\t")
	(funcall fn (format line-format function help) nil item)
	(set-extent-begin-glyph (make-extent st (point)) icon)))))

(defun edit-toolbar-create-button-alist ()
  (let ((button-alist nil)
	(buttons (specifier-instance edit-toolbar-temp-toolbar)))
    (while buttons
      (setq button-alist
	    (if (arrayp (car buttons))
		(cons (cons (symbol-name (aref (car buttons) 1)) (car buttons))
		      button-alist)
	      (cons (car buttons) button-alist)))
      (setq buttons (cdr buttons)))
    button-alist))

(defvar edit-toolbar-button-alist nil
  "List of buttons in the toolbar currently being edited.")

(defvar edit-toolbar-toolbar-alist nil
  "List of existing toolbars (used for completing read).")

(defun edit-toolbar-add-item (item)
  "Add a toolbar item ITEM at the current location."
  (let* ((toolbar (specifier-instance edit-toolbar-temp-toolbar))
	 (index (- (count-lines (point-min) (point)) 2)))
    (if (<= index 0)
	(setq toolbar (cons item toolbar))
      (setcdr (nthcdr (- index 1) toolbar)
	      (cons item (nthcdr index toolbar))))
    (set-specifier
     (symbol-value (intern-soft edit-toolbar-temp-toolbar-name)) toolbar)
    (edit-toolbar-list)
    (forward-line (+ index 2))))

;(defun edit-toolbar-check-for-save ()
;  (if (not (buffer-modified-p))
;      ()
;    (if (yes-or-no-p-maybe-dialog-box "

(defun edit-toolbar-restore ()
  "Restore the default toolbar."
  (interactive)
;  (edit-toolbar-check-for-save)
  (set-specifier edit-toolbar-temp-toolbar
                 edit-toolbar-fallback-toolbar)
  (edit-toolbar-list)
  (set-buffer-modified-p nil))
  
(defun edit-toolbar-add-separator-2D-narrow ()
  "Add a narrow 2D separator at the current position."
  (interactive)
  (edit-toolbar-add-item [:style 2D]))

(defun edit-toolbar-add-separator-3D-narrow ()
  "Add a narrow 3D separator at the current position."
  (interactive)
  (edit-toolbar-add-item [:style 3D]))

(defun edit-toolbar-add-separator-2D-wide ()
  "Add a wide 2D separator at the current position."
  (interactive)
  (edit-toolbar-add-item [:style 2D :size 30]))

(defun edit-toolbar-add-separator-3D-wide ()
  "Add a wide 3D separator at the current position."
  (interactive)
  (edit-toolbar-add-item [:style 3D :size 30]))

(defun edit-toolbar-add-separator-right-left ()
  "Add a right/left separator at the current position."
  (interactive)
  (if (memq nil (specifier-instance edit-toolbar-temp-toolbar))
      (error "Can't have more than one left/right divider in a toolbar.")
    (edit-toolbar-add-item nil)))

(defun edit-toolbar-add-button ()
  "Add a new toolbar item at the current position.
Completion is available to the known toolbar buttons."
  (interactive)
  (let ((button (completing-read
		 "New Toolbar Button (RET to create a new button): "
		 edit-toolbar-button-alist nil t)))
    (if (string-equal button "")
	(let ((prompts '("UP glyph for button: "
			 "DOWN glyph (RET for no glyph): "
			 "DISABLED glyph (RET for no glyph): "
			 "UP CAPTIONED glyph (RET for no glyph): "
			 "DOWN CAPTIONED glyph (RET for no glyph): "
			 "DISABLED CAPTIONED glyph (RET for no glyph): "))
	      (glyphs-list nil)
	      (count 0))
	  (let ((glyph-file (read-file-name (car prompts) nil "")))
	    (if (string-equal glyph-file "")
		(error "You must specify at least the UP glyph.")
	      (setq glyphs-list (list  glyph-file))
	      (setq prompts (cdr prompts))))
	  (while prompts
	    (let ((glyph-file (read-file-name (car prompts) nil "")))
	      (if (not (string-equal glyph-file ""))
		  (setq glyphs-list
			(append glyphs-list (list  glyph-file)))))
	    (setq prompts (cdr prompts)))
	  (setq added-button (gentemp edit-toolbar-button-prefix ))
	  (setf (symbol-value added-button) 
		(toolbar-make-button-list glyphs-list))
	  (setq edit-toolbar-added-buttons-alist 
		(append edit-toolbar-added-buttons-alist 
		(list	(cons added-button glyphs-list))))
	  (let ((func (read-string "Function to call: "))
		(help (read-string "Help String: ")))
	    (setq new-button (vector added-button  (intern func) t help))))
      (let ((match (assoc button edit-toolbar-button-alist)))
	(if match
	    (setq new-button (cdr match))
	  (error "Can't find button %s" button))))
    (edit-toolbar-add-item new-button)))

(defun edit-toolbar-prompt-for-initialization ()
  (popup-dialog-box
   '("Edit Toolbar has created the file ~/.xemacs/.toolbar

In order for your changes to take effect the next time
you start XEmacs, you need to add the following line
to the end of your .emacs file:

    (load \"~/.xemacs/.toolbar\")

Alternatively, I can do this for you now."
     ["Yes, please\nadd the line\nof code for me." edit-toolbar-add-initialization t]
     nil
     ["No thanks,\nI'll take care\nof it myself." ignore t])))

(defun edit-toolbar-add-initialization ()
  "Add a line to the end of the user's init file for edit-toolbar use."
  (interactive)
  (set-buffer (find-file-noselect user-init-file))
  (goto-char (point-max))
  (insert "
(if (and (featurep 'toolbar)
	 (fboundp 'console-on-window-system-p)
	 (console-on-window-system-p)
	 (file-exists-p \"" edit-toolbar-file-name "\"))
    (load-file (expand-file-name \"" edit-toolbar-file-name "\")))
")
  (save-buffer))

(defun edit-toolbar-save ()
  "Save the current toolbar in the file specified by edit-toolbar-file-name."
  (interactive)
  (save-excursion
    (let* ((exists (file-exists-p edit-toolbar-file-name))
	   (buf (find-file-noselect edit-toolbar-file-name))
	   (standard-output buf))
      (set-buffer buf)
      (erase-buffer)
      (insert "(setq edit-toolbar-added-buttons-alist '")
      (prin1 edit-toolbar-added-buttons-alist)
      (insert ")\n")
      (insert "(mapcar
  (lambda (cons)
         (setf (symbol-value (car cons)) (toolbar-make-button-list (cdr cons)))
      )
 edit-toolbar-added-buttons-alist)\n")
      (insert (concat "(set-specifier " edit-toolbar-temp-toolbar-name) " '")
      (prin1 (specifier-instance edit-toolbar-temp-toolbar))
      (insert ")")
      (save-buffer)
      (kill-buffer (current-buffer))
      (or exists (edit-toolbar-prompt-for-initialization))))
  (set-buffer-modified-p nil))

(provide 'edit-toolbar)

;;; edit-toolbar.el ends here
