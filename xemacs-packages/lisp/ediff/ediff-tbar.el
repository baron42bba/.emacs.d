;;; ediff-tbar.el --- A toolbar for Ediff control buffer

;; Copyright (C) 1996, 97, 2002, 2003, 2004, 2005,
;; 2006 Free Software Foundation, Inc.

;; Author: Marc Paquette <marcpa@cam.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:
	 

;; compiler pacifier
(eval-when-compile
  (let ((load-path (cons (expand-file-name ".") load-path)))
    (or (featurep 'ediff-init)
	(load "ediff-init.el" nil nil 'nosuffix))
    (or (featurep 'ediff-util)
	(load "ediff-util.el" nil nil 'nosuffix))
    ))
(defvar toolbar-icon-directory)
;; end pacifier

(require 'ediff-init)

(defvar ediff-toolbar-icon-directory
  (if (and ediff-xemacs-p (featurep 'toolbar))
      (let ((dir (locate-data-directory "ediff")))
	(if (stringp dir)
	    dir
	  toolbar-icon-directory))
    "Where the Ediff toolbar icons are."))

(defvar ediff-use-toolbar-p nil
  "If t, Ediff will use a toolbar for the control frame.
This has an effect only if your emacs supports Toolbars.
Currently, XEmacs does, but not Emacs.
Do not change the value of this variable interactively.
This should be done only via the menu bar or by executing
`ediff-toggle-use-toolbar'.")

(defcustom ediff-use-toolbar-p nil
  "*If t, Ediff will use a toolbar for the control frame.
This has an effect only if your emacs supports Toolbars.
Currently, XEmacs does, but Emacs doesn't.
This variable can be changed only via the menu bar or
through the Emacs customization widget."
  :type 'boolean
  :set (lambda (widget value)
	 (or (eq value ediff-use-toolbar-p)
	     (ediff-toggle-use-toolbar)))
  :group 'ediff)

(defvar ediff-toolbar-height 21
  "The height of the Ediff toolbar.
The value must match the actual size of the toolbar icons.")

(defvar ediff-toolbar-width 200.0
  "The width of the Ediff toolbar.
The value must match the actual width of the toolbar.
Here's an example:
    There are 10 buttons, each 15 pixels large, and the shadows occupy 2
   pixels each side, and the last button is right-justified (so we reserve
   about 30 pixels for fill space) = 200 pixels.")


;; Here the toolbar width is not the same width talked about in XEmacs
;; lispref info documentation : it is the minimal width needed by
;; ediff's toolbar to display all buttons, for an horizontal toolbar.
;; Ideally, we would query the toolbar for the width of each button
;; and add them, but I didn't find query functions in the doc on
;; toolbars.  Therefore, I use a static number of pixels that should
;; be adjusted if the toolbar gets more or loses some buttons. --marcpa
(defun ediff-compute-toolbar-width ()
  (if (not (ediff-use-toolbar-p))
      0
    (ceiling (/ ediff-toolbar-width (font-instance-width (face-font-instance 'default))))))

(defvar ediff-toolbar-next-icon
   (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name
	(if (featurep 'xpm) "ediff-next.xpm" "ediff-next.xbm")
	ediff-toolbar-icon-directory)))
  "Next difference icon in toolbar.")

(defvar ediff-toolbar-previous-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name
	(if (featurep 'xpm) "ediff-prev.xpm" "ediff-prev.xbm")
	ediff-toolbar-icon-directory)))
  "Previous difference icon in toolbar.")

(defvar ediff-toolbar-A-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name
	;; UP
	(if (featurep 'xpm) "ediff-A-up.xpm" "ediff-A-up.xbm")
	ediff-toolbar-icon-directory)
       (expand-file-name
	;; DOWN
	(if (featurep 'xpm) "ediff-A-up.xpm" "ediff-A-up.xbm")
	ediff-toolbar-icon-directory)
       (expand-file-name
	;; DISABLED
	(if (featurep 'xpm) "ediff-A-xx.xpm" "ediff-A-up.xbm")
	ediff-toolbar-icon-directory)
       ))
  "Select diff A icon in toolbar.")

(defvar ediff-toolbar-B-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name
	;; UP
	(if (featurep 'xpm) "ediff-B-up.xpm" "ediff-B-up.xbm")
	ediff-toolbar-icon-directory)
       (expand-file-name
	;; DOWN
	(if (featurep 'xpm) "ediff-B-up.xpm" "ediff-B-up.xbm")
	ediff-toolbar-icon-directory)
       (expand-file-name
	;; DISABLED
	(if (featurep 'xpm) "ediff-B-xx.xpm" "ediff-B-up.xbm")
	ediff-toolbar-icon-directory)
       ))
  "Select diff B icon in toolbar.")

(defvar ediff-toolbar-toggle-split-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name
	;; UP
	(if (featurep 'xpm)
	    "ediff-toggle-split-up.xpm" "ediff-toggle-split-up.xbm")
	ediff-toolbar-icon-directory)
       ))
  "Toggle split mode between side-to-side and one-on-top-of-another.")

(defvar ediff-toolbar-save-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name
	;; UP
	(if (featurep 'xpm) "ediff-save.xpm" "ediff-save.xbm")
	ediff-toolbar-icon-directory)
       (expand-file-name
	;; DOWN
	(if (featurep 'xpm) "ediff-save.xpm" "ediff-save.xbm")
	ediff-toolbar-icon-directory)
       (expand-file-name
	;; DISABLED
	(if (featurep 'xpm) "ediff-save-xx.xpm" "ediff-save-xx.xbm")
	ediff-toolbar-icon-directory)
       ))
  "Save merge buffer.")

(defvar ediff-toolbar-quit-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name
	(if (featurep 'xpm) "ediff-quit.xpm" "ediff-quit.xbm")
	ediff-toolbar-icon-directory)))
  "Exit Ediff session.")

(defvar ediff-toolbar-help-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name
	(if (featurep 'xpm) "ediff-help.xpm" "ediff-help.xbm")
	ediff-toolbar-icon-directory)))
  "Show Ediff help.")

(defvar ediff-toolbar-refresh-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name
	(if (featurep 'xpm) "ediff-update.xpm" "ediff-update.xbm")
	ediff-toolbar-icon-directory)))
  "Refresh Ediff display (aka recenter).")

(defvar ediff-toolbar-refine-icon
  (if (featurep 'toolbar)
      (toolbar-make-button-list
       (expand-file-name
	;; UP
	(if (featurep 'xpm) "ediff-refine.xpm" "ediff-refine.xbm")
	ediff-toolbar-icon-directory)
       ))
  "Refine current difference region by computing fine diffs.")

(defun ediff-toolbar-previous-difference ()
  (interactive)
  (let ((ediff-grab-mouse nil))
    (ediff-previous-difference 1)))

(defun ediff-toolbar-next-difference ()
  (interactive)
  (let ((ediff-grab-mouse nil))
    (ediff-next-difference 1)))

(defun ediff-toolbar-select/copy-A ()
  (interactive)
  (let ((ediff-grab-mouse nil))
    (cond ((or (ediff-merge-job)
	       (ediff-merge-with-ancestor-job))
	   (ediff-copy-A-to-C nil))
	  (t 
	   (ediff-copy-A-to-B nil)))))

(defun ediff-toolbar-select/copy-B ()
  (interactive)
  (let ((ediff-grab-mouse nil))
    (cond ((or (ediff-merge-job)
	       (ediff-merge-with-ancestor-job))
	   (ediff-copy-B-to-C nil))
	  (t 
	   (ediff-copy-B-to-A nil)))))

(defun ediff-toolbar-toggle-split ()
  (interactive)
  (let ((ediff-grab-mouse nil))
    (ediff-toggle-split)))


(defun ediff-toolbar-save ()
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (if (ediff-merge-job)
      (ediff-maybe-save-and-delete-merge 'save-and-continue)
    ;; 2-way or 3-way compare: save modified buffers
    (mapcar (lambda (type)
	      (let ((ebuf (ediff-get-buffer type)))
		(and (ediff-buffer-live-p ebuf)
		     (ediff-with-current-buffer ebuf
		       (and (buffer-modified-p)
			    (save-buffer))))))
	    '(A B C))))


(defun ediff-toolbar-quit ()
  (interactive)
  (let ((ediff-grab-mouse nil))
    (ediff-quit nil)))

(defun ediff-toolbar-help ()
  (interactive)
  (ediff-toggle-help))

(defun ediff-toolbar-refresh ()
  "Recenter"
  (interactive)
  (let ((ediff-grab-mouse nil))
    (ediff-recenter)))

(defun ediff-toolbar-refine ()
  "Refine current difference region by computing fine diffs."
  (interactive)
  (let ((ediff-grab-mouse nil))
    (ediff-make-or-kill-fine-diffs 'make-them)))

(defun ediff-toolbar-refine-needed-p ()
  (and (> ediff-current-difference 0)
       (> ediff-auto-refine-limit
	  (- (ediff-get-diff-posn 'A 'end ediff-current-difference)
	     (ediff-get-diff-posn 'A 'beg ediff-current-difference)))))

(defvar ediff-toolbar
  (if (featurep 'toolbar)
      '([ediff-toolbar-refine-icon
	 ediff-toolbar-refine
	 t
	 "Refine current difference region by computing fine diffs."]
	[ediff-toolbar-previous-icon
	 ediff-toolbar-previous-difference
	 t
	 "Go to the previous difference."]
	[ediff-toolbar-next-icon
	 ediff-toolbar-next-difference
	 t
	 "Advance to the next difference."]
	[ediff-toolbar-A-icon
	 ediff-toolbar-select/copy-A
	 (not (ediff-3way-comparison-job))
	 "Select/Copy difference A."]
	[ediff-toolbar-B-icon
	 ediff-toolbar-select/copy-B
	 (not (ediff-3way-comparison-job))
	 "Select/Copy difference B."]
	[ediff-toolbar-save-icon
	 ediff-toolbar-save
	 t
	 "Save buffers modified in this session."]
	[ediff-toolbar-refresh-icon
	 ediff-toolbar-refresh
	 t
	 "Refresh Ediff display (aka recenter)."]
 	[ediff-toolbar-toggle-split-icon
	  ediff-toolbar-toggle-split
	  t
	 "Toggle split mode between side-to-side and one-on-top-of-another."]
	[ediff-toolbar-help-icon
	 ediff-toolbar-help
	 t
	 "Toggle short/long help."]
	nil
 	[ediff-toolbar-quit-icon
 	 ediff-toolbar-quit
 	 t
 	 "Quit this ediff session."]
	)))

(defvar ediff-toolbar-3way
  (if (featurep 'toolbar)
      '([ediff-toolbar-refine-icon
	 ediff-toolbar-refine
	 t
	 "Refine current difference region by computing fine diffs."]
	[ediff-toolbar-previous-icon
	 ediff-toolbar-previous-difference
	 t
	 "Go to the previous difference."]
	[ediff-toolbar-next-icon
	 ediff-toolbar-next-difference
	 t
	 "Advance to the next difference."]
	[ediff-toolbar-save-icon
	 ediff-toolbar-save
	 t
	 "Save buffers modified in this session."]
	[ediff-toolbar-refresh-icon
	 ediff-toolbar-refresh
	 t
	 "Refresh Ediff display (aka recenter)."]
 	[ediff-toolbar-toggle-split-icon
	  ediff-toolbar-toggle-split
	  t
	 "Toggle split mode between side-to-side and one-on-top-of-another."]
	[ediff-toolbar-help-icon
	 ediff-toolbar-help
	 t
	 "Toggle short/long help."]
	nil
 	[ediff-toolbar-quit-icon
 	 ediff-toolbar-quit
 	 t
 	 "Quit this ediff session."]
	)))


(provide 'ediff-tbar)


;;; Local Variables:
;;; eval: (put 'ediff-defvar-local 'lisp-indent-hook 'defun)
;;; eval: (put 'ediff-with-current-buffer 'lisp-indent-hook 1)
;;; eval: (put 'ediff-with-current-buffer 'edebug-form-spec '(form body))
;;; End:

;;; ediff-tbar.el ends here
