;;; floating-toolbar.el -- popup toolbar support for XEmacs.
;; Copyright (C) 1997 Kyle E. Jones

;; Author: Kyle Jones <kyle_jones@wonderworks.com>
;; Keywords: lisp

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Popup toolbar for XEmacs (probably require XEmacs 19.14 or later)
;; Send bug reports to kyle_jones@wonderworks.com

;; The command `floating-toolbar' pops up a small frame
;; containing a toolbar.  The command should be bound to a
;; button-press event.  If the mouse press happens over an
;; extent that has a non-nil 'floating-toolbar property, the
;; value of that property is the toolbar instantiator that will
;; be displayed.  Otherwise the toolbar displayed is taken from
;; the variable `floating-toolbar'.  This variable can be made
;; buffer local to produce buffer local floating toolbars.
;;
;; `floating-toolbar-or-popup-mode-menu' works like `floating-toolbar'
;; except that if no toolbar is found, `popup-mode-menu' is called.
;;
;; `floating-toolbar-from-extent-or-popup-mode-menu' works like
;; `floating-toolbar-or-popup-mode-menu' except only extent local
;; toolbars are used; the value of floating-toolbar is not used.
;;
;; Installation:
;;
;; Byte-compile the file floating-toolbar.el (with M-x byte-compile-file)
;; and put the .elc file in a directory in your load-path.  Add the
;; following line to your .emacs:
;;
;; (require 'floating-toolbar)
;;
;; You will also need to bind a mouse click to `floating-toolbar' or to
;; `floating-toolbar-or-popup-mode-menu'.
;; 
;; For 19.12 users:
;;    If you are using fvwm, [tv]twm or ol[v]wm, you can also add
;;    the following lines to various configuration file to use
;;    minimal decorations on the toolbar frame.
;;
;;    In .emacs:
;;       (setq floating-toolbar-frame-name "floating-toolbar")
;;
;;    For ol[v]wm use this in .Xdefaults:
;;       olvwm.NoDecor: floating-toolbar
;;         or
;;       olwm.MinimalDecor: floating-toolbar
;;
;;    For fvvm use this in your .fvwmrc:
;;       NoTitle floating-toolbar
;;    or
;;       Style "floating-toolbar" NoTitle, NoHandles, BorderWidth 0
;;
;;    For twm use this in your .twmrc:
;;       NoTitle { "floating-toolbar" }
;; 
;; Under 19.13 and later versions the floating-toolbar frame uses a
;; transient window that is not normally decorated by window
;; managers.  So the window manager directives should not be
;; needed for XEmacs 19.13 and beyond.

;;; Code:

(provide 'floating-toolbar)

;; (require 'toolbar)

(defvar floating-toolbar-version "1.02"
  "Version string for the floating-toolbar package.")

(defvar floating-toolbar-use-sound nil
  "*Non-nil value means play a sound to herald the appearance
and disappearance of the floating toolbar.

`floating-toolbar-appears' will be played when the toolbar appears.
`floating-toolbar-disappears' will be played when the toolbar disappears.

See the documentation for the function `load-sound-file' to see how
define sounds.")

(defvar floating-toolbar nil
  "*Toolbar instantiator used if mouse event is not over an extent
with a non-nil 'floating-toolbar property.  This variable can be
made local to a buffer to have buffer local floating toolbars.")

(defvar floating-toolbar-help-font nil
  "*Non-nil value should be a font to be used to display toolbar help
messages.  The floating toolbar frame will have a minibuffer window
so that it can display any help text that is attached to the toolbar
buttons.")

(defvar floating-toolbar-frame-name nil
  "*The frame name for the frame used to display the floating toolbar.")

;;;
;;; End of user variables.
;;;

(defvar floating-toolbar-frame nil
  "The floating toolbar is displayed in this frame.")

(defvar floating-toolbar-display-pending nil
  "Non-nil value means the toolbar frame will be visible as soon
as the X server gets around to displaying it.  Nil means it
will be invisible as soon as the X server decides to hide it.")

(defun floating-toolbar-displayed ()
  (and (frame-live-p floating-toolbar-frame)
       (frame-visible-p floating-toolbar-frame)))

;;;###autoload
(defun floating-toolbar (event &optional extent-local-only)
  "Popup a toolbar near the current mouse position.
The toolbar instantiator used is taken from the 'floating-toolbar
property of any extent under the mouse.  If no such non-nil
property exists for any extent under the mouse, then the value of the
variable `floating-toolbar' is checked.  If its value is nil, then
no toolbar will be displayed.

This command should be bound to a button press event.

When called from a program, first arg EVENT should be the button
press event.  Optional second arg EXTENT-LOCAL-ONLY specifies
that only extent local toolbars should be used; this means the
`floating-toolbar' variable will not be consulted."
  (interactive "_e")
  (unless (featurep 'toolbar)
    (error "Floating toolbar requires built in toolbar support."))
  (if (not (mouse-event-p event))
      nil
    (let* ((buffer (event-buffer event))
	   (window (event-window event))
	   (frame (event-frame event))
	   (point (and buffer (event-point event)))
	   (glyph-extent (event-glyph-extent event))
	   (glyph-extent (if (and glyph-extent
				  (extent-property glyph-extent
						   'floating-toolbar))
			     glyph-extent))
	   (extent (or glyph-extent
		       (and point
			    (extent-at point buffer 'floating-toolbar))))
	   (toolbar (or (and extent (get extent 'floating-toolbar))
			(and (not extent-local-only)
			     (symbol-value-in-buffer 'floating-toolbar
						     buffer nil))))
	   (x nil)
	   (y nil)
	   (echo-keystrokes 0)
	   (awaiting-release t)
	   (done nil))
      (if (not (consp toolbar))
	  nil
	;; event-[xy]-pixel are relative to the top left corner
	;; of the frame.  The presence of top and left toolbar
	;; and the menubar can move this position down and
	;; leftward, but XEmacs doesn't compensate for this in
	;; the values returned.  So we do it here, as best we
	;; can.
	(let* ((params (frame-parameters frame))
	       (top (cdr (assq 'top params)))
	       (left (cdr (assq 'left params)))
	       (xtop-toolbar-height
		(if (specifier-instance top-toolbar)
		    (specifier-instance top-toolbar-height)
		  0))
	       (xleft-toolbar-width
		(if (specifier-instance left-toolbar)
		    (specifier-instance left-toolbar-width)
		  0))
	       ;; better than nothing
	       (menubar-height (if (and (featurep 'menubar)
					current-menubar) 22 0)))
	  (setq x (+ left xleft-toolbar-width (event-x-pixel event))
		y (+ top xtop-toolbar-height menubar-height
		     (event-y-pixel event))))
	;; for toolbar spec buffer local variable values
	(and buffer (set-buffer buffer))
	(floating-toolbar-display-toolbar toolbar x y)
	(while (not done)
	  (setq event (next-command-event))
	  (cond ((and awaiting-release (button-release-event-p event))
		 (setq awaiting-release nil))
		((and (button-release-event-p event)
		      (event-over-toolbar-p event)
		      (eq floating-toolbar-frame (event-frame event)))
		 (floating-toolbar-undisplay-toolbar)
		 (and window (select-frame (window-frame window)))
		 (and window (select-window window))
		 (dispatch-event event)
		 (setq done t))
		((and (button-press-event-p event)
		      (event-over-toolbar-p event)
		      (eq floating-toolbar-frame (event-frame event)))
		 (setq awaiting-release nil)
		 (dispatch-event event))
		(t
		 ;; push back the event if it was in another frame.
		 ;; eat it if it was in the toolbar frame.
		 (if (and (event-frame event)
			  (not (eq floating-toolbar-frame
				   (event-frame event))))
		     (setq unread-command-events
			   (cons event unread-command-events)))
		 (floating-toolbar-undisplay-toolbar)
		 (setq done t))))
	t ))))

;;;###autoload
(defun floating-toolbar-or-popup-mode-menu (event)
  "Like floating-toolbar, but if no toolbar is displayed
run popup-mode-menu."
  (interactive "_e")
  (or (floating-toolbar event) (popup-mode-menu)))

;;;###autoload
(defun floating-toolbar-from-extent-or-popup-mode-menu (event)
  "Like floating-toolbar-or-popup-mode-menu, but search only for an
extent local toolbar."
  (interactive "_e")
  (or (floating-toolbar event t) (popup-mode-menu)))

(defun floating-toolbar-display-toolbar (toolbar x y)
  (if (not (frame-live-p floating-toolbar-frame))
      (setq floating-toolbar-frame (floating-toolbar-make-toolbar-frame x y)))
  (set-specifier top-toolbar
		 (cons (window-buffer
			(frame-selected-window floating-toolbar-frame))
			toolbar))
  (floating-toolbar-resize-toolbar-frame toolbar)
  ;; fiddle with the x value to try to center the toolbar relative to
  ;; the mouse position.
  (setq x (max 0 (- x (/ (frame-pixel-width floating-toolbar-frame) 2))))
  (floating-toolbar-set-toolbar-frame-position x y)
  (floating-toolbar-expose-toolbar-frame))

(defun floating-toolbar-undisplay-toolbar ()
  (floating-toolbar-hide-toolbar-frame))

(defun floating-toolbar-hide-toolbar-frame ()
  (if (floating-toolbar-displayed)
      (progn
	(make-frame-invisible floating-toolbar-frame)
	(if (and floating-toolbar-use-sound floating-toolbar-display-pending)
	    (play-sound 'floating-toolbar-disappears))
	(setq floating-toolbar-display-pending nil))))

(defun floating-toolbar-expose-toolbar-frame ()
  (if (not (floating-toolbar-displayed))
      (progn
	(make-frame-visible floating-toolbar-frame)
	(if (and floating-toolbar-use-sound
		 (null floating-toolbar-display-pending))
	    (play-sound 'floating-toolbar-appears))
	(setq floating-toolbar-display-pending t))))

(defun floating-toolbar-resize-toolbar-frame (toolbar)
  (let ((width 0)
	(height nil)
	(bevel (* 2 (or (cdr (assq 'toolbar-shadow-thickness (frame-parameters)))
			0)))
	(captioned (specifier-instance toolbar-buttons-captioned-p))
	button glyph glyph-list)
    (while toolbar
      (setq button (car toolbar))
      (cond ((null button)
	     (setq width (+ width 8)))
	    ((eq (elt button 0) ':size)
	     (setq width (+ width (elt button 1))))
	    ((and (eq (elt button 0) ':style)
		  (= (length button) 4)
		  (eq (elt button 2) ':size))
	     (setq width (+ width bevel (elt button 3))))
	    (t
	      (setq glyph-list (elt button 0))
	      (if (symbolp glyph-list)
		  (setq glyph-list (symbol-value glyph-list)))
	      (if (and captioned (> (length glyph-list) 3))
		  (setq glyph (or (nth 3 glyph-list)
				  (nth 4 glyph-list)
				  (nth 5 glyph-list)))
		(setq glyph (car glyph-list)))
	      (setq width (+ width bevel (glyph-width glyph)))
	      (or height (setq height (+ bevel (glyph-height glyph))))))
      (setq toolbar (cdr toolbar)))
    (set-specifier top-toolbar-height height floating-toolbar-frame)
    (set-frame-width floating-toolbar-frame
		     (1+ (/ width (font-width (face-font 'default)
					  floating-toolbar-frame))))))

(defun floating-toolbar-set-toolbar-frame-position (x y)
  (set-frame-position floating-toolbar-frame x y))

(defun floating-toolbar-make-junk-frame ()
  (let ((window-min-height 1)
	(window-min-width 1))
    (save-excursion
      (set-buffer (generate-new-buffer "*junk-frame-buffer*"))
      (prog1
	  (make-frame '(minibuffer t initially-unmapped t width 1 height 1))
	(rename-buffer " *junk-frame-buffer*" t)))))

(defun floating-toolbar-make-toolbar-frame (x y)
  (save-excursion
    (let ((window-min-height 1)
	  (window-min-width 1)
	  (bg-color (or (x-get-resource "backgroundToolBarColor"
					"BackgroundToolBarColor"
					'string
					'global
					(selected-device)
					t)
			"grey75"))
	  (buffer (get-buffer-create " *floating-toolbar-buffer*"))
	  (frame nil))
      (set-buffer buffer)
      (set-buffer-menubar nil)
      (if floating-toolbar-help-font
	  (progn (set-buffer (window-buffer (minibuffer-window)))
		 (set-buffer-menubar nil)))
      (setq frame (make-frame (list
			       '(initially-unmapped . t)
			       ;; try to evade frame decorations
			       (cons 'name (or floating-toolbar-frame-name
					       "xclock"))
			       '(border-width . 2)
			       (cons 'border-color bg-color)
			       (cons 'top y)
			       (cons 'left x)
			       (cons 'popup
				     (floating-toolbar-make-junk-frame))
			       (if floating-toolbar-help-font
				   '(minibuffer . only)
				 '(minibuffer . nil))
			       '(width . 3)
			       '(height . 1))))
      (set-specifier text-cursor-visible-p (cons frame nil))
      (if floating-toolbar-help-font
	  (set-face-font 'default floating-toolbar-help-font frame)
	(set-face-font 'default "nil2" frame))
      (set-face-background 'default bg-color frame)
      (set-face-background 'modeline bg-color frame)
      (set-specifier modeline-shadow-thickness (cons frame 1))
      (set-specifier has-modeline-p (cons frame nil))
      (set-face-background-pixmap 'default "" frame)
      (set-window-buffer (frame-selected-window frame) buffer)
      (set-specifier top-toolbar-height (cons frame 0))
      (set-specifier left-toolbar-width (cons frame 0))
      (set-specifier right-toolbar-width (cons frame 0))
      (set-specifier bottom-toolbar-height (cons frame 0))
      (set-specifier top-toolbar (cons frame nil))
      (set-specifier left-toolbar (cons frame nil))
      (set-specifier right-toolbar (cons frame nil))
      (set-specifier bottom-toolbar (cons frame nil))
      (set-specifier scrollbar-width (cons frame 0))
      (set-specifier scrollbar-height (cons frame 0))
      frame )))

;; first popup should be faster if we go ahead and make the frame now.
(or (not (featurep 'toolbar))
    floating-toolbar-frame
    (not (eq (device-type) 'x))
    (setq floating-toolbar-frame (floating-toolbar-make-toolbar-frame 0 0)))

;;; floating-toolbar.el ends here
