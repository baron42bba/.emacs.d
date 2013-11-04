;;; Balloon help for XEmacs (requires 19.15 or later)
;;; Copyright (C) 1995, 1997 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle@wonderworks.com

;; Balloon help pops up a small frame to display help text
;; relating to objects that the mouse cursor passes over.
;;
;; Installation:
;;
;; Byte-compile the file balloon-help.el (with M-x byte-compile-file)
;; and put the .elc file in a directory in your load-path.  Add the
;; following line to your .emacs:
;;
;; (require 'balloon-help)
;; (balloon-help-mode 1)
;;
;; The balloon-help frame is a transient window that is not
;; normally decorated by window managers, so the following
;; window manager directives may not be needed.  But if they
;; are:
;;
;; For ol[v]wm use this in .Xdefaults:
;;    olvwm.NoDecor: balloon-help
;;      or
;;    olwm.MinimalDecor: balloon-help
;;
;; For fvvm version 1 use this in your .fvwmrc:
;;    NoTitle balloon-help
;; or
;;    Style "balloon-help" NoTitle, NoHandles, BorderWidth 0
;;
;; For twm use this in your .twmrc:
;;    NoTitle { "balloon-help" }
;;

(provide 'balloon-help)

(require 'custom)

(defgroup balloon-help nil
  "Balloon-help support in XEmacs"
  :group 'frames)

(defvar balloon-help-version "1.08"
  "Version string for Balloon Help.")

;;;###autoload
(defcustom balloon-help-mode nil
  "*Non-nil means Balloon help mode is enabled."
  :type 'boolean
  :set (lambda (symbol value) (balloon-help-mode (or value 0)))
  :initialize 'custom-initialize-default
  :require 'balloon-help
  :group 'balloon-help)

(defvar balloon-help-minor-mode nil
  "*Non-nil means Balloon help minor mode is enabled.")
(make-variable-buffer-local 'balloon-help-minor-mode)

(defcustom balloon-help-timeout 1500
  "*Display help after this many milliseconds of mouse inactivity."
  :type 'integer
  :group 'balloon-help)

(defcustom balloon-help-foreground "black"
  "*The foreground color for displaying balloon help text."
  :type 'string
  :group 'balloon-help)

(defcustom balloon-help-background "gray80"
  "*The background color for the balloon help frame."
  :type 'string
  :group 'balloon-help)

(defcustom balloon-help-background-pixmap ""
  "*The background pixmap for the balloon help frame."
  :type 'string
  :group 'balloon-help)

(defcustom balloon-help-font "variable"
  "*The font for displaying balloon help text."
  :type 'string
  :group 'balloon-help)

(defcustom balloon-help-border-color "black"
  "*The color for displaying balloon help frame's border."
  :type 'string
  :group 'balloon-help)

(defcustom balloon-help-border-width 1
  "*The width of the balloon help frame's border."
  :type 'integer
  :group 'balloon-help)

(defcustom balloon-help-use-sound nil
  "*Non-nil value means play a sound to herald the appearance
and disappearance of the help frame.

`balloon-help-appears' will be played when the frame appears.
`balloon-help-disappears' will be played when the frame disappears.

See the documentation for the function load-sound-file to see how
define sounds."
  :type 'boolean
  :group 'balloon-help)

(defcustom balloon-help-frame-name "balloon-help"
  "*The frame name to use for the frame to display the balloon help."
  :type 'string
  :group 'balloon-help)

(defcustom balloon-help-aggressively-follow-mouse nil
  "*Non-nil means the balloon should move with the mouse even if the mouse
is over the same object as the last mouse motion event."
  :type 'boolean
  :group 'balloon-help)

(defcustom balloon-help-mode-line-string " Bal"
  "*String to display in the modeline when `balloon-help-minor-mode' is active.
Set this to nil if you don't want a modeline indicator."
  :type '(choice string
		 (const :tag "none" nil))
  :group 'balloon-help)

;;;
;;; End of user variables.
;;;

(defvar mouse-motion-hook mouse-motion-handler
  "Hooks to be run whenever the user moves the mouse.
Each hook is called with one argument, the mouse motion event.
This hooks variable does not exist unless the \"balloon-help\" library
has been loaded.")

(defun mouse-motion-hook (event)
  "Run the hooks attached to mouse-motion-hook."
  (run-hook-with-args 'mouse-motion-hook event))

(setq mouse-motion-handler 'mouse-motion-hook)

(defvar balloon-help-frame nil
  "Balloon help is displayed in this frame.")

(defvar balloon-help-junk-frame nil
  "Junk parent frame of balloon-help-frame.")

(defvar balloon-help-help-object nil
  "Object that the mouse is over that has a help property, nil otherwise.")

(defvar balloon-help-help-object-x nil
  "Last horizontal mouse position over balloon-help-help-object.")

(defvar balloon-help-help-object-y nil
  "Last vertical mouse position over balloon-help-help-object.")

(defvar balloon-help-buffer (get-buffer-create " *balloon-help*")
  "Buffer used to display balloon help.")

(defvar balloon-help-timeout-id nil
  "Timeout id for the balloon help timeout.")

(defvar balloon-help-menubar-possible (featurep 'menubar))
(defvar balloon-help-toolbar-possible (featurep 'toolbar))
(defvar balloon-help-scrollbar-possible (featurep 'scrollbar))
(defvar balloon-help-gutter-possible (featurep 'gutter))

(defvar balloon-help-display-pending nil
  "Non-nil value means the help frame will be visible as soon
as the X server gets around to displaying it.  Nil means it
will be invisible as soon as the X server decides to hide it.")

;;;###autoload
(defun balloon-help-mode (&optional arg)
  "Toggle Balloon Help mode.
With arg, turn Balloon Help mode on iff arg is positive.

With Balloon Help enabled, a small frame is displayed whenever
the mouse rests on an object that has a help property of some
kind.  The text of that help property is displayed in the frame.

If you want Balloon Help mode enabled in some buffers only, use
`balloon-help-minor-mode' instead.

For extents, the 'balloon-help' property is checked.

For toolbar buttons, the help-string slot of the toolbar button
is checked.

If the value is a string, it is used as the help message.

If the property's value is a symbol, it is assumed to be the name
of a function and it will be called with one argument, the object
under the mouse, and the return value of that function will be
used as the help message."
  (interactive "P")
  (setq balloon-help-mode (or (and arg (> (prefix-numeric-value arg) 0))
			      (and (null arg) (null balloon-help-mode))))
  (if (null balloon-help-mode)
      (balloon-help-undisplay-help)))

;;;###autoload
(defun balloon-help-minor-mode (&optional arg)
  "Toggle Balloon Help minor mode.
With arg, turn Balloon Help minor mode on iff arg is positive.

This minor mode is useful if you want `balloon-help-mode' globally disabled,
except in some buffers.

Please refer to the function `balloon-help-mode' for more details"
  (interactive "P")
  (setq balloon-help-minor-mode
	(or (and arg (> (prefix-numeric-value arg) 0))
	    (and (null arg) (null balloon-help-minor-mode))))
  (if (and (null balloon-help-minor-mode) (null balloon-help-mode))
      (balloon-help-undisplay-help)))
(add-minor-mode 'balloon-help-minor-mode 'balloon-help-mode-line-string)

(defun balloon-help-displayed ()
  (and (frame-live-p balloon-help-frame)
       (frame-visible-p balloon-help-frame)
       (eq (frame-device balloon-help-frame) (selected-device))))

(defun balloon-help (&optional event)
  "Display Balloon Help for the object under EVENT.
If EVENT is nil, then point in the selected window is used instead.
See the documentation for balloon-help-mode to find out what this means.
This command must be bound to a mouse event."
  (interactive "e")
  (unless (device-on-window-system-p)
    (error "Cannot display balloon help on %s device" (device-type)))
  (let ((balloon-help-mode t))
    (balloon-help-motion-hook event))
  (when balloon-help-timeout-id
    (disable-timeout balloon-help-timeout-id)
    (setq balloon-help-timeout-id nil))
  (balloon-help-display-help))

(defun balloon-help-motion-hook (event)
  (cond
   ((and (null balloon-help-mode) (null balloon-help-minor-mode)) t)
   (t
    (let* ((buffer (if event (event-buffer event) (current-buffer)))
	   (frame (if event (event-frame event) (selected-frame)))
	   (point (if event (event-point event) (point)))
	   (modeline-point (if event (event-modeline-position event)))
	   (modeline-extent (and modeline-point
				 (map-extents
				  (function (lambda (e ignored) e))
				  (symbol-value-in-buffer
				   'generated-modeline-string
				   buffer)
				  modeline-point modeline-point
				  nil nil
				  'balloon-help)))
	   (glyph-extent (and event (event-glyph-extent event)))
	   (glyph-extent (if (and glyph-extent
				  (extent-property glyph-extent
						   'balloon-help))
			     glyph-extent))
	   (extent (and point
			(extent-at point buffer 'balloon-help)))
	   (button (and event (event-toolbar-button event)))
	   (button (if (and button (toolbar-button-help-string button))
		       button
		     nil))
	   (object (or modeline-extent glyph-extent extent button))
	   (id balloon-help-timeout-id))
      (if (null object)
	  (if (and balloon-help-frame
		   (not (eq frame balloon-help-frame)))
	      (progn
		(setq balloon-help-help-object nil)
		(when id
		  (disable-timeout id)
		  (setq balloon-help-timeout-id nil))
		(if (balloon-help-displayed)
		    (balloon-help-undisplay-help))))
	(let* ((params (frame-parameters frame))
	       (top (cdr (assq 'top params)))
	       (left (cdr (assq 'left params)))
	       (xtop-toolbar-height
		(if (and balloon-help-toolbar-possible
			 (specifier-instance top-toolbar-visible-p frame)
			 (specifier-instance top-toolbar frame))
		    (+ (specifier-instance top-toolbar-height frame)
		       (if (and (boundp 'top-toolbar-border-width)
				(specifierp top-toolbar-border-width))
			   (specifier-instance top-toolbar-border-width frame)
			 0))
		  0))
	       (xleft-toolbar-width
		(if (and balloon-help-toolbar-possible
			 (specifier-instance left-toolbar-visible-p frame)
			 (specifier-instance left-toolbar frame))
		    (+ (specifier-instance left-toolbar-width frame)
		       (if (and (boundp 'left-toolbar-border-width)
				(specifierp left-toolbar-border-width))
			   (specifier-instance left-toolbar-border-width frame)
			 0))
		  0))
	       (menubar-height
		(if (and balloon-help-menubar-possible buffer
			 (specifier-instance menubar-visible-p frame)
			 (save-excursion (set-buffer buffer) current-menubar))
		    22
		  0)))
	  (setq balloon-help-help-object-x
		(if event
		    (+ left xleft-toolbar-width
		       (event-x-pixel event))
		  (/ (* (device-pixel-width) 2) 5))
		balloon-help-help-object-y
		(if event
		    (+ top xtop-toolbar-height menubar-height
		       (event-y-pixel event))
		  (/ (* (device-pixel-height) 2) 5))))
	(cond ((eq frame balloon-help-frame) t)
	      ((eq object balloon-help-help-object)
	       (if (and (balloon-help-displayed)
			balloon-help-aggressively-follow-mouse)
		   (balloon-help-move-help-frame)))
	      ((balloon-help-displayed)
	       (setq balloon-help-help-object object)
	       (balloon-help-display-help))
	      (t
	       (setq balloon-help-help-object object)
	       (if id
		   (disable-timeout id))
	       (setq balloon-help-timeout-id
		     (add-timeout (/ balloon-help-timeout 1000.0)
				  (function balloon-help-display-help)
				  nil)))))))))

(defun balloon-help-display-help (&rest ignored)
  (setq balloon-help-timeout-id nil)
  (if (and balloon-help-help-object (device-on-window-system-p))
      (let* ((object balloon-help-help-object)
	     (help (or (and (extent-live-p object)
			    (extent-property object 'balloon-help))
		       (and (toolbar-button-p object)
			    (toolbar-button-help-string object))
		       (and (stringp object) object))))
	;; if help is non-null and is not a string, run it as
	;; function to produuce the help string.
	(if (or (null help) (not (symbolp help)))
	    nil
	  (condition-case data
	      (setq help (funcall help object))
	    (error
	     (setq help (format "help function signaled: %S" data)))))
	(if (stringp help)
	    (save-excursion
	      (if (or (not (frame-live-p balloon-help-frame))
		      (not (eq (selected-device)
			       (frame-device balloon-help-frame))))
		  (setq balloon-help-frame (balloon-help-make-help-frame)))
	      (set-buffer balloon-help-buffer)
	      (erase-buffer)
	      (insert help)
	      (if (not (bolp))
		  (insert ?\n))
;;;	      ;; help strings longer than 2 lines have the last
;;;	      ;; line stolen by the minibuffer, so make sure the
;;;	      ;; last line is blank.  Make the top line blank for
;;;	      ;; some symmetry.
;;;	      (if (< 2 (count-lines (point-min) (point-max)))
;;;		  (progn
;;;		    (insert ?\n)
;;;		    ;; add a second blank line at the end to
;;;		    ;; prevent the modeline bar from clipping the
;;;		    ;; descenders of the last line of text.
;;;		    (insert ?\n)
;;;		    (goto-char (point-min))
;;;		    (insert ?\n)))
	      ;; indent everything by a space for readability
 	      (indent-rigidly (point-min) (point-max) 1)
	      (balloon-help-set-frame-properties)
	      (balloon-help-resize-help-frame)
	      (balloon-help-move-help-frame)
	      (balloon-help-expose-help-frame))))))

(defun balloon-help-undisplay-help ()
  (balloon-help-hide-help-frame))

(defun balloon-help-hide-help-frame ()
  (if (balloon-help-displayed)
      (progn
	(make-frame-invisible balloon-help-frame)
	(if (and balloon-help-use-sound balloon-help-display-pending)
	    (play-sound 'balloon-help-disappears))
	(setq balloon-help-display-pending nil))))

(defun balloon-help-expose-help-frame ()
  (if (not (balloon-help-displayed))
      (progn
	(make-frame-visible balloon-help-frame)
	(if (and balloon-help-use-sound (null balloon-help-display-pending))
	    (play-sound 'balloon-help-appears))
	(setq balloon-help-display-pending t))))

(defun balloon-help-set-frame-properties ()
  (let ((frame balloon-help-frame))
    ;; don't set the font unconditionally because it makes the
    ;; frame size flap visibly while XEmacs figures out the new
    ;; frame size.
    (if (not (equal (face-font 'default frame) balloon-help-font))
	(set-face-font 'default balloon-help-font frame))
    (set-face-foreground 'default balloon-help-foreground frame)
    (set-face-background 'default balloon-help-background frame)
    (set-face-background 'modeline balloon-help-background frame)
    (set-face-background-pixmap 'default balloon-help-background-pixmap frame)
    (set-frame-property frame 'border-color balloon-help-border-color)
    (set-frame-property frame 'border-width balloon-help-border-width)))

;;;(defun balloon-help-resize-help-frame ()
;;;  (save-excursion
;;;    (set-buffer balloon-help-buffer)
;;;    (let ((longest 0)
;;;	  (lines 0)
;;;	  (done nil)
;;;	  (window-min-height 1)
;;;	  (window-min-width 1))
;;;      (goto-char (point-min))
;;;      (while (not done)
;;;	(end-of-line)
;;;	(setq longest (max longest (current-column))
;;;	      done (not (= 0 (forward-line))))
;;;	(and (not done) (setq lines (1+ lines))))
;;;      (set-frame-size balloon-help-frame (+ 1 longest) lines))))

(defun balloon-help-resize-help-frame ()
  (save-excursion
    (set-buffer balloon-help-buffer)
    (let* ((longest 0)
	   (lines 0)
	   (done nil)
	   (inst (vector 'string ':data nil))
	   (window (frame-selected-window balloon-help-frame))
	   (font-width (font-width (face-font 'default) balloon-help-frame))
	   start width
	   (window-min-height 1)
	   (window-min-width 1))
      (goto-char (point-min))
      (while (not done)
	(setq start (point))
	(end-of-line)
	(aset inst 2 (buffer-substring start (point)))
	(setq longest (max longest (glyph-width (make-glyph inst) window))
	      done (not (= 0 (forward-line))))
	(and (not done) (setq lines (1+ lines))))
      (setq width (/ longest font-width)
	    width (if (> longest (* width font-width)) (1+ width) width))
      (set-frame-size balloon-help-frame (+ 0 width) lines))))

(defun balloon-help-compute-help-frame-y-location ()
  (let* ((device-bottom (device-pixel-height
			 (frame-device balloon-help-frame)))
	 (y-pos (max 0 (+ 48 balloon-help-help-object-y)))
	 (height (frame-pixel-height balloon-help-frame))
	 (bottom (+ y-pos height)))
    (if (>= bottom device-bottom)
	(setq y-pos (max 0 (- y-pos (- bottom device-bottom)))))
    y-pos ))

(defun balloon-help-compute-help-frame-x-location ()
  (let* ((device-right (device-pixel-width (frame-device balloon-help-frame)))
	 (x-pos (max 0 (+ 32 balloon-help-help-object-x)))
	 (width (frame-pixel-width balloon-help-frame))
	 (right (+ x-pos width)))
    (if (>= right device-right)
	(setq x-pos (max 0 (- x-pos (- right device-right)))))
    x-pos ))

(defun balloon-help-move-help-frame ()
  (let ((x (balloon-help-compute-help-frame-x-location))
	(y (balloon-help-compute-help-frame-y-location)))
    (set-frame-position balloon-help-frame x y)))

(defun balloon-help-make-junk-frame ()
  (let ((window-min-height 1)
	(window-min-width 1))
    (when (framep balloon-help-junk-frame)
      (delete-frame balloon-help-junk-frame)
      (setq balloon-help-junk-frame nil))
    (prog1
	(setq balloon-help-junk-frame
	      (make-frame '(minibuffer t
			    initially-unmapped t
			    override-redirect t	; get rid of WM decorations
			    width 1
			    height 1)))
      (set-window-buffer (frame-selected-window balloon-help-junk-frame)
			 balloon-help-buffer))))

(defun balloon-help-make-help-frame ()
  (when (framep balloon-help-frame)
    (delete-frame balloon-help-frame)
    (setq balloon-help-frame nil))
  (save-excursion
    (set-buffer balloon-help-buffer)
    (setq truncate-lines t)
    (when balloon-help-menubar-possible
      (set-buffer-menubar nil))
    (let* ((x (balloon-help-compute-help-frame-x-location))
	   (y (balloon-help-compute-help-frame-y-location))
	   (window-min-height 1)
	   (window-min-width 1)
	   (junk-frame (balloon-help-make-junk-frame))
	   (frame (make-frame `(initially-unmapped t
				internal-border-width 0
				;; try to suppress frame decorations
				override-redirect t
				name ,balloon-help-frame-name
				border-width ,balloon-help-border-width
				border-color ,balloon-help-border-color
				top ,y
				left ,x
				popup ,junk-frame
				minibuffer ,(minibuffer-window junk-frame)
				width 3
				height 1))))
      (set-face-font 'default balloon-help-font frame)
      (set-face-foreground 'default balloon-help-foreground frame)
      (set-face-background 'default balloon-help-background frame)
      (set-face-background-pixmap 'default balloon-help-background-pixmap
				  frame)
      (set-window-buffer (frame-selected-window frame) balloon-help-buffer)
      (when balloon-help-toolbar-possible
	(set-specifier top-toolbar-height (cons frame 0))
	(set-specifier left-toolbar-width (cons frame 0))
	(set-specifier right-toolbar-width (cons frame 0))
	(set-specifier bottom-toolbar-height (cons frame 0))
	(set-specifier top-toolbar-visible-p (cons frame nil))
	(set-specifier left-toolbar-visible-p (cons frame nil))
	(set-specifier right-toolbar-visible-p (cons frame nil))
	(set-specifier bottom-toolbar-visible-p (cons frame nil))
	(set-specifier top-toolbar (cons frame nil))
	(set-specifier left-toolbar (cons frame nil))
	(set-specifier right-toolbar (cons frame nil))
	(set-specifier bottom-toolbar (cons frame nil)))
      (when balloon-help-gutter-possible
	(set-specifier top-gutter-height (cons frame 0))
	(set-specifier left-gutter-width (cons frame 0))
	(set-specifier right-gutter-width (cons frame 0))
	(set-specifier bottom-gutter-height (cons frame 0))
	(set-specifier top-gutter-visible-p (cons frame nil))
	(set-specifier left-gutter-visible-p (cons frame nil))
	(set-specifier right-gutter-visible-p (cons frame nil))
	(set-specifier bottom-gutter-visible-p (cons frame nil))
	(set-specifier top-gutter (cons frame nil))
	(set-specifier left-gutter (cons frame nil))
	(set-specifier right-gutter (cons frame nil))
	(set-specifier bottom-gutter (cons frame nil)))
      (when balloon-help-scrollbar-possible
	(set-specifier scrollbar-width (cons frame 0))
	(set-specifier scrollbar-height (cons frame 0)))
      (set-specifier text-cursor-visible-p (cons frame nil))
      (set-specifier has-modeline-p (cons frame nil))
      (set-specifier modeline-shadow-thickness (cons frame 0))
      (set-specifier (glyph-image truncation-glyph) [nothing] frame '(x))
      (set-face-background 'modeline balloon-help-background frame)
      frame )))

(defun balloon-help-pre-command-hook ()
  (unless (eq this-command 'balloon-help)
    (balloon-help-go-away)))

(defun balloon-help-go-away (&rest ignored)
  (setq balloon-help-help-object nil)
  (if (balloon-help-displayed)
      (balloon-help-undisplay-help)))

(defun balloon-help-mouse-leave-frame-hook (&rest ignored)
  (let* ((mouse (mouse-position))
	 (window (car mouse)))
    (if (or (null window) (not (eq (window-frame window) balloon-help-frame)))
	(balloon-help-go-away))))

;; loses with ClickToFocus under fvwm
;;(fset 'balloon-help-deselect-frame-hook 'balloon-help-go-away)
;;(add-hook 'deselect-frame-hook 'balloon-help-deselect-frame-hook)

(add-hook 'mouse-motion-hook 'balloon-help-motion-hook)

(add-hook 'pre-command-hook 'balloon-help-pre-command-hook)
(add-hook 'mouse-leave-frame-hook 'balloon-help-mouse-leave-frame-hook)
