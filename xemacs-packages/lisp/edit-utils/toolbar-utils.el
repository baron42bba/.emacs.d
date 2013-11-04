;;; toolbar-utils.el --- Toolbar utility functions for XEmacs

;; Copyright (C) 1997, 2002 by Free Software Foundation, Inc.

;; Author: Stephen J. Turnbull <stephen@xemacs.org>
;;         Jeff Miller <jmiller@smart.net>
;; Created: 1997
;; Last-Modified: 03 December 2002
;; Keywords: gui, services

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

;; This file contains utilities for managing XEmacs toolbars.
;; It doesn't seem to make sense to provide GNU compatibility here.
;; Instead, it seems much more reliable to reimplement the XEmacs
;; APIs for GNU.

;; This code is inspired by that written by Jeff Miller, and retains
;; his APIs.  However, it is a thorough rewrite.
;; See also edit-toolbar.el by Peter D. Pezaris <pez@dwwc.com>

;;; Change Log: see package ChangeLog.

;; To do:

;; 1. Probably should convert all these to use &keywords?
;; 2. Behavior-like implementation of "mode locales".
;; 3. Implement toolbar-replace-button (by position or content).
;; 5. Make an xemacs-toolbar version to do something sane in GNU.

;;; Code:

;;;###autoload
(defun restore-initial-toolbar ()
  "Restores the default toolbar defined by initial-toolbar-spec.

There is also a cache of killed buttons in `button-palette'."
  (interactive)
  (set-specifier default-toolbar initial-toolbar-spec))

;;; Variables:

;; #### need parent group
(defgroup edit-toolbar nil
  "Tools for interactive editing and non-interactive management of toolbars.")

(defcustom toolbar-button-default-position 'right
  "Default position for adding toolbar buttons on the fly.

See `toolbar-add-button-on-the-fly' for possible values and meanings."
  :type 'sexp
  :group 'edit-toolbar)

;; This needs to be a toolbar descriptor.  We should make a function to
;; stick it in a non-default toolbar, with surgery done on the function
;; so that pressing the palette buttons moves them to another toolbar.
(defvar button-palette nil
  "List of buttons cut from toolbars.

Note this is actually a toolbar descriptor.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; User commands:

;; toolbar-add-button-on-the-fly

;; #### This really belongs in edit-toolbar.el, except that it requires
;; functions from this file and edit-toolbar.el otherwise would not.
;;;###autoload
(defun toolbar-add-button-on-the-fly (description command label
				      &optional position locale)
  "Add an button at POSITION to the default toolbar of the selected window.
Returns t.

#### The return value may change.  Tell stephen@xemacs.org what value
you think would be (most) useful.

DESCRIPTION is a string describing the action, and displayed as help.
COMMAND is an interactive command (ie, a symbol with an interactive function
definition) implementing the action.
LABEL is a string used to label the button.
POSITION is an optional position (a non-negative integer, or one of the
symbols 'left, 'right, or 'extreme-right, see `toolbar-add-item').
LOCALE is an optional locale, defaulting to the current buffer.  If current-
buffer-only is not what you want, and you don't understand specifier locales,
use 'global.  It's safe and probably does what you want.

#### No error checking, use at your own risk."

  (interactive (list (read-string "Description: ")
		     (read-command "Command: ")
		     (read-string "Button label: ")
		     (read (completing-read
			    "Position: "
			    (lambda (x junk morejunk)
			      (cond ((let ((y (read x)))
				       (and (integerp y)
					    (<= 0 y)))
				     x)
				    ((try-completion x '(("left")
							 ("extreme-right")
							 ("right"))))))
		       nil nil nil nil "right"))
		     current-prefix-arg))

  (let ((domain (selected-window))
	(button (toolbar-new-button label command description)))
    (set-specifier default-toolbar
		   (toolbar-add-item (specifier-instance default-toolbar
							 domain)
				     button
				     position)
		   (or locale (current-buffer))))
  t)

;; toolbar-add-kbd-macro

;;;###autoload
(defun toolbar-add-kbd-macro (mac icon is-file)
  "Add a button invoking a keyboard macro to the toolbar of the current buffer.
The button is added at the end of the left group.

MAC is a keyboard macro name, or the empty string or nil to use a copy of
the last keyboard macro defined.
ICON is a string specifying the icon to be used.  If IS-FILE is non-nil,
it is interpreted as the name of an image file, and searched for using
`locate-data-file'.  Otherwise it is used verbatim as a label.

Interactively, prompts for the macro name MAC and an ICON.  IS-FILE is
non-nil if a prefix argument was used.

Warning: the interpretation of the prefix argument is likely to change."
  (interactive
   (list (read-string "Macro name (RET to use last defined macro): ")
	 (read-string (format "Icon %s: "
			      (if current-prefix-arg "image" "label")))
	 current-prefix-arg))
  (let* ((locale (current-buffer))
	 (position nil)
	 (mac (if (or (null mac) (= 0 (length mac)))
		  ;; is there an argument pro or con to copy-sequence?
		  last-kbd-macro
		(intern mac)))
	 ;; #### this actually probably just works, and we don't even need
	 ;; the IS-FILE argument at all
	 (icon (if is-file
		   (error 'unimplemented "We don't do image icons yet")
		 icon))
	 ;; this is actually the help string
	 (label (cond ((and is-file mac))
		      ;;  #### should truncate image file extension if any
		      ((and (stringp icon) (< 0 (length icon))) icon)
		      (t "KbdMac")))
	 (button (toolbar-new-button icon
				     `(lambda ()
					(interactive)
					(execute-kbd-macro ,mac))
				     label)))
    ;; #### need to abstract this
    ;; something like (toolbar-update-toolbar TOOLBAR ITEM POS LOCALE)
    (set-specifier default-toolbar
		   (toolbar-add-item (cdadar
				      (specifier-spec-list default-toolbar
							   locale))
				     button
				     position)
		   locale))
  t)

;; #### this should be a behavior
;;;###autoload
(defun toolbar-add-execute-macro-button ()
  "Add a button to the global toolbar to execute the last keyboard macro.

Unlike `toolbar-add-kbd-macro', this does not copy the macro.  The macro
executed will change with redefinitions.

Due to a simple implementation, this button will not appear in buffers with
local toolbars if invoked after the toolbar is installed.  If you like this
button, it's probably best to invoke this function in your init file."
  (interactive)
  (let* ((locale 'global)
	 (spec (cdadar (specifier-spec-list default-toolbar locale)))
	 (button (toolbar-new-button "LastMac"
				     #'toolbar-execute-last-kbd-macro
				     "Execute last defined keyboard macro")))
    (set-specifier default-toolbar (toolbar-add-item spec button) locale)))

;; if this is defsubst, XEmacs 21.4.10 crashes?
(defun toolbar-execute-last-kbd-macro ()
  (interactive)
  (execute-kbd-macro last-kbd-macro))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lisp API functions:

;; adding items to a toolbar, with convenience functions for constructing
;; valid buttons, spacers, and toolbar descriptors

;;;###autoload
(defun toolbar-update-toolbar (item &optional toolbar position locale)
  "Use ITEM to update TOOLBAR at POSITION in LOCALE.

ITEM is a toolbar button or spacer specification (eg, from
`toolbar-new-button' or `toolbar-new-spacer').
Optional TOOLBAR is a toolbar specifier object.  It defaults to the value
of `default-toolbar'.
Optional POSITION is a non-negative integer or a symbol (for valid values,
see `toolbar-add-item').  The default is 'right.
Optional LOCALE is a specifier locale.  The default is 'global.  (This
default is not yet set in stone; it's best not to depend on it.)

This is a convenience function for helper applications or minor modes that
would like to add a small number of buttons to an existing toolbar.  For
constructing toolbars from scratch, use list and vector primitives, or
`toolbar-add-item'."
  (setq toolbar (or toolbar default-toolbar))
  (setq position (or position 'right))
  (setq locale (or locale 'global))
  (let ((spec (cdadar (specifier-spec-list toolbar locale))))
    (set-specifier toolbar (toolbar-add-item spec item position) locale)))


;;;###autoload
(defun toolbar-add-item (toolbar-spec item &optional position)
  "Add ITEM to TOOLBAR-SPEC at POSITION, and return TOOLBAR-SPEC.
Uses functions that alter list structure.

TOOLBAR-SPEC is a toolbar descriptor (eg, from `toolbar-new-toolbar').
ITEM is a toolbar button or spacer specification (eg, from
`toolbar-new-button' or `toolbar-new-spacer').
Optional POSITION is a non-negative integer, with 0 being the extreme left and
\(length TOOLBAR-SPEC) the extreme right.  The symbols 'left, 'right, and
'extreme-right are also accepted.  'left is synonymous with 0.  'right places
ITEM at the right end of the left group of buttons.  'extreme-right places
ITEM at the extreme right of the toolbar, creating a right group if one
does not exist.

#### This function does not yet support inserting the group delimiter nil
as an explicit item.

POSITION may be greater than (length TOOLBAR-SPEC), in which case it is
truncated to (length TOOLBAR-SPEC).  Note that (length TOOLBAR-SPEC) is not
synonymous with either 'right or 'extreme-right."
  (check-valid-instantiator toolbar-spec 'toolbar)
  (check-toolbar-button-syntax item)
  (unless position (setq position toolbar-button-default-position))
  (cond ((eq position 'right)
	 (let ((tail (memq nil toolbar-spec)))
	   (if (not tail)
	       (nconc toolbar-spec (list item))
	     ;; is this overly tricky?
	     (setcar tail item)
	     (setcdr tail (cons nil (cdr tail)))
	     toolbar-spec)))
	((or (eq position 'left) (eq position 0))
	 (cons item toolbar-spec))
	((eq position 'extreme-right)
	 (nconc toolbar-spec (if (memq nil toolbar-spec)
				 (list item)
			       (list nil item))))
	((and (integerp position) (< 0 position))
	 (let ((tail (nthcdr 1 toolbar-spec)))
	   (if (null tail)
	       (nconc toolbar-spec (list item))
	     ;; is this overly tricky?
	     (setcdr tail (cons (car tail) (cdr tail)))
	     (setcar tail item)
	     toolbar-spec)))
	(t (error 'wrong-type-argument
		  'integer-or-symbol
		  position))))

;;;###autoload
(defun toolbar-new-button (icon-spec command help-string
			   &optional initially-disabled)
  "Return a checked toolbar button specification.

ICON-SPEC should be a list of glyphs (from `make-glyph'), a glyph, or a
string to use as the button's icon.  If a string or single glyph, it will
be used for the button-up glyph.  If a list, it may contain 1 to 6 glyphs,
which XEmacs will use for button up, button down, button disabled, button
up with caption, button down with caption, and button disabled with caption,
in that order.  Missing or nil glyphs will be defaulted.  (#### Strings as
list elements are not supported but could be.)
COMMAND is the (interactive) command to invoke when the button is pressed.
HELP-STRING is a string briefly describing the command, displayed in the
echo area or as balloon help when the pointer enters the button.
Optional argument INITIALLY-DISABLED, if non-nil, specifies that the button
should initially be disabled.

See `default-toolbar' or the Lispref (node toolbars) for more information."
  (setq icon-spec (cond ((glyphp icon-spec) (list icon-spec))
			((stringp icon-spec) (list (make-glyph icon-spec)))
			((listp icon-spec) icon-spec)))
  (let ((button-spec
	 (vector icon-spec command (not initially-disabled) help-string)))
    (check-toolbar-button-syntax button-spec)
    button-spec))

;;;###autoload
(defun toolbar-new-spacer (style &optional size)
  "Returns a checked toolbar spacer \"button\".

STYLE is one of the symbols '2d or '3d, indicating whether the area is
displayed without shadows (giving it a flat appearance), or with shadows
\(giving it a raised, 3-d appearance).  There is no default.
#### We may set a default style.  Tell stephen@xemacs.org which you use.
SIZE specifies the length, in pixels, of the blank area.  If omitted,
it defaults to a device-specific value (8 pixels for X devices)."
  (let* ((style (or style '2d))
	 (button (if size
		     (vector :style style :size size)
		   (vector :style style))))
    (check-toolbar-button-syntax button)
    button))

;;;###autoload
(defun make-toolbar-instantiator (&optional toolbar-spec domain)
  "Return a checked toolbar instantiator, a list of vectors.

TOOLBAR-SPEC may be a list of buttons (ie, a toolbar descriptor, see
`default-toolbar'), a toolbar specifier object, or a symbol whose value is
a toolbar specifier object.  If a toolbar specifier object or variable
containing one, the specification for DOMAIN is used.  If non-nil, DOMAIN
must be a window, a frame, or a device, otherwise it defaults to the selected
window (see `specifier-instance').  The list thus generated is checked and
returned.

If TOOLBAR-SPEC is a list, it is copied; it is safe to pass other packages'
toolbar initializers to this function.  However, you probably do not want
to change any of the objects in the returned specification.  They are
returned as is, not copied.

See `default-toolbar' or the Lispref (node toolbars) for more information."
  (setq toolbar-spec
	(cond ((null toolbar-spec) nil)

	      ((symbolp toolbar-spec)
	       (specifier-instance (symbol-value toolbar-spec) domain))
	      ((specifierp toolbar-spec)
	       (specifier-instance toolbar-spec domain))
	      ((listp toolbar-spec) (copy-sequence toolbar-spec))
	      (t toolbar-spec)))	; errors by check-valid-instantiator
  (check-valid-instantiator toolbar-spec 'toolbar)
  toolbar-spec)

;; removing buttons from a toolbar

;;;###autoload
(defun toolbar-kill-item-pos (pos &optional toolbar locale)
  "Kill the item at position POS of TOOLBAR in LOCALE.
Killed buttons are prepended to `button-palette'.

LOCALE defaults to 'global.  If there are multiple specs for LOCALE, take
the first one.

This function currently does not accept symbolic positions a la
`toolbar-add-item'.  Use `toolbar-find-item' to locate whole buttons and
spacers, and `toolbar-find-button' to locate buttons by characteristics.
See also `toolbar-find-button-by-icon', `toolbar-find-button-by-command',
and `toolbar-find-button-by-help-string'."
  (setq locale (or locale 'global))
  (let ((spec (cdadar (specifier-spec-list (or toolbar default-toolbar)
					   locale))))
    (setq button-palette (cons (nth pos spec) button-palette))
    (set-specifier toolbar (if (eq pos 0)
			       (cdr spec)
			     (setcdr (nthcdr (1- pos) spec) 
				     (nthcdr (1+ pos) spec)))
		   locale)))

;; locating buttons by their content, returning a position

;;;###autoload
(defun toolbar-find-button (item &optional toolbar locale)
  "Return the position of a button containing ITEM in its specification.

ITEM may specify a button, spacer, icon, command, help string, or nil.
If ITEM is nil, find the separator between the group of buttons to be left
justified, and the group to be right justified.  (Distinctions among the
various \"search key types\" are somewhat heuristic but are probably
reliable enough to use in library code.)

If TOOLBAR is non-nil, search it; otherwise search the default toolbar.
If LOCALE is non-nil, get TOOLBAR's descriptor in that locale, otherwise
use the global locale."
  (setq toolbar (or toolbar default-toolbar))
  (setq locale (or locale 'global))
  (cond ((or (null item) (vectorp item)) (toolbar-find-item item))
	((commandp item) (toolbar-find-button-by-command item toolbar locale))
	((stringp item)
	 (toolbar-find-button-by-help-string item toolbar locale))
	(t (toolbar-find-button-by-icon item toolbar locale))))

;;;###autoload
(defun toolbar-find-item (item &optional toolbar locale)
  "Return the position of ITEM, a button, spacer, or nil.
TOOLBAR and LOCALE determine the descriptor to be searched.

If ITEM is nil, find the separator between the group of buttons to be left
justified, and the group to be right justified.
If there are several matching items, the first is returned.  If none is
found, return nil."
  (catch 'found
    (let ((pos 0))
      (while toolbar
	(if (equal (car toolbar) item) (throw 'found pos))
	(setq pos (1+ pos))
	(setq toolbar (cdr toolbar)))
      nil)))

;; internals -- if you think these should be autoloaded, let me know
(defun toolbar-find-button-by-icon (icon &optional toolbar locale)
  "Return the position of a button with icon ICON.
ICON must be a list of glyphs or a symbols whose value is a list of glyphs.
TOOLBAR and LOCALE determine the descriptor to be searched.

If there are several matching buttons, the first is returned."
  (flet ((thunk (x) (if (symbolp x) (symbol-value x) x)))
    (toolbar-find-button-by-element icon 0 toolbar locale #'thunk)))

(defun toolbar-find-button-by-command (cmd &optional toolbar locale)
  "Return the position of a button invoking command CMD.
TOOLBAR and LOCALE determine the descriptor to be searched.

If there are several matching buttons, the first is returned."
  (toolbar-find-button-by-element cmd 2 toolbar locale))

(defun toolbar-find-button-by-help-string (str &optional toolbar locale)
  "Return the position of a button with help-string STR.
TOOLBAR and LOCALE determine the descriptor to be searched.

If there are several matching buttons, the first is returned.
This function will not find spacers."
  (toolbar-find-button-by-element str 3 toolbar locale))

(defun toolbar-find-button-by-element (object index toolbar locale
				       &optional thunk)
  "Return the position of a button containing OBJECT in element INDEX.
TOOLBAR and LOCALE determine the descriptor to be searched.

Optional argument THUNK is a function of one argument which is used to
normalize OBJECT for comparison.

If there are several matching buttons, the first is returned.
This function will not find spacers."
  (setq toolbar (or toolbar default-toolbar))
  (setq locale (or locale 'global))
  (if thunk (setq object (funcall thunk object)))
  (let ((desc (cdadar (specifier-spec-list toolbar locale)))
	(pos 0))
    ;; #### rewrite this as a catch ... throw
    (while (not (equal object (let ((el (aref (car desc) index)))
				(if thunk (funcall thunk el) el))))
      (setq desc (cdr desc))
      (setq pos (1+ pos)))
    (if toolbar pos nil)))

(provide 'toolbar-utils)

;;; toolbar-utils.el ends here
