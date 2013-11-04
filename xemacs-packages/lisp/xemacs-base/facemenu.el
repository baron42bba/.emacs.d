;;; facemenu.el --- create a face menu for interactively adding fonts to text

;; Copyright (c) 1994, 1995, 1996 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@gnu.ai.mit.edu>
;; Adapted-by: Mike Sperber <sperber@informatik.uni-tuebingen.de>
;; Maintainer: XEmacs Development Team
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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: FSF 20.2  (but not literally)

;;; Commentary:

;; This file defines a menu of faces (bold, italic, etc) which allows you to
;; set the face used for a region of the buffer.  Some faces also have
;; keybindings, which are shown in the menu.  Faces with names beginning with
;; "fg:" or "bg:", as in "fg:red", are treated specially.
;; Such faces are assumed to consist only of a foreground (if "fg:") or
;; background (if "bg:") color.  They are thus put into the color submenus
;; rather than the general Face submenu.  These faces can also be
;; automatically created by selecting the "Other..." menu items in the
;; "Foreground" and "Background" submenus.
;;
;; The menu also contains submenus for indentation and justification-changing
;; commands.

;;; Usage:
;; Selecting a face from the menu or typing the keyboard equivalent will
;; change the region to use that face.  If you use transient-mark-mode and the
;; region is not active, the face will be remembered and used for the next
;; insertion.  It will be forgotten if you move point or make other
;; modifications before inserting or typing anything.
;;
;; Faces can be selected from the keyboard as well.  
;; The standard keybindings are C-x M-f + letter:
;; C-x M-f i = "set italic",  C-x M-f b = "set bold", etc.
;;
;; Feel free to bind it to something more accessible, for instance:
;;  (global-set-key [f5] 'facemenu-keymap)

;;; Customization:
;; An alternative set of keybindings that may be easier to type can be set up
;; using "Alt" or "Hyper" keys.  This requires that you either have or create
;; an Alt or Hyper key on your keyboard.  On my keyboard, there is a key
;; labeled "Alt", but to make it act as an Alt key I have to put this command
;; into my .xinitrc:
;;    xmodmap -e "add Mod3 = Alt_L"
;; Or, I can make it into a Hyper key with this:
;;    xmodmap -e "keysym Alt_L = Hyper_L" -e "add Mod2 = Hyper_L"
;; Check with local X-perts for how to do it on your system.
;; Then you can define your keybindings with code like this in your .emacs:
;;   (setq facemenu-keybindings
;;    '((default     . [?\H-d])
;;      (bold        . [?\H-b])
;;      (italic      . [?\H-i])
;;      (bold-italic . [?\H-l])
;;      (underline   . [?\H-u])))
;;   (facemenu-update)
;;   (setq facemenu-keymap global-map)
;;   (define-key global-map [?\H-c] 'facemenu-set-foreground) ; set fg color
;;   (define-key global-map [?\H-C] 'facemenu-set-background) ; set bg color
;;
;; The order of the faces that appear in the menu and their keybindings can be
;; controlled by setting the variables `facemenu-keybindings' and
;; `facemenu-new-faces-at-end'.  List faces that you don't use in documents
;; (eg, `region') in `facemenu-unlisted-faces'.

;;; Known Problems:
;; Bold and Italic do not combine to create bold-italic if you select them
;; both, although most other combinations (eg bold + underline + some color)
;; do the intuitive thing.
;;
;; There is at present no way to display what the faces look like in
;; the menu itself.
;;
;; `edit-faces' shows the faces in a different order than
;; this menu, which could be confusing.  I do /not/ sort the list
;; alphabetically, because I like the default order: it puts the most
;; basic, common fonts first.
;;
;; Please send me any other problems, comments or ideas.

;;; Code:

(provide 'facemenu)

;; XEmacs
(require 'easymenu)

;;; Provide some binding for startup:
;;;###autoload(autoload 'facemenu-keymap "facemenu" nil t 'keymap)
;;;###autoload(define-key ctl-x-map "F" 'facemenu-keymap)

(defgroup facemenu nil
  "Create a face menu for interactively adding fonts to text."
  :group 'faces
  :prefix "facemenu-")

(defcustom facemenu-keybindings
  '((default     . "d")
    (bold        . "b")
    (italic      . "i")
    (bold-italic . "l") ; {bold} intersect {italic} = {l}
    (underline   . "u"))
  "Alist of interesting faces and keybindings. 
Each element is itself a list: the car is the name of the face,
the next element is the key to use as a keyboard equivalent of the menu item;
the binding is made in `facemenu-keymap'.

The faces specifically mentioned in this list are put at the top of
the menu, in the order specified.  All other faces which are defined,
except for those in `facemenu-unlisted-faces', are listed after them, 
but get no keyboard equivalents.

If you change this variable after loading facemenu.el, you will need to call
`facemenu-update' to make it take effect."
  :type '(repeat (cons face string))
  :group 'facemenu)

(defcustom facemenu-new-faces-at-end t
  "*Where in the menu to insert newly-created faces.
This should be nil to put them at the top of the menu, or t to put them
just before \"Other\" at the end."
  :type 'boolean
  :group 'facemenu)

;; XEmacs -- additional faces
(defcustom facemenu-unlisted-faces
  '(modeline region secondary-selection highlight scratch-face
    gui-button-face isearch hyperlink
    modeline modeline-buffer-id modeline-mousable modeline-mousable-minor-mode
    pointer primary-selection secondary-selection list-mode-item-selected
    text-cursor zmacs-region
    left-margin right-margin
    "^font-lock-" "^gnus-" "^message-" "^ediff-" "^term-" "^vc-"
    "^widget-" "^custom-" "^vm-")
  "*List of faces not to include in the Face menu.
Each element may be either a symbol, which is the name of a face, or a string,
which is a regular expression to be matched against face names.  Matching
faces will not be added to the menu.

You can set this list before loading facemenu.el, or add a face to it before
creating that face if you do not want it to be listed.  If you change the
variable so as to eliminate faces that have already been added to the menu,
call `facemenu-update' to recalculate the menu contents.

If this variable is t, no faces will be added to the menu.  This is useful for
temporarily turning off the feature that automatically adds faces to the menu
when they are created."
  :type '(choice (const :tag "Don't add" t)
		 (const :tag "None" nil)
		 (repeat (choice symbol regexp)))
  :group 'facemenu)

(defcustom facemenu-relevant-face-attributes
  '(foreground background font underline highlight dim blinking reverse)
  "*List of face attributes that facemenu fiddles with."
  :type '(repeat (symbol :tag "Attribute"))
  :group 'facemenu)

(defcustom facemenu-add-face-function nil
  "Function called at beginning of text to change or `nil'.
This function is passed the FACE to set and END of text to change, and must
return a string which is inserted.  It may set `facemenu-end-add-face'."
  :type '(choice (const :tag "None" nil)
		 function)
  :group 'facemenu)

(defcustom facemenu-end-add-face nil
  "String to insert or function called at end of text to change or `nil'.
This function is passed the FACE to set, and must return a string which is
inserted."
  :type '(choice (const :tag "None" nil)
		 string
		 function)
  :group 'facemenu)

(defcustom facemenu-remove-face-function nil
  "When non-`nil' function called to remove faces.
This function is passed the START and END of text to change.
May also be `t' meaning to use `facemenu-add-face-function'."
  :type '(choice (const :tag "None" nil)
		 (const :tag "Use add-face" t)
		 function)
  :group 'facemenu)

(easy-menu-define facemenu-face-menu ()
   "Menu for faces"
   `("Face"
     ["Other..." facemenu-set-face t]))

(easy-menu-define facemenu-foreground-menu ()
  "Menu for foreground colors"
  `("Foreground Color"
    ["Other..." facemenu-set-foreground t]))

(easy-menu-define facemenu-background-menu ()
  "Menu for background colors"
  `("Background Color"
    ["Other..." facemenu-set-background t]))

(easy-menu-define facemenu-size-menu ()
  "Menu for font sizes."
  '("Size"
    ["Default" facemenu-set-size-default t]
    ["Bigger" facemenu-make-larger t]
    ["Smaller" facemenu-make-smaller t]
    ["Much Bigger" facemenu-make-much-larger t]
    ["Much Smaller" facemenu-make-much-smaller t]))

(easy-menu-define facemenu-special-menu ()
  "Menu for non-face text-properties."
  '("Special"
    ["Read-Only" facemenu-set-read-only t]
    ["Invisible" facemenu-set-invisible t]
    ["Intangible" facemenu-set-intangible t]
    ["Remove Special" facemenu-remove-special t]))

(easy-menu-define facemenu-justification-menu ()
  "Menu for text justification commands."
  '("Justification"
    ["Center" set-justification-center t]
    ["Full" set-justification-full t]
    ["Right" set-justification-right t]
    ["Unfilled" set-justification-none t]))

(easy-menu-define facemenu-indentation-menu
  ()
  "Submenu for indentation commands."
  '("Indentation"
    ["Indent More" increase-left-margin t]
    ["Indent Less" decrease-left-margin t]
    ["Indent Right More" increase-right-margin t]
    ["Indent Right Less" decrease-right-margin t]))

;;;###autoload
(defvar facemenu-menu nil
  "Facemenu top-level menu keymap.")

(defun facemenu-update-facemenu-menu ()
  (easy-menu-define facemenu-menu ()
   "Facemenu top-level menu"
   (list "Text Properties"
	 facemenu-face-menu
	 facemenu-foreground-menu
	 facemenu-background-menu
	 facemenu-size-menu
	 facemenu-special-menu
	 "---"
	 facemenu-justification-menu
	 facemenu-indentation-menu
	 "---"
	 ["Remove Properties" facemenu-remove-props t]
	 ["List Properties" list-text-properties-at t]
	 ["Display Faces" edit-faces t]
	 ["Display Colors" list-colors-display t])))

;;;###autoload
(defvar facemenu-keymap
  (let ((map (make-sparse-keymap "Set face")))
    (define-key map ?o 'facemenu-set-face)
    map)
  "Keymap for face-changing commands.
`Facemenu-update' fills in the keymap according to the bindings
requested in `facemenu-keybindings'.")
;;;###autoload
(defalias 'facemenu-keymap facemenu-keymap)


;;; Internal Variables

(defvar facemenu-color-alist nil
  ;; Don't initialize here; that doesn't work if preloaded.
  "Alist of colors, used for completion.
If null, `facemenu-read-color' will set it.")

(defun facemenu-update ()
  "Add or update the \"Face\" menu in the menu bar.
You can call this to update things if you change any of the menu configuration
variables."
  (interactive)

  ;; Add each defined face to the menu.
  (facemenu-iterate 'facemenu-add-new-face
		    (facemenu-complete-face-list facemenu-keybindings))
  (facemenu-update-facemenu-menu)
  
  ;; Global bindings:
  (if (string-match "XEmacs" emacs-version)
      (easy-menu-change '("Edit") (car facemenu-menu) (cdr facemenu-menu))
    (define-key global-map [C-down-mouse-2] 'facemenu-menu)))

;;;###autoload
(defun facemenu-set-face (face &optional start end)
  "Add FACE to the region or next character typed.
It will be added to the top of the face list; any faces lower on the list that
will not show through at all will be removed.

Interactively, the face to be used is read with the minibuffer.

If the region is active and there is no prefix argument,
this command sets the region to the requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification." 
  (interactive (list (read-face-name "Use face: ")))
  (setq zmacs-region-stays t) ; XEmacs
  (barf-if-buffer-read-only)
  (facemenu-add-new-face face)
  (facemenu-update-facemenu-menu) ; XEmacs
  (if (and (region-active-p)
	   (not current-prefix-arg))
      (let ((start (or start (region-beginning)))
	    (end (or end (region-end))))
	(facemenu-add-face face start end))
    (facemenu-self-insert-face face)))

;;;###autoload
(defun facemenu-set-foreground (color &optional start end)
  "Set the foreground color of the region or next character typed.
The color is prompted for.  A face named `fg:color' is used \(or created).
If the region is active, it will be set to the requested face.  If
it is inactive \(even if mark-even-if-inactive is set) the next
character that is typed \(via `self-insert-command') will be set to
the selected face.  Moving point or switching buffers before
typing a character cancels the request." 
  (interactive (list (facemenu-read-color "Foreground color: ")))
  (setq zmacs-region-stays t)
  (let ((face (intern (concat "fg:" color))))
    (or (facemenu-get-face face)
	(error "Unknown color: %s" color))
    (facemenu-set-face face start end)))

;;;###autoload
(defun facemenu-set-background (color &optional start end)
  "Set the background color of the region or next character typed.
The color is prompted for.  A face named `bg:color' is used \(or created).
If the region is active, it will be set to the requested face.  If
it is inactive \(even if mark-even-if-inactive is set) the next
character that is typed \(via `self-insert-command') will be set to
the selected face.  Moving point or switching buffers before
typing a character cancels the request." 
  (interactive (list (facemenu-read-color "Background color: ")))
  (setq zmacs-region-stays t)
  (let ((face (intern (concat "bg:" color))))
    (or (facemenu-get-face face)
	(error "Unknown color: %s" color))
    (facemenu-set-face face start end)))

;;;###autoload
(defun facemenu-set-face-from-menu (face start end)
  "Set the face of the region or next character typed.
This function is designed to be called from a menu; the face to use
is the menu item's name.

If the region is active and there is no prefix argument,
this command sets the region to the requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification." 
  (interactive (list last-command-event
		     (if (and (region-active-p)
			      (not current-prefix-arg))
			 (region-beginning))
		     (if (and (region-active-p)
			      (not current-prefix-arg))
			 (region-end))))
  (barf-if-buffer-read-only)
  (setq zmacs-region-stays t) ; XEmacs
  (facemenu-get-face face)
  (if start
      (facemenu-add-face face start end)
    (facemenu-self-insert-face face))) ; XEmacs

;; XEmacs
(defun facemenu-self-insert-face (face)
  (setq self-insert-face (cond
			  ((null self-insert-face) face)
			  ((consp self-insert-face)
			   (facemenu-active-faces (cons face self-insert-face)))
			  (t
			   (facemenu-active-faces (list face self-insert-face))))
	self-insert-face-command this-command))

(defun facemenu-face-strip-size (face)
  "Create a symbol from the name of FACE devoid of size information,
i.e. remove all larger- and smaller- prefixes."
  (let* ((face-symbol (face-name face))
	 (face-name (symbol-name face-symbol))
	 (old-name face-name)
	 new-name)
    (while
	(not (string-equal
	      old-name
	      (setq new-name (replace-in-string old-name "^larger-" ""))))
      (setq old-name new-name))
    
    (while
	(not (string-equal
	      old-name
	      (setq new-name (replace-in-string old-name "^smaller-" ""))))
      (setq old-name new-name))
    
    (if (string-equal new-name face-name)
	face-symbol
      (intern new-name))))

(defun facemenu-face-default-size (face)
  (cond ((null face) nil)
	((consp face) (mapcar 'facemenu-face-strip-size face))
	(t (facemenu-face-strip-size face))))

;; This file uses `put-text-property' all over.  All of these calls
;; have been changed to `add-text-properties' in FSF, but I don't see
;; any reason to copy that change.

;;;###autoload
(defun facemenu-set-size-default (start end)
  (interactive "_r")
  (put-text-property start end 'size nil)
  (alter-text-property start end 'face 'facemenu-face-default-size))

(defun facemenu-ensure-size-property (start end)
  "Ensure that the text between START and END has a 'size text property.
If it is not present, it is set to 0."
  (let ((start start)
	pos bound)
    (while (setq pos (text-property-any start end 'size nil))
      (setq bound (or (text-property-not-all pos end 'size nil) end))
      (put-text-property pos bound 'size 0))))

(defun facemenu-sized-face (face size)
  "Make a face FACE larger or smaller according to SIZE.
If SIZE is positive, it calls `make-face-larger' SIZE times,
else it calls `make-face-smaller' -SIZE times."
  (if (zerop size)
      face
    (let ((name (symbol-name face))
	  (measure size)
	  (change-face 'make-face-larger)
	  prefix)

      (if (> measure 0)
	  (setq prefix "larger-")
	(setq prefix "smaller-")
	(setq measure (- measure))
	(setq size (- size))
	(setq change-face 'make-face-smaller))

      (while (not (zerop measure))
	(setq name (concat prefix name))
	(setq measure (1- measure)))

      (let ((symbol (intern name)))
	(or (find-face symbol)
	    (let ((face (copy-face face symbol)))
	      (while (not (zerop size))
		(funcall change-face face)
		(setq size (1- size)))
	      face))))))

(defun facemenu-adjust-face-sizes (face)
  (cond
   ((null face) (facemenu-sized-face 'default size))
   ((consp face) (mapcar 
		  #'(lambda (face)
		      (facemenu-sized-face (facemenu-face-strip-size face)
					    size))
		  face))
   ;;[BV  9-Feb-97] strip-face from this face too, please!
   (t (facemenu-sized-face (facemenu-face-strip-size face) size))))

(defun facemenu-adjust-size (from to)
  "Adjust the size of the text between FROM and TO according
to the values of the 'size property in that region."
  (let ((pos from)
	bound size)
    (while (< pos to)
      (setq size (get-text-property pos 'size))
      (setq bound (or (text-property-not-all pos to 'size size) to))
      (alter-text-property pos bound 'face 'facemenu-adjust-face-sizes)
      (setq pos bound))))

(defun facemenu-change-size (from to f)
  (facemenu-ensure-size-property from to)
  (alter-text-property from to 'size f)
  (facemenu-adjust-size from to))

;;;###autoload
(defun facemenu-make-larger (from to)
  (interactive "_r")
  (facemenu-change-size from to '1+))

;;;###autoload
(defun facemenu-make-smaller (from to)
  (interactive "_r")
  (facemenu-change-size from to '1-))

;;;###autoload
(defun facemenu-make-much-larger (from to)
  (interactive "_r")
  (facemenu-change-size from to #'(lambda (s) (+ 5 s))))

;;;###autoload
(defun facemenu-make-much-smaller (from to)
  (interactive "_r")
  (facemenu-change-size from to #'(lambda (s) (- s 5))))

;;;###autoload
(defun facemenu-set-invisible (start end)
  "Make the region invisible.
This sets the `invisible' text property; it can be undone with
`facemenu-remove-special'."
  (interactive "_r")
  (put-text-property start end 'invisible t))

;;;###autoload
(defun facemenu-set-intangible (start end)
  "Make the region intangible: disallow moving into it.
This sets the `intangible' text property; it can be undone with
`facemenu-remove-special'."
  (interactive "_r")
  ;; #### This does nothing in XEmacs.  Should use atomic-extents, but
  ;; why bother, when that's broken, too?
  (put-text-property start end 'intangible t))

;;;###autoload
(defun facemenu-set-read-only (start end)
  "Make the region unmodifiable.
This sets the `read-only' text property; it can be undone with
`facemenu-remove-special'."
  (interactive "_r")
  (put-text-property start end 'read-only t))

;;;###autoload
(defun facemenu-remove-props (start end)
  "Remove all text properties that facemenu added to region."
  (interactive "*_r") ; error if buffer is read-only despite the next line.
  (let ((inhibit-read-only t))
    (remove-text-properties 
     start end '(face nil invisible nil intangible nil 
		 read-only nil category nil size nil))))

;;;###autoload
(defun facemenu-remove-special (start end)
  "Remove all the \"special\" text properties from the region.
These special properties include `invisible', `intangible' and `read-only'."
  (interactive "*_r") ; error if buffer is read-only despite the next line.
  (let ((inhibit-read-only t))
    (remove-text-properties 
     start end '(invisible nil intangible nil read-only nil))))

;;;###autoload
(defun list-text-properties-at (p)
  "Pop up a buffer listing text-properties at LOCATION."
  (interactive "d")
  (let ((props (text-properties-at p))
	category
	str)
    (if (null props)
	(message "None")
      (if (and (not (cdr (cdr props)))
	       (not (eq (car props) 'category))
	       (< (length (setq str (format "Text property at %d:  %s  %S"
					    p (car props) (car (cdr props)))))
		  (frame-width)))
	  (message "%s" str)
	(with-output-to-temp-buffer "*Text Properties*"
	  (princ (format "Text properties at %d:\n\n" p))
	  (while props
	    (if (eq (car props) 'category)
		(setq category (car (cdr props))))
	    (princ (format "%-20s %S\n"
			   (car props) (car (cdr props))))
	    (setq props (cdr (cdr props))))
	  (if category
	      (progn
		(setq props (symbol-plist category))
		(princ (format "\nCategory %s:\n\n" category))
		(while props
		  (princ (format "%-20s %S\n"
				 (car props) (car (cdr props))))
		  (if (eq (car props) 'category)
		      (setq category (car (cdr props))))
		  (setq props (cdr (cdr props)))))))))))

;;;###autoload
(defalias 'facemenu-read-color 'read-color)

(defun facemenu-canonicalize-color (c)
  (downcase (replace-in-string c " " "")))

(defun facemenu-unique (list)
  "Uniquify LIST, deleting elements using `delete'.
Return the list with subsequent duplicate items removed by side effects."
  (let ((list list))
    (while list
      (setq list (setcdr list (delete (car list) (cdr list))))))
  list)

;;;###autoload
(defun list-colors-display (&optional list)
  "Display names of defined colors, and show what they look like.
If the optional argument LIST is non-nil, it should be a list of
colors to display.  Otherwise, this command computes a list
of colors that the current display can handle."
  (interactive)
  (setq list
	(facemenu-unique
	 (mapcar 'facemenu-canonicalize-color
		 (mapcar 'car (read-color-completion-table)))))
  (with-output-to-temp-buffer "*Colors*"
    (save-excursion
      (set-buffer standard-output)
      (let ((facemenu-unlisted-faces t)
	    s)
	(while list
	  (if (not (string-match "[0-9]" (car list)))
	      (progn
		(setq s (point))
		(insert (car list))
		(indent-to 20)
		(put-text-property s (point) 'face 
				   (facemenu-get-face 
				    (intern (concat "bg:" (car list)))))
		(setq s (point))
		(insert "  " (car list) "\n")
		(put-text-property s (point) 'face 
				   (facemenu-get-face 
				    (intern (concat "fg:" (car list)))))))
	  (setq list (cdr list)))))))

(fset 'facemenu-color-values 
	(if (fboundp 'x-color-values)
	    'x-color-values
	  #'(lambda (color)
	      (color-instance-rgb-components
	       (make-color-instance color)))))

(defun facemenu-color-equal (a b)
  "Return t if colors A and B are the same color.
A and B should be strings naming colors.
This function queries the window-system server to find out what the
color names mean.  It returns nil if the colors differ or if it can't
determine the correct answer."
  (cond ((equal a b) t)
	((and (equal (facemenu-color-values a)
		     (facemenu-color-values b))))))

(defun facemenu-add-face (face &optional start end)
  "Add FACE to text between START and END.
For each section of that region that has a different face property, FACE will
be consed onto it, and other faces that are completely hidden by that will be
removed from the list.

As a special case, if FACE is `default', then the region is left with NO face
text property.  Otherwise, selecting the default face would not have any
effect."
  (interactive "*_xFace:\nr")
  (if (and (eq face 'default)
	   (not (eq facemenu-remove-face-function t)))
      (if facemenu-remove-face-function
	  (funcall facemenu-remove-face-function start end)
	(if (and start (< start end))
	    (remove-text-properties start end '(face default))
	  (setq self-insert-face 'default
		self-insert-face-command this-command)))
    (if facemenu-add-face-function
	(save-excursion
	  (if end (goto-char end))
	  (save-excursion
	    (if start (goto-char start))
	    (insert-before-markers
	     (funcall facemenu-add-face-function face end)))
	  (if facemenu-end-add-face
	      (insert (if (stringp facemenu-end-add-face)
			  facemenu-end-add-face
			(funcall facemenu-end-add-face face)))))
      (if (and start (< start end))
	  (let ((part-start start) part-end)
	    (while (not (= part-start end))
	      (setq part-end (next-single-property-change part-start 'face
							  nil end))
	      (let ((prev (get-text-property part-start 'face)))
		(put-text-property part-start part-end 'face
				   (if (null prev)
				       face
				     (facemenu-active-faces
				      (cons face
					    (if (listp prev)
						prev
					      (list prev)))))))
	      (setq part-start part-end)))
	(setq self-insert-face (if (eq last-command self-insert-face-command)
				   (cons face (if (listp self-insert-face)
						  self-insert-face
						(list self-insert-face)))
				 face)
	      self-insert-face-command this-command)))))

;; XEmacs
(defun facemenu-face-attributes (face)
  "Create a vector of the relevant face attributes of face FACE."
  (mapvector #'(lambda (prop)
		 (face-property-instance face prop))
	     facemenu-relevant-face-attributes))

(defun facemenu-active-faces (face-list)
  "Return from FACE-LIST those faces that would be used for display.
This means each face attribute is not specified in a face earlier in FACE-LIST
and such a face is therefore active when used to display text."
  (let* ((mask-atts (copy-sequence (facemenu-face-attributes (car face-list))))
	 (default-atts (facemenu-face-attributes 'default))
	 (active-list (list (car face-list)))
	 (face-list (cdr face-list))
	 (mask-len (length mask-atts)))
    (while face-list
      (if (let ((face-atts (facemenu-face-attributes (car face-list)))
		(i mask-len)
		(useful nil))
	    (while (>= (setq i (1- i)) 0)
	      (if (and (aref face-atts i)
		       (or (not (aref mask-atts i))
			   (eq (aref mask-atts i) (aref default-atts i)))
		       (not (eq (aref face-atts i) (aref default-atts i))))
		  (aset mask-atts i (setq useful t))))
	    useful)
	  (setq active-list (cons (car face-list) active-list)))
      (setq face-list (cdr face-list)))
    (nreverse active-list)))

;;;###autoload
(defun facemenu-get-face (symbol)
  "Make sure FACE exists.
If not, create it and add it to the appropriate menu.  Return the symbol.

If this function creates a face named `fg:color', then it sets the
foreground to that color.  Likewise, `bg:color' means to set the
background.  In either case, if the color is undefined, no color is
set and a warning is issued."
  (let ((name (symbol-name symbol))
	foreground)
    (cond ((find-face symbol))
	  ((or (setq foreground (string-match "^fg:" name))
	       (string-match "^bg:" name))
	   (let* ((face (make-face symbol))
		  (color (substring name 3)))
	     (if (color-instance-p (make-color-instance color))
		 (if foreground
		     (set-face-foreground face color)
		   (set-face-background face color))
	       (warn "Color `%s' undefined" color))))
	  (t (make-face symbol))))
  symbol)

(defun facemenu-menu-has-face (menu face-name)
  "Check if menu MENU has an entry for face named by string FACE-NAME.
Returns entry if successful."
  (facemenu-iterate
   #'(lambda (m)
       (and (vectorp m) 
	    (string-equal face-name (aref m 0))
	    m))
   (cdr menu)))

(defun facemenu-insert-menu-entry (menu before-entry name function)
  "Insert menu item with name NAME and associated function FUNCTION
into menu MENU before entry BEFORE-ENTRY."
  (when (featurep 'menubar)
    (while (not (eq (cadr menu) before-entry))
      (setq menu (cdr menu)))
    (setcdr menu (cons (vector name function t) (cdr menu)))))

(defun facemenu-add-new-face (face)
  "Add a FACE to the appropriate Face menu.
Automatically called when a new face is created."
  (let* ((name (symbol-name face))
	 menu menu-value
	 (key (cdr (assoc face facemenu-keybindings))))
    (cond ((eq t facemenu-unlisted-faces))
	  ((string-match "^fg:" name)
	   (setq name (substring name 3)
		 docstring (format
			    "Select foreground color %s for subsequent insertion."
			    name)
		 menu 'facemenu-foreground-menu))
	  ((string-match "^bg:" name)
	   (setq name (substring name 3)
		 docstring (format
			    "Select background color %s for subsequent insertion."
			    name)
		 menu 'facemenu-background-menu))
	  (t
	   (setq docstring (format "Select face `%s' for subsequent insertion."
				   name)
		 menu 'facemenu-face-menu)))
    (setq menu-value (symbol-value menu))
    (cond ((eq t facemenu-unlisted-faces))
	  ((memq face facemenu-unlisted-faces))
	  ((string-match "^larger-" name))
	  ((string-match "^smaller-" name))
	  ;; Test against regexps in facemenu-unlisted-faces
	  ((let ((unlisted facemenu-unlisted-faces)
		 (matched nil))
	     (while (and unlisted (not matched))
	       (if (and (stringp (car unlisted))
			(string-match (car unlisted) name))
		   (setq matched t)
		 (setq unlisted (cdr unlisted))))
	     matched))
	  (key ; has a keyboard equivalent.  These go at the front.
	   (let ((function (intern (concat "facemenu-set-" name))))
	     (fset function
		   `(lambda ()
		      ,docstring
		      (interactive "_")
		      (facemenu-set-face (quote ,face))))
	     (define-key 'facemenu-keymap key function)
	     (unless (facemenu-menu-has-face menu-value name)
	       (set menu
		    (cons (car menu-value)
			  (cons (vector name function t)
				(cdr menu-value)))))))
	  ((facemenu-menu-has-face menu-value name))
	  (t   ; No keyboard equivalent.  Figure out where to put it:
	   (let ((before-entry
		   (or (and facemenu-new-faces-at-end
				     (facemenu-menu-has-face menu-value "Other..."))
		       (cadr menu-value))))
	     (facemenu-insert-menu-entry
	      menu-value before-entry name
	      (` (facemenu-set-face (quote (, face)))))))))
  nil) ; Return nil for facemenu-iterate

(defun facemenu-complete-face-list (&optional oldlist)
  "Return list of all faces that look different.
Starts with given ALIST of faces, and adds elements only if they display 
differently from any face already on the list.
The faces on ALIST will end up at the end of the returned list, in reverse 
order."
  (let ((list (nreverse (mapcar 'car oldlist))))
    (facemenu-iterate 
     (lambda (new-face) 
       (if (not (memq new-face list))
	   (setq list (cons new-face list)))
       nil)
     (nreverse (face-list)))
    list))

(defun facemenu-iterate (func iterate-list)
  "Apply FUNC to each element of LIST until one returns non-nil.
Returns the non-nil value it found, or nil if all were nil."
  (while (and iterate-list (not (funcall func (car iterate-list))))
    (setq iterate-list (cdr iterate-list)))
  (car iterate-list))

(facemenu-update)

;;; facemenu.el ends here
