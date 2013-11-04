;;; whitespace-visual-mode.el -- minor mode for making whitespace visible

;; Copyright (C) 1994 - 2005 Dr. Heiko Muenkel

;; Author: Dr. Heiko Muenkel <muenkel@tnt.uni-hannover.de>
;; Keywords: modes, extensions

;; This file is part of XEmacs.

;;  XEmacs is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2, or (at your
;;  option) any later version.

;;  XEmacs is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with XEmacs; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;  02111-1307, USA.

;;; Synched up with: [[ FSF 19.34. ]] Not in FSF.
 
;;; Commentary:

;; $Id: whitespace-visual-mode.el,v 1.1 2005/02/14 21:28:06 viteno Exp $
;; Description:
;;
;;	This is a minor mode, which highlights whitespaces (blanks and
;;	tabs) with different faces, so that it is easier to
;;	distinguish between them.  
;;	Toggle the mode with: M-x whitespace-visual-mode 
;;	or with: M-x whitespace-visual-incremental-mode
;;	The second one should be used in big files.
;;
;;	If you want to know how the whitespaces are highlighted then
;;	type: M-x whitespace-visual-show-faces
;;
;;	There are 2 hook variables `whitespace-visual-incremental-mode-hook'
;;	and `whitespace-visual-mode-hook' to customize the mode.
;;
;;	Look at the variable `whitespace-visual-chars', if you only want to
;;	highlight tabs or blanks and not both.
;;
;;	Set `whitespace-visual-install-toolbar-icon' to t, if you want a
;;	toolbar icon for this mode.
;;
;;	Set `whitespace-visual-install-submenu' to t, if you want a submenu
;;	for this mode. Sorry, at the moment there is no menu for the
;;	GNU Emacs. 
;;
;;	Thanks to Mike Scheidler for the toolbar icon code.
;; 
;; Changes:
;;	2005-02-10:
;;	The prefix of the package is now whitespace-visual
;;	instead of only whitespace. The new prefix whitespace-visual
;;	was choosen to avoid conflicts with another package called
;;	whitespace. Adding a whitespace toolbar icon should now work
;;	again. The whitespace submenu is now in the "Tools" menu
;;	instead of the old Apps menu.
;;
;; Installation:
;;   
;;	This file is part of the XEmacs package system. The following
;;	instructions belong to older versions of this mode. 
;;
;;      Put the files whitespace-visual-mode.el and adapt.el in one of your
;; 	load-path directories and the following lines (without the
;; 	comment signs) in your .emacs (adapt.el is already in the
;;	XEmacs 19.12).
;;
;;     (autoload 'whitespace-visual-mode "whitespace-visual-mode" 
;;       "Toggle whitespace visual mode.
;;	With arg, turn whitespace visual mode on iff arg is positive.
;;	In whitespace visual mode the different whitespaces (tab, blank return)
;;	are highlighted with different faces. The faces are:
;;	`whitespace-visual-blank-face', `whitespace-visual-tab-face' and 
;;	`whitespace-visual-return-face'."
;;	t)
;;
;;     (autoload 'whitespace-visual-incremental-mode "whitespace-visual-mode" 
;;	  "Toggle whitespace visual incremental mode.
;;      With arg, turn whitespace visual incremental mode on iff 
;;      arg is positive. In whitespace visual incremental mode 
;;	the different whitespaces (tab and blank) are highlighted with 
;;	different faces. The faces are: `whitespace-blank-face' and 
;;      `whitespace-visual-tab-face'. Use the command 
;;	`whitespace-visual-show-faces' to show their values.
;;	In this mode only these tabs and blanks are highlighted, which are in 
;;	the region from (point) - (window-height) to (point) + 
;;	(window-height)."

;;; Code:

(provide 'whitespace-visual-mode)
;; We don't need adapt
;; (require 'adapt)

;;; variables:

(defgroup whitespace-visual nil
  "Minor mode for making whitespaces visible"
  :group 'outlines
  :group 'matching)


(defcustom whitespace-visual-mode nil
  "Non-nil, if the `whitespace-visual-mode' is active."
  :type 'boolean
  :set (lambda (symbol value)
	 (whitespace-visual-mode (or value 0)))
  :require 'whitespace-visual-mode
  :initialize 'custom-initialize-default
  :group 'whitespace-visual)

(make-variable-buffer-local 'whitespace-visual-mode)

;;;###autoload
(defcustom whitespace-visual-mode-line-string " WSP"
  "*String displayed on the modeline when whitespace-visual-mode is active.
Set this to nil if you don't want a modeline indicator."
  :group 'whitespace-visual
  :type 'string)

;;;###autoload
(defcustom whitespace-visual-incremental-mode-line-string " WSPI"
  "*String displayed on the modeline when whitespace-visual-incremental-mode
is active. Set this to nil if you don't want a modeline indicator."
  :group 'whitespace-visual
  :type 'string)

(defcustom whitespace-visual-chars 'tabs-and-blanks
  "*Determines, which whitespaces are highlighted.
Valid values are:
'tabs-and-blanks => tabs and blanks are highlighted;
'tabs            => only tabs are highlighted;
'blanks          => only blanks are highlighted;.

Changing this variable during the whitespace-visual-*-mode is active could lead
to wrong highlighted whitespaces."
  :type '(radio (const tabs-and-blanks)
		(const tabs)
		(const blanks))
  :group 'whitespace-visual)

(make-variable-buffer-local 'whitespace-visual-chars)

(defcustom whitespace-visual-mode-hook nil
  "*Run after the `whitespace-visual-mode' is switched on."
  :type 'hook
  :group 'whitespace-visual)

(defcustom whitespace-visual-incremental-mode-hook nil
  "*Run after the `whitespace-visual-incremental-mode' is switched on."
  :type 'hook
  :group 'whitespace-visual)

;; We don't need adapt.
;; (if (adapt-xemacsp)
;; (progn

(defcustom whitespace-visual-install-toolbar-icon nil
  "Set it to t, if a toolbar icon should be installed during loading this file.
The icon calls the function 'whitespace-visual-toolbar-function'."
  :type 'boolean
  :group 'whitespace-visual)

(defcustom whitespace-visual-install-submenu nil
  "Set it to t, if a submenu should be installed during loading this file."
  :type 'boolean
  :group 'whitespace-visual)

;; ))


(defcustom whitespace-visual-toolbar-function 
  'whitespace-visual-incremental-mode
  "*The toolbar icon for the whitespace visual mode calls this function.
Valid values are: 
 'whitespace-visual--mode and 'whitespace-visual-incremental-mode."
  :type 'function
  :group 'whitespace-visual)

(defcustom whitespace-visual-blank-and-tab-search-string "\\( \\)\\|\\(\t\\)"
  "The regexp used to search for tabs and blanks."
  :type 'regexp
  :group 'whitespace-visual)

(defcustom whitespace-visual-tab-search-string "\t"
  "The search string used to find tabs."
  :type 'string
  :group 'whitespace-visual)

(defcustom whitespace-visual-blank-search-string " "
  "The search string used to find blanks."
  :type 'string
  :group 'whitespace-visual)

(defface whitespace-visual-blank-face
  '((t
     (:background "LightBlue1")))
  "Face to show blanks with"
  :group 'whitespace-visual)

(defface whitespace-visual-tab-face
  '((t
     (:background "yellow" :underline t)))
  "Face to show TABs with"
  :group 'whitespace-visual)

(defun whitespace-visual-show-faces ()
  "Shows the faces used by the `whitespace-visual-mode'."
  (interactive)
  (save-excursion
    (let ((actual-buffer-name (buffer-name (current-buffer)))
	  (actual-whitespace-chars whitespace-visual-chars)
	  (whitespace-visual-mode-active
	   (or whitespace-visual-mode 
	       whitespace-visual-incremental-mode))
	  (buffer (get-buffer-create "*Help*")))
      (set-buffer buffer)
      (setq whitespace-visual-chars actual-whitespace-chars)
      (delete-region (point-min) (point-max))
      (insert "In the whitespace visual minor mode\n"
	      " this \" ")
      (whitespace-visual-highlight-region (1- (point)) (point))
      (insert "\" is a blank, highlighted with "
	      "`whitespace-visual-blank-face' and\n"
	      " this \"\t")
      (whitespace-visual-highlight-region (1- (point)) (point))
      (insert "\" is a tab,  highlighted with `whitespace-visual-tab-face'.")
      
      (newline 2)
      (if (eq whitespace-visual-chars 'blanks)
	  (insert 
	   "The highlighting of tabs is switched off.\n")
	(if (eq whitespace-visual-chars 'tabs)
	    (insert
	     "The highlighting of blanks is switched off.\n")))
      (newline)
      (if whitespace-visual-mode-active
	  (insert "A whitespace-visual minor mode is active in the buffer\n  "
		  actual-buffer-name
		  ".\n")
	(insert "No whitespace-visual minor mode is active in the buffer\n  "
		actual-buffer-name
		".\n"))
      (show-temp-buffer-in-current-frame buffer)
      )))

;;;
(defun whitespace-visual-highlight-chars-in-region (char-string from to face)
  "Highlights the CHAR-STRING in the region from FROM to TO with the FACE."
  (while (search-forward char-string end t)
    (let ((extent))
      (cond ((match-beginning 0)
	     (setq extent (make-extent (match-beginning 0) (match-end 0)))
	     (set-extent-face extent face)
	     ))
      (set-extent-property extent 'start-open t)
      (set-extent-property extent 'end-open t)
      )))

(defun whitespace-visual-highlight-region (from to)
  "Highlights the whitespaces in the region from FROM to TO."
  (let ((start (min from to))
	(end (max from to)))
    (save-excursion
      ;;    (message "Highlighting tabs and blanks...")
      (goto-char start)
      (cond ((eq whitespace-visual-chars 'tabs-and-blanks)
	     (while (search-forward-regexp 
		     whitespace-visual-blank-and-tab-search-string end t)
	       (let ((extent))
		 (cond ((match-beginning 1) ; blanks ?
			(setq extent (make-extent (match-beginning 1) 
						  (match-end 1)))
			(set-extent-face extent 'whitespace-visual-blank-face)
			)
		       ((match-beginning 2) ; tabs ?
			(setq extent (make-extent (match-beginning 2) 
						  (match-end 2)))
			(set-extent-face extent 'whitespace-visual-tab-face)
			)
		       )
		 (set-extent-property extent 'start-open t)
		 (set-extent-property extent 'end-open t)
		 )))
	    ((eq whitespace-visual-chars 'tabs)
	     (whitespace-visual-highlight-chars-in-region
	      whitespace-visual-tab-search-string 
	      from 
	      to
	      'whitespace-visual-tab-face))
	    ((eq whitespace-visual-chars 'blanks)
	     (whitespace-visual-highlight-chars-in-region 
	      whitespace-visual-blank-search-string 
	      from 
	      to
	      'whitespace-visual-blank-face))
	    (t (error "ERROR: Bad value of whitespace-visual-highlight-char")))
      )))

(defun whitespace-visual-highlight-buffer ()
  "Highlights the whitespaces in the current buffer."
  (whitespace-visual-highlight-region (point-min) (point-max))
)

(defsubst whitespace-visual-find-next-highlighted-region (from to)
  "Returns nil or the next highlighted region."
  (map-extents '(lambda (extent dummy)
		 (if (extent-property extent
				      'whitespace-visual-highlighted-region)
		     extent))
	       nil
	       from
	       to))

(defun whitespace-visual-incremental-highlight (from to)
  "Highlights the region from FROM to TO incremental."
  (save-excursion
    (goto-char from)
    (let ((extent (extent-at (point) nil 'whitespace-visual-highlighted-region))
	  (next-extent nil)
	  (start nil))
      (while (< (point) to)
	(if extent
	    (goto-char (extent-end-position extent)))
	(if (< (point) to)
	    (progn
	      (setq start (point))
	      
	      (setq next-extent (whitespace-visual-find-next-highlighted-region 
				 start
				 to))
	      (if extent
		  (if next-extent
		      (progn
			(set-extent-endpoints extent 
					      (extent-start-position extent)
					      (extent-end-position next-extent)
					      )
			(whitespace-visual-highlight-region start
						     (1-
						      (extent-start-position
						       next-extent)))
			(delete-extent next-extent))
		    (set-extent-endpoints extent
					  (extent-start-position extent)
					  to)
		    (whitespace-visual-highlight-region start to))
		(if next-extent
		    (progn
		      (setq extent next-extent)
		      (whitespace-visual-highlight-region 
		       start 
		       (1- (extent-start-position
			    next-extent)))
		      (set-extent-endpoints extent
					    start
					    (extent-end-position next-extent)))
		  (setq extent (make-extent start to))
		  (set-extent-property extent
				       'whitespace-visual-highlighted-region t)
		  (whitespace-visual-highlight-region start to)))
	      ))))))


(defun whitespace-visual-highlight-window ()
  "Highlights the whitespaces in the current window."
  (whitespace-visual-incremental-highlight
   (save-excursion
     (forward-line (- (window-height)))
     (point))
   (save-excursion
     (forward-line (window-height))
     (point))))

(defun whitespace-visual-dehighlight-region (start end)
  "Dehighlights the whitespaces in the region from START to END."
  (map-extents '(lambda (extent dummy)
		  (if (or (eq (extent-face extent)
			      'whitespace-visual-blank-face)
			  (eq (extent-face extent)
			      'whitespace-visual-tab-face)
			  (extent-property
			   extent 
			   'whitespace-visual-highlighted-region))
		      (progn
			(delete-extent extent)
			nil)))
	       nil
	       start
	       end
	       )
  )

(defun whitespace-visual-dehighlight-buffer ()
  "Dehighlights the whitespaces in the current buffer."
  (whitespace-visual-dehighlight-region (point-min) (point-max))
  )

(defun whitespace-visual-highlight-after-change-function (beg end len)
  "Called, when any modification is made to buffer text.  Highlights
the whitespaces (blanks and tabs) in the region from BEG to
END. LEN isn't used, but provided from the after-change hook."
  (if (or (eq beg end)
	  (null whitespace-visual-mode))
      nil
    (whitespace-visual-dehighlight-region beg end)
    (whitespace-visual-highlight-region beg end)))

;;;###autoload
(defun whitespace-visual-mode (&optional arg)
  "Toggle whitespace visual mode.
With arg, turn whitespace visual mode on iff arg is positive.
In whitespace visual mode the different whitespaces (tab and blank)
are highlighted with different faces. The faces are:
`whitespace-visual-blank-face' and `whitespace-visual-tab-face'.
Use the command `whitespace-visual-show-faces' to show their values."
  (interactive "P")
  (setq whitespace-visual-mode
	(if (null arg) (not whitespace-visual-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (and whitespace-visual-mode whitespace-visual-incremental-mode)
      (progn
	(whitespace-visual-incremental-highlight (point-min) (point-max))
	(setq whitespace-visual-incremental-mode nil)
	(remove-hook 'post-command-hook 'whitespace-visual-highlight-window)
	(run-hooks 'whitespace-visual-mode-hook)
	)
    (setq whitespace-visual-incremental-mode nil)
    (remove-hook 'post-command-hook 'whitespace-visual-highlight-window)
    (redraw-modeline) ;(force-mode-line-update)
    (if whitespace-visual-mode
	(progn
	  (whitespace-visual-highlight-buffer)
	  (make-local-variable 'after-change-functions)
	  (add-hook 'after-change-functions 
		    'whitespace-visual-highlight-after-change-function)
	  (run-hooks 'whitespace-visual-mode-hook))
      (whitespace-visual-dehighlight-buffer)
      (remove-hook 'after-change-functions 
		   'whitespace-visual-highlight-after-change-function)
      (remove-hook 'post-command-hook 'whitespace-visual-highlight-window)
      )))

(defvar whitespace-visual-incremental-mode nil
  "Non-nil, if the `whitespace-visual-incremental-mode' is active.")

(make-variable-buffer-local 'whitespace-visual-incremental-mode)

;;;###autoload
(defun whitespace-visual-incremental-mode (&optional arg)
  "Toggle whitespace visual incremental mode.
With arg, turn whitespace-visual incremental mode on iff arg is positive.
In whitespace visual incremental mode the different whitespaces (tab and
blank) are highlighted with different faces. The faces are:
`whitespace-visual-blank-face' and `whitespace-visual-tab-face'.
Use the command `whitespace-visual-show-faces' to show their values.
In this mode only these tabs and blanks are highlighted, which are in 
the region from (point) - (window-height) to (point) + (window-height)."
  (interactive "P")
  (setq whitespace-visual-incremental-mode
	(if (null arg) (not whitespace-visual-incremental-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (and whitespace-visual-mode whitespace-visual-incremental-mode)
	(set-extent-property (make-extent (point-min) (point-max))
			     'whitespace-visual-highlighted-region
			     t))
  (setq whitespace-visual-mode nil)
  (redraw-modeline) ;(force-mode-line-update)
  ;(set-buffer-modified-p (buffer-modified-p)) ;No-op, but updates mode line.
  (if whitespace-visual-incremental-mode
      (progn
	(whitespace-visual-highlight-window)
	(make-local-variable 'post-command-hook)
	(add-hook 'post-command-hook 'whitespace-visual-highlight-window)
	(make-local-variable 'after-change-functions)
	(add-hook 'after-change-functions 
		  'whitespace-visual-highlight-after-change-function)
	(run-hooks 'whitespace-visual-incremental-mode-hook))
    (whitespace-visual-dehighlight-buffer)
    (remove-hook 'after-change-functions 
		 'whitespace-visual-highlight-after-change-function)
    (remove-hook 'post-command-hook 'whitespace-visual-highlight-window)
    ))


;;; Add whitespace-visual-mode and whitespace-visual-incremental-mode
;;; to the minor-mode-alist

;;;###autoload 
(if (fboundp 'add-minor-mode)
    (progn
      (add-minor-mode 'whitespace-visual-mode
		      whitespace-visual-mode-line-string)
      (add-minor-mode 'whitespace-visual-incremental-mode
		      whitespace-visual-incremental-mode-line-string))
  (or (assq 'whitespace-visual-mode minor-mode-alist)
      (setq minor-mode-alist
            (cons '(whitespace-visual-mode whitespace-visual-mode-line-string)
		  minor-mode-alist)))
  (or (assq 'whitespace-visual-incremental-mode minor-mode-alist)
      (setq minor-mode-alist
            (cons '(whitespace-visual-incremental-mode
		    whitespace-visual-incremental-mode-line-string)
		  minor-mode-alist))))

;;; Menu for the whitespace-visual mode

(defun whitespace-visual-set-whitespace-chars (new-whitespace-chars)
  "Sets the variable `whitespace-visual-chars' and activates the change."
  (interactive 
   (list
    (read
     (completing-read "Whitespaces to highlight: "
		      '(("tabs-and-blanks")
			("tabs")
			("blanks"))
		      nil
		      t
		      (symbol-name 'whitespace-visual-chars)))))
  (if (eq whitespace-visual-chars new-whitespace-chars)
      nil ; nothing to do
    (setq whitespace-visual-chars new-whitespace-chars)
    (setq-default whitespace-visual-chars new-whitespace-chars)
    (cond (whitespace-visual-mode (whitespace-visual-mode) 
				  (whitespace-visual-mode))
	  (whitespace-visual-incremental-mode
	   (whitespace-visual-incremental-mode)
	   (whitespace-visual-incremental-mode))
	  )))

(defvar whitespace-visual-menu nil
  "A menu for the whitespace visual minor mode.")
  
(setq whitespace-visual-menu
      '("Whitespace Menu"
	["Highlight Whitespaces" 
	 whitespace-visual-mode 
	 :style toggle 
	 :selected whitespace-visual-mode]
	["Incremental Highlighting"
	 whitespace-visual-incremental-mode
	 :style toggle
	 :selected whitespace-visual-incremental-mode
	 ]
	"---"
	["Show Whitespace Faces" whitespace-visual-show-faces t]
	"---"
	["Highlight Tabs & Blanks" 
	 (whitespace-visual-set-whitespace-chars 'tabs-and-blanks)
	 :style radio
	 :selected (eq whitespace-visual-chars 'tabs-and-blanks)]
	["Highlight Only Tabs"
	 (whitespace-visual-set-whitespace-chars 'tabs)
	 :style radio
	 :selected (eq whitespace-visual-chars 'tabs)]
	["Highlight Only Blanks"
	 (whitespace-visual-set-whitespace-chars 'blanks)
	 :style radio
	 :selected (eq whitespace-visual-chars 'blanks)]
	))

(if (and (boundp 'whitespace-visual-install-submenu)
	 whitespace-visual-install-submenu)
    (add-submenu '("Tools") whitespace-visual-menu))

;;; Toolbar icon for the XEmacs

(if (featurep 'toolbar)

(defvar toolbar-wspace-icon
  (toolbar-make-button-list
   "/* XPM */
static char * whitespace[] = {
\"28 28 4 1\",
\" 	c Gray75 s backgroundToolBarColor\",
\".	c black s foregroundToolBarColor\",
\"X	c Gray60\",
\"o	c white\",
\"                            \",
\"                            \",
\"                            \",
\"                            \",
\"         ..      .          \",
\"       XXX.XXXXXX   .       \",
\"       Xoo.oooooXX  .       \",
\" .. .. ..o.o..oo..X...  ..  \",
\"  .  . X.o..o.ooX. X.  .  . \",
\"  .  . .oo.oo.ooX.XX.  .... \",
\"   ... .oo.oo.ooo.oo.  .    \",
\"   .  .Xoo.oo.ooo.oo.  .  . \",
\"   .  .Xo...o..o...o..  ..  \",
\"       XooooooooooooX       \",
\"       XooooooooooooX       \",
\" .... ....ooo...ooo...  ..  \",
\" .  .  .oo.o.oo.oo.oX. .  . \",
\"  .    .oo.ooo..oo.oX  .... \",
\"   ..  .oo.o..o.oo.oX  .    \",
\" .  .  .oo.o.oo.oo.oX. .  . \",
\" ....  ...oo.....oo..   ..  \",
\"       .ooooooooooooX       \",
\"       .XXXXXXXXXXXXX       \",
\"       .                    \",
\"      ...                   \",
\"                            \",
\"                            \",
\"                            \"
};")
  "A whitespace icon.")
)

(defun whitespace-visual-toolbar-function ()
  "Calls the function determined by `whitespace-visual-toolbar-function'."
  (interactive)
  (call-interactively whitespace-visual-toolbar-function))

(if (and (featurep 'xemacs) ;; (adapt-xemacsp)
	 whitespace-visual-install-toolbar-icon
	 (featurep 'toolbar)
	 (eq (device-type (selected-device)) 'x))
    (let ((tb (mapcar #'(lambda (e)
			  (elt e 1)) (specifier-instance default-toolbar))))
      (and (not (member 'whitespace-visual-toolbar-function tb))
	   (set-specifier 
	    default-toolbar
	    (toolbar-add-item
	     (make-toolbar-instantiator default-toolbar)
	     [toolbar-wspace-icon whitespace-visual-toolbar-function
				  t "Toggle whitespace visual mode"]
	     (let ((n (or
		       (position 'toolbar-replace tb)
		       (position 'toolbar-undo tb)
		       (position 'toolbar-paste tb)
		       (position 'toolbar-copy tb)
		       (position 'toolbar-cut tb))))
	       (if n (1+ n) (length tb))))))))

;;; whitespace-visual-mode.el ends here
