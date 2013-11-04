;;; whitespace-mode.el -- minor mode for making whitespace visible

;; Copyright (C) 1994, 1995, 1996 Heiko Muenkel

;; Author: Heiko Muenkel <muenkel@tnt.uni-hannover.de>
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
;; 02111-1307, USA.

;;; Synched up with: [[ FSF 19.34. ]] Not in FSF.
 
;;; Commentary:

;; $Id: whitespace-mode.el,v 1.6 2004/11/08 02:21:00 ben Exp $
;; Description:
;;
;;	This is a minor mode, which highlights whitespaces (blanks and
;;	tabs) with different faces, so that it is easier to
;;	distinguish between them.  
;;	Toggle the mode with: M-x old-whitespace-mode 
;;     or with: M-x old-whitespace-incremental-mode
;;	The second one should be used in big files.
;;
;;	If you want to know how the whitespaces are highlighted then
;;	type: M-x old-whitespace-show-faces
;;
;;	There are 2 hook variables `old-whitespace-incremental-mode-hook'
;;	and `old-whitespace-mode-hook' to customize the mode.
;;
;;	Look at the variable `old-whitespace-chars', if you only want to
;;	highlight tabs or blanks and not both.
;;
;;	Set `old-whitespace-install-toolbar-icon' to t, if you want a
;;	toolbar icon for this mode.
;;
;;	Set `old-whitespace-install-submenu' to t, if you want a submenu
;;     for this mode. Sorry, at the moment there is no menu for the
;;	Emacs 19. 
;;
;;	Thanks to Mike Scheidler for the toolbar icon code.
;; 
;; Installation:
;;   
;;     Put the files whitespace-mode.el and adapt.el in one of your
;; 	load-path directories and the following lines (without the
;; 	comment signs) in your .emacs (adapt.el is already in the
;;	XEmacs 19.12).
;;
;;     (autoload 'old-whitespace-mode "whitespace-mode" 
;;       "Toggle whitespace mode.
;;	With arg, turn whitespace mode on iff arg is positive.
;;	In whitespace mode the different whitespaces (tab, blank return)
;;	are highlighted with different faces. The faces are:
;;	`old-whitespace-blank-face', `old-whitespace-tab-face' and 
;;	`old-whitespace-return-face'."
;;	t)
;;
;;     (autoload 'old-whitespace-incremental-mode "whitespace-mode" 
;;	  "Toggle whitespace incremental mode.
;;     With arg, turn whitespace incremental mode on iff arg is positive.
;;	In whitespace incremental mode the different whitespaces (tab and 
;;	blank) are highlighted with different faces. The faces are:
;;	`old-whitespace-blank-face' and `old-whitespace-tab-face'.
;;	Use the command `old-whitespace-show-faces' to show their values.
;;	In this mode only these tabs and blanks are highlighted, which are in 
;;	the region from (point) - (window-height) to (point) + (window-height)."

;;; Code:

(provide 'whitespace-mode)
;; We don't need adapt
;; (require 'adapt)

;;; variables:

(defgroup old-whitespace nil
  "Minor mode for making whitespace visible"
  :group 'outlines
  :group 'matching)


(defcustom old-whitespace-mode nil
  "Non-nil, if the `old-whitespace-mode' is active."
  :type 'boolean
  :set (lambda (symbol value)
	 (old-whitespace-mode (or value 0)))
  :require 'whitespace-mode
  :initialize 'custom-initialize-default
  :group 'old-whitespace)

(make-variable-buffer-local 'old-whitespace-mode)

;;;###autoload
(defcustom old-whitespace-mode-line-string " WSP"
  "*String displayed on the modeline when old-whitespace-mode is active.
Set this to nil if you don't want a modeline indicator."
  :group 'old-whitespace
  :type 'string)

;;;###autoload
(defcustom old-whitespace-incremental-mode-line-string " WSPI"
  "*String displayed on the modeline when old-whitespace-incremental-mode
is active. Set this to nil if you don't want a modeline indicator."
  :group 'old-whitespace
  :type 'string)

(defcustom old-whitespace-chars 'tabs-and-blanks
  "*Determines, which whitespaces are highlighted.
Valid values are:
'tabs-and-blanks => tabs and blanks are highlighted;
'tabs            => only tabs are highlighted;
'blanks          => only blanks are highlighted;.

Changing this variable during the old-whitespace-*-mode is active could lead
to wrong highlighted whitespaces."
  :type '(radio (const tabs-and-blanks)
		(const tabs)
		(const blanks))
  :group 'old-whitespace)

(make-variable-buffer-local 'old-whitespace-chars)

(defcustom old-whitespace-mode-hook nil
  "*Run after the `old-whitespace-mode' is switched on."
  :type 'hook
  :group 'old-whitespace)

(defcustom old-whitespace-incremental-mode-hook nil
  "*Run after the `old-whitespace-incremental-mode' is switched on."
  :type 'hook
  :group 'old-whitespace)

;; We don't need adapt.
;; (if (adapt-xemacsp)
;; (progn

(defcustom old-whitespace-install-toolbar-icon nil
  "Set it to t, if a toolbar icon should be installed during loading this file.
The icon calls the function 'old-whitespace-toolbar-function'."
  :type 'boolean
  :group 'old-whitespace)

(defcustom old-whitespace-install-submenu nil
  "Set it to t, if a submenu should be installed during loading this file."
  :type 'boolean
  :group 'old-whitespace)

;; ))


(defcustom old-whitespace-toolbar-function 'old-whitespace-incremental-mode
  "*The toolbar icon for the whitespace mode calls this function.
Valid values are: 'old-whitespace--mode and 'old-whitespace-incremental-mode."
  :type 'function
  :group 'old-whitespace)

(defcustom old-whitespace-blank-and-tab-search-string "\\( \\)\\|\\(\t\\)"
  "The regexp used to search for tabs and blanks."
  :type 'regexp
  :group 'old-whitespace)

(defcustom old-whitespace-tab-search-string "\t"
  "The search string used to find tabs."
  :type 'string
  :group 'old-whitespace)

(defcustom old-whitespace-blank-search-string " "
  "The search string used to find blanks."
  :type 'string
  :group 'old-whitespace)

(defface old-whitespace-blank-face
  '((t
     (:background "LightBlue1")))
  "Face to show blanks with"
  :group 'old-whitespace)

(defface old-whitespace-tab-face
  '((t
     (:background "yellow" :underline t)))
  "Face to show TABs with"
  :group 'old-whitespace)

(defun old-whitespace-show-faces ()
  "Shows the faces used by the `old-whitespace-mode'."
  (interactive)
  (save-excursion
    (let ((actual-buffer-name (buffer-name (current-buffer)))
	  (actual-whitespace-chars old-whitespace-chars)
	  (old-whitespace-mode-active (or old-whitespace-mode 
				      old-whitespace-incremental-mode))
	  (buffer (get-buffer-create "*Help*")))
      (set-buffer buffer)
      (setq old-whitespace-chars actual-whitespace-chars)
      (delete-region (point-min) (point-max))
      (insert "In the whitespace minor mode\n"
	      " this \" ")
      (old-whitespace-highlight-region (1- (point)) (point))
      (insert "\" is a blank, highlighted with `old-whitespace-blank-face' and\n"
	      " this \"\t")
      (old-whitespace-highlight-region (1- (point)) (point))
      (insert "\" is a tab,  highlighted with `old-whitespace-tab-face'.")
      
      (newline 2)
      (if (eq old-whitespace-chars 'blanks)
	  (insert 
	   "The highlighting of tabs is switched off.\n")
	(if (eq old-whitespace-chars 'tabs)
	    (insert
	     "The highlighting of blanks is switched off.\n")))
      (newline)
      (if old-whitespace-mode-active
	  (insert "A whitespace minor mode is active in the buffer\n  "
		  actual-buffer-name
		  ".\n")
	(insert "No whitespace minor mode is active in the buffer\n  "
		actual-buffer-name
		".\n"))
      (show-temp-buffer-in-current-frame buffer)
      )))

;;;
(defun old-whitespace-highlight-chars-in-region (char-string from to face)
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

(defun old-whitespace-highlight-region (from to)
  "Highlights the whitespaces in the region from FROM to TO."
  (let ((start (min from to))
	(end (max from to)))
    (save-excursion
      ;;    (message "Highlighting tabs and blanks...")
      (goto-char start)
      (cond ((eq old-whitespace-chars 'tabs-and-blanks)
	     (while (search-forward-regexp 
		     old-whitespace-blank-and-tab-search-string end t)
	       (let ((extent))
		 (cond ((match-beginning 1) ; blanks ?
			(setq extent (make-extent (match-beginning 1) 
						  (match-end 1)))
			(set-extent-face extent 'old-whitespace-blank-face)
			)
		       ((match-beginning 2) ; tabs ?
			(setq extent (make-extent (match-beginning 2) 
						  (match-end 2)))
			(set-extent-face extent 'old-whitespace-tab-face)
			)
		       )
		 (set-extent-property extent 'start-open t)
		 (set-extent-property extent 'end-open t)
		 )))
	    ((eq old-whitespace-chars 'tabs)
	     (old-whitespace-highlight-chars-in-region old-whitespace-tab-search-string 
						   from 
						   to
						   'old-whitespace-tab-face))
	    ((eq old-whitespace-chars 'blanks)
	     (old-whitespace-highlight-chars-in-region 
	      old-whitespace-blank-search-string 
	      from 
	      to
	      'old-whitespace-blank-face))
	    (t (error "ERROR: Bad value of old-whitespace-highlight-char")))
      ;;    (message "")
      )))

(defun old-whitespace-highlight-buffer ()
  "Highlights the whitespaces in the current buffer."
  (old-whitespace-highlight-region (point-min) (point-max))
)

(defsubst old-whitespace-find-next-highlighted-region (from to)
  "Returns nil or the next highlighted region."
  (map-extents '(lambda (extent dummy)
		 (if (extent-property extent 'old-whitespace-highlighted-region)
		     extent))
	       nil
	       from
	       to))

(defun old-whitespace-incremental-highlight (from to)
  "Highlights the region from FROM to TO incremental."
  (save-excursion
    (goto-char from)
    (let ((extent (extent-at (point) nil 'old-whitespace-highlighted-region))
	  (next-extent nil)
	  (start nil))
      (while (< (point) to)
	(if extent
	    (goto-char (extent-end-position extent)))
	(if (< (point) to)
	    (progn
	      (setq start (point))
	      
	      (setq next-extent (old-whitespace-find-next-highlighted-region 
				 start
				 to))
	      (if extent
		  (if next-extent
		      (progn
			(set-extent-endpoints extent 
					      (extent-start-position extent)
					      (extent-end-position next-extent)
					      )
			(old-whitespace-highlight-region start
						     (1-
						      (extent-start-position
						       next-extent)))
			(delete-extent next-extent))
		    (set-extent-endpoints extent
					  (extent-start-position extent)
					  to)
		    (old-whitespace-highlight-region start to))
		(if next-extent
		    (progn
		      (setq extent next-extent)
		      (old-whitespace-highlight-region start 
						   (1- (extent-start-position
							next-extent)))
		      (set-extent-endpoints extent
					    start
					    (extent-end-position next-extent)))
		  (setq extent (make-extent start to))
		  (set-extent-property extent 'old-whitespace-highlighted-region t)
		  (old-whitespace-highlight-region start to)))
	      ))))))


(defun old-whitespace-highlight-window ()
  "Highlights the whitespaces in the current window."
  (old-whitespace-incremental-highlight (save-excursion
				      (forward-line (- (window-height)))
				      (point))
				    (save-excursion
				      (forward-line (window-height))
				      (point))))

(defun old-whitespace-dehighlight-region (start end)
  "Dehighlights the whitespaces in the region from START to END."
  (map-extents '(lambda (extent dummy)
		  (if (or (eq (extent-face extent) 'old-whitespace-blank-face)
			  (eq (extent-face extent) 'old-whitespace-tab-face)
			  (extent-property extent 
					   'old-whitespace-highlighted-region))
		      (progn
			(delete-extent extent)
			nil)))
	       nil
	       start
	       end
	       )
  )

(defun old-whitespace-dehighlight-buffer ()
  "Dehighlights the whitespaces in the current buffer."
  (old-whitespace-dehighlight-region (point-min) (point-max))
  )

(defun old-whitespace-highlight-after-change-function (beg end old-len)
  "Called, when any modification is made to buffer text.  Highlights
the whitespaces (blanks and tabs) in the region from BEG to
END. OLD-LEN isn't used, but provided from the after-change hook."
  (if (or (eq beg end)
	  (null old-whitespace-mode))
      nil
    (old-whitespace-dehighlight-region beg end)
    (old-whitespace-highlight-region beg end)))

;;;###autoload
(defun old-whitespace-mode (&optional arg)
  "Toggle whitespace mode.
With arg, turn whitespace mode on iff arg is positive.
In whitespace mode the different whitespaces (tab and blank)
are highlighted with different faces. The faces are:
`old-whitespace-blank-face' and `old-whitespace-tab-face'.
Use the command `old-whitespace-show-faces' to show their values."
  (interactive "P")
  (setq old-whitespace-mode
	(if (null arg) (not old-whitespace-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (and old-whitespace-mode old-whitespace-incremental-mode)
      (progn
	(old-whitespace-incremental-highlight (point-min) (point-max))
	(setq old-whitespace-incremental-mode nil)
	(remove-hook 'post-command-hook 'old-whitespace-highlight-window)
	(run-hooks 'old-whitespace-mode-hook)
	)
    (setq old-whitespace-incremental-mode nil)
    (remove-hook 'post-command-hook 'old-whitespace-highlight-window)
    (redraw-modeline) ;(force-mode-line-update)
    (if old-whitespace-mode
	(progn
	  (old-whitespace-highlight-buffer)
	  (make-local-variable 'after-change-functions)
	  (add-hook 'after-change-functions 
		    'old-whitespace-highlight-after-change-function)
	  (run-hooks 'old-whitespace-mode-hook))
      (old-whitespace-dehighlight-buffer)
      (remove-hook 'after-change-functions 
		   'old-whitespace-highlight-after-change-function)
      (remove-hook 'post-command-hook 'old-whitespace-highlight-window)
      )))

(defvar old-whitespace-incremental-mode nil
  "Non-nil, if the `old-whitespace-incremental-mode' is active.")

(make-variable-buffer-local 'old-whitespace-incremental-mode)

;;;###autoload
(defun old-whitespace-incremental-mode (&optional arg)
  "Toggle whitespace incremental mode.
With arg, turn whitespace incremental mode on iff arg is positive.
In whitespace incremental mode the different whitespaces (tab and blank)
are highlighted with different faces. The faces are:
`old-whitespace-blank-face' and `old-whitespace-tab-face'.
Use the command `old-whitespace-show-faces' to show their values.
In this mode only these tabs and blanks are highlighted, which are in 
the region from (point) - (window-height) to (point) + (window-height)."
  (interactive "P")
  (setq old-whitespace-incremental-mode
	(if (null arg) (not old-whitespace-incremental-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (and old-whitespace-mode old-whitespace-incremental-mode)
	(set-extent-property (make-extent (point-min) (point-max))
			     'old-whitespace-highlighted-region
			     t))
  (setq old-whitespace-mode nil)
  (redraw-modeline) ;(force-mode-line-update)
  ;(set-buffer-modified-p (buffer-modified-p)) ;No-op, but updates mode line.
  (if old-whitespace-incremental-mode
      (progn
	(old-whitespace-highlight-window)
	(make-local-variable 'post-command-hook)
	(add-hook 'post-command-hook 'old-whitespace-highlight-window)
	(make-local-variable 'after-change-functions)
	(add-hook 'after-change-functions 
		  'old-whitespace-highlight-after-change-function)
	(run-hooks 'old-whitespace-incremental-mode-hook))
    (old-whitespace-dehighlight-buffer)
    (remove-hook 'after-change-functions 
		 'old-whitespace-highlight-after-change-function)
    (remove-hook 'post-command-hook 'old-whitespace-highlight-window)
    ))


;;; Add whitespace-mode and old-whitespace-incremental-mode to the minor-mode-alist

;;;###autoload 
(if (fboundp 'add-minor-mode)
    (progn
      (add-minor-mode 'old-whitespace-mode old-whitespace-mode-line-string)
      (add-minor-mode 'old-whitespace-incremental-mode old-whitespace-incremental-mode-line-string))
  (or (assq 'old-whitespace-mode minor-mode-alist)
      (setq minor-mode-alist
            (cons '(old-whitespace-mode old-whitespace-mode-line-string) minor-mode-alist)))
  (or (assq 'old-whitespace-incremental-mode minor-mode-alist)
      (setq minor-mode-alist
            (cons '(old-whitespace-incremental-mode old-whitespace-incremental-mode-line-string) minor-mode-alist))))

;;; Menu for the whitespace mode

(defun old-whitespace-set-whitespace-chars (new-whitespace-chars)
  "Sets the variable `old-whitespace-chars' and activates the change."
  (interactive (list (read (completing-read "Whitespaces to highlight: "
					    '(("tabs-and-blanks")
					      ("tabs")
					      ("blanks"))
					    nil
					    t
					    (symbol-name 'old-whitespace-chars)))))
  (if (eq old-whitespace-chars new-whitespace-chars)
      nil ; nothing to do
    (setq old-whitespace-chars new-whitespace-chars)
    (setq-default old-whitespace-chars new-whitespace-chars)
    (cond (old-whitespace-mode (old-whitespace-mode) 
			   (old-whitespace-mode))
	  (old-whitespace-incremental-mode (old-whitespace-incremental-mode)
				       (old-whitespace-incremental-mode))
	  )))

(defvar old-whitespace-menu nil
  "A menu for the whitespace minor mode.")
  
(setq old-whitespace-menu
      '("Whitespace Menu"
	["Highlight Whitespaces" 
	 old-whitespace-mode 
	 :style toggle 
	 :selected old-whitespace-mode]
	["Incremental Highlighting"
	 old-whitespace-incremental-mode
	 :style toggle
	 :selected old-whitespace-incremental-mode
	 ]
	"---"
	["Show Whitespace Faces" old-whitespace-show-faces t]
	"---"
	["Highlight Tabs & Blanks" 
	 (old-whitespace-set-whitespace-chars 'tabs-and-blanks)
	 :style radio
	 :selected (eq old-whitespace-chars 'tabs-and-blanks)]
	["Highlight Only Tabs"
	 (old-whitespace-set-whitespace-chars 'tabs)
	 :style radio
	 :selected (eq old-whitespace-chars 'tabs)]
	["Highlight Only Blanks"
	 (old-whitespace-set-whitespace-chars 'blanks)
	 :style radio
	 :selected (eq old-whitespace-chars 'blanks)]
	))

(if (and (boundp 'old-whitespace-install-submenu) old-whitespace-install-submenu)
    (add-submenu '("Apps") old-whitespace-menu))

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

(defun old-whitespace-toolbar-function ()
  "Calls the function determined by `old-whitespace-toolbar-function'."
  (interactive)
  (call-interactively old-whitespace-toolbar-function))

(if (and (featurep 'xemacs) ;; (adapt-xemacsp)
	 old-whitespace-install-toolbar-icon
	 (featurep 'toolbar)
	 (eq (device-type (selected-device)) 'x))
    (let ((tb (mapcar #'(lambda (e)
			  (elt e 1)) (specifier-instance default-toolbar))))
      (and (not (member 'old-whitespace-toolbar-function tb))
	   (toolbar-add-item
	    [toolbar-wspace-icon old-whitespace-toolbar-function
				 t "Toggle whitespace mode"]
	    (let ((n (or
		      (position 'toolbar-replace tb)
		      (position 'toolbar-undo tb)
		      (position 'toolbar-paste tb)
		      (position 'toolbar-copy tb)
		      (position 'toolbar-cut tb))))
	      (if n (1+ n) (length tb)))))))

;;; whitespace-mode.el ends here
