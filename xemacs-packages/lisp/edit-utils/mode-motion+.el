;;   -*- Syntax: Emacs-Lisp; Mode: emacs-lisp -*-
;;
;; Per mode and per buffer mouse tracking with highlighting
;;
;; Copyright (C) 1992, 1993 by Guido Bosch <Guido.Bosch@loria.fr>

;; This file is written in GNU Emacs Lisp, It is a part of XEmacs.

;; The software contained in this file is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Synched up with: Not in FSF.

;; Please send bugs and comments to Russell.Ritchie@gssec.bt.co.uk or
;;                                  tlp00@spg.amdahl.com.
;;
;; <DISCLAIMER>
;; This program is still under development.  Neither the author nor any
;; of the maintainers accepts responsibility to anyone for the consequences of
;; using it or for whether it serves any particular purpose or works
;; at all.

; Change History
; Revision 3.16 Fri Jun 28 13:01:12 1996 ritchier@msc.ie
; Stop multiple highlighting lossage with 19.14 release.

; Revision 3.15 Thu Feb 15 14:26:34 GMT 1996 Russell.Ritchie@gssec.bt.co.uk
; lisp-interaction-popup-menu => lisp-interaction-mode-popup-menu,
; emacs-lisp-popup-menu => emacs-lisp-mode-popup-menu.

; Revision 3.14 Tue Nov 14 11:14:38 GMT 1995 Russell.Ritchie@gssec.bt.co.uk
; Made nil the default value for mode-motion-focus-on-window.  Too many people
; hate it when the cursor warps into Dired and GNUS buffers because some
; window managers auto-raise the window with keyboard focus with predictably
; nauseous results.

; Revision 3.13 Thu Sep 14 10:30:04 1995 Russell.Ritchie@gssec.bt.co.uk
; Fix the `spontaneous scrolling' problem (at last).  It's funny how
; easy things are once you actually understand the issues involved.
; As ever, what we sought was the right question...

; Revision 3.12 Wed Jul 12 11:30:43 1995 Russell.Ritchie@gssec.bt.co.uk
; Track `don't highlight non-file lines in dired buffers' functionality (in a
; pretty tasteless manner if I say so myself :-)).

; Revision 3.11 Fri Jul  7 16:26:56 1995 Russell.Ritchie@gssec.bt.co.uk
; Minor extent detaching bug fix.

; Revision 3.10 Thu Jun 15 11:36:56 1995 Russell.Ritchie@gssec.bt.co.uk
; Quiet, faster, non-interactive initialistion, mild list-motion-handlers
; chrome and minor formatting clean-ups.

; Revision 3.9 Thu Jun 15 11:36:56 1995 Russell.Ritchie@gssec.bt.co.uk
; Fixed the `mouse-motion whilst reading filename in minibuffer auto-ftp' bug.

; Revision 3.8 Thus Mar 23 1995 tlp00@spg.amdahl.com
; added in menu controls from paquette@atomas.crim.ca
; re-added minibuffer support (from 3.5)
;
; Revision 3.7 Tue Feb 21 11:06:38 1995 Russell.Ritchie@gssec.bt.co.uk
; Extended mode-motion+-religion and made the defaulting frame-buffer aware.
; Reworked and added new mode-motion-handlers.
; Doc string clean up.
; Fixed unintentional frame/screen reversion.

; Revision 3.6 Mon Feb 20 11:46:32 1995 Russell.Ritchie@gssec.bt.co.uk
; Made mouse tracking use mode-motion-hook for better integration with
; the default mouse-motion system (help-echo and friends now work).

; Revision 3.5 1995/02/16 13:40:00 tlp00@spg.amdahl.com
; fixed sporatic scroll bug
; added M-button2 binding for mode-motion-copy
;
; Revision 3.4 1995/02/14 14:30:15 Russell.Ritchie@gssec.bt.co.uk
; Small code cleanups: let* -> let, duplicate defvars.
; Chromed list-motion-handlers a little.
; Added variable mode-motion+-religion for easy choice twixt underline & bold.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;tlp00 changes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tlp00@spg.amdahl.com 2/11/93
; modified mode-motion-track-pointer to move cursor cross windows
;          renamed mode-motion-delete to mode-motion-kill to follow kill
;            convention
;          mode-motion-highlight-with-handler to put cursor at beginning of line 
;            follow operations.
;          mode-motion-copy/delete and mode-motion-kill to position cursor at 
;            delete point start.  Also set this-command to avoid appends
; set mode-motion-extent priority to 1, so it will override font-lock
; changed default handlers for buffer-mode, c-mode, dired-mode, added occur 
;   and compilation mode.
; fixed bug in minibuffer-selection-boundaries where C-g was leaving the wrong
;   syntax table.
; added support for pending-delete.
; adds the copy/delete motion-extent to the clipboard even if kill-hooks is nil.
;
; Revision 3.3 1995/02/13 tlp00@spg.amdahl.com
; merged Russell.Ritchie@gssec.bt.co.uk versions with molli/bosch versions
; renamed versioning 3.0+ for molli/bosch versions.  
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Russell Ritchie changes;;;;;;;;;;;;;;;;;;;;;;;;
; !Log: mode-motion+.el,v !
; Revision 2.14.R  1994/09/09  10:19:18  rieke@darmstadt.gmd.de
; Merged in my changes to support motion-gray. This needs a file
; named "gray1.xbm" in your data-directory (etc) like the following.
; -------------------------------snip--------------------------
; #define bg2_width 16
; #define bg2_height 16
; static char bg2_bits[] = {
;   0x55, 0x55, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00,
;   0x55, 0x55, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00,
;   0x55, 0x55, 0x00, 0x00, 0x55, 0x55, 0x00, 0x00};
; -------------------------------snip--------------------------
; This one looks good on SUN 19'' screens with 10x20 font, 
; black foreground and khaki background. 
; To use the gray-handlers instead of the underline-handlers
; include the following into your .emacs:
; (set-mode-motion-handler 'emacs-lisp-mode 'gray-thing)
; (set-mode-motion-handler 'lisp-interaction-mode 'gray-thing)
; (set-mode-motion-handler 'c++-mode 'gray-c)
; (set-mode-motion-handler 'c-mode 'gray-c)
; (set-mode-motion-handler 'tcl-mode 'gray-tcl)
; (set-mode-motion-handler 'dired-mode 'gray-line@)
; (set-mode-motion-handler 'gnus-group-mode 'gray-vline@)
; (set-mode-motion-handler 'gnus-summary-mode 'gray-vline@)
; (set-mode-motion-handler 'texinfo-mode 'gray-Texinfo)
; (setq default-motion-handler (find-motion-handler 'gray-thing))
;
;
; Revision 2.13.R  1994/08/08  19:47:34  Russell.Ritchie@gssec.bt.co.uk
; Made default handler be underline-thing, as most bold fonts seem to
; be greater in height than their corresponding normal versions,
; causing irritating screen flicker.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Molli/bosch changes;;;;;;;;;;;;;;;;;;;;;;;;
;
; Revision 3.2  1994/09/28  15:14:29  molli
; add   "(set-mode-motion-handler 'latex-mode   'raise-LaTeX)".    Barry
; Waraw's C/C++ mode is now changed to cc-mode ...
;
; Revision 3.1  1994/09/28  15:10:36  molli
; Initial revision
;
; Revision 2.15  1993/11/18  08:13:28  bosch
; Constant `mode-motion+-version' added.
; Minor bug fix in `tcl-forward-sexp1'.
;
; Revision 2.14  1993/10/29  20:04:59  bosch
; Minibuffer name matching improved.  Made `tcl-boundaries' smarter by
; use of new function `tcl-forward-sexp1'. `tcl-commands' list updated
; -- should be complete now.  A message is printed if the syntax scanner
; matched or failed for known tcl/tk commands.  Separated `tcl-commands'
; from `tk-commands' -- `tk-commands' not yet complete.  New motion
; handler `raise-LaTeX' added, for tex-mode.
;
; Revision 2.13  1993/10/08  09:43:00  bosch
; New user option `mode-motion-setup-cut-and-paste-bindings'.  Function
; `mode-motion-copy/delete' now takes into account the primary and the
; motion selection.
;
; Revision 2.12  1993/10/08  09:08:46  bosch
; Avoid highlighting empty lines, even if
; `mode-motion-highlight-lines-when-behind' is non-nil.

; Revision 2.12  1994/07/07  18:33:38  Russell.Ritchie@gssec.bt.co.uk
; Made list-motion-handlers and mode-motion-set-handler work in lemacs-19.10.
; 
; Revision 2.11  1993/09/20  08:29:15  bosch
; Code reorganized: variables declared before used.
; Compatibility hack patched again.
;
; Revision 2.10  1993/09/17  18:50:33  bosch
; Bug in the compatibility hack fixed. Call to `make-cursor' replaced by
; `x-pointer-cache'. Compatibility hack for Lemacs 19.8 removed.  Tcl
; motion handler improved (needs still some work).
;
; Revision 2.9  1993/09/15  17:52:53  bosch
; Compatibility patch for Lucid Emacs 19.8. tcl motion handler added.
;
; Revision 2.8  1993/08/27  15:17:07  bosch
; Select window conflict between motion handlers and process filters
; resolved by using `enqueue-eval-event' for selecting a different
; window (functions `mode-motion-track-pointer' and
; `mode-motion-highlight-with-handler' modified). This fixes the nasty
; bug that made GNUS hanging during NNTP activity while the mouse was
; moved.
;
; Revision 2.7  1993/08/27  12:50:10  bosch
; TeX and LaTeX motion handler generalized.  Motion handler
; `highlight-Texinfo' added.
;
; Revision 2.6  1993/06/24  11:58:52  bosch
; Mode motion+ support for pcl-cvs added. #undef syntax for C added.
;
; Revision 2.5  1993/06/09  12:04:31  bosch
; Delivery motion handlers for `c++-c-mode', `gnus-group-mode', and
; `gnus-summary-mode' added. Mode motion commands bound to copy/cut/past
; keys for Sun keyboards (f16, f18, f20). Comment added.
;
; Revision 2.4  1993/02/15  12:59:47  bosch
; Modifications sent by Tibor Polgar integrated:
; Optional parameter added to `mode-motion-copy/delete'.  User option
; `mode-motion-focus-on-window' added. It controls window selection for
; the motion handlers. Minor changes of the delivery motion handlers.
;
; Revision 2.3  1993/02/04  18:10:09  bosch
; User option `mode-motion-minibuffer-completion' added. It controls
; the minibuffer completion highlighting.
;
; Revision 2.2  1993/01/27  13:08:12  bosch
; Improved clearing of `sticky' mode-motion-extents when leaving screen
; or buffer.  Function `mode-motion-clear-extent' added.
; Highlight line mouse cursor is behind.
; `mode-motion-highlight-with-handler' now takes an event as argument.
; Cut and paste functions renamed and rewritten. Now they are called:
; `mode-motion-move', `mode-motion-delete', `mode-motion-copy',
; `mode-motion-copy-as-kill'.  Bug fixes in the C scanner stuff.
; Motion handler `underline-c' added.
;
; Revision 2.1  1993/01/19  18:29:58  bosch
; Scanner and motion handler for C syntax added.
; Function `set-default-motion-handler' added.
; Minor improvements on the `list-motion-handlers' interface done.
; Minor bug fixes.
;
; Revision 2.0 1993/01/14   19:17:29  bosch
; Lot of things rewritten and reorganized. This version fits in only
; one file (beside the required package thing.el).
;
; New basic features are:
;  - buffer, mode and default motion handlers
;  - easy composition of own motion handlers
;  - listing of motion handlers in tabular form
;  - menu interface for changing motion handlers
;  - only two  elisp files: mode-motion+.el, thing.el
; 

(require 'thing)
(require 'mode-motion)
(defconst mode-motion+-version "3.16")

;;; This file defines a set of mouse motion handlers that do some
;;; highlighting of the text when the mouse moves over.
;;; An exhaustive list of the motion handlers defined in this file may be
;;; obtained with M-x list-motion-handlers.
;;; User Options and their Custommisation
;;;
;;; Mode-motion+ provides four user options, defined beyond. See their
;;; documentation string to know what they are good for. If you want
;;; to modify their default values, just setq them in your ~/.emacs.

(defvar mode-motion+-religion nil ; Initialised in mode-motion-init.
  "*Default highlight religion: one of bold, gray, highlight, invert or underline.

Unless you setq this otherwise, it defaults to underline when
(x-display-color-p) is non-nil and invert otherwise.
Setting it to 'highlight should cause mode-motion+ extents to be
indistinguishable from any other type of highlighted extent which may or may
not be advisable, depending on your point of view.")

(defvar mode-motion-highlight-lines-when-behind t
  "*If non-nil highlight the whole line if the mouse is past the end.")

(defvar mode-motion-focus-on-window nil
  "*Controls whether moving the mouse into another window selects this window.
The following values are possible:
 
nil	  - Window selection isn't influenced at all by mode motion.

t	  - Window selection always follows the mouse cursor. Copying motion
            active regions doesn't work any longer between different buffers.
	    
any other - window selection follows the mouse cursor if the motion handler
	    of the buffer under the mouse has the follow-point property set.
	    Useful for selecting line mode buffers just by moving the mouse
	    inside in order to execute commands there (VM summary,
	    GNUS Group and Subject, DIRED, Buffer menu etc.)")

(defvar mode-motion-setup-cut-and-paste-bindings t
  "*If non-nil, bind commands to the Copy, Paste and Cut keys.")

;;  Options sub-menu for mode-motion+
(defvar mode-motion+-options-menu 
  '("Motion Highlighting"
    "For Current Buffer"
    "---"
    ["None"
     (progn
       (set-buffer-motion-handler (current-buffer) 'no-thing)
       (mode-motion-clear-extent))
     :style radio
     :selected (eq (mode-motion+-buffer-handler-religion (current-buffer))
		   'no)
     :active (mode-motion+-active-p)]
    ["Bold"
     (progn
       (modify-buffer-motion-handler (current-buffer) 'bold))
     :style radio
     :selected (eq (mode-motion+-buffer-handler-religion (current-buffer))
		   'bold)
     :active (mode-motion+-active-p)]
    ["Underline"
     (progn
       (modify-buffer-motion-handler (current-buffer) 'underline))
     :style radio
     :selected (eq (mode-motion+-buffer-handler-religion (current-buffer))
		   'underline)
     :active (mode-motion+-active-p)]
    ["Gray"
     (progn
       (modify-buffer-motion-handler (current-buffer) 'gray))
     :style radio
     :selected (eq (mode-motion+-buffer-handler-religion (current-buffer))
		   'gray)
     :active (mode-motion+-active-p)]
    ["Highlight"
     (progn
       (modify-buffer-motion-handler (current-buffer) 'highlight))
     :style radio
     :selected (eq (mode-motion+-buffer-handler-religion (current-buffer))
		   'highlight)
     :active (mode-motion+-active-p)]
    ["Invert"
     (progn
       (modify-buffer-motion-handler (current-buffer) 'invert))
     :style radio
     :selected (eq (mode-motion+-buffer-handler-religion (current-buffer))
		   'invert)
     :active (mode-motion+-active-p)]
    "---"
    "For Current Mode"
    "---"
    ["None"
     (progn
       (set-mode-motion-handler major-mode 'no-thing)
       (mode-motion-clear-extent))
     :style radio
     :selected (eq (mode-motion+-mode-handler-religion major-mode) 'no)
     :active (mode-motion+-active-p)]
    ["Bold"
     (progn
       (modify-mode-motion-handler major-mode 'bold))
     :style radio
     :selected (eq (mode-motion+-mode-handler-religion major-mode) 'bold)
     :active (mode-motion+-active-p)]
    ["Underline"
     (progn
       (modify-mode-motion-handler major-mode 'underline))
     :style radio
     :selected (eq (mode-motion+-mode-handler-religion major-mode) 'underline)
     :active (mode-motion+-active-p)]
    ["Gray"
     (progn
       (modify-mode-motion-handler major-mode 'gray))
     :style radio
     :selected (eq (mode-motion+-mode-handler-religion major-mode) 'gray)
     :active (mode-motion+-active-p)]
    ["Highlight"
     (progn
       (modify-mode-motion-handler major-mode 'highlight))
     :style radio
     :selected (eq (mode-motion+-mode-handler-religion major-mode) 'highlight)
     :active (mode-motion+-active-p)]
    ["Invert"
     (progn
       (modify-mode-motion-handler major-mode 'invert))
     :style radio
     :selected (eq (mode-motion+-mode-handler-religion major-mode) 'invert)
     :active (mode-motion+-active-p)]
    "---"
    "For All"
    "---"
    ["None"
     (progn
       (setq mode-motion+-religion 'no)
       (mode-motion-init-handlers-according-to-religion 'force)
       (mode-motion-clear-extent))
     :style radio
     :selected (eq mode-motion+-religion 'no)
     :active (mode-motion+-active-p)]
    ["Bold"
     (progn
       (setq mode-motion+-religion 'bold)
       (mode-motion-init-handlers-according-to-religion 'force))
     :style radio
     :selected (eq mode-motion+-religion 'bold)
     :active (mode-motion+-active-p)]
    ["Underline"
     (progn
       (setq mode-motion+-religion 'underline)
       (mode-motion-init-handlers-according-to-religion 'force))
     :style radio
     :selected (eq mode-motion+-religion 'underline)
     :active (mode-motion+-active-p)]
    ["Gray"
     (progn
       (setq mode-motion+-religion 'gray)
       (mode-motion-init-handlers-according-to-religion 'force))
     :style radio
     :selected (eq mode-motion+-religion 'gray)
     :active (mode-motion+-active-p)]
    ["Highlight"
     (progn
       (setq mode-motion+-religion 'highlight)
       (mode-motion-init-handlers-according-to-religion 'force))
     :style radio
     :selected (eq mode-motion+-religion 'highlight)
     :active (mode-motion+-active-p)]
    ["Invert"
     (progn
       (setq mode-motion+-religion 'invert)
       (mode-motion-init-handlers-according-to-religion 'force))
     :style radio
     :selected (eq mode-motion+-religion 'invert)
     :active (mode-motion+-active-p)]
    "---"
    ["Minibuffer highlighting" (setq mode-motion-use-minibuffer-motion-handler
				     (not mode-motion-use-minibuffer-motion-handler))
     :style toggle :selected mode-motion-use-minibuffer-motion-handler]
     
    ["Customize..."
     (list-motion-handlers)
     t
     ;;     nil
     ]
     ["Revert Customization"
     (call-interactively 'mode-motion+-motion-hook-revert)
     (and (boundp 'mode-motion+-previous-hook) mode-motion+-previous-hook)
     ])
  "Menu for changing mode-motion+ religion and other things.")

(defun mode-motion+-active-p ()
  (cond ((symbolp mode-motion-hook)
	 (eq mode-motion-hook 'mode-motion+-highlight))
	((listp mode-motion-hook)
	 (memq 'mode-motion+-highlight mode-motion-hook))
	(t nil)))
     
(defun mode-motion+-buffer-handler-religion (buffer)
  (let* ((current-handler-name (symbol-name (motion-handler-name
					     (get-current-motion-handler))))
	 (religion-name (substring current-handler-name
				   0
				   (string-match "-" current-handler-name))))
    (intern-soft religion-name)))

(defun mode-motion+-mode-handler-religion (buffer)
  (let* ((mode-handler (or (get major-mode 'mode-motion-handler)
			   default-motion-handler))
	 (current-handler (symbol-name (motion-handler-name mode-handler)))
	 (religion (substring current-handler
			      0
			      (string-match "-" current-handler))))
    (intern-soft religion)))

(defun modify-buffer-motion-handler (buffer religion)
  (let* ((current-handler (symbol-name (motion-handler-name
					     (get-current-motion-handler))))
	 (suffix (substring current-handler
			    (string-match "-" current-handler))))
    (set-buffer-motion-handler buffer
			       (intern-soft (concat (symbol-name religion)
						    suffix)))))

(defun modify-mode-motion-handler (mode religion)
  (let* ((mode-handler (or (get major-mode 'mode-motion-handler)
			   default-motion-handler))
	 (current-handler (symbol-name (motion-handler-name mode-handler)))
	 (suffix (substring current-handler
			    (string-match "-" current-handler))))
    (set-mode-motion-handler mode (intern-soft (concat (symbol-name
							religion)
						       suffix)))))

;;;; This does not work.  I would like to be able to modify in-place
;;;; the non-selectable items, but I don't know how.
;;;; --paquette, Wed Mar  8 23:32:32 1995 (Marc Paquette) 
;;; Sensitize the mode motion+ options submenu, a la
;;; sensitize-file-and-edit-menus-hook.
(defun mode-motion+-sensitize-options-menu-hook ()
  "Hook function that will adjust title items in the mode-motion+ submenu in Options"
  (let* ((mm+-menu (cdr (car (find-menu-item
			      current-menubar
			      '("Options" "Motion Highlighting")))))
	 (buffer-item (find-menu-item mm+-menu '("For Current Buffer")))
	 (mode-item (find-menu-item mm+-menu '("For Current Mode"))))
    (setcar buffer-item (format "For Buffer `%s'" (buffer-name nil)))
    (setcar mode-item (format "For Mode `%s'" major-mode))
    nil))

;;(add-hook 'activate-menubar-hook 'mode-motion+-sensitize-options-menu-hook)
  

;;  Motion Handler Format:
;;      
;; A motion handler is vector with the following format 
;; [<name> 		       	- a symbol
;;  <region computing function> - a function or name of function 
;;				  that returns (<startpos> . <endpos>) 
;;				  or nil.
;;  <face or face name>		- as it says ...
;;  <highlight-p>		- non-nil means that the motion extent
;;				  will be highlighted using the function 
;;				  `highlight-extent'
;;  <follow-point-p>		- non-nil means that point will follow the
;;				  mouse motion. 
;; ]

;; accessor functions
(defsubst motion-handler-name (handler) (aref handler 0))
(defsubst motion-handler-boundary-function (handler) (aref handler 1))
(defsubst motion-handler-face (handler) (aref handler 2))
(defsubst motion-handler-highlight (handler) (aref handler 3))
(defsubst motion-handler-follow-point (handler) (aref handler 4))

;; modifier functions
(defsubst set-motion-handler-boundary-function (handler x) (aset handler 1 x))
(defsubst set-motion-handler-face (handler x) (aset handler 2 x))
(defsubst set-motion-handler-highlight (handler x) (aset handler 3 x))
(defsubst set-motion-handler-follow-point (handler x) (aset handler 4 x))

;; Internal global variables 
(defvar motion-handler-alist ()
  "Alist with entries of the form \(<name> . <handler>\).")

;; Minibuffer motion handler
(defvar mode-motion-use-minibuffer-motion-handler t
  "*Enable mousable highlighting when the minibuffer is active. When false only extents with the
highlight property are selectable (*Completion*)")

(defvar mode-motion-extent nil)
(make-variable-buffer-local 'mode-motion-extent)
(defvar buffer-motion-handler nil)
(make-variable-buffer-local 'buffer-motion-handler)
(defvar mode-motion-last-extent nil "The last used mode motion extent.")
(defvar default-motion-handler nil)	; Initialised in mode-motion-init.

;; Creation of motion handlers

(defun find-motion-handler (name)
  (or (symbolp name)
      (setq name (intern-soft name)))
  (cdr (assq name motion-handler-alist)))

;; internal motion handler creator
(defsubst make-motion-handler-internal 
  (name boundary-function face highlight follow-cursor)
  (vector name boundary-function (get-face face) highlight follow-cursor))

(defun make-motion-handler 
  (name boundary-function &optional face highlight follow-cursor)
  "Create a motion handler named NAME (a symbol or string) using REGION-FUNCTION.

REGION-FUNCTION is the function that computes the region to be highlighted. 
Optional arguments are: 

FACE: A face or face name to be used to highlight the region computed
      by REGION-FUNCTION.  'default is the default.
      
HIGHLIGHT: Flag that indicates whether the highlight attribute of the
      mode-motion-extent should be set or not. If FACE is the default face, 
      HIGHLIGHT defaults to t, otherwise to nil.

FOLLOW-CURSOR: Flag that indicates whether the cursor should follow
      the mouse motion. Default is nil."

  ;; required arguments
  (or name (error "motion handler name required."))
  (or (symbolp name) 
      (stringp name)
      (error "motion handler name must be a string or symbol: %s" name))
  (or boundary-function 
      (error "motion handler region function required."))
  (or (fboundp boundary-function)
      (error "not a function: %s." boundary-function))
  ;; defaults
  (or face (setq face 'default))
  
  ;; store the mode motion handler on the 'mode-motion-handler property of
  ;; its name symbol
  (let ((old-handler (cdr (assq name motion-handler-alist)))
	new-handler)
    (if old-handler
	(progn 
	  (set-motion-handler-boundary-function old-handler boundary-function)
	  (set-motion-handler-face old-handler (get-face face))
	  (set-motion-handler-highlight old-handler highlight)
	  (set-motion-handler-follow-point old-handler follow-cursor))
      (setq motion-handler-alist 
	    (cons (cons name 
			(setq new-handler (make-motion-handler-internal
					   name
					   boundary-function
					   (get-face face)
					   highlight
					   follow-cursor)))
		  motion-handler-alist)))
    (or old-handler new-handler)))

(defvar list-motion-handlers-buffer-to-customize nil
  "Name of buffer from where list-motion-handlers was called.")
(make-variable-buffer-local 'list-motion-handlers-buffer-to-customize)
(defvar list-motion-handlers-buffer-mode nil
  "Name of mode of buffer from where list-motion-handlers was called.")
(make-variable-buffer-local 'list-motion-handlers-buffer-mode)
;; Listing available motion handlers in tabular form. 
      
(defvar basic-motion-handlers (list 'mode-motion-highlight-line
				    'mode-motion-highlight-word
				    'mode-motion-highlight-symbol
				    'mode-motion-highlight-sexp)
  "The basic motion handlers provided by the underlying XEmacs.")

(defun list-motion-handlers ()
  "Display a list of available motion handlers.
The listing is in tabular form and contains the following columns:
NAME: the motion handlers name,
BOUNDARY FUNCTION: the name of the function used to compute the text  
   highlighted by the motion handler,
FACE: the face used to highlight the text.

Additionally, the following flags are used at the beginning of each line:
`*' Marks the motion handler current to the buffer this functions was called 
    from.
`H' Force highlighting of the selected text.
`F' Make point follow the mouse cursor as it moves."
  (interactive)
  (let ((current-handler (get-current-motion-handler))
	(buffer (current-buffer))
	(buffer-mode major-mode)
	(bmmh (if (symbolp mode-motion-hook)
		  (car (memq mode-motion-hook basic-motion-handlers))
		(if (and (listp mode-motion-hook) 
			 (equal 1 (length mode-motion-hook)))
		    (car (memq (car mode-motion-hook)
			       basic-motion-handlers))))))
    (save-excursion
      (with-output-to-temp-buffer "*Mouse Motion Handlers*"
	(let ((truncate-lines t))
	  (set-buffer "*Mouse Motion Handlers*")
	  (setq list-motion-handlers-buffer-to-customize buffer)
	  (setq list-motion-handlers-buffer-mode buffer-mode)
	  (let ((pos1 5)
		(pos2 25)
		(pos3 50)
		(handlers 
		 (sort 
		  (mapcar 'cdr motion-handler-alist)
		  '(lambda (x y)
		     (string<
		      (symbol-name (motion-handler-boundary-function x))
		      (symbol-name (motion-handler-boundary-function y)))))))
	    (if bmmh
		(let ((i 1)
		      (fw (frame-width)))
		  (while (< i fw)
		    (princ "*")
		    (setq i (1+ i)))
		  (princ "\nNote: this buffer is not using mode-motion+.\n\n")
		  (princ "It's using the `")
		  (princ bmmh)
		  (princ "' motion handler which claims it's:\n")
		  (insert (documentation bmmh))
		  (princ "\nSetting this motion handler will be irrevocable from this interface\n(but only for duration of this XEmacs session).\n")
		  (setq i 1)
		  (while (< i fw)
		    (princ "*")
		    (setq i (1+ i)))
		  (terpri)))
	    (princ "     NAME                BOUNDARY FUNCTION        FACE\n")
	    (princ "     ----                -----------------        ----\n")
	    (mapcar 
	     #'(lambda (handler)
		 (let ((line-start (point)))
		   (princ (if (and (not bmmh) (eq handler current-handler))
			      "*" " "))
		   (princ (if (eq handler default-motion-handler) "D" " "))
		   (princ (if (motion-handler-highlight handler) "H" " "))
		   (princ (if (motion-handler-follow-point handler) "F" " "))
		   (indent-to-column pos1 1)
		   (princ (motion-handler-name handler))
		   (indent-to-column pos2 1)
		   (princ (motion-handler-boundary-function handler))
		   (indent-to-column pos3)
		   (let ((face-start (point)))
		     (princ (face-name (motion-handler-face handler)))
		     (let ((line-extent (make-extent line-start face-start))
			   (face-extent (make-extent face-start (point))))
		     (set-extent-face face-extent
				      (motion-handler-face handler))
		     (set-extent-property
		      face-extent
		      'mode-motion-handler (motion-handler-name handler))
		     (set-extent-property
		      line-extent
		      'mode-motion-handler (motion-handler-name handler))
		     (set-extent-property line-extent 'highlight t)))
		 (terpri)))
	     handlers)
	    (princ (format "
Flags:	`D' the default motion handler
       	`H' handler with highlighting
	`F' handler with `following' property
	`*' the motion handler of buffer \"%s\""
			   list-motion-handlers-buffer-to-customize))))
	(local-set-key 'button3 'mode-motion-set-handler)
	(setq buffer-read-only t)))))

(defun mode-motion-set-handler (event)
  (interactive "@e")
  (let* ((handler (or (extent-property
		       (extent-at (event-point event) (current-buffer)
				  'mode-motion-handler)
		       'mode-motion-handler)
		      (error "Click on highlighted line to select a handler")))
	 (menu (list
		(format "Make `%s' the Motion Handler of :" handler)
		(vector (format "Buffer %s"
				list-motion-handlers-buffer-to-customize)
			(` (set-buffer-motion-handler
			    '(, list-motion-handlers-buffer-to-customize)
			    '(, handler))) t)
		(vector "Another Buffer..."
			(` (motion-handler-list-set-buffer-handler
			    '(, handler))) t)
		"---"
		(vector (format "Mode %s"
				list-motion-handlers-buffer-mode)
			(` (progn
			     (set-mode-motion-handler
			      '(, list-motion-handlers-buffer-mode)
			      '(, handler))
			     (save-excursion
			       (mapcar
				(function
				 (lambda (buf)
				   (set-buffer buf)
				   (and (eq
					 '(, list-motion-handlers-buffer-mode)
					 major-mode)
					(mode-motion+-hook-install buf t))))
				       (buffer-list))))) t)
		(vector "Another Mode..."
			(` (motion-handler-list-set-mode-handler
			    '(, handler))) t)
		"---"
		(vector "Default Motion Handler"
			(` (set-default-motion-handler '(, handler))) t))))
    (popup-menu menu)))

(defun motion-handler-list-set-buffer-handler (handler)
  (let ((buffer (read-buffer-name 
		 (format "Make `%s' the motion handler of buffer: " handler)
		 (buffer-name list-motion-handlers-buffer-to-customize))))
    (set-buffer-motion-handler buffer handler)
    (save-excursion
      (set-buffer buffer)
      (and (not (cond ((listp mode-motion-hook)
		       (memq 'mode-motion+-highlight mode-motion-hook))
		      ((symbolp mode-motion-hook)
		       (eq 'mode-motion+-highlight mode-motion-hook))
		      (t t)))
	   (y-or-n-p (format "Augment the default mode motion hook for `%s'? "
			     (buffer-name nil)))
	   (mode-motion+-hook-install buffer t)))))

(defvar mode-motion+-previous-hook nil
  "Value of previous `mode-motion-hook' in current buffer.")
(make-variable-buffer-local 'mode-motion+-previous-hook)

(defun motion-handler-list-set-mode-handler (handler)
  (let ((mode (intern (completing-read
	    (format "Make `%s' the motion handler of mode: " handler)
	    obarray
	    'fboundp
	    t	
	    (symbol-name list-motion-handlers-buffer-mode)))))
    (set-mode-motion-handler mode handler)
  (save-excursion
    (mapcar (function
	     (lambda (buf)
	       (set-buffer buf)
	       (and (eq mode major-mode)
		    (mode-motion+-hook-install buf t))))
	    (buffer-list)))))

(defun mode-motion+-hook-install (&optional buffer remove-highlight-line-p)
  "Add `mode-motion+-highlight' to the BUFFER `mode-motion-hook'.
If the optional second arg REMOVE-HIGHLIGHT-LINE-P is t, remove
`mode-motion-highlight-line' from `mode-motion-hook'.
See `mode-motion+-hook-uninstall' for reverting this operation."
  (interactive "bInstall mode-motion+ hook for buffer :
XRemove highlight-line from hook ? :")
  ;; Check for the mode-motion-hook value to make sure it's under
  ;; the control of mode-motion+.
  ;; The reasonning here is that if the user went trough the hassles
  ;; of list-motion-handlers (or if he's calling this directly from
  ;; his .emacs) , he is prepared to give up on the current
  ;; mode-motion-hook.
  ;; However, we keep the previous hook value in a buffer-local
  ;; variable: it will be then possible to revert to the old motion
  ;; handling behavior with `mode-motion+-hook-uninstall'.
  ;; --paquette, Mon Feb 27 08:54:30 1995 (Marc Paquette)
  (setq buffer (or buffer (current-buffer)))
  ;; force the uninstall of mode-motion-highlight since if its second
  ;; you'll never see ours.
  (setq remove-highlight-line-p t)
  (save-excursion
    (set-buffer buffer)
    (if (boundp 'mode-motion-hook)
	(progn
	  (setq mode-motion+-previous-hook
		(cond ((sequencep mode-motion-hook)
		       (copy-sequence mode-motion-hook))
		      (t mode-motion-hook)))
	  ;; Make sure that the mode-motion+-highlight is not saved in
	  ;; the variable, otherwise, we could not revert back to the
	  ;; "factory settings" after having played with different
	  ;; handlers
	  ;; --paquette, Mon Feb 27 08:54:21 1995 (Marc Paquette)
	  (remove-hook 'mode-motion+-previous-hook 'mode-motion+-highlight)))
    (add-hook 'mode-motion-hook 'mode-motion+-highlight)
    (and remove-highlight-line-p
	 ;; Remove the standard mode-motion-highlight hook because we
	 ;; provide an alternative to this.  I don't use setq here because
	 ;; something else could be hooked to mode-motion-hook.
	 ;; --paquette, Mon Feb 27 08:53:51 1995 (Marc Paquette)
	 (remove-hook 'mode-motion-hook 'mode-motion-highlight-line))
    (and mode-motion-extent
	 (delete-extent mode-motion-extent)
	 (setq mode-motion-extent nil))
    ;; Try to make this installed for any buffer of this mode
    (let ((this-mode-hook (intern-soft (concat (symbol-name major-mode)
					       "-hook"))))
      (and (boundp this-mode-hook)
	   (if remove-highlight-line-p
	       (add-hook this-mode-hook
			 #'(lambda () (mode-motion+-hook-install nil t))
			 'append)
	     (add-hook this-mode-hook 'mode-motion+-hook-install 'append)))))
  mode-motion-hook)

(defun mode-motion+-hook-uninstall (buffer)
  "Restore the value of `mode-motion-hook' in BUFFER to what it was at the time `mode-motion+-hook-install' was called.
See also `mode-motion+-hook-install'."
  (interactive "bRestore `mode-motion-hook' of buffer :")
  ;; Check for the mode-motion-hook value to make sure it's under
  ;; the control of mode-motion+.
  ;; The reasonning here is that if the user went trough the hassles
  ;; of list-motion-handlers (or if he's calling this directly from
  ;; his .emacs) , he is prepared to give up on the current
  ;; mode-motion-hook.
  ;; However, we keep the previous hook value in a buffer-local
  ;; variable: it will be then possible to revert to the old motion
  ;; handling behavior with `mode-motion+-hook-uninstall'.
  ;; --paquette, Mon Feb 27 08:54:30 1995 (Marc Paquette)
  (save-excursion
    (set-buffer buffer)
    (and mode-motion-extent
	 (delete-extent mode-motion-extent)
	 (setq mode-motion-extent nil))
    (if (boundp 'mode-motion+-previous-hook)
	(progn
	  (setq mode-motion-hook mode-motion+-previous-hook)
	  (setq mode-motion+-previous-hook nil)
	  (let ((this-mode-hook (intern-soft (concat (symbol-name major-mode)
						     "-hook"))))
	    (and (boundp this-mode-hook)
		 (remove-hook this-mode-hook 'mode-motion+-hook-install))))
      (error "No previous value for mode-motion-hook")))
  mode-motion-hook)

(defun mode-motion+-motion-hook-revert (&optional buffer-only-p buffer mode)
  "Revert the `mode-motion-hook' to its original value.
With optional arg BUFFER-ONLY-P non-nil, only revert in BUFFER
\(defaults to `\(current-buffer\)'\); otherwise, revert for all existing
buffers of MODE \(defaults to `major-mode' of BUFFER\)."
  (interactive
    (let* ((buffer-only-p
	   (y-or-n-p "Revert mode-motion-hook only for current buffer ? "))
	   (buffer (if buffer-only-p
		       (current-buffer)
		     (get-buffer (read-buffer-name
				  "Revert mode-motion-hook of buffer : "
				  (buffer-name (current-buffer))))))
	   (mode (if buffer-only-p
		     (save-excursion
		       (set-buffer buffer)
		       major-mode)
		   (intern-soft (completing-read "Major mode: "
						 obarray
						 'fboundp	
						 nil 
						 (symbol-name major-mode))))))
      (list buffer-only-p buffer mode)))
  (if buffer-only-p
      (mode-motion+-hook-uninstall buffer)
    (save-excursion
      (mapcar (function
	       (lambda (buf)
		 (set-buffer buf)
		 (and (eq mode major-mode)
		      (mode-motion+-hook-uninstall buf))))
	      (buffer-list)))))
  

;; Setting buffer, mode and default motion handlers

(defun set-buffer-motion-handler (buffer handler-name)
  "Make the motion handler named HANDLER-NAME (a symbol) the buffer
motion handler of BUFFER.  If HANDLER-NAME is nil, the corresponding
buffer motion handler is removed.  If HANDLER-NAME isn't the name of a
known motion handler, an error is signaled. When called interactively,
completion is provided for available buffers and motion handlers.

	1.\) buffer motion handler
	2.\) mode motion handler
	3.\) default motion handler"
  (interactive (list (read-buffer-name "Set motion handler of buffer: "
				       (buffer-name (current-buffer)))
		     (read-motion-handler-name)))

  ;; kill old mode motion extent, because the new handler
  ;; might want to initialize it differently
  (if mode-motion-extent 
      (progn 
	(detach-extent mode-motion-extent)
	(setq mode-motion-extent nil)))
  (or buffer (setq buffer (current-buffer)))
  (or (get-buffer buffer)
      (error "No such buffer: %s" buffer))
  (save-excursion 
    (set-buffer buffer)
    (setq buffer-motion-handler
	  ;; remove it if `nil'
	  (and handler-name
	       ;; set the handler if known
	       (or (find-motion-handler handler-name)
		   ;; error otherwise
		   (error "Not a known motion handler: %s"
			  handler-name)))))
  (if (interactive-p)
      (if handler-name
	  (message "Motion handler for buffer %s is `%s'." 
		   (current-buffer) handler-name)
	  (message "Motion handler removed for buffer %s." 
		   (current-buffer))))
  handler-name)

(defun read-buffer-name (prompt &optional initial-input)
  (completing-read prompt
		   (mapcar #'(lambda (buf) (list (buffer-name buf)))
			   (buffer-list))
		   ;; don't take buffers that start with a blank
		   #'(lambda (list) (not (eq (aref (car list) 0) ? )))
		   t
		   initial-input))

(defun set-mode-motion-handler (mode handler-name)
  "Make the motion handler named HANDLER-NAME (a symbol) the mode motion
handler for all buffers with major-mode MODE.  If HANDLER-NAME is nil,
the corresponding mode motion handler is removed.  If HANDLER-NAME
isn't the name of a known motion handler, an error is signaled. When
called interactively, completion is provided for available motion
handlers.

	1.\) buffer motion handler
	2.\) mode motion handler
	3.\) default motion handler"
  (interactive (list (intern (completing-read "Major mode: "
					      obarray
					      'fboundp	
					      nil 
					      (symbol-name major-mode)))
		     (read-motion-handler-name)))
  ;; kill old mode motion extent, because the new handler
  ;; might want to initialize it differently
  (if mode-motion-extent 
      (progn 
	(detach-extent mode-motion-extent)
	(setq mode-motion-extent nil)))
  (put mode 'mode-motion-handler 
	;; remove it if `nil'
	(and handler-name
	     ;; set the handler if known
	     (or (find-motion-handler handler-name)
		 ;; error otherwise
		 (error "Not a known mode motion handler: %s" handler-name))))
  (if (interactive-p)
      (if handler-name
	  (message "Motion handler for %s is `%s'." mode handler-name)
	  (message "Mode motion handler for %s removed." mode)))
  handler-name)

(defun set-default-motion-handler (handler-name)
  "Make the motion handler named HANDLER-NAME (a symbol) the default.

If HANDLER-NAME is nil, the current default motion handler is removed.  If
HANDLER-NAME isn't the name of a known motion handler, an error is
signalled. When called interactively, completion is provided for available
motion handlers.

The motion handler used in a given buffer is determined by the following
most-specific first list: buffer motion handler, mode motion handler, default
motion handler." 
  (interactive (list (read-motion-handler-name)))
  ;; kill old mode motion extent, because the new handler
  ;; might want to initialize it differently
  (if mode-motion-extent 
      (progn 
	(detach-extent mode-motion-extent)
	(setq mode-motion-extent nil)))
  (setq default-motion-handler
	;; remove it if `nil'
	(and handler-name
	     ;; set the handler if known
	     (or (find-motion-handler handler-name)
		 ;; error otherwise
		 (error "Not a known motion handler: %s" handler-name))))
  (if (interactive-p)
      (if handler-name
	  (message "Default motion handler is `%s'." handler-name)
	  (message "Default motion handler removed.")))
  handler-name)

(defun read-motion-handler-name ()
  (intern-soft (completing-read "Motion handler: "
				(mapcar #'(lambda (entry)
					    (list (symbol-name (car entry))))
					motion-handler-alist)
				nil t)))

;; clear the last active motion extent when leaving a frame.
(defun mode-motion-clear-extent (&optional extent)
  "Clear EXTENT, i.e. make it have no visible effects on the frame.
EXTENT defaults to the current buffer's mode-motion-extent."
  (or extent (setq extent mode-motion-extent))
  (and extent 
       (extent-live-p extent)
       (not (extent-detached-p extent))
       (extent-buffer extent)
       (buffer-name (extent-buffer extent))
       (progn 
	 ;; unhighlight it 
	 (highlight-extent extent nil)
	 ;; make it span a region that isn't visible and selectable
	 ;; Can this be done more elegantly? 
	 (detach-extent extent))))

(defun mode-motion-clear-last-extent (&optional frame)
  "Clear the mode-motion-last-extent."
  (or (popup-up-p) (mode-motion-clear-extent mode-motion-last-extent)))

(defun mode-motion+-highlight (event)
  "Highlight the thing under the mouse using a mode-specific motion handler.
See list-motion-handlers for more details."
  (mode-motion-clear-last-extent)
  (and (event-buffer event)
       (cond ((and mouse-grabbed-buffer
		   ;; first try to do minibuffer specific highlighting
		   (find-motion-handler 'minibuffer)
		   (let ((mode-motion-highlight-lines-when-behind nil))
		     (and (event-point event)
			  (or (extent-at (event-point event)
					 (event-buffer event) 'highlight)
			      (mode-motion-highlight-with-handler
			       (find-motion-handler 'minibuffer) event))))))
	     (t (mode-motion-highlight-with-handler
		 (get-current-motion-handler) event))))
  ;; Return nil since now this is used as a hook, and we want to let
  ;; any other hook run after us.
  nil)

(defun get-current-motion-handler ()
  (or (and (boundp 'buffer-motion-handler) buffer-motion-handler)
      (get major-mode 'mode-motion-handler)
      default-motion-handler))

(defun mode-motion-highlight-with-handler (handler event)
  ;; Perform motion highlighting using HANDLER. Information about the
  ;; current mouse position is taken form EVENT. 
  (and handler
       (let ((point (event-point event))
	     (buffer (event-buffer event))
	     (window (event-window event))
	     (window-config (current-window-configuration))
	     (buffer-save (current-buffer))
	     (point-save (point))
	     region)
	 ;; save-window-excursion and restore buffer
	 (unwind-protect
	     (progn
	       (and buffer
		    (set-buffer buffer)
		    (select-window window))
	       ;; Create a new mode-motion-extent if there isn't one 
	       ;; (or a destroyed one)
	       (if (and (extent-live-p mode-motion-extent)
			(extent-buffer mode-motion-extent))
		   ()
		 (setq mode-motion-extent (make-extent nil nil buffer))
		 (set-extent-priority mode-motion-extent 1))
	       (if (and 
		    ;; compute the region to be highlighted
		    (setq region
			  (if point
			      ;; compute the mode-motion region using the 
			      ;; handlers boundary function
			      (condition-case nil;; c
				  (funcall
				   (motion-handler-boundary-function handler)
				   point)
				;; Messages that appear during computing the
				;; region may be displayed not done
				;; here because it's rather disturbing
				(error
				 ;; (setq message (format "%s" (car (cdr c))))
				 nil))
			    ;; otherwise highlight the whole line mouse is
			    ;; behind but only if the line isn't empty
			    (if mode-motion-highlight-lines-when-behind
				(unwind-protect
				    (progn 
				      ;; (message "%s" (event-window event))
				      (move-to-window-line
				       (if (and (= emacs-major-version 19)
						(< emacs-minor-version 12))
					   (- (event-y event)
					      (nth 1 (window-edges window)))
					 (event-y event)))
				      (beginning-of-line)
				      (if (= (following-char) ?\n)
					  ;; empty line 
					  ()
					(thing-region
					 (point)
					 (progn 
					   (end-of-line)
					   ;; for `follow-point' behavoir
					   (setq point (point))
					   ;; fetch also the newline, if any
					   ;; -- handy for copying >1 line
					   (if (eobp) point (1+ point))))))
				  (goto-char point-save)))))
		    ;; (message "region: %s" region)
		    ;; the region might be in reverse order. Stop in this case
		    (<= (car region) (cdr region)))
		   (if (or (not (motion-handler-follow-point handler))
			   (pos-visible-in-window-p point))
		       (progn
			 (set-extent-endpoints
			  mode-motion-extent (car region) (cdr region))
			 (set-extent-face
			  mode-motion-extent (motion-handler-face handler))
			 ;; make point follow the mouse or point to
			 ;; the beginning of the line do not move the
			 ;; cursor if a mark is set.
			 (cond ((and (motion-handler-follow-point handler)
				     (not (mark)))
				(goto-char point)
				;; kludge to keep the cursor out the way
				(if (or (eq (motion-handler-boundary-function
					     handler)
					    'line-boundaries)
					(eq (motion-handler-boundary-function
					     handler)
					    'visible-line-boundaries))
				    (beginning-of-line))))
			 (if (and mode-motion-focus-on-window
				  (or (eq mode-motion-focus-on-window t)
				      (motion-handler-follow-point handler)))
			     ;; Select the current window FROM OUTSIDE the
			     ;; `save-window-excursion' that surrounds the call
			     ;; to the current function. This also avoids
			     ;; conflicts with running process filters.
			     (enqueue-eval-event 'select-window
						 (selected-window)))
			 ;; snap in effect, but it ain't yet workin'
			 ;; (message "X: %sl; Y: %s"(event-x event)(event-y event))
			 ;; (and motion-handler-snap-in
			 ;;    (set-mouse-position
			 ;;    (window-frame (event-window event))
			 ;;    (event-x event)
			 ;;    (event-y event)))
			 (setq mode-motion-last-extent mode-motion-extent)
			 ;; signal success
			 t))
		 ;; signal failiure
		 nil))
	   (set-window-configuration window-config)
	   (set-buffer buffer-save)))))

;; Motion Event debugging
;;
;; Useful to see what information is available from motion events

(defun debug-motion-handler (event)
   (let* ((window (event-window event))
	  (frame (or (event-frame event) (selected-frame)))
	  (buffer (and window (event-buffer event)))
	  (point  (and buffer (event-point event))))
     (with-output-to-temp-buffer "*Debug Motion Handler Output*"
       (princ 
	(format "\
 Window: %s
 Frame: %s
 Buffer: %s
 (event-x, event-y): (%s, %s)
 (event-x-pixel, event-y-pixel): (%s, %s)
 Point: %s
 Timestamp: %s"
	window 
        frame 
        buffer 
        (event-x event) (event-y event) 
        (event-x-pixel event) (event-y-pixel event)
        point 
        (event-timestamp event))))))

;(let ((mouse-motion-handler 'debug-motion-handler)
;      (temp-buffer-show-function nil))
;  (read-char))

;; Set of copy/kill/move functions for usage with highlighted regions

(put 'mode-motion-move 'pending-delete t)
(put 'mode-motion-copy 'pending-delete t)

(defun mode-motion-move ()
  "Move the motion active region to point." 
  (interactive)
  (mode-motion-insert-text (mode-motion-copy/delete t)))

(defun mode-motion-kill ()
  "Kill the motion active region and push it onto the kill ring."
  (interactive)
  (mode-motion-copy/delete t t t))

(defun mode-motion-copy ()
  "Copy the motion active region to point."
  (interactive)
  (mode-motion-insert-text (mode-motion-copy/delete)))

(defun mode-motion-copy-as-kill ()
  "Delete the motion active region and push it onto the kill ring.
Set point to the place where deletion happened."
  (interactive)
  (mode-motion-copy/delete nil t)
  (message "Text copied to the to ring and cut buffer."))

(defun mode-motion-copy/delete (&optional delete copy-as-kill set-point)
  "Return the string that is designated by the current motion active region. 
Arguments are:
           EVENT - a mouse click event used to identify the buffer and window 
&optional DELETE - delete the motion active text region
    COPY-AS-KILL - copy the string to the kill ring
       SET-POINT - set point to the start of the motion active region."
  (let ((old-buf (current-buffer))
	(old-window (selected-window)))
    (unwind-protect 
	(let ((extent (or primary-selection-extent
			  (and (extentp mode-motion-last-extent)
			       (not (extent-property mode-motion-last-extent
						     'detached))
			       mode-motion-last-extent))))

	  (if (and (extentp extent)
		   (set-buffer (extent-buffer extent))
		   (not 
		    ;; zero length extents
		    (= (extent-start-position extent)
		       (extent-end-position extent))))

	      (let* ((start (extent-start-position extent))
		     (end (extent-end-position extent))
		     (text 
		      (buffer-substring
		       (extent-start-position extent)
		       (extent-end-position extent))))

		(cond (copy-as-kill
		       (copy-region-as-kill start end)
		       (if (or (not kill-hooks)
				(eq kill-hooks 'ignore))
			   (progn 
			     (x-own-selection-internal 'PRIMARY text)
			     (x-own-clipboard text)))))

		(cond (delete 
		       (kill-region start end)
		       (x-own-selection-internal 'PRIMARY text)
		       ;; (select-window window)
		       (if set-point 
			   (goto-char start))))

		(setq this-command 'mode-motion+)
		text)
	    (error "No current primary or motion selection.")
	    ))
      (set-buffer old-buf)
      (select-window old-window))))

(defun mode-motion-insert-text (text)
  "Insert TEXT at point. Also insert one space if the 
preceding character is a word constituent or a closing paren."
  (or text (error "No highlighted text to copy."))
  (let ((prec-char-syntax (char-syntax (preceding-char))))
    (if (memq  prec-char-syntax '(?w ?\))) (insert " "))
    (insert text)))

;; Boundary functions
;;
;; The following  functions are already provided by the thing package:
;; thing-boundaries
;; thing-symbol
;; thing-word

(defun char-boundaries (point) (thing-region point (1+ point)))
      
(defun visible-line-boundaries (point)
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (and (eq major-mode 'dired-mode)
	     (save-excursion (dired-move-to-filename)))
	(let ((start (point)))
	  (end-of-line)
	  (skip-chars-backward " \t")
	  (thing-region start (point))))))

(defun line-boundaries (point)
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (if (and (eq major-mode 'dired-mode)
	     (save-excursion (dired-move-to-filename)))
	(let ((start (point)))
	  (end-of-line)
	  (thing-region start (point))))))

(defun cvs-line-boundaries (point)
    (save-excursion
      (goto-char point)
      (beginning-of-line)
      (if (looking-at "^[* ] ")
	  (thing-region  (point) (progn (end-of-line) (point))))))
      
(defun latex-boundaries (here)
  (setq *last-thing* 'sexp)
  (tex-boundaries 
   here ?\\ "a-zA-Z"
   ;; begin-fwd-regexp
   "\\\\begin *{ *\\([a-z]*\\) *}"
   ;; end-fwd-regexp
   "\\(\\\\end *{ *%s *}\\)\\|\\(\\\\begin *{ *%s *}\\)"
   ;; begin-bwd-regexp
   "\\\\end *{ *\\([a-z]*\\) *}"
   ;; begin-bwd-regexp
   "\\(\\\\end *{ *%s *}\\)\\|\\(\\\\begin *{ *%s *}\\)"
   ;; param-cmd-regexp
   "\\\\[a-zA-Z]+[ \n\t]*{"))

(defvar texinfo-paired-commands 
  (mapconcat 
   'identity 
   '(
     "enumerate"
     "example"
     "group"
     "ifinfo" 
     "iftex" 
     "ignore" 
     "itemize"
     "menu"
     "quotation"
     "table"
     "tex"
     "titlepage"
     ) 
   "\\|"))

(defvar texinfo-begin-fwd-regexp 
  (format "@\\(%s\\)" texinfo-paired-commands))
(defvar texinfo-end-bwd-regexp
  (format "@end *\\(%s\\)" texinfo-paired-commands))

(defun texinfo-boundaries (here)
  (tex-boundaries 
   here ?@ "a-z"
   texinfo-begin-fwd-regexp
   ;; end-fwd-regexp
   "\\(@end *%s\\)\\|\\(@%s\\)"
   ;; end-bwd-regexp
   texinfo-end-bwd-regexp
   ;; begin-bwd-regexp
   "\\(@end *%s\\)\\|\\(@%s\\)"
   ;; param-cmd-regexp
   "@\\(TeX\\|[a-zA]+\\)[ \n\t]*{"))

(defun tex-boundaries 
  (here cmd-start-character cmd-word-character
	begin-fwd-regexp end-fwd-regexp
	end-bwd-regexp begin-bwd-regexp
	param-cmd-regexp)
  "Generic TeX dialect scanner.
Parameters: 
cmd-start-character: character that starts a command 
	(`\' in (La)TeX, `@' in Texinfo)
cmd-word-character:  regexpression to be used by the function
	`skip-chars-backward' allowing to skip over command 
	characters other than `cmd-start-character'
begin-fwd-regexp: regexpression matching the begin part of a 
	text stretch, used in forward search. 
end-fwd-regexp: regexpression matching the end part of a 
	text stretch, used in forward search
end-bwd-regexp: regexpression matching the end part of a 
	text stretch, used in backward search.
begin-bwd-regexp: regexpression matching the begin part of a 
	text stretch, used in backward search.
param-cmd-regexp: regexpression matching a parameterized command 
        \(including the open parenthesis\)"
  (save-excursion
    (goto-char here)
    (cond ((= (following-char) cmd-start-character)
	   (forward-char 1))
	  ((= (char-syntax (following-char)) ?w)
	   (skip-chars-backward cmd-word-character)))
    (if (/= (preceding-char) cmd-start-character)
	(thing-boundaries here)
      (forward-char -1)
      (catch 'return 
	(cond ((looking-at begin-fwd-regexp)
	       (let* ((start (point))
		      (env (buffer-substring 
			    (match-beginning 1) (match-end 1)))
		      (regexp (format end-fwd-regexp env env))
		      (count 0))
		 (while (re-search-forward regexp nil t)
		   (cond ((match-beginning 2) ; \begin
			  (setq count (1+ count)))
			 ((match-beginning 1) ; \end
			  (setq count (1- count))
			  (if (= count 0) 
			      (throw 'return 
				     (thing-region start (point)))))))))
	      ((looking-at end-bwd-regexp)
	       (let* ((end (match-end 0))
		      (env (buffer-substring 
			    (match-beginning 1) (match-end 1)))
		      (regexp 
		       (format begin-bwd-regexp env env))
		      (count 1))
		 (while (re-search-backward regexp nil t)
		   (cond ((match-beginning 1) ; \end
			  (setq count (1+ count)))
			 ((match-beginning 2) ; \begin
			  (setq count (1- count))
			  (if (= count 0) 
			      (throw 'return (thing-region (point) end))))
			 ))))
	      ;; tex macros of the form \cmd {...}
	      ((looking-at param-cmd-regexp)
	       (thing-region 
		(point)
		(progn 
		  (goto-char (1- (match-end 0)))
		  (forward-sexp 1)
		  (point))))
	      ;; fetch the current macro (with backslash)
	      (t (thing-region (point) (progn (forward-word 1) (point)))))))))

;; special parse of buffer for valid selectable info
(defun minibuffer-selection-boundaries (point)
  (let ((old-syntax (syntax-table)))
    (unwind-protect
	(progn 
	  ;; best syntax table for recognizing symbols
	  (set-syntax-table emacs-lisp-mode-syntax-table)
	  (let ((file-completion (eq minibuffer-completion-table
				     'read-file-name-internal))
		region
		minibuf-string		;contents of minibuffer
		buffer-string		;string to be highlighted (or not)
		prefix			;prefix calculated from minibuf-string
		string			;string to be verified in the
					;completion table 
		)
	    (and

	     (setq region (if file-completion
			      (thing-filename point)
			    (thing-symbol point)))

	     (setq
	      minibuf-string		; contents of minibuffer
	      (save-excursion
		(set-buffer mouse-grabbed-buffer)
		(buffer-string))

	      buffer-string		; string to be highlighted (or not)
	      (buffer-substring (car region) (cdr region))
		       
	      prefix
	      (if file-completion
		  (file-name-nondirectory minibuf-string)
		minibuf-string)

	      string
	      (if file-completion
		  (concat (file-name-directory minibuf-string) buffer-string)
		buffer-string))
	     
	     (if (or (and (fboundp 'ange-ftp-ftp-path)
			  (or (ange-ftp-ftp-path buffer-string)
			      (ange-ftp-ftp-path string)))
		     (and (fboundp 'efs-ftp-path)
			  (or (efs-ftp-path buffer-string)
			      (efs-ftp-path string))))
		 ;; #### Like our counterpart in mode-motion: evil evil evil
		 t
	       (if file-completion
		   (try-completion string
				   minibuffer-completion-table
				   minibuffer-completion-predicate)
		 (eq 't (try-completion string
					minibuffer-completion-table
					minibuffer-completion-predicate))))

	     ;; the result is the region to be highlighted
	     region)))
      (set-syntax-table old-syntax))))

;; C source code scanner 
(defvar c-statement-starting-keyword-regexpr
  "\\(if\\|for\\|while\\|do\\|switch\\|break\\|continue\\)\\b")

(defun c-boundaries (here)
  (setq *last-thing* 'sexp)
  (save-excursion
    (goto-char here)
    (let ((following-char (following-char))
	  (preceding-char (preceding-char))
	  aux)
      (if (= (char-syntax following-char) ?w)
	  (progn 
	    (skip-chars-backward "a-zA-Z")
	    (setq aux (point))
	    (skip-chars-backward "\n\t ")
	    (if (= (preceding-char) ?#)
		(forward-char -1)
	      (goto-char aux))))
      (if (and (= following-char ?*)
	       (= preceding-char ?/))
	  (forward-char -1))
      (if (and (= following-char ?/)
	       (= preceding-char ?*))
	  (forward-char -1))
      (cond
       ((= (following-char) ?#) (c-scan-preproc-macros))
       ((looking-at "/\\*")	; begin comment
	(let ((start (match-beginning 0)))
	  (if (search-forward "*/" nil t)
	      (thing-region start (match-end 0)))))
       ((looking-at "\\*/")	; end comment
	(let ((end (match-end 0)))
	  (if (search-backward "/*" nil t)
	      (thing-region (match-beginning 0) end))))
       ((looking-at c-statement-starting-keyword-regexpr) ; if for while do etc
	(thing-region (match-beginning 0)
		      (c-forward-statement 
		       (buffer-substring (match-beginning 1) (match-end 1)))))
       ((looking-at "else\\b")
	(thing-region (match-beginning 0) (c-forward-else)))
       (t (if (= (char-syntax (following-char)) ?.)
	      (thing-region here  (1+ here))
	    (thing-boundaries here)))))))


(defun c-scan-preproc-macros ()
  (cond 
   ((looking-at "^#[ \n\t]*include[ \n\t]*[<\"][^>\"]*[>\"]")   ; #include
    (thing-region (match-beginning 0) (match-end 0)))
   ((looking-at "^#[ \n\t]*\\(define\\|undef\\)") ; #define, #undef
    (thing-region
     (match-beginning 0) 
     (progn 
       (end-of-line)
       (while (= (preceding-char) ?\\)
	 (forward-line 1)
	 (end-of-line))
       (point))))
   ;; #if, #ifdef, #ifndef, #else, #elif
   ((looking-at "^#[ \n\t]*\\(if\\|ifdef\\|ifndef\\|else\\|elif\\)\\b")
    (let ((start (match-beginning 0))
	  (counter 1)
	  match)
      (goto-char (match-end 0))
      (while (and (>= counter 1)
		  (re-search-forward 
		   "^#[ \n\t]*\\(if\\|ifdef\\|ifndef\\|endif\\)\\b"
		   nil t))
	(setq match 
	      (buffer-substring (match-beginning 1) (match-end 1)))
	(setq counter 
	      (if (string= match "endif")
		  (1- counter)
		(1+ counter))))
      (if (= counter 0)
	  (thing-region start (match-end 0)))))
   ((looking-at "^#[ \n\t]*endif\\b")   ; #endif
    (let ((end (match-end 0))
	  (counter 1)
	  match)
      (goto-char (match-beginning 0))
      (while (and (>= counter 1)
		  (re-search-backward
		   "^#[ \n\t]*\\(if\\|ifdef\\|ifndef\\|endif\\)\\b"
		   nil t))
	(setq match 
	      (buffer-substring (match-beginning 1) (match-end 1)))
	(setq counter 
	      (if (string= match "endif")
		  (1+ counter)
		(1- counter))))
      (if (= counter 0)
	  (thing-region (match-beginning 0) end))))))

(defun c-skip-over-comment ()
  (let ((aux (point)))
    (skip-chars-forward "\n\t ")
    (or (and (= (following-char) ?/)
	     (= (char-after (1+ (point))) ?*)
	     (search-forward "*/" nil t)
	     (point))
	(goto-char aux))))

(defun c-forward-statement (&optional keyword)
  (c-skip-over-comment)
  (skip-chars-forward " \n\t")
  (or keyword (setq keyword 
		    (if (looking-at c-statement-starting-keyword-regexpr)
			(buffer-substring 
			 (match-beginning 1)
			 (match-end 1)))))
  (if keyword
      (cond ((string= keyword "if")
	     (c-forward-if))
	    ((string= keyword "do")
	     (c-forward-do-while))
	    ((member keyword '("for" "while" "switch"))
	     (c-forward-for/while/switch))
	    ((member keyword '("break" "continue"))
	     (c-forward-break/continue)))
    (cond ((= (following-char) ?\{)
	   (forward-list 1)
	   (point))
	  (t
	   ;; Here I use that each C statement other then 
	   ;; a bloc, if, while, for, do ... ends in a `;'
	   (let (char)
	     (catch 'exit
	       (while t
		 (if (eobp) (throw 'exit nil))
		 (setq char (following-char))
		 (cond ((= (char-syntax char) ?.) 
			(forward-char 1)
			(if (= char ?\;) (throw 'exit (point))))
		       (t (forward-sexp 1)
			  (skip-chars-forward " \n\t"))))))))))

(defun c-forward-if ()
  (let (aux)
    (forward-word 1) ; if
    (forward-list 1) 
    (c-forward-statement)
    (setq aux (point))
    (skip-chars-forward "\n\t ")
    (if (looking-at "else\\b")
	(c-forward-else)
	(goto-char aux))))

(defun c-forward-else ()
  (forward-word 1) ; else
  (c-forward-statement))

(defun c-forward-for/while/switch ()
  (forward-word 1) ; for
  (forward-list 1)
  (c-forward-statement))

(defun c-forward-do-while ()
  (forward-word 1) ; do ... while
  (c-forward-statement)
  (c-forward-for/while/switch))

(defun c-forward-switch ()
  (forward-word 1) ; switch
  (forward-list 2)
  (point))

(defun c-forward-break/continue ()
  (forward-word 1) ; keyword
  (c-skip-over-comment)
  (skip-chars-forward "\n\t ")
  (if (= (following-char) ?\;)
      (goto-char (1+ (point)))))

;; Tcl syntax scanner
(defvar tcl-builtin-commands nil
  "Alist of information about tcl syntax for the tcl-boundaries function.  
An entry has the form 
	\(<command-string> . <syntax description>\) 
where 
	<command-string>     is the name of a tcl command
	<syntax description> is one of 
            list of integers: the number of possible arguments
	    t:		      any number of arguments")

(defconst tcl-commands
  '(
    ("append"	. (2 . nil))
    ("array"	. (2 . 3))
    ("break"	. 0)
    ("case" 	. 3)
    ("catch"	. 1)
    ("cd"	. 1)
    ("close"	. 1)
    ("concat"	. t)
    ("continue"	. 0)
    ("else" 	. (1 . nil))
    ("elseif"	. (1 . nil))
    ("eof"	. 1)
    ("error"	. t)
    ("eval"     . t)
    ("exec"     . t)
    ("exit"	. (0 . 1))
    ("expr" 	. 1)
    ("file"	. (2 . nil))
    ("flush"	. 1)
    ("for" 	. 4)
    ("foreach" 	. 3)
    ("format"	. (1 . nil))
    ("gets"	. (1 . 2))
    ("glob"	. t)
    ("global" 	. (1 . nil))
    ("history"	. t)
    ("if" 	. (2 . nil))
    ("incr" 	. (1 . 2))
    ("info"	. (1 . 4))
    ("join"	. (1 . 2))
    ("lappend"	. (2 . nil))
    ("lindex" 	. 2)
    ("linsert"	. (3 . nil))
    ("list"	. t)
    ("llength" 	. 1)
    ("lrange" 	. 3)
    ("lreplace"	. (3 . nil))
    ("lsearch" 	. 2)
    ("lsort"	. 1)
    ("open"	. (1 . 2))
    ("proc" 	. 3)
    ("puts"	. (1 . 3))
    ("pwd"	. 0)
    ("read"	. (1 . 2))
    ("regexp"	. (2 . nil))
    ("regsub"	. (4 . 6))
    ("rename"	. 2)
    ("return"	. (0 .1))
    ("scan"	. (3 . nil))
    ("seek"	. (2 . 3))
    ("set" 	. (1 . 2))
    ("source"	. 1)
    ("split"	. (1 . 2))
    ("string"	. (2 . 4))
    ("tell"	. 1)
    ("time"	. (1 .2))
    ("trace"	. (1 . nil))
    ("unknown"	. (1 . nil))
    ("unset"	. (1 . nil))
    ("uplevel"	. (1 . nil))
    ("upvar"	. (2 . nil))
    ("while" 	. 2)
    ))

(defconst tk-commands
  '(("bind"	. 3)
    ("button"	. t)
    ("canvas"	. t)
    ("frame"	. t)
    ("label"	. t)
    ("listbox"	. t)
    ("menu"	. t)
    ("menubutton"	. t)
    ("pack"	. t)
    ("scrollbar"	. t)
    ("tree"	. t)
    ("wm"		. t)
    ))

(defconst tcl-tk-commands
  (nconc tcl-commands tk-commands))
  
(defconst tcl-tk-commands-regexp
  (format "\\(%s\\\)\\W" (mapconcat 'car tcl-tk-commands "\\|")))

(defun tcl-boundaries (here)
  (save-excursion
    (goto-char here)
    (skip-chars-backward "a-z")
    (if (looking-at 
	 tcl-tk-commands-regexp)
	(let* ((count 0) 
	      (start (point))
	      (keyword (buffer-substring
			(match-beginning 1)
			(match-end 1)))
	      (syntax-description 
	       (cdr (assoc keyword tcl-tk-commands))))
	  (goto-char (match-end 0))
	  (while (not (looking-at "[ \t]*[]\n;}]"))
	    (setq count (1+ count))
	    (tcl-forward-sexp1)
	    ;; skipping over the parentheses of array expressions:
	    (while (not (or (looking-at "[ \t]*[]\n;}]")
			    (= (char-syntax (following-char)) ? )))
	      (tcl-forward-sexp1)))

	  (if (cond ((eq syntax-description t))
		    ((integerp syntax-description) 
		     (= syntax-description count))
		    ((consp syntax-description)
		     (and (<= (car syntax-description) count)
			  (or (null (cdr syntax-description))
			      (<= count (cdr syntax-description))))))
	      (progn 
		(message "`%s' matched."  keyword)
		(thing-region start (point)))
	    (progn 
	      (message "wrong syntax: `%s'."  keyword)
	      nil)))
      (message "")
      (thing-boundaries here))))

(defun tcl-forward-sexp (&optional arg)
  "Move forward across one balanced tcl expression.
With argument, do it that many times."
  (interactive "p")
  (if (< arg 0) (error "negative argument not allowed"))
  (or arg (setq arg 1))
  (while (> arg 0)
    (tcl-forward-sexp1)
    (setq arg (1- arg))))

(defun tcl-forward-sexp1 ()
  (interactive "")  
  (let ((start (point))
	next-char syntax (first-scan t))
    (setq next-char (following-char)
	  syntax (char-syntax next-char))

    (while (or (= next-char ?\;)
	       (memq syntax '(? ?>)))
      (forward-char 1)
      (setq next-char (following-char)
	    syntax (char-syntax next-char)))

    (condition-case var
	(catch 'exit 
	  (while t
	    (setq next-char (following-char)
		  syntax (char-syntax next-char))
	    (cond ((= next-char ?\;)
		   (throw 'exit nil))
		  ((memq syntax (if first-scan '(? ?>) '(? ?> ?\))))
		   (throw 'exit nil))
		  (t 
		   (goto-char (or (scan-sexps (point) 1) 
				  (point-max)))))
	    (setq first-scan nil)))
      (error (goto-char start)
	     (error (car (cdr var)))))))

;; (define-key tcl-mode-map "\M-\C-f" 'tcl-forward-sexp)

(defun mode-motion-eval-func (eval-func)
  (let ((old-buf (current-buffer))
	(old-window (selected-window)))
    (unwind-protect 
	(let ((extent (or primary-selection-extent
			  (and (extentp mode-motion-last-extent)
			       (not (extent-property mode-motion-last-extent
						     'detached))
			       mode-motion-last-extent))))

	  (if (and (extentp extent)
		   (set-buffer (extent-buffer extent))
		   (not 
		    ;; zero length extents
		    (= (extent-start-position extent)
		       (extent-end-position extent))))

	      (let* ((start (extent-start-position extent))
		     (end (extent-end-position extent)))

		(funcall eval-func start end))

	    (error "No current primary or motion selection.")
	    ))
      (set-buffer old-buf)
      (select-window old-window))))

(defun mode-motion-eval-region ()
  (interactive)
  (mode-motion-eval-func 'eval-region))


;; Motion highlight faces and initialization.

(defun sect-handler (string)
  "Return the symbol corresponding to the foo-STRING handler for this sect."
  (intern-soft (concat (symbol-name mode-motion+-religion) string)))

(defun mode-motion-init-handlers-according-to-religion (&optional forcep)
  (interactive)
  ;; Initialise default motion handlers depending on religious sect!
  (let ((foo-thing (sect-handler "-thing"))
	(foo-c (sect-handler "-c"))
	(foo-LaTeX (sect-handler "-laTeX"))
	(foo-line@ (sect-handler "-line@"))
	(foo-vline@ (sect-handler "-vline@")))
    (if forcep
	(progn
	  (setq default-motion-handler (find-motion-handler foo-thing))
	  (set-mode-motion-handler 'emacs-lisp-mode foo-thing)
	  (set-mode-motion-handler 'lisp-interaction-mode foo-thing)
	  (set-mode-motion-handler 'c-mode foo-c)
	  (set-mode-motion-handler 'c++-mode foo-c)
	  (set-mode-motion-handler 'c++-c-mode foo-c)
	  (set-mode-motion-handler 'tex-mode foo-LaTeX)
	  (set-mode-motion-handler 'latex-mode foo-LaTeX)
	  (set-mode-motion-handler 'Buffer-menu-mode foo-vline@)
	  (set-mode-motion-handler 'Electric-Buffer-menu-mode foo-vline@)
	  (set-mode-motion-handler 'gnus-Group-mode foo-vline@)
	  (set-mode-motion-handler 'gnus-Subject-mode foo-vline@)
	  (set-mode-motion-handler 'gnus-group-mode foo-vline@)
	  (set-mode-motion-handler 'gnus-subject-mode foo-vline@)
	  (set-mode-motion-handler 'gnus-summary-mode foo-vline@)
	  (set-mode-motion-handler 'dired-mode foo-line@)
	  (set-mode-motion-handler 'compilation-mode foo-line@)
	  (set-mode-motion-handler 'occur-mode foo-line@)
	  (set-mode-motion-handler 'tar-mode foo-vline@)
	  (set-mode-motion-handler 'rmail-summary-mode foo-vline@)
	  (set-mode-motion-handler 'vm-summary-mode (sect-handler "-line"))
	  (set-mode-motion-handler 'tcl-mode (sect-handler "-tcl"))
	  (set-mode-motion-handler 'texinfo-mode (sect-handler "-TeXinfo"))
	  (set-mode-motion-handler 'cvs-mode (sect-handler "-cvs-line")))
      (setq default-motion-handler
	    (or default-motion-handler (find-motion-handler foo-thing)))
      (or (get 'emacs-lisp-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'emacs-lisp-mode foo-thing))
      (or (get 'lisp-interaction-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'lisp-interaction-mode foo-thing))
      (or (get 'c-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'c-mode foo-c))
      (or (get 'c++-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'c++-mode foo-c))
      (or (get 'c++-c-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'c++-c-mode foo-c))
      (or (get 'tex-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'tex-mode foo-LaTeX))
      (or (get 'latex-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'latex-mode foo-LaTeX))
      (or (get 'Buffer-menu-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'Buffer-menu-mode foo-vline@))
      (or (get 'Electric-Buffer-menu-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'Electric-Buffer-menu-mode foo-vline@))
      (or (get 'gnus-Group-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'gnus-Group-mode foo-vline@))
      (or (get 'gnus-Subject-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'gnus-Subject-mode foo-vline@))
      (or (get 'gnus-group-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'gnus-group-mode foo-vline@))
      (or (get 'gnus-subject-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'gnus-subject-mode foo-vline@))
      (or (get 'gnus-summary-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'gnus-summary-mode foo-vline@))
      (or (get 'dired-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'dired-mode foo-line@))
      (or (get 'compilation-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'compilation-mode foo-line@))
      (or (get 'occur-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'occur-mode foo-line@))
      (or (get 'tar-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'tar-mode foo-vline@))
      (or (get 'rmail-summary-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'rmail-summary-mode foo-vline@))
      (or (get 'vm-summary-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'vm-summary-mode (sect-handler "-line")))
      (or (get 'tcl-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'tcl-mode (sect-handler "-tcl")))
      (or (get 'texinfo-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'texinfo-mode (sect-handler "-TeXinfo")))
      (or (get 'cvs-mode 'mode-motion-handler)
	  (set-mode-motion-handler 'cvs-mode (sect-handler "-cvs-line"))))))

;; Null Handlers (for disabling motion highlighting)
(defun thing-null (here) nil)
(make-motion-handler 'no-thing 'thing-null)
(make-motion-handler 'no-c 'thing-null)
(make-motion-handler 'no-laTeX 'thing-null)
(make-motion-handler 'no-line 'thing-null)
(make-motion-handler 'no-line@ 'thing-null)
(make-motion-handler 'no-vline 'thing-null)
(make-motion-handler 'no-vline@ 'thing-null)
(make-motion-handler 'no-tcl 'thing-null)
(make-motion-handler 'no-TeXinfo 'thing-null)
(make-motion-handler 'no-cvs-line 'thing-null)

(defun mode-motion-init ()
  "enable mode-motion+ package"
  (interactive)

(setq mode-motion-last-extent nil)
  
(global-set-key '(meta button2) 'mode-motion-copy)
(global-set-key '(meta shift button2) 'mode-motion-move)
(global-set-key '(meta control button2) 'mode-motion-kill)
(global-set-key '(meta control shift button2) 'mode-motion-copy-as-kill)
(global-set-key '(meta control symbol button2) 'mode-motion-copy-as-kill)

(if mode-motion-setup-cut-and-paste-bindings
    (progn 
      (global-set-key 'f16 'mode-motion-copy-as-kill) ; Copy
      (global-set-key 'f18 'yank)	              ; Paste
      (global-set-key 'f20 'mode-motion-kill)))       ; Cut

;; I don't want the thing-boundaries function select whitespaces 
(setq thing-report-whitespace nil thing-report-char-p nil)

;; bold motion face (bold, if this is not the default, unbold otherwise)
(if (find-face 'motion-bold)
    ()
  (make-face 'motion-bold)
  (make-face-bold 'motion-bold)
  (or (face-differs-from-default-p 'motion-bold)
      (make-face-unbold 'motion-bold)))

;; an underline face
(if (find-face 'motion-underline)
    ()
  (make-face 'motion-underline)
  (set-face-underline-p 'motion-underline t))

;; an inverted face
(if (find-face 'motion-inverted)
    ()
  (make-face 'motion-inverted)
  (make-face-bold 'motion-inverted)
  (invert-face 'motion-inverted))

(if (find-face 'motion-gray)
    ()
  (make-face 'motion-gray)
  (set-face-background-pixmap 'motion-gray "gray1.xbm"))
 
;; Motion Handlers

;; Special Minibuffer handler

(make-motion-handler 'minibuffer 'minibuffer-selection-boundaries 'highlight t nil)

;; Things
(make-motion-handler 'bold-thing 'thing-boundaries 'motion-bold)
(make-motion-handler 'gray-thing 'thing-boundaries 'motion-gray)
(make-motion-handler 'highlight-thing 'thing-boundaries 'highlight)
(make-motion-handler 'invert-thing 'thing-boundaries 'motion-inverted)
(make-motion-handler 'underline-thing 'thing-boundaries 'motion-underline)

;; Lines
(make-motion-handler 'bold-line 'line-boundaries 'motion-bold)
(make-motion-handler 'gray-line 'line-boundaries 'motion-gray)
(make-motion-handler 'highlight-line 'line-boundaries 'highlight)
(make-motion-handler 'invert-line 'line-boundaries 'motion-inverted)
(make-motion-handler 'underline-line 'line-boundaries 'motion-underline)
(make-motion-handler 'bold-line@ 'line-boundaries 'motion-bold t t)
(make-motion-handler 'gray-line@ 'line-boundaries 'motion-gray nil t)
(make-motion-handler 'highlight-line@ 'line-boundaries 'highlight nil t)
(make-motion-handler 'invert-line@ 'line-boundaries 'motion-inverted nil t)
(make-motion-handler 'underline-line@ 'line-boundaries 'motion-underline nil t)

;; Visible text of line
(make-motion-handler 'bold-vline 'visible-line-boundaries 'motion-bold)
(make-motion-handler 'gray-vline 'visible-line-boundaries 'motion-gray)
(make-motion-handler 'highlight-vline 'visible-line-boundaries 'highlight)
(make-motion-handler 'invert-vline 'visible-line-boundaries 'motion-inverted)
(make-motion-handler 'underline-vline 'visible-line-boundaries 'motion-underline)
(make-motion-handler 'bold-vline@ 'visible-line-boundaries 'motion-bold t t)
(make-motion-handler 'gray-vline@ 'visible-line-boundaries 'motion-gray nil t)
(make-motion-handler 'highlight-vline@ 'visible-line-boundaries 'highlight nil t)
(make-motion-handler 'invert-vline@ 'visible-line-boundaries 'motion-inverted nil t)
(make-motion-handler 'underline-vline@ 'visible-line-boundaries 'motion-underline nil t)

;; CVS lines
(make-motion-handler 'bold-cvs-line 'cvs-line-boundaries 'motion-bold)
(make-motion-handler 'gray-cvs-line 'cvs-line-boundaries 'motion-gray)
(make-motion-handler 'highlight-cvs-line 'cvs-line-boundaries 'highlight)
(make-motion-handler 'invert-cvs-line 'cvs-line-boundaries 'motion-inverted)
(make-motion-handler
 'underline-cvs-line 'cvs-line-boundaries 'motion-underline)

;; (La)TeX 
(make-motion-handler 'bold-LaTeX 'latex-boundaries 'motion-bold)
(make-motion-handler 'gray-LaTeX 'latex-boundaries 'motion-gray)
(make-motion-handler 'highlight-LaTeX 'latex-boundaries 'highlight)
(make-motion-handler 'invert-LaTeX 'latex-boundaries 'motion-inverted)
(make-motion-handler 'underline-LaTeX 'latex-boundaries 'motion-underline)

;; TeXinfo
(make-motion-handler 'bold-TeXinfo 'texinfo-boundaries 'motion-bold)
(make-motion-handler 'gray-TeXinfo 'texinfo-boundaries 'motion-gray)
(make-motion-handler 'highlight-TeXinfo 'texinfo-boundaries 'highlight)
(make-motion-handler 'invert-TeXinfo 'texinfo-boundaries 'motion-inverted)
(make-motion-handler 'underline-TeXinfo 'texinfo-boundaries 'motion-underline)

;; C and C++
(make-motion-handler 'bold-c 'c-boundaries 'motion-bold)
(make-motion-handler 'gray-c 'c-boundaries 'motion-gray)
(make-motion-handler 'highlight-c 'c-boundaries 'highlight)
(make-motion-handler 'invert-c 'c-boundaries 'motion-inverted)
(make-motion-handler 'underline-c 'c-boundaries 'motion-underline)

;; Tcl/Tk
(make-motion-handler 'bold-tcl 'tcl-boundaries 'motion-bold)
(make-motion-handler 'gray-tcl 'tcl-boundaries 'motion-gray)
(make-motion-handler 'highlight-tcl 'tcl-boundaries 'highlight)
(make-motion-handler 'invert-tcl 'tcl-boundaries 'motion-inverted)
(make-motion-handler 'underline-tcl 'tcl-boundaries 'motion-underline)

;; mouse tracker
(make-motion-handler 'track-mouse@ 'char-boundaries nil nil t)
(make-motion-handler 'highlight-char 'char-boundaries 'highlight)

;; augment the basic mouse motion handler (if any)
(setq-default mode-motion-hook 
	      (if (listp mode-motion-hook)
		  (if (memq #'mode-motion+-highlight mode-motion-hook)
		      mode-motion-hook
		    (append mode-motion-hook (list #'mode-motion+-highlight)))
		(list mode-motion-hook #'mode-motion+-highlight)))

(or mode-motion+-religion 
    (setq mode-motion+-religion (if (x-display-color-p) 'underline 'invert)))

(add-menu '("Options") (car mode-motion+-options-menu)
	  (cdr mode-motion+-options-menu)
	  "Paren Highlighting")

;; shut your eyes, this is a kludge. I didn't have time to find/write
;; a function to do this.
(or (member ["Eval Motion Region" mode-motion-eval-region t]
	    lisp-interaction-mode-popup-menu)
    (and (setq lisp-interaction-mode-popup-menu
	       (copy-sequence lisp-interaction-mode-popup-menu))
	 (setcdr (nthcdr 1 lisp-interaction-mode-popup-menu)
		 (cons ["Eval Motion Region" mode-motion-eval-region t]
		       (nthcdr 2 lisp-interaction-mode-popup-menu)))))

(or (member ["Eval Motion Region" mode-motion-eval-region t]
	    emacs-lisp-mode-popup-menu)
    (and (setq emacs-lisp-mode-popup-menu
	       (copy-sequence emacs-lisp-mode-popup-menu))
	 (setcdr (nthcdr 3 emacs-lisp-mode-popup-menu)
		 (cons ["Eval Motion Region" mode-motion-eval-region t]
		       (nthcdr 4 emacs-lisp-mode-popup-menu)))))

;; Clear the last active motion extent when leaving a frame.
(if (boundp 'mouse-leave-frame-hook)
    (add-hook 'mouse-leave-frame-hook 'mode-motion-clear-last-extent)
  (add-hook 'mouse-leave-screen-hook 'mode-motion-clear-last-extent))
	       
(run-hooks 'mode-motion+-load-hook)
(mode-motion-init-handlers-according-to-religion)

(if (interactive-p) (message "mode-motion+ enabled")))

(if (and (not purify-flag)
	 (or (not (boundp 'opt-mode-motion+)) opt-mode-motion+))
    (mode-motion-init))

(provide 'mode-motion+)
;; end mode-motion+
