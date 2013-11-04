;;; iswitchb.el --- switch between buffers using substrings

;; Copyright (C) 1996, 1997  Free Software Foundation, Inc.

;; Author: Stephen Eglen <stephen@cns.ed.ac.uk>
;; Maintainer: Stephen Eglen <stephen@cns.ed.ac.uk>
;; Keywords: extensions
;; location: http://www.cns.ed.ac.uk/people/stephen/emacs/
;; RCS: $Id: iswitchb.el,v 1.3 2001/07/20 14:09:12 youngs Exp $

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

;;; Commentary:

;; Installation:
;; To get the functions in this package bound to keys, do
;; (iswitchb-default-keybindings)

;; As you type in a substring, the list of buffers currently matching
;; the substring are displayed as you type.  The list is ordered so
;; that the most recent buffers visited come at the start of the list.
;; The buffer at the start of the list will be the one visited when
;; you press return.  By typing more of the substring, the list is
;; narrowed down so that gradually the buffer you want will be at the
;; top of the list.  Alternatively, you can use C-s an C-r to rotate
;; buffer names in the list until the one you want is at the top of
;; the list.  Completion is also available so that you can see what is
;; common to all of the matching buffers as you type.

;; This code is similar to a couple of other packages.  Michael R Cook
;; <mcook@cognex.com> wrote a similar buffer switching package, but
;; does exact matching rather than substring matching on buffer names.
;; I also modified a couple of functions from icomplete.el to provide
;; the completion feedback in the minibuffer.

;;; Example 

;;If I have two buffers called "123456" and "123", with "123456" the
;;most recent, when I use iswitchb, I first of all get presented with
;;the list of all the buffers
;;
;;       iswitch  {123456,123} 
;;
;; If I then press 2:
;;       iswitch 2[3]{123456,123}
;;
;; The list in {} are the matching buffers, most recent first (buffers
;; visible in the current frame are put at the end of the list by
;; default).  At any time I can select the item at the head of the
;; list by pressing RET.  I can also bring the put the first element
;; at the end of the list by pressing C-s, or put the last element at
;; the head of the list by pressing C-r.  The item in [] indicates
;; what can be added to my input by pressing TAB.  In this case, I
;; will get "3" added to my input.  So, press TAB: 
;;	 iswitch 23{123456,123}
;;
;; At this point, I still have two matching buffers.
;; If I want the first buffer in the list, I simply press RET.  If I
;; wanted the second in the list, I could press C-s to move it to the
;; top of the list and then RET to select it.
;;
;;However, If I type 4, I only have one match left:
;;       iswitch 234[123456] [Matched]
;;
;;Since there is only one matching buffer left, it is given in [] and we
;;see the text [Matched] afterwards.  I can now press TAB or RET to go
;;to that buffer.
;;
;; If however, I now type "a":
;;       iswitch 234a [No match]
;; There are no matching buffers.  If I press RET or TAB, I can be
;; prompted to create a new buffer called "234a".
;;
;; Of course, where this function comes in really useful is when you
;; can specify the buffer using only a few keystrokes.  In the above
;; example, the quickest way to get to the "123456" buffer would be
;; just to type 4 and then RET (assuming there isn't any newer buffer
;; with 4 in its name).

;; To see a full list of all matching buffers in a separate buffer,
;; hit ? or press TAB when there are no further completions to the
;; substring.  Repeated TAB presses will scroll you through this
;; separate buffer.

;; The buffer at the head of the list can be killed by pressing C-k.
;; If the buffer needs saving, you will be queried before the buffer
;; is killed.

;; If you find that the file you are after is not in a buffer, you can
;; press C-x C-f to immediately drop into find-file.

;;
;;  See the doc string of iswitchb for full keybindings and features.
;;  (describe-function 'iswitchb)

;;; Customisation

;; See the User Variables section below for easy ways to change the
;; functionality of the program.  These are accessible using the
;; custom package.
;; To modify the keybindings, use the hook provided.  For example:
;;(add-hook 'iswitchb-define-mode-map-hook
;;	  'iswitchb-my-keys)
;;
;;(defun iswitchb-my-keys ()
;;  "Add my keybindings for iswitchb."
;;  (define-key iswitchb-mode-map " " 'iswitchb-next-match)
;;  )
;;
;; Seeing all the matching buffers
;;
;; If you have many matching buffers, they may not all fit onto one
;; line of the minibuffer.  In this case, you should use rsz-mini
;; (resize-minibuffer-mode).  You can also limit iswitchb so that it
;; only shows a certain number of lines -- see the documentation for
;; `iswitchb-minibuffer-setup-hook'.


;; Changing the list of buffers

;; By default, the list of current buffers is most recent first,
;; oldest last, with the exception that the buffers visible in the
;; current frame are put at the end of the list.  A hook exists to
;; allow other functions to order the list.  For example, if you add:
;;
;; (add-hook 'iswitchb-make-buflist-hook 'iswitchb-summaries-to-end)
;;
;; then all buffers matching "Summary" are moved to the end of the
;; list.  (I find this handy for keeping the INBOX Summary and so on
;; out of the way.)  It also moves buffers matching "output\*$" to the
;; end of the list (these are created by AUC TeX when compiling.)
;; Other functions could be made available which alter the list of
;; matching buffers (either deleting or rearranging elements.)

;; Font-Lock

;; If you have font-lock loaded, the first matching buffer is
;; highlighted.  To switch this off, set (setq iswitchb-use-fonts nil)
;; I don't use font-lock that much, so I've hardcoded the faces.  If
;; this is too harsh, let me know.  Colouring of the matching buffer
;; name was suggested by Carsten Dominik (dominik@strw.leidenuniv.nl)


;; Replacement for read-buffer

;; iswitchb-read-buffer has been written to be a drop in replacement
;; for the normal buffer selection routine `read-buffer'.  To use
;; iswitch for all buffer selections in Emacs, add:
;; (setq read-buffer-function 'iswitchb-read-buffer)
;; (This variable should be present in Emacs 20.3+)
;; XEmacs users can get the same behaviour by doing:
;; (defalias 'read-buffer 'iswitchb-read-buffer) 
;; since `read-buffer' is defined in lisp.

;; Regexp matching

;; There is limited provision for regexp matching within iswitchb,
;; enabled through `iswitchb-regexp'.  This allows you to type `c$'
;; for example and see all buffer names ending in `c'.  This facility
;; is quite limited though in two respects.  First, you can't
;; currently type in expressions like `[0-9]' directly -- you have to
;; type them in when iswitchb-regexp is nil and then toggle on the
;; regexp functionality.  Likewise, don't enter an expression
;; containing `\' in regexp mode.  If you try, iswitchb gets confused,
;; so just hit C-g and try again.  Secondly, no completion mechanism
;; is currently offered when regexp searching.

;;; TODO

;;; Acknowledgements

;; Thanks to Jari Aalto <jari.aalto@poboxes.com> for help with the
;; first version of this package, iswitch-buffer.  Thanks also to many
;; others for testing earlier versions.

;;; Code:


;; CL needed for cadr and last
(require 'cl) 

;; Set up the custom library.
;; taken from http://www.dina.kvl.dk/~abraham/custom/
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args) 
      (` (defvar (, var) (, value) (, doc))))))

;;; User Variables
;;
;; These are some things you might want to change.

(defgroup iswitchb nil
  "Switch between buffers using substrings."
  :group 'extensions
  ;; These links are to be added in later versions of custom and
  ;; so are currently commented out.
  :link '(emacs-commentary-link :tag "Commentary" "iswitchb.el")
  :link '(emacs-library-link :tag "Lisp File" "iswitchb.el")
)


(defcustom iswitchb-case case-fold-search
  "*Non-nil if searching of buffer names should ignore case."
  :type 'boolean
  :group 'iswitchb)

(defcustom iswitchb-buffer-ignore
  '("^ ")
  "*List of regexps or functions matching buffer names to ignore.
For example, traditional behavior is not to list buffers whose names begin
with a space, for which the regexp is `^ '.  See the source file for
example functions that filter buffernames."
  :type '(repeat regexp)
  :group 'iswitchb)


;;; Examples for setting the value of iswitchb-buffer-ignore
;(defun iswitchb-ignore-c-mode (name)
;  "Ignore all c mode buffers -- example function for iswitchb."
;  (save-excursion
;    (set-buffer name)
;    (string-match "^C$" mode-name)))

;(setq iswitchb-buffer-ignore '("^ " iswitchb-ignore-c-mode))
;(setq iswitchb-buffer-ignore '("^ " "\\.c$" "\\.h$"))

(defcustom iswitchb-default-method  'always-frame
    "*How to switch to new buffer when using `iswitchb-buffer'.
Possible values:
`samewindow'	Show new buffer in same window
`otherwindow'	Show new buffer in another window (same frame)
`display'	Display buffer in another window without switching to it
`otherframe'	Show new buffer in another frame
`maybe-frame'	If a buffer is visible in another frame, prompt to ask if you
		you want to see the buffer in the same window of the current
  		frame or in the other frame.
`always-frame'  If a buffer is visible in another frame, raise that
		frame.  Otherwise, visit the buffer in the same window."
    :type '(choice (const samewindow) 
		   (const otherwindow)
		   (const display)
		   (const otherframe) 
		   (const maybe-frame)
		   (const always-frame))
    :group 'iswitchb)


(defcustom iswitchb-regexp nil
  "*Non-nil means that `iswitchb' will do regexp matching.
Value can be toggled within `iswitchb' using `iswitchb-toggle-regexp'."
  :type 'boolean
  :group 'iswitchb)


(defcustom iswitchb-newbuffer t
  "*Non-nil means create new buffer if no buffer matches substring.
See also `iswitchb-prompt-newbuffer'."
  :type 'boolean
  :group 'iswitchb)


(defcustom iswitchb-prompt-newbuffer t
  "*Non-nil means prompt user to confirm before creating new buffer.
See also `iswitchb-newbuffer'."
  :type 'boolean
  :group 'iswitchb)


(defcustom iswitchb-define-mode-map-hook  nil
  "*Hook to define keys in `iswitchb-mode-map' for extra keybindings."
  :type 'hook
  :group 'iswitchb)



(defcustom iswitchb-use-fonts t
  "*Non-nil means use font-lock fonts for showing first match."
  :type 'boolean
  :group 'iswitchb)


(defcustom iswitchb-make-buflist-hook  nil
  "*Hook to run when list of matching buffers is created."
  :type 'hook
  :group 'iswitchb)

(defvar iswitchb-all-frames 'visible
  "*Argument to pass to `walk-windows' when finding visible buffers.
See documentation of `walk-windows' for useful values.")

(defcustom iswitchb-minibuffer-setup-hook nil
  "*Iswitchb-specific customization of minibuffer setup.

This hook is run during minibuffer setup iff `iswitchb' will be active.
It is intended for use in customizing iswitchb for interoperation
with other packages.  For instance:

  \(add-hook 'iswitchb-minibuffer-setup-hook 
	    \(function
	     \(lambda ()
	       \(make-local-variable 'resize-minibuffer-window-max-height)
	       \(setq resize-minibuffer-window-max-height 3))))

will constrain rsz-mini to a maximum minibuffer height of 3 lines when
iswitchb is running.  Copied from `icomplete-minibuffer-setup-hook'."
  :type 'hook
  :group 'iswitchb)

;; Do we need the variable iswitchb-use-mycompletion?


;;; Internal Variables

(defvar iswitchb-method nil
  "Stores the method for viewing the selected buffer.  
Its value is one of `samewindow', `otherwindow', `display', `otherframe',
`maybe-frame' or `always-frame'.  See `iswitchb-default-method' for
details of values.")

(defvar iswitchb-eoinput 1
  "Point where minibuffer input ends and completion info begins.
Copied from `icomplete-eoinput'.")
(make-variable-buffer-local 'iswitchb-eoinput)


(defvar iswitchb-buflist nil
  "Stores the current list of buffers that will be searched through.
The list is ordered, so that the most recent buffers come first,
although by default, the buffers visible in the current frame are put
at the end of the list.  Created by `iswitchb-make-buflist'.")

;; todo -- is this necessary?

(defvar iswitchb-use-mycompletion nil
  "Non-nil means use `iswitchb-buffer' completion feedback.  
Should only be set to t by iswitchb functions, so that it doesn't
interfere with other minibuffer usage.")

(defvar iswitchb-change-word-sub nil 
  "Private variable used by `iswitchb-word-matching-substring'.")

(defvar iswitchb-common-match-string  nil
  "Stores the string that is common to all matching buffers.")

(defvar iswitchb-rescan nil
  "Non-nil means we need to regenerate the list of matching buffers.")

(defvar iswitchb-text nil
  "Stores the users string as it is typed in.")

(defvar iswitchb-matches nil
  "List of buffers currently matching `iswitchb-text'.")

(defvar iswitchb-mode-map nil
  "Keymap for `iswitchb-buffer'.")

(defvar  iswitchb-history nil
  "History of buffers selected using `iswitchb-buffer'.")

(defvar iswitchb-exit nil 
  "Flag to monitor how `iswitchb-buffer' exits.  
If equal to `takeprompt', we use the prompt as the buffer name to be
selected.")

(defvar iswitchb-buffer-ignore-orig nil
  "Stores original value of `iswitchb-buffer-ignore'.")

(defvar iswitchb-xemacs  (string-match "XEmacs" (emacs-version))
  "Non-nil if we are running XEmacs.  Otherwise, assume we are running Emacs.")

(defvar iswitchb-default nil
  "Default buffer for iswitchb.")

;;; FUNCTIONS


;;; ISWITCHB KEYMAP 
(defun iswitchb-define-mode-map ()
  "Set up the keymap for `iswitchb-buffer'."
  (interactive)
  (let (map)
    ;; generated every time so that it can inherit new functions.
    ;;(or iswitchb-mode-map

    (setq map (copy-keymap minibuffer-local-map))
    (define-key map "?" 'iswitchb-completion-help)
    (define-key map "\C-s" 'iswitchb-next-match)
    (define-key map "\C-r" 'iswitchb-prev-match)
    (define-key map "\t" 'iswitchb-complete)
    (define-key map "\C-j" 'iswitchb-select-buffer-text)
    (define-key map "\C-t" 'iswitchb-toggle-regexp)
    (define-key map "\C-x\C-f" 'iswitchb-find-file)
    ;;(define-key map "\C-a" 'iswitchb-toggle-ignore)
    (define-key map "\C-c" 'iswitchb-toggle-case)
    (define-key map "\C-k" 'iswitchb-kill-buffer)
    (define-key map "\C-m" 'iswitchb-exit-minibuffer)
    (setq iswitchb-mode-map map)
    (run-hooks 'iswitchb-define-mode-map-hook)
    ))
  


;;; MAIN FUNCTION
(defun iswitchb ()
  "Switch to buffer matching a substring.
As you type in a string, all of the buffers matching the string are
displayed.  When you have found the buffer you want, it can then be
selected.  As you type, most keys have their normal keybindings,
except for the following:
\\<iswitchb-mode-map>

RET Select the buffer at the front of the list of matches.  If the
list is empty, possibly prompt to create new buffer.

\\[iswitchb-select-buffer-text] Select the current prompt as the buffer.
If no buffer is found, prompt for a new one.

\\[iswitchb-next-match] Put the first element at the end of the list.
\\[iswitchb-prev-match] Put the last element at the start of the list.
\\[iswitchb-complete] Complete a common suffix to the current string that 
matches all buffers.  If there is only one match, select that buffer.
If there is no common suffix, show a list of all matching buffers
in a separate window.
\\[iswitchb-toggle-regexp] Toggle regexp searching.
\\[iswitchb-toggle-case] Toggle case-sensitive searching of buffer names.
\\[iswitchb-completion-help] Show list of matching buffers in separate window.
\\[iswitchb-find-file] Exit iswitchb and drop into find-file.
\\[iswitchb-kill-buffer] Kill buffer at head of buffer list."
  ;;\\[iswitchb-toggle-ignore] Toggle ignoring certain buffers (see \
  ;;`iswitchb-buffer-ignore')
  	
  (let
      (prompt buf)
    
    (setq prompt (format "iswitch "))

    (setq buf (iswitchb-read-buffer prompt))


    ;;(message "chosen text %s" iswitchb-final-text)
    ;; Choose the buffer name: either the text typed in, or the head
    ;; of the list of matches

    (cond ( (eq iswitchb-exit 'findfile)
	    (call-interactively 'find-file))

	  (t
	   ;; View the buffer
	   ;;(message "go to buf %s" buf)
	   ;; Check buf is non-nil.
	   (if buf
	       (if (get-buffer buf)
		   ;; buffer exists, so view it and then exit
		   (iswitchb-visit-buffer buf)
		 ;; else buffer doesn't exist
		 (iswitchb-possible-new-buffer buf)))
	   ))
    
    ))



(defun iswitchb-read-buffer (prompt &optional default require-match)
  "Replacement for the built-in `read-buffer'.
Return the name of a buffer selected.  
PROMPT is the prompt to give to the user.  DEFAULT if given is the default
buffer to be selected, which will go to the front of the list.
If REQUIRE-MATCH is non-nil, an existing-buffer must be selected."
  (let
      (
       buf-sel
       iswitchb-final-text
       (icomplete-mode nil) ;; prevent icomplete starting up
       ;; can only use fonts if they have been bound.
       (iswitchb-use-fonts (and iswitchb-use-fonts
				(boundp 'font-lock-comment-face)
				(boundp 'font-lock-function-name-face))))

    (iswitchb-define-mode-map)
    (setq iswitchb-exit nil)
    (setq iswitchb-rescan t)
    (setq iswitchb-text "")
    (setq iswitchb-default
	  (if (bufferp default)
	      (buffer-name default)
	    default))
    (iswitchb-make-buflist iswitchb-default)
    (iswitchb-set-matches)
    (let 
	((minibuffer-local-completion-map iswitchb-mode-map)
	 (iswitchb-prepost-hooks t)
	 (iswitchb-require-match require-match)	
	 )
      ;; prompt the user for the buffer name
      (setq iswitchb-final-text (completing-read 
				 prompt	;the prompt
				 '(("dummy".1))	;table
				 nil	;predicate
				 nil	;require-match [handled elsewhere]
				 nil	;initial-contents
				 'iswitchb-history)))
    ;; Handling the require-match must be done in a better way.
    (if (and require-match (not (iswitchb-existing-buffer-p)))
	(error "must specify valid buffer"))

    (if (or 
	 (eq iswitchb-exit 'takeprompt)
	 (null iswitchb-matches))
	(setq buf-sel iswitchb-final-text)
      ;; else take head of list
      (setq buf-sel (car iswitchb-matches)))
    
    ;; Or possibly choose the default buffer
    (if  (equal iswitchb-final-text "")	
	(setq buf-sel 
	      (car iswitchb-matches)))

    buf-sel))


(defun iswitchb-existing-buffer-p ()
  "Return non-nil if there is a matching buffer."
  (not (null iswitchb-matches)))

;;; COMPLETION CODE

(defun iswitchb-set-common-completion  ()
  "Find common completion of `iswitchb-text' in `iswitchb-matches'.
The result is stored in `iswitchb-common-match-string'."

  (let* (val)
    (setq  iswitchb-common-match-string nil)
    (if (and iswitchb-matches
	     (not iswitchb-regexp) ;; testing
             (stringp iswitchb-text)
             (> (length iswitchb-text) 0))
        (if (setq val (iswitchb-find-common-substring
                       iswitchb-matches iswitchb-text))
            (setq iswitchb-common-match-string val)))
    val
    ))


(defun iswitchb-complete ()
  "Try and complete the current pattern amongst the buffer names."
  (interactive)
  (let (res)
    (cond ((not  iswitchb-matches)
	   (setq this-command 'iswitchb-completion-help)
	   (iswitchb-completion-help)
	   )
	  
	  ((= 1 (length iswitchb-matches))
	   ;; only one choice, so select it.
	   (exit-minibuffer))
	  
	  (t
	   ;; else there could be some completions
	   (setq res iswitchb-common-match-string)
	   (if (and (not (memq res '(t nil)))
		    (not (equal res iswitchb-text)))
	       ;; found something to complete, so put it in the minibuffer.
	       (progn
		 (setq iswitchb-rescan nil)
		 (delete-region (point-min) (point))
		 (insert  res))
	     ;; else nothing to complete
	     (setq this-command 'iswitchb-completion-help)
	     (iswitchb-completion-help)
	     )
	   )
	  )))



;;; TOGGLE FUNCTIONS

(defun iswitchb-toggle-case ()
  "Toggle the value of `iswitchb-case'."
  (interactive)
  (setq iswitchb-case (not iswitchb-case))
  ;; ask for list to be regenerated.
  (setq iswitchb-rescan t)
  )

(defun iswitchb-toggle-regexp ()
  "Toggle the value of `iswitchb-regexp'."
  (interactive)
  (setq iswitchb-regexp (not iswitchb-regexp))
  ;; ask for list to be regenerated.
  (setq iswitchb-rescan t)
  )


(defun iswitchb-toggle-ignore ()
  "Toggle ignoring buffers specified with `iswitchb-buffer-ignore'."
  (interactive)
  (if iswitchb-buffer-ignore
      (progn
        (setq iswitchb-buffer-ignore-orig iswitchb-buffer-ignore)
        (setq iswitchb-buffer-ignore nil)
        )
    ;; else
    (setq iswitchb-buffer-ignore iswitchb-buffer-ignore-orig)
    )
  (iswitchb-make-buflist iswitchb-default)
  ;; ask for list to be regenerated.
  (setq iswitchb-rescan t)
  )

(defun iswitchb-exit-minibuffer ()
  "Exit minibuffer, but make sure we have a match if one is needed."
  (interactive)
  (if (or (not iswitchb-require-match)
	   (iswitchb-existing-buffer-p))
      (throw 'exit nil)
    ))

(defun iswitchb-select-buffer-text ()
  "Select the buffer named by the prompt.
If no buffer exactly matching the prompt exists, maybe create a new one."
  (interactive)
  (setq iswitchb-exit 'takeprompt)
  (exit-minibuffer))



(defun iswitchb-find-file ()
  "Drop into find-file from buffer switching."
  (interactive)
  (setq iswitchb-exit 'findfile)
  (exit-minibuffer))

(defun iswitchb-next-match () 
  "Put first element of `iswitchb-matches' at the end of the list."
  (interactive)
  (let ((next  (cadr iswitchb-matches)))
    (setq iswitchb-buflist (iswitchb-chop iswitchb-buflist next))
    (setq iswitchb-rescan t)
    ))

(defun iswitchb-prev-match () 
  "Put last element of `iswitchb-matches' at the front of the list."
  (interactive)
  (let ((prev  (car (last iswitchb-matches))))
    (setq iswitchb-buflist (iswitchb-chop iswitchb-buflist prev))
    (setq iswitchb-rescan t)
    ))




(defun iswitchb-chop (list elem)
  "Remove all elements before ELEM and put them at the end of LIST."
  (let ((ret nil)
	(next nil)
	(sofar nil))
    (while (not ret)
      (setq next (car list))
      (if (equal next elem)
	  (setq ret (append list (nreverse sofar)))
	;; else
	(progn
	  (setq list (cdr list))
	  (setq sofar (cons next sofar)))))
    ret))




;;; CREATE LIST OF ALL CURRENT BUFFERS


(defun iswitchb-make-buflist (default)
  "Set `iswitchb-buflist' to the current list of buffers.
Currently visible buffers are put at the end of the list.
The hook `iswitchb-make-buflist-hook' is run after the list has been 
created to allow the user to further modify the order of the buffer names
in this list.  If DEFAULT is non-nil, and corresponds to an existing buffer,
it is put to the start of the list."
  (setq iswitchb-buflist 
	(let* ((iswitchb-current-buffers (iswitchb-get-buffers-in-frames))
	      (buflist 
	       (delq nil 
		     (mapcar
		      (lambda (x)
			(let ((b-name (buffer-name x)))
			  (if (not 
			       (or 
				(iswitchb-ignore-buffername-p b-name)
				(memq b-name iswitchb-current-buffers)))
			      b-name)))
		      (buffer-list)))))
	  (nconc buflist iswitchb-current-buffers)
	  (run-hooks 'iswitchb-make-buflist-hook)
	  ;; Should this be after the hooks, or should the hooks be the
	  ;; final thing to be run?
	  (if default
	      (progn
		(setq buflist (delete default buflist))
		(setq buflist (cons default buflist))
		))
	    buflist)))

(defun iswitchb-to-end (lst)
  "Move the elements from LST to the end of BUFLIST."
  (mapcar 
   (lambda (elem)  
     (setq buflist (delq elem buflist)))
   lst)
  (nconc buflist lst))



(defun iswitchb-get-buffers-in-frames (&optional current)
  "Return the list of buffers that are visible in the current frame.
If optional argument `current' is given, restrict searching to the
current frame, rather than all frames, regardless of value of
`iswitchb-all-frames'."
  (let ((iswitchb-bufs-in-frame nil))
    (walk-windows 'iswitchb-get-bufname nil
		  (if current 
		      nil
		    iswitchb-all-frames))
    iswitchb-bufs-in-frame))


(defun iswitchb-get-bufname (win)
  "Used by `iswitchb-get-buffers-in-frames' to walk through all windows."
  (let ((buf (buffer-name (window-buffer win))))
	(if (not (member buf iswitchb-bufs-in-frame))
	    ;; Only add buf if it is not already in list.
	    ;; This prevents same buf in two different windows being
	    ;; put into the list twice.
	    (setq iswitchb-bufs-in-frame
		  (cons buf iswitchb-bufs-in-frame)))))


;;; FIND MATCHING BUFFERS


(defun iswitchb-set-matches ()
  "Set `iswitchb-matches' to the list of buffers matching prompt."
  (if iswitchb-rescan
      (setq iswitchb-matches
	    (let* ((buflist iswitchb-buflist)
		   )
	      (iswitchb-get-matched-buffers iswitchb-text iswitchb-regexp
					    buflist)))))

(defun iswitchb-get-matched-buffers (regexp
				     &optional string-format buffer-list)
  "Return buffers matching REGEXP.
If STRING-FORMAT is nil, consider REGEXP as just a string.
BUFFER-LIST can be list of buffers or list of strings."
  (let* ((case-fold-search  iswitchb-case)
	 ;; need reverse since we are building up list backwards
	 (list              (reverse buffer-list))
         (do-string         (stringp (car list)))
         name
         ret
         )
    (mapcar
     (lambda (x)
       
       (if do-string
	   (setq name x)               ;We already have the name
	 (setq name (buffer-name x)))
       
       (cond
	((and (or (and string-format (string-match regexp name))
		  (and (null string-format)
		       (string-match (regexp-quote regexp) name)))
	      
	      (not (iswitchb-ignore-buffername-p name))
	      )
	 (setq ret (cons name ret))
          )))
     list)
    ret
    ))




(defun iswitchb-ignore-buffername-p (bufname)
  "Return t if the buffer BUFNAME should be ignored."
  (let ((data       (match-data))
        (re-list    iswitchb-buffer-ignore)
        ignorep
        nextstr
        )
    (while re-list
      (setq nextstr (car re-list))
      (cond
       ((stringp nextstr)
        (if (string-match nextstr bufname)
            (progn
              (setq ignorep t)
              (setq re-list nil))))
       ((fboundp nextstr)
        (if (funcall nextstr bufname)
            (progn
              (setq ignorep t)
              (setq re-list nil))
          ))
       )
      (setq re-list (cdr re-list)))
    (set-match-data data)

    ;; return the result
    ignorep)
  )

(defun iswitchb-word-matching-substring (word)
  "Return part of WORD before 1st match to `iswitchb-change-word-sub'.
If `iswitchb-change-word-sub' cannot be found in WORD, return nil."
  (let ((case-fold-search iswitchb-case)) 
    (let ((m (string-match iswitchb-change-word-sub word)))
      (if m
          (substring word m)
        ;; else no match
        nil))))

(defun iswitchb-find-common-substring (lis subs)
  "Return common string following SUBS in each element of LIS."
  (let (res
        alist
        iswitchb-change-word-sub
        )
    (setq iswitchb-change-word-sub
          (if iswitchb-regexp
              subs
            (regexp-quote subs)))
    (setq res (mapcar 'iswitchb-word-matching-substring lis))
    (setq res (delq nil res)) ;; remove any nil elements (shouldn't happen)
    (setq alist (mapcar 'iswitchb-makealist res)) ;; could use an  OBARRAY

    ;; try-completion returns t if there is an exact match.
    (let ((completion-ignore-case iswitchb-case))

    (try-completion subs alist)   
    )))


(defun iswitchb-makealist (res)
  "Return dotted pair (RES . 1)."
  (cons res 1))

;; from Wayne Mesard <wmesard@esd.sgi.com>
(defun iswitchb-rotate-list (lis)
  "Destructively removes the last element from LIS.
Return the modified list with the last element prepended to it."
  (if (<= (length lis) 1)
      lis
    (let ((las lis)
          (prev lis))
      (while (consp (cdr las))
        (setq prev las
              las (cdr las)))
      (setcdr prev nil)
      (cons (car las) lis))
    ))


(defun iswitchb-completion-help ()
  "Show possible completions in a *Buffer Completions* buffer."
  ;; we could allow this buffer to be used to select match, but I think
  ;; choose-completion-string will need redefining, so it just inserts
  ;; choice with out any previous input.  
  (interactive)
  (setq iswitchb-rescan nil)
  (let ((completion-setup-hook nil)	;disable fancy highlight/selection.
	(buf (current-buffer))
	(temp-buf "*Buffer Completions*")
	(win)
	(again (eq last-command this-command)))

    (if again
	;; scroll buffer
	(progn
	  (set-buffer temp-buf)
	  (setq win (get-buffer-window temp-buf))
	  (if (pos-visible-in-window-p (point-max) win)
	      (set-window-start win (point-min))
	    (scroll-other-window))
	  (set-buffer buf)
	  )

      
      (with-output-to-temp-buffer temp-buf
	(if iswitchb-xemacs 
	    
	    ;; XEmacs extents are put on by default, doesn't seem to be
	    ;; any way of switching them off.
	    (display-completion-list (if iswitchb-matches
					 iswitchb-matches
				       iswitchb-buflist)
				     :help-string "iswitchb "
				   :activate-callback 
				   '(lambda (x y z) 
				      (message "doesn't work yet, sorry!")))
	  ;; else running Emacs
	  (display-completion-list (if iswitchb-matches
				     iswitchb-matches
				     iswitchb-buflist))
	  )))))



;;; KILL CURRENT BUFFER

(defun iswitchb-kill-buffer ()
  "Kill the buffer at the head of `iswitchb-matches'."
  (interactive)
  (let ( (enable-recursive-minibuffers t)
	 buf)

    (setq buf (car iswitchb-matches))
    ;; check to see if buf is non-nil.
    (if buf
	(progn
	  (kill-buffer buf)

	  ;; Check if buffer exists.  XEmacs gnuserv.el makes alias
	  ;; for kill-buffer which does not return t if buffer is
	  ;; killed, so we can't rely on kill-buffer return value.
	  (if (get-buffer buf)
	      ;; buffer couldn't be killed.
	      (setq iswitchb-rescan t)	
	    ;; else buffer was killed so remove name from list.
	    (setq iswitchb-buflist  (delq buf iswitchb-buflist)))))))


;;; VISIT CHOSEN BUFFER
(defun iswitchb-visit-buffer (buffer)
  "Visit buffer named BUFFER according to `iswitchb-method'."
  (let* (win  newframe)
    (cond
     ((eq iswitchb-method 'samewindow)
      (switch-to-buffer buffer))

     ((memq iswitchb-method '(always-frame maybe-frame))
      (cond
       ((and (setq win (iswitchb-window-buffer-p buffer))
	     (or (eq iswitchb-method 'always-frame)
		 (y-or-n-p "Jump to frame? ")))
	(setq newframe (window-frame win))
	(raise-frame newframe)
	(select-frame newframe)
	(select-window win)
	(if (not iswitchb-xemacs)
	    ;; reposition mouse to make frame active.  not needed in XEmacs
	    ;; This line came from the other-frame defun in Emacs.
	    (set-mouse-position (selected-frame) (1- (frame-width)) 0))
	)
       (t
	;;  No buffer in other frames...
	(switch-to-buffer buffer)
	)))



     ((eq iswitchb-method 'otherwindow)
      (switch-to-buffer-other-window buffer))

     ((eq iswitchb-method 'display)
      (display-buffer buffer))

     ((eq iswitchb-method 'otherframe)
      (progn
	(switch-to-buffer-other-frame buffer)
	(if (not iswitchb-xemacs)
	    (set-mouse-position (selected-frame) (1- (frame-width)) 0))
	)
      ) )))

(defun iswitchb-possible-new-buffer (buf)
  "Possibly create and visit a new buffer called BUF."

  (let ((newbufcreated))
    (if (and iswitchb-newbuffer
	     (or
	      (not iswitchb-prompt-newbuffer)
	      
	      (and iswitchb-prompt-newbuffer
		   (y-or-n-p
		    (format
		     "No buffer matching `%s', create one? "
		     buf)))))
	;; then create a new buffer
	(progn
	  (setq newbufcreated (get-buffer-create buf))
	  (if (fboundp 'set-buffer-major-mode)
	      (set-buffer-major-mode newbufcreated))
	  (iswitchb-visit-buffer newbufcreated))
      ;; else wont create new buffer
      (message (format "no buffer matching `%s'" buf))
      )))

(defun iswitchb-window-buffer-p  (buffer)
  "Return window pointer if BUFFER is visible in another frame.
If BUFFER is visible in the current frame, return nil."
  (interactive)
  (let ((blist (iswitchb-get-buffers-in-frames 'current)))
    ;;If the buffer is visible in current frame, return nil
    (if (memq buffer blist)
	nil
      ;;  maybe in other frame or icon
      (get-buffer-window buffer 0) ; better than 'visible
      )))

;;;###autoload
(defun iswitchb-default-keybindings ()
  "Set up default keybindings for `iswitchb-buffer'.
Call this function to override the normal bindings."
  (interactive)
  (global-set-key (read-kbd-macro "C-x b")  'iswitchb-buffer)
  (global-set-key (read-kbd-macro "C-x 4 b")  'iswitchb-buffer-other-window)
  (global-set-key (read-kbd-macro "C-x 4 C-o")  'iswitchb-display-buffer)
  (global-set-key (read-kbd-macro "C-x 5 b")  'iswitchb-buffer-other-frame))


;;;###autoload
(defun iswitchb-buffer ()
  "Switch to another buffer.

The buffer name is selected interactively by typing a substring.  The
buffer is displayed according to `iswitchb-default-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.
For details of keybindings, do `\\[describe-function] iswitchb'."
  (interactive)
  (setq iswitchb-method iswitchb-default-method)
  (iswitchb))


;;;###autoload
(defun iswitchb-buffer-other-window ()
  "Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'."
  (interactive)
  (setq iswitchb-method 'otherwindow)
  (iswitchb))



;;;###autoload
(defun iswitchb-display-buffer ()
  "Display a buffer in another window but don't select it.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'."
  (interactive)
  (setq iswitchb-method 'display)
  (iswitchb))



;;;###autoload
(defun iswitchb-buffer-other-frame ()
  "Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'."
  (interactive)
  (setq iswitchb-method 'otherframe)
  (iswitchb))

;;; XEmacs hack for showing default buffer

;; The first time we enter the minibuffer, Emacs puts up the default
;; buffer to switch to, but XEmacs doesn't -- presumably there is a
;; subtle difference in the two versions of post-command-hook.  The
;; default is shown for both whenever we delete all of our text
;; though, indicating its just a problem the first time we enter the
;; function.  To solve this, we use another entry hook for emacs to
;; show the default the first time we enter the minibuffer.

(defun iswitchb-init-XEmacs-trick ()
  "Display default buffer when first entering minibuffer.
This is a hack for XEmacs, and should really be handled by `iswitchb-exhibit'."
  (if (iswitchb-entryfn-p)
      (progn
	(iswitchb-exhibit)
	(goto-char (point-min)))))


;; add this hook for XEmacs only.
(if iswitchb-xemacs
    (add-hook 'iswitchb-minibuffer-setup-hook 
	      'iswitchb-init-XEmacs-trick))


;;; XEmacs / backspace key
;; For some reason, if the backspace key is pressed in XEmacs, the
;; line gets confused, so I've added a simple key definition to make
;; backspace act like the normal delete key.  

(defun iswitchb-xemacs-backspacekey ()
  "Bind backspace to `backward-delete-char'."
  (define-key iswitchb-mode-map '[backspace] 'backward-delete-char)
  (define-key iswitchb-mode-map '[(meta backspace)] 'backward-kill-word)
  )


(if iswitchb-xemacs
    (add-hook 'iswitchb-define-mode-map-hook 
	      'iswitchb-xemacs-backspacekey))



;;; ICOMPLETE TYPE CODE

(defun iswitchb-exhibit ()
  "Find matching buffers and display a list in the minibuffer.
Copied from `icomplete-exhibit' with two changes:
1. It prints a default buffer name when there is no text yet entered.
2. It calls my completion routine rather than the standard completion."

  (if iswitchb-use-mycompletion
      (let ((contents (buffer-substring (point-min)(point-max)))
	    (buffer-undo-list t))
	(save-excursion
	  (goto-char (point-max))
                                        ; Register the end of input, so we
                                        ; know where the extra stuff
                                        ; (match-status info) begins:
	  (if (not (boundp 'iswitchb-eoinput))
	      ;; In case it got wiped out by major mode business:
	      (make-local-variable 'iswitchb-eoinput))
	  (setq iswitchb-eoinput (point))
	  ;; Update the list of matches
	  (setq iswitchb-text contents)
	  (iswitchb-set-matches)
	  (setq iswitchb-rescan t)
	  (iswitchb-set-common-completion)

	  ;; Insert the match-status information:
	  (insert-string
	   (iswitchb-completions 
	    contents
	    minibuffer-completion-table
	    minibuffer-completion-predicate
	    (not minibuffer-completion-confirm)))
	  ))))



(defun iswitchb-completions
  (name candidates predicate require-match)
  "Return the string that is displayed after the user's text.
Modified from `icomplete-completions'."
  
  (let ((comps iswitchb-matches)
                                        ; "-determined" - only one candidate
        (open-bracket-determined (if require-match "(" "["))
        (close-bracket-determined (if require-match ")" "]"))
                                        ;"-prospects" - more than one candidate
        (open-bracket-prospects "{")
        (close-bracket-prospects "}")
	first
        )

    (if (and iswitchb-use-fonts  comps)
	(progn
	  (setq first (car comps))
	  (setq first (format "%s" first))
	  (put-text-property 0 (length first) 'face
			     (if (= (length comps) 1) 
				 'font-lock-comment-face
			       'font-lock-function-name-face)
			     first) 
	  (setq comps  (cons first (cdr comps)))
	  ))

    (cond ((null comps) (format " %sNo match%s"
				open-bracket-determined
				close-bracket-determined))

	  ((null (cdr comps))		;one match
	   (concat (if (and (> (length (car comps))
			       (length name)))
		       (concat open-bracket-determined
			       ;; when there is one match, show the 
			       ;; matching buffer name in full
			       (car comps)
			       close-bracket-determined)
		     "")
		   (if (not iswitchb-use-fonts) " [Matched]")
		   ))
	  (t				;multiple matches
	   (let* (
		  ;;(most (try-completion name candidates predicate))
		  (most nil)
		  (most-len (length most))
		  most-is-exact
		  first
		  (alternatives
		   (apply
		    (function concat)
		    (cdr (apply
			  (function nconc)
			  (mapcar '(lambda (com)
				     (if (= (length com) most-len)
					 ;; Most is one exact match,
					 ;; note that and leave out
					 ;; for later indication:
					 (progn
					   (setq most-is-exact t)
					   ())
				       (list ","
					     (substring com
							most-len))))
				  comps))))))

	     (concat

	      ;; put in common completion item -- what you get by
	      ;; pressing tab
	      (if (> (length iswitchb-common-match-string) (length name))
		  (concat open-bracket-determined
			  (substring iswitchb-common-match-string 
				     (length name))
			  close-bracket-determined)
		)
	      ;; end of partial matches...

	      ;; think this bit can be ignored.
	      (and (> most-len (length name))
		   (concat open-bracket-determined
			   (substring most (length name))
			   close-bracket-determined))
	      
	      ;; list all alternatives
	      open-bracket-prospects
	      (if most-is-exact
		  (concat "," alternatives)
		alternatives)
	      close-bracket-prospects)))
	  )))

(defun iswitchb-minibuffer-setup ()
  "Set up minibuffer for `iswitchb-buffer'.
Copied from `icomplete-minibuffer-setup-hook'."
  (if (iswitchb-entryfn-p)
      (progn

	(make-local-variable 'iswitchb-use-mycompletion)
	(setq iswitchb-use-mycompletion t)
	(make-local-hook 'pre-command-hook)
	(add-hook 'pre-command-hook
		  'iswitchb-pre-command
		  nil t)
	(make-local-hook 'post-command-hook)
	(add-hook 'post-command-hook
		  'iswitchb-post-command
		  nil t)
	
	(run-hooks 'iswitchb-minibuffer-setup-hook) 
	)
    ))


(defun iswitchb-pre-command ()
  "Run before command in `iswitchb-buffer'."
  (iswitchb-tidy))


(defun iswitchb-post-command ()
  "Run after command in `iswitchb-buffer'."
  (iswitchb-exhibit)
  )



(defun iswitchb-tidy ()
  "Remove completions display, if any, prior to new user input.
Copied from `icomplete-tidy'."

  (if (and (boundp 'iswitchb-eoinput)
	   iswitchb-eoinput)
      
      (if (> iswitchb-eoinput (point-max))
	  ;; Oops, got rug pulled out from under us - reinit:
	  (setq iswitchb-eoinput (point-max))
	(let ((buffer-undo-list buffer-undo-list )) ; prevent entry
	  (delete-region iswitchb-eoinput (point-max))))
    
    ;; Reestablish the local variable 'cause minibuffer-setup is weird:
    (make-local-variable 'iswitchb-eoinput)
    (setq iswitchb-eoinput 1)))


(defun iswitchb-entryfn-p ()
  "Return non-nil if `this-command' shows we are using `iswitchb-buffer'."
  (or (boundp 'iswitchb-prepost-hooks)
      ;; I think the of this may be redundant, since the prepost hooks
      ;; will always be set in the iswitchb defuns.
      ;;(and (symbolp this-command)		; ignore lambda functions
      ;;(memq this-command
      ;;	 '(iswitchb-buffer
      ;;	   iswitchb-buffer-other-frame
      ;;       iswitchb-display-buffer
      ;;       iswitchb-buffer-other-window))))
  ))





(defun iswitchb-summaries-to-end ()
  "Move the summaries to the end of the list.
This is an example function which can be hooked on to
`iswitchb-make-buflist-hook'.  Any buffer matching the regexps
`Summary' or `output\*$'are put to the end of the list."
  (let ((summaries (delq nil (mapcar 
			      (lambda (x) 
				 (if (or 
				      (string-match "Summary" x)
				      (string-match "output\\*$" x))
				     x))
			      buflist)
			      )))
    
    (iswitchb-to-end summaries)))



;;; HOOKS
(add-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup)

(provide 'iswitchb)

;;; iswitchb.el ends here
