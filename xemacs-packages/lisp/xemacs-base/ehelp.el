;;; ehelp.el --- bindings for electric-help mode

;; Copyright (C) 1986, 1995 Free Software Foundation, Inc.

;; Author: Richard Mlynarik <mly@ai.mit.edu>
;; Maintainer: FSF
;; Keywords: help, extensions

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

;;; Synched up with: FSF 19.34.

;;; Commentary:

;; This package provides a pre-packaged `Electric Help Mode' for
;; browsing on-line help screens.  There is one entry point,
;; `with-electric-help'; all you have to give it is a no-argument
;; function that generates the actual text of the help into the current
;; buffer.

;; To make this the default, you must do
;; (require 'ehelp)
;; (define-key global-map "\C-h" 'ehelp-command)
;; (define-key global-map [help] 'ehelp-command)
;; (define-key global-map [f1] 'ehelp-command)

;;; Code:

(require 'electric)
(defvar electric-help-map ()
  "Keymap defining commands available in `electric-help-mode'.")

(defvar electric-help-form-to-execute nil)

(put 'electric-help-undefined 'suppress-keymap t)
(if electric-help-map
    ()
  (let ((map (make-keymap)))
    ;; allow all non-self-inserting keys - search, scroll, etc, but
    ;; let M-x and C-x exit ehelp mode and retain buffer:
    (suppress-keymap map)
    (define-key map "\C-u" 'electric-help-undefined)
    (define-key map [(control ?0)] 'electric-help-undefined)
    (define-key map [(control ?1)] 'electric-help-undefined)
    (define-key map [(control ?2)] 'electric-help-undefined)
    (define-key map [(control ?3)] 'electric-help-undefined)
    (define-key map [(control ?4)] 'electric-help-undefined)
    (define-key map [(control ?5)] 'electric-help-undefined)
    (define-key map [(control ?6)] 'electric-help-undefined)
    (define-key map [(control ?7)] 'electric-help-undefined)
    (define-key map [(control ?8)] 'electric-help-undefined)
    (define-key map [(control ?9)] 'electric-help-undefined)
    (define-key map (vector help-char) 'electric-help-help)
    (define-key map "?" 'electric-help-help)
    ;; XEmacs addition
    (define-key map 'help 'electric-help-help)
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map "." 'beginning-of-buffer)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)
    ;(define-key map "\C-g" 'electric-help-exit)
    (define-key map "q" 'electric-help-exit)
    (define-key map "Q" 'electric-help-exit)
    ;;a better key than this?
    (define-key map "r" 'electric-help-retain)
    (define-key map "R" 'electric-help-retain)
    (define-key map "\ex" 'electric-help-execute-extended)
    (define-key map "\C-x" 'electric-help-ctrl-x-prefix)

    (setq electric-help-map map)))

(defun electric-help-mode ()
  "`with-electric-help' temporarily places its buffer in this mode.
\(On exit from `with-electric-help', the buffer is put in `default-major-mode'.)"
  (setq buffer-read-only t)
  (setq mode-name "Help")
  (setq major-mode 'help)
  (setq modeline-buffer-identification '(" Help:  %b"))
  (use-local-map electric-help-map)
  (add-hook 'mouse-leave-buffer-hook 'electric-help-retain)
  (view-mode -1)
  ;; this is done below in with-electric-help
  ;(run-hooks 'electric-help-mode-hook)
  )

;;;###autoload
(defun with-electric-help (thunk &optional buffer noerase minheight)
  "Pop up an \"electric\" help buffer.
The arguments are THUNK &optional BUFFER NOERASE MINHEIGHT.
THUNK is a function of no arguments which is called to initialize the
contents of BUFFER.  BUFFER defaults to `*Help*'.  BUFFER will be
erased before THUNK is called unless NOERASE is non-nil.  THUNK will
be called while BUFFER is current and with `standard-output' bound to
the buffer specified by BUFFER.

If THUNK returns nil, we display BUFFER starting at the top, and
shrink the window to fit.  If THUNK returns non-nil, we don't do those things.

After THUNK has been called, this function \"electrically\" pops up a window
in which BUFFER is displayed and allows the user to scroll through that buffer
in electric-help-mode. The window's height will be at least MINHEIGHT if
this value is non-nil.

If THUNK returns nil, we display BUFFER starting at the top, and
shrink the window to fit.  If THUNK returns non-nil, we don't do those
things.

When the user exits (with `electric-help-exit', or otherwise) the help
buffer's window disappears (i.e., we use `save-window-excursion')
BUFFER is put into `default-major-mode' (or `fundamental-mode') when we exit."
  (setq buffer (get-buffer-create (or buffer "*Help*")))
  (let ((one (one-window-p t))
	(config (current-window-configuration))
        (bury nil)
        (electric-help-form-to-execute nil))
    (unwind-protect
	(save-excursion
	  (if one (goto-char (window-start (selected-window))))
	  (let ((pop-up-windows t))
	    (pop-to-buffer buffer))
	  (save-excursion
	    (set-buffer buffer)
	    (if (and minheight (< (window-height) minheight))
		(enlarge-window (- minheight (window-height))))
	    (electric-help-mode)
	    (setq buffer-read-only nil)
	    (or noerase
		(erase-buffer)))
	  (let ((standard-output buffer))
	    (if (not (funcall thunk))
		(progn
		  (set-buffer buffer)
		  (set-buffer-modified-p nil)
		  (goto-char (point-min))
		  (if one (shrink-window-if-larger-than-buffer (selected-window))))))
	  (set-buffer buffer)
	  (run-hooks 'electric-help-mode-hook)
	  (setq buffer-read-only t)
	  (if (eq (car-safe
		   ;; XEmacs: Don't be screwed by minor-modes (view-minor-mode)
		   (let ((overriding-local-map electric-help-map))
		     (electric-help-command-loop)))
		  'retain)
	      (setq config (current-window-configuration))
	    (setq bury t)))
      (message "")
      (set-buffer buffer)
      (setq buffer-read-only nil)
      (condition-case ()
	  (funcall (or default-major-mode 'fundamental-mode))
	(error nil))
      (set-window-configuration config)
      (if bury
          (progn
            ;;>> Perhaps this shouldn't be done.
            ;; so that when we say "Press space to bury" we mean it
            (replace-buffer-in-windows buffer)
            ;; must do this outside of save-window-excursion
            (bury-buffer buffer)))
      (eval electric-help-form-to-execute))))

(defun electric-help-command-loop ()
  (catch 'exit
    (if (pos-visible-in-window-p (point-max))
	(progn (message "%s" (substitute-command-keys "<<< Press Space to bury the help buffer, Press \\[electric-help-retain] to retain it >>>"))
	       ;; XEmacs change
	       (if (equal (setq unread-command-events
				(list (next-command-event)))
			  '(?\ ))
		   (progn (setq unread-command-events nil)
			  (throw 'exit t)))))
    (let (up down both neither
	  (standard (and (eq (key-binding " ")
			     'scroll-up)
			 (eq (key-binding "\^?")
			     'scroll-down)
			 (eq (key-binding "q")
			     'electric-help-exit)
			 (eq (key-binding "r")
			     'electric-help-retain))))
      (Electric-command-loop
        'exit
	(function (lambda ()
	  (sit-for 0) ;necessary if last command was end-of-buffer or 
	              ;beginning-of-buffer - otherwise pos-visible-in-window-p 
	              ;will yield a wrong result.
	  (let ((min (pos-visible-in-window-p (point-min)))
		(max (pos-visible-in-window-p (point-max))))
	    (cond (isearch-mode 'noprompt)
		  ((and min max)
		   (cond (standard "Press q to exit, r to retain ")
			 (neither)
			 (t (setq neither (substitute-command-keys "Press \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))
		  (min
		   (cond (standard "Press SPC to scroll, q to exit, r to retain ")
			 (up)
			 (t (setq up (substitute-command-keys "Press \\[scroll-up] to scroll, \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))
		  (max
		   (cond (standard "Press DEL to scroll back, q to exit, r to retain ")
			 (down)
			 (t (setq down (substitute-command-keys "Press \\[scroll-down] to scroll back, \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))
		  (t
		   (cond (standard "Press SPC to scroll, DEL to scroll back, q to exit, r to retain ")
			 (both)
			 (t (setq both (substitute-command-keys "Press \\[scroll-up] to scroll, \\[scroll-down] to scroll back, \\[electric-help-exit] to exit, \\[electric-help-retain] to retain ")))))))))
		    t))))



;(defun electric-help-scroll-up (arg)
;  ">>>Doc"
;  (interactive "P")
;  (if (and (null arg) (pos-visible-in-window-p (point-max)))
;      (electric-help-exit)
;    (scroll-up arg)))

(defun electric-help-exit ()
  ">>>Doc"
  (interactive)
  (throw 'exit t))

(defun electric-help-retain ()
  "Exit `electric-help', retaining the current window/buffer configuration.
\(The *Help* buffer will not be selected, but \\[switch-to-buffer-other-window] RET
will select it.)"
  (interactive)
  ;; Make sure that we don't throw twice, even if two events cause
  ;; calling this function:
  (if (memq 'electric-help-retain mouse-leave-buffer-hook)
      (progn
	(remove-hook 'mouse-leave-buffer-hook 'electric-help-retain)
	(throw 'exit '(retain)))))


(defun electric-help-undefined ()
  (interactive)
  (error "%s is undefined -- Press %s to exit"
	 (mapconcat 'single-key-description (this-command-keys) " ")
	 (if (eq (key-binding "q") 'electric-help-exit)
	     "q"
	   (substitute-command-keys "\\[electric-help-exit]"))))


;>>> this needs to be hairified (recursive help, anybody?)
(defun electric-help-help ()
  (interactive)
  (if (and (eq (key-binding "q") 'electric-help-exit)
	   (eq (key-binding " ") 'scroll-up)
	   (eq (key-binding "\^?") 'scroll-down)
	   (eq (key-binding "r") 'electric-help-retain))
      (message "SPC scrolls up, DEL scrolls down, q exits burying help buffer, r exits")
    (message "%s" (substitute-command-keys "\\[scroll-up] scrolls up, \\[scroll-down] scrolls down, \\[electric-help-exit] exits burying help buffer, \\[electric-help-retain] exits")))
  (sit-for 2))


;;;###autoload
(defun electric-helpify (fun &optional name)
  (let ((name (or name "*Help*")))
    (if (save-window-excursion
	  ;; kludge-o-rama
	  (let* ((p (symbol-function 'print-help-return-message))
		 (b (get-buffer name))
		 (m (buffer-modified-p b)))
	    (and b (not (get-buffer-window b))
		 (setq b nil))
	    (unwind-protect
		(progn
		  (message "%s..." (capitalize (symbol-name fun)))
		  ;; with-output-to-temp-buffer marks the buffer as unmodified.
		  ;; kludging excessively and relying on that as some sort
		  ;;  of indication leads to the following abomination...
		  ;;>> This would be doable without such icky kludges if either
		  ;;>> (a) there were a function to read the interactive
		  ;;>>     args for a command and return a list of those args.
		  ;;>>     (To which one would then just apply the command)
		  ;;>>     (The only problem with this is that interactive-p
		  ;;>>      would break, but that is such a misfeature in
		  ;;>>      any case that I don't care)
		  ;;>>     It is easy to do this for emacs-lisp functions;
		  ;;>>     the only problem is getting the interactive spec
		  ;;>>     for subrs
		  ;;>> (b) there were a function which returned a
		  ;;>>     modification-tick for a buffer.  One could tell
		  ;;>>     whether a buffer had changed by whether the
		  ;;>>     modification-tick were different.
		  ;;>>     (Presumably there would have to be a way to either
		  ;;>>      restore the tick to some previous value, or to
		  ;;>>      suspend updating of the tick in order to allow
		  ;;>>      things like momentary-string-display)
		  (and b
		       (save-excursion
			 (set-buffer b)
			 (set-buffer-modified-p t)))
		  (fset 'print-help-return-message 'ignore)
		  (call-interactively fun)
		  (and (get-buffer name)
		       (get-buffer-window (get-buffer name))
		       (or (not b)
			   (not (eq b (get-buffer name)))
			   (not (buffer-modified-p b)))))
	      (fset 'print-help-return-message p)
	      (and b (buffer-name b)
		   (save-excursion
		     (set-buffer b)
		     (set-buffer-modified-p m))))))
	(with-electric-help 'ignore name t))))



;; This is to be bound to M-x in ehelp mode. Retains ehelp buffer and then 
;; continues with execute-extended-command.
(defun electric-help-execute-extended (prefixarg)
  (interactive "p")
  (setq electric-help-form-to-execute '(execute-extended-command nil))
  (electric-help-retain))

;; This is to be buond to C-x in ehelp mode. Retains ehelp buffer and then
;; continues with ctrl-x prefix.
(defun electric-help-ctrl-x-prefix (prefixarg)
  (interactive "p")
  (setq electric-help-form-to-execute '(progn (message nil) (setq unread-command-char ?\C-x)))
  (electric-help-retain))


(defun electric-describe-key ()
  (interactive)
  (electric-helpify 'describe-key))

(defun electric-describe-mode ()
  (interactive)
  (electric-helpify 'describe-mode))

(defun electric-view-lossage ()
  (interactive)
  (electric-helpify 'view-lossage))

;(defun electric-help-for-help ()
;  "See help-for-help"
;  (interactive)
;  )

(defun electric-describe-function ()
  (interactive)
  (electric-helpify 'describe-function))

(defun electric-describe-variable ()
  (interactive)
  (electric-helpify 'describe-variable))

(defun electric-describe-bindings ()
  (interactive)
  (electric-helpify 'describe-bindings))

(defun electric-describe-syntax ()
  (interactive)
  (electric-helpify 'describe-syntax))

(defun electric-command-apropos ()
  (interactive)
  (electric-helpify 'command-apropos "*Apropos*"))

;(define-key help-map "a" 'electric-command-apropos)

(defun electric-apropos ()
  (interactive)
  (electric-helpify 'apropos))


;;;; ehelp-map

(defvar ehelp-map ())
(if ehelp-map
    nil
  ;; #### WTF?  Why don't we just use substitute-key-definition
  ;; like FSF does?
  (let ((shadow '((apropos . electric-apropos)
		  (command-apropos . electric-command-apropos)
		  (describe-key . electric-describe-key) 
                  (describe-mode . electric-describe-mode)
                  (view-lossage . electric-view-lossage) 
                  (describe-function . electric-describe-function)
                  (describe-variable . electric-describe-variable)
                  (describe-bindings . electric-describe-bindings)
                  (describe-syntax . electric-describe-syntax)))
        (map (make-sparse-keymap)))
    (set-keymap-name map 'ehelp-map)
    (set-keymap-parents map (list help-map))
    ;; Shadow bindings which would be inherited from help-map
    ;;#### This doesn't descend into sub-keymaps
    (map-keymap (function (lambda (key binding)
                              (let ((tem (assq binding shadow)))
                                (if tem
                                    (define-key map key (cdr tem))))))
                help-map)
    (setq ehelp-map map)
    (fset 'ehelp-command map)))

;; Do (define-key global-map "\C-h" 'ehelp-command) if you want to win

(provide 'ehelp) 

;;; ehelp.el ends here
