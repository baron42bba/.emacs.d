;;; novice.el --- handling of disabled commands ("novice mode") for XEmacs.

;; Copyright (C) 1985-7, 1992-4, Free Software Foundation, Inc.

;; Maintainer: XEmacs Development Team
;; Keywords: internal, help

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

;; This mode provides a hook which is, by default, attached to various
;; putatively dangerous commands in a (probably futile) attempt to
;; prevent lusers from shooting themselves in the feet.

;;; Code:

;; This function is called (by autoloading)
;; to handle any disabled command.
;; The command is found in this-command
;; and the keys are returned by (this-command-keys).

;;;###autoload
;(setq disabled-command-hook 'disabled-command-hook)

;;;###autoload
(defun disabled-command-hook (&rest ignore)
  (let (char)
    (save-window-excursion
     (with-output-to-temp-buffer "*Help*"
       (let ((keys (this-command-keys)))
	 (if (or (equal keys []) ;XEmacs kludge
		 (eq (event-to-character (aref keys 0)) ?\r))
	     (princ "You have invoked the disabled command ")
	   (princ "You have typed ")
	   (princ (key-description keys))
	   (princ ", invoking disabled command ")))
       (princ this-command)
       (princ ":\n")
       ;; Print any special message saying why the command is disabled.
       (if (stringp (get this-command 'disabled))
	   (princ (get this-command 'disabled)))
       (princ (or (condition-case ()
		      (documentation this-command)
		    (error nil))
		  "<< not documented >>"))
       ;; Keep only the first paragraph of the documentation.
       (save-excursion
	 (set-buffer "*Help*")
	 (goto-char (point-min))
	 (if (search-forward "\n\n" nil t)
	     (delete-region (1- (point)) (point-max))
	   (goto-char (point-max))))
       (princ "\n\n")
       (princ "You can now type
Space to try the command just this once,
      but leave it disabled,
Y to try it and enable it (no questions if you use it again),
N to do nothing (command remains disabled).")
       (save-excursion
	(set-buffer standard-output)
	(help-mode)))
     (message "Type y, n or Space: ")
;     (let ((cursor-in-echo-area t))
;       (while (not (memq (setq char (downcase (read-char)))
;			 '(?  ?y ?n)))
;	 (ding)
;	 (message "Please type y, n or Space: "))))
     ;; XEmacs version
     (let ((cursor-in-echo-area t)
	   (inhibit-quit t)
	   event)
       (while (null char)
	 (if (progn
	       (setq event (next-command-event))
	       (prog1
		   (or quit-flag (eq 'keyboard-quit (key-binding event)))
		 (setq quit-flag nil)))
	     (progn
	       (setq quit-flag nil)
	       (signal 'quit '())))
	 (let* ((key (and (key-press-event-p event) (event-key event)))
		(rchar (and key (event-to-character event))))
	   (if rchar (setq rchar (downcase rchar)))
	   (cond ((eq rchar ?y)
		  (setq char rchar))
		 ((eq rchar ?n)
		  (setq char rchar))
		 ((eq rchar ? )
		  (setq char rchar))
		 (t
		  (ding nil 'y-or-n-p)
		  (discard-input)
		  (message "Please type y, n or Space: ")))))))
    (message nil)
    (if (= char ?y)
	(if (and user-init-file
		 (not (string= "" user-init-file))
		 (y-or-n-p "Enable command for future editing sessions also? "))
	    (enable-command this-command)
	  (put this-command 'disabled nil)))
    (if (/= char ?n)
	(call-interactively this-command))))

;;;###autoload
(defun enable-command (command)
  "Allow COMMAND to be executed without special confirmation from now on.
The user's `custom-file' is altered so that this will apply
to future sessions."
  (interactive "CEnable command: ")
  (put command 'disabled nil)
  (save-excursion
   (set-buffer (find-file-noselect
		(substitute-in-file-name custom-file)))
   (goto-char (point-min))
   (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
       (delete-region
	(progn (beginning-of-line) (point))
	(progn (forward-line 1) (point))))
   ;; Explicitly enable, in case this command is disabled by default
   ;; or in case the code we deleted was actually a comment.
   (goto-char (point-max))
   (or (bolp) (insert "\n"))
   (insert "(put '" (symbol-name command) " 'disabled nil)\n")
   (save-buffer)))

;;;###autoload
(defun disable-command (command)
  "Require special confirmation to execute COMMAND from now on.
The user's `custom-file' is altered so that this will apply
to future sessions."
  (interactive "CDisable command: ")
  (if (not (commandp command))
      (error "Invalid command name `%s'" command))
  (put command 'disabled t)
  (save-excursion
   (set-buffer (find-file-noselect
		(substitute-in-file-name custom-file)))
   (goto-char (point-min))
   (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
       (delete-region
	(progn (beginning-of-line) (point))
	(progn (forward-line 1) (point))))
   (goto-char (point-max))
   (or (bolp) (insert "\n"))
   (insert "(put '" (symbol-name command) " 'disabled t)\n")
   (save-buffer)))

(provide 'novice)

;;; novice.el ends here
