;;;  paranoid.el -- even more paranoid password prompter

;; Copyright (C) 1998 Ray Jones

;; Author: Ray Jones, rjones@pobox.com
;; Keywords: password, comint
;; Created: 1998-05-20

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; taken from comint.el, so it doesn't have to be loaded in its
;; entirety, and made a little more paranoid.

;; (defun comint-read-noecho (prompt &optional stars) ...)
(defun prompt-paranoidly (prompt)

  "Read a single line of text from user without echoing, and return
it.  Prompt with argument PROMPT, a string.  Input ends with RET, LFD,
or ESC.  DEL or C-h rubs out.  C-u kills line.  C-g aborts (if
`inhibit-quit' is set because e.g. this function was called from a
process filter and C-g is pressed, this function returns nil rather
than a string).

Note that the keystrokes comprising the text can still be recovered
\(temporarily) with \\[view-lossage].  Some people find this worrysome.
Once the caller uses the password, it can erase the password
by doing \(fillarray STRING 0)."
  (let ((ans "")
	(newans nil)
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t)
	(message-log-max nil)
	(done nil))
    (while (not done)
      (message "%s" prompt)
      ;; Use this instead of `read-char' to avoid "Non-character input-event".
      (setq c (read-char-exclusive))
      (cond ((= c ?\C-g)
	     ;; This function may get called from a process filter, where
	     ;; inhibit-quit is set.  In later versions of emacs read-char
	     ;; may clear quit-flag itself and return C-g.  That would make
	     ;; it impossible to quit this loop in a simple way, so
	     ;; re-enable it here (for backward-compatibility the check for
	     ;; quit-flag below would still be necessary, so this seems
	     ;; like the simplest way to do things).
	     (fillarray ans 0)
	     (setq quit-flag t
		   done t))
	    ((or (= c ?\r) (= c ?\n) (= c ?\e))
	     (setq done t))
	    ((= c ?\C-u)
	     (fillarray ans 0)
	     (setq ans ""))
	    ((and (/= c ?\b) (/= c ?\177))
	     (setq newans (concat ans (char-to-string c)))
	     (fillarray ans 0)
	     (setq ans newans))
	    ((> (length ans) 0)
	     (setq newans (substring ans 0 -1))
	     (fillarray ans 0)
	     (setq ans newans))))
    (if quit-flag
        ;; Emulate a true quit, except that we have to return a value.
        (prog1
            (setq quit-flag nil)
          (message "Quit")
          (beep t))
      (message "")
      ans)))

(provide 'paranoid)
