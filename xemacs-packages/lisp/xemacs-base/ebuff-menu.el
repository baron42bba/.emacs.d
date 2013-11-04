;;; ebuff-menu.el --- electric-buffer-list mode

;; Copyright (C) 1985, 1986, 1994 Free Software Foundation, Inc.

;; Author: Richard Mlynarik <mly@ai.mit.edu>
;; Keywords: frames

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

;; Who says one can't have typeout windows in GNU Emacs?   The entry
;; point, `electric-buffer-list' works like ^r select buffer from the
;; ITS Emacs lunar or tmacs libraries.

;;; Code:

(require 'electric)
;; XEmacs change
(require 'buff-menu)

;; this depends on the format of list-buffers (from src/buffer.c) and
;; on stuff in lisp/buff-menu.el

(defvar electric-buffer-menu-mode-map nil)

;;;###autoload
(defun electric-buffer-list (&optional files-only)
  "Pops up a buffer describing the set of Emacs buffers.
Vaguely like ITS lunar select buffer; combining typeoutoid buffer
listing with menuoid buffer selection.

If the very next character typed is a space then the buffer list
window disappears.  Otherwise, one may move around in the buffer list
window, marking buffers to be selected, saved or deleted.

To exit and select a new buffer, type a space when the cursor is on
the appropriate line of the buffer-list window.  Other commands are
much like those of buffer-menu-mode.

Calls value of `electric-buffer-menu-mode-hook' on entry if non-nil.

Non-null optional arg FILES-ONLY means mention only file buffers.
When called from Lisp code, FILES-ONLY may be a regular expression,
in which case only buffers whose names match that expression are listed,
or an arbitrary predicate function.

\\{electric-buffer-menu-mode-map}" 
  (interactive (list (if current-prefix-arg t nil)))
  (let (select buffer)
    (save-window-excursion
      (save-window-excursion (list-buffers files-only))
      (setq buffer (window-buffer (Electric-pop-up-window "*Buffer List*")))
      (unwind-protect
	  (progn
	    (set-buffer buffer)
	    (Electric-buffer-menu-mode)
	    (setq select
		  (catch 'electric-buffer-menu-select
		    (message "<<< Press Return to bury the buffer list >>>")
		    ;; XEmacs change
		    (if (eq (setq unread-command-events
				  (list (next-command-event)))
			    ?\ )
			(progn (setq unread-command-events nil)
			       (throw 'electric-buffer-menu-select nil)))
		    (let ((start-point (point))
			  (first (progn (goto-char (point-min))
					(forward-line 2)
					(point)))
			  (last (progn (goto-char (point-max))
				       (forward-line -1)
				       (point)))
			  (goal-column 0))
		      ;; Use start-point if it is meaningful.
		      (goto-char (if (or (< start-point first)
					 (> start-point last))
				     first
				   start-point))
		      (Electric-command-loop 'electric-buffer-menu-select
					     nil
					     t
					     'electric-buffer-menu-looper
					     (cons first last))))))
	(set-buffer buffer)
	(Buffer-menu-mode)
	(bury-buffer buffer)
	(message "")))
    (if select
	(progn (set-buffer buffer)
	       (let ((opoint (point-marker)))
		 (Buffer-menu-execute)
		 (goto-char (point-min))
		 (if (prog1 (search-forward "\n>" nil t)
		       (goto-char opoint) (set-marker opoint nil))
		     (Buffer-menu-select)
		   (switch-to-buffer (Buffer-menu-buffer t))))))))

(defun electric-buffer-menu-looper (state condition)
  (cond ((and condition
	      (not (memq (car condition) '(buffer-read-only
					   end-of-buffer
					   beginning-of-buffer))))
	 (signal (car condition) (cdr condition)))
	((< (point) (car state))
	 (goto-char (point-min))
	 (forward-line 2))
	((> (point) (cdr state))
	 (goto-char (point-max))
	 (forward-line -1)
	 (if (pos-visible-in-window-p (point-max))
	     (recenter -1)))))

(put 'Electric-buffer-menu-mode 'mode-class 'special)
(defun Electric-buffer-menu-mode ()
  "Major mode for editing a list of buffers.
Each line describes one of the buffers in Emacs.
Letters do not insert themselves; instead, they are commands.
\\<electric-buffer-menu-mode-map>
\\[keyboard-quit] or \\[Electric-buffer-menu-quit] -- exit buffer menu, returning to previous window and buffer
  configuration.  If the very first character typed is a space, it
  also has this effect.
\\[Electric-buffer-menu-select] -- select buffer of line point is on.
  Also show buffers marked with m in other windows,
  deletes buffers marked with \"D\", and saves those marked with \"S\".
\\[Buffer-menu-mark] -- mark buffer to be displayed.
\\[Buffer-menu-not-modified] -- clear modified-flag on that buffer.
\\[Buffer-menu-save] -- mark that buffer to be saved.
\\[Buffer-menu-delete] or \\[Buffer-menu-delete-backwards] -- mark that buffer to be deleted.
\\[Buffer-menu-unmark] -- remove all kinds of marks from current line.
\\[Electric-buffer-menu-mode-view-buffer] -- view buffer, returning when done.
\\[Buffer-menu-backup-unmark] -- back up a line and remove marks.

\\{electric-buffer-menu-mode-map}

Entry to this mode via command `electric-buffer-list' calls the value of
`electric-buffer-menu-mode-hook' if it is non-nil."
  (kill-all-local-variables)
  (use-local-map electric-buffer-menu-mode-map)
  (setq mode-name "Electric Buffer Menu")
  (setq mode-line-buffer-identification "Electric Buffer List")
  ;; XEmacs
  (if (memq 'mode-name mode-line-format)
      (progn (setq mode-line-format (copy-sequence mode-line-format))
	     (setcar (memq 'mode-name mode-line-format) "Buffers")))
  (make-local-variable 'Helper-return-blurb)
  (setq Helper-return-blurb "return to buffer editing")
  (setq truncate-lines t)
  ;; XEmacs
  (setq buffer-scrollbar-height 0)
  (setq buffer-read-only t)
  (setq major-mode 'Electric-buffer-menu-mode)
  ;; XEmacs
  (setq mode-motion-hook 'mode-motion-highlight-line)
  (goto-char (point-min))
  (if (search-forward "\n." nil t) (forward-char -1))
  (run-hooks 'electric-buffer-menu-mode-hook))

;; generally the same as Buffer-menu-mode-map
;;  (except we don't indirect to global-map)
(put 'Electric-buffer-menu-undefined 'suppress-keymap t)
(if electric-buffer-menu-mode-map
    nil
  (let ((map (make-keymap)) (submap (make-keymap)))
    ;(fillarray (car (cdr map)) 'Electric-buffer-menu-undefined) ; FSF
    (let ((i 0))
      (while (< i 128)
	(define-key map (make-string 1 i) 'Electric-buffer-menu-undefined)
	(setq i (1+ i))))
    (define-key map "\e" submap)
    ;(fillarray (car (cdr submap)) 'Electric-buffer-menu-undefined) ; FSF
    (let ((map2 (lookup-key map "\e"))
	   (i 0))
      (while (< i 128)
	(define-key map2 (make-string 1 i) 'Electric-buffer-menu-undefined)
	(setq i (1+ i))))
    (define-key map "\C-z" 'suspend-emacs)
    (define-key map "v" 'Electric-buffer-menu-mode-view-buffer)
    (define-key map (vector help-char) 'Helper-help)
    (define-key map "?" 'Helper-describe-bindings)
    (define-key map "\C-c" nil)
    (define-key map "\C-c\C-c" 'Electric-buffer-menu-quit)
    (define-key map "\C-]" 'Electric-buffer-menu-quit)
    (define-key map "q" 'Electric-buffer-menu-quit)
    (define-key map " " 'Electric-buffer-menu-select)
    (define-key map "\C-m" 'Electric-buffer-menu-select)
    (define-key map "\C-l" 'recenter)
    (define-key map "s" 'Buffer-menu-save)
    (define-key map "d" 'Buffer-menu-delete)
    (define-key map "k" 'Buffer-menu-delete)
    (define-key map "\C-d" 'Buffer-menu-delete-backwards)
    ;(define-key map "\C-k" 'Buffer-menu-delete)
    (define-key map "\177" 'Buffer-menu-backup-unmark)
    ;; XEmacs
    (define-key map 'backspace 'Buffer-menu-backup-unmark)
    (define-key map "~" 'Buffer-menu-not-modified)
    (define-key map "u" 'Buffer-menu-unmark)
    (let ((i ?0))
      (while (<= i ?9)
	(define-key map (char-to-string i) 'digit-argument)
        ;;#### Urk!
	(define-key map (concat "\e" (char-to-string i)) 'digit-argument)
	(setq i (1+ i))))
    (define-key map "-" 'negative-argument)
    (define-key map "\e-" 'negative-argument)
    (define-key map "m" 'Buffer-menu-mark)
    (define-key map "\C-u" 'universal-argument)
    (define-key map "\C-p" 'previous-line)
    (define-key map "\C-n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "\C-v" 'scroll-up)
    (define-key map "\ev" 'scroll-down)
    (define-key map ">" 'scroll-right)
    (define-key map "<" 'scroll-left)
    (define-key map "\e\C-v" 'scroll-other-window)
    (define-key map "\e>" 'end-of-buffer)
    (define-key map "\e<" 'beginning-of-buffer)
    (define-key map "\e\e" nil)
    (define-key map "\e\e\e" 'Electric-buffer-menu-quit)
    ;; XEmacs
    (define-key map [home] 'beginning-of-buffer)
    (define-key map [down] 'next-line)
    (define-key map [up] 'previous-line)
    (define-key map [prior] 'scroll-down)
    (define-key map [next] 'scroll-up)
    (define-key map 'button2 'Electric-buffer-menu-mouse-select)
    (define-key map 'button3 'Buffer-menu-popup-menu)
    (setq electric-buffer-menu-mode-map map)))
 
(defun Electric-buffer-menu-exit ()
  (interactive)
  ;; XEmacs
  (setq unread-command-event last-input-event)
  ;; for robustness
  (condition-case ()
      (throw 'electric-buffer-menu-select nil)
    (error (Buffer-menu-mode)
	   (other-buffer))))

(defun Electric-buffer-menu-select ()
  "Leave Electric Buffer Menu, selecting buffers and executing changes.
Saves buffers marked \"S\".  Deletes buffers marked \"K\".
Selects buffer at point and displays buffers marked \">\" in other windows."
  (interactive)
  (throw 'electric-buffer-menu-select (point)))

(defun Electric-buffer-menu-mouse-select (event)
  (interactive "e")
  ;; XEmacs is simpler
  (mouse-set-point event)
  (Electric-buffer-menu-select))

(defun Electric-buffer-menu-quit ()
  "Leave Electric Buffer Menu, restoring previous window configuration.
Does not execute select, save, or delete commands."
  (interactive)
  (throw 'electric-buffer-menu-select nil))

(defun Electric-buffer-menu-undefined ()
  (interactive)
  (ding)
  (message "%s"
	   (if (and (eq (key-binding "\C-c\C-c") 'Electric-buffer-menu-quit)
		    (eq (key-binding " ") 'Electric-buffer-menu-select)
		    (eq (key-binding (vector help-char)) 'Helper-help)
		    (eq (key-binding "?") 'Helper-describe-bindings))
	       (substitute-command-keys "Type C-c C-c to exit, Space to select,
Type \\[Electric-buffer-menu-quit] to exit, \
\\[Electric-buffer-menu-select] to select, \
\\[Helper-help] for help, \\[Helper-describe-bindings] for commands.")))
  (sit-for 4))

(defun Electric-buffer-menu-mode-view-buffer ()
  "View buffer on current line in Electric Buffer Menu.
Returns to Electric Buffer Menu when done."
  (interactive)
  (let ((bufnam (Buffer-menu-buffer nil)))
    (if bufnam
	(view-buffer bufnam)
      (ding)
      (message "Buffer %s does not exist!" bufnam)
      (sit-for 4))))

(provide 'ebuff-menu)

;;; ebuff-menu.el ends here
