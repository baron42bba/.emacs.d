;; Decorate a shell buffer with fonts.
;; Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

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
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;; Do this: (add-hook 'shell-mode-hook 'install-shell-fonts)
;; and the prompt in your shell-buffers will appear bold-italic, process
;; output will appear in normal face, and typein will appear in bold.
;;
;; The faces shell-prompt, shell-input and shell-output can be modified
;; as desired, for example, (copy-face 'italic 'shell-prompt).

;; Written by Jamie Zawinski, overhauled by Eric Benson.

;; TODO:
;; =====
;; Parse ANSI/VT100 escape sequences to turn on underlining/boldface/etc.
;; Automatically run nuke-nroff-bs?


(require 'text-props)	; for put-nonduplicable-text-property

(make-face 'shell-prompt)
(if (not (face-differs-from-default-p 'shell-prompt))
    (copy-face 'bold-italic 'shell-prompt))

(make-face 'shell-input)
(if (not (face-differs-from-default-p 'shell-input))
    (copy-face 'bold 'shell-input))

(make-face 'shell-output)
(if (not (face-differs-from-default-p 'shell-output))
    (progn (make-face-unbold 'shell-output)
	   (make-face-unitalic 'shell-output)
	   (set-face-underline-p 'shell-output nil)))

(defvar shell-font-read-only-prompt nil
  "*Set all shell prompts to be read-only")

(defvar shell-font-current-face 'shell-input)

(defun shell-font-fontify-region (start end delete-count)
  ;; for use as an element of after-change-functions; fontifies the inserted text.
  (if (= start end)
      nil
;    ;; This creates lots of extents (one per user-typed character)
;    ;; which is wasteful of memory.
;    (let ((e (make-extent start end)))
;      (set-extent-face e shell-font-current-face)
;      (set-extent-property e 'shell-font t))

    ;; This efficiently merges extents
    (put-nonduplicable-text-property start end 'face shell-font-current-face)
    (and shell-font-read-only-prompt
	 (eq shell-font-current-face 'shell-prompt)
	 (put-nonduplicable-text-property start end 'read-only t))
    ))

(defun shell-font-hack-prompt (limit)
  "Search backward from point-max for text matching the comint-prompt-regexp,
and put it in the `shell-prompt' face.  LIMIT is the left bound of the search."
  (save-excursion
    (goto-char (point-max))
    (save-match-data
     (cond ((re-search-backward comint-prompt-regexp limit t)
	    (goto-char (match-end 0))
	    (cond ((= (point) (point-max))
		   (skip-chars-backward " \t")
		   (let ((shell-font-current-face 'shell-prompt))
		     (shell-font-fontify-region
		      (match-beginning 0) (point) 0)))))))))

;; David Hughes 22nd April 1998
(defun shell-font-hack-input (string)
  (save-excursion
    (save-match-data
      (goto-char (- (point-max) (length (replace-in-string string "\^M" ""))))
      (skip-chars-forward " \t")
      (let ((beg (point))
            (end (save-excursion (re-search-forward "\n" nil t)))
            (shell-font-current-face 'shell-input))
        (beginning-of-line)
        (and end
             (looking-at comint-prompt-regexp)
             (re-search-forward comint-prompt-regexp)
             (= beg (point))
             (shell-font-fontify-region beg end 0))))))

(defvar shell-font-process-filter nil
  "In an interaction buffer with shell-font, this is the original proc filter.
shell-font encapsulates this.")

(defun shell-font-process-filter (proc string)
  "Invoke the original process filter, then set fonts on the output.
The original filter is in the buffer-local variable shell-font-process-filter."
  (let ((cb (current-buffer))
	(pb (process-buffer proc)))
    (if (null pb)
	;; If the proc has no buffer, leave it alone.
	(funcall shell-font-process-filter proc string)
      ;; Don't do save excursion because some proc filters want to change
      ;; the buffer's point.
      (set-buffer pb)
      (let ((p (marker-position (process-mark proc))))
	(prog1
	    ;; this let must not be around the `set-buffer' call.
	    (let ((shell-font-current-face 'shell-output))
	      (funcall shell-font-process-filter proc string))
          (shell-font-hack-prompt p)
          ;; David Hughes 22nd April 1998
	  (if comint-process-echoes (shell-font-hack-input string))
	  (set-buffer cb))))))

;;;###autoload
(defun install-shell-fonts ()
  "Decorate the current interaction buffer with fonts.
This uses the faces called `shell-prompt', `shell-input' and `shell-output';
you can alter the graphical attributes of those with the normal
face-manipulation functions."
  (let* ((proc (or (get-buffer-process (current-buffer))
		   (error "no process in %S" (current-buffer))))
	 (old (or (process-filter proc)
		  (error "no process filter on %S" proc))))
    (make-local-variable 'after-change-functions)
    (add-hook 'after-change-functions 'shell-font-fontify-region)
    (make-local-variable 'shell-font-current-face)
    (setq shell-font-current-face 'shell-input)
    (make-local-variable 'shell-font-process-filter)
    (or (eq old 'shell-font-process-filter) ; already set
	(setq shell-font-process-filter old))
    (set-process-filter proc 'shell-font-process-filter))
  nil)

(add-hook 'shell-mode-hook	'install-shell-fonts)
(add-hook 'telnet-mode-hook	'install-shell-fonts)
(add-hook 'gdb-mode-hook	'install-shell-fonts)

;; for compatibility with the 19.8 version
;(fset 'install-shell-font-prompt 'install-shell-fonts)
(make-obsolete 'install-shell-font-prompt 'install-shell-fonts)

(provide 'shell-font)

;;; shell-font.el ends here
