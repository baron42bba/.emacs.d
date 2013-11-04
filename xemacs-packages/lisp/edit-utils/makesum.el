;;; makesum.el --- generate key binding summary for Emacs

;; Copyright (C) 1985 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help

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

;;; Synched up with: FSF 20.5.

;;; Commentary:

;; Displays a nice human-readable summary of all keybindings in a
;; two-column format.

;;; Code:

;;;###autoload
(defun make-command-summary ()
  "Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first."
  (interactive)
  (let (help-buffer)
    (message "Making command summary...")
    ;; This puts a description of bindings in a buffer called *Help*.
    (save-window-excursion
      (setq help-buffer (describe-bindings)))
    (with-displaying-help-buffer
     (lambda ()
       (save-excursion
	 (let ((cur-mode mode-name))
	   (set-buffer standard-output)
	   (erase-buffer)
	   (insert-buffer-substring help-buffer)
	   (goto-char (point-min))
	   (delete-region (point) (progn (forward-line 1) (point)))
	   (while (search-forward "         " nil t)
	     (replace-match "  "))
	   (goto-char (point-min))
	   (while (search-forward "-@ " nil t)
	     (replace-match "-SP"))
	   (goto-char (point-min))
	   (while (search-forward "  .. ~ " nil t)
	     (replace-match "SP .. ~"))
	   (goto-char (point-min))
	   (while (search-forward "C-?" nil t)
	     (replace-match "DEL"))
	   (goto-char (point-min))
	   (while (search-forward "C-i" nil t)
	     (replace-match "TAB"))
	   (goto-char (point-min))
	   (if (re-search-forward "^Local Bindings:" nil t)
	       (progn
		 (forward-char -1)
		 (insert " for " cur-mode " Mode")
		 (while (search-forward "??\n" nil t)
		   (delete-region (point)
				  (progn
				    (forward-line -1)
				    (point))))))
	   (goto-char (point-min))
	   (insert
	    (cond ((featurep 'infodock) "InfoDock")
		  ((featurep 'xemacs) "XEmacs")
		  (t "Emacs")))
	   (insert " command summary, " (substring (current-time-string) 0 10)
		   ".\n")
	   ;; Delete "key    binding" and underlining of dashes.
	   (delete-region (point) (progn (forward-line 2) (point)))
	   (forward-line 1)		;Skip blank line
	   (while (not (eobp))
	     (let ((beg (point)))
	       (or (re-search-forward "^$" nil t)
		   (goto-char (point-max)))
	       (double-column beg (point))
	       (forward-line 1)))
	   (goto-char (point-min))))
       (message "Making command summary...done"))
     "Command Summary")))

(defun double-column (start end)
  (interactive "r")
  (let (half line lines nlines
	(from-end (- (point-max) end)))
    (setq nlines (count-lines start end))
    (if (<= nlines 1)
	nil
      (setq half (/ (1+ nlines) 2))
      (goto-char start)
      (save-excursion
       (forward-line half)
       (while (< half nlines)
	 (setq half (1+ half))
	 (setq line (buffer-substring (point) (save-excursion (end-of-line) (point))))
	 (setq lines (cons line lines))
	 (delete-region (point) (progn (forward-line 1) (point)))))
      (setq lines (nreverse lines))
      (while lines
	(end-of-line)
	;; XEmacs change
	(indent-to 41 1)
	(insert (car lines))
	(forward-line 1)
	(setq lines (cdr lines))))
    (goto-char (- (point-max) from-end))))

(provide 'makesum)

;;; makesum.el ends here
