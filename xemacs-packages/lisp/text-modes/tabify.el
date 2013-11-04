;;; tabify.el --- tab conversion commands for XEmacs

;; Copyright (C) 1985, 1994 Free Software Foundation, Inc.

;; Maintainer: FSF

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

;; Commands to optimize spaces to tabs or expand tabs to spaces in a region
;; (`tabify' and `untabify').  The variable tab-width does the obvious.

;;; Code:

;;;###autoload
(defun untabify (start end)
  "Convert all tabs in region to multiple spaces, preserving columns.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) end)
      (goto-char start)
      (let ((percent 5))
	(while (search-forward "\t" nil t)	; faster than re-search
	  (let ((tab-beg (point))
		(column (current-column))
		(indent-tabs-mode nil))
	    (skip-chars-backward "\t" start)
	    (delete-region tab-beg (point))
	    ;; XEmacs change -- show progress
	    (indent-to column)
	    (if (> (/ (* 100 (- (point) start)) (- (point-max) start)) percent)
		(progn
		  (message "untabify: %d%% ..." percent)
		  (setq percent (+ 5 percent)))))))
      (message "untabify: done")))
  nil)

;;;###autoload
(defun tabify (start end)
  "Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs
when this can be done without changing the column they end at.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops."
  (interactive "r")
  (save-excursion
    (save-restriction
      ;; Include the beginning of the line in the narrowing
      ;; since otherwise it will throw off current-column.
      (goto-char start)
      (beginning-of-line)
      (narrow-to-region (point) end)
      (goto-char start)
      (let ((percent 5))
	(while (re-search-forward "[ \t][ \t][ \t]*" nil t)
	  (let ((column (current-column))
		(indent-tabs-mode t))
	    (delete-region (match-beginning 0) (point))
	    ;; XEmacs change -- show progress
	    (indent-to column)
	    (if (> (/ (* 100 (- (point) start)) (- (point-max) start)) percent)
		(progn
		  (message "tabify: %d%% ..." percent)
		  (setq percent (+ 5 percent)))))))
      (message "tabify: done"))))

(provide 'tabify)

;;; tabify.el ends here
