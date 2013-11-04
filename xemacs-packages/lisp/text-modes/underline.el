;;; underline.el --- insert/remove underlining (done by overstriking) in Emacs.

;; Copyright (C) 1985, 1993 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: wp

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

;; This package deals with the primitive form of underlining
;; consisting of prefixing each character with "_\^h".  The entry
;; point `underline-region' performs such underlining on a region.
;; The entry point `ununderline-region' removes it.

;;; Code:

;;;###autoload
(defun underline-region (start end)
  "Underline all nonblank characters in the region.
Works by overstriking underscores.
Called from program, takes two arguments START and END
which specify the range to operate on."
  ;; XEmacs: FSF doesn't use a '*', a bug?  -sb
  (interactive "*r")
  (save-excursion
    (let ((end1 (make-marker)))
      (move-marker end1 (max start end))
      (goto-char (min start end))
      (while (< (point) end1)
	(or (looking-at "[_\^@- ]")
	    (insert "_\b"))
	(forward-char 1)))))

;;;###autoload
(defun ununderline-region (start end)
  "Remove all underlining (overstruck underscores) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on."
  (interactive "*r")
  (save-excursion
    (let ((end1 (make-marker)))
      (move-marker end1 (max start end))
      (goto-char (min start end))
      (while (re-search-forward "_\b\\|\b_" end1 t)
	(delete-char -2)))))

;; XEmacs:  The rest of these functions are not in FSF.  I don't see any
;; point in removing them.  -sb

;;;###autoload
(defun unoverstrike-region (start end)
  "Remove all overstriking (character-backspace-character) in the region.
Called from program, takes two arguments START and END which specify the
range to operate on."
  (interactive "*r")
  (save-excursion
    (let ((end1 (make-marker)))
      (move-marker end1 (max start end))
      (goto-char (min start end))
      (while (re-search-forward "\\(.\\)\b\\1" end1 t)
	(delete-char -2)))))

;;;###autoload
(defun overstrike-region (start end)
  "Overstrike (character-backspace-character) all nonblank characters in
the region. Called from program, takes two arguments START and END which
specify the range to operate on."
  (interactive "*r")
  (save-excursion
    (let ((end1 (make-marker)))
      (move-marker end1 (max start end))
      (goto-char (min start end))
      (while (< (point) end1)
	(or (looking-at "[_\^@- ]")
	    (insert (char-after (point)) 8))
	(forward-char 1)))))

;;;###autoload
(defun ununderline-and-unoverstrike-region (start end)
  "Remove underlining and overstriking in the region.  Called from a program,
takes two arguments START and END which specify the range to operate on."
  (interactive "*r")
  (save-excursion
    ;; This is a piece of nuke-nroff-bs from standard `man.el'.
    (goto-char (point-min))
    (while (search-forward "\b" (max start end) t)
      (let* ((preceding (char-after (- (point) 2)))
	     (following (following-char)))
	(cond ((= preceding following)
	       ;; x\bx
	       (delete-char -2))
	      ((= preceding ?\_)
	       ;; _\b
	       (delete-char -2))
	      ((= following ?\_)
	       ;; \b_
	       (delete-region (1- (point)) (1+ (point)))))))))

;;; underline.el ends here
