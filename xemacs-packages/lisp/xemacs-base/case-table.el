;;; case-table.el --- code to extend the character set and support case tables.

;; Copyright (C) 1988, 1993, 1997 Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: XEmacs Development Team
;; Keywords: i18n

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
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched with FSF.

;;; Commentary:

;; Written by:
;; TN/ETX/TX/UMG Howard Gayle        UUCP : seismo!enea!erix!howard
;; Telefonaktiebolaget L M Ericsson  Phone: +46 8 719 55 65
;; Ericsson Telecom     	     Telex: 14910 ERIC S
;; S-126 25 Stockholm                FAX  : +46 8 719 64 82
;; Sweden

;;; Code:

;;;###autoload
(defun describe-buffer-case-table ()
  "Describe the case table of the current buffer."
  (interactive)
  (let ((buffer (current-buffer))
	(i 0)
	ch)
    (with-displaying-help-buffer
     (lambda ()
       (set-buffer standard-output)
       (while (< i 256)
	 (setq ch (int-to-char i))
	 (cond ((eq (upcase ch) (downcase ch))
		nil)
	       ((eq ch (downcase ch buffer))
		(insert (text-char-description ch))
		(indent-to 16)
		(insert "uppercase, matches "
			(text-char-description (downcase ch))
			"\n"))
	       ((eq ch (upcase ch buffer))
		(insert (text-char-description ch))
		(indent-to 16)
		(insert "lowercase, matches "
			(text-char-description (upcase ch))
			"\n"))
	       ;;	      (t
	       ;;	       (insert (text-char-description ch))
	       ;;	       (indent-to 16)
	       ;;	       (insert "case-invariant\n"))
	       )
	 (setq i (1+ i)))))))

(defun invert-case (count)
  "Change the case of the character just after point and move over it.
With arg, applies to that many chars.
Negative arg inverts characters before point but does not move."
  (interactive "p")
  (if (< count 0)
      (progn (setq count (min (1- (point)) (- count)))
	     (forward-char (- count))))
  (while (> count 0)
    (let ((ch (char-after)))
      (cond ((/= (upcase ch) ch)
	     (insert (upcase ch))
	     (delete-char 1))
	    ((/= (downcase ch) ch)
	     (insert (downcase ch))
	     (delete-char 1))
	    (t
	     (forward-char 1))))
    (setq count (1- count))))

(defun set-case-syntax-delims (l r table)
  "Make characters L and R a matching pair of non-case-converting delimiters.
TABLE is a case-table object.
Sets the entries for L and R in TABLE,
standard-syntax-table, and text-mode-syntax-table to indicate
left and right delimiters."
  (if (functionp 'put-case-table-pair)
      (progn
	(put-case-table-pair l l table)
	(put-case-table-pair r r table))
    (aset (car table) l l)
    (aset (car table) r r))
  (modify-syntax-entry l (concat "(" (char-to-string r) "  ")
		       (standard-syntax-table))
  (modify-syntax-entry l (concat "(" (char-to-string r) "  ")
		       text-mode-syntax-table)
  (modify-syntax-entry r (concat ")" (char-to-string l) "  ")
		       (standard-syntax-table))
  (modify-syntax-entry r (concat ")" (char-to-string l) "  ")
		       text-mode-syntax-table))

(defun set-case-syntax-pair (uc lc table)
  "Make characters UC and LC a pair of inter-case-converting letters.
TABLE is a case-table object.
Sets the entries for characters UC and LC in
TABLE, standard-syntax-table, and
text-mode-syntax-table to indicate an (uppercase, lowercase)
pair of letters."
  (if (functionp 'put-case-table-pair)
      (put-case-table-pair uc lc table)
    (aset (car table) uc lc))
  (modify-syntax-entry lc "w   " (standard-syntax-table))
  (modify-syntax-entry lc "w   " text-mode-syntax-table)
  (modify-syntax-entry uc "w   " (standard-syntax-table))
  (modify-syntax-entry uc "w   " text-mode-syntax-table))

(defun set-case-syntax (c syntax table)
  "Make characters C case-invariant with syntax SYNTAX.
TABLE is a case-table object.
Sets the entries for character C in TABLE,
standard-syntax-table, and text-mode-syntax-table to indicate this.
SYNTAX should be \" \", \"w\", \".\" or \"_\"."
  (if (functionp 'put-case-table-pair)
      (put-case-table-pair c c table)
    (aset (car table) c c))
  (modify-syntax-entry c syntax (standard-syntax-table))
  (modify-syntax-entry c syntax text-mode-syntax-table))

(provide 'case-table)

;;; case-table.el ends here
