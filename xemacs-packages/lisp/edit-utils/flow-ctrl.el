;;; flow-ctrl.el --- help for lusers on cu(1) or ttys with wired-in ^S/^Q flow control

;;; Copyright (C) 1990, 1991, 1994 Free Software Foundation, Inc.

;; Author Kevin Gallagher
;; Maintainer: FSF
;; Adapted-By: ESR
;; Keywords: hardware

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

;; Terminals that use XON/XOFF flow control can cause problems with
;; GNU Emacs users.  This file contains Emacs Lisp code that makes it
;; easy for a user to deal with this problem, when using such a
;; terminal. 
;;      
;; To invoke these adjustments, a user need only invoke the function
;; enable-flow-control-on with a list of terminal types in his/her own
;; .emacs file.  As arguments, give it the names of one or more terminal
;; types in use by that user which require flow control adjustments.
;; Here's an example: 
;; 
;;	(enable-flow-control-on "vt200" "vt300" "vt101" "vt131")

;; Portability note: This uses (getenv "TERM"), and therefore probably
;; won't work outside of UNIX-like environments.

;;; Code:

(defvar flow-control-c-s-replacement ?\034
  "Character that replaces C-s, when flow control handling is enabled.")
(defvar flow-control-c-q-replacement ?\036
  "Character that replaces C-q, when flow control handling is enabled.")

;(put 'keyboard-translate-table 'char-table-extra-slots 0)

;;;###autoload
(defun enable-flow-control (&optional argument)
  "Toggle flow control handling.
When handling is enabled, user can type C-s as C-\\, and C-q as C-^.
With arg, enable flow control mode if arg is positive, otherwise disable."
  (interactive "P")
  (if (if argument
	  ;; Argument means enable if arg is positive.
	  (<= (prefix-numeric-value argument) 0)
	;; No arg means toggle.
	(nth 1 (current-input-mode)))
      (progn
	;; Turn flow control off, and stop exchanging chars.
	(set-input-mode t nil (nth 2 (current-input-mode)))
	;; XEmacs
	(keyboard-translate flow-control-c-s-replacement nil)
	(keyboard-translate ?\^s nil)
	(keyboard-translate flow-control-c-q-replacement nil)
	(keyboard-translate ?\^q nil))
    ;; Turn flow control on.
    ;; Tell emacs to pass C-s and C-q to OS.
    (set-input-mode nil t (nth 2 (current-input-mode)))
    ;; Initialize translate table, saving previous mappings, if any.
    ;; Swap C-s and C-\
    ;; XEmacs
    (keyboard-translate flow-control-c-s-replacement ?\^s)
    (keyboard-translate ?\^s flow-control-c-s-replacement)
    ;; Swap C-q and C-^
    (keyboard-translate flow-control-c-q-replacement ?\^q)
    (keyboard-translate ?\^q flow-control-c-q-replacement)
    (message (concat 
	      "XON/XOFF adjustment for " 
	      (getenv "TERM") 
	      ":  use C-\\ for C-s  and  use C-^ for C-q."))
    (sleep-for 2)))			; Give user a chance to see message.

;;;###autoload
(defun enable-flow-control-on (&rest losing-terminal-types)
  "Enable flow control if using one of a specified set of terminal types.
Use `(enable-flow-control-on \"vt100\" \"h19\")' to enable flow control
on VT-100 and H19 terminals.  When flow control is enabled,
you must type C-\\ to get the effect of a C-s, and type C-^
to get the effect of a C-q.

This function has no effect unless the current device is a tty.

The tty terminal type is determined from the TERM environment variable.
Trailing hyphens and everything following is stripped, so a TERM
value of \"vt100-nam\" is treated the same as \"vt100\"."
  (let ((term (getenv "TERM"))
	hyphend)
    ;; Look for TERM in LOSING-TERMINAL-TYPES.
    ;; If we don't find it literally, try stripping off words
    ;; from the end, one by one.
    (while (and term (not (member term losing-terminal-types)))
      ;; Strip off last hyphen and what follows, then try again.
      (if (setq hyphend (string-match "[-_][^-_]+$" term))
	  (setq term (substring term 0 hyphend))
	(setq term nil)))
    (if term
	(enable-flow-control))))

(provide 'flow-ctrl)

;;; flow-ctrl.el ends here
