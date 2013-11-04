;;; -*- Emacs-Lisp -*-
;;; vertical-mode.el --- Vertical mode for emacs

;; Copyright (C) 1998-2000 Pavel Machek <pavel@ucw.cz>
;;
;; Author: Pavel Machek <pavel@ucw.cz>
;; Version: $Id: vertical-mode.el,v 1.2 2002/03/24 20:52:50 youngs Exp $
;; Keywords: 
;; Requirements: 

;; This file is part of XEmacs

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; XEmacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

; This minor mode allows you to conviently edit things that are oriented vertically
; (like tables in computer programs): after each action, cursor moves down. Therefore,
; to move block of text to the right, you simply enter vertical mode and then hold
; spacebar, waiting for autorepeat to do the job for you.

;;; Code:

(defun vertical-after-change (from to num)
  "Function called after each change of text in vertical minor mode"
  (goto-char vertical-goto-point)
  (next-line 1))

(defun vertical-before-change (from to)
  (setq vertical-goto-point from)
  (setq vertical-goto-column (current-column)))

(defvar vertical-mode nil
   "Vertical mode")

(make-variable-buffer-local 'vertical-goto-point)
(make-variable-buffer-local 'vertical-goto-column)
(make-variable-buffer-local 'vertical-mode)

;;;###autoload
(defun vertical-mode (&optional arg)
   "This function toggles vertical mode on and off."
   (interactive)
   (setq vertical-mode 
      (if (null arg) (not vertical-mode)
         (> (prefix-numeric-value arg) 0)))
   (force-mode-line-update)
   (make-local-hook 'before-change-functions)
   (make-local-hook 'after-change-functions)
   (if vertical-mode
       (progn
	 (add-hook 'before-change-functions 'vertical-before-change nil t)
	 (add-hook 'after-change-functions 'vertical-after-change nil t))
       (progn
	 (remove-hook 'before-change-functions 'vertical-before-change t)
	 (remove-hook 'after-change-functions 'vertical-after-change t))))

(setq 
 minor-mode-alist (cons 
		   '(vertical-mode " Vertical") minor-mode-alist))

(provide 'vertical-mode)
;;; vertical-mode.el ends here
