;;; winmgr-mode.el --- generic window manager mode

;; Author: ???
;; Maintainer: David Konerding (rafael@cse.ucsc.edu)
;; Modifications by: Stefan Strobel <strobel@lia.univ-savoie.fr>
;;                   Barry A. Warsaw <bwarsaw@python.org>
;; Created: ???
;; Keywords: languages

;; Copyright (C) 199? Someone Claim It

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This package is a major mode for editing window configuration files and
;; also defines font-lock keywords for such files.

;; winmgr-mode mode will automatically get turned on if you visit a
;; a file whose name looks like that of a configuration file
;; (IE, .fvwmrc, .mwmrc, .tvtwmrc, etc)

;; The current font-lock keywords are:

;; any word of upper or lower case letters at the start of a line
;; followed by whitespace gets colored using
;; font-lock-function-name-face.

;; any word of upper or lower case letters at the start of a line
;; followed by a "(" (IE, an m4 macro) gets colored using
;; font-lock-comment-face

;; Put this in your .emacs :
;;
;;(setq auto-mode-alist
;;      (append '(("\\.[A-Za-z]*wm$" . winmgr-mode)
;;              ("\\.[A-Za-z]*wmrc" . winmgr-mode) 
;;	      auto-mode-alist))
;;
;;(autoload 'winmgr-mode "winmgr-mode"
;;  "Mode for editing window manager config files")
;;
;;(add-hook 'winmgr-mode-hook
;;	  '(lambda ()
;;	     (font-lock-mode t)
;;	     (setq font-lock-keywords winmgr-font-lock-keywords)
;;           (font-lock-fontify-buffer)))
;;


;;; Code:

(defgroup winmgr nil
  "Generic window manager mode."
  :tag "Window Managers"
  :group 'languages)


(defcustom winmgr-basic-offset 4
  "*Number of spaces per indentation level."
  :type 'integer
  :group 'winmgr)

(defcustom winmgr-mode-hook nil
  "Hook to be run when `winmgr-mode' is entered."
  :type 'hook
  :group 'winmgr)


(defface font-lock-m4-face
  '((((class color))
     (:foreground "blue"))
    (t
     (:underline t)))
  "Font-lock face for M4 macros."
  :group 'winmgr)

(defvar winmgr-font-lock-keywords 
  '(("^[A-Za-z]+[ \n\t]" . font-lock-function-name-face)
    ;;("^#.*" . font-lock-comment-face)
    ("^[A-Za-z]+(.*)" . font-lock-m4-face))
  "Default font-lock keywords.")


;; major-mode stuff
(defvar winmgr-mode-abbrev-table nil
  "Abbrev table used in `winmgr-mode' buffers.")
(define-abbrev-table 'winmgr-mode-abbrev-table ())


(defvar winmgr-mode-syntax-table nil
  "Syntax table used in `winmgr-mode' buffers.")
(if winmgr-mode-syntax-table
    ()
  (setq winmgr-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\#  "<" winmgr-mode-syntax-table)
  (modify-syntax-entry ?\n ">" winmgr-mode-syntax-table))


(defvar winmgr-mode-map ()
  "Keymap used in `winmgr-mode' buffers.")
(if winmgr-mode-map
    ()
  (setq winmgr-mode-map (make-sparse-keymap))
  ;; So far there aren't any winmgr-mode specific functions
  )


;;;###autoload
(defun winmgr-mode ()
  "Major mode for editing winmgr config files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table winmgr-mode-syntax-table)
  (setq major-mode 'winmgr-mode
	mode-name "Winmgr"
	local-abbrev-table winmgr-mode-abbrev-table)
  (use-local-map winmgr-mode-map)
  ;; local variables
  (make-local-variable 'parse-sexp-ignore-comments)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'indent-line-function)
  ;; now set their values
  (setq parse-sexp-ignore-comments t
	comment-start "# "
	comment-end ""
	indent-line-function 'winmgr-indent-line-function)
  (run-hooks 'winmgr-mode-hook))


;; indentation commands

(defun winmgr-indent-line-function ()
  "Indent line based on depth in parentheses.
See the variable `winmgr-basic-offset.'"
  ;; find the beginning of this construct
  (let ((depth 0)
	(here (point)))
    (condition-case nil
	(while t
	  (backward-up-list 1)
	  (setq depth (1+ depth)))
      (error nil))
    (goto-char here)
    (beginning-of-line)
    (delete-horizontal-space)
    (insert-char ?\040 (* depth winmgr-basic-offset))))


;; XEmacs addition, Unix-specific
;;;###autoload(add-to-list 'auto-mode-alist '("\\.[^/]*wm2?\\(?:rc\\)?\\'" . winmgr-mode))

(provide 'winmgr-mode)

;;; winmgr-mode.el ends here
