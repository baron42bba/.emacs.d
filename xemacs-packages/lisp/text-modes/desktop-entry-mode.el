;;; desktop-entry-mode.el --- freedesktop.org desktop entry editing

;; Copyright (C) 2003-2004, Ville Skytt�, <scop at xemacs.org>

;; Author:   Ville Skytt�, <scop at xemacs.org>
;; Keywords: unix, desktop entry

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

;;; Commentary:

;; This mode provides basic functionality, eg. syntax highlighting and
;; validation for freedesktop.org desktop entry files.
;;
;; To install it:
;;
;;   In XEmacs:
;;   Just install the XEmacs `text-modes' package, this mode is included.
;;   See <http://www.xemacs.org/Documentation/packageGuide.html>.
;;
;;   In GNU Emacs:
;;   Place this file in your load path somewhere (eg. site-lisp), and add
;;   the following to your .emacs:
;;
;;   (autoload 'desktop-entry-mode "desktop-entry-mode" "Desktop Entry mode" t)
;;   (add-to-list 'auto-mode-alist
;;                '("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode))
;;   (add-hook 'desktop-entry-mode-hook 'turn-on-font-lock)
;;
;; For more information about desktop entry files, see
;;   <http://www.freedesktop.org/Standards/desktop-entry-spec>
;;
;; This version is up to date with version 0.9.4 of the specification.

;;; Code:

(defconst desktop-entry-mode-version "0.94 (spec 0.9.4)"
  "Version of `desktop-entry-mode'.")

(defgroup desktop-entry nil
  "Support for editing freedesktop.org desktop entry files."
  :group 'languages)

(defcustom desktop-entry-validate-command "desktop-file-validate"
  "*Command for validating desktop entry files."
  :type 'string
  :group 'desktop-entry)

(defgroup desktop-entry-faces nil
  "Font lock faces for `desktop-entry-mode'."
  :prefix "desktop-entry-"
  :group 'desktop-entry
  :group 'faces)

(defface desktop-entry-group-header-face
  '((((class color) (background light)) (:foreground "mediumblue" :bold t))
    (((class color) (background dark)) (:foreground "lightblue" :bold t))
    (t (:bold t)))
  "*Face for highlighting desktop entry group headers."
  :group 'desktop-entry-faces)

(defface desktop-entry-value-face
  '((((class color) (background light)) (:foreground "darkgreen"))
    (((class color) (background dark)) (:foreground "lightgreen"))
    )
  "*Face for highlighting desktop entry values."
  :group 'desktop-entry-faces)

(defface desktop-entry-locale-face
  '((((class color) (background light)) (:foreground "dimgray"))
    (((class color) (background dark)) (:foreground "lightgray"))
    )
  "*Face for highlighting desktop entry locales."
  :group 'desktop-entry-faces)

(defconst desktop-entry-keywords
  (eval-when-compile
    (require 'regexp-opt)
    (concat
     "\\(?:"
     (regexp-opt
      '(
        "Type"
        "Version"
        "Encoding"
        "Name"
        "GenericName"
        "NoDisplay"
        "Comment"
        "Icon"
        "Hidden"
        "FilePattern"
        "TryExec"
        "Exec"
        "Path"
        "Terminal"
        "SwallowTitle"
        "SwallowExec"
        "Actions"
        "MimeType"
        "SortOrder"
        "Dev"
        "FSType"
        "MountPoint"
        "ReadOnly"
        "UnmountIcon"
        "URL"
        "Categories"
        "OnlyShowIn"
        "NotShowIn"
        "StartupNotify"
        "StartupWMClass"
        ;; Reserved for use with KDE as of spec 0.9.4.
        "ServiceTypes"
        "DocPath"
        "KeyWords"
        "InitialPreference"
        ) 'words)
     "\\|X-[A-Za-z0-9-]+\\)"))
  "Expression for matching desktop entry keys.")

(defconst desktop-entry-group-header-re
  "^\\[\\(X-[^\][]+\\|\\(?:Desktop \\(?:Entry\\|Action [a-zA-Z]+\\)\\)\\)\\]"
  "Regular expression for matching desktop entry group headers.")

(defconst desktop-entry-font-lock-keywords
  (list
   (cons "^\\s-*#.*$" '(0 'font-lock-comment-face))
   (cons (concat "^" desktop-entry-keywords) '(0 'font-lock-keyword-face))
   (cons desktop-entry-group-header-re '(1 'desktop-entry-group-header-face))
   (cons "^[A-Za-z0-9-]+?\\s-*=\\s-*\\(.*\\)"
         '(1 'desktop-entry-value-face))
   (cons "^[A-Za-z0-9-]+?\\[\\([^\]]+\\)\\]\\s-*=\\s-*\\(.*\\)"
         '((1 'desktop-entry-locale-face)
           (2 'desktop-entry-value-face)))
   )
  "Highlighting rules for `desktop-entry-mode' buffers.")

(defvar desktop-entry-imenu-generic-expression
  `((nil ,desktop-entry-group-header-re 1))
  "Imenu generic expression for `desktop-entry-mode'.
See `imenu-generic-expression'.")

;;;###autoload
(defun desktop-entry-mode ()
  "Major mode for editing freedesktop.org desktop entry files.
See <http://www.freedesktop.org/Standards/desktop-entry-spec> for more
information.  See `desktop-entry-mode-version' for information about which
version of the specification this mode is up to date with.

Turning on desktop entry mode calls the value of the variable
`desktop-entry-mode-hook' with no args, if that value is non-nil."
  (interactive)
  (set (make-local-variable 'imenu-generic-expression)
       '((nil "^\\s-*\\(.*\\)\\s-*=" 1)))
  (set (make-local-variable 'compile-command)
       (concat desktop-entry-validate-command " " buffer-file-name))
  (set (make-local-variable 'compilation-buffer-name-function)
       (lambda (x) (concat "*desktop-file-validate "
                           (file-name-nondirectory buffer-file-name) "*")))
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (setq major-mode 'desktop-entry-mode mode-name "Desktop Entry")
  (set (make-local-variable 'imenu-generic-expression)
       desktop-entry-imenu-generic-expression)
  (unless (featurep 'xemacs) ;; font-lock support for GNU Emacs
    (set (make-local-variable 'font-lock-defaults)
         '(desktop-entry-font-lock-keywords)))
  (run-hooks 'desktop-entry-mode-hook))

(defun desktop-entry-validate ()
  "Validate desktop entry in the current buffer."
  (interactive)
  (require 'compile)
  (compile compile-command))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode))

(provide 'desktop-entry-mode)

;;; desktop-entry-mode.el ends here
