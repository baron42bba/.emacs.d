;;; amburn-theme.el --- A high contrast amber theme -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Excalamus

;; Author: Excalamus <matt@excalamus.com>
;; URL: http://github.com/excalamus/amburn-theme.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; People talk about how "old" Emacs looks. I say ignore the agists
;; and embrace our long legacy.  The future of Emacs glows as brightly
;; now as the phosphor screens of yore.  Speaking of which, Emacs
;; didn't have an amber theme until now.

;;; Credits:

;; A tip of the hat to the plain-theme.el made by Yana Timoshenko and
;; others who showed us how to not repeat ourselves.

;;; Code:

(require 'cl-lib)

(deftheme amburn "A high contrast amber theme.")

(defgroup amburn-theme nil
  "A high contrast amber theme."
  :group 'faces
  :prefix "amburn-")


;; colors

(defcustom amburn-fgl "#fcc000"
  "Foreground light."
  :type 'color)

(defcustom amburn-fgd "#fbb000"
  "Foreground dark"
  :type 'color)

(defcustom amburn-bg "#282828"
  "Background."
  :type 'color)


;; styles

(defun amburn--style-fgl-on-bg (face)
  "Return spec for FACE."
  `(,face ((t (:background ,amburn-bg :foreground ,amburn-fgl)))))

(defun amburn--style-fgd-on-bg (face)
  "Return spec for FACE."
  `(,face ((t (:background ,amburn-bg :foreground ,amburn-fgd)))))

(defun amburn--style-bg-on-fgl (face)
  "Return spec for FACE."
  `(,face ((t (:background ,amburn-fgl :foreground ,amburn-bg)))))

(defun amburn--style-bg-on-fgd (face)
  "Return spec for FACE."
  `(,face ((t (:background ,amburn-fgd :foreground ,amburn-bg)))))


(defun amburn--set (style faces)
  "Set a STYLE to a list of FACES.
  
A style is one of the `amburn--style-*' functions."
  (let ((custom--inhibit-theme-enable nil)) ; apply faces immediately
    (apply 'custom-theme-set-faces 'amburn (mapcar style faces))))


;; color everything as a baseline
(amburn--set 'amburn--style-fgd-on-bg (face-list))


;; bulk faces by namespace

(defcustom amburn-bulk-fgl-on-bg '()
  "Mapping from files to face prefixes: when file is first loaded,
colorizes every face that starts with the prefix."
  :type '(alist :key-type symbol :value-type string))

(defcustom amburn-bulk-fgd-on-bg '((font-lock . "font-lock-") (helm . "helm-"))
  "Mapping from files to face prefixes: when file is first loaded,
colorizes every face that starts with the prefix."
  :type '(alist :key-type symbol :value-type string))

(defcustom amburn-bulk-bg-on-fgd '()
  "Mapping from files to face prefixes: when file is first loaded,
colorizes every face that starts with the prefix."
  :type '(alist :key-type symbol :value-type string))

(defcustom amburn-bulk-bg-on-fgl '()
  "Mapping from files to face prefixes: when file is first loaded,
colorizes every face that starts with the prefix."
  :type '(alist :key-type symbol :value-type string))

(defun amburn--namespace-face-list (namespace)
  "Return all faces that start with NAMESPACE string."
  (cl-remove-if-not (lambda (s) (string-prefix-p namespace (symbol-name s)))
                    (face-list)))

(dolist (a amburn-bulk-fgl-on-bg)
  (eval-after-load (car a)
    '(amburn--set 'amburn--style-fgl-on-bg (amburn--namespace-face-list (cdr a)))))

(dolist (a amburn-bulk-fgd-on-bg)
  (eval-after-load (car a)
    '(amburn--set 'amburn--style-fgd-on-bg (amburn--namespace-face-list (cdr a)))))

(dolist (a amburn-bulk-bg-on-fgl)
  (eval-after-load (car a)
    '(amburn--set 'amburn--style-bg-on-fgl (amburn--namespace-face-list (cdr a)))))

(dolist (a amburn-bulk-bg-on-fgd)
  (eval-after-load (car a)
    '(amburn--set 'amburn--style-bg-on-fgd (amburn--namespace-face-list (cdr a)))))


;; individual faces
;; set second to override the bulk

(defcustom amburn-individual-fgl-on-bg '(hl-line link link-visited)
    "List of faces to colorize."
    :type '(repeat symbol))

(defcustom amburn-individual-fgd-on-bg '(border default fringe gui-element header-line minibuffer-prompt mode-line-inactive shadow vertical-border widget-button widget-field)
    "List of faces to colorize."
    :type '(repeat symbol))

(defcustom amburn-individual-bg-on-fgl '(error isearch secondary-selection success warning)
    "List of faces to colorize."
    :type '(repeat symbol))

(defcustom amburn-individual-bg-on-fgd '(cursor highlight mode-line region show-paren-match show-paren-match-expression show-paren-mismatch trailing-whitespace)
  "List of faces to colorize."
  :type '(repeat symbol))

(amburn--set 'amburn--style-fgd-on-bg amburn-individual-fgd-on-bg)
(amburn--set 'amburn--style-fgl-on-bg amburn-individual-fgl-on-bg)
(amburn--set 'amburn--style-bg-on-fgd amburn-individual-bg-on-fgd)
(amburn--set 'amburn--style-bg-on-fgl amburn-individual-bg-on-fgl)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'amburn)

;;; amburn-theme.el ends here
