;;; detached-init.el --- Initialize detached -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;; This is intended to aid users in configuring `detached's integration with other packages.

;;; Code:

;;;; Requirements

(require 'subr-x)

(declare-function detached-attach-session "detached")
(declare-function detached-compile-session "detached")
(declare-function detached-delete-session "detached")
(declare-function detached-insert-session-command "detached")
(declare-function detached-open-session-directory "detached")
(declare-function detached-kill-session "detached")
(declare-function detached-rerun-session "detached")
(declare-function detached-view-session "detached")
(declare-function detached-copy-session-command "detached")
(declare-function detached-copy-session-output "detached")
(declare-function detached-diff-session "detached")
(declare-function detached-initialize-sessions "detached")
(declare-function detached-shell-mode "detached")

(declare-function detached-compile--start "detached-compile")
(declare-function detached-dired-do-shell-command "detached-dired")
(declare-function detached-eshell-mode "detached-eshell")
(declare-function detached-extra-projectile-run-compilation "detached-extra")
(declare-function detached-extra-dired-rsync "detached-extra")
(declare-function detached-list--db-update "detached-list")
(declare-function detached-list--apply-filter "detached-list")
(declare-function detached-org-babel-sh "detached-org")
(declare-function detached-shell-override-history "detached-shell")
(declare-function detached-shell-save-history-on-kill "detached-shell")
(declare-function detached-vterm-mode "detached-vterm")

(declare-function org-babel-sh-evaluate "ob-shell")
(declare-function dired-rsync--do-run "dired-rsync")
(declare-function dired-rsync "dired-rsync")
(declare-function projectile "projectile")
(declare-function vterm "vterm")

(defvar detached-list-filters)

(defvar embark-general-map)
(defvar embark-keymap-alist)
(defvar nano-modeline-mode-formats)

;;;; Variables

(defcustom detached-init-block-list nil
  "A list of blocked packages."
  :group 'detached
  :type 'list)

(defcustom detached-init-allow-list
  '(compile dired dired-rsync embark eshell nano-modeline org projectile shell vterm)
  "A list of allowed packages."
  :group 'detached
  :type 'list)

(defvar detached-embark-action-map
  (let ((map (make-sparse-keymap)))
	(define-key map "a" #'detached-attach-session)
	(define-key map "c" #'detached-compile-session)
	(define-key map "d" #'detached-delete-session)
	(define-key map "i" #'detached-insert-session-command)
	(define-key map "f" #'detached-open-session-directory)
	(define-key map "k" #'detached-kill-session)
	(define-key map "r" #'detached-rerun-session)
	(define-key map "v" #'detached-view-session)
	(define-key map "w" #'detached-copy-session-command)
	(define-key map "W" #'detached-copy-session-output)
	(define-key map "=" #'detached-diff-session)
	map))

(defvar detached-init-package-integration '((compile . detached-init--compile)
											(dired . detached-init--dired)
											(dired-rsync . detached-init--dired-rsync)
											(embark . detached-init--embark)
											(eshell . detached-init--eshell)
											(nano-modeline . detached-init--nano-modeline)
											(org . detached-init--org)
											(projectile . detached-init--projectile)
											(shell . detached-init--shell)
											(vterm . detached-init--vterm))
  "Alist which contain names of packages and their initialization function.")

;;;; Functions

;;;###autoload
(defun detached-init ()
  "Initialize `detached' integration with all packages."
  (detached-init--detached)
  (detached-init--detached-list)
  (let ((init-functions
		 (thread-last detached-init-package-integration
					  (seq-filter (lambda (it)
									(member (car it) detached-init-allow-list)))
					  (seq-remove (lambda (it)
									(member (car it) detached-init-block-list)))
					  (seq-map #'cdr))))
	(dolist (init-function init-functions)
	  (funcall init-function))))

;;;; Support functions

(defun detached-init--shell ()
  "Initialize integration with `shell'."
  (advice-add 'shell :around #'detached-shell-override-history)
  (add-hook 'shell-mode-hook #'detached-shell-save-history-on-kill))

(defun detached-init--compile ()
  "Initialize integration with `compile'."
  (add-hook 'compilation-start-hook #'detached-compile--start)
  (add-hook 'compilation-shell-minor-mode-hook #'detached-shell-mode))

(defun detached-init--eshell ()
  "Initialize integration with `eshell'."
  (add-hook 'eshell-mode-hook #'detached-eshell-mode))

(defun detached-init--org ()
  "Initialize integration with `org'."
  (advice-add 'org-babel-sh-evaluate
			  :around #'detached-org-babel-sh))

(defun detached-init--dired ()
  "Initialize integration with `dired'."
  (advice-add 'dired-do-shell-command
			  :around #'detached-dired-do-shell-command))

(defun detached-init--dired-rsync ()
  "Initialize integration with `dired-rsync'."
  (advice-add 'dired-rsync--do-run
			  :override #'detached-extra-dired-rsync))

(defun detached-init--projectile ()
  "Initialize integration with `projectile'."
  (advice-add 'projectile-run-compilation
			  :override #'detached-extra-projectile-run-compilation))

(defun detached-init--vterm ()
  "Initialize integration with `vterm'."
  (with-eval-after-load 'vterm
	(add-hook 'vterm-mode-hook #'detached-vterm-mode)))

(defun detached-init--embark ()
  "Initialize integration with `embark'."
  (with-eval-after-load 'embark
	(defvar embark-detached-map (make-composed-keymap detached-embark-action-map embark-general-map))
	(add-to-list 'embark-keymap-alist '(detached . embark-detached-map))))

(defun detached-init--nano-modeline ()
  "Initialize integration with `nano-modeline'."
  (with-eval-after-load 'nano-modeline
	(push `(detached-list-mode
			:mode-p (lambda () (derived-mode-p 'detached-list-mode))
			:format (lambda () (nano-modeline-render nil (detached-list--mode-line-indicator) "" "")))
		  nano-modeline-mode-formats)))

(defun detached-init--detached-list ()
  "Initialize `detached-list'."
  ;; Trigger initialization of sessions upon load of `detached-list'
  (with-eval-after-load 'detached-list
	(detached-list--apply-filter
	 (cdr (car detached-list-filters)))
	(add-hook 'detached-update-db-hooks #'detached-list--db-update)))

(defun detached-init--detached ()
  "Initialize `detached'."
  ;; Trigger initialization of sessions upon load of `detached'
  (with-eval-after-load 'detached
	(detached-initialize-sessions))
  ;; Required for `detached-shell-command' which is always provided
  (add-hook 'shell-mode-hook #'detached-shell-mode))

(provide 'detached-init)

;;; detached-init.el ends here
