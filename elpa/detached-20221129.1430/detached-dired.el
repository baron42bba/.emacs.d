;;; detached-dired.el --- Detached integration for dired -*- lexical-binding: t -*-

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

;; This package integrates `detached' with `dired'.

;;; Code:

;;;; Requirements

(require 'dired)
(require 'detached)

;;;; Functions

;;;###autoload
(defun detached-dired-do-shell-command (dired-do-shell-command &rest args)
  "Ensure `detached' is used before running DIRED-DO-SHELL-COMMAND with ARGS."
  (cl-letf* ((detached-session-origin 'dired)
			 ((symbol-function #'dired-run-shell-command)
			  (lambda (command)
                (let ((session (detached-create-session command)))
                  (detached-start-shell-command-session session))
				nil)))
	(pcase-let* ((`(,command ,arg ,file-list) args)
				 (modified-args `(,(string-remove-suffix " &" command) ,arg ,file-list)))
	  (apply dired-do-shell-command modified-args))))

(provide 'detached-dired)

;;; detached-dired.el ends here
