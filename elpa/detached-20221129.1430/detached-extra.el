;;; detached-extra.el --- Detached integration for external packages -*- lexical-binding: t -*-

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

;; This package provides a collection of functionality to integrate `detached' with external packages.

;;; Code:

;;;; Requirements

(declare-function detached-compile "detached")
(declare-function detached-start-session "detached")
(declare-function detached-session-command "detached")

(declare-function alert "alert")

(defvar detached-session-origin)
(defvar detached-local-session)

;;;; Functions

;;;###autoload
(defun detached-extra-projectile-run-compilation (cmd &optional use-comint-mode)
  "If CMD is a string execute it with `detached-compile'.

Optionally USE-COMINT-MODE"
  (if (functionp cmd)
	  (funcall cmd)
	(let ((detached-session-origin 'projectile))
	  (detached-compile cmd use-comint-mode))))

;;;###autoload
(defun detached-extra-dired-rsync (command _details)
  "Run COMMAND with `detached'."
  (let* ((detached-local-session t)
		 (detached-session-origin 'rsync)
         (detached-session-mode 'detached)
         (session (detached-create-session command)))
	(detached-start-session session)))

;;;###autoload
(defun detached-extra-alert-notification (session)
  "Send an `alert' notification when SESSION becomes inactive."
  (let ((status (detached-session-status session))
		(host (detached-session-host-name session)))
	(alert (detached-session-command session)
		   :title (pcase status
					('success (format "Detached finished [%s]" host))
					('failure (format "Detached failed [%s]" host)))
		   :severity (pcase status
					   ('success 'moderate)
					   ('failure 'high)))))

(provide 'detached-extra)

;;; detached-extra.el ends here
