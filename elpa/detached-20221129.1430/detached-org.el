;;; detached-org.el --- Detached integration for org -*- lexical-binding: t -*-

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

;; This package integrates `detached' with `org'.  In particular it
;; integrates with `ob-shell' in order to detach babel src blocks.

;;; Code:

;;;; Requirements

(require 'detached)
(require 'ob-shell)

;;;; Variables

(defcustom detached-org-session-action
  '(:attach detached-shell-command-attach-session
			:view detached-view-dwim
			:run detached-shell-command)
  "Actions for a session created with `detached-org'."
  :group 'detached
  :type 'plist)

;;;; Functions

;;;###autoload
(defun detached-org-babel-sh (org-babel-sh-evaluate-fun &rest args)
  "Modify ARGS before calling ORG-BABEL-SH-EVALUATE-FUN.

This function modifies the full-body in ARGS and replaces it with a
`detached' command.  The functionality is enabled by setting a header
property of :detached t in the org babel src block."
  (pcase-let* ((`(,session ,full-body ,params ,stdin ,cmdline) args))
	(if (alist-get :detached params)
		(cl-letf* ((detached-session-origin 'org)
				   (detached-session-action detached-org-session-action)
				   (detached-session-mode 'detached)
				   (new-command (replace-regexp-in-string "\n" " && " full-body))
                   (detached-session (detached-create-session new-command))
				   (dtach-command
					(if (string= "none" (alist-get :session params))
						(detached-session-start-command detached-session
                                                        :type 'string)
					  (format "%s\necho \"[detached]\"" (detached-session-start-command detached-session
                                                                                        :type 'string))))
				   ((symbol-function #'org-babel-eval)
					(lambda (_ command)
					  (start-file-process-shell-command "detached-org" nil command)
					  "[detached]")))
		  (apply org-babel-sh-evaluate-fun `(,session ,dtach-command ,params ,stdin ,cmdline)))
	  (apply org-babel-sh-evaluate-fun args))))

(provide 'detached-org)

;;; detached-org.el ends here
