;;; detached-vterm.el --- Detached integration with vterm -*- lexical-binding: t -*-

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

;; This package integrates `detached' with `vterm'.

;;; Code:

;;;; Requirements

(require 'detached)

(declare-function vterm-send-C-a "vterm")
(declare-function vterm-send-C-k "vterm")
(declare-function vterm-send-C-e "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-end-of-line "vterm")
(declare-function vterm-beginning-of-line "vterm")

(defvar vterm--process)

;;;; Variables

(defcustom detached-vterm-session-action
  '(:attach detached-shell-command-attach-session
			:view detached-view-dwim
			:run detached-shell-command)
  "Actions for a session created with `detached-vterm'."
  :group 'detached
  :type 'plist)

;;;; Commands

(defun detached-vterm-send-input (&optional detached)
  "Start a `detached-session' and attach to it, unless DETACHED."
  (interactive)
  (let* ((input (buffer-substring-no-properties (vterm-beginning-of-line) (vterm-end-of-line)))
		 (detached-session-origin 'vterm)
		 (detached-session-action detached-vterm-session-action)
		 (detached-session-mode
		  (if detached 'detached 'attached))
		 (detached-current-session (detached-create-session input))
		 (command (detached-session-start-command detached-current-session
                                                  :type 'string)))
	(vterm-send-C-a)
	(vterm-send-C-k)
	(process-send-string vterm--process command)
	(setq detached-buffer-session detached-current-session)
	(vterm-send-C-e)
	(vterm-send-return)))

(defun detached-vterm-attach (session)
  "Attach to an active `detached' SESSION."
  (interactive
   (list
    (let* ((host-name (car (detached--host)))
           (sessions
            (thread-last (detached-get-sessions)
                         (seq-filter (lambda (it)
                                       (string= (detached-session-host-name it) host-name)))
                         (seq-filter #'detached-session-active-p))))
      (detached-completing-read sessions))))
  (let ((command
         (detached-session-attach-command session
                                          :type 'string)))
	(setq detached-buffer-session session)
	(process-send-string vterm--process command)
	(vterm-send-return)))

(cl-defmethod detached--detach-session ((_mode (derived-mode vterm-mode)))
  "Detach from session when MODE is `vterm-mode'."
  (process-send-string
   vterm--process
   detached--dtach-detach-character))

;;;; Minor mode

(defvar detached-vterm-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<S-return>") #'detached-vterm-send-input)
	(define-key map (kbd "<C-return>") #'detached-vterm-attach)
	(define-key map (kbd "C-c C-.") #'detached-describe-session)
	(define-key map (kbd detached-detach-key) #'detached-detach-session)
	map)
  "Keymap for `detached-vterm-mode'.")

;;;###autoload
(define-minor-mode detached-vterm-mode
  "Integrate `detached' in `vterm'."
  :lighter " detached-vterm"
  :keymap (let ((map (make-sparse-keymap)))
			map))

(provide 'detached-vterm)

;;; detached-vterm.el ends here
