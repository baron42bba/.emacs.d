;;; detached-eshell.el --- Detached integration for eshell -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

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

;; This is a `detached' extension which provides integration for `eshell'.

;;; Code:

;;;; Requirements

(require 'detached)
(require 'eshell)
(require 'esh-mode)
(require 'esh-ext)
(require 'em-hist)

;;;; Variables

(defcustom detached-eshell-session-action
  '(:attach detached-shell-command-attach-session
			:view detached-view-dwim
			:run detached-start-shell-command-session)
  "Actions for a session created with `detached-eshell'."
  :group 'detached
  :type 'plist)

;;;; Functions

;;;###autoload
(defun detached-eshell-external-command (orig-fun &rest args)
  "Advice ORIG-FUN to optionally use `detached' on ARGS."
  (let* ((detached-session-action detached-eshell-session-action)
		 (command (string-trim-right (string-join (flatten-list args) " ")))
		 (session (detached-create-session command))
		 (command
          (detached-session-start-command session
                                          :type 'list)))
	(advice-remove #'eshell-external-command #'detached-eshell-external-command)
	(setq detached-buffer-session session)
	(setq detached-enabled nil)
	(apply orig-fun `(,(seq-first command) ,(seq-rest command)))))

;;;; Commands

(defun detached-eshell-send-input (&optional detached)
  "Start a `detached-session' and attach to it, unless DETACHED."
  (interactive "P")
  (let* ((detached-session-origin 'eshell)
		 (detached-session-mode (if detached 'detached 'attached))
		 (detached-enabled t)
		 (detached-current-session nil))
	(advice-add #'eshell-external-command :around #'detached-eshell-external-command)
	(call-interactively #'eshell-send-input)))

(defun detached-eshell-attach-session (session)
  "Attach to SESSION."
  (interactive
   (list (detached-select-host-session)))
  (when (detached-valid-session session)
    (if (detached-session-active-p session)
        (cl-letf* ((input
                    (detached-session-attach-command session
                                                     :type 'string))
                   ((symbol-function #'eshell-add-to-history) #'ignore))
          (let ((kill-ring nil))
            (eshell-kill-input))
          ;; Hide the input from the user
          (let ((begin (point))
                (end))
            (insert input)
            (setq end (point))
            (overlay-put (make-overlay begin end) 'invisible t)
            (overlay-put (make-overlay end end) 'before-string "[attached]")
            (insert " "))
          (setq detached-buffer-session session)
          (call-interactively #'eshell-send-input))
      (detached-open-session session))))

;;;; Support functions

(defun detached-eshell--get-dtach-process ()
  "Return `eshell' process if `detached' is running."
  (when-let* ((process (and eshell-process-list (caar eshell-process-list))))
    (and (string= (process-name process) "dtach")
         process)))

(cl-defmethod detached--detach-session ((_mode (derived-mode eshell-mode)))
  "Detach from session when MODE is `eshell-mode'."
  (when-let ((active-session (detached-session-active-p
                              (alist-get (detached-session-id detached-buffer-session)
                                         detached--sessions)))
             (dtach-process (detached-eshell--get-dtach-process)))
    (setq detached-buffer-session nil)
    (process-send-string dtach-process
                         detached--dtach-detach-character)))

;;;; Minor mode

(defvar detached-eshell-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<S-return>") #'detached-eshell-send-input)
	(define-key map (kbd "<C-return>") #'detached-eshell-attach-session)
	(define-key map (kbd "C-c C-.") #'detached-describe-session)
	(define-key map (kbd detached-detach-key) #'detached-detach-session)
	map)
  "Keymap for `detached-eshell-mode'.")

;;;###autoload
(define-minor-mode detached-eshell-mode
  "Integrate `detached' in `eshell-mode'."
  :lighter " detached-eshell"
  :keymap (let ((map (make-sparse-keymap)))
			map)
  (make-local-variable 'eshell-preoutput-filter-functions)
  (if detached-eshell-mode
	  (progn
		(add-hook 'eshell-preoutput-filter-functions #'detached--env-message-filter)
		(add-hook 'eshell-preoutput-filter-functions #'detached--dtach-eof-message-filter))
	(remove-hook 'eshell-preoutput-filter-functions #'detached--env-message-filter)
	(remove-hook 'eshell-preoutput-filter-functions #'detached--dtach-eof-message-filter)))

(provide 'detached-eshell)

;;; detached-eshell.el ends here
