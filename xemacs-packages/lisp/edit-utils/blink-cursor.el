;;; blink-cursor.el --- Blink the cursor on or off

;; Copyright (C) 1996 Ben Wing.

;; Keywords: display

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;;; Code:

(defvar blink-cursor-last-selected-window nil)
(defvar blink-cursor-lost-focus nil)

(defun blink-cursor-callback (foo)
  (let ((inhibit-quit t)
	(window (selected-window)))
    ;; Blinking cursor just doesn't cut it on TTY-s.  Rather give it up.
    (if (or (eq (console-type) 'tty)
	    blink-cursor-lost-focus)
	nil
      (or blink-cursor-last-selected-window
	  (setq blink-cursor-last-selected-window window))
      (if (eq window blink-cursor-last-selected-window)

	  (if (specifier-instance text-cursor-visible-p window)
	      (if (let ((current-time (current-time)))
                  (or (not last-input-time)
                      (> (car current-time) (car last-input-time))
			(> (cadr current-time) (cdr last-input-time))))
		  ;; turn cursor off only if more than a second since
		  ;; last input
		  (set-specifier text-cursor-visible-p nil window))
	    (set-specifier text-cursor-visible-p t window))

	(remove-specifier text-cursor-visible-p
			  blink-cursor-last-selected-window)
	(setq blink-cursor-last-selected-window window)
	(set-specifier text-cursor-visible-p nil window)))))

; Turn on cursor after every command
(defun blink-cursor-post-command-hook ()
  (let ((inhibit-quit t)
	(window (selected-window)))
    (if blink-cursor-lost-focus
	nil
      (set-specifier text-cursor-visible-p t window))))

(defun blink-cursor-reenable-cursor ()
  (if blink-cursor-last-selected-window
      (progn
	(remove-specifier text-cursor-visible-p
			  blink-cursor-last-selected-window)
	(setq blink-cursor-last-selected-window nil))))

(defun blink-cursor-deselect-frame-hook ()
  (blink-cursor-reenable-cursor)
  (setq blink-cursor-lost-focus t))

(defun blink-cursor-select-frame-hook ()
  (setq blink-cursor-lost-focus nil))

(add-hook 'deselect-frame-hook 'blink-cursor-deselect-frame-hook)
(add-hook 'select-frame-hook 'blink-cursor-select-frame-hook)
(add-hook 'post-command-hook 'blink-cursor-post-command-hook)

(defvar blink-cursor-timeout 1.0)
(defvar blink-cursor-timeout-id nil)

;; so that the options menu can save it - dverna apr. 98
(defgroup blink-cursor nil
  "Blinking cursor mode")

;;;###autoload
(defcustom blink-cursor-mode nil
  "Non nil means `blink-cursor-mode' is on. Normally, you shouldn't modify
this variable by hand, but use the function `blink-cursor-mode'
instead. However, the default value can be customized from the options menu."
  :type 'boolean
  :set (lambda (var val)
	 (blink-cursor-mode (or val 0)))
  :initialize 'custom-initialize-default
  :require 'blink-cursor
  :group 'blink-cursor)

;;;###autoload
(defun blink-cursor-mode (&optional timeout)
  "Enable or disable a blinking cursor.
If TIMEOUT is nil, toggle on or off.
If TIMEOUT is t, enable with the previous timeout value.
If TIMEOUT is 0, disable.
If TIMEOUT is greater than 0, then the cursor will blink once
each TIMEOUT secs (can be a float)."
  (interactive)
  (cond ((not timeout)
	 (setq timeout blink-cursor-timeout)
	 (setq blink-cursor-mode (not blink-cursor-mode)))
	((eq timeout t)
	 (setq timeout blink-cursor-timeout)
	 (setq blink-cursor-mode t))
	((<= timeout 0)
	 (setq blink-cursor-mode nil))
	(t
	 (setq blink-cursor-timeout timeout)
	 (setq blink-cursor-mode t)))
  (if blink-cursor-timeout-id
      (progn
	(disable-timeout blink-cursor-timeout-id)
	(blink-cursor-reenable-cursor)
	(setq blink-cursor-timeout-id nil)))
  (if blink-cursor-mode
      (setq blink-cursor-timeout-id
	    (add-timeout (/ (float timeout) 2) 'blink-cursor-callback nil
        (/ (float timeout) 2))))
  ; initialize last-input-time
  (if (not last-input-time)
      (setq last-input-time (cons 0 0))))

(provide 'blink-cursor)

;;; blink-cursor.el ends here
