;;; fullscreen-mode.el --- fullscreen window support for Emacs

;; Author: Ryan Mulligan <ryan@ryantm.com>
;; Version: 20130524.956
;; X-Original-Version: 0.0.1
;; URL: https://github.com/ryantm/fullscreen-mode
;; Keywords: fullscreen, fullscreen-mode

;; This file is not part of GNU Emacs.

;;; Install:

;; Install package
;; (package-install 'fullscreen-mode)
;;
;; Enable minor mode
;; (fullscreen-mode 1)

;;; Commentary:

;; Global minor mode that provides
;; `fullscreen-mode-fullscreen-toggle',
;; which toggles the frame between fullscreen and windowed.
;; `fullscreen-mode-fullscreen-toggle' is bound to F11.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Code:
(defvar fullscreen-mode-windowed-frame-state (make-hash-table :weakness 'key)
  "Hash keyed by frames with the value of the fullscreen frame parameter before going to fullscreen.
 Stored so fullscreen-mode-toggle can go back to it.")

(defun fullscreen-mode-windowed-frame-state-update ()
  "Save fullscreen-windowed-frame-state with the current frame-parameter state"
  (let ((fullscreen-frame-parameter (frame-parameter nil 'fullscreen)))
    (if (not (equal fullscreen-frame-parameter 'fullboth))
        (puthash
         (selected-frame)
         fullscreen-frame-parameter
         fullscreen-mode-windowed-frame-state))))

(defun fullscreen-mode-windowed-frame-state-restore ()
  "Restore the frame-parameter stored in fullscreen-windowed-frame-state"
  (let ((fullscreen-frame-parameter (gethash (selected-frame) fullscreen-mode-windowed-frame-state)))
    (set-frame-parameter nil 'fullscreen fullscreen-frame-parameter)))

(defun fullscreen-mode-fullscreen-p ()
  "Predicate for fullscreen frame parameter being set to 'fullboth"
  (equal (frame-parameter nil 'fullscreen) 'fullboth))

;;;###autoload
(defun fullscreen-mode-fullscreen ()
  "Sets frame's fullscreen parameter to fullboth"
  (interactive)
  (fullscreen-mode-windowed-frame-state-update)
  (set-frame-parameter nil 'fullscreen 'fullboth))

;;;###autoload
(defun fullscreen-mode-windowed ()
  "Set frame's fullscreen parameter back to it's previous windowed state"
  (interactive)
  (fullscreen-mode-windowed-frame-state-restore))

;;;###autoload
(defun fullscreen-mode-fullscreen-toggle ()
  "Toggles the frame's fullscreen state"
  (interactive)
  (if (fullscreen-mode-fullscreen-p)
      (fullscreen-mode-windowed)
    (fullscreen-mode-fullscreen)))

(defvar fullscreen-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<f11>") 'fullscreen-mode-fullscreen-toggle)
    m)
  "Keymap for `fullscreen-mode'.")

;;;###autoload
(define-minor-mode fullscreen-mode
  "Provides fullscreen-mode-toggle, bound to F11 that toggles the frame between fullscreen and windowed."
  :global t
  :keymap fullscreen-mode-map)

(provide 'fullscreen-mode)
;;; fullscreen-mode.el ends here
