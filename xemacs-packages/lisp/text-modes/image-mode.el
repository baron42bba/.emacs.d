;;; image-mode.el --- Major mode for navigating images

;; Copyright (C) 1997 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1997/6/27
;; Version: image-mode.el,v 20.3.1.2 1997/07/01 17:29:44 morioka Exp
;; Keywords: image, graphics

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

;; Modified: 2001-07-10, Glynn Clements <glynn.clements@virgin.net>
;; 	Substantially re-written to avoid use of format-alist
;; Modified: 2002-10-18, Glynn Clements <glynn.clements@virgin.net>
;; 	Autoload image-mode-install
;; Modified: 2002-10-22, Glynn Clements <glynn.clements@virgin.net>
;; 	Remove format-alist entries

;;; Code:

(require 'cl)

(defvar image-format nil)
(make-variable-buffer-local 'image-format)

(defvar image-decoded nil)
(make-variable-buffer-local 'image-decoded)

;;;###autoload
(defvar image-formats-alist
  '(("png"   . png )
    ("gif"   . gif )
    ("jpe?g" . jpeg)
    ("tiff?" . tiff)
    ("xbm"   . xbm )
    ("xpm"   . xpm )
    ("bmp"   . bmp )))

(defun image-guess-type ()
  (let (ext item)
    (and buffer-file-name
	 (setq ext (downcase (file-name-extension buffer-file-name)))
	 (setq item (assoc* ext image-formats-alist
			    :test (lambda (str re)
				    (string-match (concat "^" re "\\'")
						  str))))
	 (setq image-format (cdr item)))))

(defun image-decode-buffer ()
  (image-decode (point-min) (point-max) image-format)
  (setq image-decoded t)
  (set-buffer-modified-p nil))

(defun image-undecode-buffer ()
  (setq buffer-read-only nil)
  (map-extents (function
		(lambda (extent maparg)
		  (delete-extent extent)))
	       nil (point-min) (point-max) nil 'end-closed)
  (setq image-decoded nil))

(defun image-decode (start end type)
  "Decode the image between START and END which is encoded in TYPE."
  (save-excursion
    (let ((image (and type
		      (make-image-instance
		       (vector type :data (buffer-string start end))
		       nil nil 'no-error))))
      (unless image
	(setq image (make-image-instance
		     (vector 'string :data "format is not supported!\n")
		     nil nil 'no-error)))
      (set-extent-property (make-extent start end) 'invisible t)
      (let ((glyph (make-glyph image)))
	(set-extent-end-glyph (make-extent end end) glyph))
      (setq buffer-read-only t))))

(defvar image-mode-map (make-keymap))
(suppress-keymap image-mode-map)
(define-key image-mode-map "v" 'image-start-external-viewer)
(define-key image-mode-map "t" 'image-toggle-decoding)
(define-key image-mode-map "h" 'image-enter-hexl-mode)
(define-key image-mode-map "e" 'image-enter-xpm-mode)
(define-key image-mode-map "q" 'image-mode-quit)

;; ### There must be a general way of doing this, using mimecap....
(defvar image-external-viewer-list
  '(
    "xv"				; xv
    "display"				; ImageMagic
    )
  "*List of external viewers for image-mode.

Each viewer is a string, to be called via `start-process'.  If null,
no external viewer will be used.")

(defun image-start-external-viewer ()
  "Start external image viewer for current-buffer.

It tries each program name in `image-external-viewer-list' in order.
If `image-external-viewer-list' is empty, or none of the viewers can
be found, signals an error."

  (interactive)
  (let ((vl image-external-viewer-list))
    (if vl
	(catch 'done
	  (while vl
	    (condition-case nil
		(progn
		  (start-process "external image viewer" nil
				 (car vl) buffer-file-name)
		  (throw 'done nil))	; exit loop
	      (file-error (setq vl (cdr vl)))))
	  (error "image-start-external-viewer:  couldn't start any viewer in `image-external-viewer-list'"))
      (error "image-start-external-viewer:  `image-external-viewer-list' is empty."))))

(defun image-toggle-decoding ()
  "Toggle image display mode in current buffer."
  (interactive)
  (if image-decoded
      (image-undecode-buffer)
    (image-decode-buffer)))

(defun image-exit-hexl-mode-function ()
  (image-decode-buffer)
  (remove-hook 'hexl-mode-exit-hook 'image-exit-hexl-mode-function))

(defun image-enter-hexl-mode ()
  "Enter hexl-mode."
  (interactive)
  (when image-decoded
    (image-undecode-buffer)
    (add-hook 'hexl-mode-exit-hook 'image-exit-hexl-mode-function))
  (hexl-mode))

(defun image-enter-xpm-mode ()
  "Enter xpm-mode."
  (interactive)
  (if (not (eq image-format 'xpm))
      (error "Not an XPM image."))
  (when image-decoded
    (image-undecode-buffer))
  (xpm-mode 1))

(defun image-mode-quit ()
  "Exit image-mode."
  (interactive)
  (kill-buffer (current-buffer)))

(defun image-maybe-restore ()
  "Restore buffer if it is decoded."
  (when image-decoded
    (image-undecode-buffer)))

(add-hook 'change-major-mode-hook 'image-maybe-restore)

;;;###autoload
(defun image-mode (&optional arg)
  "\\{image-mode-map}"
  (interactive)
  (setq major-mode 'image-mode)
  (setq mode-name "Image")
  (use-local-map image-mode-map)
  (image-guess-type)
  (image-decode-buffer))

;;;###autoload
(progn
  (setq format-alist
	(remove-if (lambda (x)
		     (eq (nth 6 x) 'image-mode))
		   format-alist))
  (dolist (format image-formats-alist)
    (let* ((re (car format))
	   (type (cdr format))
	   (regexp (concat "\\.\\(" re "\\|" (upcase re) "\\)\\'"))
	   (item (cons regexp 'image-mode)))
      (and (featurep type)
	   (add-to-list 'auto-mode-alist item)))))

(provide 'image-mode)

;;; image-mode.el ends here
