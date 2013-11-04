;;; lispm-fonts.el --- quick hack to parse LISPM-style font-shift codes

;; Keywords: faces

;; Copyright (C) 1992-1993 Free Software Foundation, Inc.

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

;; This only copes with MIT/LMI/TI style font shifts, not Symbolics.
;; It doesn't do diagram lines (ha ha).  It doesn't do output.  That
;; has to wait until it is possible to attach faces to characters
;; instead of just intervals, since this code is really talking about
;; attributes of the text instead of attributes of regions of the
;; buffer.  We could do it by mapping over the extents and hacking
;; the overlaps by hand, but that would be hard.

(make-face 'variable)
(or (face-differs-from-default-p 'variable)
    (set-face-font 'variable
		   "-*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*"))

(make-face 'variable-bold)
(or (face-differs-from-default-p 'variable-bold)
    (progn
      ;; This is no good because helvetica-12-bold is a LOT larger than
      ;; helvetica-12-medium.  Someone really blew it there.
      ;; (copy-face 'variable 'variable-bold)
      ;; (make-face-bold 'variable-bold)
      (set-face-font 'variable-bold
		     "-*-helvetica-bold-r-*-*-*-100-*-*-*-*-*-*")))

(make-face 'variable-italic)
(or (face-differs-from-default-p 'variable-italic)
    (progn
      (copy-face 'variable-bold 'variable-italic) ; see above
      (make-face-unbold 'variable-italic)
      (make-face-italic 'variable-italic)))

(make-face 'variable-bold-italic)
(or (face-differs-from-default-p 'variable-bold-italic)
    (progn
      (copy-face 'variable-bold 'variable-bold-italic)
      (make-face-italic 'variable-bold-italic)))

(defconst lispm-font-to-face
  '(("tvfont"		. default)
    ("cptfont"		. default)
    ("cptfontb"		. bold)
    ("cptfonti"		. italic)
    ("cptfontbi"	. bold-italic)
    ("base-font"	. default)
    ("bigfnt"		. bold)
    ("cmb8"		. variable-bold)
    ("higher-medfnb"	. bold)
    ("higher-tr8"	. default)
    ("medfnb"		. bold)
    ("medfnt"		. normal)
    ("medfntb"		. bold)
    ("wider-font"	. bold)
    ("wider-medfnt"	. bold)
    ("mets"		. variable-large)
    ("metsb"		. variable-large-bold)
    ("metsbi"		. variable-large-bold-italic)
    ("metsi"		. variable-large-italic)
    ("cmr5"		. variable)
    ("cmr10"		. variable)
    ("cmr18"		. variable)
    ("cmold"		. variable)
    ("cmdunh"		. variable)
    ("hl10"		. variable)
    ("hl10b"		. variable-bold)
    ("hl12"		. variable)
    ("hl12b"		. variable-bold)
    ("hl12bi"		. variable-bold-italic)
    ("hl12i"		. variable-italic)
    ("hl6"		. variable)
    ("hl7"		. variable)
    ("tr10"		. variable)
    ("tr10b"		. variable-bold)
    ("tr10bi"		. variable-bold-italic)
    ("tr10i"		. variable-italic)
    ("tr12"		. variable)
    ("tr12b"		. variable-bold)
    ("tr12bi"		. variable-bold-italic)
    ("tr12i"		. variable-italic)
    ("tr18"		. variable-large)
    ("tr18b"		. variable-large-bold)
    ("tr8"		. variable)
    ("tr8b"		. variable-bold)
    ("tr8i"		. variable-italic)
    ("5x5"		. small)
    ("tiny"		. small)
    ("43vxms"		. variable-large)
    ("courier"		. bold)
    ("adobe-courier10"	. default)
    ("adobe-courier14"	. bold)
    ("adobe-courier10b"	. bold)
    ("adobe-courier14b"	. bold)
    ("adobe-hl12"	. variable)
    ("adobe-hl14"	. variable)
    ("adobe-hl14b"	. variable-bold)
    )
  "Alist of LISPM font names to Emacs face names.")


(defun lispm-font-to-face (lispm-font)
  (if (symbolp lispm-font)
      (setq lispm-font (symbol-name lispm-font)))
  (let ((case-fold-search t)
	face)
    (setq lispm-font (downcase lispm-font))
    (if (string-match "^fonts:+" lispm-font)
	(setq lispm-font (substring lispm-font (match-end 0))))
    (if (setq face (cdr (assoc lispm-font lispm-font-to-face)))
	(if (find-face face)
	    face
	  (message "warning: unknown face %s" face)
	  'default)
      (message "warning: unknown Lispm font %s" (upcase lispm-font))
      'default)))

(defvar fonts)  ; the -*- line of the file will set this.

(defun lispm-fontify-hack-local-variables ()
  ;; Sometimes code has font-shifts in the -*- line, which means that the
  ;; local variables will have been read incorrectly by the emacs-lisp reader.
  ;; In particular, the `fonts' variable might be corrupted.  So if there
  ;; are font-shifts in the prop line, re-parse it.
  (if (or (not (boundp 'fonts))
	  (null 'fonts)
	  (let ((case-fold-search t))
	    (and (looking-at "[ \t]*;.*-\\*-.*fonts[ \t]*:.*-\\*-")
		 (looking-at ".*\^F"))))
      (save-excursion
	(save-restriction
	  (end-of-line)
	  (narrow-to-region (point-min) (point))
	  (goto-char (point-min))
	  (while (re-search-forward "\^F[0-9a-zA-Z*]" nil t)
	    (delete-region (match-beginning 0) (match-end 0)))
	  (let ((enable-local-variables 'query))
	    (hack-local-variables))))))

(defun lispm-fontify-buffer ()
  (save-excursion
    (goto-char (point-min))
    (if (fboundp 'font-lock-mode) (font-lock-mode 0))
    (lispm-fontify-hack-local-variables)
    (let ((font-stack nil)
	  (p (point))
	  c)
      (while (search-forward "\^F" nil t)
	(delete-char -1)
	(setq c (following-char))
	(delete-char 1)
	(cond ((= c ?\^F)
	       (insert "\^F"))
	      ((= c ?*)
	       (if (and font-stack (/= p (point)))
		   (set-extent-face (make-extent p (point)) (car font-stack)))
	       (setq p (point))
	       (setq font-stack (cdr font-stack)))
	      ((or (< c ?0) (> c ?Z)) ; error...
	       nil)
	      ((>= (setq c (- c ?0)) (length fonts)) ; error...
	       nil)
	      (t
	       (if (and font-stack (/= p (point)))
		   (set-extent-face (make-extent p (point)) (car font-stack)))
	       (setq font-stack (cons (lispm-font-to-face (nth c fonts))
				      font-stack))
	       (setq p (point)))))
      (if (and font-stack (/= p (point)))
	  (set-extent-face (make-extent p (point)) (car font-stack))))))
