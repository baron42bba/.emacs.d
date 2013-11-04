;;;  rc16.el -- rc16, by proff@iq.org, translated to elisp

;; Copyright (C) 1998 Ray Jones

;; Author: Ray Jones, rjones@pobox.com
;; Keywords: RNG, rc4, rc16, stream cipher
;; Created: 1998-04-14

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.


;; rc16-context functions
(defun rc16-create-context ()
  (vector 0 0 (make-vector 65536 0)))

(defsubst rc16-context-x (context)
  (aref context 0))

(defsubst rc16-context-y (context)
  (aref context 1))

(defsubst rc16-context-state (context)
  (aref context 2))

;; sigh.  why doesn't elisp have (setf (aref ...))?
(defsubst rc16-context-set-x (context val)
  (aset context 0 val))

(defsubst rc16-context-set-y (context val)
  (aset context 1 val))

;; set a key, init the context
(defun rc16-set-key (context key)
  (let ((state (rc16-context-state context)))
    ;; init context
    (rc16-context-set-x context 0)
    (rc16-context-set-y context 0)
    (let ((counter 0))
      (while (< counter 65536)
	(aset state counter counter)
	(setq counter (1+ counter))))
  
    ;; mix in key
    (let ((keyidx 0)
	  (keylen (length key))
	  (stateidx 0)
	  (counter 0)
	  temp1 temp2)
      (while (< counter 65536)
	(setq temp1 (aref state counter))
	(setq stateidx (logand ?\xffff (+ stateidx temp1 (aref key keyidx))))
	(setq temp2 (aref state stateidx))
	(aset state stateidx temp1)
	(aset state counter temp2)
	(setq keyidx (1+ keyidx))
	(if (= keyidx keylen) (setq keyidx 0))
	(setq counter (1+ counter))))))

;; mixin some randomness, a little at a time
;; added by Ray.  is this a good thing?
(defun rc16-mixin (context val)
  (let* ((state (rc16-context-state context))
	 (x (logand ?\xffff (1+ (rc16-context-x context))))
	 (sx (aref state x))
	 (y (logand ?\xffff 
		    (logxor val
			    (+ sx (rc16-context-y context)))))
	 (sy (aref state y)))
    (rc16-context-set-x context x)
    (rc16-context-set-y context y)
    (aset state y sx)
    (aset state x sy)
    nil))

;; return a random value
(defun rc16-short (context)
  (let* ((state (rc16-context-state context))
	 (x (logand ?\xffff (1+ (rc16-context-x context))))
	 (sx (aref state x))
	 (y (logand ?\xffff (+ sx (rc16-context-y context))))
	 (sy (aref state y)))
    (rc16-context-set-x context x)
    (rc16-context-set-y context y)
    (aset state y sx)
    (aset state x sy)
    (aref state (logand ?\xffff (+ sx sy)))))

(defun rc16-encrypt (context data)
  (let* ((cipher (make-string (length data) 0))
	 (rc16-val (rc16-short context))
	 (hi-byte (ash rc16-val -8))
	 (lo-byte (logand rc16-val 255))
	 (bytes-left 2)
	 (count 0)
	 (length (length data)))
    (while (< count length)
      (aset cipher count (logxor (aref data count) hi-byte))
      (setq bytes-left (1- bytes-left))
      (if (= bytes-left 0)
	  (progn
	    (setq bytes-left 2)
	    (setq rc16-val (rc16-short context))
	    (setq hi-byte (ash rc16-val -8))
	    (setq lo-byte (logand rc16-val 255)))
	(setq hi-byte lo-byte))
      (setq count (1+ count)))
    cipher))

(provide 'rc16)
