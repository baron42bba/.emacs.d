;;;  ascii-armor.el -- translate data into and from ascii-armor 
;;;                    (radix64)

;; Copyright (C) 1998 Ray Jones

;; Author: Ray Jones, rjones@pobox.com
;; Keywords: base64, ascii-armor, radix64, oink
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

;;; Commentary:

;;; Code:

(require 'cl)

(defun ascii-armor-char (val)
  (cond ((< val 26) (+ val ?A))
	((< val 52) (+ (- val 26) ?a))
	((< val 62) (+ (- val 52) ?0))
	((= val 62) ?+)
	((= val 63) ?/)
	(t (error "no ascii-armor character for %d!" val))))

(defun ascii-armor-val (char)
  (cond ((and (<= ?A char) (<= char ?Z)) (- char ?A))
	((and (<= ?a char) (<= char ?z)) (+ (- char ?a) 26))
	((and (<= ?0 char) (<= char ?9)) (+ (- char ?0) 52))
	((= char ?+) 62)
	((= char ?/) 63)
	(t nil)))

(defun ascii-armor-length (n)
  "calculate the number of characters needed to encode N octets."

  (let* (
	 ;; number of bits
	 (n1 (* n 8))
	 ;; ascii armor is 6 bits per symbol...
	 (n2 (car (ceiling* n1 6)))
	 ;; but always a multiple of 4 symbols
	 (n3 (* 4 (car (ceiling* n2 4)))))
    n3))

(defun ascii-armor-data-length (str)
  "calculate the number of octets stored in an ascii-armor string"

  (let ((len (length str)))
    (while (and (> len 0) 
                (eq ?= (aref str (1- len))))
      (decf len))
    (/ (* len 6) 8)))

;; translate a vector of 16-bit values into an ascii-armor string
(defun vec16-to-ascii-armor (vec)
  (let* ((in-len (length vec))
	 (out-len (ascii-armor-length (* 2 in-len)))
	 (out-str (make-string out-len ?=))
	 (out-idx 0)
	 (bits-start 0))
    ;; helper function
    (flet ((next-out (val)
	     (aset out-str out-idx
		   (ascii-armor-char
		    (logand ?\x3f val)))
	     (incf out-idx)))

      (dotimes (in-idx in-len)
	(incf bits-start 16)
	;; read out as many bits from the current index as possible
	(while (> bits-start 0)
	  ;; do the next 6 bits straddle a boundary in vec?
	  
	  (if (< bits-start 6)
	      ;; straddle
	      (let ((hi-val (aref vec in-idx))
		    ;; pad with 0s
		    (lo-val (if (< in-idx (1- in-len))
				(aref vec (1+ in-idx))
			      0)))
		(next-out (logior
			   (ash hi-val (- 6 bits-start))
			   (ash lo-val (- 6 bits-start 16)))))
	    
	    ;; 6 bits all from the current entry in vec
	    (next-out (ash (aref vec in-idx) (- 6 bits-start))))

	  (decf bits-start 6))))

    ;; pad with ?=, if out-str isn't full
    (while (< out-idx out-len)
      (aset out-str out-idx ?=)
      (incf out-idx))
  
    out-str))

;; translate an ascii-armor string to a 16-bit vector
(defun ascii-armor-to-vec16 (string)
  ;; ascii armor is padded, so this doesn't have to be rounded, just
  ;; truncated.
  (let* ((out-len (/ (ascii-armor-data-length string) 2))
	 (out-vec (make-vector out-len 0))
	 (buf 0)
	 (bits-in-buf 0)
	 (in-idx 0))
    (dotimes (out-idx out-len)
      ;; shift bits from the string until there are enough to stick
      ;; into the output vector
      (while (< bits-in-buf 16)
	(let ((val (ascii-armor-val (aref string in-idx))))
	  (when val
	    (setq buf (logior (ash buf 6)
			      val))
	    (incf bits-in-buf 6))
	  (incf in-idx)))

      (aset out-vec out-idx (ash buf (- 16 bits-in-buf)))
      (decf bits-in-buf 16)
      ;; turn off the used bits
      (setq buf (logand buf (lognot (ash ?\xffff bits-in-buf)))))

    out-vec))

(provide 'ascii-armor)
