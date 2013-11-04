;;;  rander.el -- possibly better random numbers, using rc16 as a RNG

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


(require 'cl)
(require 'rc16)

(defvar *rander-file* (expand-file-name "~/.emacs.rander"))
(defvar *rander-file-length* 512)

;;; Initialization of the random state

;; generate a string with random information in it
(defun rander-noise-string ()
  (concat
   ;; randomness from before
   (if (file-readable-p *rander-file*)
       (let ((buf nil))
	 (unwind-protect
	     (progn
	       (setq buf (generate-new-buffer "randerdata"))
	       (save-excursion
		 (set-buffer buf)
		 (insert-file-contents-literally *rander-file*)
		 (buffer-string)))
	   (if buf
	       (let ((kill-buffer-hook nil))
		 (kill-buffer buf))))))

   ;; allocation randomness
   (if (boundp 'cons-cells-consed)
       (apply 'concat 
              (mapcar 'int-to-string 
                      (list cons-cells-consed
                            floats-consed
                            intervals-consed
                            misc-objects-consed
                            string-chars-consed
                            symbols-consed
                            vector-cells-consed)))
       "")
   
   ;; time randomness
   (apply 'concat (mapcar 'int-to-string (current-time)))
   
   ;; process randomness
   (int-to-string (emacs-pid))))


;;; Mixin keypress randomness, on the fly

;; don't use this if your (current-time) is not fine grained in the
;; microseconds.  or if your computer is particularly slow, and the
;; pre-command-hook stuff slows it down (unlikely).
(defvar *rander-use-keypress* t)
;; this number shouldn't be too high, as a call to (rander16) will
;; have to mixin up to this many values before actually returning
(defvar *rander-keypress-count* 128)
(defvar *rander-keypress-timings* 
  (and *rander-use-keypress*
       (make-vector *rander-keypress-count* 0)))
(defvar *rander-keypress-have* 0)

(defun rander-store-key-time ()
  "store keypress timings until we have filled the keypress timings buffer"
  (when (< *rander-keypress-have* *rander-keypress-count*)
    (aset *rander-keypress-timings* 
          *rander-keypress-have* 
          (nth 2 (current-time)))
    (incf *rander-keypress-have*)))

(if *rander-use-keypress*
    (add-hook 'pre-command-hook 'rander-store-key-time))

;;; main source of randomness, the rc16 generator
(defvar *rander-initialized* nil)
(defvar *rander-rc16-context* nil)

(defun rander-init ()
  (setq *rander-rc16-context* (rc16-create-context))
  (rc16-set-key *rander-rc16-context* (rander-noise-string))
  (add-hook 'kill-emacs-hook 'rander-write-file)
  (setq *rander-initialized* t))

(defun rander16 ()
  (if (not *rander-initialized*)
      (rander-init))
  (when (and *rander-use-keypress*
             (not (zerop *rander-keypress-have*)))
    ;; mixin all the keypresses we have stored to this point
    (dotimes (idx *rander-keypress-have*)
      (rc16-mixin *rander-rc16-context*
                  (aref *rander-keypress-timings* idx)))
    ;; reset the buffer
    (setq *rander-keypress-have* 0))

  ;; return a random value
  (rc16-short *rander-rc16-context*))

(defun rander-write-file ()
  "write out a seed file for the next time rander is used.  called at
the exit of emacs."  
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf (generate-new-buffer "randerdata"))
          (save-excursion
            (set-buffer buf)
            (dotimes (count *rander-file-length*)
              (insert (logand ?\xff (rander16))))
	    (let ((backup-inhibited t))
	      (write-file *rander-file*))))
      (if buf
          (let ((kill-buffer-hook nil))
            (kill-buffer buf))))))

(provide 'rander)
