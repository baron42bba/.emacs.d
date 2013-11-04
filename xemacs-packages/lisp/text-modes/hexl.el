;;; hexl.el --- edit a file in a hex dump format using the hexl filter.

;; Copyright (C) 1989, 1994, 1998 Free Software Foundation, Inc.

;; Author: Keith Gabryelski <ag@wheaties.ai.mit.edu>
;; Maintainer: FSF
;; Keywords: data

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

;;; Synched up with: FSF 21.0.103.

;;; Commentary:

;; This package implements a major mode for editing binary files.  It uses
;; a program called hexl, supplied with the GNU Emacs distribution, that
;; can filter a binary into an editable format or from the format back into
;; binary.  For full instructions, invoke `hexl-mode' on an empty buffer and
;; do `M-x describe-mode'.
;;
;; This may be useful in your .emacs:
;;
;;	(autoload 'hexl-find-file "hexl"
;;	  "Edit file FILENAME in hexl-mode." t)
;;	
;;	(define-key global-map "\C-c\C-h" 'hexl-find-file)
;;
;; NOTE: Remember to change HEXL-PROGRAM or HEXL-OPTIONS if needed.
;;
;; Currently hexl only supports big endian hex output with 16 bit
;; grouping.
;;
;; -iso in `hexl-options' will allow iso characters to display in the
;; ASCII region of the screen (if your emacs supports this) instead of
;; changing them to dots.

;;; Code:

;;
;; vars here
;;

(defgroup hexl nil
  "Edit a file in a hex dump format using the hexl filter."
  :group 'data)


(defcustom hexl-program "hexl"
  "The program that will hexlify and dehexlify its stdin.
`hexl-program' will always be concatenated with `hexl-options'
and \"-de\" when dehexlifying a buffer."
  :type 'string
  :group 'hexl)

(defcustom hexl-iso ""
  "If your emacs can handle ISO characters, this should be set to
\"-iso\" otherwise it should be \"\"."
  :type 'string
  :group 'hexl)

(defcustom hexl-options (format "-hex %s" hexl-iso)
  "Options to hexl-program that suit your needs."
  :type 'string
  :group 'hexl)

(defcustom hexlify-command
  (format "%s %s"
	  (shell-quote-argument
	   (expand-file-name hexl-program exec-directory))
	  hexl-options)
  "The command to use to hexlify a buffer."
  :type 'string
  :group 'hexl)

(defcustom dehexlify-command
  (format "%s -de %s"
	  (shell-quote-argument
	   (expand-file-name hexl-program exec-directory))
	  hexl-options)
  "The command to use to unhexlify a buffer."
  :type 'string
  :group 'hexl)

(defcustom hexl-follow-ascii t
  "If non-nil then highlight the ASCII/HEX character corresponding to point."
  :type 'boolean
  :group 'hexl
  :version "20.3")

(defvar hexl-max-address 0
  "Maximum offset into hexl buffer.")

(defvar hexl-mode-map nil)
(defvar hexl-mode-old-local-map)
(defvar hexl-mode-old-mode-name)
(defvar hexl-mode-old-major-mode)
(defvar hexl-mode-old-write-contents-hooks)
(defvar hexl-mode-old-require-final-newline)
(defvar hexl-mode-old-syntax-table)

(defvar hexl-ascii-extent nil
  "Extent used to highlight ASCII element corresponding to current point.")
(make-variable-buffer-local 'hexl-ascii-extent)
(defvar hexl-current-address)
(make-variable-buffer-local 'hexl-current-address)

;; routines

(put 'hexl-mode 'mode-class 'special)

;;;###autoload
(defun hexl-mode (&optional arg)
  "\\<hexl-mode-map>A mode for editing binary files in hex dump format.
This is not an ordinary major mode; it alters some aspects
if the current mode's behavior, but not all; also, you can exit
Hexl mode and return to the previous mode using `hexl-mode-exit'.

This function automatically converts a buffer into the hexl format
using the function `hexlify-buffer'.

Each line in the buffer has an \"address\" (displayed in hexadecimal)
representing the offset into the file that the characters on this line
are at and 16 characters from the file (displayed as hexadecimal
values grouped every 16 bits) and as their ASCII values.

If any of the characters (displayed as ASCII characters) are
unprintable (control or meta characters) they will be replaced as
periods.

If `hexl-mode' is invoked with an argument the buffer is assumed to be
in hexl format.

A sample format:

  HEX ADDR: 0001 0203 0405 0607 0809 0a0b 0c0d 0e0f     ASCII-TEXT
  --------  ---- ---- ---- ---- ---- ---- ---- ----  ----------------
  00000000: 5468 6973 2069 7320 6865 786c 2d6d 6f64  This is hexl-mod
  00000010: 652e 2020 4561 6368 206c 696e 6520 7265  e.  Each line re
  00000020: 7072 6573 656e 7473 2031 3620 6279 7465  presents 16 byte
  00000030: 7320 6173 2068 6578 6164 6563 696d 616c  s as hexadecimal
  00000040: 2041 5343 4949 0a61 6e64 2070 7269 6e74   ASCII.and print
  00000050: 6162 6c65 2041 5343 4949 2063 6861 7261  able ASCII chara
  00000060: 6374 6572 732e 2020 416e 7920 636f 6e74  cters.  Any cont
  00000070: 726f 6c20 6f72 206e 6f6e 2d41 5343 4949  rol or non-ASCII
  00000080: 2063 6861 7261 6374 6572 730a 6172 6520   characters.are 
  00000090: 6469 7370 6c61 7965 6420 6173 2070 6572  displayed as per
  000000a0: 696f 6473 2069 6e20 7468 6520 7072 696e  iods in the prin
  000000b0: 7461 626c 6520 6368 6172 6163 7465 7220  table character 
  000000c0: 7265 6769 6f6e 2e0a                      region..

Movement is as simple as movement in a normal emacs text buffer.  Most
cursor movement bindings are the same (ie. Use \\[hexl-backward-char], \\[hexl-forward-char], \\[hexl-next-line], and \\[hexl-previous-line]
to move the cursor left, right, down, and up).

Advanced cursor movement commands (ala \\[hexl-beginning-of-line], \\[hexl-end-of-line], \\[hexl-beginning-of-buffer], and \\[hexl-end-of-buffer]) are
also supported.

There are several ways to change text in hexl mode:

ASCII characters (character between space (0x20) and tilde (0x7E)) are
bound to self-insert so you can simply type the character and it will
insert itself (actually overstrike) into the buffer.

\\[hexl-quoted-insert] followed by another keystroke allows you to insert the key even if
it isn't bound to self-insert.  An octal number can be supplied in place
of another key to insert the octal number's ASCII representation.

\\[hexl-insert-hex-char] will insert a given hexadecimal value (if it is between 0 and 0xFF)
into the buffer at the current point.

\\[hexl-insert-octal-char] will insert a given octal value (if it is between 0 and 0377)
into the buffer at the current point.

\\[hexl-insert-decimal-char] will insert a given decimal value (if it is between 0 and 255)
into the buffer at the current point.

\\[hexl-mode-exit] will exit hexl-mode.

Note: saving the file with any of the usual Emacs commands
will actually convert it back to binary format while saving.

You can use \\[hexl-find-file] to visit a file in Hexl mode.

\\[describe-bindings] for advanced commands."
  (interactive "p")
  (unless (eq major-mode 'hexl-mode)
    (let ((modified (buffer-modified-p))
	  (inhibit-read-only t)
	  (original-point (1- (point)))
	  max-address)
      (and (eobp) (not (bobp))
	   (setq original-point (1- original-point)))
      (if (not (or (eq arg 1) (not arg)))
	  ;; if no argument then we guess at hexl-max-address
	  (setq max-address (+ (* (/ (1- (buffer-size)) 68) 16) 15))
	(setq max-address (1- (buffer-size)))
	(hexlify-buffer)
	(set-buffer-modified-p modified))
      (make-local-variable 'hexl-max-address)
      (setq hexl-max-address max-address)
      (hexl-goto-address original-point))

    ;; We do not turn off the old major mode; instead we just
    ;; override most of it.  That way, we can restore it perfectly.
    (make-local-variable 'hexl-mode-old-local-map)
    (setq hexl-mode-old-local-map (current-local-map))
    (use-local-map hexl-mode-map)

    (make-local-variable 'hexl-mode-old-mode-name)
    (setq hexl-mode-old-mode-name mode-name)
    (setq mode-name "Hexl")

    (make-local-variable 'hexl-mode-old-major-mode)
    (setq hexl-mode-old-major-mode major-mode)
    (setq major-mode 'hexl-mode)

    (make-local-variable 'hexl-mode-old-syntax-table)
    (setq hexl-mode-old-syntax-table (syntax-table))
    (set-syntax-table (standard-syntax-table))

    (make-local-variable 'hexl-mode-old-write-contents-hooks)
    (setq hexl-mode-old-write-contents-hooks write-contents-hooks)
    (make-local-variable 'write-contents-hooks)
    (add-hook 'write-contents-hooks 'hexl-save-buffer)

    (make-local-variable 'hexl-mode-old-require-final-newline)
    (setq hexl-mode-old-require-final-newline require-final-newline)
    (make-local-variable 'require-final-newline)
    (setq require-final-newline nil)

    ;; Add hooks to rehexlify or dehexlify on various events.
    (make-local-hook 'after-revert-hook)
    (add-hook 'after-revert-hook 'hexl-after-revert-hook nil t)

    (make-local-hook 'change-major-mode-hook)
    (add-hook 'change-major-mode-hook 'hexl-maybe-dehexlify-buffer nil t)

    (make-local-variable 'modeline-format)
    (setq modeline-format (append '("--" hexl-current-address "--")
                                  (default-value 'modeline-format)))
    
    (if hexl-follow-ascii (hexl-follow-ascii 1)))
  (run-hooks 'hexl-mode-hook))

(defun hexl-after-revert-hook ()
  (setq hexl-max-address (1- (buffer-size)))
  (hexlify-buffer)
  (set-buffer-modified-p nil))

(defvar hexl-in-save-buffer nil)

(defun hexl-save-buffer ()
  "Save a hexl format buffer as binary in visited file if modified."
  (interactive)
  (if hexl-in-save-buffer nil
    (set-buffer-modified-p (if (buffer-modified-p)
			       (save-excursion
				 (let ((buf (generate-new-buffer " hexl"))
				       (name (buffer-name))
				       ;(file-name (buffer-file-name))
				       (start (point-min))
				       (end (point-max))
				       modified)
				   (set-buffer buf)
				   (insert-buffer-substring name start end)
				   (set-buffer name)
				   (dehexlify-buffer)
				   ;; Prevent infinite recursion.
				   (let ((hexl-in-save-buffer t))
				     (save-buffer))
				   (setq modified (buffer-modified-p))
				   (delete-region (point-min) (point-max))
				   (insert-buffer-substring buf start end)
				   (kill-buffer buf)
				   modified))
			     (message "(No changes need to be saved)")
			     nil))
    ;; Return t to indicate we have saved t
    t))

;;;###autoload
(defun hexl-find-file (filename)
  "Edit file FILENAME in hexl-mode.
Switch to a buffer visiting file FILENAME, creating one if none exists."
  (interactive "fFilename: ")
  (if (featurep 'file-coding)
      (find-file filename 'binary)
    (find-file filename))
  (if (not (eq major-mode 'hexl-mode))
      (hexl-mode)))

(defun hexl-mode-exit (&optional arg)
  "Exit Hexl mode, returning to previous mode.
With arg, don't unhexlify buffer."
  (interactive "p")
  (if (or (eq arg 1) (not arg))
      (let ((modified (buffer-modified-p))
	    (inhibit-read-only t)
	    (original-point (1+ (hexl-current-address))))
	(dehexlify-buffer)
	(remove-hook 'write-contents-hooks 'hexl-save-buffer)
	(set-buffer-modified-p modified)
	(goto-char original-point)))

  (remove-hook 'after-revert-hook 'hexl-after-revert-hook t)
  (remove-hook 'change-major-mode-hook 'hexl-maybe-dehexlify-buffer t)
  (remove-hook 'post-command-hook 'hexl-follow-ascii-find t)
  (setq hexl-ascii-extent nil)

  (setq write-contents-hooks hexl-mode-old-write-contents-hooks)
  (setq require-final-newline hexl-mode-old-require-final-newline)
  (setq mode-name hexl-mode-old-mode-name)
  (use-local-map hexl-mode-old-local-map)
  (set-syntax-table hexl-mode-old-syntax-table)
  (setq major-mode hexl-mode-old-major-mode)
  (force-mode-line-update)
  (run-hooks 'hexl-mode-exit-hook))

(defun hexl-maybe-dehexlify-buffer ()
  "Convert a hexl format buffer to binary.
Ask the user for confirmation."
  (if (y-or-n-p "Convert contents back to binary format? ")
      (let ((modified (buffer-modified-p))
	    (inhibit-read-only t)
	    (original-point (1+ (hexl-current-address))))
	(dehexlify-buffer)
	(remove-hook 'write-contents-hooks 'hexl-save-buffer)
	(set-buffer-modified-p modified)
	(goto-char original-point))))

(defun hexl-current-address (&optional validate)
  "Return current hexl-address."
  (interactive)
  (let ((current-column (- (% (point) 68) 11))
	(hexl-address 0))
    (if (< current-column 0)
	(if validate
	    (error "Point is not on a character in the file")
	  (setq current-column 0)))
    (setq hexl-address
	  (+ (* (/ (point) 68) 16)
	     (if (>= current-column 41)
		 (- current-column 41)
	       (/ (- current-column  (/ current-column 5)) 2))))
    (if (interactive-p)
        (message "Current address 0x%010x = %10d" hexl-address hexl-address))
    hexl-address))

(defun hexl-hex-position (address)
  "Return the ascii-position."
  (+ (* (/ address 16) 68)
     11
     (* 2 (% address 16))
     (/ (% address 16) 2)))

(defun hexl-ascii-position (address)
  "Return the ascii-position."
  (+ (* (/ address 16) 68) 52 (% address 16)))

(defun hexl-at-ascii-position (address)
  "Return t if point is at ascii position."
  (= (point) (hexl-ascii-position address)))

(defun hexl-address-to-marker (address)
  "Return buffer position for ADDRESS."
  (interactive "nAddress: ")
  (+ (* (/ address 16) 68) 11 (/ (* (% address 16) 5) 2)))

(defun hexl-goto-address (address &optional nipple)
  "Go to hexl-mode (decimal) address ADDRESS.
Signal error if ADDRESS out of range."
  (interactive "nAddress: ")
  (if (or (< address 0) (> address hexl-max-address))
      (error "Out of hexl region."))
  (goto-char
   (if (hexl-at-ascii-position (hexl-current-address))
       (hexl-ascii-position address)
     (+ (hexl-address-to-marker address) (or nipple 0)))))

(defun hexl-goto-hex-address (hex-address)
  "Go to hexl-mode address (hex string) HEX-ADDRESS.
Signal error if HEX-ADDRESS is out of range."
  (interactive "sHex Address: ")
  (hexl-goto-address (hexl-hex-string-to-integer hex-address)))

(defun hexl-hex-string-to-integer (hex-string)
  "Return decimal integer for HEX-STRING."
  (interactive "sHex number: ")
  (let ((hex-num 0))
    (while (not (equal hex-string ""))
      (setq hex-num (+ (* hex-num 16)
		       (hexl-hex-char-to-integer (string-to-char hex-string))))
      (setq hex-string (substring hex-string 1)))
    hex-num))

(defun hexl-octal-string-to-integer (octal-string)
  "Return decimal integer for OCTAL-STRING."
  (interactive "sOctal number: ")
  (let ((oct-num 0))
    (while (not (equal octal-string ""))
      (setq oct-num (+ (* oct-num 8)
		       (hexl-oct-char-to-integer
			(string-to-char octal-string))))
      (setq octal-string (substring octal-string 1)))
    oct-num))

;; move point functions

(defun hexl-backward-char (arg)
  "Move to left ARG bytes (right if ARG negative) in hexl-mode."
  (interactive "p")
  (let ((col (current-column)))
    (if (< col 50)
        (if (member (% col 5) '(0 2))
            (hexl-goto-address (- (hexl-current-address) arg) 1)
          (hexl-goto-address (hexl-current-address) 0))
      (hexl-goto-address (- (hexl-current-address) arg) col))))

(defun hexl-forward-char (arg)
  "Move right ARG bytes (left if ARG negative) in hexl-mode."
  (interactive "p")
  (let ((col (current-column)))
    (if (< col 50)
        (if (member (% col 5) '(0 2))
            (hexl-goto-address (hexl-current-address) 1)
          (hexl-goto-address (+ (hexl-current-address) arg) 0))
      (hexl-goto-address (+ (hexl-current-address) arg) col))))

(defun hexl-backward-short (arg)
  "Move to left ARG shorts (right if ARG negative) in hexl-mode."
  (interactive "p")
  (hexl-goto-address (let ((address (hexl-current-address)))
		       (if (< arg 0)
			   (progn
			     (setq arg (- arg))
			     (while (> arg 0)
			       (if (not (equal address (logior address 3)))
				   (if (> address hexl-max-address)
				       (progn
					 (message "End of buffer.")
					 (setq address hexl-max-address))
				     (setq address (logior address 3)))
				 (if (> address hexl-max-address)
				     (progn
				       (message "End of buffer.")
				       (setq address hexl-max-address))
				   (setq address (+ address 4))))
			       (setq arg (1- arg)))
			     (if (> address hexl-max-address)
				 (progn
				   (message "End of buffer.")
				   (setq address hexl-max-address))
			       (setq address (logior address 3))))
			 (while (> arg 0)
			   (if (not (equal address (logand address -4)))
			       (setq address (logand address -4))
			     (if (not (equal address 0))
				 (setq address (- address 4))
			       (message "Beginning of buffer.")))
			   (setq arg (1- arg))))
		       address)))

(defun hexl-forward-short (arg)
  "Move right ARG shorts (left if ARG negative) in hexl-mode."
  (interactive "p")
  (hexl-backward-short (- arg)))

(defun hexl-backward-word (arg)
  "Move to left ARG words (right if ARG negative) in hexl-mode."
  (interactive "p")
  (hexl-goto-address (let ((address (hexl-current-address)))
		       (if (< arg 0)
			   (progn
			     (setq arg (- arg))
			     (while (> arg 0)
			       (if (not (equal address (logior address 7)))
				   (if (> address hexl-max-address)
				       (progn
					 (message "End of buffer.")
					 (setq address hexl-max-address))
				     (setq address (logior address 7)))
				 (if (> address hexl-max-address)
				     (progn
				       (message "End of buffer.")
				       (setq address hexl-max-address))
				   (setq address (+ address 8))))
			       (setq arg (1- arg)))
			     (if (> address hexl-max-address)
				 (progn
				   (message "End of buffer.")
				   (setq address hexl-max-address))
			       (setq address (logior address 7))))
			 (while (> arg 0)
			   (if (not (equal address (logand address -8)))
			       (setq address (logand address -8))
			     (if (not (equal address 0))
				 (setq address (- address 8))
			       (message "Beginning of buffer.")))
			   (setq arg (1- arg))))
		       address)))

(defun hexl-forward-word (arg)
  "Move right ARG words (left if ARG negative) in hexl-mode."
  (interactive "p")
  (hexl-backward-word (- arg)))

(defun hexl-previous-line (arg)
  "Move vertically up ARG lines [16 bytes] (down if ARG negative) in hexl-mode.
If there is byte at the target address move to the last byte in that line."
  (interactive "p")
  (hexl-next-line (- arg)))

(defun hexl-next-line (arg)
  "Move vertically down ARG lines [16 bytes] (up if ARG negative) in hexl-mode.
If there is no byte at the target address move to the last byte in that line."
  (interactive "p")
  (hexl-goto-address (let ((address (+ (hexl-current-address) (* arg 16))))
		       (if (and (< arg 0) (< address 0))
				(progn (message "Out of hexl region.")
				       (setq address
					     (% (hexl-current-address) 16)))
			 (if (and (> address hexl-max-address)
				  (< (% hexl-max-address 16) (% address 16)))
			     (setq address hexl-max-address)
			   (if (> address hexl-max-address)
			       (progn (message "Out of hexl region.")
				      (setq
				       address
				       (+ (logand hexl-max-address -16)
					  (% (hexl-current-address) 16)))))))
		       address)))

(defun hexl-beginning-of-buffer (arg)
  "Move to the beginning of the hexl buffer.
Leaves `hexl-mark' at previous position.
With prefix arg N, puts point N bytes of the way from the true beginning."
  (interactive "p")
  (push-mark (point))
  (hexl-goto-address (+ 0 (1- arg))))

(defun hexl-end-of-buffer (arg)
  "Go to `hexl-max-address' minus ARG."
  (interactive "p")
  (push-mark (point))
  (hexl-goto-address (- hexl-max-address (1- arg))))

(defun hexl-beginning-of-line ()
  "Goto beginning of line in hexl mode."
  (interactive)
  (let ((address (hexl-current-address)))
    (hexl-goto-address (- address (% address 16)))))

(defun hexl-end-of-line ()
  "Goto end of line in hexl mode."
  (interactive)
  (hexl-goto-address (let ((address (logior (hexl-current-address) 15)))
		       (if (> address hexl-max-address)
			   (setq address hexl-max-address))
		       address)))

(defun hexl-switch-position ()
  "Switch cursor from ASCII to HEX position or vice versa."
  (interactive)
  (goto-char
   (let ((address (hexl-current-address)))
     (if (hexl-at-ascii-position address)
	 (hexl-address-to-marker address)
       (hexl-ascii-position address)))))

(defun hexl-scroll-down (arg)
  "Scroll hexl buffer window upward ARG lines; or near full window if no ARG."
  (interactive "P")
  (if (null arg)
      (setq arg (1- (window-height)))
    (setq arg (prefix-numeric-value arg)))
  (hexl-scroll-up (- arg)))

(defun hexl-scroll-up (arg)
  "Scroll hexl buffer window upward ARG lines; or near full window if no ARG.
If there's no byte at the target address, move to the first or last line."
  (interactive "P")
  (if (null arg)
      (setq arg (1- (window-height)))
    (setq arg (prefix-numeric-value arg)))
  (let* ((movement (* arg 16))
	 (address (hexl-current-address))
	 (dest (+ address movement)))
    (cond
     ;; If possible, try to stay at the same offset from the beginning
     ;; of the 16-byte group, even if we move to the first or last
     ;; group.
     ((and (> dest hexl-max-address)
	   (>= (% hexl-max-address 16) (% address 16)))
      (setq dest (+ (logand hexl-max-address -16) (% address 16))))
     ((> dest hexl-max-address)
      (setq dest hexl-max-address))
     ((< dest 0)
      (setq dest (% address 16))))
    (if (/= dest (+ address movement))
	(message "Out of hexl region."))
    (hexl-goto-address dest)
    (recenter 0)))

(defun hexl-beginning-of-1k-page ()
  "Go to beginning of 1k boundary."
  (interactive)
  (hexl-goto-address (logand (hexl-current-address) -1024)))

(defun hexl-end-of-1k-page ()
  "Go to end of 1k boundary."
  (interactive)
  (hexl-goto-address (let ((address (logior (hexl-current-address) 1023)))
		       (if (> address hexl-max-address)
			   (setq address hexl-max-address))
		       address)))

(defun hexl-beginning-of-512b-page ()
  "Go to beginning of 512 byte boundary."
  (interactive)
  (hexl-goto-address (logand (hexl-current-address) -512)))

(defun hexl-end-of-512b-page ()
  "Go to end of 512 byte boundary."
  (interactive)
  (hexl-goto-address (let ((address (logior (hexl-current-address) 511)))
		       (if (> address hexl-max-address)
			   (setq address hexl-max-address))
		       address)))

(defun hexl-quoted-insert (arg)
  "Read next input character and insert it.
Useful for inserting control characters.
You may also type up to 3 octal digits, to insert a character with that code"
  (interactive "p")
  (hexl-insert-char (read-quoted-char) arg))

;00000000: 0011 2233 4455 6677 8899 aabb ccdd eeff  0123456789ABCDEF

;;;###autoload
(defun hexlify-buffer ()
  "Convert a binary buffer to hexl format.
This discards the buffer's undo information."
  (interactive)
  (and buffer-undo-list
       (or (y-or-n-p "Converting to hexl format discards undo info; ok? ")
	   (error "Aborted")))
  (setq buffer-undo-list nil)
  ;; Don't decode text in the ASCII part of `hexl' program output.
  (let ((coding-system-for-read 'raw-text)
	(coding-system-for-write 
	 (or (and (boundp 'buffer-file-coding-system)
		  buffer-file-coding-system)
	     'raw-text))
	(buffer-undo-list t))
    (shell-command-on-region (point-min) (point-max) hexlify-command t)
    (if (> (point) (hexl-address-to-marker hexl-max-address))
	(hexl-goto-address hexl-max-address))))

(defun dehexlify-buffer ()
  "Convert a hexl format buffer to binary.
This discards the buffer's undo information."
  (interactive)
  (and buffer-undo-list
       (or (y-or-n-p "Converting from hexl format discards undo info; ok? ")
	   (error "Aborted")))
  (setq buffer-undo-list nil)
  (let ((coding-system-for-write 'raw-text)
	(coding-system-for-read 
	 (or (and (boundp 'buffer-file-coding-system)
		  buffer-file-coding-system)
	     'raw-text))
	(buffer-undo-list t))
    (shell-command-on-region (point-min) (point-max) dehexlify-command t)))

(defun hexl-char-after-point ()
  "Return char for ASCII hex digits at point."
  (hexl-htoi (char-after (point))
	     (char-after (1+ (point)))))

(defun hexl-htoi (lh rh)
  "Hex (char) LH (char) RH to integer."
    (+ (* (hexl-hex-char-to-integer lh) 16)
       (hexl-hex-char-to-integer rh)))

(defun hexl-hex-char-to-integer (character)
  "Take a char and return its value as if it was a hex digit."
  (if (and (>= character ?0) (<= character ?9))
      (- character ?0)
    (let ((ch (logior character 32)))
      (if (and (>= ch ?a) (<= ch ?f))
	  (- ch (- ?a 10))
	(error "Invalid hex digit `%c'" ch)))))

(defun hexl-oct-char-to-integer (character)
  "Take a char and return its value as if it was a octal digit."
  (if (and (>= character ?0) (<= character ?7))
      (- character ?0)
    (error "Invalid octal digit `%c'" character)))

(defun hexl-printable-character (ch)
  "Return a displayable string for character CH."
  (format "%c" (if hexl-iso
		   (if (or (< ch 32) (and (>= ch 127) (< ch 160)))
		       46
		     ch)
		 (if (or (< ch 32) (>= ch 127))
		     46
		   ch))))

(defun hexl-self-insert-command (arg)
  "Insert this character."
  (interactive "p")
  (let ((col (current-column)))
    (if (and (>= col 10) ( <= col 48))
        (let ((point (point))
              (pos (% col 5)))
          (if (member pos '(1 3))
              (hexl-insert-char
               (hexl-hex-string-to-integer
                (concat (buffer-substring (- point 1) point)
                        (char-to-string last-command-char)))
               arg)
            (hexl-insert-char
             (hexl-hex-string-to-integer
              (concat (char-to-string last-command-char)
                      (buffer-substring (+ point 1) (+ point 2))))
             arg)
            (goto-char (1+ point))))
      (if (not last-command-char)
	  (setq last-command-char
		(event-to-character last-input-event t t t)))
      (hexl-insert-char last-command-char arg))))

(defun hexl-insert-char (ch num)
  "Insert a character in a hexl buffer."
  (let ((address (hexl-current-address t)))
    (while (> num 0)
      (let* ((hex-position (hexl-hex-position address))
	     (ascii-position (hexl-ascii-position address)))
	(save-excursion
	  (goto-char hex-position)
	  (delete-char 2)
	  (insert (format "%02x" ch))
	  (goto-char ascii-position)
	  (delete-char 1)
	  (insert (hexl-printable-character ch))
	  (or (eq address hexl-max-address)
	      (setq address (1+ address))))
	(hexl-goto-address address))
      (setq num (1- num)))))

;; hex conversion

(defun hexl-insert-hex-char (arg)
  "Insert a ASCII char ARG times at point for a given hexadecimal number."
  (interactive "p")
  (let ((num (hexl-hex-string-to-integer (read-string "Hex number: "))))
    (if (or (> num 255) (< num 0))
	(error "Hex number out of range")
      (hexl-insert-char num arg))))

(defun hexl-insert-hex-string (str arg)
  "Insert hexadecimal string STR at point ARG times.
Embedded whitespace, dashes, and periods in the string are ignored."
  (interactive "sHex string: \np")
  (setq str (replace-in-string str "[- \t.]" ""))
  (let ((chars '()))
    (let ((len (length str))
	  (idx 0))
      (if (eq (logand len 1) 1)
	  (let ((num (hexl-hex-string-to-integer (substring str 0 1))))
	    (setq chars (cons num chars))
	    (setq idx 1)))
      (while (< idx len)
	(let* ((nidx (+ idx 2))
	       (num (hexl-hex-string-to-integer (substring str idx nidx))))
	  (setq chars (cons num chars))
	  (setq idx nidx))))
    (setq chars (nreverse chars))
    (while (> arg 0)
      (let ((chars chars))
	(while chars
	  (hexl-insert-char (car chars) 1)
	  (setq chars (cdr chars))))
      (setq arg (- arg 1)))))

(defun hexl-insert-decimal-char (arg)
  "Insert a ASCII char ARG times at point for a given decimal number."
  (interactive "p")
  (let ((num (string-to-int (read-string "Decimal Number: "))))
    (if (or (> num 255) (< num 0))
	(error "Decimal number out of range")
      (hexl-insert-char num arg))))

(defun hexl-insert-octal-char (arg)
  "Insert a ASCII char ARG times at point for a given octal number."
  (interactive "p")
  (let ((num (hexl-octal-string-to-integer (read-string "Octal Number: "))))
    (if (or (> num 255) (< num 0))
	(error "Decimal number out of range")
      (hexl-insert-char num arg))))

(defun hexl-follow-ascii (&optional arg)
  "Toggle following ASCII in Hexl buffers.
With prefix ARG, turn on following if and only if ARG is positive.
When following is enabled, the ASCII character corresponding to the
element under the point is highlighted.
Customize the variable `hexl-follow-ascii' to disable this feature."
  (interactive "P")
  (let ((on-p (if arg 
		  (> (prefix-numeric-value arg) 0)
	       (not hexl-ascii-extent))))

    (make-local-hook 'post-command-hook)
		    
    (if on-p
      ;; turn it on
      (if (not hexl-ascii-extent)
	  (progn
	    (setq hexl-ascii-extent (make-extent 1 1)
		  hexl-follow-ascii t)
	    (set-extent-property hexl-ascii-extent 'face 'highlight)
	    (add-hook 'post-command-hook 'hexl-follow-ascii-find nil t)))
      ;; turn it off
      (if hexl-ascii-extent
	  (progn
	    (detach-extent hexl-ascii-extent)
	    (setq hexl-ascii-extent nil
		  hexl-follow-ascii nil)
	    (remove-hook 'post-command-hook 'hexl-follow-ascii-find t)
	    )))))

(defun hexl-follow-ascii-find ()
  "Find and highlight the ASCII/HEX element corresponding to current point."
  (setq hexl-current-address (hexl-current-address))
  (condition-case nil
      (let ((col (current-column))
	    (point (point))
	    start end)
	(if (> col 48)
	    (setq start (+ 10 (- point col) (/ (- col 51) 2)
			   (* 2 (mod (hexl-current-address) 16)))
		  end (+ start 2))
	  (setq start (+ 51 (- point col) (mod hexl-current-address 16))
		end (+ start 1)))
	(set-extent-endpoints hexl-ascii-extent start end))
    (error nil))
  (setq hexl-current-address
        (format "0x%x=%d" hexl-current-address hexl-current-address)))

;; startup stuff.

(if hexl-mode-map
    nil
  (setq hexl-mode-map (make-sparse-keymap))

  (define-key hexl-mode-map [left] 'hexl-backward-char)
  (define-key hexl-mode-map [right] 'hexl-forward-char)
  (define-key hexl-mode-map [up] 'hexl-previous-line)
  (define-key hexl-mode-map [down] 'hexl-next-line)
  (define-key hexl-mode-map [(meta left)] 'hexl-backward-short)
  (define-key hexl-mode-map [(meta right)] 'hexl-forward-short)
  (define-key hexl-mode-map [next] 'hexl-scroll-up)
  (define-key hexl-mode-map [prior] 'hexl-scroll-down)
  (define-key hexl-mode-map [home] 'hexl-beginning-of-line)
  (define-key hexl-mode-map [end] 'hexl-end-of-line)
  (define-key hexl-mode-map [(control home)] 'hexl-beginning-of-buffer)
  (define-key hexl-mode-map [(control end)] 'hexl-end-of-buffer)
  (define-key hexl-mode-map [deletechar] 'undefined)
  (define-key hexl-mode-map [deleteline] 'undefined)
  (define-key hexl-mode-map [insertline] 'undefined)
  (define-key hexl-mode-map [(shift backspace)] 'undefined)
  (define-key hexl-mode-map [(shift delete)] 'undefined)
  (define-key hexl-mode-map 'backspace 'undefined)
  (define-key hexl-mode-map 'delete 'undefined)

  (define-key hexl-mode-map "\C-a" 'hexl-beginning-of-line)
  (define-key hexl-mode-map "\C-b" 'hexl-backward-char)
  (define-key hexl-mode-map "\C-d" 'undefined)
  (define-key hexl-mode-map "\C-e" 'hexl-end-of-line)
  (define-key hexl-mode-map "\C-f" 'hexl-forward-char)

  (if (not (eq (key-binding (vector help-char)) 'help-command))
      (define-key hexl-mode-map (vector help-char) 'undefined))

  (define-key hexl-mode-map "\C-i" 'hexl-self-insert-command)
  (define-key hexl-mode-map "\C-j" 'hexl-self-insert-command)
  (define-key hexl-mode-map "\C-k" 'undefined)
  (define-key hexl-mode-map "\C-m" 'hexl-self-insert-command)
  (define-key hexl-mode-map "\C-n" 'hexl-next-line)
  (define-key hexl-mode-map "\C-o" 'undefined)
  (define-key hexl-mode-map "\C-p" 'hexl-previous-line)
  (define-key hexl-mode-map "\C-q" 'hexl-quoted-insert)
  (define-key hexl-mode-map "\C-t" 'undefined)
  (define-key hexl-mode-map "\C-v" 'hexl-scroll-up)
  (define-key hexl-mode-map "\C-w" 'undefined)
  (define-key hexl-mode-map "\C-y" 'undefined)

  (let ((ch 32))
    (while (< ch 127)
      (define-key hexl-mode-map (format "%c" ch) 'hexl-self-insert-command)
      (setq ch (1+ ch))))

  (define-key hexl-mode-map "\e\C-a" 'hexl-beginning-of-512b-page)
  (define-key hexl-mode-map "\e\C-b" 'hexl-backward-short)
  (define-key hexl-mode-map "\e\C-d" 'hexl-insert-decimal-char)
  (define-key hexl-mode-map "\e\C-e" 'hexl-end-of-512b-page)
  (define-key hexl-mode-map "\e\C-f" 'hexl-forward-short)
  (define-key hexl-mode-map "\e\C-i" 'undefined)
  (define-key hexl-mode-map "\e\C-j" 'undefined)
  (define-key hexl-mode-map "\e\C-k" 'undefined)
  (define-key hexl-mode-map "\e\C-o" 'hexl-insert-octal-char)
  (define-key hexl-mode-map "\e\C-q" 'undefined)
  (define-key hexl-mode-map "\e\C-t" 'undefined)
  (define-key hexl-mode-map "\e\C-x" 'hexl-insert-hex-char)
  (define-key hexl-mode-map "\eb" 'hexl-backward-word)
  (define-key hexl-mode-map "\ec" 'undefined)
  (define-key hexl-mode-map "\ed" 'undefined)
  (define-key hexl-mode-map "\ef" 'hexl-forward-word)
  (define-key hexl-mode-map "\eg" 'hexl-goto-hex-address)
  (define-key hexl-mode-map "\ei" 'undefined)
  (define-key hexl-mode-map "\ej" 'hexl-goto-address)
  (define-key hexl-mode-map "\ek" 'undefined)
  (define-key hexl-mode-map "\el" 'undefined)
  (define-key hexl-mode-map "\eq" 'undefined)
  (define-key hexl-mode-map "\es" 'undefined)
  (define-key hexl-mode-map "\et" 'undefined)
  (define-key hexl-mode-map "\eu" 'undefined)
  (define-key hexl-mode-map "\ev" 'hexl-scroll-down)
  (define-key hexl-mode-map "\ey" 'undefined)
  (define-key hexl-mode-map "\ez" 'undefined)
  (define-key hexl-mode-map "\e<" 'hexl-beginning-of-buffer)
  (define-key hexl-mode-map "\e>" 'hexl-end-of-buffer)

  (define-key hexl-mode-map "\C-c\C-c" 'hexl-mode-exit)

  (define-key hexl-mode-map "\C-x[" 'hexl-beginning-of-1k-page)
  (define-key hexl-mode-map "\C-x]" 'hexl-end-of-1k-page)
  (define-key hexl-mode-map "\C-x\C-p" 'undefined)
  (define-key hexl-mode-map "\C-x\C-s" 'hexl-save-buffer)
  (define-key hexl-mode-map "\C-x\C-t" 'undefined)

  (define-key hexl-mode-map [tab] 'hexl-switch-position))

(provide 'hexl)

;;; hexl.el ends here
