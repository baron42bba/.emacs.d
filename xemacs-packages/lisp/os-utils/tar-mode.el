;;; tar-mode.el --- simple editing of tar files from GNU emacs

;; Copyright (C) 1990-1993, 1997 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@jwz.org>
;; Keywords: unix
;; Created: 4 Apr 1990
;; Version: 1.32

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

;;; Synched up with: partially synched with Emacs 20.

;;; Commentary:

;; This package attempts to make dealing with Unix 'tar' archives easier.
;; When this code is loaded, visiting a file whose name ends in '.tar' will
;; cause the contents of that archive file to be displayed in a Dired-like
;; listing.  It is then possible to use the customary Dired keybindings to
;; extract sub-files from that archive, either by reading them into their own
;; editor buffers, or by copying them directly to arbitrary files on disk.
;; It is also possible to delete sub-files from within the tar file and write
;; the modified archive back to disk, or to edit sub-files within the archive
;; and re-insert the modified files into the archive.  See the documentation
;; string of tar-mode for more info.

;; To autoload, add this to your .emacs file:
;;
;;  (setq auto-mode-alist (cons '("\\.tar$" . tar-mode) auto-mode-alist))
;;  (autoload 'tar-mode "tar-mode")
;;
;; But beware: for certain tar files - those whose very first file has 
;; a -*- property line - autoloading won't work.  See the function 
;; "tar-normal-mode" to understand why.

;; This code now understands the extra fields that GNU tar adds to tar files.

;; This interacts correctly with "uncompress.el" in the Emacs library,
;; and with sufficiently recent versions of "crypt.el" by Kyle Jones.

;;    ***************   TO DO   *************** 
;;
;; o  chmod should understand "a+x,og-w".
;;
;; o  It's not possible to add a NEW file to a tar archive; not that 
;;    important, but still...
;;
;; o  The code is less efficient that it could be - in a lot of places, I
;;    pull a 512-character string out of the buffer and parse it, when I could
;;    be parsing it in place, not garbaging a string.  Should redo that.
;;
;; o  I'd like a command that searches for a string/regexp in every subfile
;;    of an archive, where <esc> would leave you in a subfile-edit buffer.
;;    (Like M-s in VM and M-r in the Zmacs mail reader.)
;;
;; o  Sometimes (but not always) reverting the tar-file buffer does not 
;;    re-grind the listing, and you are staring at the binary tar data.
;;    Typing 'g' again immediately after that will always revert and re-grind
;;    it, though.  I have no idea why this happens.
;;
;; o  Tar-mode interacts poorly with crypt.el and zcat.el because the tar
;;    write-file-hook actually writes the file.  Instead it should remove the
;;    header (and conspire to put it back afterwards) so that other write-file
;;    hooks which frob the buffer have a chance to do their dirty work.  There
;;    might be a problem if the tar write-file-hook does not come *first* on
;;    the list.
;;
;; o  Block files, sparse files, continuation files, and the various header
;;    types aren't editable.  Actually I don't know that they work at all.
;;    If you know that they work, or know that they don't, please let me know.
;;
;; o  Tar files inside of tar files don't work.
;;
;; o  When using crypt-mode, you can't save a compressed or encrypted subfile
;;    of a tar file back into the tar file: it is saved uncompressed.

;;; Code:

(defgroup tar ()
  "Simple editing of tar files from GNU emacs."
  :group 'unix
  :group 'data)


(defcustom tar-anal-blocksize 20
  "*The blocksize of tar files written by Emacs, or nil, meaning don't care.
The blocksize of a tar file is not really the size of the blocks; rather, it is
the number of blocks written with one system call.  When tarring to a tape, 
this is the size of the *tape* blocks, but when writing to a file, it doesn't
matter much.  The only noticeable difference is that if a tar file does not
have a blocksize of 20, the tar program will issue a warning; all this really
controls is how many null padding bytes go on the end of the tar file."
  :type 'integer
  :group 'tar)

(defcustom tar-update-datestamp t
  "*Whether tar-mode should play fast and loose with sub-file datestamps;
if this is true, then editing and saving a tar file entry back into its
tar file will update its datestamp.  If false, the datestamp is unchanged.
You may or may not want this - it is good in that you can tell when a file
in a tar archive has been changed, but it is bad for the same reason that
editing a file in the tar archive at all is bad - the changed version of 
the file never exists on disk.

This does not work in Emacs 18, because there's no way to get the current 
time as an integer - if this var is true, then editing a file sets its date
to midnight, Jan 1 1970 GMT, which happens to be what 0 encodes."
  :type 'boolean
  :group 'tar)


;;; First, duplicate some Common Lisp functions; I used to just (require 'cl)
;;; but "cl.el" was messing some people up (also it's really big).

;; No need for that stuff anymore -- XEmacs preloads cl.el anyway.


;;; down to business.

(defmacro tar-make-header (name mode uid git size date ck lt ln
				magic uname gname devmaj devmin)
  (list 'vector name mode uid git size date ck lt ln
	magic uname gname devmaj devmin))

(defmacro tar-header-name (x) (list 'aref x 0))
(defmacro tar-header-mode (x) (list 'aref x 1))
(defmacro tar-header-uid  (x) (list 'aref x 2))
(defmacro tar-header-gid  (x) (list 'aref x 3))
(defmacro tar-header-size (x) (list 'aref x 4))
(defmacro tar-header-date (x) (list 'aref x 5))
(defmacro tar-header-checksum  (x) (list 'aref x 6))
(defmacro tar-header-link-type (x) (list 'aref x 7))
(defmacro tar-header-link-name (x) (list 'aref x 8))
(defmacro tar-header-magic (x) (list 'aref x 9))
(defmacro tar-header-uname (x) (list 'aref x 10))
(defmacro tar-header-gname (x) (list 'aref x 11))
(defmacro tar-header-dmaj (x) (list 'aref x 12))
(defmacro tar-header-dmin (x) (list 'aref x 13))

(defmacro tar-make-desc (data-start tokens)
  (list 'cons data-start tokens))

(defmacro tar-desc-data-start (x) (list 'car x))
(defmacro tar-desc-tokens     (x) (list 'cdr x))

(defconst tar-name-offset 0)
(defconst tar-mode-offset (+ tar-name-offset 100))
(defconst tar-uid-offset  (+ tar-mode-offset 8))
(defconst tar-gid-offset  (+ tar-uid-offset 8))
(defconst tar-size-offset (+ tar-gid-offset 8))
(defconst tar-time-offset (+ tar-size-offset 12))
(defconst tar-chk-offset  (+ tar-time-offset 12))
(defconst tar-linkp-offset (+ tar-chk-offset 8))
(defconst tar-link-offset (+ tar-linkp-offset 1))
;;; GNU-tar specific slots.
(defconst tar-magic-offset (+ tar-link-offset 100))
(defconst tar-uname-offset (+ tar-magic-offset 8))
(defconst tar-gname-offset (+ tar-uname-offset 32))
(defconst tar-dmaj-offset (+ tar-gname-offset 32))
(defconst tar-dmin-offset (+ tar-dmaj-offset 8))
(defconst tar-end-offset (+ tar-dmin-offset 8))

(defun tar-tokenize-header-block (string)
  "Return a `tar-header' structure.
This is a list of name, mode, uid, gid, size, 
write-date, checksum, link-type, and link-name."
  (cond ((< (length string) 512) nil)
	(;(some 'plusp string)		 ; <-- oops, massive cycle hog!
	 (or (not (= 0 (aref string 0))) ; This will do.
	     (not (= 0 (aref string 101))))
	 (let* ((name-end (1- tar-mode-offset))
		(link-end (1- tar-magic-offset))
		(uname-end (1- tar-gname-offset))
		(gname-end (1- tar-dmaj-offset))
		(link-p (aref string tar-linkp-offset))
		(magic-str (substring string tar-magic-offset (1- tar-uname-offset)))
		(uname-valid-p (or (string= "ustar  " magic-str) (string= "GNUtar " magic-str)))
		name
		(nulsexp   "[^\000]*\000"))
	   (and (string-match nulsexp string tar-name-offset) (setq name-end (min name-end (1- (match-end 0)))))
	   (and (string-match nulsexp string tar-link-offset) (setq link-end (min link-end (1- (match-end 0)))))
	   (and (string-match nulsexp string tar-uname-offset) (setq uname-end (min uname-end (1- (match-end 0)))))
	   (and (string-match nulsexp string tar-gname-offset) (setq gname-end (min gname-end (1- (match-end 0)))))
	   (setq name (substring string tar-name-offset name-end)
		 link-p (if (or (= link-p 0) (= link-p ?0))
			    nil
			  (- link-p ?0)))
	   (if (and (null link-p) (string-match "/$" name)) (setq link-p 5)) ; directory
	   (tar-make-header
	    name
	    (tar-parse-octal-integer string tar-mode-offset (1- tar-uid-offset))
	    (tar-parse-octal-integer string tar-uid-offset (1- tar-gid-offset))
	    (tar-parse-octal-integer string tar-gid-offset (1- tar-size-offset))
	    (tar-parse-octal-integer string tar-size-offset (1- tar-time-offset))
	    (tar-parse-octal-integer-32 string tar-time-offset (1- tar-chk-offset))
	    (tar-parse-octal-integer string tar-chk-offset (1- tar-linkp-offset))
	    link-p
	    (substring string tar-link-offset link-end)
	    uname-valid-p
	    (and uname-valid-p (substring string tar-uname-offset uname-end))
	    (and uname-valid-p (substring string tar-gname-offset gname-end))
	    (tar-parse-octal-integer string tar-dmaj-offset (1- tar-dmin-offset))
	    (tar-parse-octal-integer string tar-dmin-offset (1- tar-end-offset))
	    )))
	(t 'empty-tar-block)))


(defun tar-parse-octal-integer (string &optional start end)
  (if (null start) (setq start 0))
  (if (null end) (setq end (length string)))
  (if (= (aref string start) 0)
      0
    (let ((n 0))
      (while (< start end)
	(setq n (if (< (aref string start) ?0) n
		  (+ (* n 8) (- (aref string start) 48)))
	      start (1+ start)))
      n)))

(defun tar-parse-octal-integer-32 (string &optional start end)
  ;; like tar-parse-octal-integer, but returns a cons of two 16-bit numbers,
  ;; since elisp can't handle integers of that magnitude.
  (or start (setq start 0))
  (or end (setq end (length string)))
  (let ((top (tar-parse-octal-integer string start (- end 6)))
	(bot (tar-parse-octal-integer string (- end 6) end)))
    (setq top (logior (ash top 2) (ash bot -16)))
    (setq bot (logand bot 65535))
    (cons top bot)))

(defun tar-parse-octal-integer-safe (string)
  (let ((L (length string)))
    (if (= L 0) (error "empty string"))
    (dotimes (i L)
      (if (or (< (aref string i) ?0)
	      (> (aref string i) ?7))
	  (error "'%c' is not an octal digit."))))
  (tar-parse-octal-integer string))


(defun tar-header-block-checksum (string)
  "Compute and return a tar-acceptable checksum for this block."
  (let* ((chk-field-start tar-chk-offset)
	 (chk-field-end (+ chk-field-start 8))
	 (sum 0)
	 (i 0))
    ;; Add up all of the characters except the ones in the checksum field.
    ;; Add that field as if it were filled with spaces.
    (while (< i chk-field-start)
      (setq sum (+ sum (aref string i))
	    i (1+ i)))
    (setq i chk-field-end)
    (while (< i 512)
      (setq sum (+ sum (aref string i))
	    i (1+ i)))
    (+ sum (* 32 8))))

(defun tar-header-block-check-checksum (hblock desired-checksum file-name)
  "Beep and print a warning if the checksum doesn't match."
  (if (not (= desired-checksum (tar-header-block-checksum hblock)))
      (progn (beep) (message "Invalid checksum for file %s!" file-name))))

(defun tar-header-block-recompute-checksum (hblock)
  "Modifies the given string to have a valid checksum field."
  (let* ((chk (tar-header-block-checksum hblock))
	 (chk-string (format "%6o" chk))
	 (l (length chk-string)))
    (aset hblock 154 0)
    (aset hblock 155 32)
    (dotimes (i l) (aset hblock (- 153 i) (aref chk-string (- l i 1)))))
  hblock)


(defun tar-grind-file-mode (mode string start)
  "Store `-rw--r--r--' indicating MODE into STRING beginning at START.
MODE should be an integer which is a file mode value."
  (aset string start       (if (zerop (logand 256 mode)) ?- ?r))
  (aset string (+ start 1) (if (zerop (logand 128 mode)) ?- ?w))
  (aset string (+ start 2) (if (zerop (logand  64 mode)) ?- ?x)) 
  (aset string (+ start 3) (if (zerop (logand  32 mode)) ?- ?r))
  (aset string (+ start 4) (if (zerop (logand  16 mode)) ?- ?w))
  (aset string (+ start 5) (if (zerop (logand   8 mode)) ?- ?x))
  (aset string (+ start 6) (if (zerop (logand   4 mode)) ?- ?r))
  (aset string (+ start 7) (if (zerop (logand   2 mode)) ?- ?w))
  (aset string (+ start 8) (if (zerop (logand   1 mode)) ?- ?x))
  (if (zerop (logand 1024 mode)) nil (aset string (+ start 2) ?s))
  (if (zerop (logand 2048 mode)) nil (aset string (+ start 5) ?s))
  string)


(defconst tar-can-print-dates (or (fboundp 'current-time)
				  (fboundp 'current-time-seconds))
  "true if this emacs has been built with time-printing support")

(defun tar-summarize-header-block (tar-hblock &optional mod-p)
  "Returns a line similar to the output of `tar -vtf'."
  (let ((name (tar-header-name tar-hblock))
	(mode (tar-header-mode tar-hblock))
	(uid (tar-header-uid tar-hblock))
	(gid (tar-header-gid tar-hblock))
	(uname (tar-header-uname tar-hblock))
	(gname (tar-header-gname tar-hblock))
	(size (tar-header-size tar-hblock))
	(time (tar-header-date tar-hblock))
	(ck (tar-header-checksum tar-hblock))
	(link-p (tar-header-link-type tar-hblock))
	(link-name (tar-header-link-name tar-hblock))
	)
    (let* ((left 11)
	   (namew 8)
	   (groupw 8)
	   (sizew 8)
	   (datew (if tar-can-print-dates 15 2))
	   (slash (1- (+ left namew)))
	   (lastdigit (+ slash groupw sizew))
	   (namestart (+ lastdigit datew))
	   (string (make-string (+ namestart (length name) (if link-p (+ 5 (length link-name)) 0)) 32))
	   (type (tar-header-link-type tar-hblock)))
      (aset string 0 (if mod-p ?* ? ))
      (aset string 1
	    (cond ((or (eq type nil) (eq type 0)) ?-)
		  ((eq type 1) ?l)	; link
		  ((eq type 2) ?s)	; symlink
		  ((eq type 3) ?c)	; char special
		  ((eq type 4) ?b)	; block special
		  ((eq type 5) ?d)	; directory
		  ((eq type 6) ?p)	; FIFO/pipe
		  ((eq type 20) ?*)	; directory listing
		  ((eq type 29) ?M)	; multivolume continuation
		  ((eq type 35) ?S)	; sparse
		  ((eq type 38) ?V)	; volume header
		  ))
      (tar-grind-file-mode mode string 2)
      (setq uid (if (= 0 (length uname)) (int-to-string uid) uname))
      (setq gid (if (= 0 (length gname)) (int-to-string gid) gname))
      (setq size (int-to-string size))
      (dotimes (i (min (1- namew) (length uid))) (aset string (- slash i) (aref uid (- (length uid) i 1))))
      (aset string (1+ slash) ?/)
      (dotimes (i (min (1- groupw) (length gid))) (aset string (+ (+ slash 2) i) (aref gid i)))
      (dotimes (i (min sizew (length size))) (aset string (- lastdigit i) (aref size (- (length size) i 1))))

      (if tar-can-print-dates
	  (let* ((year (substring (current-time-string) -4))
		 ;; in v18, current-time-string doesn't take an argument
		 (file (current-time-string time))
		 (file-year (substring file -4))
		 (str (if (equal year file-year)
			  (substring file 4 16)
			(concat (substring file 4 11) " " file-year))))
	    (dotimes (i 12) (aset string (- namestart (- 13 i)) (aref str i)))))

      (dotimes (i (length name)) (aset string (+ namestart i) (aref name i)))
      (if (or (eq link-p 1) (eq link-p 2))
	  (progn
	    (dotimes (i 3) (aset string (+ namestart 1 (length name) i) (aref (if (= link-p 1) "==>" "-->") i)))
	    (dotimes (i (length link-name)) (aset string (+ namestart 5 (length name) i) (aref link-name i)))))
      string)))


;; buffer-local variables in the tar file's buffer:
;;
(defvar tar-parse-info)			; the header structures
(defvar tar-header-offset)		; the end of the "pretty" data

(defun tar-summarize-buffer ()
  "Parse the contents of the tar file in the current buffer, and place a
dired-like listing on the front; then narrow to it, so that only that listing
is visible (and the real data of the buffer is hidden)."
  (message "parsing tar file...")
  (let* ((result '())
	 (pos 1)
	 (bs (max 1 (- (buffer-size) 1024))) ; always 2+ empty blocks at end.
	 (bs100 (max 1 (/ bs 100)))
	 (tokens nil))
    (while (not (eq tokens 'empty-tar-block))
      (if (> (+ pos 512) (point-max))
	  (error "truncated tar file"))
      (let* ((hblock (buffer-substring pos (+ pos 512))))
	(setq tokens (tar-tokenize-header-block hblock))
	(setq pos (+ pos 512))
	(message "parsing tar file...%s%%"
				;(/ (* pos 100) bs)   ; this gets round-off lossage
		 (/ pos bs100)		; this doesn't
		 )
	(if (eq tokens 'empty-tar-block)
	    nil
	  (if (null tokens) (error "premature EOF parsing tar file."))
	  (if (eq (tar-header-link-type tokens) 20)
	      ;; Foo.  There's an extra empty block after these.
	      (setq pos (+ pos 512)))
	  (let ((size (tar-header-size tokens)))
	    (if (< size 0)
		(error "%s has size %s - corrupted."
		       (tar-header-name tokens) size))
			;
			; This is just too slow.  Don't really need it anyway....
			;(tar-check-header-block-checksum
			;  hblock (tar-checksum-header-block hblock)
			;  (tar-header-name tokens))
	    
	    (setq result (cons (tar-make-desc pos tokens) result))
	    
	    (if (and (null (tar-header-link-type tokens))
		     (> size 0))
		(setq pos
		      (+ pos 512 (ash (ash (1- size) -9) 9)) ; this works
			;(+ pos (+ size (- 512 (rem (1- size) 512)))) ; this doesn't
		      ))
	    ))))
    (make-local-variable 'tar-parse-info)
    (setq tar-parse-info (nreverse result)))
  (message "parsing tar file...formatting...")
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-read-only nil))
      (dolist (tar-desc tar-parse-info)
	(insert
	 (tar-summarize-header-block (tar-desc-tokens tar-desc))
	 "\n"))
      (make-local-variable 'tar-header-offset)
      (setq tar-header-offset (point))
      (narrow-to-region 1 tar-header-offset)
      (set-buffer-modified-p nil)))
  (message "parsing tar file...done."))


(defvar tar-mode-map nil "*Local keymap for tar-mode listings.")

(if tar-mode-map
    nil
  (setq tar-mode-map (make-keymap))
  (suppress-keymap tar-mode-map)
  ;; Commands to mark certain categories of files
  ;; Upper case keys for operating on the marked files
  (define-key tar-mode-map "C" 'tar-copy)
  (define-key tar-mode-map "R" 'tar-rename-entry)
  (define-key tar-mode-map "M" 'tar-chmod-entry)
  (define-key tar-mode-map "G" 'tar-chgrp-entry)
  (define-key tar-mode-map "O" 'tar-chown-entry)
  ;; Lower keys for commands not operating on all the marked files
  (define-key tar-mode-map "d" 'tar-flag-deleted)
  (define-key tar-mode-map "\^D" 'tar-flag-deleted)
  (define-key tar-mode-map "e" 'tar-extract)
  (define-key tar-mode-map "f" 'tar-extract)
  (define-key tar-mode-map [return] 'tar-extract)
  (define-key tar-mode-map "g" 'revert-buffer)
  (define-key tar-mode-map "h" 'describe-mode)
  (define-key tar-mode-map "o" 'tar-extract-other-window)
  (define-key tar-mode-map "q" 'tar-quit)
  (define-key tar-mode-map "u" 'tar-unflag)
  (define-key tar-mode-map "v" 'tar-view)
  (define-key tar-mode-map "x" 'tar-expunge)
  (define-key tar-mode-map 'backspace 'tar-unflag-backwards)
  (define-key tar-mode-map 'delete 'tar-unflag-backwards)
  (define-key tar-mode-map "E" 'tar-extract-other-window)
  ;; moving
  (define-key tar-mode-map " " 'tar-next-line)
  (define-key tar-mode-map "n" 'tar-next-line)
  (define-key tar-mode-map "\^N" 'tar-next-line)
  (define-key tar-mode-map [down] 'tar-next-line)
  (define-key tar-mode-map "p" 'tar-previous-line)
  (define-key tar-mode-map "\^P" 'tar-previous-line)
  (define-key tar-mode-map [up] 'tar-previous-line)

  (cond ((and (featurep 'xemacs)
	      (not (featurep 'infodock)))
	 (define-key tar-mode-map 'button2 'tar-track-mouse-and-extract-file)
	 (define-key tar-mode-map 'button3 'tar-popup-menu)))
  )


;; XEmacs menu mouse/support added by Heiko Muenkel
;; muenkel@tnt.uni-hannover.de

(autoload 'dired-mark-region "dired-xemacs-menu")

(defvar tar-menu
  '("Tar Mode Commands"
    ["Copy Subfile to Disk" tar-copy t]
    ["Rename Subfile" tar-rename-entry t]
    "----"
    ["Delete Flaged Subfiles" tar-expunge t]
    ["Flag Subfile for Deletion" tar-flag-deleted t]
    ["Flag Subfiles in Region for Deletion"
     (dired-mark-region '(tar-flag-deleted 1))
     (mark)]
    ["Unflag Subfile" tar-unflag t]
    ["Unflag Subfiles in Region"
     (dired-mark-region '(tar-flag-deleted 1 t))
     (mark)]
    "----"
    ["Change Permissions of Subfile..." tar-chmod-entry t]
    ["Change Group of Subfile..." tar-chgrp-entry t]
    ["Change Owner of Subfile..." tar-chown-entry t]
    "----"
    ["Edit Subfile Other Window" tar-extract-other-window t]
    ["Edit Subfile" tar-extract t]
    ["View Subfile" tar-view t]
    "----"
    ["Quit Tar Mode" tar-quit t]
    ))


(defun tar-track-mouse-and-extract-file (event)
  "Visit the tar-file-entry upon which the mouse is clicked."
  (interactive "e")
  (mouse-set-point event)
  (tar-next-line 0)
  (let (buffer)
    (save-excursion
      (tar-extract)
      (setq buffer (current-buffer)))
    (switch-to-buffer buffer)))

(defun tar-popup-menu (event)
  "Display the tar-mode menu."
  (interactive "@e")
  (mouse-set-point event)
  (tar-next-line 0)
  (popup-menu tar-menu))


;; tar mode is suitable only for specially formatted data.
(put 'tar-mode 'mode-class 'special)
(put 'tar-subfile-mode 'mode-class 'special)

;;;###autoload
(defun tar-mode ()
  "Major mode for viewing a tar file as a dired-like listing of its contents.
You can move around using the usual cursor motion commands. 
Letters no longer insert themselves.
Type `e' to pull a file out of the tar file and into its own buffer;
or click mouse-2 on the file's line in the Tar mode buffer.
Type `c' to copy an entry from the tar file into another file on disk.

If you edit a sub-file of this archive (as with the `e' command) and 
save it with Control-x Control-s, the contents of that buffer will be 
saved back into the tar-file buffer; in this way you can edit a file 
inside of a tar archive without extracting it and re-archiving it.

See also: variables `tar-update-datestamp' and `tar-anal-blocksize'.
\\{tar-mode-map}"
  ;; this is not interactive because you shouldn't be turning this
  ;; mode on and off.  You can corrupt things that way.
  (make-local-variable 'tar-header-offset)
  (make-local-variable 'tar-parse-info)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil)	; binary data, dude...
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'tar-mode-revert)
  (setq major-mode 'tar-mode)
  (setq mode-name "Tar")
  (use-local-map tar-mode-map)
  (auto-save-mode 0)
  (widen)
  (if (and (boundp 'tar-header-offset) tar-header-offset)
      (narrow-to-region 1 tar-header-offset)
    (tar-summarize-buffer))
  (cond ((string-match "XEmacs" emacs-version)
	 (require 'mode-motion)
	 (setq mode-motion-hook 'mode-motion-highlight-line)
	 (when (and (boundp 'current-menubar)
		    current-menubar
		    (not (assoc "Tar" current-menubar)))
	   (set-buffer-menubar (copy-sequence current-menubar))
	   (add-menu nil "Tar" (cdr tar-menu)))
	 ))
  (run-hooks 'tar-mode-hook))

;; buffer-local variables in subfile mode.
;;
(defvar tar-subfile-mode nil)		; whether the minor-mode is on
(defvar tar-superior-buffer)		; parent buffer
(defvar tar-superior-descriptor)	; header object of this file
(defvar tar-subfile-buffer-id)		; pretty name-string
(defvar subfile-orig-mlbid)		; orig mode-line-buffer-identification

(defun tar-subfile-mode (p)
  "Minor mode for editing an element of a tar-file.
This mode arranges for \"saving\" this buffer to write the data
into the tar-file buffer that it came from.  The changes will actually
appear on disk when you save the tar-file's buffer."
  (interactive "P")
  (or (and (boundp 'tar-superior-buffer) tar-superior-buffer)
      (error "This buffer is not an element of a tar file."))
  (or (assq 'tar-subfile-mode minor-mode-alist)
      (setq minor-mode-alist (append minor-mode-alist
				     (list '(tar-subfile-mode " TarFile")))))
  (make-local-variable 'tar-subfile-mode)
  (setq tar-subfile-mode
	(if (null p)
	    (not tar-subfile-mode)
	  (> (prefix-numeric-value p) 0)))
  (cond (tar-subfile-mode
	 ;; copy the local keymap so that we don't accidentally
	 ;; alter a keymap like 'lisp-mode-map' which is shared
	 ;; by all buffers in that mode.
	 (let ((m (current-local-map)))
	   (if m (use-local-map (copy-keymap m))))
	 (local-set-key "\^X\^S" 'tar-subfile-save-buffer)
	 ;; turn off auto-save.
	 (setq buffer-auto-save-file-name nil)
	 (auto-save-mode 0)
	 (run-hooks 'tar-subfile-mode-hook))
	(t
	 ;; remove the local binding for C-x C-s.
	 (local-unset-key "\^X\^S")
	 (if subfile-orig-mlbid
	     (set (make-local-variable 'mode-line-buffer-identification)
		  subfile-orig-mlbid))
	 (setq tar-superior-buffer nil
	       tar-superior-descriptor nil
	       subfile-orig-mlbid nil)
	 ))
  )

(defun tar-subfile-after-write-file-hook ()
  ;; if the buffer has a filename, then it is no longer associated with
  ;; the tar file.  Turn off subfile mode.
  (if (and buffer-file-name tar-subfile-mode)
      (tar-subfile-mode -1)))

(defun tar-mode-revert (&optional no-autosave no-confirm)
  "Revert this buffer and turn on tar mode again, to re-compute the
directory listing."
  (setq tar-header-offset nil)
  (let ((revert-buffer-function nil))
    (revert-buffer t no-confirm)
    (widen))
  (tar-mode))


(defun tar-next-line (p)
  (interactive "p")
  (forward-line p)
  (if (eobp) nil (forward-char (if tar-can-print-dates 48 36))))

(defun tar-previous-line (p)
  (interactive "p")
  (tar-next-line (- p)))

(defun tar-current-descriptor (&optional noerror)
  "Returns the tar-descriptor of the current line, or signals an error."
  ;; I wish lines had plists, like in ZMACS...
  (or (nth (count-lines (point-min)
			(save-excursion (beginning-of-line) (point)))
	   tar-parse-info)
      (if noerror
	  nil
	(error "This line does not describe a tar-file entry."))))


(defun tar-extract (&optional other-window-p)
  "In tar-mode, extract this entry of the tar file into its own buffer."
  (interactive)
  (let* ((view-p (eq other-window-p 'view))
	 (descriptor (tar-current-descriptor))
	 (tokens (tar-desc-tokens descriptor))
	 (name (tar-header-name tokens))
	 (size (tar-header-size tokens))
	 (link-p (tar-header-link-type tokens))
	 (start (+ (tar-desc-data-start descriptor) tar-header-offset -1))
	 (end (+ start size)))
    (if link-p
	(error "This is a %s, not a real file."
	       (cond ((eq link-p 5) "directory")
		     ((eq link-p 20) "tar directory header")
		     ((eq link-p 29) "multivolume-continuation")
		     ((eq link-p 35) "sparse entry")
		     ((eq link-p 38) "volume header")
		     (t "link"))))
    (if (zerop size) (error "This is a zero-length file."))
    (let* ((tar-buffer (current-buffer))
	   (bufname (file-name-nondirectory name))
	   (bufid (concat		;" (" name " in "
		   " (in "
		   (file-name-nondirectory (buffer-file-name))
		   ")"))
	   (read-only-p (or buffer-read-only view-p))
	   (buffer nil)
	   (buffers (buffer-list))
	   (just-created nil))
      ;; find a buffer visiting this subfile from this tar file.
      (while (and buffers (not buffer))
	(set-buffer (car buffers))
	(if (and (null (buffer-file-name (car buffers)))
		 (boundp 'tar-superior-descriptor)
		 (eq tar-superior-descriptor descriptor))
	    (setq buffer (car buffers))
	  (setq buffers (cdr buffers))))
      (set-buffer tar-buffer)
      (if buffer
	  nil
	(setq buffer (generate-new-buffer bufname))
	(setq just-created t)
	(unwind-protect
	    (progn
	      (widen)
	      (save-excursion
		(set-buffer buffer)
		(insert-buffer-substring tar-buffer start end)
		(goto-char 0)
		(let ((lock-directory nil)) ; disable locking
		  (set-visited-file-name name) ; give it a name to decide mode.
		  ;;		  (normal-mode)  ; pick a mode.
		  ;;		  (after-find-file nil nil)  ; pick a mode; works with crypt.el
		  ;; Ok, instead of running after-find-file, just invoke the
		  ;; find-file-hooks instead.  This does everything we want
		  ;; from after-find-file, without losing when visiting .tar
		  ;; files via ange-ftp: doesn't probe the ftp site for the
		  ;; name of the subfile.
		  (normal-mode t)
		  (run-hooks 'find-file-hooks)
		  (set-visited-file-name nil) ; nuke the name - not meaningful.
		  )
		(make-local-variable 'tar-superior-buffer)
		(make-local-variable 'tar-superior-descriptor)
		(make-local-variable 'mode-line-buffer-identification)
		(make-local-variable 'tar-subfile-buffer-id)
		(make-local-variable 'subfile-orig-mlbid)
		(setq tar-superior-buffer tar-buffer)
		(setq tar-superior-descriptor descriptor)
		(setq tar-subfile-buffer-id bufid)
		(setq subfile-orig-mlbid mode-line-buffer-identification)
		(cond ((stringp mode-line-buffer-identification)
		       (setq mode-line-buffer-identification
			     (list mode-line-buffer-identification))))
		(let ((ms (car mode-line-buffer-identification))
		      n)
		  (cond ((and (stringp ms)
			      (string-match "%\\([0-9]+\\)b\\'" ms))
			 (setq mode-line-buffer-identification
			       (cons
				(concat (substring ms 0
						   (1- (match-beginning 1)))
					(substring ms (1+ (match-end 1))))
				(cons
				 (list (car (read-from-string
					     (substring ms (match-beginning 1)
							(match-end 1))))
				       (concat "%b" tar-subfile-buffer-id))
				 (cdr mode-line-buffer-identification)))))
			(t
			 (setq mode-line-buffer-identification
			       (list "Emacs: "
				     (list 17
					   (concat "%b"
						   tar-subfile-buffer-id)))))))
		(tar-subfile-mode 1)
		
		(setq buffer-read-only read-only-p)
		(set-buffer-modified-p nil))
	      (set-buffer tar-buffer))
	  (narrow-to-region 1 tar-header-offset)))
      (if view-p
	  (progn
	    (view-buffer-other-window buffer)
	    (save-excursion
	      (set-buffer buffer)
	      ;; for view-less.el; view.el can't do this.
	      (set (make-local-variable 'view-kill-on-exit) t)))
	(if other-window-p
	    (switch-to-buffer-other-window buffer)
	  (switch-to-buffer buffer))))))


(defun tar-extract-other-window ()
  "In tar-mode, extract this entry of the tar file into its own buffer."
  (interactive)
  (tar-extract t))

(defun tar-view ()
  "In tar-mode, view the tar file entry on this line."
  (interactive)
  (tar-extract 'view))


(defun tar-read-file-name (&optional prompt)
  "Read a file name with this line's entry as the default."
  (or prompt (setq prompt "Copy to: "))
  (let* ((default-file (expand-file-name
			(tar-header-name (tar-desc-tokens
					  (tar-current-descriptor)))))
	 (target (expand-file-name
		  (read-file-name prompt
				  (file-name-directory default-file)
				  default-file nil))))
    (if (or (string= "" (file-name-nondirectory target))
	    (file-directory-p target))
	(setq target (concat (if (string-match "/$" target)
				 (substring target 0 (1- (match-end 0)))
			       target)
			     "/"
			     (file-name-nondirectory default-file))))
    target))


(defun tar-copy (&optional to-file)
  "In tar-mode, extract this entry of the tar file into a file on disk.
If TO-FILE is not supplied, it is prompted for, defaulting to the name of
the current tar-entry."
  (interactive (list (tar-read-file-name)))
  (let* ((descriptor (tar-current-descriptor))
	 (tokens (tar-desc-tokens descriptor))
	 (name (tar-header-name tokens))
	 (size (tar-header-size tokens))
	 (link-p (tar-header-link-type tokens))
	 (start (+ (tar-desc-data-start descriptor) tar-header-offset -1))
	 (end (+ start size)))
    (if link-p (error "This is a link, not a real file."))
    (if (zerop size) (error "This is a zero-length file."))
    (let* ((tar-buffer (current-buffer))
	   buffer)
      (unwind-protect
	  (progn
	    (setq buffer (generate-new-buffer "*tar-copy-tmp*"))
	    (widen)
	    (save-excursion
	      (set-buffer buffer)
	      (insert-buffer-substring tar-buffer start end)
	      (set-buffer-modified-p nil) ; in case we abort
	      (write-file to-file)
	      (message "Copied tar entry %s to %s" name to-file)
	      (set-buffer tar-buffer)))
	(narrow-to-region 1 tar-header-offset)
	(if buffer (kill-buffer buffer)))
      )))


(defun tar-flag-deleted (p &optional unflag)
  "In tar-mode, mark this sub-file to be deleted from the tar file.
With a prefix argument, mark that many files."
  (interactive "p")
  (beginning-of-line)
  (dotimes (i (if (< p 0) (- p) p))
    (if (tar-current-descriptor unflag) ; barf if we're not on an entry-line.
	(progn
	  (delete-char 1)
	  (insert (if unflag " " "D"))))
    (forward-line (if (< p 0) -1 1)))
  (if (eobp) nil (forward-char 36)))

(defun tar-unflag (p)
  "In tar-mode, un-mark this sub-file if it is marked to be deleted.
With a prefix argument, un-mark that many files forward."
  (interactive "p")
  (tar-flag-deleted p t))

(defun tar-unflag-backwards (p)
  "In tar-mode, un-mark this sub-file if it is marked to be deleted.
With a prefix argument, un-mark that many files backward."
  (interactive "p")
  (tar-flag-deleted (- p) t))


(defun tar-expunge-internal ()
  "Expunge the tar-entry specified by the current line."
  (let* ((descriptor (tar-current-descriptor))
	 (tokens (tar-desc-tokens descriptor))
	 (line (tar-desc-data-start descriptor))
	 (name (tar-header-name tokens))
	 (size (tar-header-size tokens))
	 (link-p (tar-header-link-type tokens))
	 (start (tar-desc-data-start descriptor))
	 (following-descs (cdr (memq descriptor tar-parse-info))))
    (if link-p (setq size 0))		; size lies for hard-links.
    ;;
    ;; delete the current line...
    (beginning-of-line)
    (let ((line-start (point)))
      (end-of-line) (forward-char)
      (let ((line-len (- (point) line-start)))
	(delete-region line-start (point))
	;;
	;; decrement the header-pointer to be in synch...
	(setq tar-header-offset (- tar-header-offset line-len))))
    ;;
    ;; delete the data pointer...
    (setq tar-parse-info (delq descriptor tar-parse-info))
    ;;
    ;; delete the data from inside the file...
    (widen)
    (let* ((data-start (+ start tar-header-offset -513))
	   (data-end (+ data-start 512 (ash (ash (+ size 511) -9) 9))))
      (delete-region data-start data-end)
      ;;
      ;; and finally, decrement the start-pointers of all following
      ;; entries in the archive.  This is a pig when deleting a bunch
      ;; of files at once - we could optimize this to only do the
      ;; iteration over the files that remain, or only iterate up to
      ;; the next file to be deleted.
      (let ((data-length (- data-end data-start)))
	(dolist (desc following-descs)
	  (setf (tar-desc-data-start desc)
		(- (tar-desc-data-start desc) data-length))))
      ))
  (narrow-to-region 1 tar-header-offset))


(defun tar-expunge (&optional noconfirm)
  "In tar-mode, delete all the archived files flagged for deletion.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive)
  (if (or noconfirm
	  (y-or-n-p "Expunge files marked for deletion? "))
      (let ((n 0))
	(save-excursion
	  (goto-char 0)
	  (while (not (eobp))
	    (if (looking-at "D")
		(progn (tar-expunge-internal)
		       (setq n (1+ n)))
	      (forward-line 1)))
	  ;; after doing the deletions, add any padding that may be necessary.
	  (tar-pad-to-blocksize)
	  (narrow-to-region 1 tar-header-offset)
	  )
	(if (zerop n)
	    (message "Nothing to expunge.")
	    (message "%s files expunged.  Be sure to save this buffer." n)))))


(defun tar-clear-modification-flags ()
  "Remove the stars at the beginning of each line."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (< (point) tar-header-offset)
      (if (looking-at "*")
	  (progn (delete-char 1) (insert " ")))
      (forward-line 1))))


(defun tar-chown-entry (new-uid)
  "Change the user-id associated with this entry in the tar file.
If this tar file was written by GNU tar, then you will be able to edit
the user id as a string; otherwise, you must edit it as a number.
You can force editing as a number by calling this with a prefix arg.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive (list
		(let ((tokens (tar-desc-tokens (tar-current-descriptor))))
		  (if (or current-prefix-arg
			  (not (tar-header-magic tokens)))
		      (let (n)
			(while (not (numberp (setq n (read-minibuffer
						      "New UID number: "
						      (format "%s" (tar-header-uid tokens)))))))
			n)
		    (read-string "New UID string: " (tar-header-uname tokens))))))
  (cond ((stringp new-uid)
	 (setf (tar-header-uname (tar-desc-tokens (tar-current-descriptor)))
	       new-uid)
	 (tar-alter-one-field tar-uname-offset (concat new-uid "\000")))
	(t
	 (setf (tar-header-uid (tar-desc-tokens (tar-current-descriptor)))
	       new-uid)
	 (tar-alter-one-field
	  tar-uid-offset
	  (concat (substring (format "%6o" new-uid) 0 6) "\000 ")))))


(defun tar-chgrp-entry (new-gid)
  "Change the group-id associated with this entry in the tar file.
If this tar file was written by GNU tar, then you will be able to edit
the group id as a string; otherwise, you must edit it as a number.
You can force editing as a number by calling this with a prefix arg.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive (list
		(let ((tokens (tar-desc-tokens (tar-current-descriptor))))
		  (if (or current-prefix-arg
			  (not (tar-header-magic tokens)))
		      (let (n)
			(while (not (numberp (setq n (read-minibuffer
						      "New GID number: "
						      (format "%s" (tar-header-gid tokens)))))))
			n)
		    (read-string "New GID string: " (tar-header-gname tokens))))))
  (cond ((stringp new-gid)
	 (setf (tar-header-gname (tar-desc-tokens (tar-current-descriptor)))
	       new-gid)
	 (tar-alter-one-field tar-gname-offset
			      (concat new-gid "\000")))
	(t
	 (setf (tar-header-gid (tar-desc-tokens (tar-current-descriptor)))
	       new-gid)
	 (tar-alter-one-field
	  tar-gid-offset
	  (concat (substring (format "%6o" new-gid) 0 6) "\000 ")))))

(defun tar-rename-entry (new-name)
  "Change the name associated with this entry in the tar file.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive
   (list (read-string "New name: "
		      (tar-header-name
		       (tar-desc-tokens (tar-current-descriptor))))))
  (if (string= "" new-name) (error "zero length name"))
  (if (> (length new-name) 98) (error "name too long"))
  (setf (tar-header-name (tar-desc-tokens (tar-current-descriptor)))
	new-name)
  (tar-alter-one-field 0
		       (substring (concat new-name (make-string 99 0)) 0 99)))


(defun tar-chmod-entry (new-mode)
  "Change the protection bits associated with this entry in the tar file.
This does not modify the disk image; you must save the tar file itself
for this to be permanent."
  (interactive (list (tar-parse-octal-integer-safe
		      (read-string "New protection (octal): "))))
  (setf (tar-header-mode (tar-desc-tokens (tar-current-descriptor)))
	new-mode)
  (tar-alter-one-field
   tar-mode-offset
   (concat (substring (format "%6o" new-mode) 0 6) "\000 ")))


(defun tar-alter-one-field (data-position new-data-string)
  (let* ((descriptor (tar-current-descriptor))
	 (tokens (tar-desc-tokens descriptor)))
    (unwind-protect
	(save-excursion
	  ;;
	  ;; update the header-line.
	  (beginning-of-line)
	  (let ((p (point)))
	    (forward-line 1)
	    (delete-region p (point))
	    (insert (tar-summarize-header-block tokens) "\n")
	    (setq tar-header-offset (point-max)))
	  
	  (widen)
	  (let* ((start (+ (tar-desc-data-start descriptor) tar-header-offset -513)))
	    ;;
	    ;; delete the old field and insert a new one.
	    (goto-char (+ start data-position))
	    (delete-region (point) (+ (point) (length new-data-string))) ; <--
	    (insert new-data-string)	; <--
	    ;;
	    ;; compute a new checksum and insert it.
	    (let ((chk (tar-header-block-checksum
			(buffer-substring start (+ start 512)))))
	      (goto-char (+ start tar-chk-offset))
	      (delete-region (point) (+ (point) 8))
	      (insert (format "%6o" chk))
	      (insert 0)
	      (insert ? )
	      (setf (tar-header-checksum tokens) chk)
	      ;;
	      ;; ok, make sure we didn't botch it.
	      (tar-header-block-check-checksum
	       (buffer-substring start (+ start 512))
	       chk (tar-header-name tokens)))))
      (narrow-to-region 1 tar-header-offset))))


(defun tar-subfile-save-buffer ()
  "In tar subfile mode, save this buffer into its parent tar-file buffer.
This doesn't write anything to disk; you must save the parent tar-file buffer
to make your changes permanent."
  (interactive)
  (cond (buffer-file-name
	 ;; tar-subfile buffers should have nil as buffer-file-name.  If they
	 ;; ever gain a buffer-file-name, that means they have been written to
	 ;; a real disk file, as with ^X^W.  If this happens, behave just like
	 ;; `save-buffer.'
	 (call-interactively 'save-buffer))
	(t
	 (tar-subfile-save-buffer-internal))))

(defun tar-subfile-save-buffer-internal ()
  (if (not (and (boundp 'tar-superior-buffer) tar-superior-buffer))
      (error "This buffer has no superior tar file buffer."))
  (or (buffer-name tar-superior-buffer)
      (error "The superior tar file's buffer has been killed."))
  (if (not (and (boundp 'tar-superior-descriptor) tar-superior-descriptor))
      (error "This buffer doesn't have an index into its superior tar file!"))

  ;; Notice when crypt.el has uncompressed while reading the subfile, and
  ;; signal an error if the user tries to save back into the parent file
  ;; (because it won't work - the .Z subfile it writes won't really be
  ;; compressed.)
  ;;
					;  ;; These are for the old crypt.el
					;  (if (and (boundp 'buffer-save-encrypted) buffer-save-encrypted)
					;      (error "Don't know how to encrypt back into a tar file."))
					;  (if (and (boundp 'buffer-save-compacted) buffer-save-compacted)
					;      (error "Don't know how to compact back into a tar file."))
					;  (if (and (boundp 'buffer-save-compressed) buffer-save-compressed)
					;      (error "Don't know how to compress back into a tar file."))
					;  (if (and (boundp 'buffer-save-gzipped) buffer-save-gzipped)
					;      (error "Don't know how to gzip back into a tar file."))

  ;; These are for the new crypt++.el
  (if (and (boundp 'crypt-buffer-save-encrypted) crypt-buffer-save-encrypted)
      (error "Don't know how to encrypt back into a tar file."))
  (if (and (boundp 'crypt-buffer-save-compact) crypt-buffer-save-compact)
      (error "Don't know how to compact back into a tar file."))
  (if (and (boundp 'crypt-buffer-save-compress) crypt-buffer-save-compress)
      (error "Don't know how to compress back into a tar file."))
  (if (and (boundp 'crypt-buffer-save-gzip) crypt-buffer-save-gzip)
      (error "Don't know how to gzip back into a tar file."))
  (if (and (boundp 'crypt-buffer-save-freeze) crypt-buffer-save-freeze)
      (error "Don't know how to freeze back into a tar file."))

  (save-excursion
    (let ((subfile (current-buffer))
	  (subfile-size (buffer-size))
	  (descriptor tar-superior-descriptor))
      (set-buffer tar-superior-buffer)
      (let* ((tokens (tar-desc-tokens descriptor))
	     (start (tar-desc-data-start descriptor))
	     (name (tar-header-name tokens))
	     (size (tar-header-size tokens))
	     (size-pad (ash (ash (+ size 511) -9) 9))
	     (head (memq descriptor tar-parse-info))
	     (following-descs (cdr head)))
	(if (not head)
	    (error "Can't find this tar file entry in its parent tar file!"))
	(unwind-protect
	    (save-excursion
	      (widen)
	      ;; delete the old data...
	      (let* ((data-start (+ start tar-header-offset -1))
		     (data-end (+ data-start (ash (ash (+ size 511) -9) 9))))
		(delete-region data-start data-end)
		;; insert the new data...
		(goto-char data-start)
		(insert-buffer subfile)
		;;
		;; pad the new data out to a multiple of 512...
		(let ((subfile-size-pad (ash (ash (+ subfile-size 511) -9) 9)))
		  (goto-char (+ data-start subfile-size))
		  (insert (make-string (- subfile-size-pad subfile-size) 0))
		  ;;
		  ;; update the data pointer of this and all following files...
		  (setf (tar-header-size tokens) subfile-size)
		  (let ((difference (- subfile-size-pad size-pad)))
		    (dolist (desc following-descs)
		      (setf (tar-desc-data-start desc)
			    (+ (tar-desc-data-start desc) difference))))
		  ;;
		  ;; Update the size field in the header block.
		  (let ((header-start (- data-start 512)))
		    (goto-char (+ header-start tar-size-offset))
		    (delete-region (point) (+ (point) 12))
		    (insert (format "%11o" subfile-size))
		    (insert ? )
		    ;;
		    ;; Maybe update the datestamp.
		    (if (not tar-update-datestamp)
			nil
		      (goto-char (+ header-start tar-time-offset))
		      (delete-region (point) (+ (point) 12))
		      (let (now top bot)
			(cond ((fboundp 'current-time)
			       (setq now (current-time))
			       (setcdr now (car (cdr now))))
					;			((fboundp 'current-time-seconds)
					;			 (setq now (current-time-seconds)))
			      )
			(setq top (car now)
			      bot (cdr now))
			(cond
			 (now
			  (setf (tar-header-date tokens) now)
			  ;; hair to print two 16-bit numbers as one octal number.
			  (setq bot (logior (ash (logand top 3) 16) bot))
			  (setq top (ash top -2))
			  (insert (format "%5o" top))
			  (insert (format "%06o " bot)))
			 (t
			  ;; otherwise, set it to the epoch.
			  (insert (format "%11o " 0))
			  (setf (tar-header-date tokens) (cons 0 0))
			  ))))
		    ;;
		    ;; compute a new checksum and insert it.
		    (let ((chk (tar-header-block-checksum
				(buffer-substring header-start data-start))))
		      (goto-char (+ header-start tar-chk-offset))
		      (delete-region (point) (+ (point) 8))
		      (insert (format "%6o" chk))
		      (insert 0)
		      (insert ? )
		      (setf (tar-header-checksum tokens) chk)))
		  ;;
		  ;; alter the descriptor-line...
		  ;;
		  (let ((position (- (length tar-parse-info) (length head))))
		    (goto-char 1)
		    (next-line position)
		    (beginning-of-line)
		    (let ((p (point))
			  (m (set-marker (make-marker) tar-header-offset)))
		      (forward-line 1)
		      (delete-region p (point))
		      (insert-before-markers (tar-summarize-header-block tokens t) "\n")
		      (setq tar-header-offset (marker-position m)))
		    )))
	      ;; after doing the insertion, add any final padding that may be necessary.
	      (tar-pad-to-blocksize))
	  (narrow-to-region 1 tar-header-offset)))
      (set-buffer-modified-p t)		; mark the tar file as modified
      (set-buffer subfile)
      (set-buffer-modified-p nil)	; mark the tar subfile as unmodified
      (message "saved into tar-buffer \"%s\" - remember to save that buffer!"
	       (buffer-name tar-superior-buffer))
      )))


(defun tar-pad-to-blocksize ()
  "If we are being anal about tar file blocksizes, fix up the current buffer.
Leaves the region wide."
  (if (null tar-anal-blocksize)
      nil
    (widen)
    (let* ((last-desc (nth (1- (length tar-parse-info)) tar-parse-info))
	   (start (tar-desc-data-start last-desc))
	   (tokens (tar-desc-tokens last-desc))
	   (link-p (tar-header-link-type tokens))
	   (size (if link-p 0 (tar-header-size tokens)))
	   (data-end (+ start size))
	   (bbytes (ash tar-anal-blocksize 9))
	   (pad-to (+ bbytes (* bbytes (/ (1- data-end) bbytes))))
	   (buffer-read-only nil)	; ##
	   )
      ;; If the padding after the last data is too long, delete some;
      ;; else insert some until we are padded out to the right number of blocks.
      ;;
      (goto-char (+ (or tar-header-offset 0) data-end))
      (if (> (1+ (buffer-size)) (+ (or tar-header-offset 0) pad-to))
	  (delete-region (+ (or tar-header-offset 0) pad-to) (1+ (buffer-size)))
	(insert (make-string (- (+ (or tar-header-offset 0) pad-to)
				(1+ (buffer-size)))
			     0)))
      )))


(defun tar-maybe-write-file ()
  "Used as a write-file-hook to write tar-files out correctly."
  ;;
  ;; If the current buffer is in tar-mode and has its header-offset set,
  ;; remove the header from the file, call the remaining write-file hooks,
  ;; and then write out the buffer (if and only if one of the write-file
  ;; hooks didn't write it already).  Then put the header back on the
  ;; buffer.  Many thanks to Piet van Oostrum for this code, which causes
  ;; correct interaction with crypt.el (and probably anything like it.)
  ;;
  ;; Kludge: in XEmacs Emacs, write-file-hooks is bound to nil before the
  ;; write-file-hooks are run, to prevent them from being run recursively
  ;; (this is more of a danger in v19-vintage emacses, which have both
  ;; write-file-hooks and write-contents-hooks.)  So, we need to reference
  ;; an internal variable of basic-save-buffer to get the list of hooks
  ;; remaining to be run.
  ;;
  (and (eq major-mode 'tar-mode)
       (and (boundp 'tar-header-offset) tar-header-offset)
       (let* ((hooks (cond ((string-match "XEmacs" emacs-version)
			    ;; Internal to basic-save-buffer in XEmacs.
			    (symbol-value 'hooks))
			   ((string-lessp "19" emacs-version)
			    ;; I think this is what we need to do in fsfmacs.
			    (append write-contents-hooks write-file-hooks))
			   (t
			    write-file-hooks)))
	      (remaining-hooks (cdr (memq 'tar-maybe-write-file hooks)))
	      header-string
	      done)
	 (save-excursion
	   (save-restriction
	     (widen)
	     (tar-clear-modification-flags)
	     (setq header-string (buffer-substring 1 tar-header-offset))
	     (delete-region 1 tar-header-offset)
	     (unwind-protect
		 (progn
		   (while (and remaining-hooks
			       (not (setq done (funcall (car remaining-hooks)))))
		     (setq remaining-hooks (cdr remaining-hooks)))
		   (cond ((not done)
			  (write-region 1 (1+ (buffer-size))
					buffer-file-name nil t)
			  (setq done t))))
	       (goto-char 1)
	       (insert header-string)
	       (set-buffer-modified-p nil))))
	 done)))


;;; Patch it in.

;;;###autoload
(defvar tar-regexp "\\.tar$"
  "The regular expression used to identify tar file names.
Note that this regular expression must not match compressed tar file
names; if it does, tar-mode will attempt to parse the compressed tar
file as an uncompressed tar file, which will generate an error.  This
is not a problem, as other modules that handle compression will
uncompress the buffer and call `tar-mode' appropriately.")

;;;###autoload
(setq auto-mode-alist
      (cons (cons tar-regexp 'tar-mode) auto-mode-alist))

;; Note: the tar write-file-hook should go on the list *before* any other
;; hooks which might write the file.  Since things like crypt-mode add things
;; to the end of the write-file-hooks, this will normally be the case.

					;(or (boundp 'write-file-hooks) (setq write-file-hooks nil))
					;(or (listp write-file-hooks)
					;    (setq write-file-hooks (list write-file-hooks)))
					;(or (memq 'tar-maybe-write-file write-file-hooks)
					;    (setq write-file-hooks
					;	  (cons 'tar-maybe-write-file write-file-hooks)))

(add-hook 'write-file-hooks 'tar-maybe-write-file) ; ####write-contents-hooks??
(cond ((boundp 'after-save-hook)
       (add-hook 'after-save-hook 'tar-subfile-after-write-file-hook))
      ((boundp 'after-write-file-hooks)
       (add-hook 'after-write-file-hooks 'tar-subfile-after-write-file-hook))
      (t (error "neither after-save-hook nor after-write-file-hooks?")))


;;; This is a hack.  For files ending in .tar, we want -*- lines to be
;;; completely ignored - if there is one, it applies to the first file
;;; in the archive, and not the archive itself!  Similarly for local
;;; variables specifications in the last file of the archive.

(defun tar-normal-mode (&optional find-file)
  "Choose the major mode for this buffer automatically.
Also sets up any specified local variables of the file.
Uses the visited file name, the -*- line, and the local variables spec.

This function is called automatically from `find-file'.  In that case,
if `inhibit-local-variables' is non-`nil' we require confirmation before
processing a local variables spec.  If you run `normal-mode' explicitly,
confirmation is never required.

Note that this version of this function has been hacked to interact
correctly with tar files - when visiting a file which matches
'tar-regexp', the -*- line and local-variables are not examined,
as they would apply to a file within the archive rather than the archive
itself."
  (interactive)
  (if (and buffer-file-name
	   (string-match tar-regexp buffer-file-name))
      (tar-mode)
    (tar-real-normal-mode find-file)))

;; We have to shadow this as well to get along with crypt.el.
;; Shadowing this alone isn't enough, though; we need to shadow 
;; tar-normal-mode in order to inhibit the local variables of the
;; last file in the tar archive.
;;
(defun tar-set-auto-mode ()
  "Select major mode appropriate for current buffer.
May base decision on visited file name (See variable  auto-mode-list)
or on buffer contents (-*- line or local variables spec), but does not look
for the \"mode:\" local variable.  For that, use  hack-local-variables.

Note that this version of this function has been hacked to interact
correctly with tar files - when visiting a file which matches
'tar-regexp', the -*- line and local-variables are not examined,
as they would apply to a file within the archive rather than the archive
itself."
  (interactive)
  (if (and buffer-file-name
	   (string-match tar-regexp buffer-file-name))
      (tar-mode)
    (tar-real-set-auto-mode)))

(if (not (fboundp 'tar-real-normal-mode))
    (fset 'tar-real-normal-mode (symbol-function 'normal-mode)))
(fset 'normal-mode 'tar-normal-mode)

(if (not (fboundp 'tar-real-set-auto-mode))
    (fset 'tar-real-set-auto-mode (symbol-function 'set-auto-mode)))
(fset 'set-auto-mode 'tar-set-auto-mode)

(defun tar-quit ()
  "Kill the current tar buffer."
  (interactive)
  (kill-buffer nil))

(provide 'tar-mode)

;;; tar-mode.el ends here
