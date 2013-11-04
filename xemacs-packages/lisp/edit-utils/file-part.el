;;; file-part.el --- treat a section of a buffer as a separate file

;; Keywords: extensions, tools

;; Copyright (C) 1992-1993 Sun Microsystems.

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

;; Written by Ben Wing.

(provide 'file-part)

(define-error 'file-part-error "File part error" 'file-error)

(defvar file-part-extent-alist nil
  "Alist of file parts in the current buffer.
Each element of the alist maps an extent describing the file part
to the buffer containing the file part.  DON'T MODIFY THIS.")
(make-variable-buffer-local 'file-part-extent-alist)
(setq-default file-part-extent-alist nil)

(defvar file-part-master-extent nil
  "Extent this file part refers to in the master buffer.
NIL if this buffer is not a file part.  The master buffer itself
can be found by calling `extent-buffer' on this extent.
DON'T MODIFY THIS.")
(make-variable-buffer-local 'file-part-master-extent)
(setq-default file-part-master-extent nil)

(or (assq 'file-part-master-extent minor-mode-alist)
    (setq minor-mode-alist
	  (cons minor-mode-alist
		'((file-part-master-extent " File-part")))))

; apply a function to each element of a list and return true if
; any of the functions returns true.
(defun file-part-maptrue (fn list)
  (cond ((null list) nil)
	((funcall fn (car list)))
	(t (file-part-maptrue fn (cdr list)))))

; return a buffer to operate on.  If NIL is specified, this is the
; current buffer.  If a string is specified, this is the buffer with
; that name.
(defun file-part-buffer-from-arg (arg)
  (get-buffer (or arg (current-buffer))))

;;;###autoload
(defun make-file-part (&optional start end name buffer)
  "Make a file part on buffer BUFFER out of the region.  Call it NAME.
This command creates a new buffer containing the contents of the
region and marks the buffer as referring to the specified buffer,
called the `master buffer'.  When the file-part buffer is saved,
its changes are integrated back into the master buffer.  When the
master buffer is deleted, all file parts are deleted with it.

When called from a function, expects four arguments, START, END,
NAME, and BUFFER, all of which are optional and default to the
beginning of BUFFER, the end of BUFFER, a name generated from
BUFFER's name, and the current buffer, respectively."
  (interactive "r\nsName of file part: ")
  (setq buffer (file-part-buffer-from-arg buffer))
  (if (null start) (setq start (point-min)))
  (if (null end) (setq end (point-max)))
  (if (null name) (setq name (concat (buffer-name buffer) "-part")))
  (if (> start end) nil
    (set-buffer buffer)
    (make-local-variable 'write-contents-hooks)
    (make-local-variable 'kill-buffer-hook)
    (make-local-variable 'revert-buffer-function)
    (add-hook 'write-contents-hooks 'write-master-buffer-hook)
    (add-hook 'kill-buffer-hook 'kill-master-buffer-hook)
    (setq revert-buffer-function 'revert-master-buffer-function)
    (if (file-part-maptrue (function (lambda (x)
			     (let ((b (extent-start-position (car x)))
				   (e (extent-end-position (car x))))
			       (and
				(numberp b)
				(numberp e)
				(not (or (and (<= b start) (<= e start))
					 (and (>= b end) (>= e end))))))))
		 file-part-extent-alist)
	(signal 'file-part-error (list "Overlapping file parts not allowed"
				       buffer))
      (let ((x (make-extent start end))
 	    (filebuf (generate-new-buffer name)))
	(set-extent-property x 'read-only t)
	(setq file-part-extent-alist
	      (cons (cons x filebuf) file-part-extent-alist))
	(switch-to-buffer filebuf)
        (setq buffer-file-name (concat "File part on " (buffer-name buffer)))
        (make-local-variable 'write-file-hooks)
        (make-local-variable 'kill-buffer-hook)
	(make-local-variable 'revert-buffer-function)
	(make-local-variable 'first-change-hook)
        (add-hook 'write-file-hooks 'write-file-part-hook)
        (add-hook 'kill-buffer-hook 'kill-file-part-hook)
	(setq revert-buffer-function 'revert-file-part-function)
	(setq file-part-master-extent x)
	(insert-buffer-substring buffer start end)
	; do this after inserting the text so the master buffer isn't marked as
	; modified.
	(add-hook 'first-change-hook 'file-part-first-change-hook)
        (set-buffer-modified-p nil)
	filebuf))))

(defun kill-file-part-hook ()
  "Hook to be called when a file-part buffer is killed.
Removes the file part from the master buffer's list of file parts."
  (let ((x file-part-master-extent)
	(buf (current-buffer)))
    (if x (save-excursion
	    (set-buffer (extent-buffer x))
	    (setq file-part-extent-alist
		  (delete (cons x buf) file-part-extent-alist))
	    (delete-extent x)))))

(defun kill-all-file-parts (&optional bufname no-ask)
  "Kill all file parts on buffer BUFNAME.
The argument may be a buffer or the name of a buffer.
If one or more of the file parts needs saving, prompts for
confirmation unless optional second argument NO-ASK is non-nil.
BUFFER defaults to the current buffer if not specified."
  (interactive "b")
  (setq bufname (file-part-buffer-from-arg bufname))
  (save-excursion
    (set-buffer bufname)
    (and (or no-ask
	     (not (file-parts-modified-p bufname))
	     (y-or-n-p "Buffer has modified file parts; kill anyway? "))
	 (mapcar (function (lambda (x)
			     (set-buffer (cdr x))
			     (set-buffer-modified-p nil)
			     (kill-buffer (cdr x))))
		 file-part-extent-alist))))

(defun kill-master-buffer-hook ()
  "Hook to be called when a master buffer is killed.
Kills the associated file parts."
  (kill-all-file-parts (current-buffer) t))

(defun file-part-check-attached (x)
  (cond ((null x) nil)
	((extent-property x 'detached)
	 (kill-file-part-hook)
	 (setq buffer-file-name nil)
	 (setq file-part-master-extent nil)
	 (message "File part has become detached.")
	 nil)
	(t)))

(defun write-file-part-hook ()
  "Hook to be called when a file part is saved.
Saves the file part into the master buffer."
  (let ((x file-part-master-extent)
	(buf (current-buffer))
	(len (- (point-max) (point-min)))
	(retval (not (null file-part-master-extent))))
    (and (file-part-check-attached x)
	 (let ((b (extent-start-position x))
	       (e (extent-end-position x)))
	   (save-excursion
	     (set-buffer (extent-buffer x))
	     (set-extent-property x 'read-only nil)
	     (goto-char b)
	     (insert-buffer-substring buf)
	     (delete-region (+ len b) (+ len e))
	     (set-extent-property x 'read-only t)
	     (set-buffer buf)
	     (set-buffer-modified-p nil)
	     (message "Wrote file part %s on %s"
		      (buffer-name buf)
		      (buffer-name (extent-buffer x)))
	     t)))
    retval))

(defun write-master-buffer-hook ()
  "Hook to be called when a master buffer is saved.
If there are modified file parts on the buffer, optionally
saves the file parts back into the buffer."
  (save-some-file-part-buffers)
  nil)

(defun save-some-file-part-buffers (&optional arg buffer)
  "Save some modified file-part buffers on BUFFER.  Asks user about each one.
Optional argument (the prefix) non-nil means save all with no questions.
BUFFER defaults to the current buffer if not specified."
  (interactive "p")
  (setq buffer (file-part-buffer-from-arg buffer))
  (let ((alist file-part-extent-alist)
	(name (buffer-name buffer)))
    (while alist
      (let ((buf (cdr (car alist))))
	(and (buffer-modified-p buf)
	     (or arg
		 (y-or-n-p (format "Save file part %s on %s? "
				   (buffer-name buf) (buffer-name buffer))))
	     (condition-case ()
		 (save-excursion
		   (set-buffer buf)
		   (save-buffer))
	       (error nil))))
      (setq alist (cdr alist)))))

(defun file-parts-modified-p (&optional buffer)
  "Return true if BUFFER has any modified file parts on it.
BUFFER defaults to the current buffer if not specified."
  (save-excursion
    (and buffer (set-buffer buffer))
    (file-part-maptrue (function (lambda (x) (buffer-modified-p (cdr x))))
		       file-part-extent-alist)))

(defun revert-file-part-function (&optional check-auto noconfirm)
  "Hook to be called when a file part is reverted.
Reverts the file part from the master buffer."
  (let ((x file-part-master-extent))
    (and (file-part-check-attached x)
	 (let ((master (extent-buffer x)))
	   (and
	    (or noconfirm
		(yes-or-no-p
		 (format
		  "Revert file part from master buffer %s? "
		  (buffer-name master))))
	    (progn
	      (erase-buffer)
	      (let ((mod (buffer-modified-p master)))
		(insert-buffer-substring master
					 (extent-start-position x)
					 (extent-end-position x))
		(set-buffer-modified-p nil)
		(save-excursion
		  (set-buffer master)
		  (set-buffer-modified-p mod)))))))))

(defun revert-master-buffer-function (&optional check-auto noconfirm)
  "Hook to be called when a master-buffer is reverted.
Makes sure the user is aware that the file parts will become detached,
then proceeds as normal."
  (or noconfirm
      (null file-part-extent-alist)
      (progn
	(message "Warning: file parts will become detached.")
	(sleep-for 2)))
  (let ((revert-buffer-function nil))
    (revert-buffer (not check-auto) noconfirm)))

(defun file-part-first-change-hook ()
  "Hook to be called when a file part is first modified.
Marks the master buffer as modified."
  (let ((x file-part-master-extent))
    (and (file-part-check-attached x)
	 (save-excursion
	   (set-buffer (extent-buffer x))
	   (set-buffer-modified-p t)))))

