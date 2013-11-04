;; #(@) crontab.el - An Emacs function to assist in editing crontab entries
;; Last edited: Fri Aug 18 12:19:22 1989 by chris (Christopher D. Orr) on lxn
;;
;; Version: 1.00 - Initial Creation of mode
;;          1.01 - Added crontab-use-local-file variable
;;          1.02 - Reworked most of the library to be cleaner.
;;          1.03 - Now deletes blank lines in crontab entry

;; Copyright (C) 1989 Christopher D. Orr (chris@lxn.eds.com)

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

;;
;; TODO:
;;

;; Place the following line in your ~/.emacs file:
;;    (autoload 'crontab-edit "crontab"
;;              "Function to allow the easy editing of crontab files." t)
;;

(provide 'crontab-edit)

;;; Local Variables.  Define these to your liking.

(defgroup crontab nil
  "Assist in editing crontab files."
  :group 'languages)


(defcustom crontab-filename "~/.crontab"
  "*The name of the file to store the User's Crontab."
  :type 'file
  :group 'crontab)

(defcustom crontab-directory "/usr/spool/cron/crontabs"
  "*The name of the directory in which crontab stores it's entries."
  :type 'file
  :group 'crontab)

(defcustom crontab-use-local-file nil
  "*Non-nil means use file stored in User's Home directory, if it exists.
Otherwise, always ask crontab for the current entry (maybe)."
  :type 'boolean
  :group 'crontab)

(defcustom crontab-delete-blank-lines t
  "*Non-nil means to delete any blank lines in the crontab file on save."
  :type 'boolean
  :group 'crontab)

;;; Interactive Function called to edit a Crontab Entry.  It is called
;;; instead of crontab-edit to allow for future automatic entries.

(defun crontab-edit ()
  "Function to allow the easy editing of crontab files."

  (interactive)
  (crontab-get))


;;; Function to retrieve the crontab entry.  The Function will
;;; retrieve the file (crontab-filename) first.  If the file does not
;;; exists, a crontab -l command will be executed. 

(defun crontab-get ()
   "Retrieve a crontab file either using crontab -l or from the variable
crontab-filename"
   (message "Retrieving Crontab ... ")
   (switch-to-buffer (create-file-buffer crontab-filename))
   (erase-buffer)

   (if (file-exists-p crontab-filename)
       (if (file-newer-than-file-p (concat crontab-directory "/" (user-login-name)) (expand-file-name crontab-filename))
	   (if (yes-or-no-p "Cron has a more recent copy of your crontab.  Use it ? ")
	       (call-process "crontab" nil t t "-l")
	     (insert-file crontab-filename))
	 (if crontab-use-local-file
	     (insert-file crontab-filename)
	   (call-process "crontab" nil t t "-l")))
     (if crontab-use-local-file
	 (insert-file crontab-filename)
       (call-process "crontab" nil t t "-l")))

;; What if crontab returns a fatal ??????  Can't we check the errorlevel ????
   (goto-char (point-min))
   (if (search-forward-regexp "crontab:\\|no crontab for" nil t nil)
       (erase-buffer))
   (if (eobp)
       (crontab-initialize))
   (goto-line 6)
   (setq buffer-file-name crontab-filename)
   (set-buffer-modified-p nil)
   (make-variable-buffer-local 'write-file-hooks)
   (or (memq 'crontab-save write-file-hooks)
       (setq write-file-hooks 
	     (reverse (cons 'crontab-save (reverse write-file-hooks)))))
   (message "Save file normally when finished to update cron."))


;;; This function is called whenever a save-file operation is
;;; performed in the crontab buffer.  It saves the crontab to the file
;;; name (crontab-filename) and then removes the crontab buffer.

(defun crontab-save ()
  "Submit the edited crontab to the cron daemon for processing."

  (when crontab-delete-blank-lines
    (goto-char (point-min))
    (while (not (eobp))
      (delete-blank-lines)
      (forward-line 1))
    (redraw-display))

  (setq write-file-hooks nil)
  (let ((crontab-buffer (buffer-name)))
    (basic-save-buffer)

;; What if the call-process to crontab fails ???  Can we check for a fatal ???
;;  (call-process "crontab" nil nil nil (expand-file-name crontab-filename))
    (shell-command (concat "crontab " (expand-file-name crontab-filename)))

    (switch-to-buffer (other-buffer))
    (kill-buffer crontab-buffer))
  (message (concat "Crontab saved as " crontab-filename " and submitted to cron."))
;; fixed by Lynn D. Newton - 03/17/95
  "")
;; OLD
;; nil)

(defun crontab-initialize ()
  "Create a default Crontab file if one does not exist or is empty.
If the function (time-stamp) is available, the last modification time will
be stamped to the file."

   (insert "# Cron Table Entry for ")
   (insert (user-login-name))
   (insert " (")
   (insert (user-full-name))
   (insert ")\n# Last Edited: \n")
   (insert "#\n")
   (insert "# min    hr     day   mon    wday(0=sun)  cmd\n")
   (insert "#\n"))

;;; Watch out for the signature  :-)
