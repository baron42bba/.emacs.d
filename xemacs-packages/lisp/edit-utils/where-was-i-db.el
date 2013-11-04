;;; where-was-i-db.el --- Keep persistent state in visited files.

;; Copyright (C) 1997 by Free Software Foundation, Inc.
;; Copyright (C) 1997 by Software in the Public Interest, Inc. (Debian GNU/Linux)
;; Copyright (C) 1997 by Karl M. Hegbloom - Donated to the FSF and SPI.

;; Author: Karl M. Hegbloom <karlheg@bittersweet.inetarena.com>
;; Keywords: tools, data, wp
;; Version: 1.3
;; X-Requires: (:emacs "XEmacs" :>= 20.4 :feature 'berkeley-db)
;; X-RCS-ID: #Id: where-was-i-db.el,v 1.3 1998/01/02 23:45:29 karlheg Exp karlheg #

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; This program hopes to become a standard and favored XEmacs feature
;; on setups with Berkeley DB installed.  It is one of the first to
;; exemplify automatic installation and deinstallation of editor
;; features utilizing the `customize' interface, autoloading, lazy
;; loading, and the `unload-feature' facility.

;; Thank You all for the experience and the FREE source code.

;; This Good Thing will make XEmacs keep track of where you were last
;; time you visited a file.
;;
;; It works by installing functions on the `find-file-hooks' and on
;; the buffer-local `kill-buffer-hook'.  When a buffer is killed, if
;; it was visiting a file, and place saving has been enabled for it, a
;; database entry is made, saving the location of `point'.  When you
;; visit a file, the database is queried, keyed on `buffer-file-name',
;; and if an entry is found, point is set to the value retrieved, and
;; place saving is enabled again for that buffer.
;;
;; There is a command, which is initially not bound to a keystroke in
;; this incarnation; and may never be, since I think completion works
;; well enough:
;;
;; `M-x toggle-where-was-i'  aka `M-x tog[TAB]w[TAB]'
;;
;; ... that will turn place saving on and off on a per-file, buffer-
;; local basis.	 If `where-was-i-db' has been turned off in a buffer
;; and then you kill it, or exit XEmacs, any record for that file will
;; be purged from the database, and you'll start at the top of the
;; file next time you open it, with place saving turned off there.
;;
;; Place saving is automatically still `on' when you visit a file you'd
;; toggled it on for  in a previous session or visitation.  (That's the
;; point of it, after all.)
;;
;; Toggling place saving on in a buffer visiting a file is all that is
;; required to cause the `where-was-i-db' feature to be autoloaded, as
;; `toggle-where-was-i' will call on `install-where-was-i' if the
;; `wwi-ffh' has not yet been installed.  That WILL NOT cause this
;; feature to be automatically enabled in your next XEmacs invocation,
;; however.  For that, you must customize and save
;; `wwi-auto-install-on-startup-flag'.
;; There is more information about this in its docstring.
;;
;;  This feature can be unloaded with:
;;
;; `C-u M-x install-where-was-i'
;;
;; ... this will call `unload-feature', as well as traverse the
;; `buffer-list' removing the buffer-local `kill-buffer-hook'
;; installed by this program.  Note that any buffers that got place
;; saving enabled by having had an entry in the database for them when
;; they were first visited (thus restoring point to where it was the
;; last time you had visited the file), will not have an updated entry
;; made, nor the old entry removed (as would happen if you
;; `toggle-where-was-i' to off in just that buffer), after you
;; uninstall this feature like that.
;;
;; If you toggle it on again in a buffer visiting a file, then kill
;; that buffer, the point will be saved for it.  When you kill the
;; other buffers that had place saving enabled before you uninstalled
;; the feature with `C-u M-x install-where-was-i', no `where-was-i-db'
;; database update will happen for them, since their buffer-local
;; `kill-buffer-hook' will have been cleaned of the member
;; `wwi-save-where-i-am', the function that writes the `point' entry
;; to the database when a buffer is killed.  If you then re-visit one
;; of those files, point will get restored to the location it did the
;; last time you visited that file with `where-was-i-db' installed.
;;
;; To remove a file from the place saving database, simply visit it,
;; `M-x toggle-where-was-i' to switch `where-was-i-db' off, then kill
;; that buffer.  You can see how this works by looking at the
;; definition of `wwi-save-where-i-am'.  I dare you not to...  (may as
;; well use VI.)
;;
;; After a period of time, the database of saved file positions will
;; become cluttered with the names of files that no longer exist.  You
;; may vacume out the crumbs using:
;;
;; `M-x wwi-vacume-where-was-i-db'
;;
;; ... which will prompt you for an optional regular expression to
;; match files you want records removed for.  It will traverse the
;; database and remove entries for file names that either match the
;; regexp, or that are (not (file-exists-p file-name)).	 The main
;; reason for this command is the removal of stale entries, for files
;; that no longer exist on the filesystem.
;;
;; Note also that running `wwi-vacume-where-was-i-db' will cause EFS
;; traffic, if you've saved your place in any remote files.  Don't be
;; surprised if your dialing daemon picks up the phone when you run
;; the vacume function.  You should be able to purge the database of
;; all EFS entries with a simple regular expression passed to
;; `wwi-vacume-where-was-i-db'.  Of course, you might not want to do
;; that, for obvious reasons.  If it isn't obvious, use VI or PICO.

;;; History:

;; This program is modelled after and obsoletes "saveplace.el",
;; written by Karl Fogel <kfogel@cs.oberlin.edu> in July 1993.

;;; Code:

(eval-when 'load
  (require 'berkeley-db))

;;; Utility functions:

(defun wwi-with-open-database
  (db-file-name db-sym db-thunk &optional type subtype access mode)
  "Open the database DB-FILE-NAME, binding the resultant object to
DB-SYM, and `funcall' DB-THUNK inside that context, closing the
database on exit from the block.  TYPE, SUBTYPE, ACCESS, and MODE, if
given, are passed to `open-database', and default to 'berkeley-db,
'hash, \"rw+\", #o644, respectively."
  (unwind-protect
      (progn
	(or (set db-sym (open-database (expand-file-name db-file-name)
				       (or type 'berkeley-db)
				       (or subtype 'hash)
				       (or access "rw+")
				       (or mode #o644)))
	    (cerror "Couldn't open `%s'." db-file-name))
	(funcall db-thunk))
    (and (symbol-value db-sym) (close-database (symbol-value db-sym)))))

(put 'wwi-with-open-database 'lisp-indent-function 1)

;;; Customize definitions:
(defgroup where-was-i nil
  "Remember where you were in files you have visited."
  :group 'files)

(defcustom wwi-auto-install-on-startup-flag nil
  "Setting and saving this option will cause `where-was-i-db' to be
loaded with `require' when your options.el file is loaded, and setting
it to `on' will cause it to install its hooks at that time also.

Resetting it to the factory-default should make it no longer be
auto-required.

Please read the documentation to `toggle-where-was-i'."
  :type 'boolean
  :tag "Auto-install on startup?"
  :require 'where-was-i-db
  :group 'where-was-i)

(defcustom where-was-i-db
  (cond
   ((boundp 'load-user-init-file-p)
    (paths-construct-path (list user-init-directory ".wwi.db")))
   (t
    (paths-construct-path
     (list (concat "~" (or init-file-user ""))
	   user-init-directory
	   ".wwi.db"))))
  "Location and filename of the `where-was-i-db' database file.
The default location is usually the right place for it."
  :type 'file
  :tag "Database file-name"
  :group 'where-was-i)

;;; Install when loading?
(when wwi-auto-install-on-startup-flag
  (setq wwi-auto-install-on-startup-flag nil)
  (install-where-was-i)
  (message "`where-was-i-db' enabled."))

;;; The toggle and install functions:
;;;###autoload
(defun toggle-where-was-i (&optional parg)
  "Toggle whether to save your place in this file between sessions.

Toggling place saving on in a file's buffer will cause
`where-was-i-db' to install its hooks, via `install-where-was-i'.  You
do not need to explicitly run an install function -- just toggle it on
for a file for which you want the cursor position to be saved.  In
order for this feature to be automatically installed at startup, you
must customize `wwi-auto-install-on-startup-flag'.

Place saving is enabled ONLY for files in which you've explicitly
toggled it on for.  This prevents application programs, such as Gnus,
W3, VM, or your own programs from making extraneous `where-was-i-db'
database entries for their machine-generated files.

If this mode is enabled, point is recorded for you when you kill the
buffer or exit XEmacs.  Visiting a file again that has had a database
entry made in this fashion will cause `point' to be restored to the
saved position, even in a later XEmacs editing session.

If called with a prefix arg, the mode is enabled if and only if the
argument is positive.  This is for use by program code.  In that case,
you may also like to bind the value of `where-was-i-db'.  You should
not globally `setq' it; but bind it with a `let' around the code for
which you wish point saved in a separate-from-the-main database.

There is more information in the comment header of this program.  It
may be worth reading.  If you haven't installed the `.el' codes for
reference, on at least one machine in your system, you deserve to
lose.
"
  ;; The above note is refering to the fact that the `el' files are
  ;; often packaged separately from the byte-compiled `.elc'.  
  (interactive "P")
  (when (not buffer-file-name)
    (error (format "Buffer `%s' not visiting a file" (buffer-name))))
  (when (not (memq 'wwi-ffh find-file-hooks))
    (install-where-was-i))
  (if (or toggle-where-was-i (and parg (<= parg 0)))
      (progn
	(display-message 'no-log "No place will be saved in this file when the buffer is killed.")
	(setq toggle-where-was-i nil))
    (display-message 'no-log "Your place will be saved when this buffer is killed.")
    (make-local-hook 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'wwi-save-where-i-am nil t)
    (setq toggle-where-was-i t)))

;;;###autoload
(defun install-where-was-i (&optional parg)
  "Unconditionally activate `where-was-i-db' by installing some hook
functions.  An optional prefix arg <= 0 will uninstall the feature.

See the function documentation to `toggle-where-was-i' for more
information about the `where-was-i-db' feature, and consult the code
itself for more information about unloading the feature."
  (interactive "p")
  (if (and parg (<= parg 0))
      (progn
	(remove-hook 'find-file-hooks 'wwi-ffh)
	(dolist (buf (buffer-list))
	  (with-current-buffer buf
	    (remove-hook 'kill-buffer-hook 'wwi-save-where-i-am t)))
	;; #### Does `unload-feature' handle buffer-local and permanent-local variables (yet)?
	(unload-feature 'where-was-i-db))
    (add-hook 'find-file-hooks 'wwi-ffh)
    (put 'toggle-where-was-i :menu-tag "Save where you are here?")
    (add-minor-mode 'toggle-where-was-i " S")))

;;; Buffer-local state variable:
;;;###autoload
(defconst toggle-where-was-i nil
  ;; This should be Off by default, and autoloaded so that it is
  ;; properly initialized at startup.
  "This is set when `where-was-i-db' place saving is enabled for this
buffer's file.  This must be set by `M-x toggle-where-was-i', since it
performs additional actions beyond just toggling this permanent
buffer-local flag variable.")
(make-variable-buffer-local 'toggle-where-was-i)
(put 'toggle-where-was-i 'permanent-local t)

;;; The hook functions:
(defun wwi-save-where-i-am ()
  "Save the location of point to the `where-was-i-db' database, if
`toggle-where-was-i' is non-null.  If `toggle-where-was-i' is nil, any
entry for this `buffer-file-name' will be cleared.

This function gets placed on the buffer-local `kill-buffer-hook' by
`wwi-ffh' via `install-where-was-i'.

See also: `toggle-where-was-i'."
  (and buffer-file-name
       (wwi-with-open-database where-was-i-db
	 'wwi-db
	 #'(lambda ()
	     (if toggle-where-was-i
		 (put-database buffer-file-name
			       (format "%S" (case major-mode
					      ((hexl-mode) (hexl-current-address))
					      (t (point))))
			       wwi-db t)
	       (remove-database buffer-file-name wwi-db))))))

(defun wwi-ffh ()
  "`find-file-hooks' function for `where-was-i-db' that looks up
`buffer-file-name' in `where-was-i-db', and restores `point' if
there's an entry for this file.

This gets installed on the global `find-file-hooks' when you execute
`install-where-was-i', or `toggle-where-was-i'."
  (unless after-find-file-from-revert-buffer
    (let ((pos (wwi-with-open-database where-was-i-db
		 'wwi-db #'(lambda ()
			     (string-to-int
			      (get-database buffer-file-name wwi-db "-1"))))))
      (if (>= pos 0)
	  (progn
	    (case major-mode
	      ((hexl-mode) (hexl-goto-address pos))
	      (t (goto-char pos)))
	    (toggle-where-was-i 1))
	(toggle-where-was-i -1)))))	; mostly just for the message.


;;; Database cleanup commands:

;; #### This function tickles the crash bug that I reported... XEmacs
;; aborts in make_uninit_string().  Why???  #### Please advise.
(defun wwi-vacume-where-was-i-db (&optional regexp)

  "Clean the `where-was-i-db' database of records for nonexistent
files or whos file names match an optionally specified REGEXP.

Note also that running `wwi-vacume-where-was-i-db' will cause EFS
traffic, if you've saved your place in any remote files.  Don't be
surprised if your dialing daemon picks up the phone when you run the
vacume function.  You should be able to purge the database of all EFS
entries with a simple regular expression passed to
`wwi-vacume-where-was-i-db'.  Of course, you might not want to do
that, for obvious reasons.  If it isn't obvious, use VI or PICO.
"
  ;; (interactive "sRegexp, or [<-Enter] for none: ")
  (wwi-with-open-database where-was-i-db
    'wwi-db
    #'(lambda ()
	(map-database #'(lambda (key val)
			  (or (and regexp
				   (string-match regexp key)
				   (progn
				     (remove-database key wwi-db)
				     (message "Removing match: %s" key)))
			      (file-exists-p key)
			      (progn
				(remove-database key wwi-db)
				(message "Removing nonexisting: %s" key))))
		      wwi-db))))

(put 'wwi-with-open-database 'lisp-indent-function 1)

(provide 'where-was-i-db)

;;; where-was-i-db.el ends here
