;;; recent-files.el --- Maintain menu of recently opened files.
;;; $Header: /pack/xemacscvs/XEmacs/packages/xemacs-packages/edit-utils/recent-files.el,v 1.9 2004/11/08 02:38:42 ben Exp $
;;;
;;; Copyright (C) 1994, 1995 Juergen Nickelsen <nickel@cs.tu-berlin.de>
;;; Copyright (C) 2002 Ben Wing.
;;;
;; Keywords: menu, file

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
;;;
;;; recent-files.el is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; It is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with XEmacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;; ------------------------------------------------------------------
;;;
;;; Enough of this boring stuff. To install recent-files, put the
;;; following statements into your .emacs file 
;;;    (load "recent-files")
;;;    (recent-files-initialize)
;;; and place the file recent-files.el in a directory in your XEmacs's
;;; load-path.  In order to use recent-files with dired, dired has to
;;; be loaded first.  recent-files is known to work with Lucid Emacs /
;;; XEmacs 19.8 and higher; it does not work correctly with 19.6 or
;;; earlier versions due to a bug in add-menu.
;;;
;;; recent-files adds the menu "Recent Files" (or whatever name you
;;; choose, see "Customization:" below) to Emacs's menubar. Its
;;; entries are the files (and directories) that have recently been
;;; opened by Emacs. You can open one of these files again by
;;; selecting its entry in the "Recent Files" menu. The list of file
;;; entries in this menu is preserved from one Emacs session to
;;; another. You can prevent Emacs from saving this list by selecting
;;; "Don't save recent-files list on exit" from the menu. If you have
;;; disabled saving, you can re-enable it by selecting "Save
;;; recent-files list on exit".
;;;
;;; The menu has permanent and non-permanent entries. Permanent
;;; entries are marked with an asterisk in front of the filename. The
;;; non-permanent entries are hidden in a submenu.
;;;
;;; Each time you open a file in Emacs, it is added as a non-permanent
;;; entry to the menu. The value of `recent-files-number-of-entries'
;;; determines how many non-permanent entries are held in the
;;; menu. When the number of non-permanent entries reaches this value,
;;; the least recently added non-permanent entry is removed from the
;;; menu when another non-permanent entry is added. It is not removed
;;; from the list, though; it may reappear when entries are deleted
;;; from the list. The number of entries saved to disk is the value of
;;; the variable `recent-files-number-of-saved-entries'.
;;;
;;; Permanent entries are not removed from the menu. You can make a
;;; file entry permanent by selecting "Make <buffer> permanent" (where
;;; <buffer> is the name of the current buffer) when the current
;;; buffer holds this file. "Make <buffer> non-permanent" makes the
;;; file entry of the current buffer non-permanent.
;;;
;;; The command "Kill buffer <buffer> and delete entry" is handy when
;;; you have accidently opened a file but want to keep neither the
;;; buffer nor the entry.
;;;
;;; You can erase the list of non-permanent entries by selecting
;;; "Erase non-permanent entries" from the menu.
;;;
;;; Customization:
;;;
;;; There are lots of variables to control the behaviour of
;;; recent-files. You do not have to change any of them if you like it
;;; as it comes out of the box. However, you may want to look at these
;;; options to make it behave different.
;;;
;;; `recent-files-number-of-entries'
;;;    Controls how many non-permanent entries are shown in the
;;;    recent-files list.  The default is 15. 
;;;
;;; `recent-files-number-of-saved-entries'
;;;    Controls how many non-permanent entries are saved to disk when
;;;    Emacs exits or recent-files-save-the-list is called. The
;;;    default is 50.
;;;
;;; `recent-files-save-file'
;;;    The name of the file where the recent-files list is saved
;;;    between Emacs session. You probably don't need to change this.
;;;    The default is ".recent-files.el" in your home directory.
;;;
;;; `recent-files-dont-include'
;;;    A list of regular expressions for files that should not be
;;;    included into the recent-files list. This list is empty by
;;;    default. For instance, a list to exclude all .newsrc
;;;    files, all auto-save-files, and all files in the /tmp
;;;    directory (but not the /tmp directory itself) would look
;;;    like this:
;;;         (setq recent-files-dont-include
;;;               '("/\\.newsrc" "~$" "^/tmp/."))
;;;    The default is empty.
;;;
;;; `recent-files-use-full-names'
;;;    If the value of this variable is non-nil, the full pathnames of
;;;    the files are shown in the recent-files menu. Otherwise only
;;;    the filename part (or the last name component if it is a
;;;    directory) is shown in the menu. The default it t, i.e. show
;;;    full names.
;;;
;;; `recent-files-filename-replacements'
;;;    This is a list of pairs of regular expressions and replacement
;;;    strings. If a filename matches one of the regular expressions,
;;;    the matching part is replaced by the replacement string for
;;;    display in the recent-files menu.
;;;    Example: My home directory is "/users/mmc/nickel/". I want to
;;;    replace it with "~/". I also want to replace the directory
;;;    "/imports/teleservices/mmc/avc2/", where I work a lot, with
;;;    ".../avc2/". The list then looks like
;;;        (setq recent-files-filename-replacements
;;;              '(("/users/mmc/nickel/" . "~/")
;;;                ("/imports/teleservices/mmc/avc2/" . ".../avc2/")))
;;;    Only the first match is replaced. So, if you have several
;;;    entries in this list that may match a filename simultaneously,
;;;    put the one you want to match (usually the most special) in
;;;    front of the others. The default is to replace the home
;;;    directory with "~".
;;;
;;; `recent-files-sort-function'
;;;    Contains a function symbol to sort the display of filenames in
;;;    the recent-files menu. Supplied are two functions,
;;;    'recent-files-dont-sort and 'recent-files-sort-alphabetically.
;;;    The first, which is the default, preserves the order of "most
;;;    recent on top". 
;;;
;;; `recent-files-permanent-submenu'
;;;    If this variable is non-nil, the permanent entries are put into
;;;    a separate submenu of the recent-files menu. The default is
;;;    nil.
;;;
;;; `recent-files-non-permanent-submenu'
;;;    If this variable is non-nil, the non-permanent entries are put
;;;    into a separate submenu of the recent-files menu. [[The default
;;;    is currently t, but probably should be nil, and we may change it
;;;    back. (You can set both `recent-files-permanent-submenu' and
;;;    `recent-files-non-permanent-submenu' to t to have both lists in
;;;    separate submenus.)]] Now defaults to nil.
;;;
;;; `recent-files-commands-submenu'
;;;    If this variable is non-nil, the commands if recent-files are
;;;    placed in a submenu of the recent-files menu. The default is
;;;    nil.
;;;
;;; `recent-files-commands-submenu-title'
;;;    If the commands are placed in a submenu, this string is used as
;;;    the title of the submenu. The default is "Commands...".
;;;
;;; `recent-files-actions-on-top'
;;;    If this variable is non-nil, the "action" menu entries ("Make
;;;    <buffer> permanent" etc.) are put on top of the menu. Otherwise
;;;    they appear below the file entries or submenus. The default is
;;;    nil.
;;;
;;; `recent-files-permanent-first'
;;;    If this variable is t, the permanent entries are put first in
;;;    the recent-files menu, i.e. above the non-permanent entries. If
;;;    the value is nil, non-permanent entries appear first. If the
;;;    value is neither t nor nil, the entries are sorted according to
;;;    recent-files-sort-function. The default is 'sort.
;;;
;;; `recent-files-find-file-command'
;;;    This variable contains to commandto execute when a file entry
;;;    is selected from the menu. Usually this will be `find-file',
;;;    which is the default.
;;;
;;; KNOWN BUG:
;;;   - recent-files overwrites the recent-files-save-file
;;;     unconditionally when Emacs exits. If you have two Emacs
;;;     processes running, the one exiting later will overwrite the
;;;     file without merging in the new entries from the other Emacs
;;;     process. This can be avoided by disabling the save on exit from
;;;     the menu.

(if (not (string-match "XEmacs" (emacs-version)))
    (error "recent-files works with Lucid Emacs / XEmacs only."))

(provide 'recent-files)


;;; User options

(defgroup recent-files nil
  "Maintain a menu of recently opened files."
  :group 'files
  :group 'menu)

(defgroup recent-files-menu nil
  "Menu options of recent-files."
  :prefix "recent-files-"
  :group 'recent-files)


(defcustom recent-files-number-of-entries 15
  "*Maximum of non-permanent entries in the recent-files menu."
  :type 'integer
  :group 'recent-files)

(defcustom recent-files-number-of-saved-entries 50
  "*Maximum of non-permanent entries saved to `recent-files-save-file'."
  :type 'integer
  :group 'recent-files)

(defcustom recent-files-save-file (expand-file-name "~/.recent-files.el")
  "*File to save the recent-files list in."
  :type 'file
  :group 'recent-files)

(defcustom recent-files-dont-include nil
  "*List of regexps for filenames *not* to keep in recent-files."
  :type '(repeat regexp)
  :group 'recent-files)

(defcustom recent-files-use-full-names t
  "*If non-nil, use the full pathname of a file in the recent-files menu.
Otherwise use only the filename part. The `recent-files-filename-replacements'
are not applied in the latter case."
  :type 'boolean
  :group 'recent-files)

(defcustom recent-files-filename-replacements
  (list (cons (regexp-quote (expand-file-name "~")) "~"))
  "*List of regexp/replacement pairs for filename filenames.
If a filename of a filename matches one of the regexps, it is replaced
by the corresponding replacement."
  :type '(repeat (cons regexp (string :tag "Replacement")))
  :group 'recent-files)

(defcustom recent-files-sort-function (function recent-files-dont-sort)
  "*Function to sort the recent-files list with.
The value `recent-files-dont-sort' means to keep the \"most recent on top\"
order."
  :type 'function
  :group 'recent-files)

(defcustom recent-files-permanent-submenu nil
  "*If non-nil, put the permanent entries of recent-files into a submenu."
  :type 'boolean
  :group 'recent-files-menu)

(defcustom recent-files-non-permanent-submenu nil
  "*If non-nil, put the non-permanent entries of recent-files into a submenu."
  :type 'boolean
  :group 'recent-files-menu)

(defcustom recent-files-commands-submenu nil
  "*If non-nil, put the commands of recent-files into a submenu."
  :type 'boolean
  :group 'recent-files-menu)

(defcustom recent-files-commands-submenu-title "Commands..."
  "*Title of the commands submenu of recent-files."
  :type 'string
  :group 'recent-files-menu)

(defcustom recent-files-menu-title "Recent Files"
  "*Name to be displayed as title of the recent-files menu."
  :type 'string
  :group 'recent-files-menu)

(defcustom recent-files-menu-path nil
  "*Path where to add the recent-files menu.
A value of nil means add it as top-level menu.
For more information look up the documentation of `add-menu'."
  :type '(choice (const :tag "Top Level" nil)
		 (sexp :tag "Menu Path"))
  :group 'recent-files-menu)

(defcustom recent-files-add-menu-before nil
  "*Name of the menu before which the recent-files menu shall be added.
A value of nil means add it as the last menu in recent-files-menu-path.
For more information look up the documentation of `add-menu'."
  :type '(choice (string :tag "Name")
		 (const :tag "Last" nil))
  :group 'recent-files-menu)

(defcustom recent-files-actions-on-top nil
  "*If non-nil, put the actions on top of the recent-files menu."
  :type 'boolean
  :group 'recent-files-menu)

(defcustom recent-files-permanent-first 'sort
  "*Control the placement of entries in the recent-files menu.
If the value is t, permanent entries are put first.
If the value is nil, non-permanent entries are put first.
If the value is neither, the entries are mixed following
recent-files-sort-function if neither appear in a submenu."
  :type '(choice (const :tag "Permanent First" t)
		 (const :tag "Non-Permanent First" nil)
		 (sexp :tag "Mixed"))
  :group 'recent-files-menu)

(defcustom recent-files-find-file-command (function find-file)
  "*Command to invoke with an entry of the recent-files list."
  :type 'function
  :group 'recent-files)

(defcustom recent-files-include-save-now nil
  "*If non-nil, have a menu entry to save the recent-files list immediately."
  :type 'boolean
  :group 'recent-files-menu)

;;; Internal variables

(defconst recent-files-save-list-on-exit t
  "If non-nil, save the recent-files list on exit.
This value is toggled by a menu entry.")

(defvar recent-files-list nil
  "List of recently opened files.
Entries are pairs like (<filename> . <permanent-p>).
If <permanent-p> is non-nil, the file stays permanently in the list.")

(defvar recent-files-commands-menu
  '(list (vector (concat "Make " lastpart " permanent")
		 (function recent-files-make-permanent)
		 (and lastpart
		      (not (recent-files-permanent-p filename))
		      ;; (not (not ...)) is needed to enforce t for non-nil
		      (not (not (recent-files-retrieve-entry filename)))))
	 (vector (concat "Make " lastpart " non-permanent")
		 (function recent-files-make-non-permanent)
		 (and lastpart
		      (recent-files-permanent-p filename)
		      (not (not (recent-files-retrieve-entry filename)))))
	 (vector "Erase non-permanent entries"
		 (function recent-files-erase-non-permanent)
		 t)
	 (vector (if recent-files-save-list-on-exit
		     "Don't save recent-files list on exit"
		   "Save recent-files list on exit")
		 ;; for some weird reason a (function (lambda ...))
		 ;; doesn't work here
		 (function recent-files-toggle-save-list-on-exit)
		 t)
	 (vector "Save recent-files list now"
		 (function recent-files-save-the-list)
		 t)
	 (vector (concat "Kill buffer " lastpart
			 " and delete entry")
		 (function recent-files-kill-buffer-delete-entry)
		 lastpart))
  "Command menu definition for recent-files.
This definition is evaluated in a context where `filename' holds the file
name of the current buffer and `lastpart' holds the last component of
`filename'.")

(defvar recent-files-menu-omit-chars-list '(?n ?m ?e ?d ?s ?k))


(defconst recent-files-save-file-header
  ";; This file is generated by recent-files.
;; The car of each entry of recent-files-save-list is to appear in the
;; `recent-files' menu. If the cdr of an entry is t, the file is to stay
;; in the menu permanently.
;; Saved at %s.

" "Header to be written into the `recent-files-save-file'.")


(defconst recent-files-buffer-name " *recent files save list*"
  "Name of the buffer to build the save file in.")

(defvar recent-files-list-changed-p t
  "Non-nil if the recent-files-list has changed after last menubar update.")

(defvar recent-files-last-buffer nil
  "Buffer at the time of last recent-files menu rebuild.
If the buffer has changed, the menu must be rebuilt.")

;;; Module initialization

;;;###autoload
(defun recent-files-initialize ()
  "Initialize the recent-files menu."
  (interactive)
  (add-hook 'find-file-hooks (function recent-files-find-and-write-file-hook))
  (add-hook 'dired-after-readin-hook
	    (function recent-files-find-and-write-file-hook))
  (add-hook 'kill-emacs-hook (function recent-files-save-the-list))
  (add-hook 'activate-menubar-hook (function recent-files-update-menu))
  (add-hook 'write-file-hooks (function recent-files-find-and-write-file-hook))
  ;; Initialize recent-files-list only if it is non-nil.
  (cond (recent-files-list
	 (message "recent-files is already initialized."))
	((file-readable-p recent-files-save-file)
	 (setq recent-files-list-changed-p t)
	 (load-file recent-files-save-file)))
  (recent-files-update-menu))


(defun recent-files-version ()
  "Return a string identifying the current version of recent-files.
If called interactively, show it in the echo area."
  (interactive)
  (let ((version "$Header: /pack/xemacscvs/XEmacs/packages/xemacs-packages/edit-utils/recent-files.el,v 1.9 2004/11/08 02:38:42 ben Exp $"))
    (if (interactive-p)
	(message version)
      version)))
      

;;; Hook functions

(defun recent-files-find-and-write-file-hook ()
  "Find-file-hook, write-file-hook, and dired-mode-hook for recent-files.
Inserts the name of the file just opened or written into the
`recent-files-list' and updates the recent-files menu."
  (recent-files-add-file (recent-files-get-file-name))
  nil)


(defun recent-files-get-file-name ()
  "Return the filename of the current buffer or nil, if there is none.
This functions is supposed to do \"the right thing\" also for some modes
with no buffer-file-name. Currently supported: 'dired-mode."
  (cond (buffer-file-name
	 buffer-file-name)
	((eq major-mode 'dired-mode)
	 (dired-current-directory))))


(defun recent-files-save-the-list ()
  "Save the current `recent-files-list' to the file `recent-files-save-file'.
This is done by writing a `setq' statement to `recent-files-list' into
the file."
  (interactive)
  (if recent-files-save-list-on-exit
      (let ((l (recent-files-enforce-max-length
		recent-files-number-of-saved-entries
		recent-files-list)))
	(save-excursion
	  (set-buffer (get-buffer-create recent-files-buffer-name))
	  (erase-buffer)
	  (insert (format recent-files-save-file-header (current-time-string)))
	  (insert "(setq recent-files-list \n      '(")
	  (if l
	      (progn
		(while l
		  (if (bolp)
		      (insert "        "))
		  (prin1 (car l) (current-buffer))
		  (insert "\n")
		  (setq l (cdr l)))
		(forward-line -1)))
	  (end-of-line)
	  (insert "))")
	  (if (file-writable-p recent-files-save-file)
	      (write-region (point-min) (point-max) recent-files-save-file))
	  (kill-buffer (current-buffer))))))


;;; Construct the menu

(defun recent-files-maybe-generate-accelerator-spec (list)
  "Add auto-generated accelerators, if such capability exists.
This is a basic wrapper around `submenu-generate-accelerator-spec'; if it
exists, we just call it; otherwise, we do nothing."
  (if (fboundp 'submenu-generate-accelerator-spec)
      (submenu-generate-accelerator-spec list
					 recent-files-menu-omit-chars-list)
    list))

(defun recent-files-update-menu ()
  "Update the recent-files menu from the recent-files-list."
  (if (or recent-files-list-changed-p
	  (not recent-files-last-buffer)
	  (not (eq recent-files-last-buffer
		   (current-buffer))))
      ;; This is an ugly mess...
      (let ((action-menu-entries
	     (let ((entries
		    (let* ((filename (recent-files-get-file-name))
			   (lastpart (recent-files-last-part-of-name
				      filename)))
		      (eval recent-files-commands-menu))))
	       (if recent-files-commands-submenu
		   (list (cons recent-files-commands-submenu-title
			       entries))
		 entries)))	       
	    permanent non-permanent all)
	;; ... getting weirder by the minute ...
	(if (or recent-files-permanent-submenu
		recent-files-non-permanent-submenu
		(null recent-files-permanent-first)
		(eq t recent-files-permanent-first))
	    (progn 
	      (setq permanent (recent-files-maybe-generate-accelerator-spec
			       (recent-files-make-file-menu-entries
				recent-files-list
				(function recent-files-filter-permanent))))
	      (setq non-permanent (recent-files-maybe-generate-accelerator-spec
				   (recent-files-make-file-menu-entries
				    recent-files-list
				    (function
				     recent-files-filter-non-permanent)))))
	  (setq all (recent-files-maybe-generate-accelerator-spec
		     (recent-files-make-file-menu-entries
		      recent-files-list
		      (function (lambda (l) l))))))
	(if recent-files-permanent-submenu
	    (setq permanent
		  (list (cons "Permanent entries..."
			      permanent))))
	(if recent-files-non-permanent-submenu
	    (setq non-permanent
		  (list (cons "Non-permanent entries..."
			      non-permanent))))
    ;;; ... and now even uglier.
	(let ((menu (nconc
		     (if recent-files-actions-on-top
			 (append action-menu-entries (list "-----")))
		     (if (or recent-files-permanent-submenu
			     recent-files-non-permanent-submenu)
			 (if recent-files-permanent-first
			     (nconc permanent non-permanent)
			   (nconc non-permanent permanent))
		       ;; in this case, neither permanent nor non-permanent
		       ;; files are in submenus.  we need to decide whether
		       ;; to separate them or combine them, and in the
		       ;; former case, which one goes first.  also, we
		       ;; regenerate the accelerator specs to be continuous
		       ;; across all of them. (it's easier to do it this
		       ;; way than try to carefully avoid generating them
		       ;; twice, and totally safe, since the
		       ;; spec-generating code knows how to remove existing
		       ;; auto-generated specs before adding new ones.)
		       (cond ((eq t recent-files-permanent-first)
			      (recent-files-maybe-generate-accelerator-spec
			       (nconc permanent non-permanent)))
			     ((null recent-files-permanent-first)
			      (recent-files-maybe-generate-accelerator-spec
			       (nconc non-permanent permanent)))
			     (t all)))
		     (if (not recent-files-actions-on-top)
			 (cons "-----"
			       action-menu-entries)))))
	  (add-menu recent-files-menu-path recent-files-menu-title
		    menu recent-files-add-menu-before))
	(setq recent-files-list-changed-p nil)
	(setq recent-files-last-buffer (current-buffer))))
  nil)


(defun recent-files-retrieve-entry (filename)
  "Retrieve an entry from the recent-files list."
  (assoc filename recent-files-list))


(defun recent-files-make-file-menu-entries (recent-list filter)
  "Make file menu entries for recent-files from RECENT-LIST using FILTER."
  (mapcar (function recent-files-entry-to-menu-entry)
	  (funcall filter
		   (funcall recent-files-sort-function
				  (recent-files-enforce-max-length
				   recent-files-number-of-entries
				   recent-list)))))


(defun recent-files-last-part-of-name (filename)
  "Return last part of FILENAME."
  (if filename
      (if (and (file-directory-p filename)
	       (equal (substring filename -1) "/"))
	  (concat (file-name-nondirectory
		   (substring filename 0 -1))
		  "/")
	(file-name-nondirectory filename))))


(defun recent-files-filter-permanent (recent-list)
  "Return list of permanent entries in RECENT-LIST."
  (cond ((null recent-list) nil)
	((recent-files-permanent-p (car (car recent-list)))
	 (cons (car recent-list)
	       (recent-files-filter-permanent (cdr recent-list))))
	(t (recent-files-filter-permanent (cdr recent-list)))))


(defun recent-files-filter-non-permanent (recent-list)
  "Return list of non-permanent entries in RECENT-LIST."
  (cond ((null recent-list) nil)
	((recent-files-permanent-p (car (car recent-list)))
	 (recent-files-filter-non-permanent (cdr recent-list)))
	(t (cons (car recent-list)
		 (recent-files-filter-non-permanent (cdr recent-list))))))


(defun recent-files-permanent-p (filename)
  "Return non-nil if FILENAME is a permanent entry in the recent-files menu."
  (cdr (recent-files-retrieve-entry filename)))


(defun recent-files-entry-to-menu-entry (entry)
  "Build a menu entry from an entry in `recent-files-list'."
  (let ((prepend (if (cdr entry)
                     (if recent-files-permanent-submenu "" "* ")
		   (if recent-files-non-permanent-submenu "" "  "))))
    (vector (concat prepend
                    (if recent-files-use-full-names
                        (recent-files-replace-filenames (car entry))
		      (recent-files-last-part-of-name (car entry))))
            (list recent-files-find-file-command (car entry))
            t)))


(defun recent-files-replace-filenames (filename)
  "Replace the part of FILENAME that matches a regular expression
in recent-files-filename-replacements with the corresponding replacement.
If FILENAME does not match any regular expression, return it unchanged.
Only the first matching regexp/replacement pair is applied."
  (let ((replist recent-files-filename-replacements)
	(retval filename)
	(matched nil))
    (while (and replist
		(not matched))
      (if (string-match (car (car replist)) filename)
	  (progn
	    (setq matched t)
	    (setq retval (concat (substring filename 0 (match-beginning 0))
				 (cdr (car replist))
				 (substring filename (match-end 0))))))
      (setq replist (cdr replist)))
    retval))


;;; add a new entry

(defun recent-files-add-file (filename)
  "Add file FILENAME to `recent-files-list'.
FILENAME is not really added if it matches one of the regexps in
`recent-files-dont-include'."
  (if (and filename (recent-files-no-match filename recent-files-dont-include))
      (progn
	(setq recent-files-list-changed-p t)
	(setq recent-files-list
	      (cons (or (recent-files-retrieve-entry filename)
			(cons filename nil))
		    (recent-files-remove-entry filename
					       recent-files-list))))))


(defun recent-files-dont-sort (recent-list)
  "Return RECENT-LIST.
This is a dummy sorting function for the recent-files-list."
  recent-list)

(defun recent-files-sort-alphabetically (recent-list)
  "Return RECENT-LIST sorted alphabetically by the cars of the elements."
  (sort recent-list (function
		     (lambda (e1 e2)
		       (string-lessp (car e1) (car e2))))))


(defun recent-files-enforce-max-length (n l)
  "Return a list of all permanent and the first N non-permanent entries of L.
Preserve the order of the entries."
  (let ((count 0)
	(newlist nil))
    (while l
      (if (cdr (car l))
	  (setq newlist (cons (car l) newlist))
	(if (< count n)
	    (setq newlist (cons (car l) newlist)))
	(setq count (1+ count)))
      (setq l (cdr l)))
    (nreverse newlist)))


(defun recent-files-remove-entry (fname recent-list)
  "Delete all elements that have FNAME as a car from RECENT-LIST.
The constructed list returned, RECENT-LIST is not changed.
Comparison is done with equal."
  (let ((newlist nil))
    (while recent-list
      (if (not (equal (car (car recent-list)) fname))
	  (setq newlist (cons (car recent-list) newlist)))
      (setq recent-list (cdr recent-list)))
    (nreverse newlist)))

(defun recent-files-no-match (string re-list)
  "Return t if STRING matches none of the regexps in RE-LIST."
  (while (and re-list
	      (not (string-match (car re-list) string)))
    (setq re-list (cdr re-list)))
  (null re-list))


;;; Menu commands

(defun recent-files-make-permanent ()
  "Make the file in current buffer a permanent entry in recent-files."
  (interactive)
  (rplacd (recent-files-retrieve-entry (recent-files-get-file-name)) t)
  (setq recent-files-list-changed-p t))


(defun recent-files-make-non-permanent ()
  "Make the file in current buffer a non-permanent entry in recent-files."
  (interactive)
  (rplacd (recent-files-retrieve-entry (recent-files-get-file-name)) nil)
  (setq recent-files-list-changed-p t))


(defun recent-files-kill-buffer-delete-entry ()
  "Kill the current buffer and delete its entry in the recent-files menu."
  (interactive)
  (setq recent-files-list
	(recent-files-remove-entry (recent-files-get-file-name)
				   recent-files-list))
  (setq recent-files-list-changed-p t)
  (kill-buffer (current-buffer)))

(defun recent-files-erase-non-permanent ()
  "Erase all non-permanent entries from the recent-files menu."
  (interactive)
  (setq recent-files-list
	(recent-files-filter-permanent recent-files-list))
  (setq recent-files-list-changed-p t))

(defun recent-files-toggle-save-list-on-exit ()
  "Toggle the value of `recent-files-save-list-on-exit'."
  (interactive)
  (setq recent-files-save-list-on-exit (not recent-files-save-list-on-exit))
  (setq recent-files-list-changed-p t))

;; User commands
;;;###autoload
(defun recent-files-visit-file (filename &optional codesys)
  "Visit a recent file.
Visit a file FILENAME that was visited recently.  Optional second argument
specifies the coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system."
  (interactive (list (progn
                       (unless recent-files-list
                         (recent-files-initialize))
                       (completing-read "Recent file: " recent-files-list))
                     (if current-prefix-arg
                         (read-coding-system "Coding system: ")
                       nil)))
  (find-file filename codesys))
;;; EOF
