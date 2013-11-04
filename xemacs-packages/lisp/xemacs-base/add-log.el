;;; add-log.el --- change log maintenance commands for Emacs

;; Copyright (C) 1985, 86, 88, 93, 94, 97, 98, 2000 Free Software Foundation, Inc.
;; Copyright (C) 2000 Ben Wing.

;; Maintainer: FSF
;; Keywords: maint

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

;;; Synched up with: Emacs 21.3.

;;; Commentary:

;; This facility is documented in the Emacs Manual.

;;; Code:

(eval-when-compile
  (require 'timezone))

(require 'font-lock)

(defgroup change-log nil
  "Change log maintenance"
  :group 'tools
  :group 'maint
  :link '(custom-manual "(emacs)Change Log")
  :prefix "change-log-"
  :prefix "add-log-")


(defcustom change-log-default-name nil
  "*Name of a change log file for \\[add-change-log-entry]."
  :type '(choice (const :tag "default" nil)
		 string)
  :group 'change-log)

(defcustom change-log-mode-hook nil
  "Normal hook run by `change-log-mode'."
  :type 'hook
  :group 'change-log)

(defcustom add-log-current-defun-function nil
  "*If non-nil, function to guess name of surrounding function.
It is used by `add-log-current-defun' in preference to built-in rules.
Returns function's name as a string, or nil if outside a function."
  :type '(choice (const nil) function)
  :group 'change-log)

(defcustom add-log-full-name nil
  "*Full name of user, for inclusion in ChangeLog daily headers.
This defaults to the value returned by the function `user-full-name'."
  :type '(choice (const :tag "Default" nil)
		 string)
  :group 'change-log)

(defcustom add-log-mailing-address nil
  "*Electronic mail addresses of user, for inclusion in ChangeLog headers.
This defaults to the value of `user-mail-address'.  In addition to
being a simple string, this value can also be a list.  All elements
will be recognized as referring to the same user; when creating a new
ChangeLog entry, one element will be chosen at random."
  :type '(choice (const :tag "Default" nil)
		 (string :tag "String")
		 (repeat :tag "List of Strings" string))
  :group 'change-log)

(defcustom add-log-time-format 'iso8601-time-string
  "*Function that defines the time format.
For example, `iso8601-time-string' (time in international ISO 8601 format)
and `current-time-string' are valid values."
  :type '(radio (const :tag "International ISO 8601 format" iso8601-time-string)
		(const :tag "Old format, as returned by `current-time-string'"
		       current-time-string)
		(function :tag "Other"))
  :group 'change-log)

(defcustom add-log-keep-changes-together nil
  "*If non-nil, normally keep day's log entries for one file together.

Log entries for a given file made with \\[add-change-log-entry] or
\\[add-change-log-entry-other-window] will only be added to others \
for that file made
today if this variable is non-nil or that file comes first in today's
entries.  Otherwise another entry for that file will be started.  An
original log:

	* foo (...): ...
	* bar (...): change 1

in the latter case, \\[add-change-log-entry-other-window] in a \
buffer visiting `bar', yields:

	* bar (...): -!-
	* foo (...): ...
	* bar (...): change 1

and in the former:

	* foo (...): ...
	* bar (...): change 1
	(...): -!-

The NEW-ENTRY arg to `add-change-log-entry' can override the effect of
this variable."
  :version "20.3"
  :type 'boolean
  :group 'change-log)

(defcustom add-log-always-start-new-record nil
  "*If non-nil, `add-change-log-entry' will always start a new record."
  :version "21.4"
  :type 'boolean
  :group 'change-log)

(defcustom add-log-buffer-file-name-function nil
  "*If non-nil, function to call to identify the full filename of a buffer.
This function is called with no argument.  If this is nil, the default is to
use `buffer-file-name'."
  :type '(choice (const nil) function)
  :group 'change-log)

(defcustom add-log-file-name-function nil
  "*If non-nil, function to call to identify the filename for a ChangeLog entry.
This function is called with one argument, the value of variable
`buffer-file-name' in that buffer.  If this is nil, the default is to
use the file's name relative to the directory of the change log file."
  :type '(choice (const nil) function)
  :group 'change-log)


(defcustom change-log-version-info-enabled nil
  "*If non-nil, enable recording version numbers with the changes."
  :version "21.1"
  :type 'boolean
  :group 'change-log)

(defcustom change-log-version-number-regexp-list
  (let ((re    "\\([0-9]+\.[0-9.]+\\)"))
    (list
     ;;  (defconst ad-version "2.15"
     (concat "^(def[^ \t\n]+[ \t]+[^ \t\n][ \t]\"" re)
     ;; Revision: pcl-cvs.el,v 1.72 1999/09/05 20:21:54 monnier Exp
     (concat "^;+ *Revision: +[^ \t\n]+[ \t]+" re)))
  "*List of regexps to search for version number.
The version number must be in group 1.
Note: The search is conducted only within 10%, at the beginning of the file."
  :version "21.1"
  :type '(repeat regexp)
  :group 'change-log)

;; XEmacs change: In the GNU/Emacs version these are all `defface's,
;; but they use the `:inherit' keyword which doesn't exist in XEmacs.
(make-face 'change-log-date-face
	   "Face used to highlight dates in date lines.")
(set-face-parent 'change-log-date-face 'font-lock-string-face)

(make-face 'change-log-name-face
	   "Face for highlighting author names.")
(set-face-parent 'change-log-name-face 'font-lock-constant-face)

(make-face 'change-log-email-face
	   "Face for highlighting author email addresses.")
(set-face-parent 'change-log-email-face 'font-lock-variable-name-face)

(make-face 'change-log-file-face
	   "Face for highlighting file names.")
(set-face-parent 'change-log-file-face 'font-lock-function-name-face)

(make-face 'change-log-list-face
	   "Face for highlighting parenthesized lists of functions or variables.")
(set-face-parent 'change-log-list-face 'font-lock-keyword-face)

(make-face 'change-log-conditionals-face
	   "Face for highlighting conditionals of the form `[...]'.")
(set-face-parent 'change-log-conditionals-face 'font-lock-variable-name-face)

(make-face 'change-log-function-face
	   "Face for highlighting items of the form `<....>'.")
(set-face-parent 'change-log-function-face 'font-lock-variable-name-face)

(make-face 'change-log-acknowledgement-face
	   "Face for highlighting acknowledgments.")
(set-face-parent 'change-log-acknowledgement-face 'font-lock-comment-face)

(defvar change-log-font-lock-keywords
  '(;;
    ;; Date lines, new and old styles.
    ("^\\sw.........[0-9:+ ]*"
     (0 'change-log-date-face)
     ;; Name and e-mail; some people put e-mail in parens, not angles.
     ("\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.-]+@[A-Za-z0-9_.-]+\\)[>)]" nil nil
      (1 'change-log-name-face)
      (2 'change-log-email-face)))
    ;;
    ;; File names.
    ("^\t\\* \\([^ ,:([\n]+\\)"
     (1 'change-log-file-face)
     ;; Possibly further names in a list:
     ("\\=, \\([^ ,:([\n]+\\)" nil nil (1 'change-log-file-face))
     ;; Possibly a parenthesized list of names:
     ("\\= (\\([^) ,:\n]+\\)" nil nil (1 'change-log-list-face))
     ("\\=, *\\([^) ,:\n]+\\)" nil nil (1 'change-log-list-face)))
    ;;
    ;; Function or variable names.
    ("^\t(\\([^) ,:\n]+\\)"
     (1 'change-log-list-face)
     ("\\=, *\\([^) ,:\n]+\\)" nil nil (1 'change-log-list-face)))
    ;;
    ;; Conditionals.
    ("\\[!?\\([^]\n]+\\)\\]\\(:\\| (\\)" (1 'change-log-conditionals-face))
    ;;
    ;; Function of change.
    ("<\\([^>\n]+\\)>\\(:\\| (\\)" (1 'change-log-function-face))
    ;;
    ;; Acknowledgements.
    ("\\(^\t\\|  \\)\\(From\\|Patch\\(es\\)? by\\|Report\\(ed by\\| from\\)\\|Suggest\\(ed by\\|ion from\\)\\)"
     2 'change-log-acknowledgement-face))
  "Additional expressions to highlight in Change Log mode.")
(put 'change-log-mode 'font-lock-defaults
     '(change-log-font-lock-keywords t))

(defvar change-log-mode-map (make-sparse-keymap)
  "Keymap for Change Log major mode.")
(define-key change-log-mode-map "\C-c\C-c" 'change-log-exit)
(define-key change-log-mode-map "\C-c\C-k" 'change-log-cancel)

(defvar change-log-time-zone-rule nil
  "Time zone used for calculating change log time stamps.
It takes the same format as the TZ argument of `set-time-zone-rule'.
If nil, use local time.")

(defun iso8601-time-zone (time)
  (let* ((utc-offset (or (car (current-time-zone time)) 0))
	 (sign (if (< utc-offset 0) ?- ?+))
	 (sec (abs utc-offset))
	 (ss (% sec 60))
	 (min (/ sec 60))
	 (mm (% min 60))
	 (hh (/ min 60)))
    (format (cond ((not (zerop ss)) "%c%02d:%02d:%02d")
		  ((not (zerop mm)) "%c%02d:%02d")
		  (t "%c%02d"))
	    sign hh mm ss)))

;; Autoload this, because it can be generally useful.  It should
;; probably be moved to another file, though.
;;;###autoload
(defun iso8601-time-string ()
  (if change-log-time-zone-rule
      (let ((tz (getenv "TZ"))
	    (now (current-time)))
	(unwind-protect
	    (progn
	      (set-time-zone-rule change-log-time-zone-rule)
	      (concat
	       (format-time-string "%Y-%m-%d " now)
	       (iso8601-time-zone now)))
	  (set-time-zone-rule tz)))
    (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun add-log-convert ()
  "Convert the current buffer from the old ChangeLog format to new.
The old ChangeLogs (before XEmacs 20.2) were created with attribution
lines looking like this:

Mon Feb 10 22:20:16 1997  Hrvoje Niksic  <hniksic@xemacs.org>

The same line in new format looks like this:

1997-02-10  Hrvoje Niksic  <hniksic@xemacs.org>"
  (interactive)
  (while (re-search-forward "^[^\t\n]+[0-9]+  " nil t)
    (beginning-of-line)
    ;; Some ChangeLogs contain interspersed old and new format.  If
    ;; this is old format, just skip it.
    (unless (save-match-data
	      (looking-at "[0-9-]+ "))
      (let* ((date (cdr (split-string (buffer-substring
				       (point)
				       (progn
					 (re-search-forward "  [^0-9]")
					 (goto-char (match-beginning 0)))))))
	     (month (1+ (position (car date)
				  '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
				    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
				  :test 'equal)))
	     (day (string-to-number (cadr date)))
	     (year (string-to-number (cadddr date))))
	(delete-region (point-at-bol) (- (search-forward "  ") 2))
	(beginning-of-line)
	(insert (format "%d-%02d-%02d" year month day))))
    (forward-line 1)))

(defun change-log-name ()
  "Return default name for a change log file."
  ;; XEmacs: remove VMS
  (or change-log-default-name "ChangeLog"))

;;;###autoload
(defun prompt-for-change-log-name ()
  "Prompt for a change log name."
  (let* ((default (find-change-log))
	 (name (expand-file-name
		(read-file-name "Log file: "
                                (file-name-directory default)
                                default nil
                                (file-name-nondirectory default)))))
    ;; Handle something that is syntactically a directory name.
    ;; Look for ChangeLog or whatever in that directory.
    (if (string= (file-name-nondirectory name) "")
	(expand-file-name (file-name-nondirectory default)
			  name)
      ;; Handle specifying a file that is a directory.
      (if (file-directory-p name)
	  (expand-file-name (file-name-nondirectory default)
			    (file-name-as-directory name))
	name))))

(defun change-log-version-number-search ()
  "Return version number of current buffer's file.
This is the value returned by `vc-workfile-version' or, if that is
nil, by matching `change-log-version-number-regexp-list'."
  (let* ((size (buffer-size))
	 (end
	  ;; The version number can be anywhere in the file, but
	  ;; restrict search to the file beginning: 10% should be
	  ;; enough to prevent some mishits.
	  ;;
	  ;; Apply percentage only if buffer size is bigger than
	  ;; approx 100 lines.
	  (if (> size (* 100 80))
	      (/ size 10)
	    size))
	 version)
    (or (and buffer-file-name (vc-workfile-version buffer-file-name))
	(save-restriction
	  (widen)
	  (let ((regexps change-log-version-number-regexp-list))
	    (while regexps
	      (save-excursion
		(goto-char (point-min))
		(when (re-search-forward (pop regexps) end t)
		  (setq version (match-string 1)
			regexps nil)))))))))


;;;###autoload
(defun find-change-log (&optional file-name buffer-file)
  "Find a change log file for \\[add-change-log-entry] and return the name.

Optional arg FILE-NAME specifies the file to use.
If FILE-NAME is nil, use the value of `change-log-default-name'.
If 'change-log-default-name' is nil, behave as though it were 'ChangeLog'
\(or whatever we use on this operating system).

If 'change-log-default-name' contains a leading directory component, then
simply find it in the current directory.  Otherwise, search in the current
directory and its successive parents for a file so named.

Once a file is found, `change-log-default-name' is set locally in the
current buffer to the complete file name.
Optional arg BUFFER-FILE overrides `buffer-file-name'."
  ;; If user specified a file name or if this buffer knows which one to use,
  ;; just use that.
  (or file-name
      (setq file-name (and change-log-default-name
			   (file-name-directory change-log-default-name)
			   change-log-default-name))
      (progn
	;; Chase links in the source file
	;; and use the change log in the dir where it points.
	(setq file-name (or (and (or buffer-file buffer-file-name)
				 (file-name-directory
				  (file-chase-links
				   (or buffer-file buffer-file-name))))
			    default-directory))
	(if (file-directory-p file-name)
	    (setq file-name (expand-file-name (change-log-name) file-name)))
	;; Chase links before visiting the file.
	;; This makes it easier to use a single change log file
	;; for several related directories.
	(setq file-name (file-chase-links file-name))
	(setq file-name (expand-file-name file-name))
	;; Move up in the dir hierarchy till we find a change log file.
	(let ((file1 file-name)
	      parent-dir)
	  (while (and (not (or (get-file-buffer file1) (file-exists-p file1)))
		      (progn (setq parent-dir
				   (file-name-directory
				    (directory-file-name
				     (file-name-directory file1))))
			     ;; Give up if we are already at the root dir.
			     (not (string= (file-name-directory file1)
					   parent-dir))))
	    ;; Move up to the parent dir and try again.
	    (setq file1 (expand-file-name
			 (file-name-nondirectory (change-log-name))
			 parent-dir)))
	  ;; If we found a change log in a parent, use that.
	  (if (or (get-file-buffer file1) (file-exists-p file1))
	      (setq file-name file1)))))
  ;; Make a local variable in this buffer so we needn't search again.
  (set (make-local-variable 'change-log-default-name) file-name)
  file-name)

(defun add-log-file-name (buffer-file log-file)
  ;; Never want to add a change log entry for the ChangeLog file itself.
  (unless (or (null buffer-file) (string= buffer-file log-file))
    (if add-log-file-name-function
	(funcall add-log-file-name-function buffer-file)
      (setq buffer-file
	    (if (string-match
		 (concat "^" (regexp-quote (file-name-directory log-file)))
		 buffer-file)
		(substring buffer-file (match-end 0))
	      (file-name-nondirectory buffer-file)))
      ;; If we have a backup file, it's presumably because we're
      ;; comparing old and new versions (e.g. for deleted
      ;; functions) and we'll want to use the original name.
      (if (backup-file-name-p buffer-file)
	  (file-name-sans-versions buffer-file)
	buffer-file))))

;;;###autoload
(defun add-change-log-entry (&optional whoami file-name other-window new-entry)
  "Find change log file, and add an entry for today and an item for this file.
Optional arg WHOAMI (interactive prefix) non-nil means prompt for user
name and site.

Second arg FILE-NAME is file name of the change log.
If nil, use the value of `change-log-default-name'.

Third arg OTHER-WINDOW non-nil means visit in other window.

Fourth arg NEW-ENTRY non-nil means always create a new entry at the front;
never append to an existing entry.  Option `add-log-keep-changes-together'
otherwise affects whether a new entry is created.

Option `add-log-always-start-new-record' non-nil means always create a
new record, even when the last record was made on the same date and by
the same person.

The change log file can start with a copyright notice and a copying
permission notice.  The first blank line indicates the end of these
notices.

Today's date is calculated according to `change-log-time-zone-rule' if
non-nil, otherwise in local time."
  (interactive (list current-prefix-arg
		     (prompt-for-change-log-name)))
  (or add-log-full-name
      (setq add-log-full-name (user-full-name)))
  (or add-log-mailing-address
      (setq add-log-mailing-address (user-mail-address)))
  (if whoami
      (progn
        (setq add-log-full-name (read-string "Full name: " add-log-full-name))
	;; Note that some sites have room and phone number fields in
	;; full name which look silly when inserted.  Rather than do
	;; anything about that here, let user give prefix argument so that
	;; s/he can edit the full name field in prompter if s/he wants.
	(setq add-log-mailing-address
	      (read-string "Mailing address: " add-log-mailing-address))))

  (let* ((defun (add-log-current-defun))
	 (version (and change-log-version-info-enabled
		       (change-log-version-number-search)))
	 (buf-file-name (if add-log-buffer-file-name-function
			    (funcall add-log-buffer-file-name-function)
			  buffer-file-name))
	 (buffer-file (if buf-file-name (expand-file-name buf-file-name)))
	 (file-name (expand-file-name
		     (or file-name (find-change-log file-name buffer-file))))
	 ;; Set ITEM to the file name to use in the new item.
	 (item (add-log-file-name buffer-file file-name))
	 bound)

;; XEmacs:
    (push-window-configuration)

    (unless (equal file-name buffer-file-name)
      (if (or other-window (window-dedicated-p (selected-window)))
	  (find-file-other-window file-name)
	(find-file file-name)))
    (or (eq major-mode 'change-log-mode)
	(change-log-mode))
    (undo-boundary)
    (goto-char (point-min))

    ;; If file starts with a copyright and permission notice, skip them.
    ;; Assume they end at first blank line.
    (when (looking-at "Copyright")
      (search-forward "\n\n")
      (skip-chars-forward "\n"))

    ;; Advance into first entry if it is usable; else make new one.
    (let ((new-entries (mapcar (lambda (addr)
				 (concat (funcall add-log-time-format)
					 "  " add-log-full-name
					 "  <" addr ">"))
			       (if (consp add-log-mailing-address)
				   add-log-mailing-address
				 (list add-log-mailing-address)))))
      (if (and (not add-log-always-start-new-record)
               (let ((hit nil))
		 (dolist (entry new-entries hit)
		   (when (looking-at (regexp-quote entry))
		     (setq hit t)))))
	  (forward-line 1)
	(insert (nth (random (length new-entries))
		     new-entries)
		"\n\n")
	(forward-line -1)))

    ;; Determine where we should stop searching for a usable
    ;; item to add to, within this entry.
    (setq bound
	  (save-excursion
            (if (looking-at "\n*[^\n* \t]")
                (skip-chars-forward "\n")
	      (if add-log-keep-changes-together
		  (forward-page)	; page delimits entries for date
		(forward-paragraph)))	; paragraph delimits entries for file
	    (point)))

    ;; Now insert the new line for this item.
    (cond ((re-search-forward "^\\s *\\*\\s *$" bound t)
	   ;; Put this file name into the existing empty item.
	   (if item
	       (insert item)))
	  ((and (not new-entry)
		(let (case-fold-search)
		  (re-search-forward
		   (concat (regexp-quote (concat "* " item))
			   ;; Don't accept `foo.bar' when
			   ;; looking for `foo':
			   "\\(\\s \\|[(),:]\\)")
		   bound t)))
	   ;; Add to the existing item for the same file.
	   (re-search-forward "^\\s *$\\|^\\s \\*")
	   (goto-char (match-beginning 0))
	   ;; Delete excess empty lines; make just 2.
	   (while (and (not (eobp)) (looking-at "^\\s *$"))
	     (delete-region (point) (point-at-bol 2)))
	   (insert-char ?\n 2)
	   (forward-line -2)
	   (indent-relative-maybe))
	  (t
	   ;; Make a new item.
	   (while (looking-at "\\sW")
	     (forward-line 1))
	   (while (and (not (eobp)) (looking-at "^\\s *$"))
	     (delete-region (point) (point-at-bol 2)))
	   (insert-char ?\n 3)
	   (forward-line -2)
	   (indent-to left-margin)
	   (insert "* ")
	   (if item (insert item))))
    ;; Now insert the function name, if we have one.
    ;; Point is at the item for this file,
    ;; either at the end of the line or at the first blank line.
    (if (not defun)
	;; No function name, so put in a colon unless we have just a star.
	(unless (save-excursion
		  (beginning-of-line 1)
		  (looking-at "\\s *\\(\\*\\s *\\)?$"))
	  (insert ": ")
	  (if version (insert version ?\ )))
      ;; Make it easy to get rid of the function name.
      (undo-boundary)
      (unless (save-excursion
		(beginning-of-line 1)
		(looking-at "\\s *$"))
	(insert ?\ ))
      ;; See if the prev function name has a message yet or not.
      ;; If not, merge the two items.
      (let ((pos (point-marker)))
	(skip-syntax-backward " ")
	(skip-chars-backward "):")
	(if (and (looking-at "):")
		 (> fill-column (+ (current-column) (length defun) 4)))
	    (progn (delete-region (point) pos) (insert ", "))
	  (if (looking-at "):")
	      (delete-region (+ 1 (point)) (point-at-eol)))
	  (goto-char pos)
	  (insert "("))
	(set-marker pos nil))
      (insert defun "): ")
      (if version (insert version ?\ )))))

;;;###autoload
(defun add-change-log-entry-other-window (&optional whoami file-name)
  "Find change log file in other window and add entry and item.
This is just like `add-change-log-entry' except that it displays
the change log file in another window."
  (interactive (if current-prefix-arg
		   (list current-prefix-arg
			 (prompt-for-change-log-name))))
  (add-change-log-entry whoami file-name t))
;;;###autoload (define-key ctl-x-4-map "a" 'add-change-log-entry-other-window)

;;;###autoload
(define-derived-mode change-log-mode text-mode "Change Log"
  "Major mode for editing change logs; like Indented Text Mode.
Prevents numeric backups and sets `left-margin' to 8 and `fill-column' to 74.
New log entries are usually made with \\[add-change-log-entry] or \\[add-change-log-entry-other-window].
Each entry behaves as a paragraph, and the entries for one day as a page.
Runs `change-log-mode-hook'."
  (setq left-margin 8
	fill-column 74
	indent-tabs-mode t
	tab-width 8)
  (set (make-local-variable 'fill-paragraph-function)
       'change-log-fill-paragraph)
  (set (make-local-variable 'indent-line-function) 'indent-to-left-margin)
  ;; XEmacs change:
  ;; Lines containing only a change descriptor:
  ;;
  ;; * add-log.el (change-log-mode):
  ;;
  ;; separate paragraphs and are not part of them.  Lines containing a
  ;; change descriptor followed by text are the start of a paragraph,
  ;; and filled with the text following.  This caters to both styles
  ;; of annotations, e.g.

; 	* func-menu.el (fume-function-name-regexp-make): Use `\s_' symbol
; 	class instead of hardcoded `_' and `/' characters.

  ;; and

; 	* func-menu.el (fume-function-name-regexp-make):
; 	Use `\s_' symbol class instead of hardcoded `_' and `/'
; 	characters.

  ;; We really do want "^" in paragraph-start below: it is only the
  ;; lines that begin at column 0 (despite the left-margin of 8) that
  ;; we are looking for.  Adding `* ' allows eliding the blank line
  ;; between entries for different files.
  (let ((change-descriptor "\\*\\s +\\S +\\(\\s +(\\S +)\\)?:"))
    (set (make-local-variable 'paragraph-start)
	 (concat "\\s *$\\|\f\\|^\\<\\|" change-descriptor))
    ;; FSF comment: (?)
    ;; Let all entries for one day behave as one page.
    (set (make-local-variable 'paragraph-separate)
	 (concat "\\s *$\\|\f\\|^\\<\\|" change-descriptor "\\s *$")))
  ;; Match null string on the date-line so that the date-line
  ;; is grouped with what follows.
  (set (make-local-variable 'page-delimiter) "^\\<\\|^\f")
  (set (make-local-variable 'version-control) 'never)
  (set (make-local-variable 'smerge-resolve-function)
       'change-log-resolve-conflict)
  (set (make-local-variable 'adaptive-fill-regexp) "\\s *")
  ;; XEmacs change:
;   (set (make-local-variable 'font-lock-defaults)
;        '(change-log-font-lock-keywords t nil nil backward-paragraph)))
  ;; XEmacs:
  (when (boundp 'filladapt-mode)
    ;; Filladapt works badly with ChangeLogs.  Still, we disable it
    ;; before change-log-mode-hook, so the users can override this
    ;; choice. #### Maybe not true anymore now that we are a derived mode.
    (setq filladapt-mode nil)))

(defun change-log-exit ()
  "Save the change-log buffer, and restores the old window configuration.
Buries the buffer."
  (interactive)
  (save-buffer)
  (let ((buf (current-buffer)))
    (pop-window-configuration)
    (bury-buffer buf)))

(defun change-log-cancel ()
  "Cancel the changes to change-log buffer.
This kills the buffer without saving, and restores the old window
 configuration."
  (interactive)
  (kill-buffer (current-buffer))
  (pop-window-configuration))

;; It might be nice to have a general feature to replace this.  The idea I
;; have is a variable giving a regexp matching text which should not be
;; moved from bol by filling.  change-log-mode would set this to "^\\s *\\s(".
;; But I don't feel up to implementing that today.
(defun change-log-fill-paragraph (&optional justify)
  "Fill the paragraph, but preserve open parentheses at beginning of lines.
Prefix arg means justify as well."
  (interactive "P")
  (let ((end (progn (forward-paragraph) (point)))
	(beg (progn (backward-paragraph) (point)))
	(paragraph-start (concat paragraph-start "\\|\\s *\\s(")))
    (fill-region beg end justify)
    t))

(defcustom add-log-current-defun-header-regexp
  ;; XEmacs change
  "^\\([[:upper:]][[:upper:]_0-9 ]*[[:upper:]_0-9]\\|[-_.[:alpha:]0-9]+\\)[ \t]*[:=]"
  "*Heuristic regexp used by `add-log-current-defun' for unknown major modes."
  :type 'regexp
  :group 'change-log)

;;;###autoload
(defvar add-log-lisp-like-modes
    '(emacs-lisp-mode lisp-mode scheme-mode dsssl-mode lisp-interaction-mode)
  "*Modes that look like Lisp to `add-log-current-defun'.")

;;;###autoload
(defvar add-log-c-like-modes
    '(c-mode c++-mode c++-c-mode objc-mode java-mode)
  "*Modes that look like C to `add-log-current-defun'.")

;;;###autoload
(defvar add-log-tex-like-modes
    '(TeX-mode plain-TeX-mode LaTeX-mode plain-tex-mode latex-mode)
  "*Modes that look like TeX to `add-log-current-defun'.")

(defun-when-void match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (let ((result
		 (substring string (match-beginning num) (match-end num))))
	    (set-text-properties 0 (length result) nil result)
	    result)
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))

;;;###autoload
(defun add-log-current-defun ()
  "Return name of function definition point is in, or nil.

Understands C, Lisp, LaTeX (\"functions\" are chapters, sections, ...),
Texinfo (@node titles) and Perl.

Other modes are handled by a heuristic that looks in the 10K before
point for uppercase headings starting in the first column or
identifiers followed by `:' or `='.  See variables
`add-log-current-defun-header-regexp' and
`add-log-current-defun-function'.

Has a preference of looking backwards."
  (condition-case nil
      (save-excursion
	(let ((location (point)))
	  (cond (add-log-current-defun-function
		 (funcall add-log-current-defun-function))
		((memq major-mode add-log-lisp-like-modes)
		 ;; If we are now precisely at the beginning of a defun,
		 ;; make sure beginning-of-defun finds that one
		 ;; rather than the previous one.
		 (or (eobp) (forward-char 1))
		 (beginning-of-defun)
		 ;; Make sure we are really inside the defun found,
		 ;; not after it.
		 (when (and (looking-at "\\s(")
			    (progn (end-of-defun)
				   (< location (point)))
			    (progn (forward-sexp -1)
				   (>= location (point))))
		   (if (looking-at "\\s(")
		       (forward-char 1))
		   ;; Skip the defining construct name, typically "defun"
		   ;; or "defvar".
		   (forward-sexp 1)
		   ;; The second element is usually a symbol being defined.
		   ;; If it is not, use the first symbol in it.
		   (skip-chars-forward " \t\n'(")
		   (buffer-substring-no-properties (point)
						   (progn (forward-sexp 1)
							  (point)))))
		((and (memq major-mode add-log-c-like-modes)
		      (save-excursion
			(beginning-of-line)
			;; Use eq instead of = here to avoid
			;; error when at bob and char-after
			;; returns nil.
			(while (eq (char-after (- (point) 2)) ?\\)
			  (forward-line -1))
			(looking-at "[ \t]*#[ \t]*define[ \t]")))
		 ;; Handle a C macro definition.
		 (beginning-of-line)
		 (while (eq (char-after (- (point) 2)) ?\\) ;not =; note above
		   (forward-line -1))
		 (search-forward "define")
		 (skip-chars-forward " \t")
		 (buffer-substring-no-properties (point)
						 (progn (forward-sexp 1)
							(point))))
		((memq major-mode add-log-c-like-modes)
		 (beginning-of-line)
		 ;; See if we are in the beginning part of a function,
		 ;; before the open brace.  If so, advance forward.
		 (while (not (looking-at "{\\|\\(\\s *$\\)"))
		   (forward-line 1))
		 (or (eobp)
		     (forward-char 1))
		 (beginning-of-defun)
		 (when (progn (end-of-defun)
			      (< location (point)))
		   (backward-sexp 1)
		   (let (beg tem)

		     (forward-line -1)
		     ;; Skip back over typedefs of arglist.
		     (while (and (not (bobp))
				 (looking-at "[ \t\n]"))
		       (forward-line -1))
		     ;; See if this is using the DEFUN macro used in Emacs,
		     ;; or the DEFUN macro used by the C library.
		     (if (condition-case nil
			     (and (save-excursion
				    (end-of-line)
				    (while (= (preceding-char) ?\\)
				      (end-of-line 2))
				    (backward-sexp 1)
				    (beginning-of-line)
				    (setq tem (point))
				    (looking-at "DEFUN\\b"))
				  (>= location tem))
			   (error nil))
			 (progn
			   (goto-char tem)
			   (down-list 1)
			   (if (= (char-after (point)) ?\")
			       (progn
				 (forward-sexp 1)
				 (skip-chars-forward " ,")))
			   (buffer-substring-no-properties
			    (point)
			    (progn (forward-sexp 1)
				   (point))))
		       (if (looking-at "^[+-]")
			   (change-log-get-method-definition)
			 ;; Ordinary C function syntax.
			 (setq beg (point))
			 (if (and
			      ;; Protect against "Unbalanced parens" error.
			      (condition-case nil
				  (progn
				    (down-list 1) ; into arglist
				    (backward-up-list 1)
				    (skip-chars-backward " \t")
				    t)
				(error nil))
			      ;; Verify initial pos was after
			      ;; real start of function.
			      (save-excursion
				(goto-char beg)
				;; For this purpose, include the line
				;; that has the decl keywords.  This
				;; may also include some of the
				;; comments before the function.
				(while (and (not (bobp))
					    (save-excursion
					      (forward-line -1)
					      (looking-at "[^\n\f]")))
				  (forward-line -1))
				(>= location (point)))
			      ;; Consistency check: going down and up
			      ;; shouldn't take us back before BEG.
			      (> (point) beg))
			     (let (end middle)
			       ;; Don't include any final whitespace
			       ;; in the name we use.
			       (skip-chars-backward " \t\n")
			       (setq end (point))
			       (backward-sexp 1)
			       ;; Now find the right beginning of the name.
			       ;; Include certain keywords if they
			       ;; precede the name.
			       (setq middle (point))
			       (forward-word -1)
			       ;; Ignore these subparts of a class decl
			       ;; and move back to the class name itself.
			       (while (looking-at "public \\|private ")
				 (skip-chars-backward " \t:")
				 (setq end (point))
				 (backward-sexp 1)
				 (setq middle (point))
				 (forward-word -1))
			       (and (bolp)
				    (looking-at
				     "enum \\|struct \\|union \\|class ")
				    (setq middle (point)))
			       (goto-char end)
			       (when (eq (preceding-char) ?=)
				 (forward-char -1)
				 (skip-chars-backward " \t")
				 (setq end (point)))
			       (buffer-substring-no-properties
				middle end))))))))
		((memq major-mode add-log-tex-like-modes)
		 (if (re-search-backward
		      "\\\\\\(sub\\)*\\(section\\|paragraph\\|chapter\\)"
		      nil t)
		     (progn
		       (goto-char (match-beginning 0))
		       (buffer-substring-no-properties
			(1+ (point))	; without initial backslash
			(point-at-eol)))))
		((eq major-mode 'texinfo-mode)
		 (if (re-search-backward "^@node[ \t]+\\([^,\n]+\\)" nil t)
		     (match-string-no-properties 1)))
		((memq major-mode '(perl-mode cperl-mode))
		 (if (re-search-backward "^sub[ \t]+\\([^({ \t\n]+\\)" nil t)
		     (match-string-no-properties 1)))
		;; Emacs's autoconf-mode installs its own
		;; `add-log-current-defun-function'.  This applies to
		;; a different mode apparently for editing .m4
		;; autoconf source.
                ((eq major-mode 'autoconf-mode)
                 (if (re-search-backward
		      "^\\(\\(m4_\\)?define\\|A._DEFUN\\)(\\[?\\([A-Za-z0-9_]+\\)" nil t)
                     (match-string-no-properties 3)))
		(t
		 ;; If all else fails, try heuristics
		 (let (case-fold-search
		       result)
		   (end-of-line)
		   (when (re-search-backward
			  add-log-current-defun-header-regexp
			  (- (point) 10000)
			  t)
		     (setq result (or (match-string-no-properties 1)
				      (match-string-no-properties 0)))
		     ;; Strip whitespace away
		     (when (string-match "\\([^ \t\n\r\f].*[^ \t\n\r\f]\\)"
					 result)
		       (setq result (match-string-no-properties 1 result)))
		     result))))))
    (error nil)))

(defvar change-log-get-method-definition-md)

;; Subroutine used within change-log-get-method-definition.
;; Add the last match in the buffer to the end of `md',
;; followed by the string END; move to the end of that match.
(defun change-log-get-method-definition-1 (end)
  (setq change-log-get-method-definition-md
	(concat change-log-get-method-definition-md
		(match-string 1)
		end))
  (goto-char (match-end 0)))

(defun change-log-get-method-definition ()
"For objective C, return the method name if we are in a method."
  (let ((change-log-get-method-definition-md "["))
    (save-excursion
      (if (re-search-backward "^@implementation\\s-*\\([A-Za-z_]*\\)" nil t)
	  (change-log-get-method-definition-1 " ")))
    (save-excursion
      (cond
       ((re-search-forward "^\\([-+]\\)[ \t\n\f\r]*\\(([^)]*)\\)?\\s-*" nil t)
	(change-log-get-method-definition-1 "")
	(while (not (looking-at "[{;]"))
	  (looking-at
	   "\\([A-Za-z_]*:?\\)\\s-*\\(([^)]*)\\)?[A-Za-z_]*[ \t\n\f\r]*")
	  (change-log-get-method-definition-1 ""))
	(concat change-log-get-method-definition-md "]"))))))

(defun change-log-sortable-date-at ()
  "Return date of log entry in a consistent form for sorting.
Point is assumed to be at the start of the entry."
  (require 'timezone)
  (if (looking-at "^\\sw.........[0-9:+ ]*")
      (let ((date (match-string-no-properties 0)))
	(if date
	    (if (string-match "\\(....\\)-\\(..\\)-\\(..\\)\\s-+" date)
		(concat (match-string 1 date) (match-string 2 date)
			(match-string 3 date))
	      (condition-case nil
		  (timezone-make-date-sortable date)
		(error nil)))))
    (error "Bad date")))

(defun change-log-resolve-conflict ()
  "Function to be used in `smerge-resolve-function'."
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf (match-beginning 1) (match-end 1))
      (save-match-data (change-log-mode))
      (let ((other-buf (current-buffer)))
	(with-current-buffer buf
	  (save-excursion
	    (save-restriction
	      (narrow-to-region (match-beginning 0) (match-end 0))
	      (replace-match (match-string 3) t t)
	      (change-log-merge other-buf))))))))

;;;###autoload
(defun change-log-merge (other-log)
  "Merge the contents of ChangeLog file OTHER-LOG with this buffer.
Both must be found in Change Log mode (since the merging depends on
the appropriate motion commands).  OTHER-LOG can be either a file name
or a buffer.

Entries are inserted in chronological order.  Both the current and
old-style time formats for entries are supported."
  (interactive "*fLog file name to merge: ")
  (if (not (eq major-mode 'change-log-mode))
      (error "Not in Change Log mode"))
  (let ((other-buf (if (bufferp other-log) other-log
		     (find-file-noselect other-log)))
	(buf (current-buffer))
	date1 start end)
    (save-excursion
      (goto-char (point-min))
      (set-buffer other-buf)
      (goto-char (point-min))
      (if (not (eq major-mode 'change-log-mode))
	  (error "%s not found in Change Log mode" other-log))
      ;; Loop through all the entries in OTHER-LOG.
      (while (not (eobp))
	(setq date1 (change-log-sortable-date-at))
	(setq start (point)
	      end (progn (forward-page) (point)))
	;; Look for an entry in original buffer that isn't later.
	(with-current-buffer buf
	  (while (and (not (eobp))
		      (string< date1 (change-log-sortable-date-at)))
	    (forward-page))
	  (if (not (eobp))
	      (insert-buffer-substring other-buf start end)
	    ;; At the end of the original buffer, insert a newline to
	    ;; separate entries and then the rest of the file being
	    ;; merged.
	    (unless (or (bobp)
			(and (= ?\n (char-before))
			     (or (<= (1- (point)) (point-min))
				 (= ?\n (char-before (1- (point)))))))
	      (insert "\n"))
	    ;; Move to the end of it to terminate outer loop.
	    (with-current-buffer other-buf
	      (goto-char (point-max)))
	    (insert-buffer-substring other-buf start)))))))

;;;###autoload
(defun patch-to-change-log (devdir &rest cl-keys)
  "Convert the unified diff in the current buffer into a ChangeLog.
DEVDIR (queried interactively) specifies the directory the diff was
made relative to.  The ChangeLog entries are added to the appropriate
ChangeLog files (generally in the same directory as the diffed file but
possibly in a parent directory), which are left as modified Emacs buffers
but not saved out to disk.  After running this, you should go to the various
buffers and annotate the entries appropriately.

To work on a region on the current buffer, narrow to that region first.

NOTE: This function handles diff output both from `cvs diff' and just
running `diff' directly, but *ONLY* unified-format (-u) diffs. #### Someone
should fix this to handle context diffs as well.

The following keys are allowed:
- :my-name defines the name to use in ChangeLog entries
  (defaults to `(or add-log-full-name (user-full-name))'),
- :my-email defines the email address to use in ChangeLog entries
  (defaults to `(or add-log-mailing-address (user-mail-address))'),
- :dry-run prevents `patch-to-changelog' from generating the ChangeLog
   entries: ChangeLog files are only loaded (defaults to nil),
- :keep-source-files prevents `patch-to-changelog' from killing the source
  file buffers after the ChangeLog skeleton is created
  (defaults to nil),
- :extent-property instructs `patch-to-changelog' to create extents
  containing the newly created ChangeLog entries, with that property set
  (defaults to nil),
- :extent-property-value is used in conjunction with :extent-property to
  specify a value for the extent property
  (defaults to nil)."
  (interactive "DBase directory of patch: ")
  (cl-parsing-keywords
      ((:my-name (or add-log-full-name (user-full-name)))
       (:my-email (or add-log-mailing-address (user-mail-address)))
       :dry-run :keep-source-files :extent-property :extent-property-value)
      ()
    (let* ((old-font-lock-auto-fontify font-lock-auto-fontify)
	   (font-lock-auto-fontify nil)
	   (file-re1 "^Index: \\([^\n]*\\)")
	   (file-re2 "^\\+\\+\\+ \\([^\t]*\\)")
	   (hunk-re "^@@ -[0-9]+,[0-9]+ \\+\\([0-9]+\\),\\([0-9]+\\) @@")
	   (basename-re "\\`\\(.*\\)/\\(.*\\)\\'")
	   (lisp-defun-re "(def[a-z-]* \\([^ \n]+\\)")
; 	   (c-token-re "[][_a-zA-Z0-9]+")
; 	   (ws-re "\\(\\s-\\|\n\\+\\)*")
; 	   (c-multi-token-re (concat c-token-re "\\(" ws-re c-token-re "\\)*"))
; 	   (c-defun-re (concat "^+\\(" c-token-re ws-re "\\)*"
; 			       "\\(" c-token-re "\\)" ws-re "(" ws-re
; 			       "\\("
; 			       c-multi-token-re ws-re
; 			       "\\(," ws-re c-multi-token-re ws-re "\\)*"
; 			       "\\)?" ws-re ")" ws-re "{" ws-re "$"))
	   (new-defun-re (concat "^\\+" lisp-defun-re))
	   (nomore-defun-re (concat "^-" lisp-defun-re))
           (new-heuristic-fun-re
            (concat "^\\+" (substring add-log-current-defun-header-regexp 1)))
           (nomore-heuristic-fun-re
            (concat "^-" (substring add-log-current-defun-header-regexp 1)))
	   (done-hash       (make-hash-table :size 20 :test 'equal))
	   (new-fun-hash    (make-hash-table :size 20 :test 'equal))
	   (nomore-fun-hash (make-hash-table :size 20 :test 'equal))
	   (new-heuristic-fun-hash    (make-hash-table :size 20 :test 'equal))
	   (nomore-heuristic-fun-hash (make-hash-table :size 20 :test 'equal))
	   change-log-buffer change-log-buffers change-log-directory
	   file absfile limit current-defun
	   dirname basename previous-dirname
	   all-entries first-file-re-p
	   insertion-marker
	   )

      (flet
	  ((add-change-log-string
	    (str)
	    (with-current-buffer change-log-buffer
	      (goto-char insertion-marker)
	      (insert-before-markers str)))

	   (add-entry
	    (filename line fun str)
	    (let ((entry (cons filename fun)))
	      (unless (or (gethash entry done-hash)
			  (string-match "\n." str))
		;; (message "%s %S" str (gethash entry done-hash))
		(puthash entry t done-hash)
		(push (cons str line) all-entries))))

	   (flush-change-log-entries
	    ()
	    (setq all-entries (sort all-entries #'cdr-less-than-cdr))
	    (mapc #'(lambda (entry)
		      (add-change-log-string (car entry)))
		  all-entries)
	    (setq all-entries nil))

	   (line-num () (1+ (count-lines (point-min) (point-at-bol))))

	   (finish-up-change-log-buffer
	    ()
	    (push change-log-buffer change-log-buffers)
	    (unless cl-dry-run
	      (add-change-log-string "\n"))
	    (with-current-buffer change-log-buffer
	      (goto-char (point-min)))))

	(save-excursion
	  (goto-char (point-min))
	  (while (or (prog1 (re-search-forward file-re1 nil t)
		       (setq first-file-re-p t))
		     (prog1 (re-search-forward file-re2 nil t)
		       (setq first-file-re-p nil)))
	    (setq file (match-string 1))
	    (if (string-match basename-re file)
		(setq dirname  (match-string 1 file)
		      basename (match-string 2 file))
	      (setq dirname "" basename file))
	    (setq absfile (expand-file-name file devdir))
	    (setq limit
		  (save-excursion (or (re-search-forward
				       (if first-file-re-p file-re1 file-re2)
				       nil t)
				      (point-max))))
	    (when (not (equal dirname previous-dirname))
	      (if previous-dirname
		  (finish-up-change-log-buffer))
	      (setq previous-dirname dirname)
	      (setq change-log-buffer
		    (let ((font-lock-auto-fontify
			   old-font-lock-auto-fontify))
		      (find-file-noselect
		       ;; APA: find a change-log relative to current directory.
		       (with-temp-buffer
			 (cd (expand-file-name dirname devdir))
			 (find-change-log)))))
	      (setq change-log-directory
		    (with-current-buffer change-log-buffer default-directory))
	      (unless cl-dry-run
		(when cl-extent-property
		  (with-current-buffer change-log-buffer
		    (set-extent-properties
			(make-extent (point-min) (point-min))
		      (list 'end-open nil
			    cl-extent-property cl-extent-property-value))))
		(setq insertion-marker (point-min-marker change-log-buffer))
		(add-change-log-string
		 (format (concat "%s  " cl-my-name "  <" cl-my-email
				 ">\n\n")
			 (iso8601-time-string)))))
            ;; APA: Standardize on / in ChangeLog entry paths.
            (let ((directory-sep-char ?/))
              (setq basename
                    (file-relative-name absfile change-log-directory)))
	    ;; now do each hunk in turn.
	    (unless cl-dry-run
	      (while (re-search-forward hunk-re limit t)
		(let* ((hunk-start-line (line-num))
		       (first-file-line (string-to-int (match-string 1)))
		       (hunk-limit
			(save-excursion (or (and
					     (re-search-forward hunk-re limit
								t)
					     (match-beginning 0))
					    limit)))
		       ;; numlines is the number of lines in the hunk, not
		       ;; the number of file lines affected by the hunk, i.e.
		       ;; (match-string 2), which is generally less
		       (numlines (1- (- (save-excursion
					  (goto-char hunk-limit)
					  (line-num))
					hunk-start-line))))

		  ;; do added and/or removed functions.
		  (clrhash new-fun-hash)
		  (clrhash nomore-fun-hash)
		  (save-excursion
		    (while (re-search-forward new-defun-re hunk-limit t)
		      (puthash (match-string 1)
			       (1- (- (line-num) hunk-start-line))
			       new-fun-hash)))
		  (save-excursion
		    (while (re-search-forward nomore-defun-re hunk-limit t)
		      (let ((fun (match-string 1)))
			(if (gethash fun new-fun-hash)
			    (remhash fun new-fun-hash)
			  (puthash fun
				   (1- (- (line-num) hunk-start-line))
				   nomore-fun-hash)))))
		  ;; do added and/or removed variable heuristics.
		  (clrhash new-heuristic-fun-hash)
		  (clrhash nomore-heuristic-fun-hash)
		  (save-excursion
		    (while (re-search-forward
                            new-heuristic-fun-re hunk-limit t)
		      (let ((fun (match-string 1)))
                        (unless (gethash fun new-fun-hash)
                          (puthash (match-string 1)
                                   (1- (- (line-num) hunk-start-line))
                                   new-heuristic-fun-hash)))))
		  (save-excursion
		    (while (re-search-forward
                            nomore-heuristic-fun-re hunk-limit t)
		      (let ((fun (match-string 1)))
			(if (gethash fun new-heuristic-fun-hash)
			    (remhash fun new-heuristic-fun-hash)
                          (unless (gethash fun nomore-fun-hash)
                            (puthash fun
                                     (1- (- (line-num) hunk-start-line))
                                     nomore-heuristic-fun-hash))))))
		  (maphash
		   #'(lambda (fun val)
		       (add-entry
			basename
			;; this is not a perfect measure of the actual
			;; file line, but good enough for sorting.
			(+ first-file-line val)
			fun
			(format "\t* %s (%s): New.\n" basename fun)))
		   new-fun-hash)
		  (maphash
		   #'(lambda (fun val)
		       (add-entry
			basename
			(+ first-file-line val)
			fun
			(format "\t* %s (%s): Removed.\n" basename fun)))
		   nomore-fun-hash)
                  (maphash
                   #'(lambda (fun val)
                       (add-entry
                        basename
                        ;; this is not a perfect measure of the actual
                        ;; file line, but good enough for sorting.
                        (+ first-file-line val)
                        fun
                        (format "\t* %s (%s): New.\n" basename fun)))
                   new-heuristic-fun-hash)
                  (maphash
                   #'(lambda (fun val)
                       (add-entry
                        basename
                        (+ first-file-line val)
                        fun
                        (format "\t* %s (%s): Removed.\n" basename fun)))
                   nomore-heuristic-fun-hash)

		  ;; now try to handle what changed.
		  (let (trylines
			(trystart t)
			(line-in-file first-file-line))

		    ;; accumulate a list of lines to check.  we check
		    ;; only changed lines, and only the first such line
		    ;; per blank-line-delimited block (we assume all
		    ;; functions are preceded by a blank line).
		    (save-excursion
		      (dotimes (n numlines)
			(forward-line 1)
			(if (looking-at ".\n")
			    (setq trystart t))
			(when (not (eq ?  (char-after)))
			  (when trystart
			    (setq trylines (cons line-in-file trylines))
			    (setq trystart nil)))
			;; N is not an accurate gauge of the file line,
			;; because of the presence of deleted lines in the
			;; hunk.
			(when (not (eq ?- (char-after)))
			  (incf line-in-file))))
		    (setq trylines (nreverse trylines))
		    (save-excursion
		      (let ((already-visiting-p (get-file-buffer absfile)))
			(set-buffer (find-file-noselect absfile))
			(mapc #'(lambda (n)
				  (goto-line n)
				  (setq current-defun (add-log-current-defun))
				  (add-entry
				   basename
				   (if current-defun n 0)
				   current-defun
				   (format (if current-defun
					       "\t* %s (%s):\n" "\t* %s:\n")
					   basename current-defun)))
			      trylines)
			(unless (or already-visiting-p cl-keep-source-files)
			  (kill-buffer (current-buffer))))))))
	      (flush-change-log-entries))
	    ))
        ;; the patch might be totally blank.
	(if change-log-buffer
	    (finish-up-change-log-buffer))
	;; return the list of ChangeLog buffers
	change-log-buffers))))

;;;###autoload
(defun change-log-redate ()
  "Fix any old-style date entries in the current log file to default format."
  (interactive)
  (require 'timezone)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\sw.........[0-9:+ ]*" nil t)
      (unless (= 12 (- (match-end 0) (match-beginning 0)))
	(let* ((date (save-match-data
		       (timezone-fix-time (match-string 0) nil nil)))
	       (zone (if (consp (aref date 6))
			 (nth 1 (aref date 6)))))
	  (replace-match (format-time-string
			  "%Y-%m-%d  "
			  (encode-time (aref date 5)
				       (aref date 4)
				       (aref date 3)
				       (aref date 2)
				       (aref date 1)
				       (aref date 0)
				       zone))))))))

;; XEmacs additions
;;;###autoload(add-to-list 'auto-mode-alist '("[Cc]hange.?[Ll]og?\\(?:.[0-9]+\\)?\\'" . change-log-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode))

(provide 'add-log)

;;; add-log.el ends here
