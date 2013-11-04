;; cvs.el --- Light cvs support for emacs (ediff + msb + dired + mode line)
;;
;; Copyright (C) 1995-1998 Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;;
;; Author: Frederic Lepied <Frederic.Lepied@sugix.frmug.org>
;; Version: $Id: cvs.el,v 1.3 2004/08/15 19:12:35 scop Exp $
;; Keywords: cvs ediff mode-line
;;
;; LCD Archive Entry:
;; cvs|Frederic Lepied|Frederic.Lepied@sugix.frmug.org|
;; Light cvs support for emacs (ediff + msb + dired + mode line).|
;; $Date: 2004/08/15 19:12:35 $|$Revision: 1.3 $|~/modes/cvs.el.gz|
;;
;; This program is free  software;  you can redistribute   it and/or modify  it
;; under the terms of  the GNU General Public  License as published by the Free
;; Software  Foundation; either version  2 of the  License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it  will be useful, but WITHOUT
;; ANY WARRANTY;  without even  the   implied warranty  of  MERCHANTABILITY  or
;; FITNESS FOR A  PARTICULAR PURPOSE.  See  the GNU General  Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free  Software Foundation, Inc., 675 Mass
;; Ave, Cambridge, MA 02139, USA.
;;
;; Purpose of this package:
;;   1. Display CVS revision in mode line.
;;   2. Compare file changes between CVS revisions using ediff.
;;   3. Some keystrokes and menu entries to execute cvs status, cvs log and
;; cvsann (Thanks to Ray Nickson <nickson@cs.uq.oz.au>).
;;   4. Simple interface to cvs commit and cvs update commands.
;;   5. Status listing per directory courtesy of Stephan Heuel
;; <steve@ipb.uni-bonn.de>.
;;   6. msb support (better buffer selection).
;;   7. dired support.
;;   8. softlink tree support.
;;   9. little module support (status and update).
;;
;; Installation:
;;   put cvs.el in a directory in your load-path and byte compile it.
;;   then put (require 'cvs) in your .emacs or in site-start.el
;;
;; Thanks to Darryl  Okahata <darrylo@sr.hp.com> for the module status
;; enhancements, branch merge new command and the module update new command.

;;=============================================================================
;; dependencies
;;=============================================================================
(require 'easymenu)

;;=============================================================================
(defconst cvs:version "$Id: cvs.el,v 1.3 2004/08/15 19:12:35 scop Exp $"
  "Version number of cvs.el. To communicate with bug report")

;;=============================================================================
(defconst cvs:maintainer-address "cvs-help@sugix.frmug.org"
  "Address to send any comment, bug or report")

;;=============================================================================
(defvar cvs:current-revision  nil
  "Stores the CVS revision number of the file")
(make-variable-buffer-local 'cvs:current-revision)

;;=============================================================================
(defvar cvs-temp-dir (or (getenv "TMPDIR")
			 (getenv "TMP")
			 (getenv "TEMP"))
  "* if non nil, `cvs-temp-dir' is the directory where to extract versions.")

;;=============================================================================
(defvar cvs-command "cvs"
  "Name of the cvs command including path if needed")

;;=============================================================================
(defvar cvsann-command "cvsann"
  "Name of the cvsann command including path if needed")

;;=============================================================================
(defvar cvs-root nil
  "*If non nil, `cvs-root' is the base directory of the CVS repository.")

;;=============================================================================
(defvar cvs-minor-mode-hooks nil
  "Hooks run when Cvs mode is initialized")

;;=============================================================================
(defvar cvs-load-hooks nil
  "Hooks run when cvs.el has been loaded")

;;=============================================================================
(defvar cvs-commit-hooks nil
  "Hooks run entering commit buffer")

;;=============================================================================
(defvar cvs-before-commit-hooks nil
  "Hooks run before commiting")

;;=============================================================================
(defvar cvs-add-hooks nil
  "Hooks run after adding a file into CVS with `cvs-add'")

;;=============================================================================
(defvar cvs-mark-hooks nil
  "Hooks run after marking or unmarking a file with `cvs-mark'")

;;=============================================================================
(defvar cvs-shell-command
  (if (memq system-type '(ms-dos emx windows-nt))
      shell-file-name
    "/bin/sh")
  "Name of the shell used in cvs commands.
It is initialized from `shell-file-name' on most systems.
NB: Not all shells may be adequate as `shell-file-name'.")

;;=============================================================================
(defvar cvs-shell-command-option
  (cond ((memq system-type '(ms-dos windows-nt))
         (if (boundp 'shell-command-switch)
             shell-command-switch
           "/c"))
        (t                              ;Unix & EMX (Emacs 19 port to OS/2)
         "-c"))
  "Shell argument indicating that next argument is the command.
It is initialized from `shell-command-switch' on most systems.
NB: Not all shells may have an adequate `shell-command-switch'.")

;;=============================================================================
(defvar cvs-file-option "-F"
  "CVS option to read log from a file.
Some CVS versions need \"-f\" others need \"-F\".")

;;=============================================================================
(defvar cvs-diff-options ()
  "*Optional arguments passed to the cvs diff command (`cvs-diff').
For example you can do unified diff with '(\"-u\").")

;;=============================================================================
(defvar cvs-no-log-option nil
  "CVS option not to log the cvs_command in the command history.")

;;=============================================================================
(defvar cvs-never-use-emerge nil
  "* don't merge update conflicts with emerge function from ediff package if set to t.")

;;=============================================================================
(defvar cvs-ediff-merge-no-cleanup nil
  "*If not nil, do not rename *ediff-merge* buffer after the merge.")

;;=============================================================================
(defvar cvs-save-prefix ".#"
  "prefix used by cvs to save a file if there are conflicts while updating")

;;=============================================================================
;; minor mode status variable (buffer local).
;;=============================================================================
(defvar cvs-minor-mode nil
  "Status variable to switch to CVS minor mode if sets to t")
(make-variable-buffer-local 'cvs-minor-mode)
(put 'cvs-minor-mode 'permanent-local t)

;;=============================================================================
(defvar cvs-minor-mode-in-modeline t
  "If non-nil display the version number in the modeline.
This variable has to be set before loading the package cvs.el.")

;;=============================================================================
;; minor mode status variable (buffer local).
;;=============================================================================
(defvar cvs:mark nil
  "Status variable to say if a file will be commited in the next commit command.")
(make-variable-buffer-local 'cvs:mark)
(put 'cvs:mark 'permanent-local t)

;;=============================================================================
(defvar cvs:marked-list nil
  "List of marked files. See `cvs-mark'")

(defvar cvs:commit-list nil
  "List of files uppon which to perform cvs commit")

;;=============================================================================
;; minor mode entry point.
;;=============================================================================
(defun cvs-minor-mode (&optional arg)
  "
Help to admin CVS controlled files :
	\\[cvs-log]		display cvs log output.
	\\[cvs-file-status]		display cvs status output.
	\\[cvs-annotate]		display cvs annotate output.
	\\[cvs-history]		display cvs history output.
	\\[cvs-who]		display who is responsable of the selected region.
	\\[cvs-description]		change the description message.
	\\[cvs-change-log]		change a log message.
	\\[cvs-edit]		run the cvs edit or unedit command.
	\\[cvs-ediff-internal]		run ediff between current file and a revision.
	\\[cvs-ediff]		run ediff between two revisions of the file.
	\\[cvs-diff]		display diff between current file and a revision.
	\\[cvs-version-other-window]		retrieve a specified version in an other window.
	\\[cvs-update-file]		update the file from the repository.
	\\[cvs-revert]		revert the file to a previous version from the repository.
	\\[cvs-merge-backup]		run a merger to remove conflict from the update process.
	\\[cvs-mark]		add/remove a file to the marked list (toggle).
	\\[cvs-flush]		make the marked list empty.
	\\[cvs-commit]		perform the cvs commit command on the marked list.
	\\[cvs-commit-file]		perform the cvs commit command on the current file.
	\\[cvs-status-process]		display the status of the files in the current module.
	\\[cvs-marked-status]		display the status of the marked files.
	\\[cvs-submit-report]		send a bug/comment report to the cvs.el maintainer."
  (setq cvs-minor-mode (if (null arg) t (car arg)))
  (if cvs-minor-mode
      (progn
	(easy-menu-add cvs:menu cvs:map)
	(run-hooks 'cvs-minor-mode-hooks)))
  cvs-minor-mode)

;;=============================================================================
;; register cvs minor mode keymap and mode line display.
;;=============================================================================
(defvar cvs:map (make-sparse-keymap)
  "CVS minor mode keymap")

(defvar cvs:commit-map (make-sparse-keymap)
  "CVS commit edition buffer keymap")

(define-key cvs:map "\C-cvW" 'cvs-who)
(define-key cvs:map "\C-cvo" 'cvs-log)
(define-key cvs:map "\C-cvs" 'cvs-file-status)
(define-key cvs:map "\C-cve" 'cvs-editors)
(define-key cvs:map "\C-cvw" 'cvs-watchers)
(define-key cvs:map "\C-x\C-q" 'cvs-edit)
(define-key cvs:map "\C-cvU" 'cvs-update-directory)
(define-key cvs:map "\C-cvS" 'cvs-status-process)
(define-key cvs:map "\C-cvM" 'cvs-marked-status)
(define-key cvs:map "\C-cvB" 'cvs-merge-branch)
(define-key cvs:map "\C-cvL" 'cvs-status-mark-changed)
(define-key cvs:map "\C-cvd" 'cvs-ediff-internal)
(define-key cvs:map "\C-cvi" 'cvs-diff)
(define-key cvs:map "\C-cv\C-d" 'cvs-ediff)
(define-key cvs:map "\C-cvv" 'cvs-version-other-window)
(define-key cvs:map "\C-cvm" 'cvs-mark)
(define-key cvs:map "\C-cvc" 'cvs-commit)
(define-key cvs:map "\C-cvC" 'cvs-commit-file)
(define-key cvs:map "\C-cvl" 'cvs-list)
(define-key cvs:map "\C-cvf" 'cvs-flush)
(define-key cvs:map "\C-cvu" 'cvs-update-file)
(define-key cvs:map "\C-cvr" 'cvs-revert)
(define-key cvs:map "\C-cvb" 'cvs-submit-report)
(define-key cvs:map "\C-cvh" 'cvs-history)
(define-key cvs:map "\C-cva" 'cvs-annotate)
(define-key cvs:commit-map "\C-c\C-c" 'cvs-do-commit)
(define-key cvs:commit-map "\C-c\C-d" 'cvs-bury-buffer)
(define-key cvs:commit-map "\C-cvl" 'cvs-list)
(easy-menu-define
 cvs:menu
 cvs:map
 "CVS minor mode keymap"
 '("CVS"
    ["Update File" cvs-update-file t]
    ["Commit File" cvs-commit-file t]
    ["(Un)Edit" cvs-edit t]
    ["Log" cvs-log t]
    ["Annotate" cvs-annotate t]
    ["History" cvs-history t]
    ["File Status" cvs-file-status t]
    ["Editors" cvs-editors t]
    ["Watchers" cvs-watchers t]
    ["EDiff" cvs-ediff-internal t]
    ["Diff" cvs-diff t]
    "-----------" 
    ["Module Status" cvs-status-process t]
    ["Update Module" cvs-update-directory t]
    "-----------" 
    ("Marking Files" 
     ["(Un)Mark current buffer" cvs-mark t]
     ["Show List" cvs-list t]
     ["Flush List" cvs-flush t]
     )
    ("Action on Marked Files"
     ["Commit" cvs-commit t]
     ["Show Status" cvs-marked-status t]
     )
    ("Other Commands" 
     ["EDiff two revs" cvs-ediff t]
     ["Restore version" cvs-revert t]
     ["Merge backup" cvs-merge-backup t]
     ["Merge branch" cvs-merge-branch t]
     ["Retrieve version" cvs-version-other-window t]
     ["Change description" cvs-description t]
     ["Change log message" cvs-change-log t]
     ["Who" cvs-who t])
    "--------" 
    ["Send bug/comment report" cvs-submit-report t]))

(defconst cvs:entry
  (list 'cvs-minor-mode (cons "" '(" CVS:" cvs:current-revision)))
  "Entry to display CVS revision number in mode line")

(or (assq 'cvs-minor-mode minor-mode-alist)
    (not cvs-minor-mode-in-modeline)
    (setq minor-mode-alist (cons cvs:entry minor-mode-alist)))

(or (assq 'cvs-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'cvs-minor-mode cvs:map)
				     minor-mode-map-alist)))

;;=============================================================================
(defconst cvs:mark-entry
  (list 'cvs:mark " marked")
  "Entry to display CVS revision number in mode line")

(or (assq 'cvs:mark minor-mode-alist)
    (setq minor-mode-alist (cons cvs:mark-entry minor-mode-alist)))

;;=============================================================================
(defvar cvs:remote-regexp "^/[^/:]*[^/:]:"
  "regexp to test if a file is accessed from ftp")

(defun is-under-cvs ()
  "Test if the file in the current buffer is under CVS.
If so, set the variables `cvs:current-revision' and `cvs-minor-mode'."
  (interactive)
  (let ((result nil)
	current-revision)
    (if (and buffer-file-name
	     (not (string-match cvs:remote-regexp buffer-file-name))) ; reject remote files
	(progn
	  (save-excursion
	    (let* ((filename (file-truename buffer-file-name))
		   (buffer (current-buffer))
		   (entries-filename (concat (file-name-directory
					      filename)
					     "CVS/Entries")))
	      (if (file-exists-p entries-filename)
		  (progn
		    (set-buffer (cvs:find-file-noselect entries-filename))
		    (goto-char 1)
		    (if (re-search-forward (concat "^/" 
						   (file-name-nondirectory filename)
						   "/\\([^/]*\\)/")
					   nil t)
			(progn
			  (setq result t)
			  (setq current-revision (buffer-substring
						  (match-beginning 1)
						  (match-end 1)))
			  (if (looking-at ".*/T\\([^/\n][^/\n]*\\)[ \t]*\n")
			      (let ( (tag (buffer-substring (match-beginning 1)
							    (match-end 1))) )
				(setq current-revision
				      (format "%s-%s" tag current-revision))
				))
			  )
		      )
		    ))))
		))
    (if result
	(progn
	  (setq cvs:current-revision current-revision)
	  (cvs-minor-mode)))
    result))


(defun cvs:get-numeric-revision (revision)
  (progn
    (if (string-match "-\\([.0-9][.0-9]*\\)$" revision)
	(setq revision (match-string 1 revision)))
    revision
    ))


(defun cvs:get-tag-revision (revision)
  (let ( (case-fold-search t) )
    (if (string-match "^\\([a-z].*\\)-[.0-9][.0-9]*$" revision)
	(setq revision (match-string 1 revision)))
    revision
    ))

;;=============================================================================
(defun cvs-ediff (old-rev new-rev)
  "Run Ediff between versions `old-rev' and `new-rev' of the current buffer."
  (interactive "sFirst version to visit (default is latest version): 
sSecond version to visit (default is latest version): ") 
  (let ((old-vers (cvs-version-other-window old-rev)))
    (other-window 1)
    (cvs-version-other-window new-rev)
    ;; current-buffer is now supposed to contain the old version
    ;; in another window
    ;; We delete the temp file that was created by vc.el for the old
    ;; version
    (ediff-buffers old-vers (current-buffer)
		   (list (` (lambda () (delete-file (, (buffer-file-name))))))
		   'ediff-revision)
    ))

;;=============================================================================
(defun cvs-ediff-internal (rev)
  "Run Ediff on version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (interactive "sVersion to visit (default is latest version): ")
  (let ((newvers (current-buffer)))
    (cvs-version-other-window rev)
    ;; current-buffer is now supposed to contain the old version
    ;; in another window
    ;; We delete the temp file that was created by vc.el for the old
    ;; version
    (ediff-buffers (current-buffer) newvers
		   (list (` (lambda () (delete-file (, (buffer-file-name))))))
		   'ediff-revision)
    ))

;;=============================================================================
(defun cvs-version-other-window (rev)
  "Visit version REV of the current buffer in another window.
If the current buffer is named `F', the version is named `F.~REV~'.
If `F.~REV~' already exists, it is used instead of being re-created."
  (interactive "sVersion to visit (default is latest version): ")
  (if buffer-file-name
      (let* ((version (if (string-equal rev "")
			  "lst"
			rev))
	     (filename (if cvs-temp-dir
			   (concat (file-name-as-directory cvs-temp-dir)
				   (file-name-nondirectory buffer-file-name)
				   ".~" version "~")
			 (concat buffer-file-name ".~" version "~"))))
	(if (or (file-exists-p filename)
		(cvs:checkout (file-name-nondirectory buffer-file-name) rev
			      filename))
	    (find-file-other-window filename)))))

;;=============================================================================
(defun cvs:checkout (filename rev output-name)
  "Checkout filename with revision `rev' to `output-name'."
  (let ((command (if (string= rev "")
		     (format "%s %s %s -Q update -p %s > %s" cvs-command
			     (if cvs-root
				 (format "-d %s" cvs-root) "")
			     (if cvs-no-log-option
				 cvs-no-log-option "")
			     filename output-name)
		     (format "%s %s %s -Q update -r %s -p %s > %s" cvs-command
			     (if cvs-root
				 (format "-d %s" cvs-root) "")
			     (if cvs-no-log-option
				 cvs-no-log-option "")
			     rev filename output-name))))
    (message "Retrieving version with command : %s" command)
    (if (/= (call-process cvs-shell-command nil nil t cvs-shell-command-option command)
	    0)
	(error "Error while retrieving %s version of %s into %s"
	       (if (string= "" rev) "last" rev) filename output-name)
      output-name)))

;;=============================================================================
(defun cvs-add (msg &optional file)
  "Add the current file into the CVS system"
  (interactive "sEnter description : ")
  (if (not file)
      (setq file (buffer-file-name)))  
  (let ((command (format "%s %s add -m \"%s\" %s" cvs-command
			 (if cvs-root
			     (format "-d %s" cvs-root) "")
			 msg
			 (file-name-nondirectory file)))
	(filename (file-name-nondirectory file))
	(buf (get-buffer-create "*CVS Add output*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer))
    (if (= (call-process cvs-shell-command nil buf t cvs-shell-command-option command)
	   0)
	(progn
	  (is-under-cvs)
	  (run-hooks 'cvs-add-hooks)
	  (message "File added to CVS -- use 'commit' to make permanent")
	  )
      (cvs:display-temp-buffer buf "add")
      (error "Error while registering %s into CVS" filename))))

;;=============================================================================
;; Change CVS description or log message
;;=============================================================================
(defvar cvs:temp-filename nil
  "Internal variable used by cvs-description and cvs-change-log")
(make-variable-buffer-local 'cvs:temp-filename)
(put 'cvs:temp-filename 'permanent-local t)

(defvar cvs:temp-revision nil
  "Internal variable used by cvs-description and cvs-change-log")
(make-variable-buffer-local 'cvs:temp-revision)
(put 'cvs:temp-revision 'permanent-local t)

(defun cvs:get-description (file)
  (cvs:call-command cvs-command "*CVS temp*" "log"
		    (list "log" "-r0" (file-name-nondirectory file)))
  (save-excursion
    (set-buffer "*CVS temp*")
    (goto-char (point-min))
    (if (not (search-forward-regexp "^description:" nil t))
	(error "Invalid log output for file %s" rev file)
      (let* ((min (progn
		    (next-line 1) (beginning-of-line) (point)))
	     (max (progn
		    (search-forward "======================================")
		    (beginning-of-line) (point)))
	     (str (buffer-substring min max)))
	(kill-buffer "*CVS temp*")
	str))))

(defun cvs-description (&optional file)
  "Change the description of the current file into the CVS system"
  (interactive)
  (if (not file)
      (setq file (buffer-file-name)))
  (let ((dir default-directory)
	(str  (cvs:get-description file)))
    (switch-to-buffer-other-window (get-buffer-create "*CVS Description*"))
    (setq cvs:temp-filename file)
    (setq default-directory dir)
    (erase-buffer)
    (insert str)
    (insert "CVS: ----------------------------------------------------------------------\n")
    (insert "CVS: Enter description.  Lines beginning with `CVS: ' are removed automatically\n")
    (insert "CVS: changing description of " file "\n")
    (insert "CVS: ----------------------------------------------------------------------\n")
    (local-set-key "\C-c\C-c" 'cvs:do-description)
    (goto-char 0)
    (message "Type C-c C-c when done.")
    ))

(defun cvs:do-description ()
  "Change the description of the current file into the CVS system"
  (interactive)
  (let ((msg nil)
	(file-name cvs:temp-filename))
    (goto-char 0)
    (flush-lines "^CVS: .*$")
    (setq msg (buffer-string))
    (cvs-bury-buffer)
    (let ((command (format "%s %s admin -t-\"%s\" %s" cvs-command
			   (if cvs-root
			       (format "-d %s" cvs-root) "")
			   msg
			   (file-name-nondirectory file-name)))
	  (filename (file-name-nondirectory file-name))
	  (buf (get-buffer-create "*CVS Admin output*")) )
      (save-excursion
	(set-buffer buf)
	(setq buffer-read-only nil)
	(erase-buffer))
      (if (not (= (call-process cvs-shell-command nil buf t cvs-shell-command-option command)
		  0))
	  (progn
	    (cvs:display-temp-buffer buf "admin")
	    (error "Error while changing description of %s into CVS" filename))))))

;;=============================================================================
(defun cvs:get-change-log (file rev)
  (cvs:call-command cvs-command "*CVS temp*" "log"
		    (list "log" (concat "-r" rev) (file-name-nondirectory file)))
  (save-excursion
    (set-buffer "*CVS temp*")
    (goto-char (point-min))
    (if (not (search-forward-regexp "^date: " nil t))
	(error "Revision %s doesn't exist for file %s" rev file)
      (let* ((min (progn
		    (next-line 1) (beginning-of-line) (point)))
	     (max (progn
		    (search-forward "======================================")
		    (beginning-of-line) (point)))
	     (str (buffer-substring min max)))
	(kill-buffer "*CVS temp*")
	str))))

(defun cvs-change-log (rev &optional file)
  "Change the description of the current file into the CVS system"
  (interactive "sVersion (default is current version): ")
  (if (not file)
      (setq file (buffer-file-name)))
  (if (string= "" rev) 
      (setq rev cvs:current-revision))
  (let ((dir default-directory)
	(str (cvs:get-change-log file rev)))
    (switch-to-buffer-other-window (get-buffer-create "*CVS Description*"))
    (setq cvs:temp-filename file)
    (setq cvs:temp-revision rev)
    (setq default-directory dir)
    (erase-buffer)
    (insert str)
    (insert "CVS: ----------------------------------------------------------------------\n")
    (insert "CVS: Enter Log.  Lines beginning with `CVS: ' are removed automatically\n")
    (insert "CVS: changing log message of " file " for revision " rev "\n")
    (insert "CVS: ----------------------------------------------------------------------\n")
    (local-set-key "\C-c\C-c" 'cvs:do-change-log)
    (goto-char 0)
    (message "Type C-c C-c when done.")
    ))

(defun cvs:do-change-log ()
  "Change the description of the current file into the CVS system"
  (interactive)
  (let ((msg nil)
	(file-name cvs:temp-filename)
	(rev cvs:temp-revision))
    (goto-char 0)
    (flush-lines "^CVS: .*$")
    (setq msg (buffer-string))
    (cvs-bury-buffer)
    (let ((command (format "%s %s admin -m%s:\"%s\" %s" cvs-command
			   (if cvs-root
			       (format "-d %s" cvs-root) "")
			   rev
			   msg
			   (file-name-nondirectory file-name)))
	  (filename (file-name-nondirectory file-name))
	  (buf (get-buffer-create "*CVS Admin output*")) )
      (save-excursion
	(set-buffer buf)
	(setq buffer-read-only nil)
	(erase-buffer))
      (if (not (= (call-process cvs-shell-command nil buf t cvs-shell-command-option command)
		  0))
	  (progn
	    (cvs:display-temp-buffer buf "admin")
	    (error "Error while changing log message of %s into CVS" filename))))))

;;=============================================================================
(defun cvs-edit (&optional file)
  "Run the cvs edit or unedit command on the file.
If the file is read-only, runs the edit command else runs the unedit command."
  (interactive)
  (if (not file)
      (setq file (buffer-file-name)))
  (let* ((revision cvs:current-revision)
	 (filename (file-truename file))
	 (default-directory (file-name-directory (expand-file-name filename)))
	 (comm (if (file-writable-p file) "unedit" "edit"))
	 (command-args (if cvs-root
			   (list "-d" cvs-root comm
				 (file-name-nondirectory filename))
			 (list comm (file-name-nondirectory
				     filename)) ))
	(buf (get-buffer-create " *CVS Edit output*")) )
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer))
    (if (= (apply 'call-process cvs-command nil buf t command-args) 0)
	(let ((file-buf (get-file-buffer file)))
	  (if file-buf
	      (save-excursion
		(set-buffer file-buf)
		(revert-buffer t t))))
      (cvs:display-temp-buffer buf "edit")
      (error "Error while editing %s"
	     (file-name-nondirectory filename)))))

;;=============================================================================
(defun cvs-update-file (&optional file)
  "Update the current file from the repository"
  (interactive)
  (if (not file)
      (setq file (buffer-file-name)))
  (save-buffer)
  (let* ((revision (cvs:get-numeric-revision cvs:current-revision))
	 (filename (file-truename file))
	 (default-directory (file-name-directory (expand-file-name filename)))
	 (command-args (if cvs-root
			   (list "-d" cvs-root "update"
				(file-name-nondirectory filename))
			 (list "update" (file-name-nondirectory
					 filename))))
	 (buf (get-buffer-create "*CVS Update output*")))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer))
    (if (= (apply 'call-process cvs-command nil buf t command-args) 0)
	(progn
	  (cvs:merge-or-revert buf filename revision)
	  (if (not (string-equal file filename))
	      (let ((file-buf (get-file-buffer file)))
		(if file-buf
		    (save-excursion
		      (set-buffer file-buf)
		      (revert-buffer t t)))))
	  (message (format "Updated \"%s\"" file))
	  )
      (cvs:display-temp-buffer buf "update")
      (error "Error while updating %s"
	     (file-name-nondirectory filename)))))

;;=============================================================================
(defun cvs-update-directory ()
  "Update the current directory from the repository"
  (interactive)
  (let ((command-args (if cvs-root
			  (list "-d" cvs-root "update")
			(list "update")))
	(buf (get-buffer-create "*CVS Update output*")))
    (and (or (not (memq t (mapcar #'(lambda (buf) (and (buffer-file-name buf)
						       (buffer-modified-p buf)))
				  (buffer-list))))
	     (yes-or-no-p "Modified buffers exist; update anyway? "))
	 (progn
	   (save-excursion
	     (set-buffer buf)
	     (setq buffer-read-only nil)
	     (erase-buffer))
	   (pop-to-buffer buf)
	   (if (= (apply 'call-process cvs-command nil buf t command-args) 0)
	       (progn
		 (goto-char (point-max))
		 (insert "\ncvs update finished.\n")
		 (cvs:display-temp-buffer buf "update")
		 )
	     (progn
	       (goto-char (point-max))
	       (insert "\ncvs update finished with errors.\n")
	       (cvs:display-temp-buffer buf "update")
	       (error "Error while updating %s" default-directory)))
	   ))
    ))

;;=============================================================================
(defun cvs:merge-convert-symbolic-to-numeric (file name)
  (let ( (tmpbuf " *CVSrev*")
	 (cmd-format-args (format "%s %s update -p -r%%s %%s | head > /dev/null"
				  cvs-command (if cvs-root
						  (format "-d %s" cvs-root)
						"")))
	)
    (if (string-match "^[a-z]" name)
	(save-excursion
	  (setq tmpbuf (get-buffer-create tmpbuf))
	  (message "Converting symbolic name to numeric (please wait) ...")
	  (buffer-disable-undo tmpbuf)
	  (erase-buffer tmpbuf)
	  (call-process cvs-shell-command nil tmpbuf nil
                        cvs-shell-command-option
                        (format cmd-format-args name file))
	  (set-buffer tmpbuf)
	  (goto-char (point-min))
	  (if (not (re-search-forward "^VERS:[ \t]*\\([.0-9][.[0-9]*\\)[ \t]*$"
				      nil t))
	      (progn
		(pop-to-buffer tmpbuf)
		(error (format "Unable to convert symbolic name \"%s\"!" name))
		))
	  (setq name (buffer-substring (match-beginning 1) (match-end 1)))
	  (kill-buffer tmpbuf)
	  ))
    name
    ))

(defun cvs:merge-query-args ()
  (let ((case-fold-search t)
	from to part1 part2
	(filename (file-name-nondirectory buffer-file-name))
	)
    (while (and (setq to (read-from-minibuffer "Branch revision to merge? "))
		(or (while (string-match "[ \t][ \t]*" to)
		      (replace-match "" nil nil to))
		    t)
		(string-equal to ""))
      )
    (setq to (cvs:merge-convert-symbolic-to-numeric filename to))
    (if (not (string-match "^\\([.0-9][.0-9]*\\)\\.\\([0-9][0-9]*\\)$" to))
	(error (format "\"%s\" is not a valid revision!") to))
    (setq part1 (match-string 1 to)
	  part2 (match-string 2 to))
    (setq part2 (1- (string-to-number part2)))
    (if (> part2 0)
	(progn
	  (setq from (format "%s.%s" part1 part2))
	  )
      (progn
	(if (not (string-match "^\\([.0-9][.0-9]*\\)\\.[0-9][0-9]*\\.[0-9][0-9]*$" to))
	    (error (format "Unable to find the previous revision for revision %s!"
			   to)))
	(setq from (match-string 1 to))
	))
    (while (and (setq from (read-from-minibuffer "Merge back to revision? " from))
		(or (while (string-match "[ \t][ \t]*" from)
		      (replace-match "" nil nil from))
		    t)
		(string-equal from ""))
      )
    (setq from (cvs:merge-convert-symbolic-to-numeric filename from))
    (if (not (string-match "^\\([.0-9][.0-9]*\\)\\.\\([0-9][0-9]*\\)$" from))
	(error (format "\"%s\" is not a valid revision!") from))
    (message (format "From %s to %s" from to))
    (list from to)
    ))


(defun cvs-merge-branch (branch-from branch-to)
  "Merge a branch into the current source file."
  (interactive (cvs:merge-query-args))
  (save-buffer)
  (let ( status
	 (command-args (append (if cvs-root
				   (list "-d" cvs-root "update")
				 (list "update"))
			       (list (format "-j%s" branch-from)
				     (format "-j%s" branch-to))
			       (list (file-name-nondirectory buffer-file-name))))
	 (filename (file-name-nondirectory buffer-file-name))
	 (buf (get-buffer-create "*CVSmerge*"))
	 )
    (message (format "Merging branch %s to %s to %s ..."
		     branch-from branch-to filename))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (buffer-disable-undo buf)
      (erase-buffer buf)
      (display-buffer buf)
      (sit-for 0)
      )
    (setq status (apply 'call-process cvs-command nil buf t command-args))
    (message "Reverting buffer ...")
    (revert-buffer t t)
    (if (not (= status 0))
	(progn
	  (cvs:display-temp-buffer buf "branch merge")
	  (error "Error while merging branch %s to %s to %s"
		 branch-from branch-to filename)
	  )
      (progn
	(message (format "Merged branch %s to %s to %s"
			 branch-from branch-to filename))
	))
    ))


;;=============================================================================
(defun cvs:merge-or-revert (buf file revision)
  "Revert the buffer or run `ediff-merge-files-with-ancestor' to merge the conflicts."
  (let ((conflict (save-excursion
		    (set-buffer buf)
		    (goto-char (point-min))
		    (search-forward "conflicts" nil t)))
	(filename (file-name-nondirectory file)))
    (if conflict
	(message "conflict detected while updating %s" filename))
    (if (or (not conflict)
	    cvs-never-use-emerge
	    (and (or (fboundp 'ediff-merge-files-with-ancestor)
		     (fboundp 'emerge-files-with-ancestor))
		 (not (y-or-n-p "Conflicts detected do you want to run emerge ? ")))
	    )
	(let ((file-buf (get-file-buffer filename)))
	  (if file-buf
	      (save-excursion
		(set-buffer file-buf)
		(revert-buffer t t))))
      (rename-file file (concat filename ".old") t)
      (rename-file (concat (file-name-directory filename)
			   cvs-save-prefix
			   (file-name-nondirectory filename) "." revision)
		   file t)
      (let ((ancestor (concat filename ".~" revision "~"))
	    (new (concat filename ".~lst~")))
	(cvs:checkout filename revision ancestor)
	(cvs:checkout filename "" new)
	(if (fboundp 'ediff-merge-files-with-ancestor)
	    (progn
	      (ediff-merge-files-with-ancestor new filename ancestor
					       (list (` (lambda () 
							  (delete-file (, ancestor))
							  (delete-file (, new))))))
;; 	      (delete-other-windows)
;; 	      (write-file filename)
;; 	      (revert-buffer t t)
	      )
	  (emerge-files-with-ancestor nil new filename ancestor filename
				     (list (` (lambda () 
						(delete-file (, ancestor))
						(delete-file (, new))))))
	  ))
      )))

;;=============================================================================
;; ediff cleanup
;;=============================================================================
(add-hook 'ediff-cleanup-hook 'cvs:ediff-cleanup)

(defun cvs:buffer (buf)
  (save-excursion
    (set-buffer buf)
    (if cvs-minor-mode
	buf) ) )

(defun cvs:ediff-cleanup (&optional ask)
  "In merge jobs, kill buffers A, B, and, ancestor. Renames buffer C.
The buffer C is saved under the filename registered under CVS."
  (cond ((and (not cvs-ediff-merge-no-cleanup)
	      ediff-merge-job)
	 (let ((buf (or (cvs:buffer ediff-buffer-A)
			(cvs:buffer ediff-buffer-B) )) )
	   (cond (buf
		  (save-excursion
		    (let ((filename (buffer-file-name buf)))
		      (kill-buffer buf)
		      (set-buffer ediff-buffer-C)
		      (write-file filename nil)
		      (revert-buffer t t) )
		  ) ))
	   )
	 ;; clean all the buffers
	 (ediff-janitor ask)
	 )) )

;;=============================================================================
(defun cvs-merge-backup (&optional file)
  (interactive)
  (if (not file)
      (setq file (buffer-file-name)))
  (let* ((filename (file-truename file))
	 (files (directory-files (file-name-directory filename) nil
				 (concat "\\.#" (file-name-nondirectory filename) "\\..*")))
	 )
    (if (not files)
	(error "No backup file to merge!"))
    (if (> 1 (length files))
	(error "too much backup files (%d)" (length files)))
    (let ((version (save-excursion
		     (let ((buf (set-buffer (generate-new-buffer "*CVS work*")))
			   result)
		       (insert (car files))
		       (goto-char 1)
		       (re-search-forward (concat "\\.#" (file-name-nondirectory filename) "\\.\\(.*\\)$"))
		       (setq result (buffer-substring (match-beginning 1) (match-end 1)))
		       (kill-buffer buf)
		       result))))
      (if (or (fboundp 'ediff-merge-files-with-ancestor)
	      (fboundp 'emerge-files-with-ancestor))
	    (let ((ancestor (concat filename ".~" version "~"))
		  (new (concat filename ".~lst~")))
	      (cvs:checkout filename version ancestor)
	      (cvs:checkout filename "" new)
	      (if (fboundp 'ediff-merge-files-with-ancestor)
		  (progn
		    (ediff-merge-files-with-ancestor new (car files) ancestor
						     (list (` (lambda () 
								(delete-file (, ancestor))
								(delete-file (, new))))))
		    ;; 	      (delete-other-windows)
		    ;; 	      (write-file filename)
		    ;; 	      (revert-buffer t t)
		    )
		(emerge-files-with-ancestor nil new (car files) ancestor filename
					    (list (` (lambda () 
						       (delete-file (, ancestor))
						       (delete-file (, new))))))
		))
	    )))
    )

;;=============================================================================
(defun cvs:repository(&optional filename)
  "Retrieve repository name of current CVS file"
  (let ((result nil)
	(filename (file-truename (or filename buffer-file-name))))
    (if filename
	(let ((repository-filename (concat (file-name-directory
					    filename)
					   "CVS/Repository")))
	  (save-excursion
	    (if (file-exists-p repository-filename)
		(progn
		  (set-buffer (cvs:find-file-noselect repository-filename))
		  (goto-char 1)
		  (end-of-line)
		  (setq result (buffer-substring 1 (point)))
		  )))))
    result))
	 

;;=============================================================================
(defun cvs-log (&optional file)
  "Show the CVS log for the current buffer's file."
  (interactive)
  (if (not file)
      (setq file (buffer-file-name)))
  (cvs:call-command cvs-command "*CVS Log*" "log"
		    (list "log" (file-name-nondirectory file))))

;;=============================================================================
(defun cvs-file-status (&optional file)
  "Show the CVS status information for the current buffer's file."
  (interactive)
  (if (not file)
      (setq file (buffer-file-name)))
  (cvs:call-command cvs-command "*CVS Status*" "status"
		    (list "status" (file-name-nondirectory file)
			  )))

;;=============================================================================
(defun cvs-history (&optional file)
  "Show the CVS history information for the current buffer's file."
  (interactive)
  (if (not file)
      (setq file (buffer-file-name)))
  (cvs:call-command cvs-command "*CVS History*" "history"
		    (list "history" (file-name-nondirectory file)
			  )))

;;=============================================================================
(defun cvs-annotate (&optional file)
  "Show the CVS annotate information for the current buffer's file."
  (interactive)
  (if (not file)
      (setq file (buffer-file-name)))
  (let ((buf (cvs:call-command cvs-command "*CVS annotate*" "annotate"
			       (list "annotate" (file-name-nondirectory
						 file ) ) ))
	(mode major-mode))
    (if (and buf mode)
	(save-excursion
	  (set-buffer buf)
	  (setq major-mode mode)
	  ))))

;;=============================================================================
(defun cvs-editors (&optional file)
  "Show the CVS editors information for the current buffer's file."
  (interactive)
  (if (not file)
      (setq file (buffer-file-name)))
  (cvs:call-command cvs-command "*CVS Editors*" "editors"
		    (list "editors" (file-name-nondirectory file)
			  )))

;;=============================================================================
(defun cvs-watchers (&optional file)
  "Show the CVS watchers information for the current buffer's file."
  (interactive)
  (if (not file)
      (setq file (buffer-file-name)))
  (cvs:call-command cvs-command "*CVS Watchers*" "watchers"
		    (list "watchers" (file-name-nondirectory file)
			  )))

;;=============================================================================
(defun cvs-who (start end &optional file)
  "Who was responsible for the CVS-controlled code in the region?"
  (interactive "r")
  (if (not file)
      (setq file (buffer-file-name)))
  (save-restriction
    (widen)
    (setq start (count-lines 1 start)
          end   (count-lines 1 end))
    (let ((fil  (file-name-nondirectory 
		 (file-name-nondirectory file)))
          (all  (count-lines 1 (point-max))))
      (cvs:call-command cvsann-command "*CVS Who*" "who" (list fil))
      (set-buffer "*CVS Who*")
      (let ((buffer-read-only nil))
	(erase-buffer)
	(cvs:call-command cvsann-command "*CVS Who*" "who" (list fil))
	(setq buffer-read-only nil)
	(if (< (count-lines 1 (point-max)) all)
	    nil
	  (goto-char 1)
	  (forward-line end)
	  (delete-region (point) (point-max))
	  (goto-char 1)
	  (forward-line start)
	  (delete-region 1 (point)))))))

;;=============================================================================
(defun cvs-diff (rev &optional file)
  "Run cvs diff with version REV of the current buffer."
  (interactive "sVersion to visit (default is latest version): ")
  (if (not file)
      (setq file (buffer-file-name)))
  (if (string= "" rev) 
      (setq rev "HEAD"))
  (cvs:call-command cvs-command "*CVS Diff*" "diff"
		    (append (list "diff")
			    cvs-diff-options
			    (list "-r" rev (file-name-nondirectory
					    file)))))

;;=============================================================================
(defun cvs-revert (rev &optional file)
  "Revert current file from a version from repository"
;  (interactive "sVersion to revert from (default is latest version): ")
  (interactive (list (read-from-minibuffer
		      "Version to revert from: "
		      (cvs:get-tag-revision cvs:current-revision))
		     nil))
  (if (not file)
      (setq file (buffer-file-name)))
  (save-buffer)
  (if (yes-or-no-p (concat "All your changes on " (file-name-nondirectory
						   file)
			   " will be lost! Do you want to continue ? "))
      (progn
	(rename-file file (concat file ".old") t)
	(let (;(revision cvs:current-revision)
	      (filename file)
	      (command-args (append (if cvs-root
					(list "-d" cvs-root "update")
				      (list "update"))
				    (cond
				     ( (or (string= rev "") (string= rev "-A"))
				       '("-A"))
				     (t 
				      (list "-r" rev)))
				    (list (file-name-nondirectory file))))
	      (buf (get-buffer-create "*CVS Revert output*")))
	  (message "Reverting buffer ...")
	  (save-excursion
	    (set-buffer buf)
	    (setq buffer-read-only nil)
	    (erase-buffer))
	  (let ((result (apply 'call-process cvs-command nil buf t command-args)))
	    (revert-buffer t t)
	    (cond ((not (= result 0))
		   (cvs:display-temp-buffer buf "revert")
		   (error "Error while reverting %s"
			  (file-name-nondirectory file)))
		  (t
		   (message (format "Reverted \"%s\"" file)))
		  )
	    )))))

;;=============================================================================
(defun cvs-mark (&optional file)
  "(Un)Mark the current file to be committed in the commit command (toggle)."
  (interactive)
  (if (not file)
      (setq file (buffer-file-name)))
  (if (not (member file cvs:marked-list))
      (progn
	(setq cvs:marked-list (cons file cvs:marked-list))
	(setq cvs:mark t))
    (setq cvs:marked-list (delete file cvs:marked-list))
    (setq cvs:mark nil))
  (run-hooks 'cvs-mark-hooks)
  (force-mode-line-update))

;;=============================================================================
(defun cvs-flush-file (file)
  "Flush a single file."
  (if (get-file-buffer file)
      (save-excursion
	(set-buffer (get-file-buffer file))
	(setq cvs:mark nil)))
  )

(defun cvs-flush ()
  "Flush the list of files to be committed."
  (interactive)
  (mapcar 'cvs-flush-file cvs:commit-list)
  (mapcar 'cvs-flush-file cvs:marked-list)
  (setq cvs:marked-list nil)
  (setq cvs:commit-list nil)
  (message "Flushed file list")
  (force-mode-line-update t))

;;=============================================================================
(defun cvs-commit (&optional l)
  "Setup a buffer to enter comment associated with the commit process." 
  (interactive)
  (if (not l)
      (setq l cvs:marked-list))
  (setq cvs:commit-list (if (null l)
			    (list buffer-file-name)
			  l))
  (let ((dir default-directory))
    (switch-to-buffer-other-window (get-buffer-create "*CVS Commit*"))
    (setq default-directory dir)
    (erase-buffer)
    (insert "\n")
    (insert "CVS: ----------------------------------------------------------------------\n")
    (insert "CVS: Enter Log.  Lines beginning with `CVS: ' are removed automatically\n")
    (insert "CVS: committing files:\n")
    (mapcar (function (lambda(c)
			(insert "CVS: " c "\n")))
		cvs:commit-list)
    (insert "CVS: \n")
    (insert "CVS: Type C-c C-c when done or C-c C-d to abort.\n")
    (insert "CVS: ----------------------------------------------------------------------\n")

    (use-local-map cvs:commit-map)
    (goto-char 0)
    (run-hooks 'cvs-commit-hooks)
    (message "Type C-c C-c when done or C-c C-d to abort.")
    ))

;;=============================================================================
(defun cvs-do-commit ()
  "Commit the list of files cvs:marked-list"
  (interactive)
  (cvs:save)
  (goto-char 0)
  (flush-lines "^CVS: .*$")
  (run-hooks 'cvs-before-commit-hooks)
  (let* ((filename (make-temp-name (concat (or (and cvs-temp-dir
						    (file-name-as-directory cvs-temp-dir))
					       "/tmp/")
					   "cvs")))
	 (command-list (if cvs-root
			   (list "-d" cvs-root "commit" cvs-file-option filename)
			 (list "commit" cvs-file-option filename))))
    (write-region (point-min) (point-max) filename)
    (cvs-bury-buffer)
    (let ((buf (set-buffer (get-buffer-create "*CVS Commit output*"))))
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (mapcar
       (lambda(elt)
	 (setq default-directory (car elt))
	 (message "Committing in %s..." (car elt))
	 (if (/= (apply 'call-process cvs-command nil t t
			(append command-list (cdr elt)))
		 0)
	     (progn
	       (cvs:display-temp-buffer buf "commit")
	       (error "Error while committing %S in %s" (cdr elt) (car elt))
	       ))
	 (message "Committing in %s...done" (car elt)) )
       (cvs:files-to-alist cvs:commit-list))
      (insert "------------------------------------------------------------------------------\n")
      (cvs:display-temp-buffer buf "commit")
      (delete-file filename)
      (save-excursion
	(mapcar
	 (lambda(name)
	   (let ((buf (get-file-buffer name)))
	     (if buf
		 (progn
		   (set-buffer buf)
		   (revert-buffer t t)))))
	 cvs:commit-list))))
  (cvs-flush))

;;=============================================================================
(defun cvs:files-to-alist(l)
  "Sort a list of files to an alist containing the directory as the key and
the list of file names without directory as the value"
  (let ((alist nil)
	(elt nil)
	(file nil))
    (while l
      (setq file (file-truename (car l)))
      (setq elt (assoc (file-name-directory file) alist))
      (if elt
	  (setcdr elt
		   (nconc (cdr elt) (list (file-name-nondirectory file))))
	(setq alist (cons 
		     (cons (file-name-directory file)
			   (list (file-name-nondirectory file)))
		     alist)))
      (setq l (cdr l)))
    alist))

;;=============================================================================
(defun cvs-list (&optional dir)
  "List the files to commit cvs:marked-list in a buffer"
  (interactive)
  (if (null dir)
      (setq dir default-directory))
  (set-buffer (get-buffer-create "*CVS List of marked files to commit*"))
  (setq buffer-read-only nil)
  (setq default-directory dir)
  (erase-buffer)
  (goto-char (point-min))
  (insert "Marked file(s):\n\n")
  (if cvs:marked-list
      (mapcar (function (lambda(c)
			  (insert "  ")(insert c)(insert "\n")))
	      cvs:marked-list)
    (insert "  ***** No marked files *****\n"))
  (set-buffer-modified-p nil)
  (cvs:display-temp-buffer (current-buffer) "list"))

;;=============================================================================
;; extract from files.el and modified not to ask question to the user if the
;; file needs to be reloaded.
;;=============================================================================
(defun cvs:find-file-noselect (filename)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller."
  (setq filename
	(abbreviate-file-name
	 (expand-file-name filename)))
  (if (file-directory-p filename)
      (error "%s is a directory." filename)
    (let* ((buf (get-file-buffer filename))
	   (truename (abbreviate-file-name (file-truename filename)))
	   (number (nthcdr 10 (file-attributes truename)))
	   ;; Find any buffer for a file which has same truename.
	   (other (and (not buf) (get-file-buffer filename)))
	   error)
      ;; Let user know if there is a buffer with the same truename.
      (if other
	  (progn
	    ;; Optionally also find that buffer.
	    (if (or find-file-existing-other-name find-file-visit-truename)
		(setq buf other))))
      (if buf
	  (or (verify-visited-file-modtime buf)
	      (cond ((not (file-exists-p filename))
		     (error "File %s no longer exists!" filename))
		    (t
		     (file-name-nondirectory filename)
		     (save-excursion
		       (set-buffer buf)
		       (revert-buffer t t)))))
	(save-excursion
	  (setq buf (create-file-buffer filename))
	  (set-buffer buf)
	  (erase-buffer)
	  (condition-case ()
	      (insert-file-contents filename t)
	    (file-error
	     (setq error t)
	     ;; Run find-file-not-found-hooks until one returns non-nil.
	     (let ((hooks find-file-not-found-hooks))
	       (while (and hooks
			   (not (and (funcall (car hooks))
				     ;; If a hook succeeded, clear error.
				     (progn (setq error nil)
					    ;; Also exit the loop.
					    t))))
		 (setq hooks (cdr hooks))))))
	  ;; Find the file's truename, and maybe use that as visited name.
	  (setq buffer-file-truename truename)
	  (setq buffer-file-number number)
	  ;; On VMS, we may want to remember which directory in a search list
	  ;; the file was found in.
	  (and (eq system-type 'vax-vms)
	       (let (logical)
		 (if (string-match ":" (file-name-directory filename))
		     (setq logical (substring (file-name-directory filename)
					      0 (match-beginning 0))))
		 (not (member logical find-file-not-true-dirname-list)))
	       (setq buffer-file-name buffer-file-truename))
	  (if find-file-visit-truename
	      (setq buffer-file-name
		    (setq filename
			  (expand-file-name buffer-file-truename))))
	  ;; Set buffer's default directory to that of the file.
	  (setq default-directory (file-name-directory filename))
	  ;; Turn off backup files for certain file names.  Since
	  ;; this is a permanent local, the major mode won't eliminate it.
	  (and (not (funcall backup-enable-predicate buffer-file-name))
	       (progn
		 (make-local-variable 'backup-inhibited)
		 (setq backup-inhibited t)))
	  (after-find-file error t)))
      buf)))

;;=============================================================================
;; NB duplicated from misc-fns.el
;;=============================================================================
(defun cvs:call-command (program bufname name &optional args)
  "Call PROGRAM synchronously in a separate process.
Input comes from /dev/null.
Output goes to a buffer named BUFNAME, which is created or emptied first, and
displayed afterward (if non-empty).
Optional third arg is a list of arguments to pass to PROGRAM."
  (let ((dir default-directory))
    (set-buffer (get-buffer-create bufname))
    (setq default-directory dir)
    (setq buffer-read-only nil)
    (erase-buffer)
    (apply 'call-process program nil t nil 
	   (if cvs-root
	       (append (list "-d" cvs-root) args)
	     args))
    (goto-char 0)
    (run-hooks (intern (concat "cvs-" name "-hooks")))
    (cvs:display-temp-buffer (current-buffer) name)))

(defun cvs:display-temp-buffer (buf name)
  "Display buffer setting it read only, unmodified and binding key q to bury-buffer."
  (save-excursion
    (set-buffer buf)
    (cvs-info-mode name)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t))
  (pop-to-buffer buf)
  buf)

(defun cvs-bury-buffer (&optional buf)
  "Bury a buffer even if it is in a dedicated window"
  (interactive)
  (if (null buf)
      (setq buf (current-buffer)))
  (let ((win (get-buffer-window buf)))
    (if (window-dedicated-p win)
	(progn
	  (delete-windows-on buf t)
	  (bury-buffer buf))
      (bury-buffer)
      (other-window -1))))
  
;;=============================================================================
;; major mode stuff to display CVS info buffers
;;=============================================================================
(defvar cvs-info-mode-hooks nil
  "Hooks run when entering `cvs-info-mode'.")

(defvar cvs-info-mode-map nil
  "key map used by `cvs-info-mode'.")

(if cvs-info-mode-map
    ()
  (require 'dired)

   ;; Inherit from dired map
  (if (not (fboundp 'set-keymap-parents))
      ;; FSF way
      (setq cvs-info-mode-map (cons 'keymap dired-mode-map))
    ;; XEmacs way
    (setq cvs-info-mode-map (make-sparse-keymap))
    (set-keymap-parents cvs-info-mode-map dired-mode-map))

  (define-key cvs-info-mode-map "U" 'cvs-status-update)
  (define-key cvs-info-mode-map "C" 'cvs-status-mark-and-commit)
  (define-key cvs-info-mode-map "g" 'cvs-status-process)
  (define-key cvs-info-mode-map "\C-k" 'cvs-status-delete-file)
  (define-key cvs-info-mode-map "q" 'cvs-bury-buffer)
  (define-key cvs-info-mode-map " " 'scroll-up)
  (define-key cvs-info-mode-map "\177" 'scroll-down)
)

(defun cvs-info-mode (name)
  "Major mode to display CVS information buffers.
Special commands:
\\{cvs-info-mode-map}

Turning on `cvs-info-mode' runs the hooks `cvs-info-mode-hooks'."
  (interactive "s")
  (use-local-map cvs-info-mode-map)
  (setq cvs-dired-mode t)
  (setq mode-name (concat "CVS " name))
  (setq major-mode 'cvs-info-mode)
  (run-hooks 'cvs-info-mode-hooks)
)

;;=============================================================================
(defvar cvs:status-buffer "*CVS-Statuslist*"
  "Name of the CVS module status buffer")

(defvar cvs:status-file-keymap
  (let ((m (make-sparse-keymap)))
    (if (fboundp 'set-keymap-name)
	(set-keymap-name m 'cvs:status-file-keymap))
    (if (string-match "XEmacs" emacs-version)
	(progn
	  (define-key m 'button2 'dired-mouse-find-file)
	  (define-key m [return] 'dired-find-file)
	  ))
    m)
  "The keymap used for the highlighted messages in the status buffer.")

(defun cvs-status-get-filename ()
  (let ( (buf (current-buffer))
	 (statbuf (get-buffer cvs:status-buffer)) )
    (if (not (eq buf statbuf))
	(error "The current buffer is not the Module Status buffer!"))
    (cvs:get-filename)
    ))

(defun cvs-status-update ()
  "Update all files that are marked as needing update."
  (interactive)
  (let (filename (statbuf (get-buffer cvs:status-buffer)) (filecount 0))
    (message "Updating files ...")
    (save-excursion
      (if (not statbuf)
	  (error "The CVS Module Status buffer does not exist!"))
      (set-buffer statbuf)
      (goto-char (point-min))
      (while (re-search-forward "^ *[*] *Needs \\(Update\\|Patch\\)" (point-max) t)
	(setq filename (expand-file-name (cvs-status-get-filename)))
	(find-file-noselect filename)
	(cvs-update-file filename)
	(setq filecount (1+ filecount))
	)
      (message (format "Updated %d files" filecount))
      )
    ))

(defun cvs-status-mark-changed (&optional no-display)
  "Mark all Locally changed files in the Module Status buffer"
  (interactive)
  (let (filename (statbuf (get-buffer cvs:status-buffer)) (filecount 0)
		 (oldcount (length cvs:marked-list)))
    (save-excursion
      (if (not statbuf)
	  (error "The CVS Module Status buffer does not exist!"))
      (set-buffer statbuf)
      (goto-char (point-min))
      (while (re-search-forward "^ *[*] *Locally " (point-max) t)
	(setq filename (expand-file-name (cvs:get-filename)))
	(if (not (member filename cvs:marked-list))
	    (save-excursion
	      (let ( (buf (get-file-buffer filename)) )
		(if buf
		    (set-buffer buf))
		(cvs-mark filename)
		(setq filecount (1+ filecount))
		)))
	)
      (if (not no-display)
	  (cvs-list))
      (message (format "Marked %d new files, %d files already marked"
		       filecount oldcount))
      )
    ))

(defun cvs-status-mark-and-commit ()
  "Mark and commit all Locally changed files in the Module Status buffer"
  (interactive)
  (cvs-status-mark-changed t)
  (cvs-dired-commit))

(defun cvs-status-delete-file ()
  "Delete a file from the Module Status buffer.
The file is only deleted from the buffer, and not from the disk.
This command can only be executed from the Module Status buffer."
  (interactive)
  (let ( (buf (current-buffer))
	 (statbuf (get-buffer cvs:status-buffer))
	 case-fold-search buffer-read-only)
    (if (not (eq buf statbuf))
	(error "The current buffer is not the Module Status buffer!"))
    (save-excursion
      (beginning-of-line)
      (if (looking-at " *[*] *[A-Z]")
	  (progn
	    (delete-region (point)
			   (save-excursion
			     (forward-line 1)
			     (point)
			     ))
	    (set-buffer-modified-p nil)
	    )
	(error "No file on this line")
	)
      )
    ))

(defun cvs:status-filter (proc string)
  "Filter for the cvs update process"
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((buffer-read-only nil)
	  (buf (get-buffer cvs:status-buffer))
	  (start (process-mark proc))
	  )
      (goto-char (point-max))
      (insert string)
      (goto-char start)
      (while (re-search-forward "^\\([A-Z?]\\) \\(.*\\)" (point-max) t)
	(let* ((status (elt (buffer-substring (match-beginning 1) (match-end 1)) 0))
	       (filename (buffer-substring (match-beginning 2) (match-end 2)))
	       (status-name (cond
			     ((eq status ?U) "Needs Update")
			     ((eq status ?P) "Needs Patch")
			     ((eq status ?A) "Locally Added")
			     ((eq status ?R) "Locally Removed")
			     ((eq status ?M) "Locally Modified")
			     ((eq status ?C) "Conflict")
			     ((eq status ??) "Unknown")
			     (t (concat "Invalid " status)) )) )  
	  (save-excursion
	    (set-buffer buf)
	    (goto-char (point-max))
	    (let ( (buffer-read-only nil) 
		   begin end extent)
	      (if (fboundp 'make-extent)
		  (progn
		    (insert (format "  * %-17s : " status-name))
		    (setq begin (point))
		    (insert filename)
		    (setq end (point))
		    (insert "\n")
		    (setq extent (make-extent begin end))
		    (set-extent-face extent 'bold)
		    (set-extent-property extent 'highlight t)
		    (set-extent-property extent 'keymap cvs:status-file-keymap)
		    )
		(insert (format "  * %-17s : %s\n" status-name filename)))
	      ))))
      (set-marker (process-mark proc) (point))
      )))

(defun cvs:status-sentinel (process event)
  "Sentinel for the cvs status process"
  (save-excursion
    (let ( (buf (get-buffer cvs:status-buffer))
	   (oldbuf (current-buffer))
	   )
      (set-buffer buf)
      (goto-char (point-max))
      (let (buffer-read-only)
	(insert "=================================================================\n")
	(insert "    (Status complete)\n\n")
	(insert
	 (substitute-command-keys
	  "Use:

    \\[cvs-status-delete-file]		To delete files from the above list.
    \\[cvs-status-mark-changed]	To mark all locally-modified, added or deleted files.

    \\[cvs-dired-commit]	To commit all marked files.

    \\[cvs-status-update]		To update all files marked as needing update.
    \\[cvs-status-mark-and-commit]		To mark and commit all files marked as
		locally-modified, added or deleted.

    \\[cvs-dired-update-file]	To update the single file under the cursor.
    \\[cvs-dired-commit-file]	To commit the single file under the cursor.
"))
	)
      (cvs:display-temp-buffer buf "Statuslist")
      (buffer-enable-undo buf)
      (kill-buffer (process-buffer process))
      (pop-to-buffer oldbuf)
      )))

(defun cvs-status-process (&optional files)
  "Display the status of the current module files"
  (interactive)
  (let* ((args (cons "-d" (if cvs-root
			      (append (list "-d" cvs-root) files) files)))
	 (proc-buffer (get-buffer-create " *CVS-Temp*"))
	 proc
	 (dir default-directory)
	 (buf (get-buffer-create cvs:status-buffer)))
    (buffer-disable-undo proc-buffer)
    (save-excursion
      (set-buffer proc-buffer)
      (erase-buffer))
    (setq proc (eval (append
		      (list 'start-process "cvs status" proc-buffer
			    cvs-command "-n" "update")
		      args)))
    (set-process-filter proc 'cvs:status-filter)
    (set-process-sentinel proc 'cvs:status-sentinel)
    (set-marker (process-mark proc)
		(save-excursion (set-buffer proc-buffer)
				(point-min))
		proc-buffer)
    (save-excursion
      (pop-to-buffer buf)
      (setq default-directory dir)	; to make dired happy
      (setq dired-subdir-alist (list (list dir (point-min))))
      (cvs:display-temp-buffer buf "Statuslist")
      (let ((buffer-read-only nil))
	(erase-buffer)
	(insert (format "%s\n" "Status of files, relative to the directory:"))
	(insert (format "\t%s\n" dir))
	(insert "=================================================================\n")
	)
      )
    ))

(defun cvs:current-line ()
  "Return the current line as an integer."
  (+ (count-lines (point-min) (point))
     (if (eq (current-column) 0)
	 1
       0)))

(defun cvs:get-filename ()
  (let ((lineno (cvs:current-line)))
    (and (not (eq lineno 1)) 
	 (not (eq lineno 2))
	 (save-excursion
	   (beginning-of-line)
	   (and (re-search-forward "^.+ : \\(.*\\)"
				   (save-excursion (end-of-line) (point))
				   t)
		(buffer-substring (match-beginning 1) (match-end 1)) ) ) )
    ) )

(require 'advice)
(defadvice dired-get-filename (around check-mode activate)
  "Use an alternative function in cvs info mode"
  (cond ((eq major-mode 'cvs-info-mode)
	  (setq ad-return-value (cvs:get-filename)))
	 (t
	  ad-do-it)))

(defun cvs-marked-status (&optional l)
  "Show the status of marked files"
  (interactive)
  (if (not l)
      (setq l cvs:marked-list))
  (cvs-status-process l))

;;=============================================================================
;; explicitly committing current buffer
;;=============================================================================
(defun cvs-commit-file ()
  "Explicitly commit current buffer.
Even when there is a nonempty list of marked files.
This function uses `cvs-commit'."
  (interactive)
  (let ((cvs:marked-list nil))
    (cvs-commit)))

;;=============================================================================
(defun cvs:save ()
  "Save the buffers modified in cvs:commit-list"
  (let ((files cvs:commit-list))
    (while files
      (let ((buf (get-file-buffer (car files))))
	(if (and buf
		 (buffer-modified-p buf)
		 (y-or-n-p (concat "Save file " (car files) "? ")))
	    (save-excursion
	      (set-buffer buf)
	      (save-buffer)
	      ))
	(setq files (cdr files))))))

;;=============================================================================
(defun cvs-submit-report ()
  "Report a problem, a suggestion or a comment about cvs.el"
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   cvs:maintainer-address
   (concat "cvs.el " cvs:version)
   '(cvs-temp-dir
     cvs-command
     cvs-root
     cvs-minor-mode-hooks
     cvs-load-hooks
     cvs-commit-hooks
     cvs-before-commit-hooks
     cvs-add-hooks
     cvs-shell-command
     cvs-shell-command-option
     cvs-file-option
     cvs-no-log-option
     cvs-never-use-emerge
     cvs-save-prefix)
   nil
   (function (lambda()
	       (insert (concat "\nCVSROOT=" (getenv "CVSROOT") "\n"))
	       (call-process "cvs" nil t nil "-v")))
   "Dear cvs.el maintainer,"
   ))

;;=============================================================================
(defun cvs:hook ()
  "`Find-file' and `revert-buffer' hooks for CVS detection.
If detected, `cvs:hook' positions the ediff variable
`ediff-version-control' to process diff between CVS revisions."
  (if (is-under-cvs)
      (progn
	(if (not (boundp 'ediff-version-control-package))
	    (setq ediff-version-control-package 'vc))
	(make-local-variable 'ediff-version-control-package)
	(setq ediff-version-control-package 'cvs))))

(add-hook 'find-file-hooks (function cvs:hook))
(add-hook 'after-revert-hook (function cvs:hook))

;;=============================================================================
;; msb support
;;=============================================================================
(defun cvs:add-msb-submenu ()
  "Add msb entries to list CVS and CVS marked buffers"
  (setq msb-menu-cond (append (list '((and (boundp 'cvs-minor-mode)
					   cvs-minor-mode
					   'multi)
				      1015
				      "CVS (%d)")
				    '((and (boundp 'cvs-minor-mode)
					   cvs-minor-mode
					   cvs:mark
					   'multi)
				      1016
				      "CVS marked files (%d)"))
			      msb-menu-cond))
  (add-hook 'cvs-mark-hooks (function (lambda()
					(menu-bar-update-buffers t)))))

(or (and (featurep 'msb)
	 (cvs:add-msb-submenu))
    (add-hook 'msb-after-load-hooks
	      (function cvs:add-msb-submenu)))

;;=============================================================================
;; dired support
;;=============================================================================
(defun cvs:dired-hook ()
  "`dired-mode-hook' to add support for cvs in dired buffers"
  (let ((dir (concat (expand-file-name dired-directory) "CVS")))
    (if (and (not (string-match cvs:remote-regexp dir))
	     (file-directory-p dir))
	(cvs-dired-mode)))
  )

(add-hook 'dired-mode-hook (function cvs:dired-hook))

(defvar cvs-dired-mode nil
  "Status variable to switch to CVS minor mode if sets to t")
(make-variable-buffer-local 'cvs-dired-mode)
(put 'cvs-dired-mode 'permanent-local t)

(defconst cvs-dired:entry
  (list 'cvs-dired-mode (cons "" '(" CVS")))
  "Entry to display CVS in mode line")

(defvar cvs-dired:map (make-sparse-keymap)
  "CVS dired minor mode keymap")

(define-key cvs-dired:map "\C-cvo" 'cvs-dired-log)
(define-key cvs-dired:map "\C-cvs" 'cvs-dired-file-status)
(define-key cvs-dired:map "\C-x\C-q" 'cvs-dired-edit)
(define-key cvs-dired:map "\C-cve" 'cvs-dired-editors)
(define-key cvs-dired:map "\C-cvw" 'cvs-dired-watchers)
(define-key cvs-dired:map "\C-cvU" 'cvs-update-directory)
(define-key cvs-dired:map "\C-cvS" 'cvs-status-process)
(define-key cvs-dired:map "\C-cvM" 'cvs-dired-marked-status)
(define-key cvs-dired:map "\C-cvA" 'cvs-dired-add)
(define-key cvs-dired:map "\C-cvB" 'cvs-merge-branch)
(define-key cvs-dired:map "\C-cvL" 'cvs-status-mark-changed)
(define-key cvs-dired:map "\C-cvd" 'cvs-dired-ediff-internal)
(define-key cvs-dired:map "\C-cv\C-d" 'cvs-dired-ediff)
(define-key cvs-dired:map "\C-cvi" 'cvs-dired-diff)
(define-key cvs-dired:map "\C-cvc" 'cvs-dired-commit)
(define-key cvs-dired:map "\C-cvC" 'cvs-dired-commit-file)
(define-key cvs-dired:map "\C-cvl" 'cvs-list)
(define-key cvs-dired:map "\C-cvf" 'cvs-flush)
(define-key cvs-dired:map "\C-cvu" 'cvs-dired-update-file)
(define-key cvs-dired:map "\C-cvr" 'cvs-dired-revert)
(define-key cvs-dired:map "\C-cvb" 'cvs-submit-report)
(define-key cvs-dired:map "\C-cvh" 'cvs-dired-history)
(define-key cvs-dired:map "\C-cva" 'cvs-dired-annotate)
(easy-menu-define
 cvs-dired:menu
 cvs-dired:map
 "CVS dired minor mode keymap"
 '("CVS"
    ["Add" cvs-dired-add t]
    "-----------" 
    ["Update File" cvs-dired-update-file t]
    ["(Un)Edit" cvs-dired-edit t]
    ["Commit File" cvs-dired-commit-file t]
    ["Log" cvs-dired-log t]
    ["Annotate" cvs-dired-annotate t]
    ["History" cvs-dired-history t]
    ["File Status" cvs-dired-file-status t]
    ["Editors" cvs-dired-editors t]
    ["Watchers" cvs-dired-watchers t]
    ["EDiff" cvs-dired-ediff-internal t]
    ["Diff" cvs-dired-diff t]
    "-----------" 
    ["Module Status" cvs-status-process t]
    ["Update Module" cvs-update-directory t]
    "-----------" 
    ("Marking Files" 
     ["Show List" cvs-list t]
     ["Flush List" cvs-flush t]
     )
    ("Action on Marked Files"
     ["Commit" cvs-dired-commit t]
     ["Show Status" cvs-dired-marked-status t]
     )
    ("Other Commands" 
     ["EDiff two revs" cvs-ediff t]
     ["Restore version" cvs-dired-revert t]
     ["Change description" cvs-dired-description t]
     ["Change log message" cvs-dired-change-log t]
     ["Merge backup" cvs-dired-merge-backup t]
     ["Merge branch" cvs-merge-branch t])
    "--------" 
    ["Send mail report" cvs-submit-report t]))

(or (assq 'cvs-dired-mode minor-mode-alist)
    (setq minor-mode-alist (cons cvs-dired:entry minor-mode-alist)))

(or (assq 'cvs-dired-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'cvs-dired-mode cvs-dired:map)
				     minor-mode-map-alist)))

(defun cvs-dired-mode ()
  "Turn on cvs-dired minor mode."
  (setq cvs-dired-mode t)
  (easy-menu-add cvs-dired:menu cvs-dired:map)
  )

(defun cvs-dired-file-status ()
  "Call `cvs-file-status' on current file."
  (interactive)
  (cvs-file-status (dired-get-filename)))

(defun cvs-dired-edit ()
  "Call `cvs-edit' on current file."
  (interactive)
  (let ((filename (dired-get-filename)))
    (cvs-edit filename)
    (dired-update-file-line filename) ) )

(defun cvs-dired-editors ()
  "Call `cvs-editors' on current file."
  (interactive)
  (cvs-editors (dired-get-filename)))

(defun cvs-dired-watchers ()
  "Call `cvs-watchers' on current file."
  (interactive)
  (cvs-watchers (dired-get-filename)))

(defun cvs-dired-commit-file ()
  "Call `cvs-commit-file' on current file."
  (interactive)
  (cvs-commit (list (dired-get-filename))))

(defun cvs-dired-log ()
  "Call `cvs-log' on current file."
  (interactive)
  (cvs-log (dired-get-filename)))

(defun cvs-dired-annotate ()
  "Call `cvs-annotate' on current file."
  (interactive)
  (cvs-annotate (dired-get-filename)))

(defun cvs-dired-history ()
  "Call `cvs-history' on current file."
  (interactive)
  (cvs-history (dired-get-filename)))

(defun cvs-dired-update-file ()
  "Call `cvs-update-file' on current file."
  (interactive)
  (cvs-update-file (dired-get-filename)))

(defun cvs-dired-diff (rev)
  "Call `cvs-diff' on current file."
  (interactive "sVersion to visit (default is latest version): ")
  (cvs-diff rev (dired-get-filename)))

(defun cvs-dired-add (msg)
  "Add current file to CVS."
  (interactive "sEnter description : ")
  (cvs-add msg (dired-get-filename)))

(defun cvs-dired-description ()
  "Call `cvs-description' on current file."
  (interactive)
  (cvs-description (dired-get-filename)))

(defun cvs-dired-change-log (rev)
  "Call `cvs-change-log' on current file."
  (interactive "sVersion (default is current version): ")
  (cvs-change-log rev (dired-get-filename)))

(defun cvs-dired-revert (rev)
  "Call `cvs-revert' on current file."
  (interactive "sVersion to revert from (default is latest version): ")
  (cvs-revert rev (dired-get-filename)))

(defun cvs-dired-merge-backup ()
  "Call `cvs-merge-backup' on current file."
  (interactive)
  (cvs-merge-backup (dired-get-filename)))

(defun cvs-dired-commit ()
  "Call `cvs-commit' on marked files."
  (interactive)
  (let ((l (or (let (f (dired-get-marked-files))
		 (if (and f (> (length f) 0)) f nil)) cvs:marked-list)))
    (if (not l)
	(error "No marked files")
      (cvs-commit l))))

;; Of dubious use:
;;(defun cvs-dired-update-marked ()
;;  "Call `cvs-update-file' on marked files."
;;  (interactive)
;;  (let ((l (or (let (f (dired-get-marked-files))
;;		 (if (and f (> (length f) 0)) f nil)) cvs:marked-list)))
;;    (if (not l)
;;	(error "No marked files")
;;      (cvs-update-file l))))

(defun cvs-dired-marked-status ()
  "Call `cvs-marked-status' on marked files."
  (interactive)
  (let ((l (or (let (f (dired-get-marked-files))
		 (if (and f (> (length f) 0)) f nil)) cvs:marked-list)))
    (if (not l)
	(error "No marked file")
      (cvs-marked-status l))))

(defun cvs-dired-ediff-internal ()
  "Edit currect file and call `cvs-ediff-internal'."
  (interactive)
  (dired-find-file)
  (call-interactively 'cvs-ediff-internal))

(defun cvs-dired-ediff ()
  "Edit currect file and call `cvs-ediff'."
  (interactive)
  (dired-find-file)
  (call-interactively 'cvs-ediff))

;;=============================================================================
(run-hooks 'cvs-load-hooks)

;;=============================================================================
(provide 'cvs)

;;; end of cvs.el
