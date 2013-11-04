;;; patcher.el --- Utility for mailing patch information

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Didier Verna.

;; PRCS: $Id: patcher.el,v 1.23 2005/07/26 14:20:07 didierv Exp $

;; Author:        Didier Verna <didier@xemacs.org>
;; Maintainer:    Didier Verna <didier@xemacs.org>
;; Created:       Tue Sep 28 18:12:43 1999
;; Last Revision: Tue Aug 31 14:14:48 2004
;; Keywords:      maint

;; This file is part of Patcher.

;; Patcher is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 2,
;; as published by the Free Software Foundation.

;; Patcher is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Contents management by FCM version 0.1.

;; Patcher is an XEmacs package designed to automate and ease the
;; maintenance of archive-based projects.  It provides assistance in
;; building, reporting and committing patches, as well as in handling the
;; corresponding ChangeLog entries, for example by creating skeletons.

;; Patcher is fully documented.  Please refer to the documentation for
;; information on how to use it.  All user options can be found in the
;; "Patcher" Custom group.


;; Suggestions for further improvements:

;; #### Implement a default theme for XEmacs developpers / committers.

;; #### Investigate on the notion of adding new files (it's different across
;; RCSes).

;; #### If the user answers `no' to the confirm commit question, it should be
;; possible to edit manually the computed commit command.

;; #### The subject-related strings could benefit from almost all %
;; constructs.

;; #### Provide a way to attach patches instead of inserting them as plain
;; text.

;; #### Provide a way to filter out some files from the mailed patch.  For
;; instance, when you commit yourself, you want to have `configure' in your
;; files, but you don't want to display a patch against it.

;; #### Before sending the message, we could check that the contents is ok
;; (like, there's no more diff errors and stuff).

;; #### Implement a real error / warning mechanism.

;; #### When a project is found to be out of date, we could implement
;; something to update it and re-run patcher again.

;; #### For the 'gnus mail method, add the possibility to temporarily use a
;; different one if the user don't want to start Gnus.
;;
;; #### Consider doing a better job of handling overlapping patcher
;; instances. e.g. use a different extent property for each instance,
;; and keep a usage count for the ChangeLog files so that they're only
;; killed when no instance uses them any more.
;;
;; #### Also add an option to kill, not just bury, the mail message when
;; it's sent.
;;
;; #### When the message is sent, the cvs commit results window should be
;; removed and the corresponding buffer buried.
;;
;; #### Have a control window to clarify the progress of everything.
;; Make it perhaps a small buffer, above the mail message/ChangeLog
;; buffer.  It shows what steps have been completed, what haven't, and
;; what to do.  It should have buttons in it for the various actions.
;; One button is for include-changelogs, one for commit, one for send
;; the mail, and one for execute the commit.  These should be enabled
;; or grayed out appropriately.  It should also have buttons that show
;; the associated changelogs; clicking on a button puts that changelog
;; buffer in the main window below the control window.  By each
;; changelog button should be an indication of whether the changelog
;; has been modified so far.  The control window should stay around as
;; much as it can during the whole process (e.g. use
;; set-buffer-dedicated-p or something), so that it's always there to
;; drive the whole process.  One corollary is that you don't actually
;; have to switch to the mail buffer to (e.g.) execute
;; include-changelogs -- you just click on the control window, and it
;; does it automatically.  also, when you execute include-changelogs,
;; it can issue a warning if not all changelogs have been visited, and
;; prompt you to make sure you want to continue.  Similarly, you can
;; run include-changelogs more than once even if it succeeded the
;; first time (e.g. for some reason you didn't like the result and
;; deleted it), but it will prompt "are you sure?".  There could also
;; be an "undo include-changelogs", if you make a mistake after doing
;; include-changelogs and realize you want to go back and fix the
;; problem and do include-changelogs again.



;; Internal notes:

;; - See why the ChangeLogs are represented in absolute path instead of
;; relative to the project's directory. I can't remember, but this looks
;; weird.



;; Thanks to these people for their suggestions, testing and contributions:
;; Adrian Aichner, Ben Wing, Karl Pflasterer, Malcolm Purvis, Norbert Koch,
;; Raphael Poss, Stephen J. Turnbull, Steve Youngs.


;;; Code:

;; Require 'sendmail for getting `mail-header-separator'.
;; #### Now that a fake mail sending function exists, sendmail shouldn't be
;; #### systematically required like this.  However, since most users will
;; #### really want do send real messages, it probably doesn't hurt to keep
;; #### things as-is.
(require 'sendmail)


;; These macros are copied from bytecomp-runtime.el because they're only
;; available in XEmacs 21.5.

(defmacro patcher-globally-declare-fboundp (symbol)
  (when (cl-compiling-file)
    (setq symbol (eval symbol))
    (if (not (consp symbol))
	(setq symbol (list symbol)))
    ;; Another hack.  This works because the autoload environment is
    ;; currently used ONLY to suppress warnings, and the actual
    ;; autoload definition is not used.  (NOTE: With this definition,
    ;; we will get spurious "multiple autoloads for %s" warnings if we
    ;; have an autoload later in the file for any functions in SYMBOL.
    ;; This is not something that code should ever do, though.)
    (setq byte-compile-autoload-environment
	  (append (mapcar #'(lambda (sym) (cons sym nil)) symbol)
		  byte-compile-autoload-environment)))
  nil)

(defmacro patcher-globally-declare-boundp (symbol)
  (setq symbol (eval symbol))
  (if (not (consp symbol))
      (setq symbol (list symbol)))
  `(progn
     ;; (defvar FOO) has no side effects.
     ,@(mapcar #'(lambda (sym) `(defvar ,sym)) symbol)))


(patcher-globally-declare-boundp 'font-lock-always-fontify-immediately)



;; ===========================================================================
;; Version management
;; ===========================================================================

;; $Format: "(defconst patcher-prcs-major-version \"$ProjectMajorVersion$\")"$
(defconst patcher-prcs-major-version "version-3-8")
;; $Format: "(defconst patcher-prcs-minor-version \"$ProjectMinorVersion$\")"$
(defconst patcher-prcs-minor-version "1")
(defconst patcher-version
  (let ((level patcher-prcs-minor-version)
	major minor status)
    (string-match "\\(branch\\|version\\)-\\([0-9]+\\)-\\([0-9]+\\)"
		  patcher-prcs-major-version)
    (setq major (match-string 2 patcher-prcs-major-version)
	  minor (match-string 3 patcher-prcs-major-version)
	  status (match-string 1 patcher-prcs-major-version))
    (cond ((string= status "version")
	   (setq level (int-to-string (1- (string-to-int level))))
	   (if (string-equal level "0")
	       (concat major "." minor)
	     (concat major "." minor "." level)))
	  ((string= status "branch")
	   (concat major "." minor "-b" level)))
    ))

;;;###autoload
(defun patcher-version ()
  "Show the current version of Patcher."
  (interactive)
  (message "Patcher version %s" patcher-version))



;; ===========================================================================
;; General utilities
;; ===========================================================================

(defsubst patcher-message (msg &rest args)
  ;; Print a message, letting XEmacs time to display it.  Also, handle command
  ;; substitution.
  (message (substitute-command-keys (apply 'format msg args)))
  (sit-for 0))

(defsubst patcher-warning (msg &rest args)
  ;; Like `patcher-message, but triggers a Patcher warning instead.
  (warn (substitute-command-keys (apply 'format msg args))))

(defsubst patcher-error (msg &rest args)
  ;; Like `patcher-message, but triggers a Patcher error instead.
  (error (substitute-command-keys (apply 'format msg args))))

(defmacro patcher-with-progression (msg &rest body)
  ;; wrap BODY in "msg..." / "msg...done" messages.
  ;; Return the value of BODY execution.
  `(prog2
       (patcher-message (concat ,msg "... please wait."))
       (progn ,@body)
     (patcher-message (concat ,msg "... done."))))
(put 'patcher-with-progression 'lisp-indent-function 1)


(defsubst patcher-file-relative-name (file &optional dir raw)
  ;; Construct a filename from FILE relative to DIR (default directory if not
  ;; given).  Unless RAW is given, force unix syntax
  ;; #### NOTE: FILE may be either a string (a file name), or a list such as
  ;; #### patcher-change-logs elements, whose car must then be the file name
  ;; #### in question.
  (and (listp file) (setq file (car file)))
  (let ((directory-sep-char (if raw directory-sep-char ?/)))
    (or dir (setq dir default-directory))
    (file-relative-name (expand-file-name file (expand-file-name dir))
			(expand-file-name dir))
    ))

(defsubst patcher-files-string (files)
  ;; Convert FILES to a string of relative file names.  Unless RAW is given,
  ;; force unix syntax.
  ;; #### NOTE: FILES may be either a list of strings (file names), or a list
  ;; #### such as the one in patcher-change-logs.
  (mapconcat 'patcher-file-relative-name  files " "))

(defun patcher-files-buffers (files &optional find)
  ;; Find a buffer visiting each file in FILES, and return a list of
  ;; corresponding buffers.  Skip files that are not visited, unless optional
  ;; argument FIND is non nil.  In that case, visit the file.
  ;; #### NOTE: FILES may be either a list of strings (file names), or a list
  ;; #### such as the one in patcher-change-logs.
  (let (buffer buffers)
    (dolist (file files buffers)
      (and (listp file) (setq file (car file)))
      (setq buffer (or (get-file-buffer file)
		       (and find (find-file-noselect file))))
      (when buffer (push buffer buffers)))
    ))

(defun patcher-save-buffers (buffers)
  ;; Offer to save some buffers.
  ;; #### FIXME: this should be a standard function somewhere.
  (map-y-or-n-p
   (lambda (buffer)
     (and (buffer-modified-p buffer)
	  (not (buffer-base-buffer buffer))
	  (buffer-file-name buffer)
	  (format "Save file %s? "
		  (buffer-file-name buffer))))
   (lambda (buffer)
     (save-excursion
       (set-buffer buffer)
       (condition-case ()
	   (save-buffer)
	 (error nil))))
   buffers
   '("buffer" "buffers" "save")))

(defun patcher-goto-subject ()
  ;; Move point to the beginning of the Subject: header's contents, if any.
  ;; Return that position, or nil.
  ;; #### FIXME: maybe we should issue a warning if no subject line is found ?
  (let (pos)
    (save-excursion
      (goto-char (point-min))
      (setq pos (re-search-forward "^Subject: " nil t)))
    (and pos (goto-char pos))))

(defun patcher-goto-signature ()
  ;; Move point to the beginning of the mail signature (actually, in front of
  ;; the signature separator), if any.  Otherwise, move point to the end of
  ;; the message. Return that position.
  (goto-char (point-min))
  (if (re-search-forward
       (cond ((eq major-mode 'mail-mode)
	      ;; this is hard-wired in sendmail.el
	      "\n\n-- \n")
	     ((eq major-mode 'message-mode)
	      message-signature-separator)
	     (t
	      (patcher-warning "\
Major mode: %s.
Your mailing method is not fully supported by Patcher.
This is not critical though: Patcher may not find the message signature
correctly.

Please report to <didier@xemacs.org>."
			       major-mode)
	      ;; Use the standard one by default.
	      "\n\n-- \n"))
       nil t)
      (goto-char (match-beginning 0))
    ;; else: no signature
    (goto-char (point-max))))



;; ===========================================================================
;; Projects description
;; ===========================================================================

(defgroup patcher nil
  "Automatic archive-base project maintenance.")

(defgroup patcher-default nil
  "Default settings for Patcher project options."
  :group 'patcher)

(defcustom patcher-default-name nil
  "*Default name for Patcher projects.

This project option (a string) exists to let you define different Patcher
projects (hence with different names) sharing a common name for the
underlying diff and commit commands.  If set, it will be used rather than
the real project's name."
  :group 'patcher-default
  :type '(choice (const :tag "Patcher name" nil)
		 (string :tag "Other name")))

(defcustom patcher-default-mail-method 'compose-mail
  "*Default method used by Patcher to prepare a mail.

Currently, there are four built-in methods: 'compose-mail \(the default),
'sendmail, 'message, 'gnus and 'fake.  Please refer to the corresponding
`patcher-mail-*' function for a description of each method.

You can also define your own method, say `foo'.  In that case, you *must*
provide a function named `patcher-mail-foo' which takes two arguments: a
project descriptor and a string containing the subject of the message.
This function must prepare a mail buffer.  If you want to do this, please
see how it's done for the built-in methods."
  :group 'patcher-default
  :type '(radio (const compose-mail)
		(const sendmail)
		(const message)
		(const gnus)
		(const fake)
		(symbol :tag "other")))

(defcustom patcher-default-user-name nil
  "*Default user full name to use when sending a Patcher mail.

If not defined, `user-full-name' is used."
  :group 'patcher-default
  :type  '(choice (const :tag "user-full-name" nil)
	          string))

(defcustom patcher-default-user-mail nil
  "*Default user mail address to use when sending a Patcher mail.

If not defined, `user-mail-address' is used."
  :group 'patcher-default
  :type '(choice (const :tag "user-mail-address" nil)
	          string))

(defcustom patcher-default-to-address "xemacs-patches@xemacs.org"
  "*Default To: header value to use when sending a Patcher mail.

This variable is used by all mail methods except the 'gnus one \(see
`patcher-default-mail-method').  If not defined, it is prompted for."
  :group 'patcher-default
  :type '(choice (const :tag "Ask" nil)
		 string))

(defcustom patcher-default-gnus-group nil
  "*Default Gnus group to use when sending a Patcher mail.

This variable is used only in the 'gnus mail method \(see
`patcher-default-mail-method').  The mail sending process will behave as if
you had typed `C-u a' in the group buffer on that Gnus group.  If not
defined, it is prompted for."
  :group 'patcher-default
  :type  '(choice (const :tag "Ask" nil)
	          string))

(defcustom patcher-default-subject-prefix "[PATCH]"
  "*Default prefix for the subject of Patcher mails.

The following string transformations are performed:
- %n: the value of the :name project option if set, or the project's name
      in the Patcher sense.
- %N: the project's name in the Patcher sense.

A space will be inserted between the prefix and the rest of the subject,
as appropriate.  This part of the subject is never prompted for.  See
also `patcher-default-subject' and
`patcher-default-subject-committed-prefix'."
  :group 'patcher-default
  :type 'string)

(defcustom patcher-default-subject-committed-prefix "[COMMIT]"
  "*Default prefix for the subject of Patcher mails.

Same as `patcher-default-subject-prefix', but for committed patches.  If nil,
keep the normal subject prefix."
  :group 'patcher-default
  :type '(choice (const :tag "Don't change" nil)
		 string))

(defcustom patcher-default-subject ""
  "*Default subject for Patcher mails.

The following string transformations are performed:
- %n: the value of the :name project option if set, or the project's name
      in the Patcher sense.
- %N: the project's name in the Patcher sense.

Please note that this is used *only* to provide a default value for prompted
subjects.  Subjects are *always* prompted for.

See also `patcher-default-subject-prefix' and
`patcher-default-subject-committed-prefix', which are not subject to
prompting."
  :group 'patcher-default
  :type 'string)

(defcustom patcher-default-mail-prologue ""
  "*Default string to insert at the beginning of every Patcher mail."
  :group 'patcher-default
  :type 'string)

(defcustom patcher-default-change-logs-updating 'automatic
  "*Controls the way ChangeLog fields are updated.

Possible values and their meaning are:
- 'automatic: \(the default) Patcher generates ChangeLog skeletons
   automatically based on the created diff (you then have to fill up the
   entries as needed).
- 'manual: you are supposed to have updated the ChangeLog files by hand,
   prior to calling Patcher.
-  nil: you don't do ChangeLogs at all."
  :group 'patcher-default
  :type '(radio (const :tag "Automatic" automatic)
		(const :tag "Manual" manual)
		(const :tag "None" nil)))

(defcustom patcher-default-change-logs-user-name nil
  "*User full name for generated ChangeLog entries.

If nil, let `patch-to-change-log' decide what to use.
Otherwise, it should be a string."
  :group 'patcher-default
  :type '(choice (const :tag "Default" nil)
		 (string :tag "Other name")))

(defcustom patcher-default-change-logs-user-mail nil
  "*User mail address for generated ChangeLog entries.

If nil, let `patch-to-change-log' decide what to use.
Otherwise, it should be a string."
  :group 'patcher-default
  :type '(choice (const :tag "Default" nil)
		 (string :tag "Other mail")))

(defcustom patcher-default-change-logs-appearance 'verbatim
  "*Controls the appearance of ChangeLog entries in Patcher mails.

The values currently supported are:
- 'verbatim \(the default): ChangeLog entries appear simply as text above
   the patch.  A short line mentioning the ChangeLog file they belong to
   is added when necessary.
- 'packed: ChangeLog files are diff'ed, but the output is packed above the
   rest of the patch.
- 'patch: ChangeLog files are diff'ed, and the output appears as part of
   the patch itself.
-  nil: ChangeLog entries don't appear at all.

See also the `patcher-default-change-logs-diff-command' user option."
  :group 'patcher-default
  :type '(radio (const :tag "Verbatim" verbatim)
		(const :tag "Diff, packed together" packed)
		(const :tag "Diff, part of the patch" patch)
		(const :tag "Don't appear in message" nil)))

(defcustom patcher-default-change-logs-prologue "%f addition:"
  "*Default string to insert just before ChangeLogs in every Patcher mail.

This applies when ChangeLog additions appear verbatim in the message.
A %f occurring in this string will be replaced with the ChangeLog file name
(relative to the project's directory)."
  :group 'patcher-default
  :type 'string)

(defcustom patcher-default-diff-prologue-function
  'patcher-default-diff-prologue
  "*Function used to insert a prologue before each diff output.

Insertion must occur at current point in current buffer.
This function should take one argument indicating the kind of diff:
- a value of 'sources indicates a source diff only,
- a value of 'change-logs indicates a ChangeLog diff only,
- a value of 'mixed indicates a diff on both source and ChangeLog files.

The following variables are bound (when appropriate) when this function
is executed:
- `name': the name of the current Patcher project,
- `source-diff': the command used to create a source diff,
- `change-log-diff': the command used to create a ChangeLog diff,
- `source-files': sources files affected by the current patch,
- `change-log-files': ChangeLog files affected by the current patch.

In the case of a 'mixed diff, a nil value for `change-log-diff' indicates
that the same command was used for both the source and ChangeLog files."
  :group 'patcher-default
  :type '(choice (const :tag "Default" patcher-default-diff-prologue)
		 (const :tag "None" nil)
		 (symbol :tag "Other")))

(defcustom patcher-default-command-directory nil
  "*Default command directory for Patcher projects.

This directory (a string) can be relative to the project's directory.
All diff and commit commands are executed from this directory if set.
Otherwise, the project's directory is used."
  :group 'patcher-default
  :type  '(choice (const :tag "Same directory" nil)
		  (string :tag "Other directory")))


(defcustom patcher-default-pre-command ""
  "*Default string to prefix patcher commands with.

This is where you would put things like \"runsocks\"."
  :group 'patcher-default
  :type 'string)

(defcustom patcher-default-diff-command "cvs -q diff -u %f"
  "*Default method used by Patcher to generate a patch.

The following string transformations are performed:
- %n: the value of the :name project option if set, or the project's name
      in the Patcher sense.
- %N: the project's name in the Patcher sense.
- %f: the files affected by the patch.  These files can be specified by
      using `patcher-mail-subproject' instead of `patcher-mail' to prepare
      the patch.  Otherwise, the %f will simply be removed."
  :group 'patcher-default
  :type 'string)

(defcustom patcher-default-after-diff-hook nil
  "*Hook run on the output of a Patcher diff comand.

The functions in this hook should operate on the current buffer and take
two optional arguments limiting the processing to a buffer region
(in the absence of arguments, the whole buffer should be processed).

When each function in this hook is run, the point is placed at the
beginning of the region, and the buffer excursion is saved for you.

There is a special built-in function named `patcher-prcs-diff-convert'
that can be used in this hook in order to convert a PRCS diff output to
a traditional one."
  :group 'patcher-default
  :type 'hook)

(defcustom patcher-default-diff-line-filter "\\? .*"
  "*Default line filter to pass Patcher diffs through.

When inserting a diff in Patcher mails, lines matching this regexp will
be excluded.

Note: the regexp must match the whole line.  Don't add beginning and end
of line markers to it, Patcher will do this for you.

By default, local files unknown to CVS are filtered out."
  :group 'patcher-default
  :type 'regexp)

(defcustom patcher-default-change-logs-diff-command "cvs -q diff -U 0 %f"
  "*Command to use to generate ChangeLog diffs.

This value is used when the ChangeLog appearance is either 'packed or
'patch (see the variable `patcher-default-change-logs-appearance').

If set to 'diff, use the same command as for the rest of the patch.
Otherwise, it should be a string.

The following string transformations are performed:
- %n: the value of the :name project option if set, or the project's name
      in the Patcher sense.
- %N: the project's name in the Patcher sense.
- %f: the ChangeLog filenames.

Note: it is highly recommended to remove the context from ChangeLog diffs
because they often fail to apply correctly."
  :group 'patcher-default
  :type '(choice (const  :tag "Project diff command" diff)
		 (string :tag "Other diff command" "cvs -q diff -U 0 %f")))

(defcustom patcher-default-commit-privilege nil
  "*Default value for Patcher commit privilege status.

If you have the privilege to commit patches yourself, you should set
this option to t."
  :group 'patcher-default
  :type 'boolean)

(defcustom patcher-default-commit-command "cvs commit -F %s %f"
  "*Default method used by Patcher to commit a patch.

The following string transformations are performed:
- %n: the value of the :name project option if set, or the project's name
      in the Patcher sense.
- %N: the project's name in the Patcher sense.
- %s: the name of a file containing the commit log message.
- %S: the commit log message itself (quoted to prevent shell expansion).
- %f: the files affected by the patch.  These files can be specified by using
     `patcher-mail-subproject' instead of `patcher-mail' to prepare the patch.
      Otherwise, the %f will simply be removed."
  :group 'patcher-default
  :type 'string)

(defcustom patcher-default-confirm-commits t
  "*Whether Patcher asks for a confirmation before doing a commit."
  :group 'patcher-default
  :type 'boolean)

(defcustom patcher-default-committed-notice
  "NOTE: This patch has been committed."
  "*Notice added to a mail when the patch is committed before sending."
  :group 'patcher-default
  :type 'string)

(defcustom patcher-default-failed-command-regexp "^cvs \\[[^]]* aborted\\]"
  "*Default regular expression for matching the result of a failed command.

Commands in question are the diff and the commit one."
  :group 'patcher-default
  :type 'regexp)

(defcustom patcher-default-log-message-items '(subject)
  "*Elements used to initialize a Patcher commit log message.

This is nil, or a list of the following items:
- 'subject: the subject of the corresponding Patcher mail (sans the prefix),
- 'compressed-change-logs: the compressed ChangeLog entries for the current
   patch.
- 'change-logs: the ChangeLog entries for the current patch.  If some items
   appear before the ChangeLog entries, the ChangeLogs separator will
   automatically be included."
  :group 'patcher-default
  :type '(set (const :tag "Subject" subject)
	      (const :tag "Compressed ChangeLogs" compressed-change-logs)
	      (const :tag "ChangeLogs" change-logs)))

(defcustom patcher-default-change-logs-separator
  "-------------------- ChangeLog entries follow: --------------------"
  "*ChangeLog entries separator for Patcher commit log messages.

Either nil, or a string which will be inserted in a line of its own.

See also the function `patcher-logmsg-insert-change-logs'."
  :group 'patcher-default
  :type 'string)

(defcustom patcher-default-edit-log-message t
  "*Whether Patcher lets you edit the commit log message.

If nil, Patcher will directly use the initialization value \(see
`patcher-default-init-log-message')."
  :group 'patcher-default
  :type 'boolean)

(defcustom patcher-default-kill-source-files-after-sending t
  "*Whether to kill source files after sending the mail.

This is effective only when sources files have not been killed already
\(see the variable `patcher-default-kill-source-files-after-diffing').

That feature is not implemented yet."
  :group 'patcher-default
  :type 'boolean)

(defcustom patcher-default-kill-change-logs-after-sending t
  "*Whether to kill the ChangeLog files after sending the mail."
  :group 'patcher-default
  :type 'boolean)

(defcustom patcher-default-kill-source-files-after-diffing t
  "*Whether to kill source files after building the ChangeLog skeletons.

These files are loaded temporarily by `patch-to-change-log'.  If this
variable is non nil, `patch-to-change-log' will be instructed to remove
them when they are not needed anymore.

See also the variable `patcher-default-kill-source-files-after-sending'."
  :group 'patcher-default
  :type 'boolean)

(defcustom patcher-default-themes nil
  "*Default themes to use in Patcher projects.

This is a list of theme names (symbols) that must be defined in the
`patcher-themes' user option."
  :group 'patcher-default
  ;; #### NOTE: ideally, this type should be computed automatically, depending
  ;; #### on the defined themes.  This arises the interesting question of
  ;; #### custom dynamic types.  Without them, it's a complex thing to do.
  :type '(repeat (symbol :tag "Theme name")))


;; Defining these const avoids coding special cases for the :inheritance,
;; :subdirectory, :files and :command-directory (sub)project option in the
;; accessor functions.
(defconst patcher-default-inheritance nil)
(defconst patcher-default-subdirectory nil)
(defconst patcher-default-files nil)

(defconst patcher-project-options-custom-type
  '((list :inline t :tag "Project name"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :name)
	  (choice (const :tag "Patcher name" nil)
		  (string :tag "Other name")))
    (list :inline t :tag "Mail method"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :mail-method)
	  (choice (const compose-mail)
		  (const sendmail)
		  (const message)
		  (const gnus)
		  (const fake)
		  (symbol :tag "other")))
    (list :inline t :tag "User name"
	  :format "%{%t%}: %v"
	  (const  :tag "" :value :user-name)
	  (choice (const :tag "user-full-name" nil)
		  string))
    (list :inline t :tag "User mail"
	  :format "%{%t%}: %v"
	  (const  :tag "" :value :user-mail)
	  (choice (const :tag "user-mail-address" nil)
	          string))
    (list :inline t :tag "To address"
	  :format "%{%t%}: %v"
	  (const  :tag "" :value :to-address)
	  (choice (const :tag "Ask" nil)
		  string))
    (list :inline t :tag "Gnus group"
	  :format "%{%t%}: %v"
	  (const  :tag "" :value :gnus-group)
	  (choice (const :tag "Ask" nil)
		  string))
    (list :inline t :tag "Subject prefix"
	  :format "%{%t%}: %v"
	  (const  :tag "" :value :subject-prefix)
	  string)
    (list :inline t :tag "Subject committed prefix"
	  :format "%{%t%}: %v"
	  (const  :tag "" :value :subject-committed-prefix)
	  (choice (const :tag "None" nil)
		  string))
    (list :inline t :tag "Subject"
	  :format "%{%t%}: %v"
	  (const  :tag "" :value :subject)
	  string)
    (list :inline t :tag "Mail prologue"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :mail-prologue)
	  string)
    (list :inline t :tag "ChangeLogs updating"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :change-logs-updating)
	  (choice (const :tag "Automatic" automatic)
		  (const :tag "Manual" manual)
		  (const :tag "None" nil)))
    (list :inline t :tag "ChangeLogs user name"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :change-logs-user-name)
	  (choice (const :tag "Default" nil)
		  (string :tag "Other name")))
    (list :inline t :tag "ChangeLogs user mail"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :change-logs-user-mail)
	  (choice (const :tag "Default" nil)
		  (string :tag "Other mail")))
    (list :inline t :tag "ChangeLogs appearance"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :change-logs-appearance)
	  (choice (const :tag "Verbatim" verbatim)
		  (const :tag "Diff, packed together" packed)
		  (const :tag "Diff, part of the patch" patch)
		  (const :tag "Don't appear" nil)))
    (list :inline t :tag "ChangeLogs prologue"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :change-logs-prologue)
	  string)
    (list :inline t :tag "Diff prologue function"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :diff-prologue-function)
	  (choice (const :tag "Default" patcher-default-diff-prologue)
		  (const :tag "None" nil)
		  (symbol :tag "Other")))
    (list :inline t :tag "Command directory"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :command-directory)
	  (choice (const :tag "Same directory" nil)
		  (string :tag "Other directory")))
    (list :inline t :tag "Pre command"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :pre-command)
	  string)
    (list :inline t :tag "Diff command"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :diff-command)
	  string)
    (list :inline t :tag "After diff hook"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :after-diff-hook)
	  hook)
    (list :inline t :tag "Diff line filter"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :diff-line-filter)
	  regexp)
    (list :inline t :tag "ChangeLogs diff command"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :change-logs-diff-command)
	  (choice (const :tag "Project diff command" diff)
		  (string :tag "Other diff command" "cvs -q diff -U 0 %f")))
    (list :inline t :tag "Commit privilege"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :commit-privilege)
	  boolean)
    (list :inline t :tag "Commit command"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :commit-command)
	  string)
    (list :inline t :tag "Confirm commits"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :confirm-commits)
	  boolean)
    (list :inline t :tag "Committed notice"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :committed-notice)
	  string)
    (list :inline t :tag "Failed command regexp"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :failed-command-regexp)
	  regexp)
    (list :inline t :tag "Log message items"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :log-message-items)
	  (set (const :tag "Subject" subject)
	       (const :tag "Compressed ChangeLogs" compressed-change-logs)
	       (const :tag "ChangeLogs" change-logs)))
    (list :inline t :tag "ChangeLogs separator"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :change-logs-separator)
	  string)
    (list :inline t :tag "Edit log message"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :edit-log-message)
	  boolean)
    (list :inline t
	  :tag "Kill source files after sending"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :kill-source-files-after-sending)
	  boolean)
    (list :inline t
	  :tag "Kill changeLogs after sending"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :kill-change-logs-after-sending)
	  boolean)
    (list :inline t
	  :tag "Kill source files after diffing"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :kill-source-files-after-diffing)
	  boolean)
    (list :inline t :tag "Themes"
	  :format "%{%t%}: %v"
	  (const :tag "" :value :themes)
	  ;; #### NOTE: ideally, this type should be computed automatically,
	  ;; #### depending on the defined themes.  This arises the
	  ;; #### interesting question of custom dynamic types.  Without them,
	  ;; #### it's a complex thing to do.
	  (repeat (symbol :tag "Theme name"))))
  ;; This is currently useless, and would cause problems in the custom type:
  ;; it will match the inheritance field in patcher-projects before the
  ;; corresponding custom type definition.
  ;;    (list :inline t :tag "Other"
  ;;	  symbol
  ;;	  sexp))
  ;; Custom type elements for Patcher project options common to
  ;; `patcher-projects' and `patcher-subprojects'.
  )


(defgroup patcher-themes nil
  "Theme settings for Patcher projects."
  :group 'patcher)

(defcustom patcher-themes '()
  "*List of themes to use in Patcher projects.

Each element looks like \(NAME :OPTION VALUE ...).  NAME is the theme
name (a symbol).  The remainder of the list is the same as in project
descriptors (see `patcher-projects').

See also `patcher-max-theme-depth'."
  :group 'patcher-themes
  :type `(repeat
	  (group (symbol :tag "Theme name")
		 ;; #### NOTE: we could be tempted to add an `inheritance'
		 ;; #### mechanism for themes, just like for projects.
		 ;; #### However, don't forget that a theme can contain other
		 ;; #### themes because themes belong to
		 ;; #### `patcher-project-options-custom-type'.
		 (repeat :inline t :tag "Options"
			 (choice :inline t :value (:mail-method compose-mail)
				 ,@patcher-project-options-custom-type))
		 ))
  )


(defgroup patcher-projects nil
  "Project settings for Patcher."
  :group 'patcher)

(defcustom patcher-projects '()
  "*List of project descriptors.

Each project descriptor looks like \(NAME DIR :OPTION VALUE ...).
- NAME is the project's name \(a string).
- DIR is the project's root directory (a string).

The remainder of the project descriptor is composed of \"project options\"
\(keyword / value pairs).  When Patcher needs a project option, it tries
to find it at different places:
- First, it looks for it in the project descriptor itself.
- If that fails, it tries to find it in the project themes, if any.
- If that fails, it tries to find it in the inherited projects, if any.
- If that fails, it falls back to the corresponding `patcher-default-*'
  user option."
  :group 'patcher-projects
  :type `(repeat
	  (group (string :tag "Project")
		 (directory :tag "Project directory")
		 (repeat :inline t :tag "Options"
			 (choice :inline t :value (:mail-method compose-mail)
				 ,@patcher-project-options-custom-type
				 (list :inline t :tag "Inheritance"
				       :format "%{%t%}: %v"
				       (const :tag "" :value :inheritance)
				       (repeat :tag "From"
					       (string :tag "Project")))))
		 ))
  )

(defcustom patcher-subprojects '()
  "*List of Patcher subproject descriptors.

Subproject descriptors are similar to project descriptors \(see the
variable `patcher-projects') with a few exceptions:

- Instead of the project directory field DIR, you specify the name of the
  project this subproject is based on.
- Two project options are available in addition to the standard ones:
  - :subdirectory lets you specify a subdirectory \(of the parent
     project's directory) in which the whole subproject resides.  There is
     no corresponding `patcher-default-subdirectory' fallback..
  - :files lets you specify a list of files or directories composing the
     subproject.  Each file specification can contain wildcards.  If a
     :subdirectory option is given, these files or directories should be
     relative to this subdirectory.  Otherwise, they should be relative to
     the base project's directory.  There is no corresponding
     `patcher-default-files' variable.
  Note that a subproject with neither a :subdirectory nor a :files option
  behaves exactly like the corresponding base project.
- Subprojects don't have an :inheritance mechanism.  Instead, they
  implicitly inherit from their base project \(which in turn can inherit
  from other projects).

Note: the normal way to use predefined Patcher subprojects is to call
`patcher-mail', *not* `patcher-mail-subproject'.  Using the former will
directly use the set of files and/or directory you have specified.  Using
the latter will also let you modify this set."
  :group 'patcher-projects
  :type `(repeat
	  (group (string :tag "Subproject")
		 (string :tag "Of project")
		 (repeat :inline t :tag "Options"
			 (choice :inline t :value (:subdirectory "")
				 ;; #### Look inside the widget library to see
				 ;; #### how we can modify the completion
				 ;; #### behavior
				 (list :inline t :tag "Subdirectory"
				       :format "%{%t%}: %v"
				       (const :tag "" :value :subdirectory)
				       directory)
				 (list :inline t :tag "Files"
				       :format "%{%t%}: %v"
				       (const :tag "" :value :files)
				       (repeat :format "\n%v%i\n" file))
				 ,@patcher-project-options-custom-type))
		 ))
  )


;; Project descriptors Accessors =============================================

;; #### NOTE: the accessors routines don't handle the case where the same
;; #### option is given several times.  Only the first one is used.  This
;; #### currently would have any sensible meaning anyway.

(defsubst patcher-project-patcher-name (project)
  (nth 0 project))

(defsubst patcher-subproject-p (project)
  ;; Return non nil if PROJECT is defined in `patcher-subprojects'.
  (member project patcher-subprojects))

(defcustom patcher-max-theme-depth 8
  "*Maximum nesting level in Patcher themes.

This option is a guard against infinite loops that might occur for wrong
settings of Patcher themes (as themes can contain themes)."
  :group 'patcher-themes
  :type 'integer)

(defun patcher-themes-option (themes option level)
  ;; Look for an option in a list of themes.  Note that themes can have the
  ;; :themes option set.  The themes tree (it shouldn't be a graph) is
  ;; traversed in depth first.
  (let (theme value)
    (while (and (not value) (setq theme (pop themes)))
      (setq theme (assoc theme patcher-themes))
      (or theme (patcher-error "`%s': no such theme" theme))
      (let ((theme-options (cdr theme)))
	(setq value (member option theme-options))
	(unless value
	  (let ((subthemes (member :themes theme-options)))
	    (when (> level patcher-max-theme-depth)
	      (patcher-error "\
Theme `%s': maximum nesting level of themes exceeded.
Either you have an infinite loop in your theme's :themes option, or you should
increase the value of `patcher-max-theme-depth'"
	       (car theme)))
	    (setq value
		  (patcher-themes-option (cadr subthemes) option (1+ level)))
	    ))
	))
    value))

(defcustom patcher-max-inheritance-depth 8
  "*Maximum nesting level in Patcher projects.

This option is a guard against infinite loops that might occur for wrong
settings of Patcher projects (as projects can inherit projects)."
  :group 'patcher-projects
  :type 'integer)

(defun patcher-project-option-1 (project option level)
  ;; Try to find an option in the project descriptor, otherwise, try in each
  ;; project from the project's inheritance list.
  ;; The whole option form is returned: '(:stuff value)
  (when (> level patcher-max-inheritance-depth)
    (patcher-error "\
Project `%s': maximum nesting level of projects exceeded.
Either you have an infinite loop in your project's inheritance, or you should
increase the value of `patcher-max-inheritance-depth'"
		   (patcher-project-patcher-name project)))
  (let* ((is-subproject (patcher-subproject-p project))
	 (options (cddr project))
	 (value (member option options)))
    ;; Try to find the option in themes.
    (unless value
      (let ((themes (member :themes options)))
	(when themes
	  (setq value (patcher-themes-option (cadr themes) option 0)))
	))
    ;; Try to find the option in inherited projects.  Note that inherited
    ;; projects can have their :inherit option set in turn.  The inheritance
    ;; tree (it shouldn't be a graph) is traverse in depth first.
    (unless value
      (let ((projs (if is-subproject
		       (list (nth 1 project))
		     (cadr (member :inheritance options))))
	    proj)
	(when projs
	  (while (and (not value) (setq proj (pop projs)))
	    ;; #### FIXME: what happens if we inherit from something like a
	    ;; #### subproject which is unrelated to the current project ?
	    (setq value (patcher-project-option-1 (assoc proj patcher-projects)
						  option (1+ level)))))
	))
    ;; Now some checkings.
    (when (and (eq option :files) value)
      (if is-subproject
	  ;; Return the files as a string, not as the original list.
	  (setq value (list :files (mapconcat #'identity (cadr value) " ")))
	;; #### NOTE: we don't normally check other user-level errors (like,
	;; #### only projects can have an :inheritance option above).
	;; #### However, that case is special: we have some blind calls to
	;; #### `patcher-project-option' that could get an illegal :files
	;; #### options  from illegal projects.  These calls are supposed to
	;; #### return `nil' as a  result, so we perform the checking.
	(patcher-error "Project `%s': only subprojects can have a :file option"
		       (patcher-project-patcher-name project))
	(setq value nil)))
    value))

(defun patcher-project-option (project option)
  ;; Returns either a project's option, or the patcher-default-* value.
  (let ((opt (patcher-project-option-1 project option 0)))
    (if opt
	(cadr opt)
      (symbol-value
       (intern-soft
	(concat "patcher-default-" (substring (symbol-name option) 1))))
      )))
(put 'patcher-project-option 'lisp-indent-function 1)

(defsubst patcher-project-name (project)
  (let ((name (patcher-project-option project :name)))
    (or name (patcher-project-patcher-name project))
    ))

(defun patcher-project-directory (project)
  ;; Returns the project directory of PROJECT, possibly expanded as a project
  ;; subdir if PROJECT is a subproject.
  (if (patcher-subproject-p project)
      (let ((prj (assoc (nth 1 project) patcher-projects)))
	(unless prj
	  (patcher-error "Can't find base project for subproject `%s'"
			 (patcher-project-patcher-name project)))
	(let ((subdir (patcher-project-option project :subdirectory)))
	  (if subdir
	      (expand-file-name subdir (patcher-project-directory prj))
	    (patcher-project-directory prj))))
    ;; else: (member project patcher-projects)
    (nth 1 project)))


;; ===========================================================================
;; Internal utilities
;; ===========================================================================

;; #### NOTE: this is currently useless.
(defvar patcher-instances nil
  ;; A list of all alive instances of Patcher (an instance is dead after the
  ;; mail has been sent.  Each element is of the form '(BUFFER_NAME . BUFFER).
  )

(defconst patcher-change-log-entry-start-regexp
  "^[0-9]\\{4,4\\}-[0-9]\\{2,2\\}-[0-9]\\{2,2\\} "
  ;; Regexp matching the beginning of a ChangeLog entry
  )

;; Buffer local variables ====================================================

;; The following variables get local values in various Patcher buffers (mail
;; buffer, process output buffer etc).

(make-variable-buffer-local
 (defvar patcher-project nil
   ;; Patcher project related to the current patch.  This is also set in
   ;; auxiliary buffers.
   ))

(make-variable-buffer-local
 (defvar patcher-mail-buffer nil
   ;; Mail buffer corresponding to Patcher auxiliary buffers.
   ))


;; Utility functions =========================================================

;;(defun patcher-keyword-value (keyword values)
;;  ;; Return the value of KEYWORD from a (KEY VAL ...) list.  VAL may be omitted
;;  ;; in the list, in which case t is returned.
;;  (let ((vals values)
;;	key val)
;;    (catch 'found
;;      (while (setq key (pop vals))
;;	(setq val (or (not (car vals))
;;		      (if (keywordp (car vals)) t (pop vals))))
;;	(and (eq keyword key)
;;	     (throw 'found val))))
;;    ))


(defsubst patcher-substitute-name (project str)
  ;; Replace a %N in string STR with the current project's name.
  ;; Replace a %n in string STR with the value of :name, if set, and with the
  ;; current project's name otherwise.
  (let ((name (patcher-project-name project))
	(patcher-name (patcher-project-patcher-name project))
	case-fold-search)
    (replace-in-string (replace-in-string str "%N" patcher-name)
		       "%n" name)
    ))

(defun patcher-command (project command &optional files)
  ;; Build a Patcher command from COMMAND that applies to FILES.
  ;; This involves %n and %f substitution, and :pre-command handling.

  ;; 1/ %n substitution:
  (setq command (patcher-substitute-name project command))

  ;; 2/ Force Unix syntax, and replace a %f in COMMAND with the files
  ;; (if any) converted to a string, or append it to the end of STR.
  (setq files (if files
		  (replace-in-string (mapconcat #'identity files " ")
				     "\\\\" "/")
		""))
  (cond ((string-match "%f" command)
	 (setq command (replace-in-string
			(replace-in-string command "%f" files)
			"[ \t]+$" "")))
	((> (length files) 0)
	 (setq command (concat command " " files)))
	)

  ;; 3/ Prepend the :pre-command option to COMMAND, if any.
  (let ((precmd (patcher-project-option project :pre-command)))
    (when (> (length precmd) 0)
      (setq command (concat precmd " " command))
      ))
  command)

(defsubst patcher-call-process (command &optional buffer)
  ;; Call a shell process which executes COMMAND (a string) with output to
  ;; BUFFER (the current buffer by default).
  (apply 'call-process shell-file-name nil (or buffer t) nil
	 shell-command-switch (list command)))

(defsubst patcher-extent (property &optional value buffer)
  ;; Get the extent that has PROPERTY set to VALUE (t if not given) in BUFFER
  ;; (current buffer if nil).
  (car (mapcar-extents #'identity nil buffer nil nil nil
		       property (or value t))))

(defun patcher-process-output-buffer (&optional mail-buffer)
  ;; Get a process output buffer for the current Patcher MAIL-BUFFER (current
  ;; buffer by default), and prepare it.  We can reuse an already existing one
  ;; because auxiliary buffers are currently used only in one Lisp shot, so
  ;; there's no risk of Patcher instances overlapping.
  (let ((project patcher-project)
	(directory default-directory)
	(buffer (get-buffer-create " *Patcher Process Output*")))
    (or mail-buffer (setq mail-buffer (current-buffer)))
    (with-current-buffer buffer
      (cd directory)
      (setq patcher-project project)
      (setq patcher-mail-buffer mail-buffer)
      (erase-buffer))
    buffer))



;; ==========================================================================
;; ChangeLog buffers
;; ==========================================================================

(defun patcher-read-natnum (prompt &optional default-value)
  ;; Hacked from read-number
  ;; Read a natural number from the minibuffer, prompting with PROMPT.
  ;; If optional second argument DEFAULT-VALUE is non-nil, return that if user
  ;; enters an empty line.
  (let ((pred (lambda (val) (and (integerp val) (> val 0))))
	num)
    (while (not (funcall pred num))
      (setq num (condition-case ()
		    (let ((minibuffer-completion-table nil))
		      (read-from-minibuffer
		       prompt (if num (prin1-to-string num)) nil t
		       nil nil (and default-value
				    (prin1-to-string default-value))))
		  (input-error nil)
		  (invalid-read-syntax nil)
		  (end-of-file nil)))
      (or (funcall pred num) (beep)))
    num))

(defun patcher-change-log-extent (change-log mail)
  ;; Return (maybe after creating it) the extent in buffer CHANGE-LOG which
  ;; has the 'patcher property set to the buffer MAIL.
  (let ((extent (patcher-extent 'patcher mail change-log)))
    (unless extent
      (save-window-excursion
	(display-buffer change-log t)
	(let ((entries (patcher-read-natnum "Number of entries (1): " 1))
	      beg end)
	  (save-excursion
	    (set-buffer change-log)
	    (save-restriction
	      (widen)
	      (goto-char (point-min))
	      (skip-chars-forward " \n\t")
	      (unless (looking-at patcher-change-log-entry-start-regexp)
		(patcher-error "\
Beginning of buffer doesn't look like a ChangeLog entry."))
	      (setq beg (point))
	      (condition-case nil
		  (while (> entries 0)
		    (re-search-forward patcher-change-log-entry-start-regexp)
		    (setq entries (1- entries)))
		(t
		 (patcher-error "\
Buffer is missing %s ChangeLog entr%s to do the count."
				entries (if (= entries 1) "y" "ies"))))
	      (setq end
		    (or (and (re-search-forward
			      patcher-change-log-entry-start-regexp nil t)
			     (progn (beginning-of-line) (point)))
			(point-max)))
	      (set-extent-properties (setq extent (make-extent beg end))
		`(patcher ,mail))
	      ))
	  )))
    extent))



;; ==========================================================================
;; The LogMsg buffer
;; ==========================================================================

(make-variable-buffer-local
 (defvar patcher-logmsg-file-name nil
   ;; Name of the temporary file where the log message is stored.
   ))

(make-variable-buffer-local
 (defvar patcher-logmsg-commit-command
   ;; Commit command used for the current Patcher LogMsg buffer.  This variable
   ;; is needed because the user has the ability to override the command with
   ;; a prefix argument.
   ))

(defun patcher-logmsg-compress-change-logs ()
  ;; Compress ChangeLog entries appearing in the current buffer between FROM
  ;; and TO.  This function compresses the output into something that conveys
  ;; the essence of what has been changed, but much more compactly.
  (save-excursion
    (goto-char (point-min))
    (let ((prologue (patcher-project-option patcher-project
		      :change-logs-prologue)))
      (when (> (length prologue) 0)
	(setq prologue (concat
			"^"
			(replace-in-string
			 (regexp-quote prologue) "%f" ".+")
			"$"))
	(delete-matching-lines prologue)))
    (delete-matching-lines patcher-change-log-entry-start-regexp)
    ;; Now compress the change log specs into just files, so that mostly just
    ;; the annotations are left.
    (let ((change-log-change-line
	   "^\\([ \t]+\\)\\* \\(\\S-+\\)\\( (.*)\\)?:\\( New\\.\\)?"))
      (while (re-search-forward change-log-change-line nil t)
	(let ((beg (match-beginning 1));; Change to match-end if you want the
	      ;; indentation.
	      (end (match-end 0))
	      files)
	  (push (match-string 2) files)
	  (forward-line 1)
	  (while (looking-at change-log-change-line)
	    (setq end (match-end 0))
	    (unless (member (match-string 2) files)
	      (push (match-string 2) files))
	    (forward-line 1))
	  (goto-char beg)
	  (delete-region beg end)
	  (insert (mapconcat 'identity (nreverse files) ", ") ":")
	  (when (looking-at "\\s-+")
	    (let ((p (point))
		  (end (match-end 0)))
	      ;; If there's no annotation at all for this change, make sure we
	      ;; don't treat the next change as an annotation for this one!
	      (if (save-excursion
		    (goto-char end)
		    (beginning-of-line)
		    (looking-at change-log-change-line))
		  (progn
		    (if (looking-at "[ \t]+")
			(delete-region p (match-end 0))))
		(delete-region p end)
		(insert " "))))
	  )))
    ;; Shrink extra blank lines.
    (let ((blank-line "^\\s-*$"))
      (goto-char (point-min))
      (while (and (not (eobp))
		  (progn (forward-line 1)
			 (re-search-forward blank-line nil t)))
	(delete-blank-lines))
      (goto-char (point-min))
      (if (looking-at blank-line)
	  (delete-blank-lines)))
    ))


;; Patcher LogMsg mode ======================================================

(defun patcher-logmsg-insert-subject ()
  "Insert the Patcher mail subject into the current LogMsg buffer at point."
  (interactive)
  (let ((subject "(none)"))
    (with-current-buffer patcher-mail-buffer
      (save-excursion
	(let ((extent (patcher-extent 'patcher-subject-prefix)))
	  (if extent
	      (progn
		(goto-char (extent-end-position extent))
		(skip-chars-forward " \t\f\r")
		(unless (eq (point) (point-at-eol))
		  (setq subject
			(buffer-substring (point) (point-at-eol)))))
	    (goto-char (point-min))
	    (when (patcher-goto-subject)
	      (skip-chars-forward " \t\f\r")
	      (unless (eq (point) (point-at-eol))
		(setq subject
		      (buffer-substring (point) (point-at-eol))))))
	  )))
    (let ((doit (> (length subject) 0)))
      (when doit (insert subject))
      doit)
    ))

(defun patcher-logmsg-insert-change-logs (&optional separator)
  "Insert ChangeLog entries in the current Patcher LogMsg buffer at point.
When called interactively, use a prefix argument to also insert the
ChangeLogs separator string defined by the :change-logs-separator project
option."
  (interactive "P")
  (unless (point-at-bol)
    (insert "\n"))
  (when separator
    (setq separator (patcher-project-option patcher-project
		      :change-logs-separator))
    (when (> (length separator) 0)
      (insert separator "\n\n")))
  (let ((prologue (patcher-project-option patcher-project
		    :change-logs-prologue)))
    (dolist (change-log (patcher-files-buffers
			 (symbol-value-in-buffer 'patcher-change-logs
						 patcher-mail-buffer)
			 'find))
      (when (> (length prologue) 0)
	(insert (replace-in-string prologue "%f"
				   (patcher-file-relative-name
				    (buffer-file-name change-log)))
		    "\n\n"))
      (insert (extent-string
	       ;; #### NOTE: there is an empty line at the end of this extent.
	       (patcher-change-log-extent change-log patcher-mail-buffer))))
    )
  ;; -2 is because 1/ there's a spurious empty line inserted after the last
  ;; element, and 2/ because we don't want automatic newlines at the end of
  ;; the buffer, in case log messages are used as strings (for instance with
  ;; PRCS).
  (delete-char -2))

(defun patcher-logmsg-insert-compressed-change-logs ()
  "Insert compressed ChangeLog entries in the current Patcher LogMsg buffer."
  (interactive)
  (let ((beg (point)))
    (patcher-logmsg-insert-change-logs)
    (narrow-to-region beg (point))
    (patcher-logmsg-compress-change-logs)
    (widen)
    ))


;; #### NOTE: This should be defined in the Mail Buffer section, but defining
;; #### it here avoids a compiler warning.
(make-variable-buffer-local
 (defvar patcher-change-committed nil
   ;; Boolean indicating whether the change has been committed already.
   ))

(defun patcher-logmsg-commit (&optional arg)
  "Commit the change described in the current Patcher LogMsg buffer.
When called interactively, use a prefix to override the commit command."
  (interactive "P")
  (let ((output-buffer (patcher-process-output-buffer patcher-mail-buffer))
	(log-buffer (current-buffer))
	runbuf
	(change-logs (symbol-value-in-buffer 'patcher-change-logs
					     patcher-mail-buffer))
	(sources (symbol-value-in-buffer 'patcher-sources
					 patcher-mail-buffer))
	(pre-commit-window-config (symbol-value-in-buffer
				   'patcher-pre-commit-window-config
				   patcher-mail-buffer))
	(confirm-commits (patcher-project-option patcher-project
			   :confirm-commits)))
    (patcher-save-buffers (patcher-files-buffers change-logs))
    (and arg (setq patcher-logmsg-commit-command
		   (read-shell-command "Commit command: "
				       patcher-logmsg-commit-command)))
    (let* ((command
	    (patcher-command
	     patcher-project
	     (replace-in-string
	      (let (case-fold-search)
		(replace-in-string patcher-logmsg-commit-command
				   "%S" (shell-quote-argument
					 (buffer-substring)) t))
	      "%s" patcher-logmsg-file-name t)
	     (when sources
	       (append (mapcar #'patcher-file-relative-name change-logs)
		       sources)))))
      ;; Maybe display the commit command, and make sure the user agrees.
      (when (or (not confirm-commits)
		(save-window-excursion
		  (setq runbuf (get-buffer-create
				"*Patcher Commit Command*"))
		  (erase-buffer runbuf)
		  (insert-string (format "Command to run:\n\n%s" command)
				 runbuf)
		  (display-buffer runbuf)
		  (y-or-n-p "Run commit command? ")))
	;; Write out the log message, or "(none)"
	(and (= (point-min) (point-max)) (insert "(none)"))
	(write-region (point-min) (point-max) patcher-logmsg-file-name
		      nil 'silent)
	(patcher-with-progression "Committing changes"
	  (patcher-call-process command output-buffer))
	;; Don't kill the log message buffer.  This will be done after sending
	;; the message -- i.e. when we are done with this project.  We don't
	;; kill the log message buffer now in case the user needs it later --
	;; e.g. if the commit failed and needs to be redone (we try to detect
	;; this, but we might not succeed in all cases.).
	;; Try to see if the commit failed.
	(with-current-buffer output-buffer
	  (goto-char (point-min))
	  (when (re-search-forward (patcher-project-option patcher-project
				     :failed-command-regexp) nil t)
	    (display-buffer output-buffer t)
	    (with-current-buffer log-buffer
	      ;; make sure substitute-command-keys is run in the right buffer
	      (patcher-error "\
Error during commit.  Please fix the problem and type \
\\[patcher-logmsg-commit] to try again."))))
	;; Otherwise, record the successful commit in the mail message.
	;; #### NOTE: it is normal to protect the re-search-forward calls
	;; #### against errors, because when the `fake mail' method is used,
	;; #### neither the Subject line nore the mail-header-separator one
	;; #### exist.
	(with-current-buffer patcher-mail-buffer
	  (setq patcher-change-committed t)
	  (save-excursion
	    ;; Possibly change the subject:
	    (goto-char (point-min))
	    (when (patcher-goto-subject)
	      (let ((subject-committed-prefix
		     (patcher-project-option patcher-project
		       :subject-committed-prefix))
		    (extent (patcher-extent 'patcher-subject-prefix)))
		(when subject-committed-prefix
		  (setq subject-committed-prefix
			(patcher-substitute-name patcher-project
						 subject-committed-prefix))
		  (when extent
		    (goto-char (extent-start-position extent))
		    (delete-region (point) (extent-end-position extent)))
		  (insert subject-committed-prefix)
		  (and (looking-at "\\S-") (insert " ")))
		))
	    ;; Insert the `committed' notice:
	    (goto-char (point-min))
	    (when (re-search-forward
		   (concat "^" (regexp-quote mail-header-separator))
		   nil t)
	      (forward-line 1)
	      (let ((notice (patcher-project-option patcher-project
			      :committed-notice)))
		(when (> (length notice) 0)
		  (insert notice "\n"))))
	    ))
	;; Bury the log message (see above).  Remove the log message window
	;; and display the output buffer.
	(bury-buffer log-buffer)
	(set-window-configuration pre-commit-window-config)
	(display-buffer output-buffer t))
      (and runbuf (bury-buffer runbuf))
      )))

(defun patcher-logmsg-init-message ()
  "(Re)Init the log message in the current Patcher LogMsg buffer.
This is done conforming to the :log-message-items project option."
  (interactive)
  (erase-buffer)
  (let ((items (patcher-project-option patcher-project :log-message-items))
	(edit-log-message (patcher-project-option patcher-project
			    :edit-log-message))
	inserted)
    (dolist (item items)
      (cond ((eq item 'subject)
	     (when inserted (insert "\n\n"))
	     (setq inserted (patcher-logmsg-insert-subject)))
	    ((eq item 'compressed-change-logs)
	     (when inserted (insert "\n\n"))
	     (patcher-logmsg-insert-compressed-change-logs)
	     (setq inserted t))
	    ((eq item 'change-logs)
	     (when inserted (insert "\n\n"))
	     (patcher-logmsg-insert-change-logs inserted))
	    (t
	     (patcher-error "invalid log message item: %s" item))))
    (goto-char (point-min))
    (if edit-log-message
	(patcher-message "\
Edit the log message, and press \\[patcher-logmsg-commit] when done.")
      (patcher-logmsg-commit))
    ))

(defcustom patcher-logmsg-mode-hook nil
  "*Hook to run after setting up Patcher-Logmsg mode."
  :group 'patcher
  :type 'hook)

(defvar patcher-logmsg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) (control p) s] 'patcher-logmsg-insert-subject)
    (define-key map [(control c) (control p) l]
      'patcher-logmsg-insert-change-logs)
    (define-key map [(control c) (control p) c]
      'patcher-logmsg-insert-compressed-change-logs)
    (define-key map [(control c) (control p) i] 'patcher-logmsg-init-message)
    (define-key map [(control c) (control c)] 'patcher-logmsg-commit)
    map))

(defun patcher-logmsg-mode ()
  "Major mode for Patcher commit log message management.
You're not supposed to use this mode manually, unless you know what you're
doing.

\\{patcher-logmsg-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'patcher-logmsg)
  (setq mode-name "Patcher-LogMsg")
  (use-local-map patcher-logmsg-mode-map)
  (run-hooks 'patcher-logmsg-mode-hook))



;; ===========================================================================
;; The Patcher mail buffer
;; ===========================================================================

(make-variable-buffer-local
 (defvar patcher-diff-marker nil
   ;; Marker indicating the beginning of the diff.
   ))

(make-variable-buffer-local
 (defvar patcher-diff-command nil
   ;; String containing the diff command to use.  This string is not supposed
   ;; to include the files to which the command applies.  Only the command
   ;; itself.  This variable is needed because the user has the ability to
   ;; override the project's command by giving a prefix to
   ;; `patcher-generate-diff'.
   ))

(make-variable-buffer-local
 (defvar patcher-sources nil
   ;; List of files/directories command-line specification for the diff
   ;; command.  This variable is needed because the user has the ability to
   ;; override the project's files by calling `patcher-mail-subproject'
   ;; instead of `patcher-mail'.
   ))

(make-variable-buffer-local
 (defvar patcher-change-logs-marker nil
   ;; Marker indicating the beginning of the ChangeLog entries, when they are
   ;; separated from the patch.
   ))

(make-variable-buffer-local
 (defvar patcher-change-logs nil
   ;; List of ChangeLog absolute file names.  This is computed after the
   ;; initial diff by `patcher-diff-base'.  Each element is a list of the form
   ;; (FILENAME . LOADED). LOADED is a boolean indicating whether we loaded
   ;; the file ourselves, and hence can kill it once we're done.
   ))

(make-variable-buffer-local
 (defvar patcher-pre-commit-window-config nil
   ;; Window configuration, just prior to beginning a commit operation, so we
   ;; can get back to it at the appropriate time later.
   ))

(make-variable-buffer-local
 (defvar patcher-logmsg-buffer nil
   ;; Buffer containing the commit log message of the current Patcher mail.
   ;; This buffer is not killed after the commit operation, but should when
   ;; the message is sent.
   ))

(defmacro patcher-with-information (information &rest body)
  `(save-window-excursion
     (save-excursion
       (with-output-to-temp-buffer
	   " *Patcher Information*"
	 (set-buffer " *Patcher Information*")
	 (insert ,information))
       ,@body)))
(put 'patcher-with-information 'lisp-indent-function 1)

(defsubst patcher-delete-extent-and-region (extent)
  ;; Delete EXTENT and the corresponding region.
  (when extent
    (delete-region (extent-start-position extent) (extent-end-position extent)
		   (extent-object extent))
    (delete-extent extent)
    ))

(defun patcher-parse-region (&optional min max buffer)
  ;; Parse a diff output between MIN and MAX in BUFFER.  Defaults to point min,
  ;; point max and current buffer respectively.
  ;; For each diffed file, create an extent with the following properties:
  ;; 'patcher-change-log = <absolute filename> for ChangeLog files.
  ;; 'patcher-source = <absolute filename> for source files.
  ;; Return non nil if an error occured.
  (with-current-buffer (or buffer (current-buffer))
    (let ((file-re1 "^Index: \\(\\S-*\\)");; for archive  diff
	  (file-re "^\\+\\+\\+ \\(\\S-*\\)");; for standard diff
	  (basename-re "\\`\\(.*\\)/\\(.*\\)\\'")
	  (min (or min (point-min)))
	  (max (or max (point-max)))
	  change-log file absfile dirname beg end)
      (save-excursion
	(goto-char min)
	(save-excursion
	  (and (re-search-forward file-re1 max t)
	       (setq file-re file-re1)))
	(while (re-search-forward file-re max t)
	  (setq file (match-string 1))
	  (if (string-match basename-re file)
	      (setq dirname  (match-string 1 file))
	    (setq dirname ""))
	  (setq absfile (expand-file-name file default-directory))
	  (setq beg (point-at-bol))
	  (setq end (or (save-excursion
			  (and (re-search-forward file-re max t)
			       (point-at-bol)))
			max))
	  (setq change-log
		(with-temp-buffer
		  (cd (expand-file-name dirname default-directory))
		  (find-change-log)))
	  (let ((extent (make-extent beg end)))
	    (set-extent-properties extent '(start-open t duplicable t))
	    (set-extent-property extent
				 (if (string= change-log absfile)
				     'patcher-change-log
				   'patcher-source)
				 absfile)))
	(goto-char min)
	(re-search-forward (patcher-project-option patcher-project
			     :failed-command-regexp)
			   max t))
      )))

(defun patcher-generate-change-logs (&optional min max buffer)
  ;; Generate ChangeLog skeletons based on the diff between MIN and MAX in
  ;; BUFFER.  Defaults to point min, point max and current buffer respectively.
  ;; Check `patcher-mail-buffer' first because if that is non nil, we're
  ;; in an auxiliary buffer.  Otherwise, we're in a Patcher mail one.
  (let ((mailbuf (or patcher-mail-buffer (current-buffer))))
    (with-current-buffer (or buffer (current-buffer))
      (patcher-with-progression "Generating ChangeLog skeletons"
	(save-restriction
	  (require 'add-log)
	  (narrow-to-region (or min (point-min)) (or max (point-max)))
	  (patch-to-change-log
	   default-directory
	   :my-name (patcher-project-option patcher-project
					    :change-logs-user-name)
	   :my-email (patcher-project-option patcher-project
					     :change-logs-user-mail)
	   :keep-source-files
	   (not (patcher-project-option patcher-project
					:kill-source-files-after-diffing))
	   :extent-property 'patcher
	   :extent-property-value mailbuf)))
      ;; patch-to-change-log has the unfortunate side effect of burying all
      ;; the ChangeLog buffers when it's done.  This is exactly the opposite of
      ;; what we want, since once the ChangeLogs have been generated, the next
      ;; step is to go visit them.  so put them (in order!) directly below the
      ;; current buffer.
      (let ((topbuf (car (buffer-list))))
	(dolist (x (patcher-files-buffers
		    (symbol-value-in-buffer 'patcher-change-logs mailbuf)))
	  (bury-buffer x topbuf)
	  ;; window-start ends up past the newly inserted entry, so fix that.
	  (with-current-buffer x
	    (let ((ex (patcher-extent 'patcher mailbuf)))
	      (and ex (goto-char (extent-start-position ex))))))
	(bury-buffer topbuf (car (buffer-list))))
      )))

(defun patcher-ungenerate-change-logs ()
  ;; Delete ChangeLog skeletons created by a former call to
  ;; `patcher-generate-change-logs', in the current Patcher mail buffer.
  (dolist (change-log (patcher-files-buffers patcher-change-logs 'find))
    (patcher-delete-extent-and-region
     (patcher-change-log-extent change-log (current-buffer)))
    (with-current-buffer change-log (save-buffer))))

(defmacro patcher-map-change-log-extents (&optional buffer &rest body)
  ;; Map BODY over all extents marking a ChangeLog contents in BUFFER.
  `(mapcar-extents
    (lambda (extent) ,@body)
    nil (or ,buffer (current-buffer)) nil nil nil 'patcher-change-log))
(put 'patcher-map-change-log-extents 'lisp-indent-function 1)

(defmacro patcher-map-source-extents (&optional buffer &rest body)
  ;; Map BODY over all extents marking a source contents in BUFFER.
  `(mapcar-extents
    (lambda (extent) ,@body)
    nil (or ,buffer (current-buffer)) nil nil nil 'patcher-source))
(put 'patcher-map-source-extents 'lisp-indent-function 1)

(defun patcher-change-logs (&optional buffer)
  ;; Return the list of ChangeLog absolute file names appearing in BUFFER
  ;; (current buffer by default).
  (let (change-logs)
    (patcher-map-change-log-extents buffer
      (let ((change-log (extent-property extent 'patcher-change-log)))
	(push change-log change-logs)))
    change-logs))

(defun patcher-sources (&optional buffer)
  ;; Return the list of source absolute file names appearing in BUFFER
  ;; (current buffer by default).
  (let (sources)
    (patcher-map-source-extents buffer
      (let ((source (extent-property extent 'patcher-source)))
	(push source sources)))
    sources))

(defun patcher-remove-change-logs (&optional buffer)
  ;; Remove ChangeLog contents from BUFFER (current buffer by default).
  (patcher-with-progression "Removing ChangeLog contents"
    (patcher-map-change-log-extents buffer
      (patcher-delete-extent-and-region extent))))

(patcher-globally-declare-boundp
 '(name source-diff source-files change-log-diff change-log-files))

(defun patcher-default-diff-prologue (kind)
  ;; Default function for inserting a diff prologue.
  (cond ((eq kind 'sources)
	 (insert name " source patch:\n"
		 "Diff command:   " source-diff "\n"
		 "Files affected: " source-files "\n"
		 "\n")
	 )
	((eq kind 'change-logs)
	 (insert name " ChangeLog patch:\n"
		 "Diff command:   " change-log-diff "\n"
		 "Files affected: " change-log-files "\n"
		 "\n")
	 )
	((eq kind 'mixed)
	 (insert name " patch:\n")
	 (if (not change-log-diff)
	     (insert "Diff command:             " source-diff "\n"
		     "ChangeLog files affected: " change-log-files "\n"
		     "Source files affected:    " source-files "\n")
	   (insert "ChangeLog files diff command: " change-log-diff "\n"
		   "Files affected:               " change-log-files "\n"
		   "Source files diff command:    " source-diff "\n"
		   "Files affected:               " source-files "\n"))
	 (insert "\n")
	 )
	))

(defun patcher-insert-diff (buffer)
  ;; Insert the diff created in auxiliary BUFFER, and create the patcher-diff
  ;; extent.  This function also filters out lines specified by the
  ;; diff-line-filter project option.
  (save-excursion
    (goto-char patcher-diff-marker)
    (let ((font-lock-always-fontify-immediately t)
	  (pos (with-current-buffer buffer (goto-char (point-min))))
	  (diff-line-filter
	   (patcher-project-option patcher-project :diff-line-filter)))
      (when diff-line-filter
	(setq diff-line-filter (concat "^" diff-line-filter "\n"))
	(while (re-search-forward diff-line-filter nil t nil buffer)
	  (insert (buffer-substring pos (match-beginning 0) buffer))
	  (setq pos (point buffer))))
      (insert (buffer-substring pos nil buffer)))
    (set-extent-properties (make-extent patcher-diff-marker (point))
      '(start-open t patcher-diff t))
    ))

(defun patcher-prcs-diff-convert (&optional beg end)
  "Patcher post-processor for PRCS diffs.
This function removes the PRCS repository-specific path in front of
filenames to turn the output into a standard diff output.
This function is meant to be used as part of the :after-diff-hook
project option"
  (goto-char (or beg (point-min)))
  (while (re-search-forward "^\\(Index:\\|---\\|\\+\+\\+\\) " end t)
    (setq beg (point))
    (skip-chars-forward "^/")
    (forward-char 1)
    (delete-region beg (point)))
  )

(defun patcher-run-after-diff-hook (buffer &optional beg end)
  ;; If any, call the after-diff hooks on BUFFER (auxiliary or mail
  ;; buffer), possibly limiting to the region (BEG END).
  ;; #### NOTE: remember that patcher-projects is also set in auxiliary
  ;; #### buffers.
  (with-current-buffer buffer
    (let ((after-diff-hook (patcher-project-option patcher-project
			    :after-diff-hook)))
      (when after-diff-hook
	(patcher-with-progression "Running after diff hooks"
	  (save-excursion
	    (mapcar (lambda (func)
		      (goto-char (point-min))
		      (funcall func beg end))
		    after-diff-hook)))))
    ))

(defun patcher-diff-all ()
  ;; Create a global diff with both ChangeLogs and given files, insert it in
  ;; the current Patcher mail buffer at the patcher-diff-marker position if it
  ;; succeeded, and create the patcher-diff extent.
  (patcher-save-buffers (patcher-files-buffers patcher-change-logs))
  (let ((command
	 (patcher-command patcher-project patcher-diff-command
			  (when patcher-sources
			    (append (mapcar #'patcher-file-relative-name
					    patcher-change-logs)
				    patcher-sources))))
	(buffer (patcher-process-output-buffer)))
    (patcher-with-progression "Generating global diff"
      (patcher-call-process command buffer))
    (patcher-run-after-diff-hook buffer)
    (when (patcher-parse-region nil nil buffer)
      (display-buffer buffer t)
      (patcher-error "\
Error during diff.  Please fix the problem and type \
\\[patcher-generate-diff] to try again."))
    (patcher-insert-diff buffer)
    ))

(defun patcher-insert-change-logs-verbatim ()
  ;; Insert ChangeLog contents verbatim in the current Patcher mail buffer,
  ;; and create the patcher-change-logs extent.
  (let ((prologue
	 (patcher-project-option patcher-project :change-logs-prologue)))
    (patcher-with-progression "Inserting ChangeLog contents"
      (save-excursion
	(goto-char patcher-change-logs-marker)
	(dolist (change-log (patcher-files-buffers patcher-change-logs 'find))
	  (let ((extent
		 (patcher-change-log-extent change-log (current-buffer)))
		(beg (point)))
	    (insert "\n")
	    (when (> (length prologue) 0)
	      (insert (replace-in-string prologue "%f"
					 (patcher-file-relative-name
					  (buffer-file-name change-log)))
		      "\n\n"))
	    (insert (extent-string extent))
	    (set-extent-properties (make-extent beg (point))
	      `(start-open t patcher-change-log
			   ,(buffer-file-name change-log)))))
	(set-extent-properties (make-extent patcher-change-logs-marker (point))
	  '(start-open t patcher-change-logs t))))
    ))

(defun patcher-insert-change-logs-diff-prologue (command)
  ;; Insert a ChangeLog diff prologue at point in current Patcher mail buffer.
  (let ((function (patcher-project-option patcher-project
		    :diff-prologue-function)))
    (when function
      (let ((name (patcher-project-name patcher-project))
	    (change-log-files (patcher-files-string patcher-change-logs))
	    (change-log-diff (patcher-command patcher-project command)))
	(funcall function 'change-logs))
      )))

(defun patcher-diff-change-logs (command)
  ;; Create a diff with only ChangeLogs, insert it in the current Patcher mail
  ;; buffer at the patcher-change-logs-marker position if it succeeded, and
  ;; create the patcher-change-logs extent.
  (patcher-save-buffers (patcher-files-buffers patcher-change-logs))
  (let ((buffer (patcher-process-output-buffer)))
    (patcher-with-progression "Generating the ChangeLogs diff"
      (patcher-call-process
       (patcher-command patcher-project command
			(mapcar #'patcher-file-relative-name
				patcher-change-logs))
       buffer))
    (patcher-run-after-diff-hook buffer)
    (when (patcher-parse-region nil nil buffer)
      (display-buffer buffer t)
      (patcher-error "\
Error during diff.  Please fix the problem and type \
\\[patcher-insert-change-logs] to try again."))
    ;; #### FIXME: maybe check that all changelogs are diff'ed (meaning the
    ;; #### user has not forgotten to update one of them).
    (save-excursion
      (goto-char patcher-change-logs-marker)
      (patcher-insert-change-logs-diff-prologue command)
      (let ((font-lock-always-fontify-immediately t))
	(insert (buffer-substring nil nil buffer)))
      (set-extent-properties (make-extent patcher-change-logs-marker (point))
	'(start-open t patcher-change-logs t)))
    ))

(defun patcher-pack-change-logs ()
  ;; Pack ChangeLog diffs to the change-logs marker in the current Patcher
  ;; mail buffer, and create the patcher-change-logs extent.
  (patcher-with-progression "Packing ChangeLog diffs"
    (save-excursion
      (goto-char patcher-change-logs-marker)
      (patcher-insert-change-logs-diff-prologue patcher-diff-command)
      (patcher-map-change-log-extents nil
	(let ((contents (extent-string extent))
	      (change-log (extent-property extent 'patcher-change-log))
	      (beg (point)))
	  (patcher-delete-extent-and-region extent)
	  (insert contents)
	  (set-extent-properties (make-extent beg (point))
	    `(start-open t patcher-change-log ,change-log))))
      (set-extent-properties (make-extent patcher-change-logs-marker (point))
	`(start-open t patcher-change-logs t)))
    ))

(defun patcher-extent-error (extent)
  ;; Look for an error in EXTENT.
  ;; Update the 'patcher-error property as needed.
  ;; Return 0 if status is unchanged, 1 if an error appeared, -1 if an error
  ;; was fixed.
  (let* ((old-error (if (extent-property extent 'patcher-error) 1 0))
	 (new-error (if (save-excursion
			  (goto-char (extent-start-position extent))
			  (re-search-forward
			   (patcher-project-option patcher-project
			     :failed-command-regexp)
			   (extent-end-position extent) t))
			1 0))
	 (error (- new-error old-error)))
    (cond ((eq error 1)
	   (set-extent-property extent 'patcher-error t))
	  ((eq error -1)
	   (set-extent-property extent 'patcher-error nil)))
    error))

(defun patcher-convert-change-log-diffs (command)
  ;; Scan all ChangeLog diffs in the current Patcher mail buffer, and remake
  ;; them one by one with the proper diff COMMAND, directly in place.
  (save-excursion
    (let ((diff-extent (patcher-extent 'patcher-diff))
	  (errors 0)
	  change-log beg end)
      ;; #### Don't forget to start-close the diff extent !! A ChangeLog could
      ;; #### appear at the beginning of the diff.
      (set-extent-property diff-extent 'start-open nil)
      (patcher-with-progression "Regenerating ChangeLog diffs"
	(patcher-map-change-log-extents nil
	  ;; #### WARNING: it seems that if I modify the extent contents here,
	  ;; #### instead of deleting and recreating it, map(car)-extents goes
	  ;; #### into an infinite loop, on all extents over and over again.
	  (setq change-log (extent-property extent 'patcher-change-log))
	  (goto-char (extent-start-position extent))
	  (setq beg (point-marker))
	  (patcher-delete-extent-and-region extent)
	  (patcher-call-process
	   (patcher-command patcher-project command
			    (list (patcher-file-relative-name change-log))))
	  (setq end (point-marker))
	  (patcher-run-after-diff-hook (current-buffer) beg end)
	  (setq extent (make-extent beg end))
	  (set-extent-properties extent
	    `(start-open t patcher-change-log ,change-log))
	  (setq errors (+ errors (patcher-extent-error extent)))))
      (set-extent-property diff-extent 'start-open t)
      (and (> errors 0)
	   (set-extent-property (patcher-extent 'patcher-diff)
				'patcher-error errors)
	   (patcher-error "\
Problems during diff.  \
Please type \\[patcher-insert-change-logs] to try again."))
      )))

(defun patcher-insert-diff-prologue (command)
  ;; Insert a prologue at the top of the diff in the current Patcher mail
  ;; buffer.
  (let ((function (patcher-project-option patcher-project
		    :diff-prologue-function)))
    (when function
      (let ((extent (patcher-extent 'patcher-diff))
	    (name (patcher-project-name patcher-project))
	    (source-diff
	     (patcher-command patcher-project patcher-diff-command))
	    ;; #### NOTE: maybe passing a list instead of a string would be
	    ;; #### better.  I won't break backward compatibility though, at
	    ;; #### least not before a major release.
	    (source-files (patcher-files-string (patcher-sources)))
	    (change-log-files (patcher-files-string patcher-change-logs))
	    (change-log-diff
	     (and (stringp command)
		  (patcher-command patcher-project command))))
	(set-extent-property extent 'start-open nil)
	(save-excursion
	  (goto-char patcher-diff-marker)
	  (funcall function (if (symbolp command) command 'mixed)))
	(set-extent-property extent 'start-open t)
	))
    ))

(defun patcher-diff-base (buffer)
  ;; Create the initial diff and deduce the ChangeLog files (these files can't
  ;; be deduced from the variable `patcher-sources', even when set, because it
  ;; might contain directory specifications).  No return value.
  (patcher-with-progression "Diff'ing the project"
    (patcher-call-process
     (patcher-command patcher-project patcher-diff-command patcher-sources)
     buffer))
  (patcher-run-after-diff-hook buffer)
  (when (patcher-parse-region nil nil buffer)
    (display-buffer buffer t)
    (patcher-error "\
Error during diff.  \
Please fix the problem and type \\[patcher-generate-diff] to try again."))
  (unless (patcher-sources buffer)
    (patcher-error "Your source files do not differ from the archive."))
  (when (patcher-project-option patcher-project :change-logs-updating)
    (setq patcher-change-logs nil)
    (patcher-map-source-extents buffer
      (let* ((file (extent-property extent 'patcher-source))
	     (change-log (with-temp-buffer
			   (cd (file-name-directory file))
			   (find-change-log))))
	(unless (assoc change-log patcher-change-logs)
	  (push (cons change-log (not (get-file-buffer change-log)))
		patcher-change-logs))))))

(defun patcher-change-logs-diff-error ()
  (patcher-error "\
Patcher has detected a ChangeLog diff.  This can mean two things:

- your project might be out of date (someone else has modified the ChangeLog
  file in the meantime.  You should then update your project before running
  Patcher.

- you have spurious ChangeLog entries.  This happens for instance when you have
  filled the ChangeLogs files manually, but Patcher is supposed to do so (the
  :change-log-updating project option is 'automatic).  You should then clean up
  your ChangeLog file before running Patcher."))

(defun patcher-generate-diff-1 ()
  ;; (Re)Create a diff in the current Patcher mail buffer.
  (let ((buffer (patcher-process-output-buffer))
	(updating (patcher-project-option patcher-project
		    :change-logs-updating))
	(appearance (patcher-project-option patcher-project
		      :change-logs-appearance))
	(regenerate (or (patcher-extent 'patcher-diff) patcher-change-logs)))
    ;; Maybe clean up the place for a new diff.
    (and regenerate
	 (patcher-delete-extent-and-region (patcher-extent 'patcher-diff)))
    (if (not updating)
	;; We don't do ChangeLogs: just (re)diff the project.
	(progn
	  (patcher-diff-base buffer)
	  (patcher-insert-diff buffer)
	  (patcher-insert-diff-prologue 'sources)
	  (patcher-message "\
To commit your changes, type \\[patcher-commit-change]."))
      ;; We do ChangeLogs, so deal with the formatting.
      (cond ((eq updating 'automatic)
	     ;; In the "automatic" case, ChangeLog contents insertion is
	     ;; postponed until the user has edited the skeletons.  If no files
	     ;; were specified, we have a chance to check that the project is
	     ;; up to date: if a ChangeLog appears in the diff, the project
	     ;; needs to be updated first.  Note that this does not catch all
	     ;; cases though.
	     (cond ((or (eq appearance 'verbatim)
			(eq appearance 'packed))
		    (let ((generate-change-logs t)
			  (change-logs-extent
			   (patcher-extent 'patcher-change-logs)))
		      (when regenerate
			(patcher-with-information
			    (format "\
ChangeLog skeletons for this patch have already been generated%s.

If you answer `yes' to the question below, both the diff and the ChangeLog
entries will be regenerated.  This means that current ChangeLog entries will be
lost.  If otherwise your answer is `no', only the diff will be regenerated."
				    (if change-logs-extent " and inserted" ""))
			  (setq generate-change-logs (yes-or-no-p "\
Regenerate ChangeLog skeletons ? ")))
			(when generate-change-logs
			  (patcher-delete-extent-and-region change-logs-extent)
			  (patcher-ungenerate-change-logs)))
		      (patcher-diff-base buffer)
		      (when (if regenerate
				(and generate-change-logs
				     (not patcher-sources)
				     (patcher-change-logs buffer))
			      (and (not patcher-sources)
				   (patcher-change-logs buffer)))
			(patcher-change-logs-diff-error))
		      ;; ChangeLogs appear outside the patch, so we can insert
		      ;; the diff right now, and then generate the skeletons.
		      (patcher-insert-diff buffer)
		      (patcher-insert-diff-prologue 'sources)
		      (if generate-change-logs
			  (progn
			    (patcher-generate-change-logs patcher-diff-marker
							  (extent-end-position
							   (patcher-extent
							    'patcher-diff)))
			    (patcher-message "\
Please annotate the ChangeLog skeletons, \
and type \\[patcher-insert-change-logs] to %s them."
					     (if (eq appearance 'verbatim)
						 "insert"
					       "diff")))
			;; not generate-change-logs
			(if change-logs-extent
			    (patcher-message "\
To commit your changes, type \\[patcher-commit-change].")
			  (patcher-message "\
Please type \\[patcher-insert-change-logs] to %s the ChangeLogs"
					   (if (eq appearance 'verbatim)
					       "insert"
					     "diff")))
			)))
		   ((eq appearance 'patch)
		    (let ((generate-change-logs t))
		      (when regenerate
			(patcher-with-information "\
ChangeLog skeletons for this patch have already been generated.

If you answer `yes' to the question below, the ChangeLog entries will be
regenerated.  This means that current ones will be lost.  If otherwise your
answer is `no', it is assumed that you have edited the skeletons, and the
project will be rediff'ed with them."
			  (setq generate-change-logs (yes-or-no-p "\
Regenerate ChangeLog skeletons ? ")))
			(and generate-change-logs
			     (patcher-ungenerate-change-logs)))
		      (if generate-change-logs
			  (progn
			    (patcher-diff-base buffer)
			    (when (and (not patcher-sources)
				       (patcher-change-logs buffer))
			      (patcher-change-logs-diff-error))
			    ;; ChangeLogs must appear in the patch, so there's
			    ;; no point in inserting the diff right now.  It
			    ;; needs to be redone afterwards.
			    (patcher-generate-change-logs nil nil buffer)
			    (patcher-message "\
Please annotate the ChangeLog skeletons, \
and type \\[patcher-insert-change-logs] to create the whole diff.")
			    )
			;; not generate-change-logs
			;; ChangeLogs are supposed to be written, so
			;; everything goes as if we were in a 'manual case:
			(let ((command (patcher-project-option patcher-project
					 :change-logs-diff-command)))
			  (cond ((eq command 'diff)
				 (patcher-diff-all)
				 (patcher-insert-diff-prologue 'mixed)
				 )
				((stringp command)
				 (patcher-diff-all)
				 (patcher-convert-change-log-diffs command)
				 (patcher-insert-diff-prologue command)
				 )
				(t
				 (patcher-error "\
invalid `change-logs-diff-command' option: %s" command))
				))
			(patcher-message "\
To commit your changes, type \\[patcher-commit-change].")
			)))
		   ((not appearance)
		    (let ((generate-change-logs t))
		      (when regenerate
			(patcher-with-information "\
ChangeLog skeletons for this patch have already been generated.

If you answer `yes' to the question below, the ChangeLog entries will be
regenerated.  This means that current ones will be lost.  If otherwise your
answer is `no', the current CHangeLog entries won't be touched."
			  (setq generate-change-logs (yes-or-no-p "\
Regenerate ChangeLog skeletons ? ")))
			(and generate-change-logs
			     (patcher-ungenerate-change-logs)))
		      (if generate-change-logs
			  (progn
			    (patcher-diff-base buffer)
			    (when (and (not patcher-sources)
				       (patcher-change-logs buffer))
			      (patcher-change-logs-diff-error))
			    ;; ChangeLogs do not appear, so we can insert the
			    ;; diff right now, and then generate the
			    ;; skeletons.
			    (patcher-insert-diff buffer)
			    (patcher-insert-diff-prologue 'sources)
			    (patcher-generate-change-logs patcher-diff-marker
							  (extent-end-position
							   (patcher-extent
							    'patcher-diff)))
			    (message "\
Please don't forget to annotate the ChangeLog skeletons.")
			    )
			;; not generate-change-logs
			(patcher-diff-base buffer)
			(patcher-remove-change-logs buffer)
			(patcher-insert-diff buffer)
			(patcher-insert-diff-prologue 'sources)
			(patcher-message "\
To commit your changes, type \\[patcher-commit-change].")
			)))
		   (t
		    (patcher-error "\
invalid `change-logs-appearance' option: %s" appearance)))
	     )
	    ((eq updating 'manual)
	     ;; In the "manual" case, ChangeLogs are supposed to be already
	     ;; written, so their insertion does not have to be postponed.  If
	     ;; no files were specified, we have a chance to check that
	     ;; ChangeLogs /really/ are up to date: the diff output should
	     ;; contain all ChangeLog entries.
	     (patcher-diff-base buffer)
	     (when (and (not patcher-sources)
			(not (equal (patcher-change-logs buffer)
				    (mapcar 'car patcher-change-logs))))
	       (patcher-error "\
Some ChangeLog files are not updated.  \
Please update them before running Patcher."))
	     (cond ((eq appearance 'verbatim)
		    ;; #### NOTE: when ChangeLog entries are part of the diff,
		    ;; #### we could try to convert the diff to a verbatim
		    ;; #### version  instead of calling
		    ;; `patcher-insert-change-logs-verbatim'.
		    (patcher-remove-change-logs buffer)
		    (patcher-insert-diff buffer)
		    (patcher-insert-diff-prologue 'sources)
		    (or regenerate (patcher-insert-change-logs-verbatim)))
		   ((eq appearance 'packed)
		    (let ((command (patcher-project-option patcher-project
				     :change-logs-diff-command)))
		      (cond ((eq command 'diff)
			     ;; We use the same diff command:
			     (if (not patcher-sources)
				 ;; All ChangeLogs appear in the diff.  We can
				 ;; just move them to a pack.
				 (progn
				   (and regenerate
					(patcher-remove-change-logs buffer))
				   (patcher-insert-diff buffer)
				   (patcher-insert-diff-prologue 'sources)
				   (or regenerate
				       (patcher-pack-change-logs)))
			       ;; Otherwise, some ChangeLogs may not be there,
			       ;; so rediff them all.
			       (patcher-remove-change-logs buffer)
			       (patcher-insert-diff buffer)
			       (patcher-insert-diff-prologue 'sources)
			       (or regenerate
				   (patcher-diff-change-logs
				    patcher-diff-command)))
			     )
			    ((stringp command)
			     ;; The diff command is different.  We have to
			     ;; (re)diff them anyway.
			     (patcher-remove-change-logs buffer)
			     (patcher-insert-diff buffer)
			     (patcher-insert-diff-prologue 'sources)
			     (or regenerate
				 (patcher-diff-change-logs command))
			     )
			    (t
			     (patcher-error "\
invalid `change-logs-diff-command' option: %s" command))
			    )))
		   ((eq appearance 'patch)
		    (let ((command (patcher-project-option patcher-project
				     :change-logs-diff-command)))
		      (cond ((eq command 'diff)
			     (if patcher-sources
				 ;; Some ChangeLog entries might not be
				 ;; present, so we must rediff the whole
				 ;; stuff.
				 (progn
				   (patcher-diff-all)
				   (patcher-insert-diff-prologue 'mixed))
			       ;; Otherwise, the ChangeLog entries are in the
			       ;; diff.
			       (patcher-insert-diff buffer)
			       (patcher-insert-diff-prologue 'mixed)
			       )
			     )
			    ((stringp command)
			     (if (not patcher-sources)
				 (progn
				   (patcher-insert-diff buffer)
				   (patcher-insert-diff-prologue command)
				   (patcher-convert-change-log-diffs command))
			       ;; else some ChangeLog entries might not be
			       ;; present, so we must rediff the whole thing,
			       ;; and convert each ChangeLog diff to the
			       ;; proper command.
			       (patcher-diff-all)
			       (patcher-insert-diff-prologue command)
			       (patcher-convert-change-log-diffs command)
			       )
			     )
			    (t
			     (patcher-error "\
invalid `change-logs-diff-command' option: %s" command)
			     ))
		      ))
		   ((not appearance)
		    (patcher-remove-change-logs buffer)
		    (patcher-insert-diff buffer)
		    (patcher-insert-diff-prologue 'sources))
		   (t
		    (patcher-error "\
invalid `change-logs-appearance' option: %s"
				   appearance)))
	     (patcher-message "\
To commit your changes, type \\[patcher-commit-change]."))
	    (t
	     (patcher-error "invalid `change-logs-updating' option: %s"
			    updating))
	    ))
    ))


;; Patcher minor-mode ========================================================

(defun patcher-insert-change-logs ()
  "(Re)Insert ChangeLog entries in the current Patcher mail buffer."
  (interactive)
  (let ((updating
	 (or (patcher-project-option patcher-project :change-logs-updating)
	     (patcher-error "This project does not handle ChangeLogs")))
	(appearance
	 (or (patcher-project-option patcher-project :change-logs-appearance)
	     (patcher-error
	      "ChangeLogs are not supposed to appear in the message.")))
	)
    (cond ((or (eq updating 'automatic)
	       (eq updating 'manual))
	   (cond ((eq appearance 'verbatim)
		  (or (patcher-extent 'patcher-diff)
		      (patcher-error "Please generate the diff first."))
		  (let* ((extent (patcher-extent 'patcher-change-logs))
			 (do-it (or (not extent) (y-or-n-p "\
ChangeLog entries already inserted.  Replace ? "))))
		    (when do-it
		      (patcher-delete-extent-and-region extent)
		      (patcher-insert-change-logs-verbatim)))
		  )
		 ((eq appearance 'packed)
		  (or (patcher-extent 'patcher-diff)
		      (patcher-error "Please generate the diff first."))
		  (let* ((extent (patcher-extent 'patcher-change-logs))
			 (do-it (or (not extent) (y-or-n-p "\
ChangeLog entries already inserted.  Replace ? "))))
		    (when do-it
		      (patcher-delete-extent-and-region extent)
		      (let ((command (patcher-project-option patcher-project
				       :change-logs-diff-command)))
			(cond ((eq command 'diff)
			       ;; We use the same diff command:
			       (patcher-diff-change-logs patcher-diff-command)
			       )
			      ((stringp command)
			       (patcher-diff-change-logs command)
			       )
			      (t
			       (patcher-error "\
invalid `change-logs-diff-command' option: %s" command))
			      ))
		      ))
		  )
		 ((eq appearance 'patch)
		  (when (or (not (patcher-change-logs))
			    (y-or-n-p "\
ChangeLog entries already inserted.  Replace ? "))
		    (patcher-delete-extent-and-region
		     (patcher-extent 'patcher-diff))
		    (let ((command (patcher-project-option patcher-project
				     :change-logs-diff-command)))
		      (cond ((eq command 'diff)
			     (patcher-diff-all)
			     (patcher-insert-diff-prologue 'mixed)
			     )
			    ((stringp command)
			     (patcher-diff-all)
			     (patcher-convert-change-log-diffs command)
			     (patcher-insert-diff-prologue command)
			     )
			    (t
			     (patcher-error "\
invalid `change-logs-diff-command' option: %s" command))
			    ))
		    )
		  )
		 (t
		  (patcher-error "invalid `change-logs-appearance' option: %s"
				 appearance))
		 )
	   )
	  (t
	   (patcher-error "invalid `change-logs-updating' option: %s"
			  updating)))
    ))

(defun patcher-commit-change (&optional arg)
  "Prepare to, and possibly commit a change to a project's repository.
The change is the one that is announced in the mail buffer.

When called interactively, use a prefix (ARG) to override the commit
command to use.  Note that this is not meant to modify the source and
ChangeLog files affected by the commit: they are computed automatically."
  (interactive "P")
  (and patcher-change-committed
       (patcher-error "Change already committed !"))
  (let* ((buffer (generate-new-buffer "*Patcher Log Message*"))
	 (project patcher-project)
	 (directory default-directory)
	 (mail-buffer (current-buffer)))
    (with-current-buffer buffer
      (patcher-logmsg-mode)
      (cd directory)
      (setq patcher-project project)
      (setq patcher-mail-buffer mail-buffer)
      (setq patcher-logmsg-file-name
	    (replace-in-string (make-temp-name
				(expand-file-name "patch" (temp-directory)))
			       "\\\\" "/"))
      (setq patcher-logmsg-commit-command
	    (patcher-project-option patcher-project :commit-command))
      (and arg
	   (setq patcher-logmsg-commit-command
		 (read-shell-command "Commit command: "
				     patcher-logmsg-commit-command))))
    (setq patcher-logmsg-buffer buffer)
    (setq patcher-pre-commit-window-config (current-window-configuration))
    (pop-to-buffer buffer)
    (patcher-logmsg-init-message)
    ))

(defun patcher-generate-diff (&optional arg)
  "(Re)generate the diff in the current Patcher mail buffer.
When called interactively, use a prefix to override the diff command
used for this project.

Note that this is *not* the way to specify files affected by this patch.
See the variable `patcher-subprojects' or the function
`patcher-mail-subproject' for that."
  (interactive "P")
  (when (or (and (not (patcher-extent 'patcher-diff))
		 (not patcher-change-logs))
	    (y-or-n-p "Really regenerate the diff ? "))
    (and arg (setq patcher-diff-command
		   (read-shell-command "Diff command: " patcher-diff-command)))
    (patcher-generate-diff-1)))

(defun patcher-insert-patcher-header ()
  ;; Insert a Patcher version header in the message.
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^X-Generated-By: Patcher " nil t)
      ;; This search can fail in case of fake mail method.
      (when (re-search-forward
	     (concat "^" (regexp-quote mail-header-separator)) nil t)
	(goto-char (point-at-bol))
	(insert "X-Generated-By: " (patcher-version) "\n")))
    ))


(defcustom patcher-minor-mode-string " Patch"
  "*Patcher minor mode modeline string."
  :group 'patcher
  :type 'string)

(defcustom patcher-minor-mode-hook nil
  "*Hooks to run after setting up Patcher minor mode."
  :group 'patcher
  :type 'hook)

(defvar patcher-minor-mode-map
  (let ((map (make-sparse-keymap 'patcher-minor-mode-map)))
    (define-key map [(control c) (control p) d] 'patcher-generate-diff)
    (define-key map [(control c) (control p) i] 'patcher-insert-change-logs)
    (define-key map [(control c) (control p) c] 'patcher-commit-change)
    (define-key map [(control c) (control p) v] 'patcher-version)
    map)
  ;; Patcher minor mode keymap.
  )

(make-variable-buffer-local
 (defvar patcher-minor-mode nil))

(defun patcher-minor-mode (arg)
  "Toggles Patcher minor mode.
Used for mails prepared with `patcher-mail'.  You're not supposed to use
this, unless you know what you're doing.

\\{patcher-minor-mode-map}"
  (interactive "*P")
  (setq patcher-minor-mode
	(if (null arg) (not patcher-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (patcher-insert-patcher-header)
  (run-hooks 'patcher-minor-mode-hook))

(add-minor-mode
 'patcher-minor-mode patcher-minor-mode-string patcher-minor-mode-map)


;; ===========================================================================
;; Mail preparation routines
;; ===========================================================================

(patcher-globally-declare-boundp '(message-exit-actions))


(defvar patcher-projects-history nil
  ;; History used for prompting patcher projects.
  )

(defvar patcher-subjects-history nil
  ;; History used for prompting patcher mail subjects.
  )

(defgroup patcher-mail nil
  "Mailing options for Patcher projects."
  :group 'patcher)

(defcustom patcher-mail-check-change-logs-insertion 'ask
  "*ChangeLogs insertion checking prior to sending a Patcher mail.

This option affects the behavior of Patcher when ChangeLogs are supposed
to appear by manual insertion into the mail buffer:
- if nil, Patcher never checks that you have inserted them, and lets you
  send the message as-is,
- if t, Patcher blindly aborts the sending process if you have forgotten
  to insert the ChangeLogs in the message buffer,
- if 'ask (the default), Patcher asks you whether you want to proceed with
  sending or not."
  :group 'patcher-mail
  :type '(radio (const :tag "Never check" nil)
		(const :tag "Abort sending upon omission" t)
		(const :tag "Ask the user" ask)))

(defcustom patcher-mail-check-commit-action 'ask
  "*Commit action checking prior to sending a Patcher mail.

This option affects the behavior of Patcher when you have set the
:commit-privilege project option:
- if nil, Patcher never checks that you have commited your changes,
  and lets you send the message without having done so,
- if t, Patcher blindly aborts the sending process if you have forgotten
  to commit your changes,
- if 'ask (the default), Patcher asks you whether you want to proceed with
  sending or not."
  :group 'patcher-mail
  :type '(radio (const :tag "Never check" nil)
		(const :tag "Abort sending upon omission" t)
		(const :tag "Ask the user" ask)))

(defun patcher-before-send ()
  ;; Function hooked in the different mailing methods to perform some
  ;; checkings prior to sending the message.
  ;; #### NOTE: it is currently impossible (and probably not worth it) to
  ;; #### offer an automatic ChangeLog insertion or commit operation at that
  ;; #### point: we're already in an interactive call (the message sending
  ;; #### pocess) and a complex trickery would be necessary in case of
  ;; #### operation failure.  So it's simpler to just abort the sending, let
  ;; #### the user manually fix things, and re-send the message.

  ;; Check ChangeLogs insertion:
  (let ((updating
	 (patcher-project-option patcher-project :change-logs-updating))
	(appearance
	 (patcher-project-option patcher-project :change-logs-appearance)))
    (when (and patcher-mail-check-change-logs-insertion
	       (eq updating 'automatic) appearance)
      (cond ((or (eq appearance 'verbatim) (eq appearance 'packed))
	     (or (patcher-extent 'patcher-diff)
		 (patcher-error "There's no diff in this message !"))
	     (when (null (patcher-extent 'patcher-change-logs))
	       (let ((proceed
		      (or (null patcher-mail-check-change-logs-insertion)
			  (and (eq patcher-mail-check-change-logs-insertion
				   'ask)
			       (y-or-n-p "\
You did not insert the ChangeLog entries.  Proceed with sending anyway ? ")))))
		 (unless proceed (patcher-error "\
Sending aborted.  Please insert the ChangeLogs first."))))
	     )
	    ((eq appearance 'patch)
	     (unless (patcher-change-logs)
	       (let ((proceed
		      (or (null patcher-mail-check-change-logs-insertion)
			  (and (eq patcher-mail-check-change-logs-insertion
				   'ask)
			       (y-or-n-p "\
You did not insert the ChangeLog entries.  Proceed with sending anyway ? ")))))
		 (unless proceed(patcher-error "\
Sending aborted.  Please insert the ChangeLogs first."))))
	     )
	    (t
	     (patcher-error "invalid `change-logs-appearance' option: %s"
			    appearance))
	    )))
  ;; Check commit operation:
  (when (and (patcher-project-option patcher-project :commit-privilege)
	     (not patcher-change-committed))
    (let ((proceed (or (null patcher-mail-check-commit-action)
		       (and (eq patcher-mail-check-commit-action 'ask)
			    (y-or-n-p "\
You did not commit your changes.  Proceed with sending anyway ? ")))))
      (unless proceed (patcher-error "\
Sending aborted.  Please commit your changes first.")))
    ))

(defun patcher-after-send (&optional unused)
  ;; Function hooked in the different mailing methods to clean up the place
  ;; when a Patcher mail is sent.
  (setq patcher-instances (remassoc (buffer-name) patcher-instances))
  (when (patcher-project-option patcher-project
	  :kill-change-logs-after-sending)
    (let ((buffers (patcher-files-buffers patcher-change-logs)))
      (patcher-save-buffers buffers)
      (dolist (b buffers)
	(let ((ac (assoc (buffer-file-name b) patcher-change-logs)))
	  (when (or (not ac) ;; #### ??????
		    (cdr ac))
	    (kill-buffer b))))))
  (when patcher-logmsg-buffer
    (kill-buffer patcher-logmsg-buffer))
  ;; #### Implement kill-source-files-after-sending here.
  (when patcher-pre-commit-window-config
    (set-window-configuration patcher-pre-commit-window-config)))

(defun patcher-install-send-hooks ()
  ;; Install before- and after-send hooks into the MUA.
  (cond ((eq major-mode 'mail-mode)
	 (add-local-hook 'mail-send-hook 'patcher-before-send)
	 (push '(patcher-after-send) mail-send-actions))
	((eq major-mode 'message-mode)
	 (add-local-hook 'message-send-hook 'patcher-before-send)
	 ;; `message-exit-actions' is probably more appropriate than
	 ;; `message-send-actions' to perform the cleanup.
	 (push '(patcher-after-send) message-exit-actions))
	(t
	 (patcher-warning "\
Major mode: %s.
This mailing method is not fully supported by Patcher.
This is not critical though: Patcher won't be able to perform checks or
cleanups during mail sending.

Please report to <didier@xemacs.org>."
			  major-mode))))


;; Patcher FakeMail mode ====================================================

(defun patcher-fakemail-send ()
  "Pretend to send a fake Patcher mail.
Only perform the usual cleanup after real Patcher mails are sent."
  (interactive)
  (patcher-before-send)
  (patcher-after-send)
  (kill-buffer (current-buffer)))

(defvar patcher-fakemail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) (control c)] 'patcher-fakemail-send)
    map))

(defun patcher-fakemail-mode ()
  "Sets up Patcher-FakeMail major mode.
Used for editing a fake Patcher mail.

\\{patcher-fakemail-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'patcher-fakemail)
  (setq mode-name "Patcher-FakeMail")
  (use-local-map patcher-fakemail-mode-map)
  (run-hooks 'patcher-logmsg-mode-hook))


;; Interface to the different mailing methods ================================

(patcher-globally-declare-fboundp
 '(gnus-alive-p gnus gnus-other-frame gnus-post-news
		message-mail message-goto-body))

(defmacro patcher-with-mail-parameters (project &rest body)
  ;; Wrap BODY in a let construct possibly defining user-full-name and
  ;; user-mail-address by Patcher options.
  ;; Return the value of BODY execution.
  ;; #### NOTE: why is it called like this ? Because I'm sure one day or
  ;; #### another, some sucker will ask for more parameters, like the mail
  ;; #### signature for instance ;-)
  `(let ((user-full-name (or (patcher-project-option ,project :user-name)
			     user-full-name))
	 (user-mail-address (or (patcher-project-option ,project :user-mail)
				user-mail-address)))
    ,@body))
(put 'patcher-with-mail-parameters 'lisp-indent-function 1)


(defun patcher-mail-compose-mail (project subject)
  "Prepare a patch-related mail with the `compose-mail' function.

This function uses the `:to-address' project option to determine the email
address for sending the message.  Otherwise, the address is prompted for.

See also the `mail-user-agent' variable."

  (patcher-with-mail-parameters project
    (compose-mail (or (patcher-project-option project :to-address)
		      (read-string "To address: "))
		  subject))
  (patcher-install-send-hooks))


(defun patcher-mail-sendmail (project subject)
  "Prepare a patch-related mail with the `mail' function.
This method requires the `sendmail' library.

This function uses the `:to-address' project option to determine the email
address for sending the message.  Otherwise, the address is prompted for."
  (require 'sendmail)
  (patcher-with-mail-parameters project
    (mail nil (or (patcher-project-option project :to-address)
		  (read-string "To address: "))
	  subject))
  (add-local-hook 'mail-send-hook 'patcher-before-send)
  (push '(patcher-after-send) mail-send-actions))

(defun patcher-mail-message (project subject)
  "Prepare a patch-related mail with the `message-mail' function.
This method requires the `message' library.

This function uses the `:to-address' project option to determine the email
address for sending the message.  Otherwise, the address is prompted for."
  (require 'message)
  (patcher-with-mail-parameters project
    (message-mail (or (patcher-project-option project :to-address)
		      (read-string "To address: "))
		  subject))
  (add-local-hook 'message-send-hook 'patcher-before-send)
  ;; `message-exit-actions' is probably more appropriate than
  ;; `message-send-actions' to perform the cleanup.
  (push '(patcher-after-send) message-exit-actions))


(defcustom patcher-mail-run-gnus 'prompt
  "*Whether Patcher should run Gnus.

The 'gnus mailing method of Patcher needs a running Gnus session.
If Gnus is not running at the time it is needed, Patcher can start
it (or not) depending on this variable:
- if nil, Patcher will abort execution,
- it 'prompt (the default), Patcher will ask you what to do,
- if t Patcher will unconditionally start Gnus.

See also the function `patcher-mail-gnus'."
  :group 'patcher-mail
  :type '(radio (const :tag "never" nil)
		(const :tag "ask user" prompt)
		(const :tag "as needed" t)))

(defcustom patcher-mail-run-gnus-other-frame t
  "*Whether Patcher should start Gnus in a new frame.

This is used in case Patcher has to start Gnus by itself \(see the
variable `patcher-mail-run-gnus').  Possible values are:
- nil:     start Gnus in the current frame,
- t:       start Gnus in a new frame,
- 'follow: start Gnus in a new frame, and also use this frame to prepare
           the new Patcher message."
  :group 'patcher-mail
  :type '(radio (const :tag "Use current frame" nil)
		(const :tag "Create new frame" t)
		(const :tag "Create new frame, and use it for patcher" follow)
		))

(defun patcher-mail-run-gnus ()
  ;; Start a gnus session.
  (require 'gnus)
  (save-excursion
    (cond ((eq patcher-mail-run-gnus-other-frame t)
	   (save-selected-frame (gnus-other-frame)))
	  ((eq patcher-mail-run-gnus-other-frame 'follow)
	   (gnus-other-frame))
	  ((not patcher-mail-run-gnus-other-frame)
	   (gnus))
	  (t
	   (patcher-error "\
Invalid value for `patcher-mail-run-gnus-other-frame': "
			  patcher-mail-run-gnus-other-frame)))
    ))

(defun patcher-mail-gnus (project subject)
  "Prepare a patch-related mail with the `gnus-post-news' function.
Don't worry, this function can also send mails ;-).  This method
requires that you have Gnus *running* in your XEmacs session \(see
the variable `patcher-mail-run-gnus').

This function uses the `:gnus-group' project option to determine the Gnus
group to use \(as if you had typed `C-u a' on that group in the Group
buffer).  Otherwise, the group is prompted for."
  (require 'gnus-util)
  (unless (gnus-alive-p)
    (cond ((not patcher-mail-run-gnus)
	   (patcher-error
	    "The 'gnus mailing method requires a running Gnus session"))
	  ((eq patcher-mail-run-gnus t)
	   (patcher-mail-run-gnus))
	  ((eq patcher-mail-run-gnus 'prompt)
	   (if (y-or-n-p "Gnus is not currently running.  Start it ? ")
	       (patcher-mail-run-gnus)
	     (patcher-error
	      "The 'gnus mailing method requires a running Gnus session")))
	  (t
	   (patcher-error "Invalid value for `patcher-mail-run-gnus': "
			  patcher-mail-run-gnus))
	  ))
  ;; This binding is necessary to let message-mode hooks perform correctly.
  (let ((gnus-newsgroup-name (or (patcher-project-option project :gnus-group)
				 (read-string "Gnus group name: "))))
    (patcher-with-mail-parameters project
      (gnus-post-news 'post gnus-newsgroup-name)))
  (when (patcher-goto-subject)
    (insert subject))
  (message-goto-body)
  (add-local-hook 'message-send-hook 'patcher-before-send)
  ;; `message-exit-actions' is probably more appropriate than
  ;; `message-send-actions' to perform the cleanup.
  (push '(patcher-after-send) message-exit-actions))

(defun patcher-mail-fake (project subject)
  "Prepare a patch-related fake mail.
Use this function if you want to do all that Patcher can do, apart from
sending a real mail.  This function generates a fake mail buffer which acts
as a standard Patcher mail buffer, apart from the fact that when you type
\\<patcher-fakemail-mode-map>\\[patcher-fakemail-send] in it, it doesn't
really send a mail, but just clean things up."
  (let ((buffer (generate-new-buffer "*Patcher Fake Mail*")))
    (switch-to-buffer buffer)
    ;; #### NOTE: Patcher asks for a subject even with the fakemail method,
    ;; #### which is arguable.  However, even with a fake mail, one could
    ;; #### require log message initialization from a fake subject.  We could
    ;; #### do something more clever here.
    (insert "Subject: " subject "\n")
    (patcher-fakemail-mode)
    ))

(defun patcher-mail-setup (project files)
  ;; Setup patcher-minor-mode and initialize Patcher local variables in mails
  ;; (both generated or adapted).
  (push (cons (buffer-name) (current-buffer)) patcher-instances)
  (patcher-minor-mode t)
  (setq patcher-project project)
  (let ((command-directory
	 (patcher-project-option patcher-project :command-directory)))
    (when files (setq files (split-string files)))
    (if command-directory
	(let ((project-directory (patcher-project-directory project)))
	  (setq command-directory
		(expand-file-name command-directory project-directory))
	  (cd command-directory)
	  (setq patcher-sources
		(if (not files)
		    (list (patcher-file-relative-name
			   project-directory command-directory 'raw))
		  (mapcar
		   (lambda (file)
		     (patcher-file-relative-name
		      (expand-file-name file project-directory)
		      command-directory 'raw))
		   files)))
	  )
      ;; else: (null command-directory)
      (cd (patcher-project-directory project))
      (setq patcher-sources files)
      ))
  (setq patcher-diff-command (patcher-project-option project :diff-command)))


;; Mail generation entry points =============================================

(defun patcher-mail-1 (project subject files override)
  ;; Perform the real job of preparing the mail buffer.
  (let ((subject-prefix (patcher-project-option project :subject-prefix))
	extent)
    ;; Construct the subject, maybe with an extent marking the prefix:
    (when (> (length subject-prefix) 0)
      (setq subject-prefix (patcher-substitute-name project subject-prefix))
      (setq extent (make-extent 0 (length subject-prefix) subject-prefix))
      (set-extent-properties extent
	'(duplicable t patcher-subject-prefix t)))
    (setq subject (concat subject-prefix
			  (and subject-prefix (> (length subject-prefix) 0)
			       subject (> (length subject) 0)
			       " ")
			  subject))
    (funcall
     (intern (concat "patcher-mail-"
		     (symbol-name
		      (patcher-project-option project :mail-method))))
     project subject))
  (patcher-mail-setup project files)
  (let ((mail-prologue (patcher-project-option project :mail-prologue)))
    (when (> (length mail-prologue) 0)
      (insert "\n" mail-prologue)))
  (save-excursion
    (insert "\n\n")
    (when (patcher-project-option project :change-logs-updating)
      (let ((appearance
	     (patcher-project-option project :change-logs-appearance)))
	(when (and appearance (not (eq appearance 'patch)))
	  (setq patcher-change-logs-marker (point-marker))
	  (insert "\n"))))
    (setq patcher-diff-marker (point-marker))
    (patcher-generate-diff override)))


;;;###autoload
(defun patcher-mail-subproject (project subject files &optional arg)
  "Prepare a mail about a patch to apply on part of a project.
PROJECT is the name of the project (see the variables `patcher-projects'
and `patcher-subprojects').
SUBJECT is the subject of the mail.
FILES is a string listing one or more files, possibly with wild cards --
  essentially, part of a command line to be interpolated into the `diff'
  and maybe the `commit' commands issued by Patcher.

When called interactively, use a prefix (ARG) to override the value of
the diff command to use for this project.

This function is intended for one-time only subprojects.  Alternately, you
can define subprojects in the variable `patcher-subprojects' and continue
using `patcher-mail'.  If you call this function on a predefined subproject,
you will have the opportunity to modify the predefined list of files or
directories the subproject is composed of.

When you use this command instead of `patcher-mail', any commits issued
from the mail buffer (using \\<patcher-minor-mode-map>\\[patcher-commit-change]) will automatically include
the associated ChangeLogs, as well as the file(s) specified as part of
this command.

Please note that you can have multiple occurrences of a Patcher mail at
the same time, but not more than one at a time on the same project unless
you use `patcher-mail-subproject' and the sections of the project don't
overlap."
  (interactive
   (let* ((prj (assoc (completing-read "Project: " (append patcher-subprojects
							   patcher-projects)
				       nil t nil 'patcher-projects-history)
		      (append patcher-subprojects patcher-projects)))
	  (sbj (read-string
		"Subject: "
		(let ((s (patcher-project-option prj :subject)))
		  (when (> (length s) 0)
		    (patcher-substitute-name prj s)))
		patcher-subjects-history))
	  (dir (patcher-project-directory prj))
	  (fls (let ((default-directory (file-name-as-directory dir)))
		 (or (let ((f (patcher-project-option prj :files)))
		       (and f (read-shell-command "Files: " (concat f " ")
						  nil f)))
		     (let* ((default-file (and (buffer-file-name)
					       (patcher-file-relative-name
						(buffer-file-name)
						dir 'raw)))
			    (default-file
			      ;; If the file is not actually underneath the
			      ;; project, then don't suggest it as a
			      ;; possibility.
			      (and default-file
				   (if (string-match "^\\.\\.$\\|^\\.\\.[/\\]"
						     default-file)
				       nil default-file))))
		       (read-shell-command
			"Files: "
			default-file nil default-file))))))
     (list prj sbj fls current-prefix-arg)))
  (patcher-mail-1 project subject files (and (interactive-p) arg)))

;;;###autoload
(defun patcher-mail (project subject &optional arg)
  "Prepare a mail about a patch to apply on a project.
PROJECT is the name of the project (see the variables `patcher-projects'
and `patcher-subprojects').
SUBJECT is the subject of the mail.

When called interactively, use a prefix (ARG) to override the value of
the diff command to use for this project.  Note that this is *not* the way
to restrict the diff to certain files.  If you want to work on a subset of
the project (e.g. some files, subdirectories etc), you have two
alternatives:

- for temporary subprojects, you can use the function
  `patcher-mail-subproject', which lets you specify the list of modified
  files / directories.
- otherwise, you can also define the subprojects in the variable
  `patcher-subprojects' and continue using this function.

Please note that you can have multiple occurrences of a Patcher mail at
the same time, but not more than one at a time on the same project unless
you use `patcher-mail-subproject' and the sections of the project don't
overlap."
  (interactive
   (let* ((prj (assoc (completing-read "Project: " (append patcher-subprojects
							   patcher-projects)
				       nil t nil 'patcher-projects-history)
		      (append patcher-subprojects patcher-projects)))
	  (sbj (read-string
		"Subject: "
		(let ((s (patcher-project-option prj :subject)))
		  (when (> (length s) 0)
		    (patcher-substitute-name prj s)))
		patcher-subjects-history)))
     (list prj sbj current-prefix-arg)))
  (patcher-mail-1 project subject (patcher-project-option project :files)
		  (and (interactive-p) arg))
  )


;; Mail adaptation entry points =============================================

;; #### NOTE: the prefix argument usage in patcher-mail[-subproject]
;; #### to override the diff command is broken by design (it comes from an
;; #### early version of Patcher): why the diff command and not any other
;; #### option ? I'm not going to propagate this misconception here, so the
;; #### adaptation functions don't have a prefix argument at all.

(defun patcher-mail-adapt-1 (project files)
  ;; Like `patcher-mail-1', but for already existing mails.
  (let ((subject-prefix (patcher-project-option project :subject-prefix))
	extent)
    ;; Construct the subject, maybe with an extent marking the prefix:
    (when (> (length subject-prefix) 0)
      (setq subject-prefix (patcher-substitute-name project subject-prefix))
      (setq extent (make-extent 0 (length subject-prefix) subject-prefix))
      (set-extent-properties extent '(duplicable t patcher-subject-prefix t))
      (when (patcher-goto-subject)
	(insert subject-prefix " "))))
  (patcher-install-send-hooks)
  (patcher-mail-setup project files)
  ;; #### FIXME: currently, I have simply discarded the mail-prologue
  ;; #### insertion for adapted mails. This is because mail adaptation is
  ;; #### mostly for replies in which you probably don't want the standard
  ;; #### prologue. However, this could be turned into a standard option.
  ;;  (let ((mail-prologue (patcher-project-option project :mail-prologue)))
  ;;    (when (> (length mail-prologue) 0)
  ;;      (insert "\n" mail-prologue)))
  (patcher-goto-signature)
  (when (patcher-project-option project :change-logs-updating)
    (let ((appearance
	   (patcher-project-option project :change-logs-appearance)))
      (when (and appearance (not (eq appearance 'patch)))
	(setq patcher-change-logs-marker (point-marker))
	(insert "\n"))))
  (setq patcher-diff-marker (point-marker))
  (patcher-generate-diff))

;;;###autoload
(defun patcher-mail-adapt (project)
  "Same as `patcher-mail', but for already started mails.
This function is mostly designed to adapt replies or followups probably
started with your usual MUA to Patcher.

Note two differences with `patcher-mail' however:
1. there is no SUBJECT argument to this function,
2. no prefix argument is available to override the diff command."
  (interactive
   (list (assoc (completing-read "Project: " (append patcher-subprojects
						     patcher-projects)
				 nil t nil 'patcher-projects-history)
		(append patcher-subprojects patcher-projects))))
  (patcher-mail-adapt-1 project (patcher-project-option project :files)))

;;;###autoload
(defun patcher-mail-adapt-subproject (project files)
  "Same as `patcher-mail-subproject', but for already started mails.
This function is mostly designed to adapt replies or followups probably
started with your usual MUA to Patcher.

Note two differences with `patcher-mail-subproject' however:
1. there is no SUBJECT argument to this function,
2. no prefix argument is available to override the diff command."
  (interactive
   (let* ((prj (assoc (completing-read "Project: " (append patcher-subprojects
							   patcher-projects)
				       nil t nil 'patcher-projects-history)
		      (append patcher-subprojects patcher-projects)))
	  (dir (patcher-project-directory prj))
	  (fls (let ((default-directory (file-name-as-directory dir)))
		 (or (let ((f (patcher-project-option prj :files)))
		       (and f (read-shell-command "Files: " (concat f " ")
						  nil f)))
		     (let* ((default-file (and (buffer-file-name)
					       (patcher-file-relative-name
						(buffer-file-name)
						dir 'raw)))
			    (default-file
			      ;; If the file is not actually underneath the
			      ;; project, then don't suggest it as a
			      ;; possibility.
			      (and default-file
				   (if (string-match "^\\.\\.$\\|^\\.\\.[/\\]"
						     default-file)
				       nil default-file))))
		       (read-shell-command
			"Files: "
			default-file nil default-file))))))
     (list prj fls)))
  (patcher-mail-adapt-1 project files))


;; Patcher Gnus Summary minor mode ==========================================

(patcher-globally-declare-fboundp
 '(gnus-summary-followup gnus-summary-followup-with-original
   gnus-summary-reply gnus-summary-reply-with-original))

(defun patcher-gnus-summary-followup (&optional arg)
  "Prepare a Patcher followup from the Gnus Summary buffer.
With a prefix argument, behave like `patcher-mail-subproject' instead of
`patcher-mail'."
  (interactive "P")
  (gnus-summary-followup nil)
  (call-interactively
   (if arg
       'patcher-mail-adapt-subproject
     'patcher-mail-adapt)))

(defun patcher-gnus-summary-followup-with-original (&optional arg)
  "Prepare a Patcher followup from the Gnus Summary buffer.
The original message is yanked.
With a prefix argument, behave like `patcher-mail-subproject' instead of
`patcher-mail'."
  (interactive "P")
  (gnus-summary-followup-with-original nil)
  (call-interactively
   (if arg
       'patcher-mail-adapt-subproject
     'patcher-mail-adapt)))

(defun patcher-gnus-summary-reply (&optional arg)
  "Prepare a Patcher reply from the Gnus Summary buffer.
With a prefix argument, behave like `patcher-mail-subproject' instead of
`patcher-mail'."
  (interactive "P")
  ;; #### NOTE: it is strange that this function's first argument is not
  ;; #### mandatory, as in the 3 other ones.
  (gnus-summary-reply)
  (call-interactively
   (if arg
       'patcher-mail-adapt-subproject
     'patcher-mail-adapt)))

(defun patcher-gnus-summary-reply-with-original (&optional arg)
  "Prepare a Patcher reply from the Gnus Summary buffer.
The original message is yanked.
With a prefix argument, behave like `patcher-mail-subproject' instead of
`patcher-mail'."
  (interactive "P")
  (gnus-summary-reply-with-original nil)
  (call-interactively
   (if arg
       'patcher-mail-adapt-subproject
     'patcher-mail-adapt)))

(defcustom patcher-gnus-summary-minor-mode-string " Patch"
  "*Patcher Gnus Summary minor mode modeline string."
  :group 'patcher
  :type 'string)

(defcustom patcher-gnus-summary-minor-mode-hook nil
  "*Hooks to run after setting up Patcher Gnus Summary minor mode."
  :group 'patcher
  :type 'hook)

(defvar patcher-gnus-summary-minor-mode-map
  (let ((map (make-sparse-keymap 'patcher-minor-mode-map)))
    (define-key map [(control c) (control p) f]
      'patcher-gnus-summary-followup)
    (define-key map [(control c) (control p) F]
      'patcher-gnus-summary-followup-with-original)
    (define-key map [(control c) (control p) r]
      'patcher-gnus-summary-reply)
    (define-key map [(control c) (control p) R]
      'patcher-gnus-summary-reply-with-original)
    map)
  ;; Patcher Gnus Summary minor mode keymap.
  )

(make-variable-buffer-local
 (defvar patcher-gnus-summary-minor-mode nil))

(defun patcher-gnus-summary-minor-mode (arg)
  "Toggles Patcher Gnus Summary minor mode.
Used for Patcher messages composed as Gnus replies and followups.
You're not supposed to use this, unless you know what you're doing.

\\{patcher-gnus-summary-minor-mode-map}"
  (interactive "*P")
  (setq patcher-gnus-summary-minor-mode
	(if (null arg) (not patcher-gnus-summary-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (run-hooks 'patcher-gnus-summary-minor-mode-hook))

(add-minor-mode
 'patcher-gnus-summary-minor-mode
 patcher-gnus-summary-minor-mode-string
 patcher-gnus-summary-minor-mode-map)


;; Patcher Gnus Article minor mode ==========================================

(defcustom patcher-gnus-article-minor-mode-string " Patch"
  "*Patcher Gnus Article minor mode modeline string."
  :group 'patcher
  :type 'string)

(defcustom patcher-gnus-article-minor-mode-hook nil
  "*Hooks to run after setting up Patcher Gnus Article minor mode."
  :group 'patcher
  :type 'hook)

(defvar patcher-gnus-article-minor-mode-map
  (let ((map (make-sparse-keymap 'patcher-minor-mode-map)))
    (define-key map [(control c) (control p) f]
      'patcher-gnus-summary-followup)
    (define-key map [(control c) (control p) F]
      'patcher-gnus-summary-followup-with-original)
    (define-key map [(control c) (control p) r]
      'patcher-gnus-summary-reply)
    (define-key map [(control c) (control p) R]
      'patcher-gnus-summary-reply-with-original)
    map)
  ;; Patcher Gnus Article minor mode keymap.
  )

(make-variable-buffer-local
 (defvar patcher-gnus-article-minor-mode nil))

(defun patcher-gnus-article-minor-mode (arg)
  "Toggles Patcher Gnus Article minor mode.
Used for Patcher messages composed as Gnus replies and followups.
You're not supposed to use this, unless you know what you're doing.

\\{patcher-gnus-article-minor-mode-map}"
  (interactive "*P")
  (setq patcher-gnus-article-minor-mode
	(if (null arg) (not patcher-gnus-article-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (run-hooks 'patcher-gnus-article-minor-mode-hook))

(add-minor-mode
 'patcher-gnus-article-minor-mode
 patcher-gnus-article-minor-mode-string
 patcher-gnus-article-minor-mode-map)


;; ==========================================================================
;; Routines to plug Patcher into external libraries
;; ==========================================================================

(patcher-globally-declare-boundp
 '(gnus-summary-mode-hook gnus-article-mode-hook))

;;;###autoload
(defun patcher-insinuate-gnus ()
  "This function plugs Patcher into Gnus.
It should be called from your gnusrc file."
  (add-hook 'gnus-summary-mode-hook
	    '(lambda () (patcher-gnus-summary-minor-mode 1)))
  (add-hook 'gnus-article-mode-hook
	    '(lambda () (patcher-gnus-article-minor-mode 1))))


(provide 'patcher)

;;; patcher.el ends here
