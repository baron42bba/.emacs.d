;;; xemacsbug.el --- command to report XEmacs bugs to appropriate mailing list.

;; Copyright (C) 2001 Free Software Foundation, Inc.
;; Copyright (C) 2001 Steve Youngs <youngs@xemacs.org>

;; Author: Steve Youngs <youngs@xemacs.org>
;; Based on 'emacsbug.el' by: K. Shane Hartman

;; Maintainer: XEmacs Development Team
;; Keywords: maint mail bugs

;; Not fully installed because it can work only on Internet hosts.
;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; `M-x report-xemacs-bug ' starts an email note to the XEmacs maintainers
;; describing a problem.  Here's how it's done...

;;; Code:

(require 'sendmail)
(require 'shadow)

(eval-when-compile
  (defvar mh-before-send-letter-hook))

(defgroup xemacsbug nil
  "Sending XEmacs bug reports."
  :group 'maint
  :group 'mail)

;; >> These should be addresses which are accessible to your machine,
;; >> otherwise you can't use this file.  It will only work on the
;; >> internet with this address.

(defcustom report-xemacs-bug-address "XEmacs Beta <xemacs-beta@xemacs.org>"
  "*Address of mailing list for XEmacs bugs."
  :group 'xemacsbug
  :type 'string)

(defcustom report-xemacs-bug-extra-headers nil
  "*An alist of mail-header value pairs for XEamcs bugs.

It takes the format (HEADER . VALUE) where both HEADER and VALUE are
strings. See `compose-mail'."
  :group 'xemacsbug
  :type '(repeat 
	  (cons (string :tag "Header") 
		(string :tag "Value"))))

(defcustom report-xemacs-bug-beta-address "XEmacs Beta <xemacs-beta@xemacs.org>"
  "*Address of mailing list for XEmacs beta bugs."
  :group 'xemacsbug
  :type 'string)

(defcustom report-xemacs-bug-beta-extra-headers nil
  "*An alist of mail-header value pairs for XEmacs beta bugs.

It takes the format (HEADER . VALUE) where both HEADER and VALUE are
strings. See `compose-mail'."
  :group 'xemacsbug
  :type '(repeat 
	  (cons (string :tag "Header") 
		(string :tag "Value"))))

(defvar report-xemacs-bug-orig-text nil
  "The automatically-created initial text of bug report.")

(defcustom report-xemacs-bug-no-confirmation nil
  "*If non-nil, suppress the confirmations asked for the sake of novice users."
  :group 'xemacsbug
  :type 'boolean)

(defcustom report-xemacs-bug-no-explanations nil
  "*If non-nil, suppress the explanations given for the sake of novice users."
  :group 'xemacsbug
  :type 'boolean)

(defcustom report-xemacs-bug-send-init nil
  "*If non-nil, include the user's init.el file in the bug report."
  :group 'xemacsbug
  :type 'boolean)

(defconst report-xemacs-bug-help
"\nThis bug report will be sent to the XEmacs Development Team,
not to your local site managers!!

Please write in English, because the XEmacs maintainers do not have
translators to read other languages for them.

Please describe as succinctly as possible:
\t- What happened.
\t- What you thought should have happened.
\t- Precisely what you were doing at the time.

Also include a reliable recipe for triggering the bug, as well as
any C and lisp back-traces that you may have.
\(setq stack-trace-on-error t\), or \(setq debug-on-error t\) if you
are familiar with the debugger, to get a lisp back-trace.
To get a core file for the C back-trace on a GNU/Linux system do 
'ulimit -c unlimited' in the shell prior to starting XEmacs.

Type \\[report-xemacs-bug-info] to visit in Info the XEmacs Manual section
about when and how to write a bug report,
and what information to supply so that the bug can be fixed.
Type SPC to scroll through this section and its subsections.

You are very welcome to scan through the bug report and remove any
potentially sensitive data.

Turn off this help buffer permanently by adding:

\t \(setq report-xemacs-bug-no-explanations t\)

To your ~/.xemacs/init.el")

(defun report-xemacs-bug-help ()
  "Display the help buffer for `report-xemacs-bug'."
  (with-electric-help
   '(lambda () 
      (define-key (current-local-map) "\C-c\C-i" 'report-xemacs-bug-info)
      (princ (substitute-command-keys report-xemacs-bug-help)) nil) "*Bug Help*"))

(defun report-xemacs-bug-packages-list ()
  "Insert into the current buffer a list of installed packages."
  (let ((pkgs packages-package-list))
    (while pkgs
      (insert
       (format "(%s ver: %s upstream: %s)\n"
	       (nth 0 (car pkgs))
	       (nth 2 (car pkgs))
	       (nth 4 (car pkgs))))
      (setq pkgs (cdr pkgs)))))

;;;###autoload
(defun report-xemacs-bug (topic &optional recent-keys)
  "Report a bug in XEmacs.
Prompts for bug subject.  Leaves you in a mail buffer."
  ;; This strange form ensures that (recent-keys) is the value before
  ;; the bug subject string is read.
  (interactive (reverse (list (recent-keys) (read-string "Bug Subject: "))))
  (let (user-point)
    (setq topic (concat "[Bug: " emacs-program-version "] " topic))
    (if xemacs-betaname
	  (compose-mail report-xemacs-bug-beta-address
			topic
			report-xemacs-bug-beta-extra-headers)
      (compose-mail report-xemacs-bug-address
		    topic
		    report-xemacs-bug-extra-headers))
    ;; The rest of this does not execute
    ;; if the user was asked to confirm and said no.
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (forward-line 1)
    (insert "================================================================\n")
    (insert "Dear Bug Team!\n\n")
    (setq user-point (point))
    (insert "\n\n================================================================\n
System Info to help track down your bug:
---------------------------------------\n\n")
    ;; Insert the output of 'describe-installation'.
    (insert (symbol-value 'Installation-string))
    ;; Load-path shadows can cause some grief.
    (flet ((append-message
	     (&rest args) ())
	   (clear-message
	     (&optional label frame stdout-p no-restore)
	     ()))
      (insert "\n\nLoad-Path Lisp Shadows:\n"
	      "----------------------\n")
      (let ((before-shadows (point)))
	(insert
	  (format "%s"
		  (find-emacs-lisp-shadows load-path)))
	(save-restriction
	  (narrow-to-region before-shadows (point))
	  (fill-paragraph t)
	  (insert "\n"))))
    ;; Insert a list of installed packages.
    (insert "\n\nInstalled XEmacs Packages:\n"
	    "-------------------------\n")
    (report-xemacs-bug-packages-list)
    (insert "\n")
    ;; Insert a list of installed modules.
    (if (fboundp 'list-modules)
	(progn
	  (insert "\n\nInstalled Modules:\n"
		  "-----------------\n")
	    (let* ((mods (list-modules)))
	      (while mods
		(cl-prettyprint (cdr (car mods)))
		(setq mods (cdr mods))))))
    ;; Insert a list of loaded features
    (let ((before-features (point)))
      (insert
       (format "\n\nFeatures:\n--------\n\n%s" (symbol-value 'features)))
      (save-restriction
	(narrow-to-region before-features (point))
	(fill-paragraph t)
	(insert "\n")))
    ;; Insert recent keystrokes.
    (insert "\n\n"
	    "Recent keystrokes:\n-----------------\n\n")
    (let ((before-keys (point)))
      (insert (key-description recent-keys))
      (save-restriction
	(narrow-to-region before-keys (point))
	(goto-char before-keys)
	(while (progn (move-to-column 50) (not (eobp)))
	  (search-forward " " nil t)
	  (insert "\n"))))
    ;; Insert recent minibuffer messages.
    (insert "\n\n\nRecent messages (most recent first):\n"
	    "-----------------------------------\n")
    (let ((standard-output (current-buffer)))
      (print-recent-messages 20)
      (insert "\n"))
    ;; Insert the contents of the user's init file if it exists.
    (if report-xemacs-bug-send-init
      (if (file-readable-p user-init-file)
	  (save-excursion
	    (goto-char (point-max))
	    (beginning-of-line)
	    (insert "\n\nUser Init File:\n--------------\n\n")
	    (insert-file-contents user-init-file))))
    ;; This is so the user has to type something
    ;; in order to send easily.
    (use-local-map (let ((map (make-sparse-keymap)))
		     (set-keymap-parents map (current-local-map))
		     map))
    (define-key (current-local-map) "\C-c\C-i" 'report-xemacs-bug-info)
    ;; Make it less likely people will send empty messages.
    (cond
     ((eq mail-user-agent 'sendmail-user-agent)
      (make-local-variable 'mail-send-hook)
      (add-hook 'mail-send-hook 'report-xemacs-bug-hook))
     ((eq mail-user-agent 'message-user-agent)
      (make-local-variable 'message-send-hook)
      (add-hook 'message-send-hook 'report-xemacs-bug-hook))
     ((eq mail-user-agent 'mh-e-user-agent)
      (make-local-variable 'mh-before-send-letter-hook)
      (add-hook 'mh-before-send-letter-hook 'report-xemacs-bug-hook))
     (t
      (make-local-variable 'mail-send-hook)
      (add-hook 'mail-send-hook 'report-xemacs-bug-hook)))
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (make-local-variable 'report-xemacs-bug-orig-text)
      (setq report-xemacs-bug-orig-text (buffer-substring (point-min) (point))))
    (goto-char user-point))
  (delete-other-windows)
  (unless report-xemacs-bug-no-explanations
    (report-xemacs-bug-help)
    (cond
     ((eq mail-user-agent 'sendmail-user-agent)
      (message (substitute-command-keys
		"Type \\[mail-send-and-exit] to send the bug report, \\[kill-buffer] to cancel.")))
     ((eq mail-user-agent 'message-user-agent)
      (message (substitute-command-keys
		"Type \\[message-send-and-exit] to send the bug report, \\[kill-buffer] to cancel.")))
     ((eq mail-user-agent 'mh-e-user-agent)
      (message (substitute-command-keys
		"Type \\[mh-send-letter] to send the bug report, \\[kill-buffer] to cancel.")))
     (t
      (message (substitute-command-keys
		"Type \\[mail-send-and-exit] to send the bug report, \\[kill-buffer] to cancel."))))))


;; For backwards compatibility
;;;###autoload
(defalias 'report-emacs-bug 'report-xemacs-bug)

(defun report-xemacs-bug-info ()
  "Go to the Info node on reporting XEmacs bugs."
  (interactive)
  (Info-goto-node "(xemacs)Bugs"))

(defun report-xemacs-bug-hook ()
  "Hook run before sending a bug report."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (if (and (= (- (point) (point-min))
		(length report-xemacs-bug-orig-text))
	     (equal (buffer-substring (point-min) (point))
		    report-xemacs-bug-orig-text))
	(error "No text entered in bug report"))

    ;; The last warning for novice users.
    (if (or report-xemacs-bug-no-confirmation
	    (yes-or-no-p
	     "Send this bug report to the XEmacs maintainers? "))
	;; Just send the current mail.
	nil
      (goto-char (point-min))
      (let* ((top (point)))
	(re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
	(save-restriction
	  (narrow-to-region top (point))
	  (goto-char (point-min))
	  (if (re-search-forward "^To: " (eobp) t)
	      (let ((pos (point)))
		(end-of-line)
		(delete-region pos (point))))
	  (goto-char (point-min))
	  (if (re-search-forward "^Cc: " (eobp) t)
	      (let ((pos (point)))
		(end-of-line)
		(delete-region pos (point))))))
      (cond
       ((eq mail-user-agent 'sendmail-user-agent)
	(kill-local-variable 'mail-send-hook))
       ((eq mail-user-agent 'message-user-agent)
	(kill-local-variable 'message-send-hook))
       ((eq mail-user-agent 'mh-e-user-agent)
	(kill-local-variable 'mh-before-send-letter-hook))
       (t
	(kill-local-variable 'mail-send-hook)))
      (unless report-xemacs-bug-no-explanations
	(with-electric-help
	 '(lambda ()
	    (insert "\n
You invoked the command M-x report-xemacs-bug,
but you decided not to mail the bug report to the XEmacs maintainers.

If you want to mail it to someone else instead,
please insert the proper e-mail address after \"To: \",
and send the mail again.") nil) "*Bug Help*"))
      (error "Sending Bug Report Cancelled"))))

(provide 'xemacsbug)

;;; xemacsbug.el ends here
