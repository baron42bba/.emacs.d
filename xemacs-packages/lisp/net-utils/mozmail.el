;; mozmail.el --- Open mailto links from Mozilla in an (X)Emacs mailer.

;; Copyright (C) 2003,04 Steve Youngs

;; RCS: $Id: mozmail.el,v 1.8 2004/07/14 08:51:55 youngs Exp $
;; Author:        Steve Youngs <steve@youngs.au.com>
;; Maintainer:    Steve Youngs <steve@youngs.au.com>
;; Created:       <2003-12-22>
;; Last-Modified: <2004-06-10 18:43:58 (steve)>
;; Homepage:      None, contact maintainer for the latest version.
;;                Or get it from the XEmacs "net-utils" package.
;; Keywords:      mail

;; This file is part of mozmail.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; 3. Neither the name of the author nor the names of any contributors
;;    may be used to endorse or promote products derived from this
;;    software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; 
;;   Mozilla is a terrific web browser, but for mail and news I
;;   much prefer XEmacs & Gnus.  Once this is set up, clicking on a
;;   mailto link in Mozilla will fire up an (X)Emacs MUA with all
;;   the appropriate fields filled in.  MUAs that are supported at
;;   this time are: Gnus, VM, MH-E, MEW, Wanderlust, RMail, and the
;;   built-in Emacs mailer.
;;
;; Setup (Mozilla):
;;
;;   To get this to work you will need a very recent version of Mozilla,
;;   I was using 1.6b when I wrote this.  If you get `mozmail.el' to
;;   work with older versions of Mozilla, please let me know.
;;
;;   The first thing you must do is tweak your Mozilla settings so
;;   mailto links will invoke an external process.  Fire up Mozilla,
;;   and in the location bar type: about:config
;;
;;   That will give you a list of all of your settings.  There are
;;   litterally hundreds of them so prune them down by typing
;;   "protocol-handler" in the filter bar.  Now right-click on one of
;;   the items in the list and choose "New -> Boolean".  In the
;;   resulting dialog, type:
;;   "network.protocol-handler.external.mailto" (sans quotes).
;;   Another dialog will appear prompting for a value for this new
;;   variable, enter "true" (sans quotes).
;;
;;   Next, add another variable: right-click on a list item and choose "New
;;   -> String", in the dialog put: "network.protocol-handler.app.mailto".
;;   In the value dialog for this variable, put: "mozmail.sh".
;;
;;   That's all you need to do on the Mozilla side of things.  Restarting
;;   Mozilla probably wouldn't be a bad idea.
;;
;; Setup (Shell Script):
;;
;;   You will also need a very small (2 line) wrapper script.  Copy
;;   the following text to `mozmail.sh', put it somewhere in your
;;   $PATH and make it executable.
;;
;;      #!/bin/bash
;;      gnuclient -eval "(mozmail \"$1\")"
;;
;; Setup ((X)Emacs):
;;
;;   Add the following to your init file...
;;
;;   (gnuserv-start)
;;   (require 'mozmail)
;;
;;   People who use MUA's other than Gnus will need to customise the
;;   variable `mozmail-default-mua'.  See that variable's doc string
;;   for details.
;;
;;   Gnus users can optionally customise `mozmail-gnus-is-plugged' to
;;   run Gnus in either "plugged" or "unplugged" modes.
;;
;; Alternative Setup for GNOME users:
;;
;;   I received some feedback from a user who had this to say about
;;   setting mozmail up with GNOME (on Debian):
;;
;;   Here's different way to enable it for GNOME users (at least on
;;   Debian).  Instead of modifying Mozilla's preferences just run
;;   gnome-default-applications-properties (see the
;;   Applications/Desktop Preferences/Advanced/Preferred Applications
;;   menu entry), on the "Mail Reader" tab select "Custom Mail Reader"
;;   and enter "mozmail.sh %s".
;;
;;   With this mozmail gets used by all Mozilla based browser on my
;;   system.

;;; Todo:
;;
;;   o Can this be done without using gnuserv/gnuclient?

;;; ChangeLog:
;;
;;  From this point on, `mozmail.el' is in the XEmacs packages CVS
;;  repository.  For further changes please consult
;;  ./xemacs-packages/net-utils/ChangeLog.
;;
;;  Revision 1.1  2003-12-23 14:49:47+10  steve
;;  Initial revision
;;

;;; Code:
(defun mozmail-version (&optional arg)
  "Return the current version info for mozmail.

With optional argument ARG, insert version info at point in the current
buffer."
  (interactive "P")
  (let (ver)
    (with-temp-buffer
      (erase-buffer)
      (insert-file (locate-library "mozmail.el"))
      (goto-char (point-min))
      (re-search-forward 
       "mozmail\\.el,v\\s-\\([0-9]+[0-9\\.]*[0-9]+\\)" nil t)
      (setq ver (match-string 1)))
    (if (interactive-p)
	(if arg
	    (insert (format "mozmail v%s" ver))
	  (message "mozmail v%s" ver))
      ver)))

(eval-and-compile
  (if (featurep 'url-util)
      (autoload 'url-unhex-string "url-util")
    (autoload 'url-unhex-string "url"))
  (autoload 'with-electric-help "ehelp")
  (autoload 'gnus-alive-p "gnus-util")
  (autoload 'gnus-group-mail "gnus-msg" nil t)
  (autoload 'message-goto-to "message" nil t)
  (autoload 'message-goto-subject "message" nil t)
  (autoload 'message-goto-cc "message" nil t)
  (autoload 'message-goto-bcc "message" nil t)
  (autoload 'message-goto-body "message" nil t)
  (autoload 'gnus "gnus" nil t)
  (autoload 'vm-mail "vm-startup" nil t)
  (autoload 'mew-send "mew" nil t)
  (autoload 'wl-draft "wl-draft" nil t)
  (autoload 'mh-smail "mh-comp" nil t)
  (autoload 'rmail-mail "rmail" nil t)
  (autoload 'mail-to "sendmail" nil t)
  (autoload 'mail-cc "sendmail" nil t)
  (autoload 'mail-bcc "sendmail" nil t)
  (autoload 'mail-subject "sendmail" nil t)
  (autoload 'mail-text "sendmail" nil t))

(eval-when-compile
  (defalias 'mozmail-compose 'ignore))

(defun mozmail-commentary ()
  "*Display the commentary section of mozmail.el."
  (interactive)
  (with-electric-help
   '(lambda ()
      (insert
       (with-temp-buffer
	 (erase-buffer)
	 (insert (lm-commentary (locate-library "mozmail.el")))
	 (goto-char (point-min))
	 (while (re-search-forward "^;+ ?" nil t)
	   (replace-match "" nil nil))
	 (buffer-string (current-buffer)))))
   "*Mozmail Commentary*"))

(defun mozmail-copyright ()
  "*Display the copyright notice for mozmail."
  (interactive)
  (with-electric-help
   '(lambda ()
      (insert
       (with-temp-buffer
	 (erase-buffer)
	 (insert-file-contents (locate-library "mozmail.el"))
	 (goto-char (point-min))
	 (re-search-forward ";;; Commentary" nil t)
	 (beginning-of-line)
	 (narrow-to-region (point-min) (point))
	 (while (re-search-backward "^;+ ?" nil t)
	   (replace-match "" nil nil))
	 (buffer-string (current-buffer)))))
   "*Mozmail Copyright Notice*"))

(defgroup mozmail nil
  "Customisations for mozmail."
  :prefix "mozmail-"
  :group 'mail)

(defcustom mozmail-gnus-is-plugged t
  "*When non-nil use Gnus in \"plugged\" mode."
  :type 'boolean
  :group 'mozmail)

(defcustom mozmail-default-mua gnus
  "*The default \(X\)Emacs mailer to use.

Valid symbols are: gnus, vm, mhe, mew, wanderlust, rmail, emacs.

CAUTION: If you wish to set this variable outside of the custom
interface, you MUST set it via `customize-set-variable'.

For example:

    \(customize-set-variable 'mozmail-default-mua 'gnus\)"
  :type '(choice (const :tag "Gnus" :value gnus)
		 (const :tag "VM" :value vm)
		 (const :tag "MH-E" :value mhe)
		 (const :tag "MEW" :value mew)
		 (const :tag "Wanderlust" :value wanderlust)
		 (const :tag "RMail" :value rmail)
		 (const :tag "Emacs Mail" :value emacs))
  :require 'mozmail
  :initialize (lambda (symbol value)
		(progn
		  (let ((gnus 'gnus)
			(vm 'vm)
			(mhe 'mhe)
			(mew 'mew)
			(wanderlust 'wanderlust)
			(rmail 'rmail)
			(emacs 'emacs))
		    (custom-initialize-default symbol value)
		    (defalias 'mozmail-compose
		      (intern (format "mozmail-compose-%s" value)))))
		(message (format "%s set to %s" symbol value)))
  :set (lambda (symbol value)
	 (defalias 'mozmail-compose
	   (intern (format "mozmail-compose-%s" value)))
	 (message (format "%s set to %s" symbol value)))
  :group 'mozmail)

(defun mozmail-compose-gnus (to &optional subject cc bcc body)
  "Compose a mail in Gnus from a Mozilla mailto link.

Argument TO is the receipient of the mail.
Optional argument SUBJECT is the mail's subject.
Optional argument CC - carbon copy.
Optional argument BCC - blind carbon copy.
Optional argument BODY - text that will appear in the body of the
mail."
  (unless (gnus-alive-p)
    (if mozmail-gnus-is-plugged
	(gnus)
      (gnus-unplugged)))
  (gnus-group-mail)
  (message-goto-to)
  (insert (url-unhex-string to))
  (when subject
    (message-goto-subject)
    (insert (url-unhex-string subject)))
  (when cc
    (message-goto-cc)
    (insert (url-unhex-string cc)))
  (when bcc
    (message-goto-bcc)
    (insert (url-unhex-string bcc)))
  (when body
    (message-goto-body)
    (insert (url-unhex-string body 'allow-newlines))))

(defun mozmail-populate-headers (to &optional subject cc bcc body)
  "Populate the mail headers from a mailto link.

Argument TO is the receipient of the mail.
Optional argument SUBJECT is the mail's subject.
Optional argument CC - carbon copy.
Optional argument BCC - blind carbon copy.
Optional argument BODY - text that will appear in the body of the
mail."
  (mail-to)
  (insert (url-unhex-string to))
  (when subject
    (mail-subject)
    (insert (url-unhex-string subject)))
  (when cc
    (mail-cc)
    (insert (url-unhex-string cc)))
  (when bcc
    (mail-bcc)
    (insert (url-unhex-string bcc)))
  (when body
    (mail-text)
    (insert (url-unhex-string body 'allow-newlines))))

(defun mozmail-compose-vm (to &optional subject cc bcc body)
  "Compose a mail in VM from a Mozilla mailto link.

Argument TO is the receipient of the mail.
Optional argument SUBJECT is the mail's subject.
Optional argument CC - carbon copy.
Optional argument BCC - blind carbon copy.
Optional argument BODY - text that will appear in the body of the
mail."
  (vm-mail)
  (mozmail-populate-headers to subject cc bcc body))

(defun mozmail-compose-mhe (to &optional subject cc bcc body)
  "Compose a mail in MH-E from a Mozilla mailto link.

Argument TO is the receipient of the mail.
Optional argument SUBJECT is the mail's subject.
Optional argument CC - carbon copy.
Optional argument BCC - blind carbon copy.
Optional argument BODY - text that will appear in the body of the
mail."
  (mh-smail)
  (mozmail-populate-headers to subject cc bcc body))

(defun mozmail-compose-mew (to &optional subject cc bcc body)
  "Compose a mail in MEW from a Mozilla mailto link.

Argument TO is the receipient of the mail.
Optional argument SUBJECT is the mail's subject.
Optional argument CC - carbon copy.
Optional argument BCC - blind carbon copy.
Optional argument BODY - text that will appear in the body of the
mail."
  (mew-send)
  (mozmail-populate-headers to subject cc bcc body))

(defun mozmail-compose-wanderlust (to &optional subject cc bcc body)
  "Compose a mail in Wanderlust from a Mozilla mailto link.

Argument TO is the receipient of the mail.
Optional argument SUBJECT is the mail's subject.
Optional argument CC - carbon copy.
Optional argument BCC - blind carbon copy.
Optional argument BODY - text that will appear in the body of the
mail."
  (wl-draft)
  (mozmail-populate-headers to subject cc bcc body))

(defun mozmail-compose-emacs (to &optional subject cc bcc body)
  "Compose a mail in Emacs from a Mozilla mailto link.

Argument TO is the receipient of the mail.
Optional argument SUBJECT is the mail's subject.
Optional argument CC - carbon copy.
Optional argument BCC - blind carbon copy.
Optional argument BODY - text that will appear in the body of the
mail."
  (mail)
  (mozmail-populate-headers to subject cc bcc body))

(defun mozmail-compose-rmail (to &optional subject cc bcc body)
  "Compose a mail in RMail from a Mozilla mailto link.

Argument TO is the receipient of the mail.
Optional argument SUBJECT is the mail's subject.
Optional argument CC - carbon copy.
Optional argument BCC - blind carbon copy.
Optional argument BODY - text that will appear in the body of the
mail."
  (rmail-mail)
  (mozmail-populate-headers to subject cc bcc body))

(defun mozmail-split-string (string char)
  "Does `split-string-by-char' in XEmacs and `split-string' in GNU/Emacs."
  (if (featurep 'xemacs)
      ;; XEmacs
      (split-string-by-char string char)
    ;; GNU/Emacs
    (split-string string (char-to-string char))))

(defun mozmail-split-url (url sym)
  "Split a mailto URL into its various components.

Argument URL is a mailto URL.
Argument SYM is a symbol representing the field name that you
want a value for.  Valid symbols are: `to', `subject', `cc', `bcc',
and `body'."
  (let ((value nil))
    (cond ((eq sym 'to)
	   (setq value (substring (car (mozmail-split-string url ?\?)) 7)))
	  ((eq sym 'subject)
	   (setq url (cdr (mozmail-split-string url ?\?)))
	   (when url
	     (setq url (mozmail-split-string (car url) ?&))
	     (while url
	       (when (string= "subject=" (downcase (substring (car url) 0 8)))
		 (setq value (substring (car url) 8)))
	       (setq url (cdr url)))))
	  ((eq sym 'cc)
	   (setq url (cdr (mozmail-split-string url ?\?)))
	   (when url
	     (setq url (mozmail-split-string (car url) ?&))
	     (while url
	       (when (string= "cc=" (downcase (substring (car url) 0 3)))
		 (setq value (substring (car url) 3)))
	       (setq url (cdr url)))))
	  ((eq sym 'bcc)
	   (setq url (cdr (mozmail-split-string url ?\?)))
	   (when url
	     (setq url (mozmail-split-string (car url) ?&))
	     (while url
	       (when (string= "bcc=" (downcase (substring (car url) 0 4)))
		 (setq value (substring (car url) 4)))
	       (setq url (cdr url)))))
	  ((eq sym 'body)
	   (setq url (cdr (mozmail-split-string url ?\?)))
	   (when url
	     (setq url (mozmail-split-string (car url) ?&))
	     (while url
	       (when (string= "body=" (downcase (substring (car url) 0 5)))
		 (setq value (substring (car url) 5)))
	       (setq url (cdr url)))))
	  (t
	   (error 'invalid-argument sym)))
    value))

(defun mozmail (url)
  "Use an (X)Emacs MUA as the target of a Mozilla mailto link.

See `mozmail-commentary' for instructions on how to set this up in
Mozilla."
  ;; A URL that consists of just "mailto:" and nothing else is obviously
  ;; wrong.
  (when (string= (substring url 7) "")
    (error 'invalid-argument url))
  (let ((to (mozmail-split-url url 'to))
	(subject (mozmail-split-url url 'subject))
	(cc (mozmail-split-url url 'cc))
	(bcc (mozmail-split-url url 'bcc))
	(body (mozmail-split-url url 'body)))
    (mozmail-compose to subject cc bcc body)))

(provide 'mozmail)

;;; mozmail.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End:
