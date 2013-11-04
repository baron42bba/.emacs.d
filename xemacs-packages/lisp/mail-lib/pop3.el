;;; pop3.el --- Post Office Protocol (RFC 1460) interface

;; Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.
;; Copyright (C) 1997 Franklin Lee

;; Author: Richard L. Pieri <ratinox@peorth.gweep.net>
;; Author: Franklin Lee <flee@lehman.com>
;; Author: Andy Piper <andy@xemacs.org>

;; Maintainer:      Andy Piper <andy@xemacs.org>

;; Keywords: mail, pop3
;; Version: 2.06

;; Sync'ed up with: official pop3.el version 1.3s.  
;; This version is a fork of the original pop3.el and the changes have
;; not been merged back in to that version due to political difficulties.

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

;;; Commentary:

;; Most of the standard and optional Post Office Protocol version 3
;; (RFC 1460) commands are implemented.

;; This program was inspired by Kyle E. Jones's vm-pop program.

;; This version has been enhanced for speed, UIDL and regexp matching
;; of headers by Andy Piper <andy@xemacs.org>.  UIDL support has been
;; mostly stolen from epop3mail. Please address problems with this
;; version (2.xx) to me.

;; Speed has been enhanced by trying to slurp a maildrop in one
;; go. Also performance is vastly affected by how many POP3 commands
;; are issued, thus I have tried to cut down on this. Note that now
;; the single biggest time hog by a long way is
;; accept-process-output. I don't know of a good way to fix this as
;; this program is now close to the theoretical maximum as measured
;; against movemail. Under cygwin (and probably windows also) I think
;; socket I/O is just plain slow. YMMV.

;;; Setup:

;; To use this with gnus add:
;;	(setq nnmail-movemail-program 'pop3-nnmail-movemail)
;; to your .emacs

;; You may also want to add the following
;;	(setq pop3-leave-mail-on-server t)

;; If you (like me) use gnus to only pick up some of your mail you can
;; add something like:
;;	(setq pop3-retr-regexp "..:.*someaddress")
;; You probably want to leave mail on server in this instance as this
;; will prevent matching every message in your maildrop every time you
;; read mail. If you do this you may want to:
;;	(setq pop3-delete-retrieved-mail t)
;; so that mail you have actually downloaded gets deleted from your
;; maildrop.

;;; Code:

(require 'mail-utils)

(defconst pop3-version "2.06-xemacs")

(defvar pop3-maildrop (or (user-login-name) (getenv "LOGNAME") (getenv "USER") nil)
  "*POP3 maildrop.")
(defvar pop3-mailhost (or (getenv "MAILHOST") nil)
  "*POP3 mailhost.")
(defvar pop3-port 110
  "*POP3 port.")

(defvar pop3-password-required t
  "*Non-nil if a password is required when connecting to POP server.")

(defvar pop3-password nil
  "*Password to use when connecting to POP server.")

(defvar pop3-authentication-scheme 'pass
  "*POP3 authentication scheme.
Defaults to 'pass, for the standard USER/PASS authentication.  Other valid
values are 'apop.")

(defvar pop3-timestamp nil
  "Timestamp returned when initially connected to the POP server.
Used for APOP authentication.")

(defvar pop3-extended-response-beginning nil
  "Start of the region containing the last pop3 extended response.
This does NOT include the initial response from `pop3-read-response'.")

(defvar pop3-extended-response-end nil
  "End of the region containing the last pop3 extended response.")

(defvar pop3-leave-mail-on-server nil
  "Non-nil if mail is to be left on the server and UIDL used for message retrieval.")

(defvar pop3-delete-retrieved-mail nil
  "Non-nil if mail that has been retrieved is to be deleted from the server.
This is not the opposite to `pop3-leave-mail-on-server' since it is
possible to not download all mail, hence leaving mail on the server
and hence requiring the use of UIDL support, but still wanting to
delete the mail that has been downloaded. If
`pop3-leave-mail-on-server' is nil then this variable has no effect.")

(defvar pop3-retr-regexp nil
  "If non-nil only retrieve messages matching this regexp.")

(defvar pop3-maximum-message-size nil
  "If non-nil only download messages smaller than this.")

(defvar pop3-uidl-file-name "~/.uidls"
  "File in which to store the UIDL of processed messages.")

(defvar pop3-cache-password nil
  "Whether to cache the pop password or not.
If NIL ask for password each time mail is retrieved. Otherwise cache
the password.")

(defvar pop3-read-point nil)
(defvar pop3-debug nil)
(defvar pop3-uidl-support 'dont-know
  "Whether the server supports UIDL.
Nil means no, t means yes, not-nil-or-t means yet to be determined.")

(defvar pop3-utab nil
  "Uidl hash table.")

;;;###autoload
(defun pop3-nnmail-movemail (inbox crashbox)
  "Function to move mail from INBOX on a pop3 server to file CRASHBOX."
  (let ((pop3-maildrop
         (substring inbox (match-end (string-match "^po:" inbox)))))
    (pop3-movemail crashbox)))

;;;###autoload
(defun pop3-movemail (&optional crashbox)
  "Transfer contents of a maildrop to the specified CRASHBOX."
  (or crashbox (setq crashbox (expand-file-name "~/.crashbox")))
  (let* ((process (pop3-open-server pop3-mailhost pop3-port))
	 (crashbuf (get-buffer-create " *pop3-retr*"))
	 (n 1)
	 (msgid 1)
	 (msglen 0)
	 message-count
	 (password pop3-password)
	 (retrieved-messages '())
	 message-list)
    ;; for debugging only
    (if pop3-debug (switch-to-buffer (process-buffer process)))
    ;; query for password
    (if (and pop3-password-required (not pop3-password))
	(setq password
	      (pop3-read-passwd (format "Password for %s: " pop3-maildrop))))
    (cond ((equal 'apop pop3-authentication-scheme)
	   (pop3-apop process password pop3-maildrop))
	  ((equal 'pass pop3-authentication-scheme)
	   (pop3-user process pop3-maildrop)
	   (pop3-pass process password))
	  (t (error "Invalid POP3 authentication scheme.")))
    ;; cache the password if required
    (when pop3-cache-password
      (setq pop3-password password))
    ;; reset uidl support
    (unless pop3-uidl-support 
      (setq pop3-uidl-support 'dont-know))
    ;; get messages that are suitable for download
    (message "Retrieving message list...")
    (setq message-list (pop3-get-message-numbers process))
    (setq message-count (length message-list))
    (message (format "Retrieving message list...%d unread" message-count))
    ;; now get messages
    (unwind-protect
	(while (<= n message-count)
	  (setq msgid (car (car message-list)))
	  (setq msglen (cdr (car message-list)))
	  (setq message-list (cdr message-list))
	  ;; only retrieve messages matching our regexp or in the uidl list
	  (if (or (not msgid)
		  ;; don't download messages that are too large
		  (and pop3-maximum-message-size 
		       (> msglen pop3-maximum-message-size))
		  (and (or (null pop3-leave-mail-on-server)
			   ;; we top messages that are longer than 10k
			   ;; since retrieving a large message only to
			   ;; ignore it is wasteful.
			   (> msglen 10000))
		       pop3-retr-regexp
		       (not (string-match pop3-retr-regexp
					  (pop3-top process msgid)))))
	      (message (format "Ignoring message %d of %d from %s..."
			       n message-count pop3-mailhost))
	    (message (format "Retrieving message %d of %d from %s..."
			     n message-count pop3-mailhost))
	    (if (pop3-retr process msgid crashbuf)
		(setq retrieved-messages (cons msgid retrieved-messages))
	      (message (format "Retrieving message %d of %d from %s...ignored"
			       n message-count pop3-mailhost))))
	  ;; deleted a whole bunch of stuff here that updates the crashbox
	  ;; incrementally. This is way slow and mostly uneccessary, gnus
	  ;; and others will simply slurp the whole mail buffer anyway so
	  ;; why bother?
	  (setq n (+ 1 n)))
      ;; frob the crash buffer at the end
      (set-buffer crashbuf)
      (let ((coding-system-for-write 'binary))
	(append-to-file (point-min) (point-max) crashbox))
      ;; mark messages as read
      (when pop3-leave-mail-on-server
	(pop3-save-uidls))
      ;; now delete the messages we have retrieved
      (unless (and pop3-leave-mail-on-server (null pop3-delete-retrieved-mail))
	(mapcar
	 '(lambda (n)
	    (message (format "Deleting message %d of %d from %s..."
			     n message-count pop3-mailhost))
	    (pop3-dele process n)) retrieved-messages))
      (pop3-quit process))
    (kill-buffer crashbuf)
    )
  t)

(defun pop3-open-server (mailhost port)
  "Open TCP connection to MAILHOST.
Returns the process associated with the connection."
  (let ((process-buffer
	 (get-buffer-create (format "trace of POP session to %s" mailhost)))
	(process)
	(coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (save-excursion
      (set-buffer process-buffer)
      (erase-buffer))
    (setq process
	  (open-network-stream "POP" process-buffer mailhost port))
    (setq pop3-read-point (point-min))
    (let ((response (pop3-read-response process t)))
      (setq pop3-timestamp
	    (substring response (or (string-match "<" response) 0)
		       (+ 1 (or (string-match ">" response) -1)))))
    process
    ))

;; Support functions

(defun pop3-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)))

(defun pop3-send-command (process command)
    (set-buffer (process-buffer process))
    (goto-char (point-max))
;;    (if (= (aref command 0) ?P)
;;	(insert "PASS <omitted>\r\n")
;;      (insert command "\r\n"))
    (setq pop3-read-point (point))
    (goto-char (point-max))
    (process-send-string process (concat command "\r\n"))
    )

(defun pop3-read-response (process &optional return)
  "Read the response from the server.
Return the response string if optional second argument is non-nil."
  (let ((case-fold-search nil)
	match-end)
    (save-excursion
      (set-buffer (process-buffer process))
      (goto-char pop3-read-point)
      (while (not (search-forward "\r\n" nil t))
	(accept-process-output process)
	(goto-char pop3-read-point))
      (setq match-end (point))
      (goto-char pop3-read-point)
      (if (looking-at "-ERR")
	  (signal 'error (list (buffer-substring (point) (- match-end 2))))
	(if (not (looking-at "+OK"))
	    (progn (setq pop3-read-point match-end) nil)
	  (setq pop3-read-point match-end)
	  (if return
	      (buffer-substring (point) match-end)
	    t)
	  )))))

(defun pop3-string-to-list (string &optional regexp)
  "Chop up a string into a list."
  (let ((list)
	(regexp (or regexp " "))
	(string (if (string-match "\r" string)
		    (substring string 0 (match-beginning 0))
		  string)))
    (store-match-data nil)
    (while string
      (if (string-match regexp string)
	  (setq list (cons (substring string 0 (- (match-end 0) 1)) list)
		string (substring string (match-end 0)))
	(setq list (cons string list)
	      string nil)))
    (nreverse list)))

(defvar pop3-read-passwd nil)
(defun pop3-read-passwd (prompt)
  (if (not pop3-read-passwd)
      (if (load "passwd" t)
	  (setq pop3-read-passwd 'read-passwd)
	(autoload 'ange-ftp-read-passwd "ange-ftp")
	(setq pop3-read-passwd 'ange-ftp-read-passwd)))
  (funcall pop3-read-passwd prompt))

(defun pop3-clean-region (start end)
  "Convert CRLF line endings to LF line endings.
Also remove '.' from the beginning of lines.
Also escape 'From ' after '\n\n' with '>' (for mbox)."
  (setq end (set-marker (make-marker) end))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end) (search-forward "\r\n" end t))
      (replace-match "\n" t t))
    (goto-char start)
	(while (re-search-forward "\n\n\\(From \\)" end t)
	  (replace-match "\n\n>\\1" t nil))
    (goto-char start)
    (while (and (< (point) end) (re-search-forward "^\\." end t))
      (replace-match "" t t)
      (forward-char)))
  (set-marker end nil))

(defun pop3-munge-message-separator (start end)
  "Check to see if a message separator exists.  If not, generate one."
  (if (not (fboundp 'message-make-date)) (autoload 'message-make-date "message"))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (if (not (or (looking-at "From .?") ; Unix mail
		   (looking-at "\001\001\001\001\n") ; MMDF
		   (looking-at "BABYL OPTIONS:") ; Babyl
		   ))
	  (let ((from (mail-strip-quoted-names (mail-fetch-field "From")))
		(date (pop3-string-to-list (or (mail-fetch-field "Date")
					       (message-make-date))))
		(From_))
	    ;; sample date formats I have seen
	    ;; Date: Tue, 9 Jul 1996 09:04:21 -0400 (EDT)
	    ;; Date: 08 Jul 1996 23:22:24 -0400
	    ;; should be
	    ;; Tue Jul 9 09:04:21 1996
	    (setq date
		  (cond ((string-match "[A-Z]" (nth 0 date))
			 (format "%s %s %s %s %s"
				 (nth 0 date) (nth 2 date) (nth 1 date)
				 (nth 4 date) (nth 3 date)))
			(t
			 ;; this really needs to be better but I don't feel
			 ;; like writing a date to day converter.
			 (format "Sun %s %s %s %s"
				 (nth 1 date) (nth 0 date)
				 (nth 3 date) (nth 2 date)))
			))
	    (setq From_ (format "\nFrom %s  %s\n" from date))
	    (while (string-match "," From_)
	      (setq From_ (concat (substring From_ 0 (match-beginning 0))
				  (substring From_ (match-end 0)))))
	    (goto-char (point-min))
	    (insert From_)
	    (re-search-forward "\n\n")
	    (narrow-to-region (point) (point-max))
	    (let ((size (- (point-max) (point-min))))
	      (goto-char (point-min))
	      (widen)
	      (forward-line -1)
	      (insert (format "Content-Length: %s\n" size)))
	    )))))

;; UIDL support, mostly copied from epop3

(defun pop3-get-message-numbers (process)
  "Get the list of message numbers and lengths to retrieve via PROCESS."
  ;; we use the LIST comand first anyway to get the message lengths.
  ;; then if we're leaving mail on the server, see if the UIDL command
  ;; is implemented. if so, we use it to get the message number list.
  (let ((msg-list (pop3-list process))
	(uidl (if pop3-leave-mail-on-server
		  (pop3-get-uidl process)))
	(tmplist '()))
    (when msg-list
      (setq msg-list (cdr msg-list))
      (if (eq pop3-uidl-support t)
	  ;; remove elements not in the uidl, this assumes the uidl is short
	  (mapcar '(lambda (elt)
		     (when (memq (car elt) uidl)
		       (push elt tmplist))) (reverse msg-list))
	(setq tmplist msg-list))
      tmplist)))

(defun pop3-get-list (process)
  "Use PROCESS to get a list of message numbers."
  (let ((pairs (pop3-list process))
	(tmplist '()))
    (when pairs
      (mapcar '(lambda (elt)
		 (push (car elt) tmplist))
	      (cdr pairs)) tmplist)))

(defun pop3-get-uidl (process)
  "Use PROCESS to get a list of unread message numbers."
  (let ((pairs (pop3-uidl process)))
    (if (or (null pairs) (null pop3-uidl-support))
	(setq pop3-uidl-support nil)
      (setq pop3-uidl-support t)
      (pop3-init-uidl-tables)
      (mapcar 'pop3-update-tables (cdr pairs))
      (pop3-get-unread-message-numbers))))

(defun pop3-init-uidl-tables ()
  "Create the hash table for uidl processing.
This is only sensible to do when 'pop3-leave-mail-on-server' is non-nil."
  (save-excursion
    (with-temp-buffer
      (let ((uid nil))
	(when (file-readable-p pop3-uidl-file-name)
	  (insert-file-contents pop3-uidl-file-name))
	(setq pop3-utab (make-hash-table :test 'equal))
	(goto-char (point-min))
	(while (looking-at "\\([^ \n\t]+\\)")
	  (setq uid (buffer-substring (match-beginning 1) (match-end 1)))
	  (puthash uid (cons nil t) pop3-utab)
	  (forward-line 1))))))

(defun pop3-get-unread-message-numbers ()
  "Return a sorted list of unread msg numbers to retrieve."
  (let ((pop3-tmplist '()))
    (maphash '(lambda (key val)
		(if (not (cdr val))
		    (push (car val) pop3-tmplist))) pop3-utab) 
    (sort pop3-tmplist '<)))

(defun pop3-update-tables (pair)
  "Update uidl hash-tables given a PAIR list (msgno uid)."
  (let ((msgno (car pair))
        (uid (cdr pair)))
    (if (null (gethash uid pop3-utab))
	(puthash uid (cons msgno nil) pop3-utab)
      (puthash uid (cons msgno t) pop3-utab))))
    
;    (if (null (gethash msgno pop3-utab))
;        (puthash uid uid pop3-utab))))

(defun pop3-save-uidls ()
  "Save the updated UIDLs to disk for use next time."
  ;;
  ;; write the uidl, msgid to the local uidl file EXCEPT the ones which
  ;; don't have msgnos, since they've been deleted from the server
  ;;

  (when (and pop3-leave-mail-on-server
             pop3-utab
             (hash-table-count pop3-utab))
    (save-excursion
      (with-temp-buffer
        ;; back this up because we'll write to it later.
        (when (file-readable-p pop3-uidl-file-name)
          (copy-file pop3-uidl-file-name
                     (concat pop3-uidl-file-name ".old")
                     t t))
        (maphash '(lambda (key val)
		    (when (car val)
		      (insert (format "%s\n" key))))
		 pop3-utab)
        (write-file pop3-uidl-file-name)))))

;; The Command Set

;; AUTHORIZATION STATE

(defun pop3-user (process user)
  "Send USER information to POP3 server."
  (pop3-send-command process (format "USER %s" user))
  (let ((response (pop3-read-response process t)))
    (if (not (and response (string-match "+OK" response)))
	(error (format "USER %s not valid." user)))))

(defun pop3-pass (process password)
  "Send authentication information to the server."
  (pop3-send-command process (format "PASS %s" password))
  (let ((response (pop3-read-response process t)))
    (if (not (and response (string-match "+OK" response)))
	(pop3-quit process))))

(defun pop3-apop (process password user)
  "Send alternate authentication information to the server."
  (if (not (fboundp 'md5)) (autoload 'md5 "md5"))
  (let ((hash (md5 (concat pop3-timestamp password))))
    (pop3-send-command process (format "APOP %s %s" user hash))
    (let ((response (pop3-read-response process t)))
      (if (not (and response (string-match "+OK" response)))
	  (pop3-quit process)))))

;; TRANSACTION STATE

(defun pop3-stat (process)
  "Return the number of messages in the maildrop and the maildrop's size."
  (pop3-send-command process "STAT")
  (let ((response (pop3-read-response process t)))
    (list (string-to-int (nth 1 (pop3-string-to-list response)))
	  (string-to-int (nth 2 (pop3-string-to-list response))))
    ))

(defun pop3-retr (process msg crashbuf)
  "Retrieve message-id MSG to buffer CRASHBUF."
  (pop3-send-command process (format "RETR %s" msg))
  (pop3-read-response process)
;  (accept-process-output process)
  (save-excursion
    (let ((retrieved t))
      (pop3-get-extended-response process)
      (pop3-munge-message-separator pop3-extended-response-beginning
				    pop3-extended-response-end)
      ;; only get message if it matches
      (if (or (null pop3-leave-mail-on-server)
	      (null pop3-retr-regexp)
	      (and (goto-char pop3-extended-response-beginning)
		   (re-search-forward pop3-retr-regexp pop3-extended-response-end t)))
	  (append-to-buffer crashbuf pop3-extended-response-beginning
			    pop3-extended-response-end)
	;; we didn't get it
	(setq retrieved nil))
      (delete-region pop3-extended-response-beginning
		     pop3-extended-response-end)
      retrieved)))

(defun pop3-dele (process msg)
  "Mark message-id MSG as deleted."
  (pop3-send-command process (format "DELE %s" msg))
  (pop3-read-response process))

(defun pop3-noop (process msg)
  "No-operation."
  (pop3-send-command process "NOOP")
  (pop3-read-response process))

(defun pop3-last (process)
  "Return highest accessed message-id number for the session."
  (pop3-send-command process "LAST")
  (let ((response (pop3-read-response process t)))
    (string-to-int (nth 1 (pop3-string-to-list response)))
    ))

(defun pop3-rset (process)
  "Remove all delete marks from current maildrop."
  (pop3-send-command process "RSET")
  (pop3-read-response process))

;; UPDATE

(defun pop3-quit (process)
  "Close connection to POP3 server.
Tell server to remove all messages marked as deleted, unlock the maildrop,
and close the connection."
  (pop3-send-command process "QUIT")
  (pop3-read-response process t)
  (if process
      (save-excursion
	(set-buffer (process-buffer process))
	(goto-char (point-max))
	(delete-process process))))

(defun pop3-uidl (process &optional msgno)
  "Return the results of a UIDL command in PROCESS for optional MSGNO.
If UIDL is unsupported on this mail server or if msgno is invalid, return nil.
Otherwise, return a list in the form

   (N (1 UIDL-1) (2 UIDL-2) ... (N UIDL-N))

where

   N is an integer for the number of UIDLs returned (could be 0)
   UIDL-n is a string."

  (if msgno
      (pop3-send-command process (format "UIDL %d" msgno))
    (pop3-send-command process "UIDL"))
  
  (let ((uidl-not-supported nil))
    (condition-case ()
        (pop3-read-response process t)
      (error (setq uidl-not-supported t)))

    (unless uidl-not-supported
      (let ((retlist '())
            (uidl nil)
            (msgno nil))
        (save-excursion
          (pop3-get-extended-response process)
          (goto-char pop3-extended-response-beginning)
	  
          (while (looking-at "\\([^ \n\t]*\\) \\([^ \n\t]*\\)")
            (setq msgno (string-to-int
                         (buffer-substring (match-beginning 1) (match-end 1))))
            (setq uidl (buffer-substring (match-beginning 2) (match-end 2)))
            (push (cons msgno uidl) retlist)
            (beginning-of-line 2))
          (cons (length retlist) (nreverse retlist)))))))

(defun pop3-list (process &optional msgno)
  "Return the results of a LIST command for PROCESS and optional MSGNO.
If (optional) msgno is invalid, return nil.  Otherwise, return a list
in the form

   (N (1 LEN-1) (2 LEN-2) ... (N LEN-N))

where

   N is an integer for the number of msg/len pairs (could be 0)
   LEN-n is an integer."
  (let ((bad-msgno nil))

    (if msgno
	(pop3-send-command process (format "LIST %d" msgno))
      (pop3-send-command process "LIST"))

    (condition-case ()
	(pop3-read-response process t)
      (error (setq bad-msgno t)))
    
    (unless bad-msgno
      (let ((retlist '())
	    (len nil)
	    (msgno nil))
	(save-excursion
	  (pop3-get-extended-response process)
	  (goto-char pop3-extended-response-beginning)
	  
	  (while (looking-at "\\([^ \n\t]*\\) \\([^ \n\t]*\\)")
	    (setq msgno (string-to-int
			 (buffer-substring (match-beginning 1) (match-end 1))))
	    (setq len (string-to-int
		       (buffer-substring (match-beginning 2) (match-end 2))))
	    (push (cons msgno len) retlist)
	    (beginning-of-line 2))
	  (cons (length retlist) (nreverse retlist)))))))

(defun pop3-top (process msgno &optional lines)
  "Return the top LINES of messages for PROCESS and MSGNO.
If msgno is invalid, return nil.  Otherwise, return a string."
  (let ((bad-msgno nil))
    (pop3-send-command process (format "TOP %d %d" msgno (or lines 1)))
    ;; get a response
    (condition-case ()
	(pop3-read-response process t)
      (error (setq bad-msgno t)))

    (unless bad-msgno
      (save-excursion
	(pop3-get-extended-response process)
	(buffer-substring pop3-extended-response-beginning
			  pop3-extended-response-end)))))

;;; Utility code

(defun pop3-get-extended-response (process)
  "Get the extended pop3 response in the PROCESS buffer."
  (let ((start pop3-read-point) end)
    (set-buffer (process-buffer process))
    (while (not (re-search-forward "^\\.\r\n" nil t))
      (accept-process-output process)
      (goto-char start))
    (setq pop3-extended-response-beginning start)
    (setq pop3-read-point (point-marker))
    (goto-char (match-beginning 0))
    (setq end (point-marker)
	  pop3-extended-response-end (point-marker))
    (pop3-clean-region start end)))


;; Summary of POP3 (Post Office Protocol version 3) commands and responses

;;; AUTHORIZATION STATE

;; Initial TCP connection
;; Arguments: none
;; Restrictions: none
;; Possible responses:
;;  +OK [POP3 server ready]

;; USER name
;; Arguments: a server specific user-id (required)
;; Restrictions: authorization state [after unsuccessful USER or PASS
;; Possible responses:
;;  +OK [valid user-id]
;;  -ERR [invalid user-id]

;; PASS string
;; Arguments: a server/user-id specific password (required)
;; Restrictions: authorization state, after successful USER
;; Possible responses:
;;  +OK [maildrop locked and ready]
;;  -ERR [invalid password]
;;  -ERR [unable to lock maildrop]

;;; TRANSACTION STATE

;; STAT
;; Arguments: none
;; Restrictions: transaction state
;; Possible responses:
;;  +OK nn mm [# of messages, size of maildrop]

;; LIST [msg]
;; Arguments: a message-id (optional)
;; Restrictions: transaction state; msg must not be deleted
;; Possible responses:
;;  +OK [scan listing follows]
;;  -ERR [no such message]

;; TOP msg [lines]
;; Arguments: a message-id (required), number of lines (optional)
;; Restrictions: transaction state; msg must not be deleted
;; Possible responses:
;;  +OK [partial message listing follows]
;;  -ERR [no such message]

;; UIDL [msg]
;; Arguments: a message-id (optional)
;; Restrictions: transaction state; msg must not be deleted
;; Possible responses:
;;  +OK [uidl listing follows]
;;  -ERR [no such message]

;; RETR msg
;; Arguments: a message-id (required)
;; Restrictions: transaction state; msg must not be deleted
;; Possible responses:
;;  +OK [message contents follow]
;;  -ERR [no such message]

;; DELE msg
;; Arguments: a message-id (required)
;; Restrictions: transaction state; msg must not be deleted
;; Possible responses:
;;  +OK [message deleted]
;;  -ERR [no such message]

;; NOOP
;; Arguments: none
;; Restrictions: transaction state
;; Possible responses:
;;  +OK

;; LAST
;; Arguments: none
;; Restrictions: transaction state
;; Possible responses:
;;  +OK nn [highest numbered message accessed]

;; RSET
;; Arguments: none
;; Restrictions: transaction state
;; Possible responses:
;;  +OK [all delete marks removed]

;;; UPDATE STATE

;; QUIT
;; Arguments: none
;; Restrictions: none
;; Possible responses:
;;  +OK [TCP connection closed]

;; pop3 ends here
(provide 'pop3)

