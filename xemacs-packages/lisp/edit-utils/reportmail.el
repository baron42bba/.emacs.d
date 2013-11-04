;; REPORTMAIL: Display time and load in mode line of Emacs.
;; Originally time.el in the emacs distribution.
;; Mods by BCP, DCP, and JWZ to display incoming mail.
;;
;; Mods by Bob Weiner to eliminate repeated beeps on startup, to eliminate
;;   redundancy in subject display and to move mail indicator display to
;;   follow the time string.
;;
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Installation
; ------------
;
; To use reportmail, add the following to your .emacs file:
;
;    (load-library "reportmail")
;
;    ;; Edit this list as appropriate
;    (setq display-time-my-addresses
;     '("Benjamin.Pierce" "bcp" "Benjamin Pierce" "Benjamin C. Pierce"))
; 
;    ;; By default, mail arrival is reported with a message but no beep
;    (setq display-time-mail-ring-bell t)
; 
;    (display-time)
; 
; When new mail arrives, a brief blurb about it will be displayed in the
; mode line, and a more verbose message will be printed in the echo area.
; But unlike most echo-area messages, this message will not go away at
; the next keystroke - it doesn't go away until the next extended-command
; is used.  This is cool because that means you won't miss seeing the 
; subject of the newly-arrived mail because you happened to be typing when
; it arrived.
;
; But if you set the variable `display-time-flush-echo-area' to t, then this
; message will be cleared every `display-time-interval' seconds.  This means
; the message will be around for at most 30 seconds or so, which you may
; prefer.
;
; Site Configuration
; ------------------
;
; The variables display-time-incoming-mail-file and 
; display-time-message-separator identify the location and format of 
; your waiting messages.  If you are in the CMU SCS environment, or
; are on a generic BSD unix system, this code should work right away.
; Otherwise, you might need to modify the values of these to make things
; work.
;
; Junk Mail
; ---------
;
; The reportmail package has a notion of "junk mail," which can be used to
; reduce the frequency of irritating interruptions by reporting only the
; arrival of messages that seem to be interesting.  If you're on a lot
; of high-volume mailing lists, this can be quite convenient.  To use
; this facility, add something like the following to your .emacs file:
; 
;   ;; The value of this variable is a list of lists, where the first
;   ;; element in each list is the name of a header field and the
;   ;; remaining elements are various elements of the value of this
;   ;; header field that signal the junkiness of a message.  
;   (setq display-time-junk-mail-checklist
;     '(("From" "bcp" "Benjamin Pierce" "Benjamin.Pierce"
;               "Mail Delivery Subsystem" "network" "daemon@bartok")
;       ("To" "sml-request" "sml-redistribution-request" 
;        "scheme" "TeXhax-Distribution-list")
;       ("Resent-From" "Benjamin.Pierce")
;       ("Sender" "WRITERS" "Haskell" "Electronic Music Digest" "NEW-LIST")))
;   
; By default, the entries in this list are matched exactly as 
; substrings of the given header fields.  If an entry begins with 
; the character ^ it will be matched as a regular expression.  If the 
; variable display-time-match-using-regexps is set, then all entries
; will be matched as regular expressions.
;
; Note that elements of display-time-my-addresses are NOT automatically
; included in display-time-junk-mail-checklist.  If you want mail from
; yourself to be considered junkmail, you must add your addresses to 
; display-time-junk-mail-checklist too.
;
;
; Xbiff Interface
; ---------------
;
; If you normally keep your emacs window iconified, reportmail can 
; maintain an xbiff or xbiff++ display as well.  The xbiff window will only
; be highlighted when non-junk mail is waiting to be read.  For example:
;
;    (if window-system-version
;        (setq display-time-use-xbiff t))
;    (setq display-time-xbiff-arg-list '("-update" "30" "-geometry" "+0+0"))
;    (setq display-time-xbiff-program "xbiff++")
;
; Other
; -----
;
; There are several other user-customization variables that you may wish
; to modify.  These are documented below.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; HISTORY
;
; 19 dec 93	Jamie Zawinski <jwz@lucid.com>
;	Protected it from edits of the *reportmail* buffer; made the process
;	filters not interfere with the match data.
;
; 15 dec 93	Jamie Zawinski <jwz@lucid.com>
;	Kyle renamed timer.el to itimer.el; made this use the new names.
;
; 27 aug 93	Jamie Zawinski <jwz@lucid.com>
;	Use mail-extr to parse addresses if it is loadable.
;
; 15 oct 92	Benjamin Pierce (bcp@cs.cmu.edu)
;	Merged recent changes
;
; 14 oct 92	Jamie Zawinski <jwz@lucid.com>
;	Added support for xbiff++.
;
; 17 sep 92	Benjamin Pierce (bcp@cs.cmu.edu)
;	Improvements to message display code.
;
; 15 sep 92	Benjamin Pierce (bcp@cs.cmu.edu)
;	Minor bug fixes.
;
; 1 may 92	Jamie Zawinski <jwz@lucid.com>
;	Converted to work with Kyle Jones' timer.el package.
;
; 3 may 91	Jamie Zawinski <jwz@lucid.com>
;	Made the display-time-sentinel make a fuss when the process dies.
;
; 26 mar 91	Jamie Zawinski <jwz@lucid.com>
;	Merged with BCP's latest posted version
;
;  5 mar 91	Jamie Zawinski <jwz@lucid.com>
;	Added compatibility with Emacs 18.57.
;
; 25 Jan 91	Benjamin Pierce (bcp@cs.cmu.edu)
;	Added facility for regular-expression matching of junk-mail
;	checklist.  Set inhibit-local-variables to t inside of 
;	display-time-process-new-mail to prevent letterbombs 
;	(suggested by jwz).
;
; 15 feb 91	Jamie Zawinski <jwz@lucid.com>
;	Made the values of display-time-message-separator and 
;	display-time-incoming-mail-file be initialized when this code
;	starts, instead of forcing the user to do it.  This means that
;	this code can safely be dumped with emacs.  Also, it now notices
;	when it's at CMU, and defaults to something reasonable.  Removed
;	display-time-wait-hard, because I learned how to make echo-area
;	messages be persistent (not go away at the first key).  I wish
;	GC messages didn't destroy it, though...
;
; 20 Dec 90	Jamie Zawinski <jwz@lucid.com>
;	Added new variables: display-time-no-file-means-no-mail, 
;	display-time-wait-hard, and display-time-junk-mail-ring-bell.
;	Made display-time-message-separator be compared case-insensitively.
;	Made the junk-mail checklist use a member-search rather than a 
;	prefix-search.
;
; 22 Jul 90	Benjamin Pierce (bcp@cs.cmu.edu)
;	Added support for debugging.
;
; 19 Jul 90	Benjamin Pierce (bcp@cs.cmu.edu)
;	Improved user documentation and eliminated known CMU dependencies.
;
; 13 Jul 90	Mark Leone (mleone@cs.cmu.edu)
;	Added display-time-use-xbiff option.  Various layout changes.
;
; 20 May 90	Benjamin Pierce (bcp@proof)
;	Fixed a bug that occasionally caused fields to be extracted
;	from the wrong buffer.
;
; 14 May 90	Benjamin Pierce (bcp@proof)
;	Added concept of junk mail and ability to display message
;	recipient in addition to sender and subject.  (Major internal
;	reorganization was needed to implement this cleanly.)
;
; 18 Nov 89	Benjamin Pierce (bcp@proof)
;	Fixed to work when display-time is called with 
;	global-mode-string not a list
;
; 15 Jan 89	David Plaut (dcp@k)
;	Added ability to discard load from displayed string
;
;	To use: (setq display-time-load nil)
;
;	Added facility for reporting incoming mail (modeled after gosmacs
;	reportmail.ml package written by Benjamin Pierce).


(if (string-match "XEmacs" emacs-version)
    (require 'itimer))

(condition-case ()
    (require 'mail-extr)
  (error nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       User Variables                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar display-time-announce-mail t
  "*Toggles whether name of mail sender is displayed in mode line.")

(defvar display-time-announce-junk-mail-too nil
  "*When non-NIL, announce incoming junk mail as well as interesting mail")

(defvar display-time-time t
  "*Toggles whether the time is displayed.")

(defvar display-time-load nil
  "*Toggles whether machine load is displayed.")

(defvar display-time-day-and-date nil
  "*Toggles whether day and date are displayed.")

(defvar display-time-mail-ring-bell nil
  "*Toggles whether bell is rung on mail arrival.")

(defvar display-time-junk-mail-ring-bell nil
  "*Toggles whether bell is rung on junk mail arrival.
If display-time-mail-ring-bell is nil, this variable is ignored.")

(defvar display-time-my-addresses nil
  "*Report the addressee of incoming mail in the message announcement, 
unless it appears in this list  (See also display-time-match-using-regexps.)")
;; For example:
;; (setq display-time-my-addresses
;;  '("Benjamin.Pierce" "bcp" "Benjamin Pierce" "Benjamin C. Pierce"))

(defvar display-time-junk-mail-checklist nil
  "*A list of lists of strings.  In each sublist, the first component is the
name of a message field and the rest are values that flag a piece of
junk mail.  If an entry begins with the character ^ it is matched as
a regular expression rather than an exact prefix of the given header 
field.  (See also display-time-match-using-regexps.)  

Note: elements of display-time-my-addresses are NOT automatically
      included in display-time-junk-mail-checklist")
;; For example:
;; (setq display-time-junk-mail-checklist
;;  '(("From" "bcp" "Benjamin Pierce" "Benjamin.Pierce"
;;            "Mail Delivery Subsystem" "network" "daemon@bartok")
;;    ("To" "sml-request" "sml-redistribution-request" "computermusic" 
;;     "scheme" "TeXhax-Distribution-list")
;;    ("Resent-From" "Benjamin.Pierce")
;;    ("Sender" "WRITERS" "Haskell" "Electronic Music Digest" "NEW-LIST")))

(defvar display-time-match-using-regexps nil "*When non-nil, elements of 
display-time-junk-mail-checklist and display-time-my-addresses are matched
as regular expressions instead of literal prefixes of header fields.")

(defvar display-time-max-from-length 35
  "*Truncate sender name to this length in mail announcements")

(defvar display-time-max-to-length 11
  "*Truncate addressee name to this length in mail announcements")

(defvar display-time-interval 30
  "*Seconds between updates of time in the mode line.  Also used
as interval for checking incoming mail.")

(defvar display-time-no-file-means-no-mail t
  "*Set this to T if you are on a system which deletes your mail-spool file 
when there is no new mail.")

(defvar display-time-incoming-mail-file nil
  "*User's incoming mail file.  Default is value of environment variable MAIL,
if set;  otherwise /usr/spool/mail/$USER is used.")

(defvar display-time-message-separator nil)

(defvar display-time-flush-echo-area nil
  "*If true, then display-time's echo-area message will be 
automatically cleared when display-time-interval has expired.")

(defvar display-time-use-xbiff nil
  "*If set, display-time uses xbiff to announce new mail.")

(defvar display-time-xbiff-program "xbiff") ; xbiff++ if you're cool

(defvar display-time-xbiff-arg-list nil
  "*List of arguments passed to xbiff.  Useful for setting geometry, etc.")
;;; For example: 
;;; (setq display-time-xbiff-arg-list '("-update" "30" "-geometry" "+0+0"))

(defvar display-time-mail-arrived-file nil
  "New mail announcements saved in this file if xbiff used.  Deleted when 
mail is read.  Xbiff is used to monitor existence of this file.
This file will contain the headers (and only the headers) of all of the
messages in your inbox.  If you do not wish this to be readable by others, 
you should name a file here which is in a protected directory.  Protecting
the file itself is not sufficient, because the file gets deleted and
recreated, and emacs does not make it easy to create protected files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Internal Variables                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar display-time-loadst-process nil
  "The process providing time, load, and mail info.")

(defvar display-time-xbiff-process nil
  "The xbiff process used to announce incoming mail.")

(defvar display-time-string nil
  "Time displayed in mode line")

(defvar display-time-mail-buffer-name "*reportmail*"
  "Name of buffer used for announcing mail.")

(defvar display-time-may-need-to-reset t
  "Set to NIL when display-time-total-reset has not been called 
since the last time we changed from having mail in the queue to an empty
queue.")

(defvar display-time-debugging nil
  "*When non-NIL, reportmail records various status information
as it's working.")

(defvar display-time-debugging-delay nil 
   "*When non-nil and display-time-debugging is set, sit for this 
long after displaying each debugging message in mode line")

(defvar display-time-debugging-buffer "*Reportmail-Debugging*"
  "Status messages are appended here.")
  
(defvar display-time-max-debug-info 20000
  "Maximum size of debugging buffer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Macros                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro display-time-del-file (filename)
  (list 'if (list 'file-exists-p filename) (list 'delete-file filename)))

(defmacro display-time-debug (mesg &rest args)
  (list
     'if 'display-time-debugging
         (list 'display-time-debug-mesg
	       (append (list 'format mesg) args))))

(defmacro display-time-save-match-data (&rest body)
  ;; Execute the BODY forms, restoring the global value of the match data.
  ;; We need this because it's antisocial for process filters to change
  ;; the regexp match registers.
  (list 'let '((_match_data_ (match-data)))
	(list 'unwind-protect
	      (cons 'progn body)
	      '(store-match-data _match_data_))))

(defun display-time-init ()
  ;; If the mail-file isn't set, figure it out.
  (or display-time-incoming-mail-file
      (setq display-time-incoming-mail-file
	    (or (getenv "MAIL")
		(let ((user-name (or (getenv "USER") (user-login-name))))
		  (and user-name
		       (cond ((file-directory-p "/usr/spool/mail/") ; bsd
			      (concat "/usr/spool/mail/" user-name))
			     ((file-directory-p "/var/mail/") ; sysv
			      (concat "/usr/spool/mail/" user-name)))))
		"")))
  ;; If the message-separator isn't set, set it to "From " unless
  ;; the local hostname ends in ".CMU.EDU", where "^C" is used.
  (or display-time-message-separator
      (setq display-time-message-separator
	    (let ((case-fold-search t))
	      (if (string-match "\\.cmu\\.edu" (system-name))
		  "\^C"
		  "From "))))
  ;; if this isn't set, these are probably right...
  (or display-time-my-addresses
      (setq display-time-my-addresses
	    (list (user-full-name) (user-login-name))))
  ;;
  (or display-time-mail-arrived-file
      (setq display-time-mail-arrived-file
	    (expand-file-name ".mail-arrived" (user-home-directory))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Time Display                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-time-kill ()
  "Kill all display-time processes.  Done automatically if display-time
is re-invoked."
  (interactive)
  (display-time-debug "display-time-kill")
  (if display-time-loadst-process (delete-process display-time-loadst-process))
  (if display-time-xbiff-process (delete-process display-time-xbiff-process))
)

(defun display-time ()
  "Displays current time, date, load level, and incoming mail status in 
mode line of each buffer (if corresponding user variables are set)."
  (interactive)
  (display-time-debug "display-time")
  (display-time-init)
  (let ((process-connection-type nil))	; UIUCDCS mod
    (save-excursion
      (display-time-kill)
      (if (or (string-equal "" display-time-incoming-mail-file)
	      (and (not display-time-no-file-means-no-mail)
		   (not (file-exists-p display-time-incoming-mail-file))))
	  (progn 
	     (message "Reportmail: mail spool file \"%s\" not found" 
		      display-time-incoming-mail-file)
	     (sit-for 1)
	     (beep)))
      (if (not global-mode-string) (setq global-mode-string '("")))
      (if (not (listp global-mode-string))
	  (setq global-mode-string (list global-mode-string "  ")))
      (if (not (memq 'display-time-string global-mode-string))
	  (setq global-mode-string
		(append global-mode-string '(display-time-string))))
      (setq display-time-string "time and load")
      
      (if (featurep 'itimer)
	  (let ((old (get-itimer "display-time")))
	    (if old (delete-itimer old))
	    (start-itimer "display-time" 'display-time-timer-function
			  display-time-interval display-time-interval)
	    (display-time-timer-function))
	;; if we don't have timers, then use one of the process mechanisms.
	(setq display-time-loadst-process
	      (if (string-match "18\\.5[0-5]" (emacs-version))
		  (start-process "display-time-loadst" nil
				 "loadst" 
				 "-n" (int-to-string display-time-interval))
		(start-process "display-time-wakeup" nil
			       (concat exec-directory "wakeup")
			       (int-to-string display-time-interval))))
	(process-kill-without-query display-time-loadst-process)
	(set-process-sentinel display-time-loadst-process 
			      'display-time-sentinel)
	(set-process-filter display-time-loadst-process
			    (if (string-match "^18\\.5[0-5]" (emacs-version))
				'display-time-filter-18-55
			      'display-time-filter-18-57)))
      
      (if display-time-use-xbiff
	  (progn
	    (display-time-del-file display-time-mail-arrived-file)
	    (setq display-time-xbiff-process
		  (apply 'start-process "display-time-xbiff" nil
			 display-time-xbiff-program
			 "-file" display-time-mail-arrived-file
			 display-time-xbiff-arg-list))
	    (process-kill-without-query display-time-xbiff-process)
	    (sit-for 1)			; Need time to see if xbiff fails.
	    (if (/= 0 (process-exit-status display-time-xbiff-process))
		(error "Display time: xbiff failed.  Check xbiff-arg-list"))))))
  (display-time-total-reset))


(defun display-time-sentinel (proc reason)
 (display-time-save-match-data
  ;; notice if the process has died an untimely death...
  (display-time-debug "display-time-sentinel")
  (cond ((memq (process-status proc) '(stop exit closed signal))
	 (if (and (stringp reason) (string-match "\n?\n*\\'" reason))
	     (setq reason (substring reason 0 (match-beginning 0))))
	 (beep)
	 (setq display-time-string (format "%s" reason))
	 (display-time-message "")
	 (message "process %s: %s (%s)" proc reason (process-status proc))))
  (display-time-force-redisplay)))

(defun display-time-filter-18-55 (proc string)
 (display-time-save-match-data
  (if display-time-flush-echo-area (display-time-message ""))
  ;; Desired data can't need more than the last 30 chars,
  ;; so save time by flushing the rest.
  ;; This way, if we have many different times all collected at once,
  ;; we can discard all but the last few very fast.
  (display-time-debug "display-time-filter-18-55")
  (if (> (length string) 30) (setq string (substring string -30)))
  ;; Now discard all but the very last one.
  (while (and (> (length string) 4)
	      (string-match "[0-9]+:[0-9][0-9].." string 4))
    (setq string (substring string (match-beginning 0))))
  (if (string-match "[^0-9][0-9]+:" string)
      (setq string (substring string 0 (1+ (match-beginning 0)))))
  ;; If we're announcing mail and mail has come, process any new messages
  (if display-time-announce-mail
      (if (string-match "Mail" string)
	  (display-time-process-new-mail)
	  (display-time-total-reset)))
  ;; Format the mode line time display
  (let ((time-string (if (string-match "Mail" string)
			 (if display-time-announce-mail 
			     display-time-mail-modeline
			     "Mail "))))
    (if (and display-time-time (string-match "[0-9]+:[0-9][0-9].." string))
	(setq time-string 
	      (concat time-string
		      (substring string (match-beginning 0) (match-end 0))
		      " ")))
    (if display-time-day-and-date
	(setq time-string
	      (concat time-string
		      (substring (current-time-string) 0 11))))
    (if (and display-time-load (string-match "[0-9]+\\.[0-9][0-9]" string))
	(setq time-string
	      (concat time-string
		      (substring string (match-beginning 0) (match-end 0))
		      " ")))
    ;; Install the new time for display.
    (setq display-time-string time-string)
    (display-time-force-redisplay))))

(defun display-time-filter-18-57 (proc string) ; args are ignored
 (display-time-save-match-data
  (display-time-debug "display-time-filter-18-57")
  (if display-time-flush-echo-area
      (progn
	(display-time-debug "flush echo area")
	(display-time-message "")))
  (let ((mailp (and (file-exists-p display-time-incoming-mail-file)
		    (not (eq 0 (nth 7 (file-attributes
				       display-time-incoming-mail-file)))))))
    (if display-time-announce-mail
	(if mailp
	    (display-time-process-new-mail)
	    (display-time-total-reset)))
    ;; Format the mode line time display
    (let ((time-string (if mailp
			   (if display-time-announce-mail
			       display-time-mail-modeline
			       "Mail "))))
      (if display-time-time
	  (let* ((time (current-time-string))
		 (hour (read (substring time 11 13)))
		 (pm (>= hour 12)))
	    (if (> hour 12) (setq hour (- hour 12)))
	    (if (= hour 0) (setq hour 12))
	    (setq time-string
		  (concat time-string
			  (format "%d" hour) (substring time 13 16)
			  (if pm "pm " "am ")))))
      (if display-time-day-and-date
	  (setq time-string
		(concat time-string
			(substring (current-time-string) 0 11))))
      (if display-time-load
	  (setq time-string
	      (concat time-string
		      (condition-case ()
                          (let* ((la (car (load-average)))
                                 (load (if (zerop la)
                                           nil
                                         (format "%03d" la))))
                            (if load
                                (concat (substring load 0 -2)
                                        "." (substring load -2))
			      ""))
                        (error "load-error"))
		      " ")))
      ;; Install the new time for display.
      (setq display-time-string time-string)

      (display-time-force-redisplay)))))

(defun display-time-timer-function ()
  (display-time-filter-18-57 nil nil))

(defun display-time-force-redisplay ()
  "Force redisplay of all buffers' mode lines to be considered."
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  ;; Do redisplay right now, if no input pending.
  (sit-for 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Mail processing                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar display-time-mail-who-from ""
  "Short-form name of sender of last piece of interesting unread mail")

(defvar display-time-mail-modeline ""
  "Terse mail announcement (displayed in modeline)")

(defvar display-time-previous-mail-buffer-max 1
  "The length of the mail buffer the last time we looked at it")

(defvar display-time-msg-count 0
  "How many interesting messages have arrived")

(defvar display-time-junk-msg-count 0
  "How many junk messages have arrived")

(defvar display-time-last-message nil) ; enormous hack


;; A test procedure for trying out new display-time features
;(defun display-time-test ()
;  (interactive)
;  (display-time-reset-mail-processing)
;  (display-time-process-new-mail))

(defun display-time-manual-reset ()
  "Utility function to be called externally to make reportmail notice
that things may have changed."
  (display-time-debug "Manual reset")
  (display-time-timer-function))

(defun display-time-total-reset ()
  (display-time-debug "display-time-total-reset")
  (if display-time-may-need-to-reset
   (progn
    (setq display-time-may-need-to-reset nil)
    (display-time-debug "Resetting mail processing")
    (let ((mail-buffer (get-buffer display-time-mail-buffer-name)))
      (cond (mail-buffer
	     ;; unmodify it before killing it in case it has accidentally
	     ;; been typed in to.
	     (save-excursion
	       (set-buffer mail-buffer)
	       (set-buffer-modified-p nil))
	     (kill-buffer mail-buffer))))
    (if display-time-use-xbiff
	;; This function is only called when no mail is in the spool.
	;; Hence we should delete the mail-arrived file.
	(display-time-del-file display-time-mail-arrived-file))
    (display-time-reset)
    )))

(defun display-time-reset ()
  (display-time-debug "display-time-reset")
  (setq display-time-msg-count 0)
  (setq display-time-junk-msg-count 0)
  (setq display-time-mail-who-from "Junk mail")
  (setq display-time-mail-modeline "")
  (setq display-time-previous-mail-buffer-max 1)
  (display-time-message "") ; clear the echo-area.
  )

(or (fboundp 'buffer-disable-undo)
    (fset 'buffer-disable-undo 'buffer-flush-undo))

(defun display-time-process-new-mail ()
  (setq display-time-may-need-to-reset t)
  (let ((mail-buffer (get-buffer display-time-mail-buffer-name))
	(inhibit-local-variables t)
	(enable-local-variables nil))
    (if (not (and mail-buffer (verify-visited-file-modtime mail-buffer)))
      (save-window-excursion
       (save-excursion
	(display-time-debug "Spool file has changed... rereading...")
	(cond (mail-buffer
	       ;; unmodify it before killing it in case it has accidentally
	       ;; been typed in to.
	       (save-excursion
		 (set-buffer mail-buffer)
		 (set-buffer-modified-p nil))
	       (kill-buffer mail-buffer))))
	;; Change to pop-to-buffer when we're debugging:
	(set-buffer (get-buffer-create display-time-mail-buffer-name))
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(condition-case nil
	    ;; I wish we didn't have to mark the buffer as visiting the file,
	    ;; since that interferes with the user's ability to use find-file
	    ;; on their spool file, but there's no way to simulate what
	    ;; verify-visited-file-modtime does.  Lose lose.
	    (let ((buffer-read-only nil))
	      (insert-file-contents display-time-incoming-mail-file t))
	  (file-error nil))
	;; this buffer belongs to us; hands off.
	(setq buffer-read-only t)
	(display-time-process-mail-buffer)))))

(defun display-time-process-mail-buffer ()
  (if (< display-time-previous-mail-buffer-max (point-max))
      (let ((case-fold-search nil))
	(goto-char display-time-previous-mail-buffer-max)
	(if (not (looking-at
		  (regexp-quote display-time-message-separator)))
	    (display-time-reset)))
    (display-time-reset))
  (goto-char display-time-previous-mail-buffer-max)
  (if display-time-use-xbiff
      (save-excursion
	(set-buffer (get-buffer-create " *reportmail-tmp*"))
	(erase-buffer)))
  (let ((case-fold-search nil)
	(start (point))
	end junkp ring-bell)
    (while (not (eobp))
      (if (search-forward (concat "\n" display-time-message-separator)
			  nil 'end)
	  (setq end (1+ (match-beginning 0)))
	(setq end (point-max)))
      (narrow-to-region start end)
      (setq junkp (display-time-process-this-message))
      (if (and display-time-mail-ring-bell (not ring-bell))
	  (setq ring-bell (if junkp display-time-junk-mail-ring-bell t)))
      (widen)
      (goto-char (if (= end (point-max)) (point-max) (1+ end)))
      (setq start end))

    (if ring-bell
	(if (string-match "XEmacs" emacs-version)
	    (beep nil 'reportmail)
	  (beep))))
  
  (if display-time-use-xbiff
      (save-excursion
	(set-buffer (get-buffer-create " *reportmail-tmp*"))
	(if (zerop (buffer-size))
	    nil
	  (write-region (point-min) (point-max)
			display-time-mail-arrived-file
			t 'nomsg)
	  (erase-buffer)
;	  ;; there's no way to get append-to-file to not dump the message
;	  ;; "Wrote file ..." in the echo area, so re-write the last message
;	  ;; we intended to write.
;	  (if display-time-last-message
;	      (display-time-message "%s" display-time-last-message))
	  )))
  
  (setq display-time-previous-mail-buffer-max (point-max)))

(defun display-time-process-this-message ()
  (display-time-debug "display-time-process-this-message")
  (let ((junk-p (display-time-junk-message)))
    (if junk-p
	(display-time-process-junk-message)
      (display-time-process-good-message))
    ;; Update mode line contents
    (setq display-time-mail-modeline 
	  (concat "[" (display-time-format-msg-count) 
		  display-time-mail-who-from
		  "] "))
    (display-time-debug "New mode line: %s " display-time-mail-modeline)
    junk-p))

(defun display-time-junk-message ()
  "Check to see whether this message is interesting"

  (display-time-debug "Comparing current message to junk mail checklist")

  (let ((checklist display-time-junk-mail-checklist)
	(junk nil))
    (while (and checklist (not junk))
      (if (display-time-member 
	   (display-time-get-field (car (car checklist)))
	   (cdr (car checklist)))
	  (setq junk t)
	  (setq checklist (cdr checklist))))
    junk))

(defun display-time-message (&rest message-args)
  (let ((str (apply 'format message-args))
	(in-echo-area-already (eq (selected-window) (minibuffer-window))))
    (setq display-time-last-message str)
    ;; don't stomp the echo-area-buffer if reading from the minibuffer now.
    (display-time-debug "display-time-message (%s)" str)
    (if (not in-echo-area-already)
	(save-excursion
	  (save-window-excursion
	    (display-time-debug "Overwriting echo area with message")
	    (select-window (minibuffer-window))
	    (delete-region (point-min) (point-max))
	    (insert str))))
    ;; if we're reading from the echo-area, and all we were going to do is
    ;; clear the thing, like, don't bother, that's annoying.
    (if (and in-echo-area-already (string= "" str))
	nil
      (if (and (string= str "") (string-match "^19" emacs-version))
	  (message nil)
	(message "%s" str)))))

(defun display-time-process-good-message ()
  (display-time-debug "Formatting message announcement (good message)")

  ;; Update the message counter
  (setq display-time-msg-count (+ display-time-msg-count 1))

  ;; Format components of announcement
  (let* ((subject (display-time-get-field "Subject" ""))
	 (from (display-time-get-field "From" ""))
	 (to (display-time-get-field "To" ""))
	 (print-subject (if (string= subject "")
			    ""
			    (concat " (" subject ")")))
	 (print-from (display-time-truncate from display-time-max-from-length))
	 (short-from (display-time-truncate 
		      (display-time-extract-short-addr from) 25))
	 (print-to (if (display-time-member to display-time-my-addresses)
		       ""
		       (display-time-truncate 
			(display-time-extract-short-addr to)
			display-time-max-to-length))))

    ;; Announce message
    (let ((msg (concat 
		   (display-time-format-msg-count)
		   "Mail " 
		   (if (string= print-to "") "" 
		       (concat "to " print-to " "))
		   "from " print-from 
		   print-subject)))
      (if display-time-use-xbiff
	  (save-excursion
	    (let* ((tmp-buf (get-buffer-create " *reportmail-tmp*"))
		   (buf (current-buffer))
		   (start (point-min))
		   (end (save-excursion
			  (goto-char start)
			  (search-forward "\n\n" nil 0)
			  (point))))
	      (set-buffer tmp-buf)
	      (goto-char (point-max))
	      (insert-buffer-substring buf start end)
	      (insert "\n\n")
	      )))
      (display-time-debug "Message: %s" msg)
      (display-time-message "%s" msg))
    ;; Update mode line information
    (setq display-time-mail-who-from short-from)))

(defun display-time-process-junk-message ()
  (display-time-debug "Formatting message announcement (junk message)")

  ;; Update the message counter
  (setq display-time-junk-msg-count (+ display-time-junk-msg-count 1))

  ;; Format components of announcement
  (let* ((subject (display-time-get-field "Subject" ""))
	 (from (display-time-get-field "From" ""))
	 (to (display-time-get-field "To" ""))
	 (print-subject (if (string= subject "")
			    ""
			    (concat " (" subject ")")))
	 (print-from (display-time-truncate from display-time-max-from-length))
	 (short-from (display-time-truncate 
		      (display-time-extract-short-addr from) 25))
	 (print-to (if (display-time-member to display-time-my-addresses)
		       ""
		       (display-time-truncate 
			(display-time-extract-short-addr to)
			display-time-max-to-length))))

    ;; Announce message
    (if display-time-announce-junk-mail-too
	  (let ((msg (concat 
		      (display-time-format-msg-count)
		      "Junk Mail " 
		      (if (string= print-to "") "" 
			(concat "to " print-to " "))
		      "from " print-from 
		      print-subject)))
	    (display-time-message "%s" msg)
	    (display-time-debug "Message: %s" msg)))))

(defun display-time-format-msg-count ()
   (if (> (+ display-time-msg-count display-time-junk-msg-count) 1) 
       (concat 
	(int-to-string display-time-msg-count) 
	(if (> display-time-junk-msg-count 0)
	    (concat "(" (int-to-string display-time-junk-msg-count) ")"))
	": ")
       ""))

(defun display-time-get-field (field &optional default)
  (cond ((not (equal (buffer-name) display-time-mail-buffer-name))
    (beep)
    (message "reportmail bug: processing buffer %s, not %s"
	     (buffer-name)
	     display-time-mail-buffer-name)
    (sit-for 2)))
  (goto-char (point-min))
  (let* ((case-fold-search t)
	 (result
	 (if (re-search-forward (concat "^" field ":[ |\C-i]*") nil t)
	     (let ((start (point)))
	       (end-of-line)
	       (while (looking-at "\n[ \t]")
		 (forward-line 1)
		 (end-of-line))
	       (buffer-substring start (point)))
	     (or default "<unknown>"))))
    (display-time-debug "value of %s field is %s" field result)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Auxilliary Functions                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-time-member (e l)
  "Is string E matched by an element of list L?
When an element of L begins with ^, match it as a regexp.  Otherwise,
ignore case and match exactly.  If display-time-match-using-regexps is
non-nil, always match using regexps."
  (let ((done nil)
	(result nil))
    (while (not done)
      (cond 
       ((null l) (setq done t))
       ((or display-time-match-using-regexps (= (elt (car l) 0) ?^))
	(if (string-match (car l) e)
	    (setq result l done t)
	  (setq l (cdr l))))
       ((string-match (regexp-quote (downcase (car l))) (downcase e)) 
	(setq result l done t))
       (t 
	(setq l (cdr l)))))
    result))

(defun display-time-truncate (s max)
  (if (and s (>= (length s) max))
      (concat (substring s 0 max) "\\")
      s))

(defun display-time-extract-short-addr (long-addr)
  (let ((result (and (fboundp 'mail-extract-address-components)
		     (mail-extract-address-components long-addr))))
    (or (nth 0 result)  ; hairily extracted real name
	(let ((name "\\([A-Za-z0-9-_+\\. ]+\\)"))
	  (setq long-addr (or (nth 2 result) long-addr))
	  (if (or 
	       ;; David Plaut <dcp@CS.CMU.EDU>	 -> David Plaut
	       ;; (doesn't happen if mail-extr loaded)
	       (string-match (concat name "[ |	]+<.+>") long-addr)
	
	       ;; anything (David Plaut) anything	 -> David Plaut
	       ;; (doesn't happen if mail-extr loaded)
	       (string-match ".+(\\(.+\\)).*" long-addr)
	 
	       ;; plaut%address.bitnet@vma.cc.cmu.edu -> plaut
	       (string-match (concat name "%.+@.+") long-addr)

	       ;; random!uucp!addresses!dcp@uu.relay.net -> dcp
	       (string-match (concat ".*!" name "@.+") long-addr)

	       ;; David.Plaut@CS.CMU.EDU		 -> David.Plaut
	       (string-match (concat name "@.+") long-addr)
	       )
	      (substring long-addr (match-beginning 1) (match-end 1))
	    long-addr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       Debugging Support                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar display-time-debugging-messages nil
  "When non-NIL, reportmail displays status messages in real time.")

(defun display-time-debug-mesg (mesg)
 (display-time-save-match-data
  (if display-time-debugging-messages
      (progn 
	(message "Reportmail: %s" mesg)
	(sit-for 1)
	))
  (save-excursion
    (save-window-excursion
      (set-buffer (get-buffer-create display-time-debugging-buffer))
      (goto-char (point-max))
      (insert (substring (current-time-string) 11 16) "  " mesg "\n")
      ;; Make sure the debugging buffer doesn't get out of hand
      (if (> (point-max) display-time-max-debug-info)
	  (delete-region (point-min) 
			 (- (point-max) display-time-max-debug-info)))))
  (if display-time-debugging-delay
      (progn (message "Reportmail: %s" mesg)
	     (sit-for display-time-debugging-delay)))))

(provide 'reportmail)
