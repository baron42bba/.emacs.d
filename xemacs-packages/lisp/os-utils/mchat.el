;;; mchat.el --- Multicast Chatting package for XEmacs

;; Copyright (C) 1997-2000 Didier Verna.

;; Author:        Didier Verna <didier@xemacs.org>
;; Maintainer:    Didier Verna <didier@xemacs.org>
;; Created:       Mon Feb 15 18:45:02 1999 under XEmacs 21.2 (beta 9)
;; Last Revision: Fri Aug  4 13:47:56 2000
;; Keywords:      comm processes

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; Contents management by FCM version 0.1.

;; MChat is a package allowing a conversation to take place between a
;; potentially infinite number of people across the Internet. It uses the
;; Multicast support that I've added to XEmacs in the early 21.0 days. At that
;; time a rudimentary version of MChat was written, merely to illustrate the
;; feature, and almost nobody knew it, appart from the other developpers. This
;; version is much improved and completely backward incompatible ;-).

;; #### NOTE: before using this package, you most likely have to customize the
;; `mchat-nsl-method' first. It specifies a program like nsl or nslookup in
;; order to retrieve the IP number of your machine.

;; The main entry point to the package is the `mchat' function which allows
;; you to open a multicast group based on the MChat protocol. There are a
;; number a variables that you can customize in the 'mchat custom group. Their
;; doc-string is rather self-explanatory I think. You might also want to look
;; at the bindings (C-h b) in MChat buffers, or at the menubar entries to get
;; an idea of what you can do.

;; #### WARNING: the current version (but there's also a limitation in the
;; internals of XEmacs's processes) doesn't let you send messages longer than
;; 500 octets or so, including the protocol header. I plan to change this in
;; the future (see the todo list).


;;; Acknowledgements:

;; The original idea of MChat is from Philippe Dax <dax@infres.enst.fr>. Akim
;; Demaille <akim@epita.fr> was *THE* beta tester for the 2.0 release. Many
;; good ideas come from him.


;;; TODO:

;; - implement fragmentation support. When we have a proper packaging scheme
;;   for providing both lisp and C modules, I'll probably rewrite the
;;   multicast support as a module because the current process abstraction is
;;   not very well suited to it. In UDP, you'd like to have access to more of
;;   the datagrams components, and use proper non-connected mode routines to
;;   send/receive to/from the network.
;; - maybe implement dynamic heart-bit


;;; Change Log:


;;; Code:


;; User Configurable stuff ==================================================

(defgroup mchat nil
  "Multicast Chatting package.")

(defcustom mchat-user-tag (user-full-name)
  "*The tag to use to identify your messages."
  :group 'mchat
  :type 'string)

(defcustom mchat-registered-groups nil
  "*Alist of registered mchat groups. Each element looks like
(NAME ADDRESS) where NAME is a string used to identify the group, and
ADDRESS is the multicast address. NAME must be unique. Please refer to
`open-multicast-group' for the syntax of ADDRESS."
  :group 'mchat
  :type '(repeat (group (string :tag "   Name")
		        (string :tag "Address"))))

(defcustom mchat-message-cache-size 32
  "*Number of sent messages to remember for each MChat group."
  :group 'mchat
  :type 'integer)

(defcustom mchat-circulate-messages nil
  "*If non nil, consider the message caches as circular lists, allowing
to jump directly from one end to the other."
  :group 'mchat
  :type 'boolean)

(defcustom mchat-ring-sound t
  "*Sound to use when somebody rings an MChat group (see `sound-alist').
Otherwise, t means just beep and nil means don't ever produce any sound."
  :group 'mchat
  :type 'symbol)

(defcustom mchat-flash-on-message 'needed
  "*When you receive a message in an MChat group, the selected frame can
be flashed according to the value of this variable:
- if t, always flash when a message arrives. As a special exception, this
  doesn't flash when you are in one of the two MChat buffers corresponding
  to this group.
- if 'needed, flash only when the MChat buffer is not visible,
- if nil, never flash.

NOTE: currently, it's not always possible to figure out exactly whether
the MChat buffer is visible (for instance if its frame is mapped, but
obscured by some other window)."
  :group 'mchat
  :type '(choice (const :tag "always" t)
		 (const :tag "needed" needed)
		 (const :tag "never" nil)))

(defcustom mchat-major-mode-string "MChat"
  "*String to use in the modeline when the MChat major mode is active."
  :group 'mchat
  :type 'string)

(defcustom mchat-major-mode-hooks nil
  "*Hooks to run after setting up the MChat major mode."
  :group 'mchat
  :type 'hook)

(defcustom mchat-minor-mode-string " mchat"
  "*String to use in the modeline when the MChat minor mode is active."
  :group 'mchat
  :type 'string)

(defcustom mchat-minor-mode-hooks nil
  "*Hooks to run after setting up the MChat minor mode."
  :group 'mchat
  :type 'hook)

(defcustom mchat-before-send-hooks nil
  "*Hooks to run before sending a message. The hooks are executed in the
MChat message buffer."
  :group 'mchat
  :type 'hook)

(defcustom mchat-treat-message-hooks nil
  "*Hooks to run after a message is received. The hooks will be called with
two arguments, the beginning and end position of the message in the MChat
group buffer. The group buffer will be current when the hooks are executed."
  :group 'mchat
  :type 'hook)

(defcustom mchat-message-buffer-major-mode 'text
  "*Major mode to use for MChat message buffers (less the -mode suffix)."
  :group 'mchat
  :type 'symbol)

(defcustom mchat-loaded-hooks nil
  "*Hooks run when mchat is loaded."
  :group 'mchat
  :type 'hook)

(defcustom mchat-window-percent 75
  "*Percentage of frame occupied by the Group buffer. The rest will go to the
message buffer."
  :group 'mchat
  :type 'integer)

(defcustom mchat-nsl-method
  '("nsl"
    (system-name)
    "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"
    1)
  "*Method to use in order to retrieve one IP address from your machine. It
looks like (PROGRAM ARGS REGEXP START) where:
- PROGRAM is the program to be called (a string)
- ARGS is a lisp expression returning a string of arguments you might want to
  pass to PROGRAM,
- REGEXP is the regexp to find the IP address in the output of PROGRAM,
- START is the indice of the sub-match returning the first component of the
  address."
  :group 'mchat
  :type '(list (string  :tag "    Program")
	       (sexp    :tag "  Arguments")
	       (string  :tag "     Regexp")
	       (integer :tag "First match")))

(defface mchat-comment-face '((t (:italic t)))
  "*Face used when inserting text that is not a message in the MChat group
buffers."
  :group 'mchat)

(defface mchat-tag-face '((t (:bold t)))
  "*Face used for identifying the sender of a message."
  :group 'mchat)


;; Private variables ========================================================

;; $Format: "(defconst mchat-prcs-major-version \"$ProjectMajorVersion$\")"$
(defconst mchat-prcs-major-version "version-2-1")
;; $Format: "(defconst mchat-prcs-minor-version \"$ProjectMinorVersion$\")"$
(defconst mchat-prcs-minor-version "1")
(defconst mchat-version
  (let ((level mchat-prcs-minor-version)
	major minor status)
    (string-match "\\(branch\\|version\\)-\\([0-9]+\\)-\\([0-9]+\\)"
		  mchat-prcs-major-version)
    (setq major (match-string 2 mchat-prcs-major-version)
	  minor (match-string 3 mchat-prcs-major-version)
	  status (match-string 1 mchat-prcs-major-version))
    (cond ((string= status "version")
	   (setq level (int-to-string (1- (string-to-int level))))
	   (if (string-equal level "0")
	       (concat major "." minor)
	     (concat major "." minor "." level)))
	  ((string= status "branch")
	   (concat major "." minor "-b" level)))
    ))

;; #### NOTE: the group informations stored in each entry are constant stuff.
;; Things likely to change (like members) are stored in buffer-local
;; variables.
(defvar mchat-groups nil
  ;; List of all currently active mchat groups.
  ;; Each element of the list is of the form: (NAME ADDRESS (KEY . VALUE) ...)
  ;; NAME is the name use to identify the group,
  ;; ADDRESS is the multicast address for the group,
  ;; KEY can currently be one of the following:
  ;; buffer:       VALUE is the message edition buffer for this group,
  ;; proc:         VALUE is the process id associated with this group,
  ;; zombie-check: VALUE is the timeout id corresponding to the zonbie check.
  )

;; Used to retrieve back the group desc from mchat-groups.
(make-variable-buffer-local
 (defvar mchat-group-name nil
   ;; Becomes automatically buffer local when set. Contains the name of the
   ;; MChat group the buffer is associated with.
   ))

;; See the note on top of `mchat-groups'
(make-variable-buffer-local
 (defvar mchat-group-members nil
   ;; Becomes automatically buffer local when set. This is a list of group
   ;; members. Each element looks like: (ID (KEY . VALUE) ...)
   ;; ID is the unique identifier for each member.
   ;; KEY can be one the following:
   ;; tag:        VALUE is the tag to used to identify messages from this guy,
   ;; time-stamp: VALUE is the last time I received a message from this guy.
   ;; messges:    VALUE is a list of buffered messages to be processed *after*
   ;;             we get information on the guy (especially the tag).
   ))

(make-variable-buffer-local
 (defvar mchat-heart-bit-timeout nil
   ;; Becomes automatically buffer local when set. The heart-bit timeout id.
   ))

(make-variable-buffer-local
 (defvar mchat-message-cache nil
   ;; Becomes automatically buffer local when set. The cache of messages
   ;; already sent.
   ))

(make-variable-buffer-local
 (defvar mchat-message-cache-indice nil
   ;; Becomes automatically buffer local when set. The cache indice of the
   ;; message currently displayed in the mchat message buffer.
   ))

(make-variable-buffer-local
 (defvar mchat-unsent-message nil
   ;; Becomes automatically buffer local when set. Remembers a message being
   ;; edited we're moving in the messages history
   ))

(defconst mchat-submenu
  '("MChat"
    :included mchat-group-name
    "Message:"
    "-"
    [ "send it" mchat-message t ]
    [ "discard it" mchat-discard-message t ]
    [ "pop next" mchat-next-message t ]
    [ "pop previous" mchat-previous-message t ]
    [ "pop first" mchat-first-message t ]
    [ "pop last" mchat-last-message t ]
    "Group:"
    "--"
    [ "erase buffer" mchat-erase-buffer t ]
    [ "ring bell" mchat-ring t ]
    [ "show who's here" mchat-who t ]
    [ "show info" mchat-info t ]
    ;;[ "listening is " mchat-suspend t "on" ]
    [ "quit" mchat-quit t ]
    "Control:"
    "---"
    [ "register group" mchat-register t ]
    [ "unregister group" mchat-unregister t ]
    [ "switch group" mchat-switch-to-group t ]
    [ "bury group" mchat-bury t ]
    "Misc."
    "----"
    [ "MChat version" mchat-version t ]
    )
  ;; MChat-menu definition.
  )


;; MChat protocol management routines =======================================

;; Here's a description of the MChat protocol (version 2.0) in pseudo-BNF:

;; MESSAGE := PROTO_VERSION USER_ID LENGTH DATA
;; PROTO_VERSION := PROTO_MAJOR (1 octet) PROTO_MINOR (1 octet)
;;                  (see `mchat-protocol-*-version')
;; USER_ID := USER_IP USER_PID
;; LENGTH := length of the data segment (2 octets)
;; DATA := ATOM ATOM_DATA
;; USER_IP := one IP address of the machine we're running on (4 octets)
;; USER_PID := PID of the process running an MChat client (8 octets)

;; ATOM <ATOM_DATA>: meaning (see `mchat-protocol-atom-*')
;; 'join <user tag>:     join the group
;; 'quit <nothing>:      quit the group
;; 'whois <user id>:     ask <user id> for info
;; 'iam <user tag>:      send info on me
;; 'ring <nothing>:      ring the group
;; 'msg  <text>:         send a message to the group
;; 'heart-bit <nothing>: just show people that we exist

;; The user id serves for identifying uniquely each member of the group. Some
;; of them could use the same tag however.

;; The main use of the LENGTH field is because of possible messages
;; concatenation. It's then important to separate each message from the next
;; one.

;; - 'join and 'quit are sent once.
;; - 'iam should be sent if somebody sent a 'whois request about me.
;; - 'whois should be sent when we receive something (except 'join and 'iam)
;;   about a member we don't know.
;; 'heart-bit should be sent only if we were idle for a long time.


;; Each one must be representable on 8 bits, but I doubt I'll ever reach 255.
;; $Format: "(defconst mchat-protocol-major-version $ProtocolMajorVersion$)"$
(defconst mchat-protocol-major-version 2)
;; $Format: "(defconst mchat-protocol-minor-version $ProtocolMinorVersion$)"$
(defconst mchat-protocol-minor-version 0)

(defconst mchat-protocol-version
  (let ((str "\0\0"))
    (aset str 0 (int-to-char mchat-protocol-major-version))
    (aset str 1 (int-to-char mchat-protocol-minor-version))
    str))

(defconst mchat-heart-bit-delay 60
  ;; Maximum inactivity period.
  )

(defconst mchat-zombie-latency 5
  ;; If I didn't receive any message (heart-bit included) from a member after
  ;; this number of times the heart-bit, the guy must have been killed.
  )

(defvar mchat-user-id nil
  ;; #### FIXME: is it necessary to use 64bits for pids ?
  ;; 12 octets header for all mchat messages. Octets list with explanation:
  ;; 0- 3 -> IP address
  ;; 4-12 -> emacs pid on 64 bits
  ;; The pair IP-addres/emacs pid makes a unique identifier for everybody.
  )

(defun mchat-user-id ()
  ;; Build and return the user id, or nil.
  (condition-case nil
      (let ((str (make-string 12 0))
	    (pid (emacs-pid))
	    (i 0)
	    (n (cadddr mchat-nsl-method)))
	(with-temp-buffer
	  (call-process (car mchat-nsl-method)
			nil (current-buffer)
			nil (eval (cadr mchat-nsl-method)))
	  (goto-char (point-min))
	  (re-search-forward (caddr mchat-nsl-method))
	  (while (< i 4)
	    (aset str i
		  (int-to-char (string-to-int (match-string (+ i n)))))
	    (setq i (1+ i)))
	  (setq i 11)
	  (while (> i 4)
	    (setq n (% pid 256))
	    (aset str i (int-to-char n))
	    (setq pid (/ (- pid n) 256))
	    (setq i (1- i))))
	str)
    (t nil)
    ))

;; MChat protocol atoms
(defconst mchat-protocol-atom-join      (int-to-char 0))
(defconst mchat-protocol-atom-quit      (int-to-char 1))
(defconst mchat-protocol-atom-whois     (int-to-char 10))
(defconst mchat-protocol-atom-iam       (int-to-char 11))
(defconst mchat-protocol-atom-ring      (int-to-char 50))
(defconst mchat-protocol-atom-msg       (int-to-char 100))
(defconst mchat-protocol-atom-heart-bit (int-to-char 255))

;; Message sending routines ------------------------------------------------

;; #### NOTE: should implement message fragmentation
(defun mchat-send (proc data)
  ;; send DATA to the process PROC, in the MChat protocol format. Disable the
  ;; heart bit before sending, and restores it afterwards.
  (let ((data-len "\0\0")
	(len (length data))
	n)
    (with-current-buffer (process-buffer proc)
      (disable-timeout mchat-heart-bit-timeout)
      ;; compute and add the length of the message
      (setq n (% len 256))
      (aset data-len 1 (int-to-char n))
      (setq n (/ (- len n) 256))
      (aset data-len 0 n)
      (process-send-string proc (concat mchat-protocol-version
					mchat-user-id
					data-len
					data))
      (setq mchat-heart-bit-timeout
	    (add-timeout mchat-heart-bit-delay 'mchat-heart-bit proc
			 mchat-heart-bit-delay)))
    ))

(defun mchat-send-join (proc)
  ;; Sends a `join' message
  (mchat-send proc (concat (char-to-string mchat-protocol-atom-join)
			   mchat-user-tag)))

(defun mchat-send-quit (proc)
  ;; Sends a `quit' message
  (mchat-send proc (char-to-string mchat-protocol-atom-quit)))

(defun mchat-send-whois (proc data)
  ;; Sends a `whois' message
  (mchat-send proc (concat (char-to-string mchat-protocol-atom-whois)
			   data)))

(defun mchat-send-iam (proc)
  ;; Sends a `iam' message
  (mchat-send proc (concat (char-to-string mchat-protocol-atom-iam)
			   mchat-user-tag)))

(defun mchat-send-ring (proc)
  ;; Sends a `ring' message
  (mchat-send proc (char-to-string mchat-protocol-atom-ring)))

(defun mchat-send-msg (proc data)
  ;; Sends a real text message
  (mchat-send proc (concat (char-to-string mchat-protocol-atom-msg)
			   data)))

(defun mchat-send-heart-bit (proc)
  ;; sends the heart-bit message
  (mchat-send proc (char-to-string mchat-protocol-atom-heart-bit)))


;; Message receiving routines ----------------------------------------------

;; stolen from gnus.
(defun mchat-current-time ()
  ;; Return the current time in seconds
  (let ((tm (current-time)))
    (+ (* (car tm) 65536.0)
       (cadr tm)
       (/ (or (caddr tm) 0) 1000000.0))
    ))

(defmacro mchat-with-face (face &rest body)
  ;; Execute BODY and put the result in face FACE
  `(let ((start (point)))
     ,@body
     (set-extent-face (make-extent start (point)) ,face)
     ))
(put 'mchat-with-face 'lisp-indent-function 1)

(defmacro mchat-insertion (&rest body)
  ;; Execute BODY in current buffer as if it was a process output, and put it
  ;; in face FACE.
  ;; #### WARNING: BODY must use `insert-before-markers', not `insert'.
  `(save-excursion
     (let ((proc (get-buffer-process (current-buffer)))
	   buffer-read-only)
       (goto-char (process-mark proc))
       ,@body
       (set-marker (process-mark proc) (point)))
     ))
(put 'mchat-insertion 'lisp-indent-function 0)

;; #### NOTE: For some reason, I've noticed that I'm likely to receive several
;; messages twice. I couldn't figure out why in the multicast API. Maybe a
;; problem of loopback of some sort ? Just ignore doublets anyway.
(defun mchat-treat-join (grp id data) ;; DATA is the user tag
  (let* ((proc (cdr (assoc 'proc grp)))
	 (buf (process-buffer proc))
	 who)
    (with-current-buffer buf
      (setq who (assoc id mchat-group-members))
      (unless who
	;; update the members list
	(setq who (list id
			(cons 'tag data)
			(cons 'time-stamp (mchat-current-time))))
	(push who mchat-group-members)
	(mchat-insertion
	    (mchat-with-face 'mchat-comment-face
	      (insert-before-markers (concat data " joins.\n"))))
	))
    ))

(defun mchat-treat-quit (grp id)
  (let ((buf (process-buffer (cdr (assoc 'proc grp))))
	who)
    (with-current-buffer buf
      (setq who (assoc id mchat-group-members))
      (when who ;; otherwise, I didn't know the guy anyway
	(mchat-insertion
	    (mchat-with-face 'mchat-comment-face
	      (insert-before-markers
	       (concat (or (cdr (assoc 'tag who)) "<unknown>") " quits.\n"))))
	;; remove the guy
	(setq mchat-group-members (remassoc id mchat-group-members))
	))
    ))

;; #### NOTE: those two functions should probably use the CL package to
;; perform substitutions, but I'm not sure it's worth it. Maybe if one day I
;; have more things to update in the members structure, I could write a more
;; general function.
(defun mchat-update-time-stamp (id)
  ;; Update the timestamp for user ID in current MChat group buffer
  (let ((who (assoc id mchat-group-members)))
    (setq mchat-group-members (remassoc id mchat-group-members))
    (setq who (remassoc 'time-stamp who))
    (setq who (append who (list (cons 'time-stamp (mchat-current-time)))))
    (push who mchat-group-members)
    ))

(defun mchat-bufferize-message (id data)
  ;; Add a message to the unprocessed list
  (let* ((who (assoc id mchat-group-members))
	 (messages (assoc 'messages who)))
    (setq mchat-group-members (remassoc id mchat-group-members))
    (setq who (remassoc 'messages who))
    (if messages
	(setq messages (append messages (list data)))
      ;; else
      (setq messages (list 'messages data)))
    (setq who (append who messages))
    (push who mchat-group-members)
    ))

(defun mchat-treat-whois (grp id data) ;; DATA is the user id to check
  (let* ((proc (cdr (assoc 'proc grp)))
	 (buf (process-buffer proc))
	 who)
    ;; honor the request
    (when (string-equal mchat-user-id data)
      (mchat-send-iam proc))
    ;; update the guy
    (with-current-buffer buf
      (setq who (assoc id mchat-group-members))
      (cond ((not who) ;; yet unknown
	     ;; update the members list
	     (setq who (list id	(cons 'time-stamp (mchat-current-time))))
	     (push who mchat-group-members)
	     (mchat-send-whois proc id))
	    ((not (assoc 'tag who)) ;; half-known
	     (mchat-update-time-stamp id)
	     (mchat-send-whois proc id))
	    (t ;; completely known
	     (mchat-update-time-stamp id))))
    ))

(defun mchat-treat-iam (grp id data) ;; DATA is the user tag
  (let ((buf (process-buffer (cdr (assoc 'proc grp))))
	who)
    (with-current-buffer buf
      (setq who (assoc id mchat-group-members))
      (cond ((not who) ;; yet unknown
	     (setq who (list id
			     (cons 'tag data)
			     (cons 'time-stamp (mchat-current-time))))
	     (push who mchat-group-members)
	     (mchat-insertion
	       (mchat-with-face 'mchat-comment-face
		 (insert-before-markers (concat data " is here.\n")))))
	    ((not (assoc 'tag who)) ;; half-known
	     (let ((messages (cdr (assoc 'messages who))))
	       (setq who (append who (list (cons 'tag data))))
	       (when messages
		 (setq who (remassoc 'messages who)))
	       (setq mchat-group-members (remassoc id mchat-group-members))
	       (push who mchat-group-members)
	       (mchat-update-time-stamp id)
	       (mchat-insertion
		 (mchat-with-face 'mchat-comment-face
		   (insert-before-markers (concat data " is here.\n"))))
	       (when messages
		 (let (msg)
		   (while (setq msg (pop messages))
		     (mchat-treat-data grp id msg))))))
	    (t ;; completely known
	     ;; #### FIXME: should check if tag is still the same
	     (mchat-update-time-stamp id))))
    ))

(defun mchat-ding ()
  ;; like 'ding but check mchat-ring-sound
  (cond ((eq mchat-ring-sound t) ;; just the bell
	 (ding t))
	(mchat-ring-sound ;; a real sound. Try it or use the bell
	 (if (and (or (featurep 'native-sound)
		      (featurep 'nas-sound))
		  (device-sound-enabled-p))
	     (ding t mchat-ring-sound)
	   ;; else
	   (ding t)))))

;; Handles my own messages
(defun mchat-treat-ring (grp id)
  (let* ((proc (cdr (assoc 'proc grp)))
	 (buf (process-buffer proc))
	 who)
    (if (string-equal id mchat-user-id)
	(with-current-buffer buf
	  (mchat-insertion
	    (mchat-with-face 'mchat-comment-face
	      (insert-before-markers "I ring.\n")))
	  (mchat-ding))
      ;; else
      (with-current-buffer buf
	(setq who (assoc id mchat-group-members))
	(cond ((not who) ;; unknown
	       (setq who (list id
			       (cons 'time-stamp (mchat-current-time))
			       (list 'messages
				     (char-to-string
				      mchat-protocol-atom-ring))
			       ))
	       (push who mchat-group-members)
	       (mchat-send-whois proc id))
	      ((not (assoc 'tag who)) ;; half known
	       (mchat-bufferize-message id (char-to-string
					    mchat-protocol-atom-ring))
	       (mchat-update-time-stamp id)
	       (mchat-send-whois proc id))
	      (t ;;completely known
	       (mchat-update-time-stamp id)
	       (mchat-insertion
		 (mchat-with-face 'mchat-comment-face
		   (insert-before-markers
		    (concat (cdr (assoc 'tag who)) " rings.\n"))))
	       (mchat-ding)))))
    ))

;; Handles my own messages
(defun mchat-treat-msg (grp id data) ;; DATA is a normal message
  (let* ((proc (cdr (assoc 'proc grp)))
	 (buf (process-buffer proc))
	 who flash)
    (if (string-equal id mchat-user-id)
	(with-current-buffer buf
	  (mchat-insertion
	    (mchat-with-face 'mchat-tag-face
	      (insert-before-markers "I say:\n"))
	    (let ((beg (point))
		  end)
	      (insert-before-markers (concat data "\n"))
	      (setq end (point))
	      (run-hook-with-args 'mchat-treat-message-hooks
				  beg end))))
      ;; else
      (with-current-buffer buf
	(setq who (assoc id mchat-group-members))
	(cond ((not who) ;; unknown
	       (setq who (list id
			       (cons 'time-stamp (mchat-current-time))
			       (list 'messages
				     (concat (char-to-string
					      mchat-protocol-atom-msg)
					     data))
			       ))
	       (push who mchat-group-members)
	       (mchat-send-whois proc id))
	      ((not (assoc 'tag who)) ;; half known
	       (mchat-bufferize-message
		id
		(concat (char-to-string mchat-protocol-atom-msg)
			data))
	       (mchat-update-time-stamp id)
	       (mchat-send-whois proc id))
	      (t ;; completely known
	       (setq flash t)
	       (mchat-insertion
		 (mchat-with-face 'mchat-tag-face
		   (insert-before-markers
		    (concat (cdr (assoc 'tag who)) " says:\n")))
		 (let ((beg (point))
		       end)
		   (insert-before-markers (concat data "\n"))
		   (setq end (point))
		   (run-hook-with-args 'mchat-treat-message-hooks
				       beg end))))))
      ;; #### WARNING: do this outside buf !!
      ;; Now possibly flash if the buffer is not visible (but not if
      ;; we're in it).
      (when flash
	(cond ((and (eq mchat-flash-on-message t)
		    (not (eq (current-buffer) buf))
		    (not (eq (current-buffer)
			     (cdr (assoc 'buffer grp)))))
	       (let ((visible-bell t)) (ding t t)))
	      ((and (eq mchat-flash-on-message 'needed)
		    (not (get-buffer-window buf 'visible)))
	       (let ((visible-bell t)) (ding t t))))))
    ))

(defun mchat-treat-heart-bit (grp id)
  (let* ((proc (cdr (assoc 'proc grp)))
	 (buf (process-buffer proc))
	 who)
    (with-current-buffer buf
      (setq who (assoc id mchat-group-members))
      (cond ((not who) ;; unknown
	     (setq who (list id (cons 'time-stamp (mchat-current-time))))
	     (push who mchat-group-members)
	     (mchat-send-whois proc id))
	    ((not (assoc 'tag who)) ;; half known
	     (mchat-update-time-stamp id)
	     (mchat-send-whois proc id))
	    (t ;; completely known
	     (mchat-update-time-stamp id))))
    ))

(defun mchat-treat-data (grp id data)
  ;; treat an MChat protocol message for group GRP. Branch to the proper
  ;; subroutine
  (cond ((eq (aref data 0) mchat-protocol-atom-join) ;; join
	 (unless (string-equal id mchat-user-id)
	   (mchat-treat-join grp id (substring data 1))))
	((eq (aref data 0) mchat-protocol-atom-quit) ;; quit
	 (unless (string-equal id mchat-user-id)
	   (mchat-treat-quit grp id)))
	((eq (aref data 0) mchat-protocol-atom-whois) ;; whois
	 (unless (string-equal id mchat-user-id)
	   (mchat-treat-whois grp id (substring data 1))))
	((eq (aref data 0) mchat-protocol-atom-iam) ;; iam
	 (unless (string-equal id mchat-user-id)
	   (mchat-treat-iam grp id (substring data 1))))
	;; Handle my own messages
	((eq (aref data 0) mchat-protocol-atom-ring) ;; ring
	 (mchat-treat-ring grp id))
	;; Handle my own messages
	((eq (aref data 0) mchat-protocol-atom-msg) ;; msg
	 (mchat-treat-msg grp id (substring data 1)))
	((eq (aref data 0) mchat-protocol-atom-heart-bit) ;; heart bit
	 (unless (string-equal id mchat-user-id)
	   (mchat-treat-heart-bit grp id)))
	))

;; #### FIXME: I think we can loose UDP packets, but we can assume that if one
;; is received, it's not corrupted, right ?
;; Also, I think I've encountered cases where packets were concatenated, so I
;; handle this by adding the length of each DATA segment, and calling again
;; the process filter myself if the message is too long.
(defun mchat-process-filter (proc str)
  ;; Filter for all received mchat messages
  (let ((grp (catch 'found
	       (let ((grps mchat-groups) grp)
		 (while (setq grp (pop grps))
		   (if (equal proc (cdr (assoc 'proc grp)))
		       (throw 'found grp))))))
	(major (char-to-int (aref str 0)))
	(minor (char-to-int (aref str 1)))
	id len data next)
    ;; Check that we understand this protocol version:
    (if (> (+ (* 256 major) minor)
	   (+ (* 256 mchat-protocol-major-version)
	      mchat-protocol-minor-version))
	(with-current-buffer (process-buffer proc)
	  (mchat-insertion
	    (mchat-with-face 'mchat-comment-face
	      (insert-before-markers
	       "Received a message with MChat protocol version "
	       (int-to-string major) "." (int-to-string minor) " !\n")
	      (insert-before-markers
	       "Please check if you have an outdated package.\n")
	      (insert-before-markers
	       "(also, note that this might just be a corrupted message)\n")
	      )))
      ;; else, this protocol version is understood, decode the message.
      ;; #### NOTE: the following stuff could change for newer protocol
      ;; versions. Just there's only one now (2.0).
      (setq id (substring str 2 14)
	    len (+ (* 256 (char-to-int (aref str 14)))
		   (char-to-int (aref str 15)))
	    data (substring str 16 (+ 16 len))
	    next (and (> (length str) (+ 16 len))
		      (substring str (+ 16 len))))
      (mchat-treat-data grp id data)
      (and next (mchat-process-filter proc next)))
    ))


;; MChat groups display routines ============================================

;; Each frame in which an MChat group is displayed gets a
;; 'mchat-window-configurations property. This property is a list of
;; window configurations current before each call to `mchat-switch-to-group'.

(defun mchat-switch-to-group (grp)
  "Switch to the MChat group GRP in the current frame. This function remembers
the previous window configuration, that you will be able to get back using
`mchat-bury'."
  (interactive
   (list (assoc (completing-read "Group: " mchat-groups nil t) mchat-groups)))
  ;; Remember the window configuration
  (set-frame-property (selected-frame) 'mchat-window-configurations
		      (cons (current-window-configuration)
			    (frame-property (selected-frame)
					    'mchat-window-configurations)))
  ;; Setup the frame for this group
  (delete-other-windows)
  (switch-to-buffer (process-buffer (cdr (assoc 'proc grp))))
  (split-window nil (/ (* (window-height) mchat-window-percent) 100))
  (other-window 1)
  (switch-to-buffer (cdr (assoc 'buffer grp)))
  )

(defun mchat-bury ()
  "Bury the MChat group displayed in the current frame. This function
actually restores the window configuration immediately preceding the last call
to `mchat-switch-to-group' in this frame.

If this frame didn't display an MChat group before (properly speaking, that is
if `mchat-switch-to-group' was not called in this frame before), this function
does nothing."
  (interactive)
  (let ((wcs (frame-property (selected-frame) 'mchat-window-configurations)))
    (when wcs
      (set-frame-property (selected-frame)
			  'mchat-window-configurations (cdr wcs))
      ;; #### NOTE: `set-window-configuration' doesn't seem to produce any
      ;; errors when the window configuration can't be restored (like, if one
      ;; buffer is killed, or something).
      (set-window-configuration (car wcs)))
    ))


;; MChat group destruction routines =========================================

(defun mchat-kill-group (grp)
  ;; Destroy the group and clean up.
  ;; XEmacs handles deleting the processes associated with buffers being
  ;; killed, but I prefer to do it myself, in priority.
  (let* ((proc (cdr (assoc 'proc grp)))
	 (group-buffer (process-buffer proc))
	 (message-buffer (cdr (assoc 'buffer grp)))
	 (zombie-check (cdr (assoc 'zombie-check grp))))
    ;; #### WARNING: this is the first thing to do, because mchat-send reputs
    ;; the timeout after sending the message !!
    (mchat-send-quit proc)
    ;; Don't call the hooks !!
    (with-current-buffer group-buffer
      (disable-timeout mchat-heart-bit-timeout)
      (remove-hook 'kill-buffer-hook 'mchat-kill-buffer-hook t))
    (with-current-buffer message-buffer
      (remove-hook 'kill-buffer-hook 'mchat-kill-buffer-hook t))
    (disable-timeout zombie-check)
    (delete-process proc)
    (kill-buffer group-buffer)
    (kill-buffer message-buffer)
    (setq mchat-groups (remassoc (car grp) mchat-groups))
    ))


;; MChat modes routines =====================================================

;; #### NOTE: for the moment, all of the following functions are supposed to
;; be run only from an MChat buffer. Maybe we could make some of them query
;; the group otherwise, but there again, I don't find this particularly
;; useful.

(defsubst mchat-interactive ()
  (let* ((grp (assoc mchat-group-name mchat-groups)))
    (if grp
	(list grp)
      (error "No MChat group associated with this buffer."))
    ))

(defun mchat-quit (grp)
  "Quit the MChat group GRP."
  (interactive (mchat-interactive))
  (mchat-bury)
  (mchat-kill-group grp))

(defun mchat-ring (grp)
  "Ring the bell on the MChat group GRP. If people are not looking at the
buffer, at least they can hear you... Hmm maybe I should have called this
function `mchat-annoy-user'... See also `mchat-flash-on-message'."
  (interactive (mchat-interactive))
  (mchat-send-ring (cdr (assoc 'proc grp))))

(defsubst mchat-maybe-remember-message ()
  ;; Remember the message being edited, *even* if it's still empty. Indeed, we
  ;; want to get it back after moving in the cache.
  (when (or (buffer-modified-p) (eq 0 (length (buffer-substring))))
    (setq mchat-unsent-message (buffer-substring))))

(defun mchat-next-message (grp &optional n)
  "Restore the next Nth message (previous if N is negative) in the
message buffer of group GRP. The variable `mchat-circulate-messages' controls
the behavior when N is out of bounds. If N is not given, it is 1 by default.

When called interactively, use the prefix to change N."
  (interactive (append
		(mchat-interactive)
		(list (if current-prefix-arg
			  (prefix-numeric-value current-prefix-arg)
			1))))
  (unless (eq 0 n)
    (with-current-buffer (cdr (assoc 'buffer grp))
      (mchat-maybe-remember-message)
      ;; When moving in the cache the unset message must be virtually part of
      ;; the cache in order to get it back.
      (let* ((cache (append mchat-message-cache (list mchat-unsent-message)))
	     (len (length cache))
	     (indice (or mchat-message-cache-indice (1- len))))
	;; find the new indice
	(setq n (+ indice n))
	(cond (mchat-circulate-messages
	       (while (< n 0)
		 (setq n (+ n len)))
	       (setq n (% n len)))
	      (t
	       (if (< n 0)
		   (setq n 0)
		 (if (>= n len)
		     (setq n (1- len))))))
	;; Pop the message
	(erase-buffer)
	(insert (nth n cache))
	(setq mchat-message-cache-indice n)
	(unless (and (eq n (1- len)) (> (length (buffer-substring)) 0))
	  (set-buffer-modified-p nil))
	))
    ))

(defun mchat-previous-message (grp)
  "Restore the previous message in the message buffer of group GRP. See also
the variable `mchat-circulate-messages'"
  (interactive (mchat-interactive))
  (mchat-next-message grp -1))

(defsubst mchat-pop-cached-message (n)
  ;; Pops the message number n from the cache.
  (erase-buffer)
  (insert (nth n mchat-message-cache))
  (setq mchat-message-cache-indice n)
  (set-buffer-modified-p nil))

(defun mchat-first-message (grp)
  "Restore the oldest cached message in the message buffer of group GRP."
  (interactive (mchat-interactive))
  (with-current-buffer (cdr (assoc 'buffer grp))
    (when mchat-message-cache
      (mchat-maybe-remember-message)
      (mchat-pop-cached-message 0))
    ))

;; #### NOTE: contrary to `mchat-next-message', this function doesn't consider
;; the message currently edited as part of the cache. However, it is
;; remembered and can be accessed with one `M-n'.
(defun mchat-last-message (grp)
  "Restore the youngest cached message in the message buffer of group GRP."
  (interactive (mchat-interactive))
  (with-current-buffer (cdr (assoc 'buffer grp))
    (when mchat-message-cache
      (mchat-maybe-remember-message)
      (mchat-pop-cached-message (1- (length mchat-message-cache))))
    ))

(defun mchat-message (grp)
  "Send the contents of the message buffer to the MChat group GRP."
  (interactive (mchat-interactive))
  (with-current-buffer (cdr (assoc 'buffer grp))
    ;; the hooks might change something, so run them here.
    (run-hooks 'mchat-before-send-hooks)
    (let ((str (buffer-substring)))
      (cond ((or (not str) (= 0 (length str)))
	     (error "The message buffer is empty."))
	    ((> (length str) 450)
	     (error "Message too long. Please send it in shorter pieces."))
	    (t
	     (when (buffer-modified-p) ;; a new message
	       ;; make room for it in the cache ('and should be enough)
	       (while (>= (length mchat-message-cache)
			  mchat-message-cache-size)
		 (pop mchat-message-cache))
	       ;; add it
	       (setq mchat-message-cache
		     (append mchat-message-cache (list str)))
	       (setq mchat-message-cache-indice nil)
	       (setq mchat-unsent-message nil)
	       (erase-buffer)
	       (set-buffer-modified-p nil))
	     ;; Finally, send it
	     (mchat-send-msg (cdr (assoc 'proc grp)) str)
	     )))
    ))

(defun mchat-info (grp)
  "Display information on the current MChat group."
  (interactive (mchat-interactive))
  (message "\"%s\" on %s" (car grp) (cadr grp)))

(defun mchat-who (grp)
  "Display the list of known members of the current group."
  (interactive (mchat-interactive))
  (with-current-buffer (process-buffer (cdr (assoc 'proc grp)))
    (mchat-insertion
	(mchat-with-face 'mchat-comment-face
	  (let ((members mchat-group-members)
		member)
	    (if (not members)
		(insert-before-markers
		 "You're the only one, Max. Getting bored?\n")
	      ;; else
	      (insert-before-markers
	       (concat "People currently known on `" (car grp) "' are:\n"))
	      (while (setq member (pop members))
		(insert-before-markers
		 (concat "- "(cdr (assoc 'tag member)) ",\n")))
	      (delete-backward-char 2)
	      (insert-before-markers ".\n")))
	  ))
    ))

(defun mchat-erase-buffer (grp)
  "Erase the current MChat group buffer."
  (interactive (mchat-interactive)) ;; this is just a protection
  (with-current-buffer (process-buffer (cdr (assoc 'proc grp)))
    (let ((buffer-read-only nil))
      (erase-buffer))
    ))

(defun mchat-discard-message (grp)
  "Discard the contents of the current MChat message buffer."
  (interactive (mchat-interactive))
  (with-current-buffer (cdr (assoc 'buffer grp))
    (erase-buffer)))

(defun mchat-register (grp)
  "Register the current group (add it to `mchat-registered-groups')."
  (interactive (mchat-interactive))
  (if (assoc (car grp) mchat-registered-groups)
      (error "This group is already defined.")
    ;; else
    (setq mchat-registered-groups
	  (append `((,(car grp) ,(cadr grp))) mchat-registered-groups))
    (when (y-or-n-p "Group registered. Keep it for future sessions ? ")
      ;; #### FIXME: should I use Custom also to set the variable, or only if
      ;; we want to save the value ?
      (custom-set-variables
       `(mchat-registered-groups (quote ,mchat-registered-groups) t))
      (custom-save-all))
    ))

(defun mchat-unregister (grp)
  "Unregister the current group (remove it to `mchat-registered-groups')."
  (interactive (mchat-interactive))
  (setq mchat-registered-groups (remassoc (car grp) mchat-registered-groups))
  (when (y-or-n-p "Group unregistered. Forget it for future sessions ? ")
    ;; #### FIXME: should I use Custom also to set the variable, or only if
    ;; we want to save the value ?
    (custom-set-variables
     `(mchat-registered-groups (quote ,mchat-registered-groups) t))
    (custom-save-all)
    ))

(defun mchat-version ()
  "Show the current version of MChat."
  (interactive)
  (message "MChat version %s" mchat-version))


;; MChat major mode (for the group buffers) =================================

;; #### NOTE: I could let more bindings here (the message ones in particular),
;; but since people are not supposed to live in the group buffer, I restrict
;; the commands available on purpose.

(defvar mchat-major-mode-map
  (let ((m (make-sparse-keymap 'mchat-major-mode-map)))
    ;; -
    (define-key m "e" 'mchat-erase-buffer)
    (define-key m "b" 'mchat-ring)
    (define-key m "w" 'mchat-who)
    (define-key m "i" 'mchat-info)
;;; (define-key m "s" 'mchat-suspend)
    (define-key m "q" 'mchat-quit)
    ;; --
    (define-key m "r" 'mchat-register)
    (define-key m "u" 'mchat-unregister)
    (define-key m "o" 'mchat-switch-to-group)
    (define-key m "x" 'mchat-bury)
    ;; ---
    (define-key m "v" 'mchat-version)
    m)
  ;; MChat major mode keymap
  )

(defun mchat-major-mode ()
  "Turns on the MChat major mode. Used for MChat group buffers. You're not
supposed to use this. To join an MChat group, use `mchat' instead."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (use-local-map mchat-major-mode-map)
  (setq major-mode 'mchat-major-mode)
  (setq mode-name mchat-major-mode-string)
  (when (featurep 'menubar)
    (setq mode-popup-menu mchat-submenu))
  (run-hooks 'mchat-major-mode-hooks))


;; MChat minor mode (for the message buffers) ===============================

(defvar mchat-minor-mode-map
  (let ((m (make-sparse-keymap 'mchat-minor-mode-map)))
    ;; -
    (define-key m [(control c) (control c)] 'mchat-message)
    (define-key m [(control c) (control d)] 'mchat-discard-message)
    (define-key m [(meta n)] 'mchat-next-message)
    (define-key m [(meta p)] 'mchat-previous-message)
    (define-key m [(meta P)] 'mchat-first-message)
    (define-key m [(meta N)] 'mchat-last-message)
    ;; --
    (define-key m [(control c) (control e)] 'mchat-erase-buffer)
    (define-key m [(control c) (control b)] 'mchat-ring)
    (define-key m [(control c) (control w)] 'mchat-who)
    (define-key m [(control c) (control i)] 'mchat-info)
;;; (define-key m "s" 'mchat-suspend)
    (define-key m [(control c) (control q)] 'mchat-quit)
    ;; ---
    (define-key m [(control c) (control r)] 'mchat-register)
    (define-key m [(control c) (control u)] 'mchat-unregister)
    (define-key m [(control c) (control o)] 'mchat-switch-to-group)
    (define-key m [(control c) (control x)] 'mchat-bury)
    ;; ----
    (define-key m [(control c) (control v)] 'mchat-version)
    m)
  ;; MChat minor mode keymap
  )

(make-variable-buffer-local
 (defvar mchat-minor-mode nil))

(defun mchat-minor-mode (arg)
  "Toggles the MChat minor mode. Used for MChat message buffers. You're not
supposed to use this. To join an MChat group, use `mchat' instead."
  (interactive "*P")
  (setq mchat-minor-mode
	(if (null arg) (not mchat-minor-mode)
	  (> (prefix-numeric-value arg) 0)))
  (when (featurep 'menubar)
    ;; I know, I know, this should be the *major* mode menu, not mine.
    (setq mode-popup-menu mchat-submenu))
  (run-hooks 'mchat-minor-mode-hooks))

(add-minor-mode
 'mchat-minor-mode mchat-minor-mode-string mchat-minor-mode-map)


;; MChat group creation routines ============================================

;; It's OK to call mchat-kill-group, because it's OK to re-kill killed
;; buffers.
(defun mchat-kill-buffer-hook ()
  (mchat-kill-group (assoc mchat-group-name mchat-groups)))

(defun mchat-heart-bit (proc)
  ;; Timeout to resend a `heart-bit' message
  (mchat-send-heart-bit proc))

(defun mchat-zombie-check (buffer)
  (with-current-buffer buffer
    (let ((members mchat-group-members)
	  (now (mchat-current-time))
	  member)
      (while (setq member (pop members))
	(when (> now (+ (cdr (assoc 'time-stamp member))
			(* mchat-heart-bit-delay mchat-zombie-latency)))
	  ;; this guy must be dead
	  (setq mchat-group-members
		(remassoc (car member) mchat-group-members))
	  (mchat-insertion
	    (mchat-with-face 'mchat-comment-face
	      (insert-before-markers
	       (concat (cdr (assoc 'tag member)) " seems to have died.\n"))))
	  )))
    ))

(defun mchat-create-group (name address)
  ;; Ensure the user id is created, create the MChat group NAME at address
  ;; ADDRESS, update mchat-groups, and return the group description.
  ;; #### NOTE: previously, the user id was created at load time which was
  ;; VERY BAD, because the user would not have the opportunity to correct a
  ;; bogus nsl-method.
  (unless mchat-user-id
    (or (setq mchat-user-id (mchat-user-id))
	(error "Can't build user id. Please check your mchat-nsl-method.")
	))
  (let* ((bufname (concat "MChat (" name ")"))
	 (proc (open-multicast-group bufname bufname address))
	 (buf (get-buffer-create (concat "MChat message (" name ")")))
	 (desc `(,name
		 ,address
		 (buffer . ,buf)
		 (proc . ,proc))))
    ;; Group buffer setup
    (with-current-buffer bufname
      (mchat-major-mode)
      (setq mchat-group-name name)
      ;; #### NOTE: it's a bit silly, but sending the 'join message will
      ;; delete this timeout and reput it after :-/
      (setq mchat-heart-bit-timeout
	    (add-timeout mchat-heart-bit-delay 'mchat-heart-bit proc
			 mchat-heart-bit-delay))
      ;; I'm not sure this is really needed all the time, but in case the
      ;; menubar is not the default one, make sure we have the submenu.
      (and (featurep 'menubar) (add-submenu nil mchat-submenu))
      (make-local-hook 'kill-buffer-hook)
      (add-hook 'kill-buffer-hook 'mchat-kill-buffer-hook nil t))
    ;; Message buffer setup
    (with-current-buffer buf
      (apply (intern (concat (symbol-name mchat-message-buffer-major-mode)
			     "-mode"))
	     nil) ;; I'm not sure why this is needed.
      (setq mchat-group-name name)
      (mchat-minor-mode 1)
      (make-local-hook 'kill-buffer-hook)
      (add-hook 'kill-buffer-hook 'mchat-kill-buffer-hook nil t))
    ;; Final setup:
    (set-process-filter proc 'mchat-process-filter)
    (set-marker (process-mark proc) (point) (process-buffer proc))
    (setq desc
	  (append desc (list (cons 'zombie-check
				   (add-timeout (* mchat-zombie-latency
						   mchat-heart-bit-delay)
						'mchat-zombie-check
						(process-buffer proc)
						(* mchat-zombie-latency
						   mchat-heart-bit-delay))))))
    ;; say `hello'
    (mchat-send-join proc)
    (setq mchat-groups (cons desc mchat-groups))
    desc))

;; #### FIXME: remove this function. It seems to be used only once.
(defun mchat-known-groups ()
  ;; Returns an alist of elements of the form (NAME  ADDRESS) for ALL groups
  ;; known to MChat. This includes those in `mchat-registered-groups', and
  ;; those that are currently known but not registered. Since group names must
  ;; be unique, it's quicker to match the names only.
  (let ((grps mchat-groups) grp res)
    (while (setq grp (pop grps))
      (if (not (assoc (car grp) mchat-registered-groups))
	  (setq res (cons (list (car grp) (cadr grp)) res))))
    (append res mchat-registered-groups)
    ))

(defsubst mchat-all-interactive ()
  ;; We don't allow the same name for different groups, so if a known group
  ;; name is given, don't ask for the address. The completion occurs for both
  ;; predefined groups and groups that exist but are not registered.
  (let* ((grps (mchat-known-groups))
	 (grpname (completing-read "Name: " grps))
	 (grpaddr (or (cadr (assoc grpname grps))
		      (read-string "Address: "))))
    (list grpname grpaddr)
    ))

(defmacro mchat-sanity-check ()
  ;; For non interactive calls, check the validity of the given address, or
  ;; find it. Really, I'm too good to you, guys.
  `(unless (interactive-p)
     (let ((grp (assoc name mchat-registered-groups)))
       (if grp
	   (if (not address)
	       (setq address (cadr grp))
	     (or (equal address (cadr grp))
		 (error "Address doesn't not match group name!")))
	 (or address
	     (error "No address given for unregistered group!"))))))

(defmacro mchat-maybe-create-and-switch-to-group (&rest body)
  ;; Maybe create the group, execute body, and setup the frame
  `(let ((grp (assoc name mchat-groups)))
     (unless grp
       (setq grp (mchat-create-group name address)))
     ,@body
     (mchat-switch-to-group grp)))

;;;###autoload
(defun mchat (name &optional address)
  "Join MChat multicast group NAME at address ADDRESS. When called
interactively, you will be prompted for the group name, and if the group is
not registered, for the corresponding multicast address. Please refer to
`mchat-predefined-groups' for a list of known groups, and
`open-multicast-group' for the syntax of ADDRESS."
  (interactive (mchat-all-interactive))
  (mchat-sanity-check)
  (mchat-maybe-create-and-switch-to-group))

;;;###autoload
(defun mchat-other-frame (name &optional address)
  "Like `mchat', but pop up a new frame."
  (interactive (mchat-all-interactive))
  (mchat-sanity-check)
  (mchat-maybe-create-and-switch-to-group
   (select-frame (make-frame))))

(provide 'mchat)

(run-hooks 'mchat-loaded-hooks)

;;; mchat.el ends here
