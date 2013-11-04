;;; telnet.el --- run a telnet session from within an Emacs buffer

;; Copyright (C) 1985, 1988, 1992, 1994 Free Software Foundation, Inc.

;; Author: William F. Schelter
;; Keywords: comm, unix
;; Maintainer: Pete Ware <ware@cis.ohio-state.edu>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This mode is intended to be used for telnet or rsh to a remote host;
;; `telnet' and `rsh' are the two entry points.  Multiple telnet or rsh
;; sessions are supported.
;;
;; Normally, input is sent to the remote telnet/rsh line-by-line, as you
;; type RET or LFD.  C-c C-c sends a C-c to the remote immediately; 
;; C-c C-z sends C-z immediately.  C-c C-q followed by any character
;; sends that character immediately.
;;
;; All RET characters are filtered out of the output coming back from the
;; remote system.  The mode tries to do other useful translations based
;; on what it sees coming back from the other system before the password
;; query.  It knows about UNIX, ITS, TOPS-20 and Explorer systems.
;;
;; You can use the global telnet-host-properties to associate a telnet
;; program and login name with each host you regularly telnet to.

;;; Code:

;; to do fix software types for lispm:
;; to eval current expression.  Also to try to send escape keys correctly.
;; essentially we'll want the rubout-handler off.

;; filter is simplistic but should be okay for typical shell usage.
;; needs hacking if it is going to deal with asynchronous output in a sane
;; manner

(require 'comint)

(defgroup telnet nil
  "Run a telnet session from within an Emacs buffer."
  :group 'comint)

(defvar telnet-host-properties ()
  "Specify which telnet program to use for particular hosts.
Each element has the form (HOSTNAME PROGRAM [LOGIN-NAME])
HOSTNAME says which machine the element applies to.
PROGRAM says which program to run, to talk to that machine.
LOGIN-NAME, which is optional, says what to log in as on that machine.")

(defvar telnet-new-line "\r")
(defvar telnet-mode-map nil)
(defvar telnet-default-prompt-pattern "^[^#$%>\n]*[#$%>] *")
(defvar telnet-prompt-pattern telnet-default-prompt-pattern)

(defvar telnet-replace-c-g nil)
(make-variable-buffer-local
 (defvar telnet-remote-echoes t
   "True if the telnet process will echo input."))
(make-variable-buffer-local
 (defvar telnet-interrupt-string "\C-c" "String sent by C-c."))

(defvar telnet-count 0
  "Number of output strings read from the telnet process
while looking for the initial password.")
;; (make-variable-buffer-local 'telnet-count)

(defcustom telnet-program "telnet"
  "*Program to run to open a telnet connection."
  :type 'string
  :group 'telnet)

(defcustom rsh-eat-password-string nil
  "Non-nil means rsh will look for a string matching a password prompt."
  :type 'boolean
  :group 'telnet)

(defvar telnet-initial-count -75
  "Initial value of `telnet-count'.  Should be set to the negative of the
number of terminal writes telnet will make setting up the host connection.")

(defvar telnet-maximum-count 4
  "Maximum value `telnet-count' can have.
After this many passes, we stop looking for initial setup data.
Should be set to the number of terminal writes telnet will make
rejecting one login and prompting again for a username and password.")

(defun telnet-interrupt-subjob ()
  (interactive)
  "Interrupt the program running through telnet on the remote host."
  (process-send-string nil telnet-interrupt-string))

(defun telnet-c-z ()
  (interactive)
  (process-send-string nil "\C-z"))

;; Keep telnet- prefix)
(defun telnet-send-process-next-char ()
  (interactive)
  (process-send-string nil
	       (char-to-string
		(let ((inhibit-quit t))
		  (prog1 (read-char)
		    (setq quit-flag nil))))))

; initialization on first load.
(if telnet-mode-map
    nil
  (setq telnet-mode-map (make-sparse-keymap))
  (set-keymap-parent telnet-mode-map comint-mode-map)
  (define-key telnet-mode-map "\C-m" 'telnet-send-input)
;  (define-key telnet-mode-map "\C-j" 'telnet-send-input)
  (define-key telnet-mode-map "\C-c\C-q" 'telnet-send-process-next-char)
  (define-key telnet-mode-map "\C-c\C-c" 'telnet-interrupt-subjob) 
  (define-key telnet-mode-map "\C-c\C-z" 'telnet-c-z))

;;maybe should have a flag for when have found type
(defun telnet-check-software-type-initialize (string)
  "Tries to put correct initializations in.  Needs work."
  (let ((case-fold-search t))
    (cond ((string-match "unix" string)
	   (setq telnet-prompt-pattern shell-prompt-pattern)
	   (setq telnet-new-line "\n"))
	  ((string-match "tops-20" string) ;;maybe add telnet-replace-c-g
	   (setq telnet-prompt-pattern  "[@>] *"))
	  ((string-match "its" string)
	   (setq telnet-prompt-pattern  "^[^*>\n]*[*>] *"))
	  ((string-match "explorer" string) ;;explorer telnet needs work
	   (setq telnet-replace-c-g ?\n))))
  (setq comint-prompt-regexp telnet-prompt-pattern))

(defun telnet-initial-filter (proc string)
  (let ((case-fold-search t))
    ;For reading up to and including password; also will get machine type.
    (cond ((string-match "No such host" string)
	   (kill-buffer (process-buffer proc))
	   (error "No such host."))
	  ((string-match "passw" string)
	   (telnet-filter proc string)
	   (let ((password (comint-read-noecho "Password: " t)))
	     (setq telnet-count 0)
	     (process-send-string proc (concat password telnet-new-line))))
	  (t (telnet-check-software-type-initialize string)
	     (telnet-filter proc string)
	     (cond ((> telnet-count telnet-maximum-count)
		    ;; (set-process-filter proc 'telnet-filter) Kludge
		    ;; for shell-fonts -- this is the only mode that
		    ;; actually changes what its process filter is at
		    ;; run time, which confuses shell-font.  So we
		    ;; special-case that here.
		    ;; #### Danger, knows an internal shell-font variable name.
		    (let ((old-filter (process-filter proc)))
		      (if (eq old-filter 'shell-font-process-filter)
			  (set (make-local-variable 'shell-font-process-filter)
			       'telnet-filter)
			(set-process-filter proc 'telnet-filter))))
		   (t (setq telnet-count (1+ telnet-count))))))))

;; Identical to comint-simple-send, except that it sends telnet-new-line
;; instead of "\n".
(defun telnet-simple-send (proc string)
  (comint-send-string proc string)
  (comint-send-string proc telnet-new-line))

(defun telnet-filter (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (save-match-data
     (let* ((last-insertion (marker-position (process-mark proc)))
	    (delta (- (point) last-insertion))
	    (ie (and comint-last-input-end
		     (marker-position comint-last-input-end)))
	    (w (get-buffer-window (current-buffer)))
	    (ws (and w (window-start w))))
       (goto-char last-insertion)
	;; Insert STRING, omitting all C-m characters.
       (insert-before-markers string)
       (set-marker (process-mark proc) (point))
       ;; the insert-before-markers may have screwed window-start
       ;; and likely moved comint-last-input-end.  This is why the
       ;; insertion-reaction should be a property of markers, not
       ;; of the function which does the inserting.
       (if ws (set-window-start w ws t))
       (if ie (set-marker comint-last-input-end ie))
       (while (progn (skip-chars-backward "^\C-m" last-insertion)
		     (> (point) last-insertion))
	 (delete-region (1- (point)) (point)))
       (goto-char (process-mark proc))
       (and telnet-replace-c-g
	    (subst-char-in-region last-insertion (point) ?\C-g
				  telnet-replace-c-g t))
      ;; If point is after the insertion place, move it
      ;; along with the text.
      (if (> delta 0)
	  (goto-char (+ (process-mark proc) delta)))))))

(defun telnet-send-input ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
	p1 p2)
    (if (and telnet-remote-echoes
	     (>= (point) (process-mark proc)))
	(save-excursion
	  (if comint-eol-on-send (end-of-line))
	  (setq p1 (marker-position (process-mark proc))
		p2 (point))))
    (prog1
	(comint-send-input)
      ;; at this point, comint-send-input has moved the process mark, inserted
      ;; a newline, and possibly inserted the (echoed) output.  If the host is
      ;; in remote-echo mode, then delete our local copy of the command, and
      ;; the newline that comint-send-input sent.
      (if p1
	  (delete-region p1 (1+ p2))))))

;;;###autoload (add-hook 'same-window-regexps "\\*telnet-.*\\*\\(\\|<[0-9]+>\\)")

;;;###autoload
(defun telnet (host &optional port)
  "Open a network login connection to host named HOST (a string).
With a prefix argument, prompts for the port name or number as well.
Communication with HOST is recorded in a buffer `*PROGRAM-HOST*'
where PROGRAM is the telnet program being used.  This program
is controlled by the contents of the global variable `telnet-host-properties',
falling back on the value of the global variable `telnet-program'.
Normally input is edited in Emacs and sent a line at a time.
See also `\\[rsh]'."
  (interactive (list (read-string "Open telnet connection to host: ")
		     (if current-prefix-arg
			 (read-string "Port name or number: ")
		       nil)))
  (let* ((comint-delimiter-argument-list '(?\  ?\t))
	 (properties (cdr (assoc host telnet-host-properties)))
	 (telnet-program (if properties (car properties) telnet-program))
         (name (concat telnet-program "-" (comint-arguments host 0 nil) ))
	 (buffer (get-buffer (concat "*" name "*")))
	 (telnet-options (if (cdr properties) (cons "-l" (cdr properties))))
	 process)
    (if (and buffer (get-buffer-process buffer))
	(pop-to-buffer buffer)
      (pop-to-buffer 
       (apply 'make-comint name telnet-program nil telnet-options))
      (setq process (get-buffer-process (current-buffer)))
      (set-process-filter process 'telnet-initial-filter)
      
      ;; SunOS and IRIX don't print "unix" in their rsh or telnet
      ;; login banners, so let's get a reasonable default here.
      ;; #### This patch from jwz mimics what is done in rsh done
      ;; below.  However, it (along with the one in rsh) mean that
      ;; telnet-check-software-type-initialize is effectively a
      ;; wastoid function.  Reworking it like it claims to need is
      ;; probably the better solution but I'm not going to do it.
      ;; --cet
      (telnet-check-software-type-initialize "unix")
      
      ;; Don't send the `open' cmd till telnet is ready for it.
      (accept-process-output process)
      (erase-buffer)
      (process-send-string process (concat "open " host
					   (if port (concat " " port) "")
					   "\n"))
      (setq comint-input-sender 'telnet-simple-send)
      (setq telnet-count telnet-initial-count)
      (telnet-mode))))

(put 'telnet-mode 'mode-class 'special)

(defun telnet-mode ()
  "This mode is for using telnet (or rsh) from a buffer to another host.
It has most of the same commands as comint-mode.
There is a variable ``telnet-interrupt-string'' which is the character
sent to try to stop execution of a job on the remote host.
Data is sent to the remote host when RET is typed.

\\{telnet-mode-map}
"
  (interactive)
  (comint-mode)
  (setq major-mode 'telnet-mode
        mode-name "Telnet"
        comint-prompt-regexp telnet-prompt-pattern)
  (use-local-map telnet-mode-map)
  (set (make-local-variable 'telnet-count) telnet-initial-count)
  (run-hooks 'telnet-mode-hook))

;;;###autoload (add-hook 'same-window-regexps "\\*rsh-[^-]*\\*\\(\\|<[0-9]*>\\)")

;;;###autoload
(defun rsh (host)
  "Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*rsh-HOST*'.
Normally input is edited in Emacs and sent a line at a time.
See also `\\[telnet]'."
  (interactive "sOpen rsh connection to host: ")
  (require 'shell)
  (let ((name (concat "rsh-" host )))
    (pop-to-buffer (make-comint name remote-shell-program nil host))
    (setq telnet-count telnet-initial-count)
    ;;
    ;; SunOS doesn't print "unix" in its rsh login banner, so let's get a
    ;; reasonable default here.  There do exist non-Unix machines which
    ;; speak the rsh protocol, but let's hope they print their OS name
    ;; when one connects.
    ;;
    (telnet-check-software-type-initialize "unix")
    ;;
    ;; I think we should use telnet-filter here instead of -initial-filter,
    ;; because rsh generally doesn't prompt for a password, and gobbling the
    ;; first line that contains "passw" is extremely antisocial.  More
    ;; antisocial than echoing a password, and more likely than connecting
    ;; to a non-Unix rsh host these days...
    ;;
    ;; I disagree with the above.  -sb
    ;;
    (set-process-filter (get-process name) (if rsh-eat-password-string
					       'telnet-initial-filter
					     'telnet-filter))
    ;; (set-process-filter (get-process name) 'telnet-filter)
    ;; run last so that hooks can change things.
    (telnet-mode)))

(provide 'telnet)

;;; telnet.el ends here
