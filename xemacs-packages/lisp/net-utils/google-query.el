;; google-query.el --- Query Google from within XEmacs.   -*- Emacs-Lisp -*-

;; Copyright (C) 2003, 2004 Steve Youngs

;; RCS: $Id: google-query.el,v 1.7 2005/05/01 01:09:38 youngs Exp $
;; Author:        Steve Youngs <sryoungs@bigpond.net.au>
;; Maintainer:    Steve Youngs <sryoungs@bigpond.net.au>
;; Created:       <2003-12-16>
;; Last-Modified: <2005-05-01 10:49:42 (steve)>
;; Keywords:      web google search query

;; This file is part of google-query.

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
;;   I got the idea for this from Erik Arneson's `google-search.el'
;;   which you can get from <http://erik.arneson.org/google-search.el>
;;
;;   There are 2 entry points here, `google-query' and
;;   `google-query-region'.  The former will prompt for a string to
;;   query Google for, and the latter will query Google for whatever
;;   text is in the active region in the current buffer.  Bind these
;;   functions to some global keys for convenience.
;;
;;   Once the query completes XEmacs pops up a buffer containing
;;   the query results, sans all the cruft an advertising you get
;;   from Google.  Hitting button2 or RET on a URL will fire up your
;;   default browser with that URL.

;;; Todo:
;;
;;   

;;; ChangeLog:
;;
;;  From this point on, `google-query.el' is in the XEmacs packages
;;  CVS repository.  For further changes please consult
;;  ./xemacs-packages/net-utils/ChangeLog.
;;
;;  Revision 1.4  2003-12-16 23:15:46+10  steve
;;  Deactivate the region after sending the query from
;;  `google-query-region' because processing the results works on
;;  regions.
;;
;;  Revision 1.3  2003-12-16 18:38:10+10  steve
;;  Rename `google-search-version' to `google-query-version'.
;;
;;  Revision 1.2  2003-12-16 18:24:50+10  steve
;;  Fix a couple of byte-compiler warnings.
;;
;;  Revision 1.1  2003-12-16 17:10:03+10  steve
;;  Initial revision
;;

;;; Code:

(defun google-query-version (&optional arg)
  "Return the current version info for google-query.

With optional argument ARG, insert version info at point in the current
buffer."
  (interactive "P")
  (let (ver)
    (with-temp-buffer
      (erase-buffer)
      (insert-file (locate-library "google-query.el"))
      (goto-char (point-min))
      (re-search-forward 
       "google-query\\.el,v\\s-\\([0-9]+[0-9\\.]*[0-9]+\\)" nil t)
      (setq ver (match-string 1)))
    (if (interactive-p)
	(if arg
	    (insert (format "Google Query v%s" ver))
	  (message "Google Query v%s" ver))
      ver)))

(eval-and-compile
  (autoload 'with-electric-help "ehelp")
  (autoload 'browse-url "browse-url" nil t))

(defgroup google nil
  "Why leave XEmacs just to search Google..."
  :prefix "google-"
  :group 'hypermedia)

(defcustom google-query-maxlen 100
  "Maximum string length of query string.

This prevents you from accidentally sending a five megabyte query
string to Google.

You can set this reasonably high, all the same.  I think the maximum
length that Google can take is 2048 characters."
  :type 'number
  :group 'google)

(defcustom google-query-result-count 10
  "Max number of results to return from a `google-query'."
  :type 'number
  :group 'google)

(defcustom google-query-mirror "www.google.com"
  "*Your favourite Google mirror.

Omit the \"http://\" part, all we want here is a domain."
  :type 'string
  :group 'google)

(defcustom google-query-debug nil
  "When non-nil keep the process buffer around."
  :type 'boolean
  :group 'google)

(defun google-query-commentary ()
  "*Display the commentary section of google-query.el."
  (interactive)
  (with-electric-help
   '(lambda ()
      (insert
       (with-temp-buffer
	 (erase-buffer)
	 (insert (lm-commentary (locate-library "google-query.el")))
	 (goto-char (point-min))
	 (while (re-search-forward "^;+ ?" nil t)
	   (replace-match "" nil nil))
	 (buffer-string (current-buffer)))))
   "*Google-query Commentary*"))

(defun google-query-copyright ()
  "*Display the copyright notice for google-query."
  (interactive)
  (with-electric-help
   '(lambda ()
      (insert
       (with-temp-buffer
	 (erase-buffer)
	 (insert-file-contents (locate-library "google-query.el"))
	 (goto-char (point-min))
	 (re-search-forward ";;; Commentary" nil t)
	 (beginning-of-line)
	 (narrow-to-region (point-min) (point))
	 (while (re-search-backward "^;+ ?" nil t)
	   (replace-match "" nil nil))
	 (buffer-string (current-buffer)))))
   "*Google-query Copyright Notice*"))

;; Ripped from thingatpt.el
(defconst google-query-url-regexp
  (concat
   "\\(https?://\\|ftp://\\|gopher://\\|telnet://\\|wais://\\|file:/\\|s?news:\\|mailto:\\)"
   "[^]\t\n \"'()<>[^`{}]*[^]\t\n \"'()<>[^`{}.,;]+")
  "A regular expression matching URLs.")

(defun google-query-url-at-point ()
  "Browse to a URL from the google-query buffer."
  (interactive)
  (when (extentp (extent-at (point)))
    (browse-url (extent-string (extent-at (point))))))

(defun google-query-url-at-mouse (event)
  "Browse to a URL at EVENT via the mouse from the google-query buffer."
  (interactive "e")
  (when (extentp (extent-at-event event))
    (browse-url (extent-string (extent-at-event event)))))

(defun google-query-kill-buffer ()
  (interactive)
  (kill-buffer nil))

(defconst google-query-mode-map
  (let* ((map (make-sparse-keymap 'google-query-mode-map)))
    (define-key map [space] 'scroll-up)
    (define-key map [delete] 'scroll-down)
    (define-key map [q] 'bury-buffer)
    (define-key map [Q] 'google-query-kill-buffer)
    map)
  "A keymap for the google query buffer.")

(defconst google-query-ext-map
  (let* ((map (make-sparse-keymap 'google-query-ext-map)))
    (define-key map [button2] 'google-query-url-at-mouse)
    (define-key map [return] 'google-query-url-at-point)
    map)
  "A keymap for the extents in google query results buffer.")

;; Unashamedly stolen from Bill Perry's URL package.
(defconst google-query-unreserved-chars
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
       ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
       ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\))
  "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from RFC 2396.")

;; Unashamedly stolen from Bill Perry's URL package.
(defun google-query-hexify-string (str)
  "Escape characters STR so STR can be used in a URL."
  (mapconcat
   (lambda (char)
     ;; Fixme: use a char table instead.
     (if (not (memq char google-query-unreserved-chars))
	 (if (< char 16)
	     (format "%%0%X" char)
	   (if (> char 255)
	       (error "Hexifying multibyte character %s" str))
	   (format "%%%X" char))
       (char-to-string char)))
   str ""))

(defun google-query-make-url-extents ()
  "Create extent objects for all the URLs in the buffer."
  (goto-char (point-min))
  (save-excursion
    (while (re-search-forward google-query-url-regexp nil t)
      (let ((extent (make-extent (match-beginning 0) (match-end 0)))
	    (echo "RET or Button2 to visit this URL."))
	(set-extent-property extent 'face 'bold)
	(set-extent-property extent 'mouse-face 'highlight)
	(set-extent-property extent 'keymap google-query-ext-map)
	(set-extent-property extent 'help-echo echo)
	(set-extent-property extent 'balloon-help echo)
	(set-extent-property extent 'duplicable t)))))

(defun google-query-mode ()
  "Major mode for google-query results buffer.
\\{google-query-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map google-query-mode-map)
  (setq mode-name "Google")
  (setq major-mode 'google-query-mode))

(defun google-query-process-results (results)
  "Process the RESULTS of `google-query'."
  (let ((buf (get-buffer-create "*google-query-results*"))
	matches)
    (switch-to-buffer buf)
    (google-query-mode)
    (erase-buffer)
    (insert results)
    (goto-char (point-min))
    ;; Get rid of lots of useless raw HTML and advertising.
    (re-search-forward "dict&q=" nil t)
    (forward-line 1)
    (kill-region (point-min) (point))
    (goto-char (point-min))
    (re-search-forward "<span class" nil t)
    (forward-line -1)
    (kill-region (point) (point-max))
    ;; Collect the stuff we want.
    (goto-char (point-min))
    (while (re-search-forward "<a href=\\(.*\\)<br>" nil t)
      (setq matches (push (match-string 1) matches)))
    (setq matches (nreverse matches))
    ;; Replace the contents of the buffer with our matches.
    (erase-buffer)
    (insert "Google Query Results\n====================\n\n")
    (while matches
      (insert (car matches))
      (insert "\n\n")
      (setq matches (cdr matches)))
    (goto-char (point-min))
    (center-line 2)
    (mapcar
     '(lambda (x) (save-excursion (eval x)))
     '((replace-string "<b>" " ")
       (replace-string "</b>" "")
       (replace-regexp "<.*>" " ")
       (replace-string ">" " ")))
    (save-excursion
      (fill-region (point) (point-max)))
    (google-query-make-url-extents)))

;;;###autoload
(defun google-query (string)
  "Query google for STRING."
  (interactive "sQuery Google for: ")
  (let* ((host google-query-mirror)
	 (user-agent (concat "XEmacs-" emacs-program-version))
	 (str (google-query-hexify-string 
	       (truncate-string-to-width string google-query-maxlen)))
	 (query (concat "search?&q=" str 
			"&num=" (format "%d" google-query-result-count)))
	 (coding-system-for-read 'binary)
	 (coding-system-for-write 'binary)
	 (google (open-network-stream
		  "google-query"
		  " *google-query-proc*"
		  host
		  80))
	 (pbuf (process-buffer google)))
    (process-send-string
     google
     (concat "GET /" query " HTTP/1.1\r\n"
	     "MIME-Version: 1.0\r\n"
	     "Connection: close\r\n"
	     "Extension: Security/Digest Security/SSL\r\n"
	     "Host: " host "\r\n"
	     "Accept: */*\r\n"
	     "User-Agent: " user-agent "\r\n\r\n"))
    (message "Talking to Google, please wait...")
    (while (eq (process-status google) 'open)
      (sleep-for 0.05))
    (google-query-process-results (buffer-string pbuf))
    (unless google-query-debug
      (kill-buffer pbuf))))

;;;###autoload    
(defun google-query-region (beg end)
  "Query google for the string BEG END."
  (interactive "r")
  (let ((str (buffer-substring-no-properties beg end)))
    (zmacs-deactivate-region)
    (google-query str)))

(provide 'google-query)
;;; google-query.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End:
