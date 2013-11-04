;;; highlight-headers.el --- highlighting message headers.

;; Copyright (C) 1992, 1993, 1994, 1995, 2002 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems

;; Keywords: mail, news

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

;;; Synched up with: Not in FSF.

;; This code is shared by RMAIL and VM.
;;
;; Faces:
;;
;; message-headers			the part before the colon
;; message-header-contents		the part after the colon
;; message-highlighted-header-contents	contents of "special" headers
;; message-cited-text			quoted text from other messages
;;
;; Variables:
;;
;; highlight-headers-regexp			what makes a "special" header
;; highlight-headers-citation-regexp		matches lines of quoted text
;; highlight-headers-citation-header-regexp	matches headers for quoted text

(defgroup highlight-headers nil
  "Fancify rfc822 documents."
  :group 'mail
  :group 'news)

(defgroup highlight-headers-faces nil
  "Faces of highlighted headers."
  :group 'highlight-headers
  :group 'faces)

(defface message-headers '((t (:bold t)))
  "Face used for header part before colon."
  :group 'highlight-headers-faces)

(defface message-header-contents '((t (:italic t)))
  "Face used for header part after colon."
  :group 'highlight-headers-faces)

(defface message-highlighted-header-contents '((t (:italic t :bold t)))
  "Face used for contents of \"special\" headers."
  :group 'highlight-headers-faces)

(defface message-cited-text '((t (:italic t)))
  "Face used for cited text."
  :group 'highlight-headers-faces)

(defface message-url '((t (:bold t)))
  "Face used for URLs."
  :group 'highlight-headers-faces)

(defface x-face '((t (:background "white" :foreground "black")))
  "Face used for X-Face icon."
  :group 'highlight-headers-faces)

;;(condition-case nil
;;    (face-name 'message-addresses)
;;  (wrong-type-argument
;;   (make-face 'message-addresses)
;;   (or (face-differs-from-default-p 'message-addresses)
;;       (progn
;;	 (copy-face 'bold-italic 'message-addresses)
;;	 (set-face-underline-p 'message-addresses
;;			       (face-underline-p
;;				'message-highlighted-header-contents))))))

(defcustom highlight-headers-regexp "Subject[ \t]*:"
  "*The headers whose contents should be emphasized more.
The contents of these headers will be displayed in the face 
`message-highlighted-header-contents' instead of `message-header-contents'."
  :type 'regexp
  :group 'highlight-headers)

(defcustom highlight-headers-citation-regexp
  (concat "^\\("
	  (mapconcat 'identity
	   '("[ \t]*[a-zA-Z0-9_]+>+"	; supercite
	     "[ \t]*[>]+"		; ">" with leading spaces
	     "[]}<>|:]+[ \t]*"		; other chars, no leading space
	     )
	   "\\|")
	  "\\)[ \t]*")
  "*The pattern to match cited text.
Text in the body of a message which matches this will be displayed in
the face `message-cited-text'."
  :type 'regexp
  :group 'highlight-headers)

(defcustom highlight-headers-citation-header-regexp
  (concat "^In article\\|^In message\\|"
	  "^[^ \t].*\\(writes\\|wrote\\|said\\):\n"
	  (substring highlight-headers-citation-regexp 1))
  "*The pattern to match the prolog of a cited block.
Text in the body of a message which matches this will be displayed in
the `message-headers' face."
  :type 'regexp
  :group 'highlight-headers)

(defcustom highlight-headers-highlight-citation-too nil
  "*Whether the whole citation line should go in the `message-cited-text' face.
If nil, the text matched by `highlight-headers-citation-regexp' is in the
default face, and the remainder of the line is in the message-cited-text face."
  :type 'boolean
  :group 'highlight-headers)

(defcustom highlight-headers-max-message-size 10000
  "*If the message body is larger than this many chars, don't highlight it.
This is to prevent us from wasting time trying to fontify things like
uuencoded files and large digests.  If this is nil, all messages will
be highlighted."
  :type '(choice integer
		 (const :tag "Highlight All" nil))
  :group 'highlight-headers)

(defcustom highlight-headers-hack-x-face-p (featurep 'xface)
  "*If true, then the bitmap in an X-Face header will be displayed
in the buffer.  This assumes you have the `uncompface' and `icontopbm'
programs on your path."
  :type 'boolean
  :group 'highlight-headers)

(defcustom highlight-headers-convert-quietly nil
  "*Non-nil inhibits the message that is normally displayed when external
filters are used to convert an X-Face header.  This has no effect if
XEmacs is compiled with internal support for x-faces."
  :type 'boolean
  :group 'highlight-headers)

(defcustom highlight-headers-invert-x-face-data nil 
  "*If true, causes the foreground and background bits in an X-Face
header to be flipped before the image is displayed. If you use a
light foreground color on a dark background color, you probably want
to set this to t. This assumes that you have the `pnminvert' program
on your path.  This doesn't presently work with internal xface support."
  :type 'boolean
  :group 'highlight-headers)


;;;###autoload
(defun highlight-headers (start end hack-sig)
  "Highlight message headers between start and end.
Faces used:
  message-headers			the part before the colon
  message-header-contents		the part after the colon
  message-highlighted-header-contents	contents of \"special\" headers
  message-cited-text			quoted text from other messages
  message-url				URLs (WWW uniform resource locators)

Variables used:

  highlight-headers-regexp			what makes a \"special\" header
  highlight-headers-citation-regexp		matches lines of quoted text
  highlight-headers-citation-header-regexp	matches headers for quoted text

If HACK-SIG is true,then we search backward from END for something that
looks like the beginning of a signature block, and don't consider that a
part of the message (this is because signatures are often incorrectly
interpreted as cited text.)"
  (if (< end start)
      (let ((s start)) (setq start end end s)))
  (let* ((too-big (and highlight-headers-max-message-size
		       (> (- end start)
			  highlight-headers-max-message-size)))
	 (real-end end)
	 e p hend)
    ;; delete previous highlighting
    (map-extents (function (lambda (extent ignore)
			     (if (extent-property extent 'headers)
				 (delete-extent extent))
			     nil))
		 (current-buffer) start end)
    (save-excursion
      (save-restriction
	(widen)
	;; take off signature
	(if (and hack-sig (not too-big))
	    (save-excursion
	      (goto-char end)
	      (if (re-search-backward "\n--+ *\n" start t)
		  (if (eq (char-after (point)) ?\n)
		      (setq end (1+ (point)))
		    (setq end (point))))))
	(narrow-to-region start end)

	(save-restriction
	  ;; narrow down to just the headers...
	  (goto-char start)
	  ;; If this search fails then the narrowing performed above
	  ;; is sufficient
	  (if (re-search-forward "^$" nil t)
	      (narrow-to-region (point-min) (point)))
	  (goto-char start)
	  (while (not (eobp))
	    (cond
	     ((looking-at "^\\([^ \t\n:]+[ \t]*:\\) *\\(.*\\(\n[ \t].*\\)*\n\\)")
	      (setq hend (match-end 0))
	      (setq e (make-extent (match-beginning 1) (match-end 1)))
	      (set-extent-face e 'message-headers)
	      (set-extent-property e 'headers t)
	      (setq p (match-end 1))
	      (cond
	       ((and highlight-headers-hack-x-face-p
		     (save-match-data (looking-at "^X-Face: *")))
		;; make the whole header invisible
		(setq e (make-extent (match-beginning 0) (match-end 0)))
		(set-extent-property e 'invisible t)
		(set-extent-property e 'headers t)
		;; now extract the xface and put it somewhere interesting
		(let ((xface (highlight-headers-x-face-to-pixmap
			      (match-beginning 2)
			      (match-end 2))))
		  (if (not xface)
		      nil		; just leave the header invisible if
					; we can't convert the face for some
					; reason 
		    (cond ((save-excursion
			     (goto-char (point-min))
			     (save-excursion (re-search-forward "^From: *"
								nil t)))
			   (setq e (make-extent (match-end 0)
						(match-end 0))))
			  (t
			   ;; okay, make the beginning of the invisible
			   ;; move forward to only hide the modem noise...
			   (set-extent-endpoints e
						 (match-beginning 2)
						 (1- (match-end 2)))
			   ;; kludge: if a zero-length extent exists at the
			   ;; starting point of an invisible extent, then
			   ;; it's invisible... even if the invisible extent
			   ;; is start-open.  
			   (setq e (make-extent (1- (match-beginning 2))
						(match-beginning 2)))
			   ))
		    (set-extent-property e 'headers t)
		    (set-extent-end-glyph e xface))
		  ))
;;; I don't think this is worth the effort
;;;           ((looking-at "\\(From\\|Resent-From\\)[ \t]*:")
;;;            (setq current 'message-highlighted-header-contents)
;;;            (goto-char (match-end 0))
;;;            (or (looking-at ".*(\\(.*\\))")
;;;                (looking-at "\\(.*\\)<")
;;;                (looking-at "\\(.*\\)[@%]")
;;;                (looking-at "\\(.*\\)"))
;;;            (end-of-line)
;;;            (setq e (make-extent p (match-beginning 1)))
;;;            (set-extent-face e current)
;;;            (set-extent-property e 'headers t)
;;;            (setq e (make-extent (match-beginning 1) (match-end 1)))
;;;            (set-extent-face e 'message-addresses)
;;;            (set-extent-property e 'headers t)
;;;            (setq e (make-extent (match-end 1) (point)))
;;;            (set-extent-face e current)
;;;            (set-extent-property e 'headers t)
;;;            )
	       ((and highlight-headers-regexp
		     (save-match-data (looking-at highlight-headers-regexp)))
		(setq e (make-extent (match-beginning 2) (match-end 2)))
		(set-extent-face e 'message-highlighted-header-contents)
		(set-extent-property e 'headers t))
	       (t
		(setq e (make-extent (match-beginning 2) (match-end 2)))
		(set-extent-face e 'message-header-contents)
		(set-extent-property e 'headers t))
 	       )
 	      (goto-char hend))
 	     ;; ignore non-header field name lines
 	     (t (forward-line 1)))))

	;; now do the body, unless it's too big....
	(if too-big
	    nil
	  (while (not (eobp))
	    (cond ((null highlight-headers-citation-regexp)
		   nil)
		  ((looking-at highlight-headers-citation-regexp)
		   (or highlight-headers-highlight-citation-too
		       (goto-char (match-end 0)))
		   (or (save-excursion
			 (beginning-of-line)
			 (let ((case-fold-search nil)) ; aaaaah, unix...
			   (looking-at "^>From ")))
		       (setq current 'message-cited-text)))
;;;                ((or (looking-at "^In article\\|^In message")
;;;                     (looking-at
;;;            "^[^ \t].*\\(writes\\|wrote\\|said\\):\n[ \t]+[A-Z]*[]}<>|]"))
;;;                 (setq current 'message-headers))
		  ((null highlight-headers-citation-header-regexp)
		   nil)
		  ((looking-at highlight-headers-citation-header-regexp)
		   (setq current 'message-headers))
		  (t (setq current nil)))
	    (cond (current
		   (setq p (point))
		   (forward-line 1) ; this is to put the \n in the face too
		   (setq e (make-extent p (point)))
		   (forward-char -1)
		   (set-extent-face e current)
		   (set-extent-property e 'headers t)
		   ))
	    (forward-line 1)))
	))
    (save-excursion
      (save-restriction
	(widen)
	(narrow-to-region start real-end)
	(highlight-headers-mark-urls start real-end)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X-Face header conversion:

;;; This cache is only used if x-face conversion is done with external
;;; filters.  If XEmacs is compiled --with-xface, then it's better to
;;; convert it twice than to suck up memory for a potentially large cache of
;;; stuff that's not difficult to recreate.
(defvar highlight-headers-x-face-to-pixmap-cache nil)

(defun highlight-headers-x-face-to-pixmap (start end)
  (let* ((string (if (stringp start) start (buffer-substring start end)))
	 (data (assoc string highlight-headers-x-face-to-pixmap-cache)))
    (if (featurep 'xface)
	(let ((new-face (make-glyph (concat "X-Face: " string))))
	  (set-glyph-face new-face 'x-face)
	  new-face)
      ;; YUCK this is the old two-external-filters-plus-a-bunch-of-lisp method
      (if data
	  (cdr data)
	(setq data (cons string
			 (condition-case c
			     (highlight-headers-parse-x-face-data start end)
			   (error
			    (display-error c nil)
			    (sit-for 2)
			    nil))))
	(setq highlight-headers-x-face-to-pixmap-cache
	      (cons data highlight-headers-x-face-to-pixmap-cache))
	(cdr data)))
    ))

;;; Kludge kludge kludge for displaying the bitmap in the X-Face header.

;;; This depends on the following programs: icontopbm, from the pbmplus
;;; toolkit (available everywhere) and uncompface, which comes with
;;; several faces-related packages, and can also be had at ftp.clark.net
;;; in /pub/liebman/compface.tar.Z.  See also xfaces 3.*.  Not needed
;;; for this, but a very nice xbiff replacment.

(defconst highlight-headers-x-face-bitrev
  (purecopy
   (eval-when-compile
     (let* ((v (make-string 256 0))
	    (i (1- (length v))))
       (while (>= i 0)
	 (let ((j 7)
	       (k 0))
	   (while (>= j 0)
	     (if (/= 0 (logand i (lsh 1 (- 7 j))))
		 (setq k (logior k (lsh 1 j))))
	     (setq j (1- j)))
	   (aset v i k))
	 (setq i (1- i)))
       v))))

(defun highlight-headers-parse-x-face-data (start end)
  (save-excursion
    (let ((b (current-buffer))
	  (lines 0)
	  p)
      (or highlight-headers-convert-quietly
	  (message "Converting X-Face header to pixmap ..."))
      (set-buffer (get-buffer-create " *x-face-tmp*"))
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (if (stringp start)
	  (insert start)
	(insert-buffer-substring b start end))
      (while (search-forward "\n" nil t)
	(skip-chars-backward " \t\n")
	(setq p (point))
	(skip-chars-forward " \t\n")
	(delete-region p (point)))
      (call-process-region (point-min) (point-max) "uncompface" t t nil)
      (goto-char (point-min))
      (while (not (eobp))
	(or (looking-at "0x....,0x....,0x...., *$")
	    (error "unexpected uncompface output"))
	(forward-line 1)
	(setq lines (1+ lines))
	(delete-char -1))
      (goto-char (point-min))
      (insert (format "/* Format_version=1, Width=%d, Height=%d" lines lines))
      (insert ", Depth=1, Valid_bits_per_item=16\n */\n")
      (while (not (eobp))
	(insert ?\t)
	(forward-char 56) ; 7 groups per line
	(insert ?\n))
      (forward-char -1)
      (delete-char -1)  ; take off last comma
      ;;
      ;; Ok, now we've got the format that "icontopbm" knows about.
      (call-process-region (point-min) (point-max) "icontopbm" t t nil)
      ;; Invert the image if the user wants us to...
      (if highlight-headers-invert-x-face-data
	  (call-process-region (point-min) (point-max) "pnminvert" t t nil))
      ;;
      ;; If PBM is using binary mode, we're winning.
      (goto-char (point-min))
      (let ((new-face))
	(cond ((looking-at "P4\n")
	       (forward-line 2)
	       (delete-region (point-min) (point))
	       (while (not (eobp))
		 (insert (aref highlight-headers-x-face-bitrev
			       (following-char)))
		 (delete-char 1))
	       (setq new-face (make-glyph
			       (vector 'xbm :data
				       (list lines lines (prog1 (buffer-string)
							   (erase-buffer))))))
	       (set-glyph-image new-face "[xface]" 'global 'tty)
	       (set-glyph-face new-face 'x-face))
	      (t ; fix me
	       (error "I only understand binary-format PBM...")))
	(or highlight-headers-convert-quietly
	    (message "Converting X-Face header to pixmap ... done."))
	new-face)
      )))


;;; "The Internet's new BBS!" -Boardwatch Magazine
;;; URL support by jwz@jwz.org

(defcustom highlight-headers-mark-urls (string-match "XEmacs" emacs-version)
  "*Whether to make URLs clickable in message bodies."
  :type 'boolean
  :group 'highlight-headers)

;; We use browse-url for opening the URLs.

(defvar highlight-headers-follow-url-function 'w3-fetch "Not used.")
(make-obsolete-variable 'highlight-headers-follow-url-function
			'browse-url-browser-function)

(defvar highlight-headers-follow-url-netscape-auto-raise t "Not used.")
(make-obsolete-variable
 'highlight-headers-follow-url-netscape-auto-raise "see `browse-url-netscape'")

(define-obsolete-variable-alias
  'highlight-headers-follow-url-netscape-new-window
  'browse-url-new-window-flag)

;;;###autoload
(define-obsolete-function-alias
  'highlight-headers-follow-url-netscape 'browse-url-netscape)

;;;###autoload
(define-obsolete-function-alias
  'highlight-headers-follow-url-kfm 'browse-url-kde)

;;;###autoload
(define-obsolete-function-alias
  'highlight-headers-follow-url-mosaic 'browse-url-mosaic)

(defvar highlight-headers-url-keymap
  (let ((m (make-sparse-keymap)))
    (set-keymap-name m 'highlight-headers-url-keymap)
    (if (string-match "XEmacs" emacs-version)
	(progn
	  (define-key m 'button2 'highlight-headers-follow-url)
	  ))
    m))

;;;###autoload
(defun highlight-headers-follow-url (event)
  (interactive "e")
  (let* ((p (event-point event))
	 (buffer (window-buffer (event-window event)))
	 (extent (and p (extent-at p buffer 'highlight)))
	 (url (and extent
		   (save-excursion
		     (set-buffer buffer)
		     (buffer-substring (extent-start-position extent)
				       (extent-end-position extent))))))
    (if (and url (string-match "\\`<\\([^>]+\\)>\\'" url))
	(setq url (concat "news:"
			  (substring url (match-beginning 1) (match-end 1)))))
    (if url
	(browse-url url)
      (beep))))


(defconst highlight-headers-url-pattern
  (concat
   "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|s?news\\|telnet\\|mailbox\\):"
	  "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
	  "[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]+"
	  ))

(defun highlight-headers-mark-urls (start end)
  (cond
   (highlight-headers-mark-urls
    (save-excursion
      (goto-char start)
      (while (re-search-forward highlight-headers-url-pattern nil t)
	(let ((s (match-beginning 0))
	      e
	      extent)
	  (goto-char (match-end 0))
	  ;(skip-chars-forward "^ \t\n\r")
	  (skip-chars-backward ".?#!*()")
	  (setq e (point))
	  (setq extent (make-extent s e))
	  (set-extent-face extent 'bold)
	  (set-extent-property extent 'highlight t)
	  (set-extent-property extent 'headers t)
	  (set-extent-property extent 'keymap highlight-headers-url-keymap)
	  ))

      (goto-char start)
      (while (re-search-forward "^Message-ID: \\(<[^>\n]+>\\)" nil t)
	(let ((s (match-beginning 1))
	      (e (match-end 1))
	      extent)
	  (setq extent (make-extent s e))
	  (set-extent-face extent 'message-url)
	  (set-extent-property extent 'highlight t)
	  (set-extent-property extent 'headers t)
	  (set-extent-property extent 'keymap highlight-headers-url-keymap)))
      ))))


(provide 'highlight-headers)
