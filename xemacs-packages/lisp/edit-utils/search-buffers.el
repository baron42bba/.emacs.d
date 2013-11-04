;;; search-buffers.el --- Searching REGEXP in all buffers for XEmacs

;; Copyright (C) 1998 Adrian Aichner

;; Author: Adrian Aichner, Teradyne GmbH Munich <aichner@ecf.teradyne.com>
;; Date: Sat Dec 26 1998
;; Version: $Revision: 1.2 $
;; Keywords: internal

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; The Idea:
;; Search all live buffers for REGEXP and present matching lines in
;; separate buffer with hyperlinks to their occurences.

;; The Concept:
;; After creating countless buffers in an XEmacs session, user
;; executes
;; M-x list-matches-in-buffers RET \<problem\> RET .* RET
;; to find all matches of the single word "problem" in any of them.
;; The result is presented in a buffer named
;; *Matches for "\<problem\>" in buffers*
;; with hyperlinks to any occurence.  User may navigate to the next
;; (n) or previous (p) match.

;; The Status:
;; Basic functionality is complete.

;; The Author:
;; Adrian Aichner, Teradyne GmbH Munich, Sun., Dec. 26, 1998.

;;; Code:

(defvar search-buffers-current-extent nil)

(defvar search-buffers-highlight-xtnt nil)

(defvar search-buffer nil)

;;;###autoload
(defun list-matches-in-buffers (regexp)
  "List lines matching REGEXP in any matching buffer.
All buffers chosen via `buffer-regexp-list' are searched.  Results are
displayed in a buffer named *Matches for \"REGEXP\" in buffers*
including hyperlinks to visit any match in any buffer."
  (interactive "sREGEXP: ")
  (if (equal regexp "")
      (error "cannot search buffers for empty regexp."))
  (let ((b (get-buffer-create
	    (format "*Matches for \"%s\" in buffers*" regexp)))
	(xtnt-keymap (make-keymap)))
    ;; Prepare local keymap and variables for buffer displaying
    ;; matches.
    (save-excursion
      (set-buffer b)
      (kill-all-local-variables)
      (make-variable-buffer-local 'search-buffers-current-extent)
      (setq search-buffers-current-extent nil)
      (make-variable-buffer-local 'kill-buffer-hook)
      (setq kill-buffer-hook 'search-buffers-cleanup)
      (toggle-read-only -1)
      (erase-buffer)
      (define-key xtnt-keymap
	[(button1)]
	'switch-to-match-by-event)
      (define-key xtnt-keymap
	[?n]
	'switch-to-next-match-by-extent)
      (define-key xtnt-keymap
	[?p]
	'switch-to-previous-match-by-extent)
      (define-key xtnt-keymap
	[?q]
	(function
	 (lambda ()
	   (interactive)
	   (kill-buffer search-buffer))))
      (use-local-map xtnt-keymap))
    ;; Map over matching buffers to build list of matching lines.
    (mapc
     (function
      (lambda (buffer-name)
	(set-buffer buffer-name)
	(goto-char (point-min))
	;; Carefully search for REGEXP moving to limit of search to
	;; avoid infinite looping.
	(while (search-forward-regexp regexp (point-max) 'move)
	  (let* ((line-num (count-lines (point-min) (point)))
		 (bol (point-at-bol))
		 (eol (point-at-eol))
		 (line (buffer-substring bol eol))
		 (beg (- (match-beginning 0) bol))
		 (end (- (match-end 0) bol))
		 offset
		 xtnt)
	    (goto-char eol)
	    (save-excursion
	      (set-buffer b)
	      ;; For buffers other than the search-buffer, insert
	      ;; match information in the search-buffer (which is
	      ;; current).
	      (unless (equal (get-buffer buffer-name) (current-buffer))
		(insert (format "%s:%d:"
				buffer-name line-num))
		(setq offset (point))
		(insert (format "%s\n" line))
		;; Make an extent to be used as hyperlink to the
		;; matching string in the buffer currently being
		;; searched.
		(setq xtnt (make-extent (+ offset beg) (+ offset end)))
		;; Detaching these extents defeats their utility.
		(set-extent-property xtnt 'detachable nil)
		;; Set properties to
		;; o locate matching string (buffer-name, line-num,
		;;   line-char-begin, line-char-end),
		;; o set hyperlink appearance (face, mouse-face),
		;; o set extent's keymap for buffer navigation
		;;   (keymap):
		(set-extent-property xtnt 'buffer-name buffer-name)
		(set-extent-property xtnt 'line-num line-num)
		(set-extent-property xtnt 'line-char-begin beg)
		(set-extent-property xtnt 'line-char-end end)
		(set-extent-property xtnt 'face 'bold)
		(set-extent-property xtnt 'mouse-face 'highlight)
		(if (featurep 'atomic-extents)
		    (set-extent-property xtnt 'atomic t))
		(set-extent-property xtnt 'keymap xtnt-keymap)))))))
     (buffer-regexp-list))
    ;; Pop to the beginning of buffer displaying matching lines.  Mark
    ;; that buffer unmodified and read-only.
    (pop-to-buffer b)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (toggle-read-only t)))

(defun switch-to-match-by-event (event)
  "Pop to buffer determined by the extent associated with event EVENT.
See `switch-to-match-by-extent' for details on the extent properties
required by these functions."
  (interactive "e")
;;; DEBUG
;;;   (display-info event)
  (pop-to-buffer (event-buffer event))
  (switch-to-match-by-extent
   (extent-at
    (event-point event)
    (event-buffer event))))

(defun switch-to-next-match-by-extent ()
  (interactive)
  ;; Store next extent to be the current one.
  (setq search-buffers-current-extent
	(next-extent (or search-buffers-current-extent
			 (current-buffer))))
  ;; Switch to the buffer described by the current extent, if defined.
  (if search-buffers-current-extent
      (switch-to-match-by-extent search-buffers-current-extent)
    (message "End of matches.")))

(defun switch-to-previous-match-by-extent ()
  (interactive)
  ;; Store previous extent to be the current one.
  (setq search-buffers-current-extent
	(previous-extent (or search-buffers-current-extent
			     (current-buffer))))
  ;; Switch to the buffer described by the current extent, if defined.
  (if search-buffers-current-extent
      (switch-to-match-by-extent search-buffers-current-extent)
    (message "Begin of matches.")))

(defun switch-to-match-by-extent (xtnt)
  (if xtnt
      (let ((buffer-name
	     (get xtnt 'buffer-name))
	    (line-char-begin
	     (get xtnt 'line-char-begin))
	    (line-char-end
	     (get xtnt 'line-char-end))
	    (line-num
	     (get xtnt 'line-num))
	    (xtnt-keymap (make-keymap))
	    (search-buffer (current-buffer))
	    offset)
	(setq search-buffers-current-extent xtnt)
	(goto-char
	 (extent-start-position xtnt))
	;; Pop to bufer indicated by extent.
	(if (get-buffer buffer-name)
	    (pop-to-buffer (get-buffer buffer-name))
	  (message "No such buffer %s." buffer-name))
;;; DEBUG
;;;	(display-info (allocate-event 'button-press '(button 1)))
	(if line-num
	    (goto-line line-num))
	(setq offset (point))
	(if line-char-begin
	    (forward-char line-char-begin))
	(if search-buffers-highlight-xtnt
	    (delete-extent search-buffers-highlight-xtnt))
	(setq search-buffers-highlight-xtnt
	      (make-extent (+ offset (or line-char-begin 0))
			   (+ offset (or line-char-end 0))))
	(define-key xtnt-keymap
	  [(button1)]
	  'switch-to-match-by-event)
	(set-extent-property search-buffers-highlight-xtnt
			     'face 'highlight)
	(set-extent-property search-buffers-highlight-xtnt
			     'buffer-name (buffer-name search-buffer))
	(set-extent-property search-buffers-highlight-xtnt
			     'keymap xtnt-keymap)
	(other-window 1))))

(defun display-info (event)
  (interactive "e")
  (with-current-buffer
      (get-buffer-create "*Matches Info*")
    (erase-buffer)
    (event-info event)
    (mapcar-extents 'extent-info 'identity (event-buffer event))))

(defun extent-info (xtnt)
  (insert (format "%s\n" (extent-properties xtnt))))

(defun event-info (event)
  (insert (format "%s\n" (event-properties event))))

(defun search-buffers-cleanup ()
  (if search-buffers-highlight-xtnt
      (delete-extent search-buffers-highlight-xtnt))
  (if search-buffer
      (setq search-buffer nil)))

(defun profile-it (arg)
  (interactive "P")
  (with-output-to-temp-buffer "*Profiling Output*"
    (unless (null arg)
      (clear-profiling-info))
    (profile-results
     (profile 
      (list-matches-in-buffers "fix")))))
;;; Function Name            Ticks    %/Total   Call Count
;;; =====================    =====    =======   ==========
;;; count-lines              1570     69.224    450
;;; search-forward-regexp    117      5.159     483
;;; buffer-substring         106      4.674     450
;;; insert                   102      4.497     450
;;; format                   78       3.439     451
;;; save-excursion           52       2.293     451
;;; set-buffer               50       2.205     484
;;; set-extent-property      34       1.499     1350
;;; let*                     32       1.411     450

(defun buffer-regexp-list ()
  "Return the list of buffer names matching REGEXP.
The list of buffer names is displayed for the user to either confirm
or re-enter a REGEXP to choose the desired list of buffers."
  (interactive)
  (let (buffers regexp tmpbuf)
    (while
	(progn
	  (setq regexp (read-string "Buffer Name REGEXP: " ".*"))
	  (setq tmpbuf (format "*Buffers matching \"%s\"*" regexp))
	  (setq buffers
		(mapcan
		 (function
		  (lambda (b)
		    (if (string-match regexp (buffer-name b))
			(list (buffer-name b)))))
		 (buffer-list)))
	  (with-output-to-temp-buffer
	      tmpbuf
	    (mapc
	     (function
	      (lambda (b)
		(prin1 b)
		(princ "\n")))
	     buffers))
	  (not (y-or-n-p "OK to use these buffers? "))))
    (kill-buffer tmpbuf)
    buffers))
