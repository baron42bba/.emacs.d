;;; patch-keywords.el --- Insert action keywords into patch followups.

;; Copyright (C) 2002 Steve Youngs

;; RCS: $Id: patch-keywords.el,v 1.4 2003/08/17 09:34:55 adrian Exp $
;; Author:        Steve Youngs <youngs@xemacs.org>
;; Maintainer:    Steve Youngs <youngs@xemacs.org>
;; Created:       2002-01-14
;; Last-Modified: <2002-03-04 07:18:01 (steve)>
;; Keywords:      maint

;; This file is part of XEmacs

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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA


;;; Commentary:
;;
;;  This file is an aid to mailing followups to patches submitted via
;;  email.  It adds "Reviewer Action Keywords" to the message.  These
;;  "keywords" can later be used as an aid to patch tracking.

;;  At this stage, this file is probably only useful to anyone who
;;  reviews patches submitted to <xemacs-patches@xemacs.org>.

;;  To use: 
;;    - M-x customize-group RET patch-review RET
;;       Change things to your liking.
;;    - (require 'patch-keywords)
;;       In your ~/.xemacs/init.el

;;  Then when you are following up to a patch submission, just hit
;;  'M-p'.  You'll be prompted for the keywords to apply to the
;;  message.  You can use the history mechanism to select keywords.
;;  Enter a null keyword (just hit 'RET') to terminate the list of
;;  keywords.

;;  The keywords are added to 3 separate places in the message.
;;    - In abbreviated form (1st character of each keyword) enclosed
;;      in square brackets at the start of the subject header (version
;;      numbers are not abbreviated).
;;
;;    - In a "X-Reviewer-Action:" header (full keywords).
;;
;;    - At line 0, column 0 of the message body.

;;  To see how to setup a "X-Reviewer-Action:" header, see
;;  `gnus-posting-styles'

;;  Many thanks to Adrian Aichner <adrian@xemacs.org> for his ideas,
;;  code examples and testing.

;;; Code:
(eval-when-compile
  (require 'message)
  (autoload 'gnus-continuum-version "gnus"))

;;;###autoload
(defgroup patch-review nil
  "Patch submission review."
  :group 'mail)

(defcustom patch-keywords
  '("APPROVE"
    "COMMIT"
    "FORWARD"
    "QUERY"
    "RECOMMEND"
    "SUPERSEDES"
    "VETO"
    "21.4"
    "21.5")
  "List of keywords used for reviewing patches.

The default values are the keywords currently used by the XEmacs
Review Board."
  :group 'patch-review
  :type '(repeat (string :tag "Review Action Keyword"))
  :tag "Action Keywords")

(defcustom patch-review-mua 'gnus
  "The MUA (Mail User Agent) you use for reviewing patches.

Currently, the only MUAs that are supported are Gnus and VM.  Should
we even bother with things like MEW or Rmail?"
  :group 'patch-review
  :type '(choice
	  (item gnus)
	  (item vm)
	  (item mew\ \(Not\ Supported\))
	  (item rmail\ \(Not\ Supported\)))
  :tag "MUA")

(defcustom patch-keywords-followup-to "XEmacs Beta <xemacs-beta@xemacs.org>"
  "The address to put into the \"Mail-Followup-To:\" header.
This is so that any further discussions relating to the submitted
patch can take place in a separate forum."
  :group 'patch-review
  :type 'string
  :tag "Followups Address")

;; I use the bleeding edge Gnus (Oort 0.05), so consequently we have
;; to define a couple of functions that aren't in the XEmacs package
;; version of Gnus (5.8.8).  Later on they're wrapped in a version
;; test.
(defun patch-keywords-in-header-p ()
  "Return t if point is in the header.
Same as `message-point-in-header-p' which exists in Gnus Oort, but not
in Gnus 5.8.8"
  (save-excursion
    (let ((p (point)))
      (goto-char (point-min))
      (not (re-search-forward
 	    (concat "^" (regexp-quote mail-header-separator) "\n")
 	    p t)))))

(defun patch-keywords-message-beginning-of-line (&optional n)
  "Move point to beginning of header value or to beginning of line.
Optional argument N non-nil or 1, move forward N - 1 lines first.
Same as `message-beginning-of-line' which exists in Gnus Oort, but not
in Gnus 5.8.8."
  (interactive "p")
  (if (if (< (gnus-continuum-version gnus-version) 5.090004)
	(patch-keywords-in-header-p)
      (message-point-in-header-p))
      (let* ((here (point))
	     (bol (progn (beginning-of-line n) (point)))
	     (eol (gnus-point-at-eol))
	     (eoh (re-search-forward ": *" eol t)))
	(if (or (not eoh) (equal here eoh))
	    (goto-char bol)
	  (goto-char eoh)))
    (beginning-of-line n)))

(defun patch-keywords-insert-gnus (patch-key)
  "Insert the action keywords into patch followups.

Argument PATCH-KEY A list of action keywords as defined in
`patch-keywords'.  They may be chosen interactively via the
history mechanism.

Insert abbreviated (1st char) keywords at the beginning of the subject
header.  Full keywords into the \"X-Reviewer-Action:\" header, if
present, and also at the start of the message body.

The \"X-Reviewer-Action:\" header can be easily inserted using
`gnus-posting-styles'.

This function also sets followups to xemacs-beta@xemacs.org."
  (interactive
   (let
       ((hist patch-keywords)
        key
        keys)
     (while (not
             (string-equal
              (setq key
                    (read-string
		     "Enter patch keywords (or RET to finish): " "" 'hist))
              ""))
       (setq keys (cons key keys)))
     (list (mapconcat 'identity (reverse keys) " "))))
  (if (string-equal patch-key "")
      (error "Choose at least one patch-key from %s"
             (mapconcat 'identity patch-keywords ", ")))
  (save-excursion
    ;; We need to preserve the original subject header so something
    ;; like "fix for 21.5 not for 21.4" doesn't turn into "fix for
    ;; 21.5not for 21.4"
    (message-goto-subject)
    (if (< (gnus-continuum-version gnus-version) 5.090004)
	(patch-keywords-message-beginning-of-line)
      (message-beginning-of-line))
    (re-search-forward ".*$" (eolp) t)
    (let ((oldsub (match-string 0))
	  (keywords (concat "\\("
			    (regexp-opt patch-keywords)
			    "\\) ")))
      ;; Clear the original subject (reinstate it later)
      (if (< (gnus-continuum-version gnus-version) 5.090004)
	  (patch-keywords-message-beginning-of-line)
	(message-beginning-of-line))
      (if (re-search-forward ".*$" (eolp) t)
	  (replace-match ""))
      ;; Insert the long patch keywords
      (insert-string
       (concat "[" patch-key " ]"))
      (insert-string " ")
      ;; Convert to abbreviated patch keywords
      (if (< (gnus-continuum-version gnus-version) 5.090004)
	  (patch-keywords-message-beginning-of-line)
	(message-beginning-of-line))
      (save-restriction
        (narrow-to-region (point) (point-at-eol))
        (while (re-search-forward keywords (eolp) t)
          (let ((keyword (match-string 1)))
            (if (save-match-data
                  (string-match "\\`[.0-9]+\\'" keyword))
                (replace-match (match-string 1))
              (replace-match (substring (match-string 1) 0 1))))))
      ;; Reinstate the original subject header after the keywords
      (end-of-line)
      (insert-string oldsub))
    ;; Insert keywords into the 'X-Reviewer-Action:' header
    (goto-line 0)
    (if (re-search-forward "^X-Reviewer-Action: " nil t)
	(insert-string patch-key))
    ;; Set followups to go to xemacs-beta
    (if (< (gnus-continuum-version gnus-version) 5.090004)
	(message-position-on-field "Mail-Followup-To" "From")
      (message-goto-mail-followup-to))
    (insert-string patch-keywords-followup-to)
    ;; Insert the keywords into the body of the message
    (message-goto-body)
    (insert-string patch-key)
    (insert-string "\n\n")))

(defun patch-keywords-insert-vm (patch-key)
  "Insert the action keywords into patch followups.

Argument PATCH-KEY A list of action keywords as defined in
`patch-keywords'.  They may be chosen interactively via the
history mechanism.

Insert abbreviated (1st char) keywords at the beginning of the subject
header.  Full keywords into the \"X-Reviewer-Action:\" header, and
also at the start of the message body.

This function also sets followups to xemacs-beta@xemacs.org."
  (interactive
   (let
       ((hist patch-keywords)
        key
        keys)
     (while (not
             (string-equal
              (setq key
                    (read-string
		     "Enter patch keywords (or RET to finish): " "" 'hist))
              ""))
       (setq keys (cons key keys)))
     (list (mapconcat 'identity (reverse keys) " "))))
  (if (string-equal patch-key "")
      (error "Choose at least one patch-key from %s"
             (mapconcat 'identity patch-keywords ", ")))
  (save-excursion
    ;; We need to preserve the original subject header so something
    ;; like "fix for 21.5 not for 21.4" doesn't turn into "fix for
    ;; 21.5not for 21.4"
    (mail-subject)
    (patch-keywords-message-beginning-of-line)
    (re-search-forward ".*$" (eolp) t)
    (let ((oldsub (match-string 0))
	  (keywords (concat "\\("
			    (regexp-opt patch-keywords)
			    "\\) ")))
      ;; Clear the original subject (reinstate it later)
      (patch-keywords-message-beginning-of-line)
      (if (re-search-forward ".*$" (eolp) t)
	  (replace-match ""))
      ;; Insert the long patch keywords
      (insert-string
       (concat "[" patch-key " ]"))
      (insert-string " ")
      ;; Convert to abbreviated patch keywords
      (patch-keywords-message-beginning-of-line)
      (save-restriction
        (narrow-to-region (point) (point-at-eol))
        (while (re-search-forward keywords (eolp) t)
          (let ((keyword (match-string 1)))
            (if (save-match-data
                  (string-match "\\`[.0-9]+\\'" keyword))
                (replace-match (match-string 1))
              (replace-match (substring (match-string 1) 0 1))))))
      ;; Reinstate the original subject header after the keywords
      (end-of-line)
      (insert-string oldsub))
    ;; Insert keywords into the 'X-Reviewer-Action:' header
    (goto-line 0)
    (insert-string (concat "X-Reviewer-Action: " patch-key "\n"))
    ;; Set followups to go to xemacs-beta
    (goto-line 0)
    (insert-string "Mail-Followup-To: ")
    (insert-string (concat patch-keywords-followup-to "\n"))
    ;; Insert the keywords into the body of the message
    (mail-text)
    (insert-string patch-key)
    (insert-string "\n\n")))

;; Bind 'patch-keywords-insert-MUA' to M-p.
(cond ((string= patch-review-mua "gnus")
       (define-key message-mode-map "\M-p" 'patch-keywords-insert-gnus))
      ((string= patch-review-mua "vm")
       (define-key mail-mode-map "\M-p" 'patch-keywords-insert-vm)))

(provide 'patch-keywords)

;;; patch-keywords.el ends here

;Local Variables:
;time-stamp-start: "Last-Modified:[ 	]+\\\\?[\"<]+"
;time-stamp-end: "\\\\?[\">]"
;time-stamp-line-limit: 10
;time-stamp-format: "%4y-%02m-%02d %02H:%02M:%02S (%u)"
;End:
