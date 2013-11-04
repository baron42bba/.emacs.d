;;; mail-abbrev.el --- Abbrev-expansion of mail aliases.

;;; Copyright (C) 1985-1994 Free Software Foundation, Inc.

;; Created: 19 oct 90, Jamie Zawinski <jwz@jwz.org>
;; Modified: 5 apr 92, Roland McGrath <roland@gnu.ai.mit.edu>
;; Maintainer: XEmacs Development Team

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

;;; Synched up with:  Not synched.

;;; Commentary:

;; This file ensures that, when the point is in a To:, CC:, BCC:, or From: 
;; field, word-abbrevs are defined for each of your mail aliases.  These
;; aliases will be defined from your .mailrc file (or the file specified by
;; the MAILRC environment variable) if it exists.  Your mail aliases will
;; expand any time you type a word-delimiter at the end of an abbreviation.

;; What you see is what you get: no abbreviations will be expanded after you
;; have sent the mail, unlike the old system.  This means you don't suffer
;; the annoyance of having the system do things behind your back -- if an
;; address you typed is going to be rewritten, you know it immediately,
;; instead of after the mail has been sent and it's too late to do anything
;; about it.  You will never again be screwed because you forgot to delete an
;; old alias from your .mailrc when a new local user arrives and is given a
;; userid which conflicts with one of your aliases, for example.

;; Your mail alias abbrevs will be in effect only when the point is in an
;; appropriate header field.  When in the body of the message, or other
;; header fields, the mail aliases will not expand.  Rather, the normal
;; mode-specific abbrev table (mail-mode-abbrev-table) will be used if 
;; defined.  So if you use mail-mode specific abbrevs, this code will not
;; adversely affect you.  You can control which header fields the abbrevs
;; are used in by changing the variable mail-abbrev-mode-regexp.

;; If auto-fill mode is on, abbrevs will wrap at commas instead of at word
;; boundaries; also, header continuation-lines will be properly indented.

;; You can also insert a mail alias with mail-interactive-insert-alias
;; (bound to C-c C-a), which prompts you for an alias (with completion)
;; and inserts its expansion at point.

;; This file fixes a bug in the old system which prohibited your .mailrc
;; file from having lines like

;;     alias someone "John Doe <doe@quux.com>"

;; That is, if you want an address to have embedded spaces, simply surround it
;; with quotes.  This is necessary because the format of the .mailrc file
;; bogusly uses spaces as address delimiters.  The following line defines an
;; alias which expands to three addresses:

;;     alias foobar addr-1 addr-2 "address three <addr-3>"

;; (This is bogus because mail-delivery programs want commas, not spaces,
;; but that's what the file format is, so we have to live with it.)

;; If you like, you can call the function define-mail-alias to define your
;; mail-aliases instead of using a .mailrc file.  When you call it in this
;; way, addresses are separated by commas.

;; CAVEAT: This works on most Sun systems; I have been told that some versions
;; of /bin/mail do not understand double-quotes in the .mailrc file.  So you
;; should make sure your version does before including verbose addresses like
;; this.  One solution to this, if you are on a system whose /bin/mail doesn't
;; work that way, (and you still want to be able to /bin/mail to send mail in
;; addition to emacs) is to define minimal aliases (without full names) in
;; your .mailrc file, and use define-mail-alias to redefine them when sending
;; mail from emacs; this way, mail sent from /bin/mail will work, and mail
;; sent from emacs will be pretty.

;; Aliases in the mailrc file may be nested.  If you define aliases like
;;     alias group1 fred ethel
;;     alias group2 larry curly moe
;;     alias everybody group1 group2
;; Then when you type "everybody" on the To: line, it will be expanded to
;;     fred, ethyl, larry, curly, moe

;; Aliases may also contain forward references; the alias of "everybody" can
;; precede the aliases of "group1" and "group2".

;; This code also understands the "source" .mailrc command, for reading
;; aliases from some other file as well.

;; Aliases may contain hyphens, as in "alias foo-bar foo@bar"; word-abbrevs
;; normally cannot contain hyphens, but this code works around that for the
;; specific case of mail-alias word-abbrevs.

;; To read in the contents of another .mailrc-type file from emacs, use the
;; command Meta-X merge-mail-aliases.  The rebuild-mail-aliases command is
;; similar, but will delete existing aliases first.

;; If you want multiple addresses separated by a string other than ", " then
;; you can set the variable mail-alias-separator-string to it.  This has to
;; be a comma bracketed by whitespace if you want any kind of reasonable
;; behaviour.

;; Some versions of /bin/mail append the contents of multiple definitions of
;; the same alias together, so that
;;     alias group one two three
;;     alias group four five
;; would define "group" as "one two three four five" instead of "four five".
;; This code does *not* support that syntax, because it's a horrible syntax
;; and isn't worth the effort or added code complexity.  (So there.)
;;
;; Thanks to Harald Hanche-Olsen, Michael Ernst, David Loeffler, Noah
;; Friedman, and Michelangelo Grigni for suggestions and bug reports.


;;; Code:

(require 'sendmail)

(defgroup mail-abbrevs nil
  "Mail abbreviation (addressbook)"
  :group 'mail)

;;;###autoload
(defcustom mail-abbrev-mailrc-file nil
  "Name of file with mail aliases.   If nil, ~/.mailrc is used."
  :type '(choice (const :tag "Default" nil)
		 file)
  :group 'mail-abbrevs)

;;;###autoload
(defun mail-abbrev-mailrc-file ()
  (or mail-abbrev-mailrc-file
      (setq mail-abbrev-mailrc-file
	    (or (getenv "MAILRC") "~/.mailrc"))))

;; originally defined in sendmail.el - used to be an alist, now is a table.
;;;###autoload
(defvar mail-aliases nil
  "Word-abbrev table of mail address aliases.
If this is nil, it means the aliases have not yet been initialized and
should be read from the .mailrc file.  (This is distinct from there being
no aliases, which is represented by this being a table with no entries.)")

;;;###autoload
(defun mail-aliases-setup ()
  (if (and (not (vectorp mail-aliases))
	   (file-exists-p (mail-abbrev-mailrc-file)))
      (build-mail-aliases))
  (make-local-variable 'pre-abbrev-expand-hook)
  (setq pre-abbrev-expand-hook
    (cond ((and (listp pre-abbrev-expand-hook)
		(not (eq 'lambda (car pre-abbrev-expand-hook))))
	   (cons 'sendmail-pre-abbrev-expand-hook pre-abbrev-expand-hook))
	  (t
	   (list 'sendmail-pre-abbrev-expand-hook pre-abbrev-expand-hook))))
  (abbrev-mode 1))

;;;###autoload
(defun build-mail-aliases (&optional file recursivep)
  "Read mail aliases from .mailrc and set mail-aliases."
  (setq file (expand-file-name (or file (mail-abbrev-mailrc-file))))
  (or (vectorp mail-aliases)
      (setq mail-aliases (make-abbrev-table)))
  (message "Parsing %s..." file)
  (let ((buffer nil)
	(obuf (current-buffer)))
    (unwind-protect
	(progn
	  (setq buffer (generate-new-buffer "mailrc"))
	  (buffer-disable-undo buffer)
	  (set-buffer buffer)
	  (cond ((get-file-buffer file)
		 (insert (save-excursion
			   (set-buffer (get-file-buffer file))
			   (buffer-substring (point-min) (point-max)))))
		((not (file-exists-p file)))
		(t (insert-file-contents file)))
	  ;; Don't lose if no final newline.
	  (goto-char (point-max))
	  (or (eq (preceding-char) ?\n) (newline))
	  (goto-char (point-min))
	  ;; Delete comments from the file
	  (while (search-forward "# " nil t)
	    (let ((p (- (point) 2)))
	      (end-of-line)
	      (delete-region p (point))))
	  (goto-char (point-min))
	  ;; handle "\\\n" continuation lines
	  (while (not (eobp))
	    (end-of-line)
	    (if (= (preceding-char) ?\\)
		(progn (delete-char -1) (delete-char 1) (insert ?\ ))
	        (forward-char 1)))
	  (goto-char (point-min))
	  (while (re-search-forward
		  "^\\(a\\(lias\\)?\\|g\\(roup\\)?\\|source\\)[ \t]+" nil t)
	    (beginning-of-line)
	    (if (looking-at "source[ \t]+\\([^ \t\n]+\\)")
		(progn
		  (end-of-line)
		  (build-mail-aliases
		   (substitute-in-file-name
		    (buffer-substring (match-beginning 1) (match-end 1)))
		   t))
	      (re-search-forward "[ \t]+\\([^ \t\n]+\\)")
	      (let* ((name (buffer-substring
			    (match-beginning 1) (match-end 1)))
		     (start (progn (skip-chars-forward " \t") (point))))
		(end-of-line)
;		(message "** %s \"%s\"" name (buffer-substring start (point)))(sit-for 1)
		(define-mail-alias
		    name
		    (buffer-substring start (point))
		    t))))
	  ;; Resolve forward references in .mailrc file.
	  ;; This would happen automatically before the first abbrev was
	  ;; expanded, but why not do it now.
	  (or recursivep (mail-resolve-all-aliases))
	  )
      (if buffer (kill-buffer buffer))
      (set-buffer obuf)))
    (message "Parsing %s... done" file)
    ;; Bob Weiner, Altrasoft, 12/10/96: Return nil so can be
    ;; used as a local-write-file-hook in ~/.mailrc.
    nil)

(defcustom mail-alias-separator-string ", "
  "*A string inserted between addresses in multi-address mail aliases.
This has to contain a comma, so \", \" is a reasonable value.  You might 
also want something like \",\\n    \" to get each address on its own line."
  :type 'string
  :group 'mail-abbrevs)

;; define-mail-alias sets this flag, which causes mail-resolve-all-aliases
;; to be called before expanding abbrevs if it's necessary.
(defvar mail-abbrev-aliases-need-to-be-resolved t)

;; originally defined in mailalias.el ; build-mail-aliases calls this with
;; stuff parsed from the .mailrc file.
;;
;;;###autoload
(defun define-mail-alias (name definition &optional from-mailrc-file)
  "Define NAME as a mail-alias that translates to DEFINITION.
If DEFINITION contains multiple addresses, separate them with commas."
  ;; When this is called from build-mail-aliases, the third argument is
  ;; true, and we do some evil space->comma hacking like /bin/mail does.
  (interactive "sDefine mail alias: \nsDefine %s as mail alias for: ")
  ;; Read the defaults first, if we have not done so.
  (if (vectorp mail-aliases)
      nil
    (setq mail-aliases (make-abbrev-table))
    (if (file-exists-p (mail-abbrev-mailrc-file))
	(build-mail-aliases)))
  ;; strip garbage from front and end
  (if (string-match "\\`[ \t\n,]+" definition)
      (setq definition (substring definition (match-end 0))))
  (if (string-match "[ \t\n,]+\\'" definition)
      (setq definition (substring definition 0 (match-beginning 0))))
  (let ((result '())
	(start 0)
	(L (length definition))
	end)
    (while start
      ;; If we're reading from the mailrc file, then addresses are delimited
      ;; by spaces, and addresses with embedded spaces must be surrounded by
      ;; single or double-quotes.  Otherwise, addresses are separated by
      ;; commas.
      (if from-mailrc-file
	  (cond ((eq ?\" (aref definition start))
		 (setq start (1+ start)
		       end (string-match "\"[ \t,]*" definition start)))
		((eq ?\' (aref definition start))
		 (setq start (1+ start)
		       end (string-match "\'[ \t,]*" definition start)))
		(t
		 (setq end (string-match "[ \t,]+" definition start))))
	(setq end (string-match "[ \t\n,]*,[ \t\n,]*" definition start)))
      (setq result (cons (substring definition start end) result))
      (setq start (and end
		       (/= (match-end 0) L)
		       (match-end 0))))
    (setq definition (mapconcat (function identity)
				(nreverse result)
				mail-alias-separator-string)))
  (setq mail-abbrev-aliases-need-to-be-resolved t)
  (setq name (downcase name))
  ;; use an abbrev table instead of an alist for mail-aliases.
  (let ((abbrevs-changed abbrevs-changed))  ; protect this from being changed.
    (define-abbrev mail-aliases name definition 'mail-abbrev-expand-hook)))


(defun mail-resolve-all-aliases ()
  "Resolve all forward references in the mail aliases table."
  (if mail-abbrev-aliases-need-to-be-resolved
      (progn
;;	(message "Resolving mail aliases...")
	(if (vectorp mail-aliases)
	    (mapatoms (function mail-resolve-all-aliases-1) mail-aliases))
	(setq mail-abbrev-aliases-need-to-be-resolved nil)
;;	(message "Resolving mail aliases... done.")
	)))

(defun mail-resolve-all-aliases-1 (sym &optional so-far)
  (if (memq sym so-far)
      (error "mail alias loop detected: %s"
	     (mapconcat 'symbol-name (cons sym so-far) " <- ")))
  (let ((definition (and (boundp sym) (symbol-value sym))))
    (if definition
	(let ((result '())
	      (start 0))
	  (while start
	    (let ((end (string-match "[ \t\n]*,[, \t\n]*" definition start)))
	      (setq result (cons (substring definition start end) result)
		    start (and end (match-end 0)))))
	  (setq definition
		(mapconcat (function (lambda (x)
			     (or (mail-resolve-all-aliases-1
				   (intern-soft (downcase x) mail-aliases)
				   (cons sym so-far))
				 x)))
			   (nreverse result)
			   mail-alias-separator-string))
	  (set sym definition))))
  (symbol-value sym))


(defun mail-abbrev-expand-hook ()
  "For use as the fourth arg to define-abbrev.
After expanding a mail-abbrev, if fill-mode is on and we're past the
fill-column, break the line at the previous comma, and indent the next
line."
  (save-excursion
    (let ((p (point))
	  bol comma fp)
      (beginning-of-line)
      (setq bol (point))
      (goto-char p)
      (while (and auto-fill-function
		  (>= (current-column) fill-column)
		  (search-backward "," bol t))
	(setq comma (point))
	(forward-char 1)		; Now we are just past the comma.
	(insert "\n")
	(delete-horizontal-space)
 	(setq p (point))
	;; Prevent abbrev expansion from happening again, since
	;; sendmail-pre-abbrev-expand-hook will already have done it.
	(let ((abbrev-mode nil))
	  (indent-relative))
	(setq fp (buffer-substring p (point)))
	;; Go to the end of the new line.
	(end-of-line)
	(if (> (current-column) fill-column)
	    ;; It's still too long; do normal auto-fill.
	    (let ((fill-prefix (or fp "\t")))
	      (do-auto-fill)))
	;; Resume the search.
	(goto-char comma)
	))))

;;; Syntax tables and abbrev-expansion

(defcustom mail-abbrev-mode-regexp
  "^\\(Resent-\\)?\\(To\\|From\\|CC\\|BCC\\|Reply-to\\):"
  "*Regexp to select mail-headers in which mail aliases should be expanded.
This string it will be handed to `looking-at' with the point at the beginning
of the current line; if it matches, abbrev mode will be turned on, otherwise
it will be turned off.  (You don't need to worry about continuation lines.)
This should be set to match those mail fields in which you want abbreviations
turned on."
  :type 'regexp
  :group 'mail-abbrevs)

(defcustom mail-rename-buffer-regexp
  "^\\(?:Resent-\\)?\\To:[ \t]*\\(.+\\)"
  "*Regexp to select mail-headers which should be used to automatically set
the name of the mail composition buffer.  Set this to an empty string to disable
auto-renaming."
  :type 'regexp
  :group 'mail-abbrevs)

(defvar mail-mode-syntax-table (copy-syntax-table text-mode-syntax-table)
  "The syntax table which is used in send-mail mode message bodies.")

(defvar mail-mode-header-syntax-table
  (let ((tab (copy-syntax-table text-mode-syntax-table)))
    ;; This makes the characters "@%!._-" be considered symbol-consituents
    ;; but not word-constituents, so forward-sexp will move you over an
    ;; entire address, but forward-word will only move you over a sequence
    ;; of alphanumerics.  (Clearly the right thing.)
    (modify-syntax-entry ?@ "_" tab)
    (modify-syntax-entry ?% "_" tab)
    (modify-syntax-entry ?! "_" tab)
    (modify-syntax-entry ?. "_" tab)
    (modify-syntax-entry ?_ "_" tab)
    (modify-syntax-entry ?- "_" tab)
    (modify-syntax-entry ?< "(>" tab)
    (modify-syntax-entry ?> ")<" tab)
    tab)
  "The syntax table used in send-mail mode when in a mail-address header.
mail-mode-syntax-table is used when the cursor is in the message body or in
non-address headers.")

(defvar mail-abbrev-syntax-table
  (let ((tab (copy-syntax-table mail-mode-header-syntax-table)))
    (if (vectorp tab)
	(let ((i (1- (length tab)))
	      (_ (aref (standard-syntax-table) ?_))
	      (w (aref (standard-syntax-table) ?w)))
	  (while (>= i 0)
	    (if (= (aref tab i) _) (aset tab i w))
	    (setq i (1- i))))
      (map-syntax-table
       #'(lambda (key val)
	   (if (eq (char-syntax-from-code val) ?_)
	       (put-char-table key (set-char-syntax-in-code val ?w) tab)
	       ))
       tab))
    tab)
  "The syntax-table used for abbrev-expansion purposes; this is not actually
made the current syntax table of the buffer, but simply controls the set of
characters which may be a part of the name of a mail-alias.")


(defun mail-abbrev-in-expansion-header-p ()
  "Whether point is in a mail-address header field."
  (let ((case-fold-search t))
    (and ;;
         ;; we are on an appropriate header line...
     (save-excursion
       (beginning-of-line)
       ;; skip backwards over continuation lines.
       (while (and (looking-at "^[ \t]")
		   (not (= (point) (point-min))))
	 (forward-line -1))
       ;; are we at the front of an appropriate header line?
       (looking-at mail-abbrev-mode-regexp))
     ;;
     ;; ...and we are before the mail-header-separator
     (< (point)
	(save-excursion
	  (goto-char (point-min))
	  (search-forward (concat "\n" mail-header-separator "\n")
			  nil 0)
	  (point))))))

(defvar mail-mode-abbrev-table) ; quiet the compiler
(defvar message-mode-map) ; quiet the compiler

(defun mail-maybe-rename-buffer ()
  (let ((case-fold-search t))
    (and ;; user has not disabled this feature...
     (not (string-equal mail-rename-buffer-regexp ""))
         ;; ... and we are before the mail-header-separator...
     (< (point)
	(save-excursion
	  (goto-char (point-min))
	  (search-forward (concat "\n" mail-header-separator "\n")
			  nil 0)
	  (point)))
     ;; ... and we are on an appropriate header line
     (save-excursion
       (beginning-of-line)
       ;; skip backwards over continuation lines.
       (while (and (looking-at "^[ \t]")
		   (not (= (point) (point-min))))
	 (forward-line -1))
       (looking-at mail-rename-buffer-regexp))
     (rename-buffer (concat "mail to " (match-string 1)) t))))

(defun sendmail-pre-abbrev-expand-hook ()
  (if mail-abbrev-aliases-need-to-be-resolved
      (mail-resolve-all-aliases))
  (if (and mail-aliases (not (eq mail-aliases t)))
      (if (not (mail-abbrev-in-expansion-header-p))
	  ;;
	  ;; If we're not in a mail header in which mail aliases should
	  ;; be expanded, then use the normal mail-mode abbrev table (if any)
	  ;; and the normal mail-mode syntax table.
	  ;;
	  (progn
	    (setq local-abbrev-table (and (boundp 'mail-mode-abbrev-table)
					  mail-mode-abbrev-table))
	    (set-syntax-table mail-mode-syntax-table))
	;;
	;; Otherwise, we are in a To: (or CC:, or whatever) header, and
	;; should use word-abbrevs to expand mail aliases.
	;;   -  First, install the mail-aliases as the word-abbrev table.
	;;   -  Then install the mail-abbrev-syntax-table, which temporarily
	;;      marks all of the non-alphanumeric-atom-characters (the "_"
	;;      syntax ones) as being normal word-syntax.  We do this because
	;;      the C code for expand-abbrev only works on words, and we want
	;;      these characters to be considered words for the purpose of
	;;      abbrev expansion.
	;;   -  Then we call expand-abbrev again, recursively, to do the abbrev
	;;      expansion with the above syntax table.
	;;   -  Then we do a trick which tells the expand-abbrev frame which
	;;      invoked us to not continue (and thus not expand twice.)
	;;      This means that any abbrev expansion will happen as a result
	;;      of this function's call to expand-abbrev, and not as a result
	;;      of the call to expand-abbrev which invoked *us*.
	;;   -  Then we set the syntax table to mail-mode-header-syntax-table,
	;;      which doesn't have anything to do with abbrev expansion, but
	;;      is just for the user's convenience (see its doc string.)
	;;
	(mail-maybe-rename-buffer)
	(setq local-abbrev-table mail-aliases)
	;; If the character just typed was non-alpha-symbol-syntax, then don't
	;; expand the abbrev now (that is, don't expand when the user types -.)
	;; Check the character's syntax in the mail-mode-header-syntax-table.
	(set-syntax-table mail-mode-header-syntax-table)
	(or (and last-command-char
		 (eq (char-syntax last-command-char) ?_))
	    (let ((pre-abbrev-expand-hook nil)) ; That's us; don't loop.
	      ;; Use this table so that abbrevs can have hyphens in them.
	      (set-syntax-table mail-abbrev-syntax-table)
	      (expand-abbrev)
	      ;; Now set it back to what it was before.
	      (set-syntax-table mail-mode-header-syntax-table)))
	(setq abbrev-start-location (point)  ; This is the trick.
	      abbrev-start-location-buffer (current-buffer))
	)))

;;; Reading addresses from the minibuffer; by David Hughes <djh@Harston.CV.COM>

(defun mail-abbrev-minibuffer-setup-hook ()
  ;; Use as the value of minibuffer-setup-hook when reading addresses
  ;; from the minibuffer, as in:
  ;;  (let ((minibuffer-setup-hook 'mail-abbrev-minibuffer-setup-hook))
  ;;    (read-string "Who: "))
  (if (and (not (vectorp mail-aliases))
	   (file-exists-p (mail-abbrev-mailrc-file)))
      (build-mail-aliases))
  (make-local-variable 'pre-abbrev-expand-hook)
  (setq pre-abbrev-expand-hook
	(function
	 (lambda ()
	   (setq local-abbrev-table mail-aliases)
	   (set-syntax-table mail-mode-header-syntax-table)
	   (or (and last-command-char
		    (eq (char-syntax last-command-char) ?_))
	       (let ((pre-abbrev-expand-hook nil)) ; That's us; don't loop.
		 ;; Use this table so that abbrevs can have hyphens in them.
		 (set-syntax-table mail-abbrev-syntax-table)
		 (expand-abbrev)
		 ;; Now set it back to what it was before.
		 (set-syntax-table mail-mode-header-syntax-table)))
	   (setq abbrev-start-location (point)  ; This is the trick.
		 abbrev-start-location-buffer (current-buffer)))))
  (abbrev-mode 1))


;;; utilities

(defun merge-mail-aliases (file)
  "Merge mail aliases from the given file with existing ones."
  (interactive (list
		(let ((insert-default-directory t)
		      (default-directory (expand-file-name "~/"))
		      (def (mail-abbrev-mailrc-file)))
		  (read-file-name
		    (format "Read additional aliases from file: (default %s) "
			    def)
		    default-directory
		    (expand-file-name def default-directory)
		    t))))
  (build-mail-aliases file))

(defun rebuild-mail-aliases (file)
  "Rebuild all the mail aliases from the given file."
  (interactive (list
		(let ((insert-default-directory t)
		      (default-directory (expand-file-name "~/"))
		      (def (mail-abbrev-mailrc-file)))
		  (read-file-name
		   (format "Read mail aliases from file: (default %s) " def)
		   default-directory
		   (expand-file-name def default-directory)
		   t))))
  (setq mail-aliases nil)
  (build-mail-aliases file))

(defun mail-interactive-insert-alias (&optional alias)
  "Prompt for and insert a mail alias."
  (interactive (progn
		(if (not (vectorp mail-aliases)) (mail-aliases-setup))
		(list (completing-read "Expand alias: " mail-aliases nil t))))
  (if (not (vectorp mail-aliases)) (mail-aliases-setup))
  (insert (or (and alias (symbol-value (intern-soft alias mail-aliases))) "")))

(defun abbrev-hacking-next-line (&optional arg)
  "Just like `next-line' (\\<global-map>\\[next-line]) but expands abbrevs \
when at end of line."
  (interactive "_p")
  (if (and (looking-at "[ \t]*\n")
	   (= (char-syntax (preceding-char)) ?w))
      (expand-abbrev))
  (next-line (or arg 1)))

(defun abbrev-hacking-end-of-buffer (&optional arg)
  "Just like `end-of-buffer' (\\<global-map>\\[end-of-buffer]) but expands \
abbrevs when at end of buffer."
  (interactive "_P")
  (if (and (looking-at "[ \t]*\n")
	   (= (char-syntax (preceding-char)) ?w))
      (expand-abbrev))
  (end-of-buffer arg))

(defun mail-abbrev-init-keys (keymap)
  "Initialize Mail Abbrevs for the given keymap."

  (define-key keymap "\C-c\C-a" 'mail-interactive-insert-alias)

  ;;(define-key mail-mode-map "\C-n" 'abbrev-hacking-next-line)
  ;;(define-key mail-mode-map "\M->" 'abbrev-hacking-end-of-buffer)
  (let ((subst '((next-line . abbrev-hacking-next-line)
		 (fkey-next-line . abbrev-hacking-next-line)
		 (end-of-buffer . abbrev-hacking-end-of-buffer)
		 (fkey-end-of-buffer . abbrev-hacking-end-of-buffer)
		 )))
    (while subst
      (let ((keys
	     (delq nil
		   (nconc (where-is-internal (car (car subst)) keymap)
			  (where-is-internal (car (car subst)))))))
	(while keys
	  (define-key keymap (car keys) (cdr (car subst)))
	  (pop keys)))
      (pop subst))))

(mail-abbrev-init-keys mail-mode-map)
(when (boundp 'message-mode-map)
  (mail-abbrev-init-keys message-mode-map))

(provide 'mail-abbrevs)

;;; mail-abbrevs.el ends here
