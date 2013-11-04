;;; checkdoc --- Check documentation strings for style requirements

;;;  Copyright (C) 1997, 1998, 1999, 2000  Free Software Foundation

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.7
;; Keywords: docs, maint, lisp

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
;;
;;   The Emacs Lisp manual has a nice chapter on how to write
;; documentation strings.  Many stylistic suggestions are fairly
;; deterministic and easy to check for syntactically, but also easy
;; to forget.  The main checkdoc engine will perform the stylistic
;; checks needed to make sure these styles are remembered.
;;
;; There are two ways to use checkdoc:
;;   1) Periodically use `checkdoc' or `checkdoc-current-buffer'.
;;      `checkdoc' is a more interactive version of
;;      `checkdoc-current-buffer'
;;   2) Use `checkdoc-minor-mode' to automatically check your
;;      documentation whenever you evaluate Lisp code with C-M-x
;;      or [menu-bar emacs-lisp eval-buffer].  Additional key-bindings
;;      are also provided under C-c ? KEY
;;        (require 'checkdoc)
;;        (add-hook 'emacs-lisp-mode-hook
;;	             '(lambda () (checkdoc-minor-mode 1)))
;;
;; Using `checkdoc':
;;
;;   The commands `checkdoc' and `checkdoc-ispell' are the top-level
;; entry points to all of the different checks that are available.  It
;; breaks examination of your Lisp file into four sections (comments,
;; documentation, messages, and spacing) and indicates its current
;; state in a status buffer.
;;
;;   The Comments check examines your headers, footers, and
;; various tags (such as "Code:") to make sure that your code is ready
;; for easy integration into existing systems.
;;
;;   The Documentation check deals with documentation strings
;; and their elements that help make Emacs easier to use.
;;
;;   The Messages check ensures that the strings displayed in the
;; minibuffer by some commands (such as `error' and `y-or-n-p')
;; are consistent with the Emacs environment.
;;
;;   The Spacing check cleans up white-space at the end of lines.
;;
;;   The interface while working with documentation and messages is
;; slightly different when being run in the interactive mode.  The
;; interface offers several options, including the ability to skip to
;; the next error, or back up to previous errors.  Auto-fixing is
;; turned off at this stage, but you can use the `f' or `F' key to fix
;; a given error (if the fix is available.)
;;
;; Auto-fixing:
;;
;;   There are four classifications of style errors in terms of how
;; easy they are to fix.  They are simple, complex, really complex,
;; and impossible.  (Impossible really means that checkdoc does not
;; have a fixing routine yet.)  Typically white-space errors are
;; classified as simple, and are auto-fixed by default.  Typographic
;; changes are considered complex, and the user is asked if they want
;; the problem fixed before checkdoc makes the change.  These changes
;; can be done without asking if `checkdoc-autofix-flag' is properly
;; set.  Potentially redundant changes are considered really complex,
;; and the user is always asked before a change is inserted.  The
;; variable `checkdoc-autofix-flag' controls how these types of errors
;; are fixed.
;;
;; Spell checking text:
;;
;;   The variable `checkdoc-spellcheck-documentation-flag' can be set
;; to customize how spell checking is to be done.  Since spell
;; checking can be quite slow, you can optimize how best you want your
;; checking done.  The default is 'defun, which spell checks each time
;; `checkdoc-defun' or `checkdoc-eval-defun' is used.  Setting to nil
;; prevents spell checking during normal usage.
;;   Setting this variable to nil does not mean you cannot take
;; advantage of the spell checking.  You can instead use the
;; interactive functions `checkdoc-ispell-*' to check the spelling of
;; your documentation.
;;   There is a list of Lisp-specific words which checkdoc will
;; install into Ispell on the fly, but only if Ispell is not already
;; running.  Use `ispell-kill-ispell' to make checkdoc restart it with
;; these words enabled.
;;
;; Checking parameters:
;;
;;   You might not always want a function to have its parameters listed
;; in order.  When this is the case, put the following comment just in
;; front of the documentation string: "; checkdoc-order: nil"  This
;; overrides the value of `checkdoc-arguments-in-order-flag'.
;;
;;   If you specifically wish to avoid mentioning a parameter of a
;; function in the doc string (such as a hidden parameter, or a
;; parameter which is very obvious like events), you can have checkdoc
;; skip looking for it by putting the following comment just in front
;; of the documentation string: "; checkdoc-params: (args go here)"
;;
;; Checking message strings:
;;
;;   The text that follows the `error' and `y-or-n-p' commands is
;; also checked.  The documentation for `error' clearly states some
;; simple style rules to follow which checkdoc will auto-fix for you.
;; `y-or-n-p' also states that it should end in a space.  I added that
;; it should end in "? " since that is almost always used.
;;
;; Adding your own checks:
;;
;;   You can experiment with adding your own checks by setting the
;; hooks `checkdoc-style-hooks' and `checkdoc-comment-style-hooks'.
;; Return a string which is the error you wish to report.  The cursor
;; position should be preserved.
;;
;; Error errors:
;;
;;   Checkdoc does not always flag errors correctly.  There are a
;; couple ways you can coax your file into passing all of checkdoc's
;; tests through buffer local variables.
;;
;;   The variable `checkdoc-verb-check-experimental-flag' can be used
;; to turn off the check for verb-voice in case you use words that are
;; not semantically verbs, but are still in the incomplete list.
;;
;;   The variable `checkdoc-symbol-words' can be a list of words that
;; happen to also be symbols.  This is not a problem for one-word
;; symbols, but if you use a hyphenated word that is also a symbol,
;; then you may need this.
;;
;;   The symbol `checkdoc-force-docstrings-flag' can be set to nil if
;; you have many undocumented functions you don't wish to document.
;;
;;   See the above section "Checking Parameters" for details about
;; parameter checking.
;;
;; Dependencies:
;;
;;   This file requires lisp-mnt (Lisp maintenance routines) for the
;;   comment checkers.
;;
;;   Requires custom for Emacs v20.

;;; TO DO:
;;   Hook into the byte compiler on a defun/defvar level to generate
;;     warnings in the byte-compiler's warning/error buffer.
;;   Better ways to override more typical `eval' functions.  Advice
;;     might be good but hard to turn on/off as a minor mode.
;;
;;; Maybe Do:
;;   Code sweep checks for "forbidden functions", proper use of hooks,
;;     proper keybindings, and other items from the manual that are
;;     not specifically docstring related.  Would this even be useful?

;;; Code:
(defvar checkdoc-version "0.7"
  "Release version of checkdoc you are currently running.")

;; From custom web page for compatibility between versions of custom:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro custom-add-option (&rest args)
      nil)
    (defmacro defcustom (var value doc &rest args)
      (` (defvar (, var) (, value) (, doc))))))

(defvar checkdoc-xemacs-p (featurep 'xemacs))

(defcustom checkdoc-autofix-flag 'semiautomatic
  "*Non-nil means attempt auto-fixing of doc strings.
If this value is the symbol `query', then the user is queried before
any change is made.  If the value is `automatic', then all changes are
made without asking unless the change is very-complex.  If the value
is `semiautomatic' or any other value, then simple fixes are made
without asking, and complex changes are made by asking the user first.
The value `never' is the same as nil, never ask or change anything."
  :group 'checkdoc
  :type '(choice (const automatic)
		 (const query)
		 (const never)
		 (const semiautomatic)
		 ;(other :tag "semiautomatic" semiautomatic)
		 ))

(defcustom checkdoc-bouncy-flag t
  "*Non-nil means to \"bounce\" to auto-fix locations.
Setting this to nil will silently make fixes that require no user
interaction.  See `checkdoc-autofix-flag' for auto-fixing details."
  :group 'checkdoc
  :type 'boolean)

(defcustom checkdoc-force-docstrings-flag t
  "*Non-nil means that all checkable definitions should have documentation.
Style guide dictates that interactive functions MUST have documentation,
and that it's good but not required practice to make non user visible items
have doc strings."
  :group 'checkdoc
  :type 'boolean)

(defcustom checkdoc-force-history-flag t
  "*Non-nil means that files should have a History section or ChangeLog file.
This helps document the evolution of, and recent changes to, the package."
  :group 'checkdoc
  :type 'boolean)

(defcustom checkdoc-permit-comma-termination-flag nil
  "*Non-nil means the first line of a docstring may end with a comma.
Ordinarily, a full sentence is required.  This may be misleading when
there is a substantial caveat to the one-line description -- the comma
should be used when the first part could stand alone as a sentence, but
it indicates that a modifying clause follows."
  :group 'checkdoc
  :type 'boolean)

(defcustom checkdoc-triple-semi-comment-check-flag t
  "*Non-nil means to check for multiple adjacent occurrences of ;;; comments.
According to the style of Emacs code in the Lisp libraries, a block
comment can look like this:
;;; Title
;;  text
;;  text
But when inside a function, code can be commented out using the ;;;
construct for all lines.  When this variable is nil, the ;;; construct
is ignored regardless of its location in the code."
  :group 'checkdoc
  :type 'boolean)

(defcustom checkdoc-spellcheck-documentation-flag nil
  "*Non-nil means run Ispell on text based on value.
This is automatically set to nil if Ispell does not exist on your
system.  Possible values are:

  nil         - Don't spell-check during basic style checks.
  defun       - Spell-check when style checking a single defun
  buffer      - Spell-check when style checking the whole buffer
  interactive - Spell-check during any interactive check.
  t           - Always spell-check"
  :group 'checkdoc
  :type '(choice (const nil)
		 (const defun)
		 (const buffer)
		 (const interactive)
		 (const t)))

(defvar checkdoc-ispell-lisp-words
  '("alist" "emacs" "etags" "iff" "keymap" "paren" "regexp" "sexp" "xemacs")
  "List of words that are correct when spell-checking Lisp documentation.")

(defcustom checkdoc-max-keyref-before-warn 10
  "*The number of \\ [command-to-keystroke] tokens allowed in a doc string.
Any more than this and a warning is generated suggesting that the construct
\\ {keymap} be used instead."
  :group 'checkdoc
  :type 'integer)

(defcustom checkdoc-arguments-in-order-flag t
  "*Non-nil means warn if arguments appear out of order.
Setting this to nil will mean only checking that all the arguments
appear in the proper form in the documentation, not that they are in
the same order as they appear in the argument list.  No mention is
made in the style guide relating to order."
  :group 'checkdoc
  :type 'boolean)

(defvar checkdoc-style-hooks nil
  "Hooks called after the standard style check is completed.
All hooks must return nil or a string representing the error found.
Useful for adding new user implemented commands.

Each hook is called with two parameters, (DEFUNINFO ENDPOINT).
DEFUNINFO is the return value of `checkdoc-defun-info'.  ENDPOINT is the
location of end of the documentation string.")

(defvar checkdoc-comment-style-hooks nil
  "Hooks called after the standard comment style check is completed.
Must return nil if no errors are found, or a string describing the
problem discovered.  This is useful for adding additional checks.")

(defvar checkdoc-diagnostic-buffer "*Style Warnings*"
  "Name of warning message buffer.")

(defcustom checkdoc-defun-regexp
  "^(def\\(un\\|var\\|custom\\|macro\\|const\\|subst\\|advice\\|method\\)\
\\s-+\\(\\(\\sw\\|\\s_\\)+\\)[ \t\n]+"
  "*Regular expression used to identify a defun.
A search leaves the cursor in front of the parameter list."
  :group 'checkdoc
  :type 'string)

(defcustom checkdoc-verb-check-experimental-flag t
  "*Non-nil means to attempt to check the voice of the doc string.
This check keys off some words which are commonly misused.  See the
variable `checkdoc-common-verbs-wrong-voice' if you wish to add your own."
  :group 'checkdoc
  :type 'boolean)

(defvar checkdoc-generate-compile-warnings-flag nil
  "Non-nil means generate warnings in a buffer for browsing.
Do not set this by hand, use a function like `checkdoc-current-buffer'
with a universal argument.")

(defcustom checkdoc-symbol-words nil
  "A list of symbols which also happen to make good words.
These symbol-words are ignored when unquoted symbols are searched for.
This should be set in an Emacs Lisp file's local variables."
  :group 'checkdoc
  :type '(repeat (symbol :tag "Word")))

(defvar checkdoc-proper-noun-list
  '("ispell" "xemacs" "emacs" "lisp")
  "List of words (not capitalized) which should be capitalized.")

(defvar checkdoc-proper-noun-regexp
  (let ((expr "\\<\\(")
	(l checkdoc-proper-noun-list))
    (while l
      (setq expr (concat expr (car l) (if (cdr l) "\\|" ""))
	    l (cdr l)))
    (concat expr "\\)\\>"))
  "Regular expression derived from `checkdoc-proper-noun-regexp'.")

(defvar checkdoc-common-verbs-regexp nil
  "Regular expression derived from `checkdoc-common-verbs-regexp'.")

(defvar checkdoc-common-verbs-wrong-voice
  '(("adds" . "add")
    ("allows" . "allow")
    ("appends" . "append")
    ("applies" . "apply")
    ("arranges" . "arrange")
    ("brings" . "bring")
    ("calls" . "call")
    ("catches" . "catch")
    ("changes" . "change")
    ("checks" . "check")
    ("contains" . "contain")
    ("creates" . "create")
    ("destroys" . "destroy")
    ("disables" . "disable")
    ("executes" . "execute")
    ("evals" . "evaluate")
    ("evaluates" . "evaluate")
    ("finds" . "find")
    ("forces" . "force")
    ("gathers" . "gather")
    ("generates" . "generate")
    ("goes" . "go")
    ("guesses" . "guess")
    ("highlights" . "highlight")
    ("holds" . "hold")
    ("ignores" . "ignore")
    ("indents" . "indent")
    ("initializes" . "initialize")
    ("inserts" . "insert")
    ("installs" . "install")
    ("investigates" . "investigate")
    ("keeps" . "keep")
    ("kills" . "kill")
    ("leaves" . "leave")
    ("lets" . "let")
    ("loads" . "load")
    ("looks" . "look")
    ("makes" . "make")
    ("marks" . "mark")
    ("matches" . "match")
    ("notifies" . "notify")
    ("offers" . "offer")
    ("parses" . "parse")
    ("performs" . "perform")
    ("prepares" . "prepare")
    ("prepends" . "prepend")
    ("reads" . "read")
    ("raises" . "raise")
    ("removes" . "remove")
    ("replaces" . "replace")
    ("resets" . "reset")
    ("restores" . "restore")
    ("returns" . "return")
    ("runs" . "run")
    ("saves" . "save")
    ("says" . "say")
    ("searches" . "search")
    ("selects" . "select")
    ("sets" . "set")
    ("sex" . "s*x")
    ("shows" . "show")
    ("signifies" . "signify")
    ("sorts" . "sort")
    ("starts" . "start")
    ("stores" . "store")
    ("switches" . "switch")
    ("tells" . "tell")
    ("tests" . "test")
    ("toggles" . "toggle")
    ("tries" . "try")
    ("turns" . "turn")
    ("undoes" . "undo")
    ("unloads" . "unload")
    ("unmarks" . "unmark")
    ("updates" . "update")
    ("uses" . "use")
    ("yanks" . "yank")
    )
  "Alist of common words in the wrong voice and what should be used instead.
Set `checkdoc-verb-check-experimental-flag' to nil to avoid this costly
and experimental check.  Do not modify this list without setting
the value of `checkdoc-common-verbs-regexp' to nil which cause it to
be re-created.")

(defvar checkdoc-syntax-table nil
  "Syntax table used by checkdoc in document strings.")

(if checkdoc-syntax-table
    nil
  (setq checkdoc-syntax-table (copy-syntax-table emacs-lisp-mode-syntax-table))
  ;; When dealing with syntax in doc strings, make sure that - are encompassed
  ;; in words so we can use cheap \\> to get the end of a symbol, not the
  ;; end of a word in a conglomerate.
  (modify-syntax-entry ?- "w" checkdoc-syntax-table)
  )
	

;;; Compatibility
;;
(if checkdoc-xemacs-p
    (progn
      (defalias 'checkdoc-make-overlay 'make-extent)
      (defalias 'checkdoc-overlay-put 'set-extent-property)
      (defalias 'checkdoc-delete-overlay 'delete-extent)
      (defalias 'checkdoc-mode-line-update 'redraw-modeline)
      (defalias 'checkdoc-call-eval-buffer 'eval-buffer)
      )
  (defalias 'checkdoc-make-overlay 'make-overlay)
  (defalias 'checkdoc-overlay-put 'overlay-put)
  (defalias 'checkdoc-delete-overlay 'delete-overlay)
  (defalias 'checkdoc-mode-line-update 'force-mode-line-update)
  (defalias 'checkdoc-call-eval-buffer 'eval-current-buffer)
  )

;; Emacs 20 has this handy function.
(if (not (fboundp 'princ-list))
    (defun princ-list (&rest args)
      "Call `princ' on ARGS."
      (mapcar 'princ args)))

;; Emacs 20s have MULE characters which don't equate to numbers.
(if (fboundp 'char=)
    (defalias 'checkdoc-char= 'char=)
  (defalias 'checkdoc-char= '=))

;; Emacs 19.28 and earlier don't have the handy 'add-to-list function
(if (fboundp 'add-to-list)

    (defalias 'checkdoc-add-to-list 'add-to-list)

  (defun checkdoc-add-to-list (list-var element)
    "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet."
    (if (not (member element (symbol-value list-var)))
	(set list-var (cons element (symbol-value list-var)))))
  )

;; To be safe in new Emacsen, we want to read events, not characters
(if (fboundp 'read-event)
    (defalias 'checkdoc-read-event 'read-event)
  (defalias 'checkdoc-read-event 'read-char))

;;; User level commands
;;
;;;###autoload
(defun checkdoc ()
  "Interactively check the entire buffer for style errors.
The current status of the check will be displayed in a buffer for
user to view as each check is completed."
  (interactive)
  (let ((status (list "Checking..." "-" "-" "-"))
	(checkdoc-spellcheck-documentation-flag
	 (member checkdoc-spellcheck-documentation-flag
		 '(buffer interactive t)))
	;; if the user set autofix to never, then that breaks the
	;; obviously requested asking implied by using this function.
	;; Set it to paranoia level.
	(checkdoc-autofix-flag (if (or (not checkdoc-autofix-flag)
				       (eq checkdoc-autofix-flag 'never))
				   'query
				 checkdoc-autofix-flag))
	tmp)
    (checkdoc-display-status-buffer status)
    ;; check the comments
    (if (not buffer-file-name)
	(setcar status "Not checked")
      (if (checkdoc-file-comments-engine)
	  (setcar status "Errors")
	(setcar status "Ok")))
    (setcar (cdr status) "Checking...")
    (checkdoc-display-status-buffer status)
    ;; Check the documentation
    (setq tmp (checkdoc-interactive nil t))
    (if tmp
	(setcar (cdr status) (format "%d Errors" (length tmp)))
      (setcar (cdr status) "Ok"))
    (setcar (cdr (cdr status)) "Checking...")
    (checkdoc-display-status-buffer status)
    ;; Check the message text
    (if (setq tmp (checkdoc-message-interactive nil t))
	(setcar (cdr (cdr status)) (format "%d Errors" (length tmp)))
      (setcar (cdr (cdr status)) "Ok"))
    (setcar (cdr (cdr (cdr status))) "Checking...")
    (checkdoc-display-status-buffer status)
    ;; Rogue spacing
    (if (condition-case nil
	    (checkdoc-rogue-spaces nil t)
	  (error t))
	(setcar (cdr (cdr (cdr status))) "Errors")
      (setcar (cdr (cdr (cdr status))) "Ok"))
    (checkdoc-display-status-buffer status)))

(defun checkdoc-display-status-buffer (check)
  "Display and update the status buffer for the current checkdoc mode.
CHECK is a vector stating the current status of each test as an
element is the status of that level of test."
  (let (temp-buffer-setup-hook)
    (with-output-to-temp-buffer " *Checkdoc Status*"
      (with-current-buffer " *Checkdoc Status*"
	(fundamental-mode))
      (princ-list
       "Buffer comments and tags:  " (nth 0 check) "\n"
       "Documentation style:       " (nth 1 check) "\n"
       "Message/Query text style:  " (nth 2 check) "\n"
       "Unwanted Spaces:           " (nth 3 check)
       )))
  (shrink-window-if-larger-than-buffer
   (get-buffer-window " *Checkdoc Status*"))
  (message nil)
  (sit-for 0))

;;;###autoload
(defun checkdoc-interactive (&optional start-here showstatus)
  "Interactively check the current buffer for doc string errors.
Prefix argument START-HERE will start the checking from the current
point, otherwise the check starts at the beginning of the current
buffer.  Allows navigation forward and backwards through document
errors.  Does not check for comment or space warnings.
Optional argument SHOWSTATUS indicates that we should update the
checkdoc status window instead of the usual behavior."
  (interactive "P")
  (let ((checkdoc-spellcheck-documentation-flag
	 (member checkdoc-spellcheck-documentation-flag
		 '(interactive t))))
    (checkdoc-interactive-loop start-here showstatus 'checkdoc-next-error)))

;;;###autoload
(defun checkdoc-message-interactive (&optional start-here showstatus)
  "Interactively check the current buffer for message string errors.
Prefix argument START-HERE will start the checking from the current
point, otherwise the check starts at the beginning of the current
buffer.  Allows navigation forward and backwards through document
errors.  Does not check for comment or space warnings.
Optional argument SHOWSTATUS indicates that we should update the
checkdoc status window instead of the usual behavior."
  (interactive "P")
  (let ((checkdoc-spellcheck-documentation-flag
	 (member checkdoc-spellcheck-documentation-flag
		 '(interactive t))))
    (checkdoc-interactive-loop start-here showstatus
			       'checkdoc-next-message-error)))

(defun checkdoc-interactive-loop (start-here showstatus findfunc)
  "Interactively loop over all errors that can be found by a given method.
Searching starts at START-HERE.  SHOWSTATUS expresses the verbosity
of the search, and whether ending the search will auto-exit this function.
FINDFUNC is a symbol representing a function that will position the
cursor, and return error message text to present to the user.  It is
assumed that the cursor will stop just before a major sexp, which will
be highlighted to present the user with feedback as to the offending
style."
  ;; Determine where to start the test
  (let* ((begin (prog1 (point)
		  (if (not start-here) (goto-char (point-min)))))
	 ;; Assign a flag to spellcheck flag
	 (checkdoc-spellcheck-documentation-flag
	  (member checkdoc-spellcheck-documentation-flag
		  '(buffer interactive t)))
	 ;; Fetch the error list
	 (err-list (list (funcall findfunc nil)))
	 (cdo nil)
	 (returnme nil)
	 c)
    (save-window-excursion
      (if (not (car err-list)) (setq err-list nil))
      ;; Include whatever function point is in for good measure.
      (beginning-of-defun)
      (while err-list
	(goto-char (cdr (car err-list)))
	;; The cursor should be just in front of the offending doc string
	(if (stringp (car (car err-list)))
	    (setq cdo (save-excursion (checkdoc-make-overlay
				       (point) (progn (forward-sexp 1)
						      (point)))))
	  (setq cdo (checkdoc-make-overlay
		     (checkdoc-error-start (car (car err-list)))
		     (checkdoc-error-end (car (car err-list))))))
	(unwind-protect
	    (progn
	      (checkdoc-overlay-put cdo 'face 'highlight)
	      ;; Make sure the whole doc string is visible if possible.
	      (sit-for 0)
	      (if (and (looking-at "\"")
		       (not (pos-visible-in-window-p
			     (save-excursion (forward-sexp 1) (point))
			     (selected-window))))
		  (let ((l (count-lines (point)
					(save-excursion
					  (forward-sexp 1) (point)))))
		    (if (> l (window-height))
			(recenter 1)
		      (recenter (/ (- (window-height) l) 2))))
		(recenter))
	      (message "%s (C-h,%se,n,p,q)" (checkdoc-error-text
					      (car (car err-list)))
		       (if (checkdoc-error-unfixable (car (car err-list)))
			   "" "f,"))
	      (save-excursion
		(goto-char (checkdoc-error-start (car (car err-list))))
		(if (not (pos-visible-in-window-p))
		    (recenter (- (window-height) 2)))
		(setq c (checkdoc-read-event)))
	      (if (and (not checkdoc-xemacs-p)
		       (not (integerp c)))
		  (setq c ??))
	      (cond
	       ;; Exit condition
	       ((checkdoc-char= c ?\C-g) (signal 'quit nil))
	       ;; Request an auto-fix
	       ((or (checkdoc-char= c ?y) (checkdoc-char= c ?f))
		(checkdoc-delete-overlay cdo)
		(setq cdo nil)
		(goto-char (cdr (car err-list)))
		;; `automatic-then-never' tells the autofix function
		;; to only allow one fix to be automatic.  The autofix
		;; function will than set the flag to 'never, allowing
		;; the checker to return a different error.
		(let ((checkdoc-autofix-flag 'automatic-then-never)
		      (fixed nil))
		  (funcall findfunc t)
		  (setq fixed (not (eq checkdoc-autofix-flag
				       'automatic-then-never)))
		  (if (not fixed)
		      (progn
			(message "A Fix was not available.")
			(sit-for 2))
		    (setq err-list (cdr err-list))))
		(beginning-of-defun)
		(let ((pe (car err-list))
		      (ne (funcall findfunc nil)))
		  (if ne
		      (setq err-list (cons ne err-list))
		    (cond ((not err-list)
			   (message "No More Stylistic Errors.")
			   (sit-for 2))
			  (t
			   (message
			    "No Additional style errors.  Continuing...")
			   (sit-for 2))))))
	       ;; Move to the next error (if available)
	       ((or (checkdoc-char= c ?n) (checkdoc-char= c ?\ ))
		(let ((ne (funcall findfunc nil)))
		  (if (not ne)
		      (if showstatus
			  (setq returnme err-list
				err-list nil)
			(if (not err-list)
			    (message "No More Stylistic Errors.")
			  (message "No Additional style errors.  Continuing..."))
			(sit-for 2))
		    (setq err-list (cons ne err-list)))))
	       ;; Go backwards in the list of errors
	       ((or (checkdoc-char= c ?p) (checkdoc-char= c ?\C-?))
		(if (/= (length err-list) 1)
		    (progn
		      (setq err-list (cdr err-list))
		      (goto-char (cdr (car err-list)))
		      (beginning-of-defun))
		  (message "No Previous Errors.")
		  (sit-for 2)))
	       ;; Edit the buffer recursively.
	       ((checkdoc-char= c ?e)
		(checkdoc-recursive-edit
		 (checkdoc-error-text (car (car err-list))))
		(checkdoc-delete-overlay cdo)
		(setq err-list (cdr err-list)) ;back up the error found.
		(beginning-of-defun)
		(let ((ne (funcall findfunc nil)))
		  (if (not ne)
		      (if showstatus
			  (setq returnme err-list
				err-list nil)
			(message "No More Stylistic Errors.")
			(sit-for 2))
		    (setq err-list (cons ne err-list)))))
	       ;; Quit checkdoc
	       ((checkdoc-char= c ?q)
		(setq returnme err-list
		      err-list nil
		      begin (point)))
	       ;; Goofy s tuff
	       (t
		(if (get-buffer-window "*Checkdoc Help*")
		    (progn
		      (delete-window (get-buffer-window "*Checkdoc Help*"))
		      (kill-buffer "*Checkdoc Help*"))
		  (with-output-to-temp-buffer "*Checkdoc Help*"
		    (princ-list
		     "Checkdoc Keyboard Summary:\n"
		     (if (checkdoc-error-unfixable (car (car err-list)))
			 ""
		       (concat
			"f, y    - auto Fix this warning without asking (if\
 available.)\n"
			"         Very complex operations will still query.\n")
		       )
		     "e      - Enter recursive Edit.  Press C-M-c to exit.\n"
		     "SPC, n - skip to the Next error.\n"
		     "DEL, p - skip to the Previous error.\n"
		     "q      - Quit checkdoc.\n"
		     "C-h    - Toggle this help buffer."))
		  (shrink-window-if-larger-than-buffer
		   (get-buffer-window "*Checkdoc Help*"))))))
	  (if cdo (checkdoc-delete-overlay cdo)))))
    (goto-char begin)
    (if (get-buffer "*Checkdoc Help*") (kill-buffer "*Checkdoc Help*"))
    (message "Checkdoc: Done.")
    returnme))

(defun checkdoc-next-error (enable-fix)
  "Find and return the next checkdoc error list, or nil.
Only documentation strings are checked.
Add error vector is of the form (WARNING . POSITION) where WARNING
is the warning text, and POSITION is the point in the buffer where the
error was found.  We can use points and not markers because we promise
not to edit the buffer before point without re-executing this check.
Argument ENABLE-FIX will enable auto-fixing while looking for the next
error.  This argument assumes that the cursor is already positioned to
perform the fix."
  (if enable-fix
      (checkdoc-this-string-valid)
    (let ((msg nil) (p (point))
	  (checkdoc-autofix-flag nil))
      (condition-case nil
	  (while (and (not msg) (checkdoc-next-docstring))
	    (message "Searching for doc string error...%d%%"
		     (/ (* 100 (point)) (point-max)))
	    (if (setq msg (checkdoc-this-string-valid))
		(setq msg (cons msg (point)))))
	;; Quit.. restore position,  Other errors, leave alone
	(quit (goto-char p)))
      msg)))

(defun checkdoc-next-message-error (enable-fix)
  "Find and return the next checkdoc message related error list, or nil.
Only text for error and `y-or-n-p' strings are checked.  See
`checkdoc-next-error' for details on the return value.
Argument ENABLE-FIX turns on the auto-fix feature.  This argument
assumes that the cursor is already positioned to perform the fix."
  (if enable-fix
      (checkdoc-message-text-engine)
    (let ((msg nil) (p (point)) (type nil)
	  (checkdoc-autofix-flag nil))
      (condition-case nil
	  (while (and (not msg)
		      (setq type
			    (checkdoc-message-text-next-string (point-max))))
	    (message "Searching for message string error...%d%%"
		     (/ (* 100 (point)) (point-max)))
	    (if (setq msg (checkdoc-message-text-engine type))
		(setq msg (cons msg (point)))))
	;; Quit.. restore position,  Other errors, leave alone
	(quit (goto-char p)))
      msg)))

(defun checkdoc-recursive-edit (msg)
  "Enter recursive edit to permit a user to fix some error checkdoc has found.
MSG is the error that was found, which is displayed in a help buffer."
  (with-output-to-temp-buffer "*Checkdoc Help*"
    (princ-list
     "Error message:\n  " msg
     "\n\nEdit to fix this problem, and press C-M-c to continue."))
  (shrink-window-if-larger-than-buffer
   (get-buffer-window "*Checkdoc Help*"))
  (message "When you're done editing press C-M-c to continue.")
  (unwind-protect
      (recursive-edit)
    (if (get-buffer-window "*Checkdoc Help*")
	(progn
	  (delete-window (get-buffer-window "*Checkdoc Help*"))
	  (kill-buffer "*Checkdoc Help*")))))

;;;###autoload
(defun checkdoc-eval-current-buffer ()
  "Evaluate and check documentation for the current buffer.
Evaluation is done first because good documentation for something that
doesn't work is just not useful.  Comments, doc strings, and rogue
spacing are all verified."
  (interactive)
  (checkdoc-call-eval-buffer nil)
  (checkdoc-current-buffer t))

;;;###autoload
(defun checkdoc-current-buffer (&optional take-notes)
  "Check current buffer for document, comment, error style, and rogue spaces.
With a prefix argument (in Lisp, the argument TAKE-NOTES),
store all errors found in a warnings buffer,
otherwise stop after the first error."
  (interactive "P")
  (if (interactive-p) (message "Checking buffer for style..."))
  ;; Assign a flag to spellcheck flag
  (let ((checkdoc-spellcheck-documentation-flag
	 (memq checkdoc-spellcheck-documentation-flag '(buffer t)))
	(checkdoc-autofix-flag (if take-notes 'never
				 checkdoc-autofix-flag))
	(checkdoc-generate-compile-warnings-flag
	 (or take-notes checkdoc-generate-compile-warnings-flag)))
    (if take-notes
	(checkdoc-start-section "checkdoc-current-buffer"))
    ;; every test is responsible for returning the cursor.
    (or (and buffer-file-name ;; only check comments in a file
	     (checkdoc-comments))
	(checkdoc-start)
	(checkdoc-message-text)
	(checkdoc-rogue-spaces)
	(not (interactive-p))
	(if take-notes (checkdoc-show-diagnostics))
	(message "Checking buffer for style...Done."))))

;;;###autoload
(defun checkdoc-start (&optional take-notes)
  "Start scanning the current buffer for documentation string style errors.
Only documentation strings are checked.
Use `checkdoc-continue' to continue checking if an error cannot be fixed.
Prefix argument TAKE-NOTES means to collect all the warning messages into
a separate buffer."
  (interactive "P")
  (let ((p (point)))
    (goto-char (point-min))
    (if (and take-notes (interactive-p))
	(checkdoc-start-section "checkdoc-start"))
    (if (interactive-p)
	(call-interactively 'checkdoc-continue take-notes)
      (checkdoc-continue take-notes))
    ;; Go back since we can't be here without success above.
    (goto-char p)
    nil))

;;;###autoload
(defun checkdoc-continue (&optional take-notes)
  "Find the next doc string in the current buffer which has a style error.
Prefix argument TAKE-NOTES means to continue through the whole buffer and
save warnings in a separate buffer."
  (interactive "P")
  (let ((wrong nil) (msg nil) (errors nil)
	;; Assign a flag to spellcheck flag
	(checkdoc-spellcheck-documentation-flag
	 (member checkdoc-spellcheck-documentation-flag
		 '(buffer t)))
	(checkdoc-autofix-flag (if take-notes 'never
				 checkdoc-autofix-flag))
	(checkdoc-generate-compile-warnings-flag
	 (or take-notes checkdoc-generate-compile-warnings-flag)))
    (save-excursion
      ;; If we are taking notes, encompass the whole buffer, otherwise
      ;; the user is navigating down through the buffer.
      (while (and (not wrong) (checkdoc-next-docstring))
	;; OK, let's look at the doc string.
	(setq msg (checkdoc-this-string-valid))
	(if msg (setq wrong (point)))))
    (if wrong
	(progn
	  (goto-char wrong)
	  (if (not take-notes)
	      (error (checkdoc-error-text msg)))))
    (checkdoc-show-diagnostics)
    (if (interactive-p)
	(message "No style warnings."))))

(defun checkdoc-next-docstring ()
  "Move to the next doc string after point, and return t.
Return nil if there are no more doc strings."
  (if (not (re-search-forward checkdoc-defun-regexp nil t))
      nil
    ;; If we are in a method, a :starting symbol may exist.
    (if (looking-at "\\s-*:\\w+") (forward-sexp 1))
    ;; search drops us after the identifier.  The next sexp is either
    ;; the argument list or the value of the variable.  skip it.
    (forward-sexp 1)
    (skip-chars-forward " \n\t")
    t))

;;; ###autoload
(defun checkdoc-directory (dir &optional take-notes)
  "Run checkdoc on all Emacs Lisp files in DIR.
Prefix argument TAKE-NOTES means to continue through all files and
save warnings in a buffer."
  (interactive "DDirectory: \nP")
  (let* ((files (directory-files dir t "\\.el$"))
	 (n (length files))
	 kill kill-me)
    (if (and take-notes (interactive-p))
	(checkdoc-start-section (format "checkdoc-directory: %s" dir)))
    (while files
      (setq kill (get-file-buffer (car files)))
      (set-buffer (setq kill-me (find-file-noselect (car files))))
      (if (interactive-p)
	  (message "File %d of %d: %s" (- n (length files) -1) n
		   (file-name-nondirectory (car files))))
      (checkdoc-start take-notes)
      (if (not kill) (kill-buffer kill-me))
      (sit-for 0)
      (setq files (cdr files)))
    (checkdoc-show-diagnostics)))

;;; ###autoload
(defun checkdoc-comments (&optional take-notes)
  "Find missing comment sections in the current Emacs Lisp file.
Prefix argument TAKE-NOTES non-nil means to save warnings in a
separate buffer.  Otherwise print a message.  This returns the error
if there is one."
  (interactive "P")
  (if take-notes (checkdoc-start-section "checkdoc-comments"))
  (if (not buffer-file-name)
     (error "Can only check comments for a file buffer"))
  (let* ((checkdoc-spellcheck-documentation-flag
	  (member checkdoc-spellcheck-documentation-flag
		  '(buffer t)))
	 (checkdoc-autofix-flag (if take-notes 'never checkdoc-autofix-flag))
	 ;; This is just irritating when taking notes.
	 (checkdoc-triple-semi-comment-check-flag
	  (if take-notes nil checkdoc-triple-semi-comment-check-flag))
	 (e (checkdoc-file-comments-engine))
	(checkdoc-generate-compile-warnings-flag
	 (or take-notes checkdoc-generate-compile-warnings-flag)))
    (if e (error (checkdoc-error-text e)))
    (checkdoc-show-diagnostics)
    e))

;;;###autoload
(defun checkdoc-rogue-spaces (&optional take-notes interact)
  "Find extra spaces at the end of lines in the current file.
Prefix argument TAKE-NOTES non-nil means to save warnings in a
separate buffer.  Otherwise print a message.  This returns the error
if there is one.
Optional argument INTERACT permits more interactive fixing."
  (interactive "P")
  (if take-notes (checkdoc-start-section "checkdoc-rogue-spaces"))
  (let* ((checkdoc-autofix-flag (if take-notes 'never checkdoc-autofix-flag))
	 (e (checkdoc-rogue-space-check-engine nil nil interact))
	(checkdoc-generate-compile-warnings-flag
	 (or take-notes checkdoc-generate-compile-warnings-flag)))
    (if (not (interactive-p))
	e
      (if e
	  (message (checkdoc-error-text e))
	(checkdoc-show-diagnostics)
	(message "Space Check: done.")))))

;;;###autoload
(defun checkdoc-message-text (&optional take-notes)
  "Scan the buffer for occurrences of the error function, and verify text.
Optional argument TAKE-NOTES causes all errors to be logged."
  (interactive "P")
  (if take-notes (checkdoc-start-section "checkdoc-message-text"))
  (let* ((p (point)) e
	 (checkdoc-autofix-flag (if take-notes 'never checkdoc-autofix-flag))
	 (checkdoc-generate-compile-warnings-flag
	  (or take-notes checkdoc-generate-compile-warnings-flag)))
    (setq e (checkdoc-message-text-search))
    (if (not (interactive-p))
	e
      (if e
	  (error (checkdoc-error-text e))
	(checkdoc-show-diagnostics)))
    (goto-char p))
  (if (interactive-p) (message "Checking interactive message text...done.")))
    
;;;###autoload
(defun checkdoc-eval-defun ()
  "Evaluate the current form with `eval-defun' and check its documentation.
Evaluation is done first so the form will be read before the
documentation is checked.  If there is a documentation error, then the display
of what was evaluated will be overwritten by the diagnostic message."
  (interactive)
  (eval-defun nil)
  (checkdoc-defun))

;;;###autoload
(defun checkdoc-defun (&optional no-error)
  "Examine the doc string of the function or variable under point.
Call `error' if the doc string has problems.  If NO-ERROR is
non-nil, then do not call error, but call `message' instead.
If the doc string passes the test, then check the function for rogue white
space at the end of each line."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (if (not (looking-at checkdoc-defun-regexp))
	;; I found this more annoying than useful.
	;;(if (not no-error)
	;;    (message "Cannot check this sexp's doc string."))
	nil
      ;; search drops us after the identifier.  The next sexp is either
      ;; the argument list or the value of the variable.  skip it.
      (goto-char (match-end 0))
      ;; If we are in a method, a :starting symbol may exist.
      (if (looking-at "\\s-*:\\w+") (forward-sexp 1))
      (forward-sexp 1)
      (skip-chars-forward " \n\t")
      (let* ((checkdoc-spellcheck-documentation-flag
	      (member checkdoc-spellcheck-documentation-flag
		      '(defun t)))
	     (beg (save-excursion (beginning-of-defun) (point)))
	     (end (save-excursion (end-of-defun) (point)))
	     (msg (checkdoc-this-string-valid)))
	(if msg (if no-error
		    (message (checkdoc-error-text msg))
		  (error (checkdoc-error-text msg)))
	  (setq msg (checkdoc-message-text-search beg end))
	  (if msg (if no-error
		      (message (checkdoc-error-text msg))
		    (error (checkdoc-error-text msg)))
	    (setq msg (checkdoc-rogue-space-check-engine beg end))
	    (if msg (if no-error
			(message (checkdoc-error-text msg))
		      (error (checkdoc-error-text msg))))))
	(if (interactive-p) (message "Checkdoc: done."))))))

;;; Ispell interface for forcing a spell check
;;

;;;###autoload
(defun checkdoc-ispell (&optional take-notes)
  "Check the style and spelling of everything interactively.
Calls `checkdoc' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc'"
  (interactive)
  (let ((checkdoc-spellcheck-documentation-flag t))
    (call-interactively 'checkdoc nil current-prefix-arg)))

;;;###autoload
(defun checkdoc-ispell-current-buffer (&optional take-notes)
  "Check the style and spelling of the current buffer.
Calls `checkdoc-current-buffer' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-current-buffer'"
  (interactive)
  (let ((checkdoc-spellcheck-documentation-flag t))
    (call-interactively 'checkdoc-current-buffer nil current-prefix-arg)))

;;;###autoload
(defun checkdoc-ispell-interactive (&optional take-notes)
  "Check the style and spelling of the current buffer interactively.
Calls `checkdoc-interactive' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-interactive'"
  (interactive)
  (let ((checkdoc-spellcheck-documentation-flag t))
    (call-interactively 'checkdoc-interactive nil current-prefix-arg)))

;;;###autoload
(defun checkdoc-ispell-message-interactive (&optional take-notes)
  "Check the style and spelling of message text interactively.
Calls `checkdoc-message-interactive' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-message-interactive'"
  (interactive)
  (let ((checkdoc-spellcheck-documentation-flag t))
    (call-interactively 'checkdoc-message-interactive nil current-prefix-arg)))

;;;###autoload
(defun checkdoc-ispell-message-text (&optional take-notes)
  "Check the style and spelling of message text interactively.
Calls `checkdoc-message-text' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-message-text'"
  (interactive)
  (let ((checkdoc-spellcheck-documentation-flag t))
    (call-interactively 'checkdoc-message-text nil current-prefix-arg)))

;;;###autoload
(defun checkdoc-ispell-start (&optional take-notes)
  "Check the style and spelling of the current buffer.
Calls `checkdoc-start' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-start'"
  (interactive)
  (let ((checkdoc-spellcheck-documentation-flag t))
    (call-interactively 'checkdoc-start nil current-prefix-arg)))

;;;###autoload
(defun checkdoc-ispell-continue (&optional take-notes)
  "Check the style and spelling of the current buffer after point.
Calls `checkdoc-continue' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-continue'"
  (interactive)
  (let ((checkdoc-spellcheck-documentation-flag t))
    (call-interactively 'checkdoc-continue nil current-prefix-arg)))

;;;###autoload
(defun checkdoc-ispell-comments (&optional take-notes)
  "Check the style and spelling of the current buffer's comments.
Calls `checkdoc-comments' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-comments'"
  (interactive)
  (let ((checkdoc-spellcheck-documentation-flag t))
    (call-interactively 'checkdoc-comments nil current-prefix-arg)))

;;;###autoload
(defun checkdoc-ispell-defun (&optional take-notes)
  "Check the style and spelling of the current defun with Ispell.
Calls `checkdoc-defun' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-defun'"
  (interactive)
  (let ((checkdoc-spellcheck-documentation-flag t))
    (call-interactively 'checkdoc-defun nil current-prefix-arg)))

;;; Error Management
;;
;; Errors returned from checkdoc functions can have various
;; features and behaviors, so we need some ways of specifying
;; them, and making them easier to use in the wacked-out interfaces
;; people are requesting
(defun checkdoc-create-error (text start end &optional unfixable)
  "Used to create the return error text returned from all engines.
TEXT is the descriptive text of the error.  START and END define the region
it is sensible to highlight when describing the problem.
Optional argument UNFIXABLE means that the error has no auto-fix available.

A list of the form (TEXT START END UNFIXABLE) is returned if we are not
generating a buffered list of errors."
  (if checkdoc-generate-compile-warnings-flag
      (progn (checkdoc-error start text)
	     nil)
    (list text start end unfixable)))

(defun checkdoc-error-text (err)
  "Return the text specified in the checkdoc ERR."
  ;; string-p part is for backwards compatibility
  (if (stringp err) err (car err)))

(defun checkdoc-error-start (err)
  "Return the start point specified in the checkdoc ERR."
  ;; string-p part is for backwards compatibility
  (if (stringp err) nil (nth 1 err)))

(defun checkdoc-error-end (err)
  "Return the end point specified in the checkdoc ERR."
  ;; string-p part is for backwards compatibility
  (if (stringp err) nil (nth 2 err)))

(defun checkdoc-error-unfixable (err)
  "Return the t if we cannot autofix the error specified in the checkdoc ERR."
  ;; string-p part is for backwards compatibility
  (if (stringp err) nil (nth 3 err)))

;;; Minor Mode specification
;;
(defvar checkdoc-minor-mode nil
  "Non-nil in `emacs-lisp-mode' for automatic documentation checking.")
(make-variable-buffer-local 'checkdoc-minor-mode)

(checkdoc-add-to-list 'minor-mode-alist '(checkdoc-minor-mode " CDoc"))

(defvar checkdoc-minor-keymap
  (let ((map (make-sparse-keymap))
	(pmap (make-sparse-keymap)))
    ;; Override some bindings
    (define-key map "\C-\M-x" 'checkdoc-eval-defun)
    ;(define-key map "\C-x`" 'checkdoc-continue)
    (if (not checkdoc-xemacs-p)
	(define-key map [menu-bar emacs-lisp eval-buffer]
	  'checkdoc-eval-current-buffer))
    ;; Add some new bindings under C-c ?
    (define-key pmap "x" 'checkdoc-defun)
    (define-key pmap "X" 'checkdoc-ispell-defun)
    (define-key pmap "`" 'checkdoc-continue)
    (define-key pmap "~" 'checkdoc-ispell-continue)
    (define-key pmap "s" 'checkdoc-start)
    (define-key pmap "S" 'checkdoc-ispell-start)
    (define-key pmap "d" 'checkdoc)
    (define-key pmap "D" 'checkdoc-ispell)
    (define-key pmap "b" 'checkdoc-current-buffer)
    (define-key pmap "B" 'checkdoc-ispell-current-buffer)
    (define-key pmap "e" 'checkdoc-eval-current-buffer)
    (define-key pmap "m" 'checkdoc-message-text)
    (define-key pmap "M" 'checkdoc-ispell-message-text)
    (define-key pmap "c" 'checkdoc-comments)
    (define-key pmap "C" 'checkdoc-ispell-comments)
    (define-key pmap " " 'checkdoc-rogue-spaces)

    ;; bind our submap into map
    (define-key map "\C-c?" pmap)
    map)
  "Keymap used to override evaluation key-bindings for documentation checking.")

;; Add in a menubar with easy-menu

(if checkdoc-minor-keymap
    (easy-menu-define
     checkdoc-minor-menu checkdoc-minor-keymap "Checkdoc Minor Mode Menu"
     '("CheckDoc"
       ["Interactive Buffer Style Check" checkdoc t]
       ["Interactive Buffer Style and Spelling Check" checkdoc-ispell t]
       ["Check Buffer" checkdoc-current-buffer t]
       ["Check and Spell Buffer" checkdoc-ispell-current-buffer t]
       "---"
       ["Interactive Style Check" checkdoc-interactive t]
       ["Interactive Style and Spelling Check" checkdoc-ispell-interactive t]
       ["Find First Style Error" checkdoc-start t]
       ["Find First Style or Spelling  Error" checkdoc-ispell-start t]
       ["Next Style Error" checkdoc-continue t]
       ["Next Style or Spelling  Error" checkdoc-ispell-continue t]
       ["Interactive Message Text Style Check" checkdoc-message-interactive t]
       ["Interactive Message Text Style and Spelling Check"
	checkdoc-ispell-message-interactive t]
       ["Check Message Text" checkdoc-message-text t]
       ["Check and Spell Message Text" checkdoc-ispell-message-text t]
       ["Check Comment Style" checkdoc-comments buffer-file-name]
       ["Check Comment Style and Spelling" checkdoc-ispell-comments
	buffer-file-name]
       ["Check for Rogue Spaces" checkdoc-rogue-spaces t]
       "---"
       ["Check Defun" checkdoc-defun t]
       ["Check and Spell Defun" checkdoc-ispell-defun t]
       ["Check and Evaluate Defun" checkdoc-eval-defun t]
       ["Check and Evaluate Buffer" checkdoc-eval-current-buffer t]
       )))
;; XEmacs requires some weird stuff to add this menu in a minor mode.
;; What is it?

;; Allow re-insertion of a new keymap
(let ((a (assoc 'checkdoc-minor-mode minor-mode-map-alist)))
  (if a
      (setcdr a checkdoc-minor-keymap)
    (checkdoc-add-to-list 'minor-mode-map-alist (cons 'checkdoc-minor-mode
						      checkdoc-minor-keymap))))

;;;###autoload
(defun checkdoc-minor-mode (&optional arg)
  "Toggle Checkdoc minor mode, a mode for checking Lisp doc strings.
With prefix ARG, turn Checkdoc minor mode on iff ARG is positive.

In Checkdoc minor mode, the usual bindings for `eval-defun' which is
bound to \\<checkdoc-minor-keymap> \\[checkdoc-eval-defun] and `checkdoc-eval-current-buffer' are overridden to include
checking of documentation strings.

\\{checkdoc-minor-keymap}"
  (interactive "P")
  (setq checkdoc-minor-mode
	(not (or (and (null arg) checkdoc-minor-mode)
		 (<= (prefix-numeric-value arg) 0))))
  (checkdoc-mode-line-update))

;;; Subst utils
;;
(defsubst checkdoc-run-hooks (hookvar &rest args)
  "Run hooks in HOOKVAR with ARGS."
  (if (fboundp 'run-hook-with-args-until-success)
      (apply 'run-hook-with-args-until-success hookvar args)
    ;; This method was similar to above.  We ignore the warning
    ;; since we will use the above for future Emacs versions
    (apply 'run-hook-with-args hookvar args)))

(defsubst checkdoc-create-common-verbs-regexp ()
  "Rebuild the contents of `checkdoc-common-verbs-regexp'."
  (or checkdoc-common-verbs-regexp
      (setq checkdoc-common-verbs-regexp
	    (concat "\\<\\("
		    (mapconcat (lambda (e) (concat (car e)))
			       checkdoc-common-verbs-wrong-voice "\\|")
		    "\\)\\>"))))

;; Profiler says this is not yet faster than just calling assoc
;;(defun checkdoc-word-in-alist-vector (word vector)
;;  "Check to see if WORD is in the car of an element of VECTOR.
;;VECTOR must be sorted.  The CDR should be a replacement.  Since the
;;word list is getting bigger, it is time for a quick bisecting search."
;;  (let ((max (length vector)) (min 0) i
;;	(found nil) (fw nil))
;;    (setq i (/ max 2))
;;    (while (and (not found) (/= min max))
;;      (setq fw (car (aref vector i)))
;;      (cond ((string= word fw) (setq found (cdr (aref vector i))))
;;	    ((string< word fw) (setq max i))
;;	    (t (setq min i)))
;;      (setq i (/ (+ max min) 2))
;;      )
;;    found))

;;; Checking engines
;;
(defun checkdoc-this-string-valid ()
  "Return a message string if the current doc string is invalid.
Check for style only, such as the first line always being a complete
sentence, whitespace restrictions, and making sure there are no
hard-coded key-codes such as C-[char] or mouse-[number] in the comment.
See the style guide in the Emacs Lisp manual for more details."

  ;; Jump over comments between the last object and the doc string
  (while (looking-at "[ \t\n]*;")
    (forward-line 1)
    (beginning-of-line)
    (skip-chars-forward " \n\t"))

  (let ((fp (checkdoc-defun-info))
	(err nil))
    (setq
     err
     ;; * Every command, function, or variable intended for users to know
     ;;   about should have a documentation string.
     ;;
     ;; * An internal variable or subroutine of a Lisp program might as well
     ;;   have a documentation string.  In earlier Emacs versions, you could
     ;;   save space by using a comment instead of a documentation string,
     ;;   but that is no longer the case.
     (if (and (not (nth 1 fp))		; not a variable
	      (or (nth 2 fp)		; is interactive
		  checkdoc-force-docstrings-flag) ;or we always complain
	      (not (checkdoc-char= (following-char) ?\"))) ; no doc string
	 ;; Sometimes old code has comments where the documentation should
	 ;; be.  Let's see if we can find the comment, and offer to turn it
	 ;; into documentation for them.
	 (let ((have-comment nil)
	       (comment-start ";"))
	   (condition-case nil
	       (progn
		 (forward-sexp -1)
		 (forward-sexp 1)
		 (skip-chars-forward "\n \t")
		 (setq have-comment (looking-at comment-start)))
	     (error nil))
	   (if have-comment
	       (if (or (eq checkdoc-autofix-flag
			   'automatic-then-never)
		       (checkdoc-y-or-n-p
			"Convert comment to documentation? "))
		   (save-excursion
		     ;; Our point is at the beginning of the comment!
		     ;; Insert a quote, then remove the comment chars.
		     (insert "\"")
		     (let ((docstring-start-point (point)))
		       (while (looking-at comment-start)
			 (while (looking-at comment-start)
			   (delete-char 1))
			 (if (looking-at "[ \t]+")
			     (delete-region (match-beginning 0) (match-end 0)))
			 (forward-line 1)
			 (beginning-of-line)
			 (skip-chars-forward " \t")
			 (if (looking-at comment-start)
			     (progn
			       (beginning-of-line)
			       (zap-to-char 1 ?\;))))
		       (beginning-of-line)
		       (forward-char -1)
		       (insert "\"")
		       (forward-char -1)
		       ;; quote any double-quote characters in the comment.
		       (while (search-backward "\"" docstring-start-point t)
			 (insert "\\"))
		       (if (eq checkdoc-autofix-flag 'automatic-then-never)
			   (setq checkdoc-autofix-flag 'never))))
		 (checkdoc-create-error
		  "You should convert this comment to documentation"
		  (point) (save-excursion (end-of-line) (point))))
	     (checkdoc-create-error
	      (if (nth 2 fp)
		  "All interactive functions should have documentation"
		"All variables and subroutines might as well have a \
documentation string")
	      (point) (+ (point) 1) t)))))
    (if (and (not err) (looking-at "\""))
	(let ((old-syntax-table (syntax-table)))
	  (unwind-protect
	      (progn
		(set-syntax-table checkdoc-syntax-table)
		(checkdoc-this-string-valid-engine fp))
	    (set-syntax-table old-syntax-table)))
      err)))

(defun checkdoc-this-string-valid-engine (fp)
  "Return an error list or string if the current doc string is invalid.
Depends on `checkdoc-this-string-valid' to reset the syntax table so that
regexp short cuts work.  FP is the function defun information."
  (let ((case-fold-search nil)
	;; Use a marker so if an early check modifies the text,
	;; we won't accidentally loose our place.  This could cause
	;; end-of doc string whitespace to also delete the " char.
	(s (point))
	(e (if (looking-at "\"")
	       (save-excursion (forward-sexp 1) (point-marker))
	     (point))))
    (or
     ;; * *Do not* indent subsequent lines of a documentation string so that
     ;;   the text is lined up in the source code with the text of the first
     ;;   line.  This looks nice in the source code, but looks bizarre when
     ;;   users view the documentation.  Remember that the indentation
     ;;   before the starting double-quote is not part of the string!
     (save-excursion
       (forward-line 1)
       (beginning-of-line)
       (if (and (< (point) e)
		(looking-at "\\([ \t]+\\)[^ \t\n]"))
	   (if (checkdoc-autofix-ask-replace (match-beginning 1)
					     (match-end 1)
					     "Remove this whitespace? "
					     "")
	       nil
	     (checkdoc-create-error
	      "Second line should not have indentation"
	      (match-beginning 1)
	      (match-end 1)))))
     ;; * Do not start or end a documentation string with whitespace.
     (let (start end)
       (if (or (if (looking-at "\"\\([ \t\n]+\\)")
		   (setq start (match-beginning 1)
			 end (match-end 1)))
	       (save-excursion
		 (forward-sexp 1)
		 (forward-char -1)
		 (if (/= (skip-chars-backward " \t\n") 0)
		     (setq start (point)
			   end (1- e)))))
	   (if (checkdoc-autofix-ask-replace
		start end "Remove this whitespace? " "")
	       nil
	     (checkdoc-create-error
	      "Documentation strings should not start or end with whitespace"
	      start end))))
     ;; * The first line of the documentation string should consist of one
     ;;   or two complete sentences that stand on their own as a summary.
     ;;   `M-x apropos' displays just the first line, and if it doesn't
     ;;   stand on its own, the result looks bad.  In particular, start the
     ;;   first line with a capital letter and end with a period.
     (save-excursion
       (end-of-line)
       (skip-chars-backward " \t\n")
       (if (> (point) e) (goto-char e)) ;of the form (defun n () "doc" nil)
       (forward-char -1)
       (cond
	((and (checkdoc-char= (following-char) ?\")
	      ;; A backslashed double quote at the end of a sentence
	      (not (checkdoc-char= (preceding-char) ?\\)))
	 ;; We might have to add a period in this case
	 (forward-char -1)
	 (if (looking-at "[.!?]")
	     nil
	   (forward-char 1)
	   (if (checkdoc-autofix-ask-replace
		(point) (1+ (point)) "Add period to sentence? "
		".\"" t)
	       nil
	     (checkdoc-create-error
	      "First sentence should end with punctuation"
	      (point) (1+ (point))))))
	((looking-at "[\\!?;:.)]")
	 ;; These are ok
	 nil)
        ((and checkdoc-permit-comma-termination-flag (looking-at ","))
	 nil)
	(t
	 ;; If it is not a complete sentence, let's see if we can
	 ;; predict a clever way to make it one.
	 (let ((msg "First line is not a complete sentence")
	       (e (point)))
	   (beginning-of-line)
	   (if (re-search-forward "\\. +" e t)
	       ;; Here we have found a complete sentence, but no break.
	       (if (checkdoc-autofix-ask-replace
		    (1+ (match-beginning 0)) (match-end 0)
		    "First line not a complete sentence.  Add RET here? "
		    "\n" t)
		   (let (l1 l2)
		     (forward-line 1)
		     (end-of-line)
		     (setq l1 (current-column)
			   l2 (save-excursion
				(forward-line 1)
				(end-of-line)
				(current-column)))
		     (if (> (+ l1 l2 1) 80)
			 (setq msg "Incomplete auto-fix; doc string \
may require more formatting")
		       ;; We can merge these lines!  Replace this CR
		       ;; with a space.
		       (delete-char 1) (insert " ")
		       (setq msg nil))))
	     ;; Let's see if there is enough room to draw the next
	     ;; line's sentence up here.  I often get hit w/
	     ;; auto-fill moving my words around.
	     (let ((numc (progn (end-of-line) (- 80 (current-column))))
		   (p    (point)))
	       (forward-line 1)
	       (beginning-of-line)
	       (if (and (re-search-forward "[.!?:\"]\\([ \t\n]+\\|\"\\)"
					   (save-excursion
					     (end-of-line)
					     (point))
					   t)
			(< (current-column) numc))
		   (if (checkdoc-autofix-ask-replace
			p (1+ p)
			"1st line not a complete sentence.  Join these lines? "
			" " t)
		       (progn
			 ;; They said yes.  We have more fill work to do...
			 (goto-char (match-beginning 1))
			 (delete-region (point) (match-end 1))
			 (insert "\n")
			 (setq msg nil))))))
	   (if msg
	       (checkdoc-create-error msg s (save-excursion
					      (goto-char s)
					      (end-of-line)
					      (point)))
	     nil) ))))
     ;; Continuation of above.  Make sure our sentence is capitalized.
     (save-excursion
       (skip-chars-forward "\"\\*")
       (if (looking-at "[a-z]")
	   (if (checkdoc-autofix-ask-replace
		(match-beginning 0) (match-end 0)
		"Capitalize your sentence? " (upcase (match-string 0))
		t)
	       nil
	     (checkdoc-create-error
	      "First line should be capitalized"
	      (match-beginning 0) (match-end 0)))
	 nil))
     ;;   * Don't write key sequences directly in documentation strings.
     ;;     Instead, use the `\\[...]' construct to stand for them.
     (save-excursion
       (let ((f nil) (m nil) (start (point))
	     (re "[^`A-Za-z0-9_]\\([CMA]-[a-zA-Z]\\|\\(\\([CMA]-\\)?\
mouse-[0-3]\\)\\)\\>"))
	 ;; Find the first key sequence not in a sample
	 (while (and (not f) (setq m (re-search-forward re e t)))
	   (setq f (not (checkdoc-in-sample-code-p start e))))
	 (if m
	     (checkdoc-create-error
	      (concat
	       "Keycode " (match-string 1)
	       " embedded in doc string.  Use \\\\<keymap> & \\\\[function] "
	       "instead")
	      (match-beginning 1) (match-end 1) t))))
     ;; It is not practical to use `\\[...]' very many times, because
     ;; display of the documentation string will become slow.  So use this
     ;; to describe the most important commands in your major mode, and
     ;; then use `\\{...}' to display the rest of the mode's keymap.
     (save-excursion
       (if (re-search-forward "\\\\\\\\\\[\\w+" e t
			      (1+ checkdoc-max-keyref-before-warn))
	   (checkdoc-create-error
	    "Too many occurrences of \\[function].  Use \\{keymap} instead"
	    s (marker-position e))))
     ;; Ambiguous quoted symbol.  When a symbol is both bound and fbound,
     ;; and is referred to in documentation, it should be prefixed with
     ;; something to disambiguate it.  This check must be before the
     ;; 80 column check because it will probably break that.
     (save-excursion
       (let ((case-fold-search t)
	     (ret nil) mb me)
	 (while (and (re-search-forward "`\\(\\sw\\(\\sw\\|\\s_\\)+\\)'" e t)
		     (not ret))
	   (let* ((ms1 (match-string 1))
		  (sym (intern-soft ms1)))
	     (setq mb (match-beginning 1)
		   me (match-end 1))
	     (if (and sym (boundp sym) (fboundp sym)
		      (save-excursion
			(goto-char mb)
			(forward-word -1)
			(not (looking-at
			      "variable\\|option\\|function\\|command\\|symbol"))))
		 (if (checkdoc-autofix-ask-replace
		      mb me "Prefix this ambiguous symbol? " ms1 t)
		     ;; We didn't actually replace anything.  Here we find
		     ;; out what special word form they wish to use as
		     ;; a prefix.
		     (let ((disambiguate
			    (completing-read
			     "Disambiguating Keyword (default: variable): "
			     '(("function") ("command") ("variable")
			       ("option") ("symbol"))
			     nil t nil nil "variable")))
		       (goto-char (1- mb))
		       (insert disambiguate " ")
		       (forward-word 1))
		   (setq ret
			 (format "Disambiguate %s by preceding w/ \
function,command,variable,option or symbol." ms1))))))
	 (if ret
	     (checkdoc-create-error ret mb me)
	   nil)))
     ;; * Format the documentation string so that it fits in an
     ;;   Emacs window on an 80-column screen.  It is a good idea
     ;;   for most lines to be no wider than 60 characters.  The
     ;;   first line can be wider if necessary to fit the
     ;;   information that ought to be there.
     (save-excursion
       (let ((start (point))
	     (eol nil))
	 (while (and (< (point) e)
		     (or (progn (end-of-line) (setq eol (point))
				(< (current-column) 80))
			 (progn (beginning-of-line)
				(re-search-forward "\\\\\\\\[[<{]"
						   eol t))
			 (checkdoc-in-sample-code-p start e)))
	   (forward-line 1))
	 (end-of-line)
	 (if (and (< (point) e) (> (current-column) 80))
	     (checkdoc-create-error
	      "Some lines are over 80 columns wide"
	      s (save-excursion (goto-char s) (end-of-line) (point)) ))))
     ;; Here we deviate to tests based on a variable or function.
     ;; We must do this before checking for symbols in quotes because there
     ;; is a chance that just such a symbol might really be an argument.
     (cond ((eq (nth 1 fp) t)
	    ;; This is if we are in a variable
	    (or
	     ;; * The documentation string for a variable that is a
	     ;;   yes-or-no flag should start with words such as Non-nil
	     ;;   means..., to make it clear that all non-`nil' values are
	     ;;   equivalent and indicate explicitly what `nil' and non-`nil'
	     ;;   mean.
	     ;; * If a user option variable records a true-or-false
	     ;;   condition, give it a name that ends in `-flag'.

	     ;; If the variable has -flag in the name, make sure
	     (if (and (string-match "-flag$" (car fp))
		      (not (looking-at "\"\\*?Non-nil\\s-+means\\s-+")))
		 (checkdoc-create-error
		  "Flag variable doc strings should start: Non-nil means"
		  s (marker-position e) t))
	     ;; If the doc string starts with "Non-nil means"
	     (if (and (looking-at "\"\\*?Non-nil\\s-+means\\s-+")
		      (not (string-match "-flag$" (car fp))))
		 (if (checkdoc-y-or-n-p
		      (format
		       "Rename to %s and Query-Replace all occurances? "
		       (concat (car fp) "-flag")))
		     (progn
		       (beginning-of-defun)
		       (query-replace-regexp
			(concat "\\<" (regexp-quote (car fp)) "\\>")
			(concat (car fp) "-flag")))
		   (checkdoc-create-error
		    "Flag variables should end in `-flag'" s
		    (marker-position e))))
	     ;; Done with variables
	     ))
	   (t
	    ;; This if we are in a function definition
	    (or
	     ;; * When a function's documentation string mentions the value
	     ;;   of an argument of the function, use the argument name in
	     ;;   capital letters as if it were a name for that value.  Thus,
	     ;;   the documentation string of the function `/' refers to its
	     ;;   second argument as `DIVISOR', because the actual argument
	     ;;   name is `divisor'.

	     ;;   Addendum:  Make sure they appear in the doc in the same
	     ;;              order that they are found in the arg list.
	     (let ((args (cdr (cdr (cdr (cdr fp)))))
		   (last-pos 0)
		   (found 1)
		   (order (and (nth 3 fp) (car (nth 3 fp))))
		   (nocheck (append '("&optional" "&rest") (nth 3 fp)))
		   (inopts nil))
	       (while (and args found (> found last-pos))
		 (if (member (car args) nocheck)
		     (setq args (cdr args)
			   inopts t)
		   (setq last-pos found
			 found (save-excursion
				 (re-search-forward
				  (concat "\\<" (upcase (car args))
					  ;; Require whitespace OR
					  ;; ITEMth<space> OR
					  ;; ITEMs<space>
					  "\\(\\>\\|th\\>\\|s\\>\\)")
				  e t)))
		   (if (not found)
		       (let ((case-fold-search t))
			 ;; If the symbol was not found, let's see if we
			 ;; can find it with a different capitalization
			 ;; and see if the user wants to capitalize it.
			 (if (save-excursion
			       (re-search-forward
				(concat "\\<\\(" (car args)
					;; Require whitespace OR
					;; ITEMth<space> OR
					;; ITEMs<space>
					"\\)\\(\\>\\|th\\>\\|s\\>\\)")
				e t))
			     (if (checkdoc-autofix-ask-replace
				  (match-beginning 1) (match-end 1)
				  (format
				   "Argument `%s' should appear as `%s'.  Fix? "
				   (car args) (upcase (car args)))
				  (upcase (car args)) t)
				 (setq found (match-beginning 1))))))
		   (if found (setq args (cdr args)))))
	       (if (not found)
		   ;; It wasn't found at all!  Offer to attach this new symbol
		   ;; to the end of the documentation string.
		   (if (checkdoc-y-or-n-p
			(format
			 "Add %s documentation to end of doc string? "
			 (upcase (car args))))
		       ;; Now do some magic and invent a doc string.
		       (save-excursion
			 (goto-char e) (forward-char -1)
			 (insert "\n"
				 (if inopts "Optional a" "A")
				 "rgument " (upcase (car args))
				 " ")
			 (insert (read-string "Describe: "))
			 (if (not (save-excursion (forward-char -1)
						  (looking-at "[.?!]")))
			     (insert "."))
			 nil)
		     ;; Well, they don't want that.  Add ingore param?
		     (if (checkdoc-y-or-n-p
			  (format "Add %s to be never documented? " (car args)))
			 (save-excursion
			   (goto-char s)
			   (insert "; checkdoc-params: (" (car args) ")\n")
			   (indent-for-tab-command)
			   )
		       (checkdoc-create-error
			(format
			 "Argument `%s' should appear (as `%s') in the doc string"
			 (car args) (upcase (car args)))
			s (marker-position e))))
		 (if (or (and order (eq order 'yes))
			 (and (not order) checkdoc-arguments-in-order-flag))
		     (if (< found last-pos)
			 (checkdoc-create-error
			  "Arguments occur in the doc string out of order"
			  s (marker-position e) t)))))
	     ;; * For consistency, phrase the verb in the first sentence of a
	     ;;   documentation string for functions as an infinitive with
	     ;;   "to" omitted.  For instance, use `Return the cons of A and
	     ;;   B.' in preference to `Returns the cons of A and B.'
	     ;;   Usually it looks good to do likewise for the rest of the
	     ;;   first paragraph.  Subsequent paragraphs usually look better
	     ;;   if they have proper subjects.
	     ;;
	     ;; This is the least important of the above tests.  Make sure
	     ;; it occurs last.
	     (and checkdoc-verb-check-experimental-flag
		  (save-excursion
		    ;; Maybe rebuild the monster-regex
		    (checkdoc-create-common-verbs-regexp)
		    (let ((lim (save-excursion
				 (end-of-line)
				 ;; check string-continuation
				 (if (checkdoc-char= (preceding-char) ?\\)
				     (progn (forward-line 1)
					    (end-of-line)))
				 (point)))
			  (rs nil) replace original (case-fold-search t))
		      (while (and (not rs)
				  (re-search-forward
				   checkdoc-common-verbs-regexp
				   lim t))
			(setq original (buffer-substring-no-properties
					(match-beginning 1) (match-end 1))
			      rs (assoc (downcase original)
					checkdoc-common-verbs-wrong-voice))
			(if (not rs) (error "Verb voice alist corrupted"))
			(setq replace (let ((case-fold-search nil))
					(save-match-data
					  (if (string-match "^[A-Z]" original)
					      (capitalize (cdr rs))
					    (cdr rs)))))
			(if (checkdoc-autofix-ask-replace
			     (match-beginning 1) (match-end 1)
			     (format "Use the infinitive for `%s'.  \
Replace with `%s'? " original replace)
			     replace t)
			    (setq rs nil)))
		      (if rs
			  ;; there was a match, but no replace
			  (checkdoc-create-error
			   (format
			    "Infinitive `%s' should be replaced with `%s'"
			    original replace)
			   (match-beginning 1) (match-end 1))))))
	     ;; Done with functions
	     )))
     ;;* When a documentation string refers to a Lisp symbol, write it as
     ;;  it would be printed (which usually means in lower case), with
     ;;  single-quotes around it.  For example: `lambda'.  There are two
     ;;  exceptions: write t and nil without single-quotes.  (In this
     ;;  manual, we normally do use single-quotes for those symbols.)
     (save-excursion
       (let ((found nil) (start (point)) (msg nil) (ms nil))
	 (while (and (not msg)
		     (re-search-forward
		      "[^-([`':a-zA-Z]\\(\\w+[:-]\\(\\w\\|\\s_\\)+\\)[^]']"
		      e t))
	   (setq ms (match-string 1))
	   (save-match-data
	     ;; A . is a \s_ char, so we must remove periods from
	     ;; sentences more carefully.
	     (if (string-match "\\.$" ms)
		 (setq ms (substring ms 0 (1- (length ms))))))
	   (if (and (not (checkdoc-in-sample-code-p start e))
		    (not (checkdoc-in-example-string-p start e))
		    (not (member ms checkdoc-symbol-words))
		    (setq found (intern-soft ms))
		    (or (boundp found) (fboundp found)))
	       (progn
		 (setq msg (format "Add quotes around Lisp symbol `%s'? "
				   ms))
		 (if (checkdoc-autofix-ask-replace
		      (match-beginning 1) (+ (match-beginning 1)
					     (length ms))
		      msg (concat "`" ms "'") t)
		     (setq msg nil)
		   (setq msg
			 (format "Lisp symbol `%s' should appear in quotes"
				 ms))))))
	 (if msg
	     (checkdoc-create-error msg (match-beginning 1)
				    (+ (match-beginning 1)
				       (length ms)))
	   nil)))
     ;; t and nil case
     (save-excursion
       (if (re-search-forward "\\(`\\(t\\|nil\\)'\\)" e t)
	   (if (checkdoc-autofix-ask-replace
		(match-beginning 1) (match-end 1)
		(format "%s should not appear in quotes.  Remove? "
			(match-string 2))
		(match-string 2) t)
	       nil
	     (checkdoc-create-error
	      "Symbols t and nil should not appear in `quotes'"
	      (match-beginning 1) (match-end 1)))))
     ;; Here is some basic sentence formatting
     (checkdoc-sentencespace-region-engine (point) e)
     ;; Here are common proper nouns that should always appear capitalized.
     (checkdoc-proper-noun-region-engine (point) e)
     ;; Make sure the doc string has correctly spelled English words
     ;; in it.  This function is extracted due to its complexity,
     ;; and reliance on the Ispell program.
     (checkdoc-ispell-docstring-engine e)
     ;; User supplied checks
     (save-excursion (checkdoc-run-hooks 'checkdoc-style-hooks fp e))
     ;; Done!
     )))

(defun checkdoc-defun-info nil
  "Return a list of details about the current sexp.
It is a list of the form:
   (NAME VARIABLE INTERACTIVE NODOCPARAMS PARAMETERS ...)
where NAME is the name, VARIABLE is t if this is a `defvar',
INTERACTIVE is nil if this is not an interactive function, otherwise
it is the position of the `interactive' call, and PARAMETERS is a
string which is the name of each variable in the function's argument
list.  The NODOCPARAMS is a sublist of parameters specified by a checkdoc
comment for a given defun.  If the first element is not a string, then
the token checkdoc-order: <TOKEN> exists, and TOKEN is a symbol read
from the comment."
  (save-excursion
    (beginning-of-defun)
    (let ((defun (looking-at "(def\\(un\\|macro\\|subst\\|advice\\|method\\)"))
	  (is-advice (looking-at "(defadvice"))
	  (is-method (looking-at "(defmethod"))
	  (lst nil)
	  (ret nil)
	  (oo (make-vector 3 0)))	;substitute obarray for `read'
      (forward-char 1)
      (forward-sexp 1)
      (skip-chars-forward " \n\t")
      (setq ret
	    (list (buffer-substring-no-properties
		   (point) (progn (forward-sexp 1) (point)))))
      (if (not defun)
	  (setq ret (cons t ret))
	;; The variable spot
	(setq ret (cons nil ret))
	;; Interactive
	(save-excursion
	  (setq ret (cons
		     (re-search-forward "(interactive"
					(save-excursion (end-of-defun) (point))
					t)
		     ret)))
	(skip-chars-forward " \t\n")
	(let ((bss (buffer-substring (point) (save-excursion (forward-sexp 1)
							     (point))))
	      ;; Overload th main obarray so read doesn't intern the
	      ;; local symbols of the function we are checking.
	      ;; Without this we end up cluttering the symbol space w/
	      ;; useless symbols.
	      (obarray oo))
	  (if (and (symbolp (read bss)) is-method)
	      (save-excursion
		(setq bss (buffer-substring (progn (forward-sexp 1) (point))
					    (save-excursion (forward-sexp 1)
							    (point))))))
	  ;; Ok, check for checkdoc parameter comment here
	  (save-excursion
	    (setq ret
		  (cons
		   (let ((sl1 nil))
		     (if (re-search-forward ";\\s-+checkdoc-order:\\s-+"
					    (save-excursion (end-of-defun)
							    (point))
					    t)
			 (setq sl1 (list (cond ((looking-at "nil") 'no)
					       ((looking-at "t") 'yes)))))
		     (if (re-search-forward ";\\s-+checkdoc-params:\\s-+"
					    (save-excursion (end-of-defun)
							    (point))
					    t)
			 (let ((sl nil))
			   (goto-char (match-end 0))
			   (condition-case nil
			       (setq lst (read (current-buffer)))
			     (error (setq lst nil))) ; error in text
                           (if (not (listp lst)) ; not a list of args
                               (setq lst (list lst)))
			   (if (and lst (not (symbolp (car lst)))) ;weird arg
			       (setq lst nil))
			   (while lst
			     (setq sl (cons (symbol-name (car lst)) sl)
				   lst (cdr lst)))
			   (setq sl1 (append sl1 sl))))
		     sl1)
		   ret)))
	  ;; Read the list of parameters, but do not put the symbols in
	  ;; the standard obarray.
	  (setq lst (read bss)))
	;; If we are in a method, the first argument is itself a list;
	(if (and is-method (listp (car lst)))
	    (setcar lst (car (car lst))))
	;; This is because read will intern nil if it doesn't into the
	;; new obarray.
	(if (not (listp lst)) (setq lst nil))
	(if is-advice nil
	  (while lst
	    (setq ret (cons (symbol-name (car lst)) ret)
		  lst (cdr lst)))))
      (nreverse ret))))

(defun checkdoc-in-sample-code-p (start limit)
  "Return non-nil if the current point is in a code fragment.
A code fragment is identified by an open parenthesis followed by a
symbol which is a valid function or a word in all CAPS, or a parenthesis
that is quoted with the ' character.  Only the region from START to LIMIT
is is allowed while searching for the bounding parenthesis."
  (save-match-data
    (save-restriction
      (narrow-to-region start limit)
      (save-excursion
	(and (condition-case nil (progn (up-list 1) t) (error nil))
	     (condition-case nil (progn (forward-list -1) t) (error nil))
	     (or (save-excursion (forward-char -1) (looking-at "'("))
		 (and (looking-at "(\\(\\(\\w\\|[-:_]\\)+\\)[ \t\n;]")
		      (let ((ms (buffer-substring-no-properties
				 (match-beginning 1) (match-end 1))))
			;; if this string is function bound, we are in
			;; sample code.  If it has a - or : character in
			;; the name, then it is probably supposed to be bound
			;; but isn't yet.
			(or (fboundp (intern-soft ms))
			    (let ((case-fold-search nil))
			      (string-match "^[A-Z-]+$" ms))
			    (string-match "\\w[-:_]+\\w" ms))))))))))

(defun checkdoc-in-example-string-p (start limit)
  "Return non-nil if the current point is in an \"example string\".
This string is identified by the characters \\\" surrounding the text.
The text checked is between START and LIMIT."
  (save-match-data
    (save-excursion
      (let ((p (point))
	    (c 0))
	(goto-char start)
	(while (and (< (point) p) (re-search-forward "\\\\\"" limit t))
	  (setq c (1+ c)))
	(and (< 0 c) (= (% c 2) 0))))))

(defun checkdoc-proper-noun-region-engine (begin end)
  "Check all text between BEGIN and END for lower case proper nouns.
These are Emacs centric proper nouns which should be capitalized for
consistency.  Return an error list if any are not fixed, but
internally skip over no answers.
If the offending word is in a piece of quoted text, then it is skipped."
  (save-excursion
    (let ((case-fold-search nil)
	  (errtxt nil) bb be
	  (old-syntax-table (syntax-table)))
      (unwind-protect
	  (progn
	    (set-syntax-table checkdoc-syntax-table)
	    (goto-char begin)
	    (while (re-search-forward checkdoc-proper-noun-regexp end t)
	      (let ((text (match-string 1))
		    (b (match-beginning 1))
		    (e (match-end 1)))
		(if (and (not (save-excursion
				(goto-char b)
				(forward-char -1)
				(looking-at "`\\|\"\\|\\.\\|\\\\")))
			 ;; surrounded by /, as in a URL or filename: /emacs/
			 (not (and (= ?/ (char-after e))
				   (= ?/ (char-before b))))
			 (not (checkdoc-in-example-string-p begin end)))
		    (if (checkdoc-autofix-ask-replace
			 b e (format "Text %s should be capitalized.  Fix? "
				     text)
			 (capitalize text) t)
			nil
		      (if errtxt
			  ;; If there is already an error, then generate
			  ;; the warning output if applicable
			  (if checkdoc-generate-compile-warnings-flag
			      (checkdoc-create-error
			       (format
				"Name %s should appear capitalized as %s"
				text (capitalize text))
			       b e))
			(setq errtxt
			      (format
			       "Name %s should appear capitalized as %s"
			       text (capitalize text))
			      bb b be e)))))))
	(set-syntax-table old-syntax-table))
      (if errtxt (checkdoc-create-error errtxt bb be)))))

(defun checkdoc-sentencespace-region-engine (begin end)
  "Make sure all sentences have double spaces between BEGIN and END."
  (if sentence-end-double-space
      (save-excursion
	(let ((case-fold-search nil)
	      (errtxt nil) bb be
	      (old-syntax-table (syntax-table)))
	  (unwind-protect
	      (progn
		(set-syntax-table checkdoc-syntax-table)
		(goto-char begin)
		(while (re-search-forward "[^.0-9]\\(\\. \\)[^ \n]" end t)
		  (let ((b (match-beginning 1))
			(e (match-end 1)))
		    (if (and (not (checkdoc-in-sample-code-p begin end))
			     (not (checkdoc-in-example-string-p begin end))
			     (not (save-excursion
				    (goto-char (match-beginning 1))
				    (condition-case nil
					(progn
					  (forward-sexp -1)
					  ;; piece of an abbreviation
					  (looking-at
					   "\\([a-z]\\|[iI]\\.?e\\|[eE]\\.?g\\)\\."))
				      (error t)))))
			(if (checkdoc-autofix-ask-replace
			     b e
			     "There should be two spaces after a period.  Fix? "
			     ".  ")
			    nil
			  (if errtxt
			      ;; If there is already an error, then generate
			      ;; the warning output if applicable
			      (if checkdoc-generate-compile-warnings-flag
				  (checkdoc-create-error
				   "There should be two spaces after a period"
				   b e))
			    (setq errtxt
				  "There should be two spaces after a period"
				  bb b be e)))))))
	    (set-syntax-table old-syntax-table))
	  (if errtxt (checkdoc-create-error errtxt bb be))))))

;;; Ispell engine
;;
(eval-when-compile (require 'ispell))

(defun checkdoc-ispell-init ()
  "Initialize Ispell process (default version) with Lisp words.
The words used are from `checkdoc-ispell-lisp-words'.  If `ispell'
cannot be loaded, then set `checkdoc-spellcheck-documentation-flag' to
nil."
  (require 'ispell)
  (if (not (symbol-value 'ispell-process)) ;Silence byteCompiler
      (condition-case nil
	  (progn
	    (ispell-buffer-local-words)
	    ;; This code copied in part from ispell.el Emacs 19.34
	    (let ((w checkdoc-ispell-lisp-words))
	      (while w
		(process-send-string
		 ;;  Silence byte compiler
		 (symbol-value 'ispell-process)
		 (concat "@" (car w) "\n"))
		(setq w (cdr w)))))
	(error (setq checkdoc-spellcheck-documentation-flag nil)))))

(defun checkdoc-ispell-docstring-engine (end)
  "Run the Ispell tools on the doc string between point and END.
Since Ispell isn't Lisp-smart, we must pre-process the doc string
before using the Ispell engine on it."
  (if (or (not checkdoc-spellcheck-documentation-flag)
	  ;; If the user wants no questions or fixing, then we must
	  ;; disable spell checking as not useful.
	  (not checkdoc-autofix-flag)
	  (eq checkdoc-autofix-flag 'never))
      nil
    (checkdoc-ispell-init)
    (save-excursion
      (skip-chars-forward "^a-zA-Z")
      (let ((word nil) (sym nil) (case-fold-search nil) (err nil))
	(while (and (not err) (< (point) end))
	  (if (save-excursion (forward-char -1) (looking-at "[('`]"))
	      ;; Skip lists describing meta-syntax, or bound variables
	      (forward-sexp 1)
	    (setq word (buffer-substring-no-properties
			(point) (progn
				  (skip-chars-forward "a-zA-Z-")
				  (point)))
		  sym (intern-soft word))
	    (if (and sym (or (boundp sym) (fboundp sym)))
		;; This is probably repetitive in most cases, but not always.
		nil
	      ;; Find out how we spell-check this word.
	      (if (or
		   ;; All caps w/ option th, or s tacked on the end
		   ;; for pluralization or numberthness.
		   (string-match "^[A-Z][A-Z]+\\(s\\|th\\)?$" word)
		   (looking-at "}") ; a keymap expression
		   )
		  nil
		(save-excursion
		  (if (not (eq checkdoc-autofix-flag 'never))
		      (let ((lk last-input-event))
			(ispell-word nil t)
			(if (not (equal last-input-event lk))
			    (progn
			      (sit-for 0)
			      (message "Continuing..."))))
		    ;; Nothing here.
		    )))))
	  (skip-chars-forward "^a-zA-Z"))
	err))))

;;; Rogue space checking engine
;;
(defun checkdoc-rogue-space-check-engine (&optional start end interact)
  "Return a message list if there is a line with white space at the end.
If `checkdoc-autofix-flag' permits, delete that whitespace instead.
If optional arguments START and END are non nil, bound the check to
this region.
Optional argument INTERACT may permit the user to fix problems on the fly."
  (let ((p (point))
	(msg nil) s e (f nil))
    (if (not start) (setq start (point-min)))
    ;; If end is nil, it means end of buffer to search anyway
    (or
     ;; Check for an error if `? ' or `?\ ' is used at the end of a line.
     ;; (It's dangerous)
     (progn
       (goto-char start)
       (while (and (not msg) (re-search-forward "\\?\\\\?[ \t][ \t]*$" end t))
	 (setq msg
	       "Don't use `? ' at the end of a line. \
News agents may remove it"
	       s (match-beginning 0) e (match-end 0) f t)
	 ;; If interactive is passed down, give them a chance to fix things.
	 (if (and interact (y-or-n-p (concat msg ". Fix? ")))
	     (progn
	       (checkdoc-recursive-edit msg)
	       (setq msg nil)
	       (goto-char s)
	       (beginning-of-line)))))
     ;; Check for, and potentially remove whitespace appearing at the
     ;; end of different lines.
     (progn
       (goto-char start)
       ;; There is no documentation in the Emacs Lisp manual about this check,
       ;; it is intended to help clean up messy code and reduce the file size.
       (while (and (not msg) (re-search-forward "[^ \t\n;]\\([ \t]+\\)$" end t))
	 ;; This is not a complex activity
	 (if (checkdoc-autofix-ask-replace
	      (match-beginning 1) (match-end 1)
	      "White space at end of line.  Remove? " "")
	     nil
	   (setq msg "White space found at end of line"
		 s (match-beginning 1) e (match-end 1))))))
    ;; Return an error and leave the cursor at that spot, or restore
    ;; the cursor.
    (if msg
	(checkdoc-create-error msg s e f)
      (goto-char p)
      nil)))

;;; Comment checking engine
;;
(eval-when-compile
  ;; We must load this to:
  ;; a) get symbols for compile and
  ;; b) determine if we have lm-history symbol which doesn't always exist
  (require 'lisp-mnt))

(defun checkdoc-file-comments-engine ()
  "Return a message list if this file does not match the Emacs standard.
This checks for style only, such as the first line, Commentary:,
Code:, and others referenced in the style guide."
  (if (featurep 'lisp-mnt)
      nil
    (require 'lisp-mnt)
    ;; Old XEmacs don't have `lm-commentary-mark'
    (if (and (not (fboundp 'lm-commentary-mark)) (boundp 'lm-commentary))
	(defalias 'lm-commentary-mark 'lm-commentary)))
  (save-excursion
    (let* ((f1 (file-name-nondirectory (buffer-file-name)))
	   (fn (file-name-sans-extension f1))
	   (fe (substring f1 (length fn)))
	   (err nil))
      (goto-char (point-min))
      ;; This file has been set up where ERR is a variable.  Each check is
      ;; asked, and the function will make sure that if the user does not
      ;; auto-fix some error, that we still move on to the next auto-fix,
      ;; AND we remember the past errors.
      (setq
       err
       ;; Lisp Maintenance checks first
       ;; Was: (lm-verify) -> not flexible enough for some people
       ;; * Summary at the beginning of the file:
       (if (not (lm-summary))
	   ;; This certifies as very complex so always ask unless
	   ;; it's set to never
	   (if (checkdoc-y-or-n-p "There is no first line summary!  Add one? ")
	       (progn
		 (goto-char (point-min))
		 (insert ";;; " fn fe " --- " (read-string "Summary: ") "\n"))
	     (checkdoc-create-error
	      "The first line should be of the form: \";;; package --- Summary\""
	      (point-min) (save-excursion (goto-char (point-min)) (end-of-line)
					  (point))))
	 nil))
      (setq
       err
       (or
	;; * Commentary Section
	(if (not (lm-commentary-mark))
	    (progn
	      (goto-char (point-min))
	      (cond
	       ((re-search-forward
		 "write\\s-+to\\s-+the\\s-+Free Software Foundation, Inc."
		 nil t)
		(re-search-forward "^;;\\s-*\n\\|^\n" nil t))
	       ((or (re-search-forward "^;;; History" nil t)
		    (re-search-forward "^;;; Code" nil t)
		    (re-search-forward "^(require" nil t)
		    (re-search-forward "^("))
		(beginning-of-line)))
	      (if (checkdoc-y-or-n-p
		   "You should have a \";;; Commentary:\", add one? ")
		  (insert "\n;;; Commentary:\n;; \n\n")
		(checkdoc-create-error
		 "You should have a section marked \";;; Commentary:\""
		 nil nil t)))
	  nil)
	err))
      (setq
       err
       (or
	;; * History section.  Say nothing if there is a file ChangeLog
	(if (or (not checkdoc-force-history-flag)
		(file-exists-p "ChangeLog")
		(file-exists-p "../ChangeLog")
		(and (boundp 'change-log-default-name)
		     (file-exists-p change-log-default-name))
		(let ((fn 'lm-history-mark)) ;bestill byte-compiler
		  (and (fboundp fn) (funcall fn))))
	    nil
	  (progn
	    (goto-char (or (lm-commentary-mark) (point-min)))
	    (cond
	     ((re-search-forward
	       "write\\s-+to\\s-+the\\s-+Free Software Foundation, Inc."
	       nil t)
	      (re-search-forward "^;;\\s-*\n\\|^\n" nil t))
	     ((or (re-search-forward "^;;; Code" nil t)
		  (re-search-forward "^(require" nil t)
		  (re-search-forward "^("))
	      (beginning-of-line)))
	    (if (checkdoc-y-or-n-p
		 "You should have a \";;; History:\", add one? ")
		(insert "\n;;; History:\n;; \n\n")
	      (checkdoc-create-error
	       "You should have a section marked \";;; History:\" or use a ChangeLog"
	       (point) nil))))
	err))
      (setq
       err
       (or
	;; * Code section
	(if (not (lm-code-mark))
	    (let ((cont t))
	      (goto-char (point-min))
	      (while (and cont (re-search-forward "^(" nil t))
		(setq cont (looking-at "require\\s-+")))
	      (if (and (not cont)
		       (checkdoc-y-or-n-p
			"There is no ;;; Code: marker.  Insert one? "))
		  (progn (beginning-of-line)
			 (insert ";;; Code:\n")
			 nil)
		(checkdoc-create-error
		 "You should have a section marked \";;; Code:\""
		 (point) nil)))
	  nil)
	err))
      (setq
       err
       (or
	;; * A footer.  Not compartmentalized from lm-verify: too bad.
	;;              The following is partially clipped from lm-verify
	(save-excursion
	  (goto-char (point-max))
	  (if (not (re-search-backward
		    (concat "^;;;[ \t]+" fn "\\(" (regexp-quote fe)
			    "\\)?[ \t]+ends here[ \t]*$"
			    "\\|^;;;[ \t]+ End of file[ \t]+"
			    fn "\\(" (regexp-quote fe) "\\)?")
		    nil t))
	      (if (checkdoc-y-or-n-p "No identifiable footer!  Add one? ")
		  (progn
		    (goto-char (point-max))
		    (insert "\n(provide '" fn ")\n\n;;; " fn fe " ends here\n"))
		(checkdoc-create-error
		 (format "The footer should be: (provide '%s)\\n;;; %s%s ends here"
			 fn fn fe)
		 (1- (point-max)) (point-max)))))
	err))
      ;; The below checks will not return errors if the user says NO
      
      ;; Ok, now let's look for multiple occurrences of ;;;, and offer
      ;; to remove the extra ";" if applicable.  This pre-supposes
      ;; that the user has semiautomatic fixing on to be useful.
      
      ;; In the info node (elisp)Library Headers a header is three ;
      ;; (the header) followed by text of only two ;
      ;; In (elisp)Comment Tips, however it says this:
      ;; * Another use for triple-semicolon comments is for commenting out
      ;;   lines within a function.  We use triple-semicolons for this
      ;;   precisely so that they remain at the left margin.
      (let ((msg nil))
	(goto-char (point-min))
	(while (and checkdoc-triple-semi-comment-check-flag
		    (not msg) (re-search-forward "^;;;[^;]" nil t))
	  ;; We found a triple, let's check all following lines.
	  (if (not (bolp)) (progn (beginning-of-line) (forward-line 1)))
	  (let ((complex-replace t)
		(dont-replace nil))
	    (while (looking-at ";;\\(;\\)[^;#]")
	      (if (and (not dont-replace)
		       (checkdoc-outside-major-sexp) ;in code is ok.
		       (checkdoc-autofix-ask-replace
			(match-beginning 1) (match-end 1)
			"Multiple occurrences of ;;; found.  Use ;; instead? "
			"" complex-replace))
		  ;; Learn that, yea, the user did want to do this a
		  ;; whole bunch of times.
		  (setq complex-replace nil)
		;; In this case, skip all this crap
		(setq dont-replace t))
	      (beginning-of-line)
	      (forward-line 1)))))

      ;; Let's spellcheck the commentary section.  This is the only
      ;; section that is easy to pick out, and it is also the most
      ;; visible section (with the finder).
      (let ((cm (lm-commentary-mark)))
	(if cm
	    (save-excursion
	      (goto-char (lm-commentary-mark))
	      ;; Spellcheck between the commentary, and the first
	      ;; non-comment line.  We could use lm-commentary, but that
	      ;; returns a string, and Ispell wants to talk to a buffer.
	      ;; Since the comments talk about Lisp, use the specialized
	      ;; spell-checker we also used for doc strings.
	      (let ((e (save-excursion (re-search-forward "^[^;]" nil t)
				       (point))))
		(checkdoc-sentencespace-region-engine (point) e)
		(checkdoc-proper-noun-region-engine (point) e)
		(checkdoc-ispell-docstring-engine e)))))
;;; test comment out code
;;;       (foo 1 3)
;;;       (bar 5 7)
      (setq
       err
       (or
	;; Generic Full-file checks (should be comment related)
	(checkdoc-run-hooks 'checkdoc-comment-style-hooks)
	err))
      ;; Done with full file comment checks
      err)))

(defun checkdoc-outside-major-sexp ()
  "Return t if point is outside the bounds of a valid sexp."
  (save-match-data
    (save-excursion
      (let ((p (point)))
	(or (progn (beginning-of-defun) (bobp))
	    (progn (end-of-defun) (< (point) p)))))))

;;; `error' and `message' text verifier.
;;
(defun checkdoc-message-text-search (&optional beg end)
  "Search between BEG and END for a style error with message text.
Optional arguments BEG and END represent the boundary of the check.
The default boundary is the entire buffer."
  (let ((e nil)
	(type nil))
    (if (not (or beg end)) (setq beg (point-min) end (point-max)))
    (goto-char beg)
    (while (setq type (checkdoc-message-text-next-string end))
      (setq e (checkdoc-message-text-engine type)))
    e))
  
(defun checkdoc-message-text-next-string (end)
  "Move cursor to the next checkable message string after point.
Return the message classification.
Argument END is the maximum bounds to search in."
  (let ((return nil))
    (while (and (not return)
		(re-search-forward
		 "(\\s-*\\(\\(\\w\\|\\s_\\)*error\\|\
\\(\\w\\|\\s_\\)*y-or-n-p\\(-with-timeout\\)?\
\\|checkdoc-autofix-ask-replace\\)[ \t\n]+" end t))
      (let* ((fn (match-string 1))
	     (type (cond ((string-match "error" fn)
			  'error)
			 (t 'y-or-n-p))))
	(if (string-match "checkdoc-autofix-ask-replace" fn)
	    (progn (forward-sexp 2)
		   (skip-chars-forward " \t\n")))
	(if (and (eq type 'y-or-n-p)
		 (looking-at "(format[ \t\n]+"))
	    (goto-char (match-end 0)))
	(skip-chars-forward " \t\n")
	(if (not (looking-at "\""))
	    nil
	  (setq return type))))
    return))

(defun checkdoc-message-text-engine (&optional type)
  "Return or fix errors found in strings passed to a message display function.
According to the documentation for the function `error', the error list
should not end with a period, and should start with a capital letter.
The function `y-or-n-p' has similar constraints.
Argument TYPE specifies the type of question, such as `error or `y-or-n-p."
  ;; If type is nil, then attempt to derive it.
  (if (not type)
      (save-excursion
	(up-list -1)
	(if (looking-at "(format")
	    (up-list -1))
	(setq type
	      (cond ((looking-at "(error")
		     'error)
		    (t 'y-or-n-p)))))
  (let ((case-fold-search nil))
    (or
     ;; From the documentation of the symbol `error':
     ;; In Emacs, the convention is that error messages start with a capital
     ;; letter but *do not* end with a period.  Please follow this convention
     ;; for the sake of consistency.
     (if (and (save-excursion (forward-char 1)
			      (looking-at "[a-z]\\w+"))
	      (not (checkdoc-autofix-ask-replace
		    (match-beginning 0) (match-end 0)
		    "Capitalize your message text? "
		    (capitalize (match-string 0))
		    t)))
	 (checkdoc-create-error
	  "Messages should start with a capital letter"
	  (match-beginning 0) (match-end 0))
       nil)
     ;; In general, sentences should have two spaces after the period.
     (checkdoc-sentencespace-region-engine (point)
					   (save-excursion (forward-sexp 1)
							   (point)))
     ;; Look for proper nouns in this region too.
     (checkdoc-proper-noun-region-engine (point)
					 (save-excursion (forward-sexp 1)
							 (point)))
     ;; Here are message type specific questions.
     (if (and (eq type 'error)
	      (save-excursion (forward-sexp 1)
			      (forward-char -2)
			      (looking-at "\\."))
	      (not (checkdoc-autofix-ask-replace (match-beginning 0)
						 (match-end 0)
						 "Remove period from error? "
						 ""
						 t)))
	 (checkdoc-create-error
	  "Error messages should *not* end with a period"
	  (match-beginning 0) (match-end 0))
       nil)
     ;; `y-or-n-p' documentation explicitly says:
     ;; It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
     ;; I added the ? requirement.  Without it, it is unclear that we
     ;; ask a question and it appears to be an undocumented style.
     (if (eq type 'y-or-n-p)
	 (if (not (save-excursion (forward-sexp 1)
				  (forward-char -3)
				  (not (looking-at "\\? "))))
	     nil
	   (if (save-excursion (forward-sexp 1)
			       (forward-char -2)
			       (looking-at "\\?"))
	       ;; If we see a ?, then replace with "? ".
	       (if (checkdoc-autofix-ask-replace
		    (match-beginning 0) (match-end 0)
		    "`y-or-n-p' argument should end with \"? \".  Fix? "
		    "? " t)
		   nil
		 (checkdoc-create-error
		  "`y-or-n-p' argument should end with \"? \""
		  (match-beginning 0) (match-end 0)))
	     (if (save-excursion (forward-sexp 1)
				 (forward-char -2)
				 (looking-at " "))
		 (if (checkdoc-autofix-ask-replace
		      (match-beginning 0) (match-end 0)
		      "`y-or-n-p' argument should end with \"? \".  Fix? "
		      "? " t)
		     nil
		   (checkdoc-create-error
		    "`y-or-n-p' argument should end with \"? \""
		    (match-beginning 0) (match-end 0)))
	       (if (and ;; if this isn't true, we have a problem.
		    (save-excursion (forward-sexp 1)
				    (forward-char -1)
				    (looking-at "\""))
		    (checkdoc-autofix-ask-replace
		     (match-beginning 0) (match-end 0)
		     "`y-or-n-p' argument should end with \"? \".  Fix? "
		     "? \"" t))
		   nil
		 (checkdoc-create-error
		  "`y-or-n-p' argument should end with \"? \""
		  (match-beginning 0) (match-end 0)))))))
     ;; Now, let's just run the spell checker on this guy.
     (checkdoc-ispell-docstring-engine (save-excursion (forward-sexp 1)
						       (point)))
     )))

;;; Auto-fix helper functions
;;
(defun checkdoc-y-or-n-p (question)
  "Like `y-or-n-p', but pays attention to `checkdoc-autofix-flag'.
Argument QUESTION is the prompt passed to `y-or-n-p'."
  (prog1
      (if (or (not checkdoc-autofix-flag)
	      (eq checkdoc-autofix-flag 'never))
	  nil
	(y-or-n-p question))
    (if (eq checkdoc-autofix-flag 'automatic-then-never)
	(setq checkdoc-autofix-flag 'never))))

(defun checkdoc-autofix-ask-replace (start end question replacewith
					   &optional complex)
  "Highlight between START and END and queries the user with QUESTION.
If the user says yes, or if `checkdoc-autofix-flag' permits, replace
the region marked by START and END with REPLACEWITH.  If optional flag
COMPLEX is non-nil, then we may ask the user a question.  See the
documentation for `checkdoc-autofix-flag' for details.

If a section is auto-replaced without asking the user, this function
will pause near the fixed code so the user will briefly see what
happened.

This function returns non-nil if the text was replaced.

This function will not modify `match-data'."
  (if (and checkdoc-autofix-flag
	   (not (eq checkdoc-autofix-flag 'never)))
      (let ((o (checkdoc-make-overlay start end))
	    (ret nil)
	    (md (match-data)))
	(unwind-protect
	    (progn
	      (checkdoc-overlay-put o 'face 'highlight)
	      (if (or (eq checkdoc-autofix-flag 'automatic)
		      (eq checkdoc-autofix-flag 'automatic-then-never)
		      (and (eq checkdoc-autofix-flag 'semiautomatic)
			   (not complex))
		      (and (or (eq checkdoc-autofix-flag 'query) complex)
			   (y-or-n-p question)))
		  (save-excursion
		    (goto-char start)
		    ;; On the off chance this is automatic, display
		    ;; the question anyway so the user knows what's
		    ;; going on.
		    (if checkdoc-bouncy-flag (message "%s -> done" question))
		    (delete-region start end)
		    (insert replacewith)
		    (if checkdoc-bouncy-flag (sit-for 0))
		    (setq ret t)))
	      (checkdoc-delete-overlay o)
	      (set-match-data md))
	  (checkdoc-delete-overlay o)
	  (set-match-data md))
	(if (eq checkdoc-autofix-flag 'automatic-then-never)
	    (setq checkdoc-autofix-flag 'never))
	ret)))

;;; Warning management
;;
(defvar checkdoc-output-font-lock-keywords
  '(("\\(\\w+\\.el\\): \\(\\w+\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-comment-face))
    ("^\\(\\w+\\.el\\):" 1 font-lock-function-name-face)
    (":\\([0-9]+\\):" 1 font-lock-constant-face))
  "Keywords used to highlight a checkdoc diagnostic buffer.")

(defvar checkdoc-output-mode-map nil
  "Keymap used in `checkdoc-output-mode'.")

(defvar checkdoc-pending-errors nil
  "Non-nil when there are errors that have not been displayed yet.")

(if checkdoc-output-mode-map
    nil
  (setq checkdoc-output-mode-map (make-sparse-keymap))
  (if (not checkdoc-xemacs-p)
      (define-key checkdoc-output-mode-map [mouse-2]
	'checkdoc-find-error-mouse))
  (define-key checkdoc-output-mode-map "\C-c\C-c" 'checkdoc-find-error)
  (define-key checkdoc-output-mode-map "\C-m" 'checkdoc-find-error))

(defun checkdoc-output-mode ()
  "Create and setup the buffer used to maintain checkdoc warnings.
\\<checkdoc-output-mode-map>\\[checkdoc-find-error]  - Go to this error location
\\[checkdoc-find-error-mouse] - Goto the error clicked on."
  (if (get-buffer checkdoc-diagnostic-buffer)
      (get-buffer checkdoc-diagnostic-buffer)
    (save-excursion
      (set-buffer (get-buffer-create checkdoc-diagnostic-buffer))
      (kill-all-local-variables)
      (setq mode-name "Checkdoc"
	    major-mode 'checkdoc-output-mode)
      (set (make-local-variable 'font-lock-defaults)
	   '((checkdoc-output-font-lock-keywords) t t ((?- . "w") (?_ . "w"))))
      (use-local-map checkdoc-output-mode-map)
      (run-hooks 'checkdoc-output-mode-hook)
      (current-buffer))))

(defun checkdoc-find-error-mouse (e)
  ;; checkdoc-params: (e)
  "Call `checkdoc-find-error' where the user clicks the mouse."
  (interactive "e")
  (mouse-set-point e)
  (checkdoc-find-error))

(defun checkdoc-find-error ()
  "In a checkdoc diagnostic buffer, find the error under point."
  (interactive)
  (beginning-of-line)
  (if (looking-at "#<buffer \\(\\(\\w+\\|\\s_\\)+\\.el\\)>:\\([0-9]+\\):")
      (let ((l (string-to-int (match-string 3)))
	    (f (match-string 1)))
	(if (not (get-buffer f))
	    (error "Can't find buffer %s" f))
	(switch-to-buffer-other-window (get-buffer f))
	(goto-line l))))

(defun checkdoc-buffer-label ()
  "The name to use for a checkdoc buffer in the error list."
  (if (buffer-file-name)
      (file-name-nondirectory (buffer-file-name))
    (concat "#<buffer "(buffer-name) ">")))

(defun checkdoc-start-section (check-type)
  "Initialize the checkdoc diagnostic buffer for a pass.
Create the header so that the string CHECK-TYPE is displayed as the
function called to create the messages."
  (checkdoc-output-to-error-buffer
   "\n\n\C-l\n*** "
   (checkdoc-buffer-label) ": " check-type " V " checkdoc-version))

(defun checkdoc-error (point msg)
  "Store POINT and MSG as errors in the checkdoc diagnostic buffer."
  (setq checkdoc-pending-errors t)
  (checkdoc-output-to-error-buffer
   "\n" (checkdoc-buffer-label) ":"
   (int-to-string (count-lines (point-min) (or point 1))) ": "
   msg))

(defun checkdoc-output-to-error-buffer (&rest text)
  "Place TEXT into the checkdoc diagnostic buffer."
  (save-excursion
    (set-buffer (checkdoc-output-mode))
    (goto-char (point-max))
    (apply 'insert text)))

(defun checkdoc-show-diagnostics ()
  "Display the checkdoc diagnostic buffer in a temporary window."
  (if checkdoc-pending-errors
      (let ((b (get-buffer checkdoc-diagnostic-buffer)))
	(if b (progn (pop-to-buffer b)
		     (goto-char (point-max))
		     (re-search-backward "\C-l" nil t)
		     (beginning-of-line)
		     (forward-line 1)
		     (recenter 0)))
	(other-window -1)
	(setq checkdoc-pending-errors nil)
	nil)))

(defgroup checkdoc nil
  "Support for doc string checking in Emacs Lisp."
  :prefix "checkdoc"
  :group 'lisp
;  :version "20.3"
  )

(custom-add-option 'emacs-lisp-mode-hook
		   (lambda () (checkdoc-minor-mode 1)))

(provide 'checkdoc)

;;; checkdoc.el ends here
