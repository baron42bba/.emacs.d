;;; edmacro.el --- keyboard macro editor

;; Copyright (C) 1993, 1994, 1997 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;;         Hrvoje Niksic <hniksic@xemacs.org>  -- XEmacs rewrite
;; Maintainer: Hrvoje Niksic <hniksic@xemacs.org>
;; Version: 3.20
;; Keywords: abbrev, internal

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

;;; Synched up with: FSF 19.34.
;;; Most of this file has been rewritten for XEmacs, so the
;;; implementations are out of synch.  The original version depended
;;; too closely on GNU Emacs key representation and the equivalence of
;;; characters and integers to be usable.

;;; Commentary:

;;; Usage:
;;
;; The `C-x C-k' (`edit-kbd-macro') command edits a keyboard macro
;; in a special buffer.  It prompts you to type a key sequence,
;; which should be one of:
;;
;;  * RET or `C-x e' (call-last-kbd-macro), to edit the most 
;;    recently defined keyboard macro.
;;
;;  * `M-x' followed by a command name, to edit a named command
;;    whose definition is a keyboard macro.
;;
;;  * `C-h l' (view-lossage), to edit the 100 most recent keystrokes
;;    and install them as the "current" macro.
;;
;;  * any key sequence whose definition is a keyboard macro.
;;
;; This file includes a version of `insert-kbd-macro' that uses the
;; more readable format defined by these routines.
;;
;; Also, the `read-kbd-macro' command parses the region as
;; a keyboard macro, and installs it as the "current" macro.
;; This and `format-kbd-macro' can also be called directly as
;; Lisp functions.

;; The `kbd' macro is a shorter-named and more efficient form of
;; `read-kbd-macro'.  Unlike `read-kbd-macro', it is evaluated at
;; read-time, and doesn't bring any overhead to compiled programs.  It
;; is recommended to use in your programs and initializations, as you
;; needn't know the internal keysym representation.  For example:
;;
;; (define-key foo-mode-map (kbd "C-c <up>") 'foo-up)
;;
;; is the exact equivalent of
;;
;; (define-key foo-mode-map [(control ?c) up] 'foo-up)
;;

;; Type `C-h m', or see the documentation for `edmacro-mode' below,
;; for information about the format of written keyboard macros.

;; `edit-kbd-macro' formats the macro with one command per line,
;; including the command names as comments on the right.  If the
;; formatter gets confused about which keymap was used for the
;; characters, the command-name comments will be wrong but that
;; won't hurt anything.

;; With a prefix argument, `edit-kbd-macro' will format the
;; macro in a more concise way that omits the comments.

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup edmacro nil
  "Keyboard macro editor."
  :group 'keyboard)

(defcustom edmacro-eight-bits nil
  "*Non-nil if edit-kbd-macro should leave 8-bit characters intact.
Default nil means to write characters above \\177 in octal notation."
  :type 'boolean
  :group 'edmacro)

(defcustom edmacro-format-hook nil
  "*Hook run after formatting the keyboard macro."
  :type 'hook
  :group 'edmacro)

(defvar edmacro-finish-hook nil)
(defvar edmacro-store-hook nil)
(defvar edmacro-original-buffer nil)

;;; The user-level commands for editing macros.

;;;###autoload (define-key ctl-x-map "\C-k" 'edit-kbd-macro)

(defvar edmacro-mode-map nil)
(unless edmacro-mode-map
  (setq edmacro-mode-map (make-sparse-keymap))
  (define-key edmacro-mode-map "\C-c\C-c" 'edmacro-finish-edit)
  (define-key edmacro-mode-map "\C-c\C-q" 'edmacro-insert-key))

;;;###autoload
(defun edit-kbd-macro (keys &optional prefix finish-hook store-hook)
  "Edit a keyboard macro.
At the prompt, type any key sequence which is bound to a keyboard macro.
Or, type `C-x e' or RET to edit the last keyboard macro, `C-h l' to edit
the last 100 keystrokes as a keyboard macro, or `M-x' to edit a macro by
its command name.
With a prefix argument, format the macro in a more concise way."
  (interactive "kKeyboard macro to edit (C-x e, M-x, C-h l, or keys): \nP")
  (when (vectorp keys)
    (setq keys (edmacro-events-to-keys keys)))
  (let ((cmd (if (symbolp keys) keys (key-binding keys)))
	(mac nil))
    (cond (store-hook
	   (setq mac keys)
	   (setq cmd nil))
	  ((or (eq cmd 'call-last-kbd-macro)
	       (and (arrayp keys)
		    (= 1 (length keys))
		    (or (eq 'return (aref keys 0))
			(eq ?\r (aref keys 0))
			(equal '(control ?m) (aref keys 0)))))
	   (or last-kbd-macro
	       (y-or-n-p "No keyboard macro defined.  Create one? ")
	       (keyboard-quit))
	   (setq mac (or last-kbd-macro []))
	   (setq cmd 'last-kbd-macro))
	  ((eq cmd 'execute-extended-command)
	   (setq cmd (edmacro-minibuf-read "Name of keyboard macro to edit: "))
	   (if (string-equal cmd "")
	   (error "No command name given"))
	   (setq mac (symbol-function cmd)))
	  ((eq cmd 'view-lossage)
	   (setq mac (recent-keys))
	   (setq cmd 'last-kbd-macro))
	  ((null cmd)
	   (error "Key sequence `%s' is not defined" (key-description keys)))
	  ((symbolp cmd)
	   (setq mac (symbol-function cmd)))
	  (t
	   (setq mac cmd)
	   (setq cmd nil)))
    (unless (arrayp mac)
      (error "Key sequence `%s' is not a keyboard macro"
	     (key-description keys)))
    (message "Formatting keyboard macro...")
    (let ((oldbuf (current-buffer))
	  (fmt (edmacro-format-keys mac))
	  (fmtv (edmacro-format-keys mac (not prefix)))
	  (buf (get-buffer-create "*Edit Macro*")))
      (message "Formatting keyboard macro...done")
      (switch-to-buffer buf)
      (kill-all-local-variables)
      (use-local-map edmacro-mode-map)
      (setq buffer-read-only nil)
      (setq major-mode 'edmacro-mode)
      (setq mode-name "Edit Macro")
      (set (make-local-variable 'edmacro-original-buffer) oldbuf)
      (set (make-local-variable 'edmacro-finish-hook) finish-hook)
      (set (make-local-variable 'edmacro-store-hook) store-hook)
      (erase-buffer)
      (insert ";; Keyboard Macro Editor.  Press C-c C-c to finish; "
	      "press C-x k RET to cancel.\n")
      (insert ";; Original keys: " fmt "\n")
      (unless store-hook
	(insert "\nCommand: " (if cmd (symbol-name cmd) "none") "\n")
	(let ((keys (where-is-internal (or cmd mac))))
	  (if keys
	      (dolist (key keys)
		(insert "Key: " (edmacro-format-keys key) "\n"))
	    (insert "Key: none\n"))))
      (insert "\nMacro:\n\n")
      (save-excursion
	(insert fmtv "\n"))
      (recenter '(4))
      (run-hooks 'edmacro-format-hook))))

;;; The next two commands are provided for convenience and backward
;;; compatibility.

;;;###autoload
(defun edit-last-kbd-macro (&optional prefix)
  "Edit the most recently defined keyboard macro."
  (interactive "P")
  (edit-kbd-macro 'call-last-kbd-macro prefix))

;;;###autoload
(defun edit-named-kbd-macro (&optional prefix)
  "Edit a keyboard macro which has been given a name by `name-last-kbd-macro'."
  (interactive "P")
  (edit-kbd-macro 'execute-extended-command prefix))

;;;###autoload
(defun read-kbd-macro (start &optional end)
  "Read the region as a keyboard macro definition.
The region is interpreted as spelled-out keystrokes, e.g., \"M-x abc RET\".
See documentation for `edmacro-mode' for details.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result will be a vector of keystrokes."
  (interactive "r")
  (if (stringp start)
      (edmacro-parse-keys start)
    (setq last-kbd-macro (edmacro-parse-keys (buffer-substring start end)))))

;;;###autoload
(defun format-kbd-macro (&optional macro verbose)
  "Return the keyboard macro MACRO as a human-readable string.
This string is suitable for passing to `read-kbd-macro'.
Second argument VERBOSE means to put one command per line with comments.
If VERBOSE is nil, put everything on one line."
  (and macro (symbolp macro) (setq macro (symbol-function macro)))
  (edmacro-format-keys (or macro last-kbd-macro) verbose))


;;; Commands for *Edit Macro* buffer.

(defun edmacro-finish-edit ()
  (interactive)
  (unless (eq major-mode 'edmacro-mode)
    (error
     "This command is valid only in buffers created by `edit-kbd-macro'"))
  (run-hooks 'edmacro-finish-hook)
  (let ((cmd nil) (keys nil) (no-keys nil)
	(top (point-min)))
    (goto-char top)
    (let ((case-fold-search nil))
      (while (cond ((looking-at "[ \t]*\\($\\|;;\\|REM[ \t\n]\\)")
		    t)
		   ((looking-at "Command:[ \t]*\\([^ \t\n]*\\)[ \t]*$")
		    (when edmacro-store-hook
		      (error "\"Command\" line not allowed in this context"))
		    (let ((str (buffer-substring (match-beginning 1)
						 (match-end 1))))
		      (unless (equal str "")
			(setq cmd (and (not (equal str "none"))
				       (intern str)))
			(and (fboundp cmd) (not (arrayp (symbol-function cmd)))
			     (not (y-or-n-p
				   (format "Command %s is already defined; %s"
					   cmd "proceed? ")))
			     (keyboard-quit))))
		    t)
		   ((looking-at "Key:\\(.*\\)$")
		    (when edmacro-store-hook
		      (error "\"Key\" line not allowed in this context"))
		    (let ((key (edmacro-parse-keys
				(buffer-substring (match-beginning 1)
						  (match-end 1)))))
		      (unless (equal key [])
			(if (equal key [?n ?o ?n ?e])
			    (setq no-keys t)
			  (push key keys)
			  (let ((b (key-binding key)))
			    (and b (commandp b) (not (arrayp b))
				 (or (not (fboundp b))
				     (not (arrayp (symbol-function b))))
				 (not (y-or-n-p
				       (format
					"Key `%s' is already defined; %s"
					(edmacro-format-keys key)
					"proceed? ")))
				 (keyboard-quit))))))
		    t)
		   ((looking-at "Macro:[ \t\n]*")
		    (goto-char (match-end 0))
		    nil)
		   ((eobp) nil)
		   (t (error "Expected a `Macro:' line")))
	(forward-line 1))
      (setq top (point)))
    (let* ((buf (current-buffer))
	   (str (buffer-substring top (point-max)))
	   (modp (buffer-modified-p))
	   (obuf edmacro-original-buffer)
	   (store-hook edmacro-store-hook))
      (unless (or cmd keys store-hook (equal str ""))
	(error "No command name or keys specified"))
      (when modp
	(when (buffer-name obuf)
	  (set-buffer obuf))
	(message "Compiling keyboard macro...")
	(let ((mac (edmacro-parse-keys str)))
	  (message "Compiling keyboard macro...done")
	  (if store-hook
	      (funcall store-hook mac)
	    (when (eq cmd 'last-kbd-macro)
	      (setq last-kbd-macro (and (> (length mac) 0) mac))
	      (setq cmd nil))
	    (when cmd
	      (if (= (length mac) 0)
		  (fmakunbound cmd)
		(fset cmd mac)))
	    (if no-keys
		(when cmd
		  (loop for key in (where-is-internal cmd) do
			(global-unset-key key)))
	      (when keys
		(if (= (length mac) 0)
		    (loop for key in keys do (global-unset-key key))
		  (loop for key in keys do
			(global-set-key key (or cmd mac)))))))))
      (kill-buffer buf)
      (when (buffer-name obuf)
	(switch-to-buffer obuf)))))

(defun edmacro-insert-key (key)
  "Insert the written name of a key in the buffer."
  (interactive "kKey to insert: ")
  (if (bolp)
      (insert (edmacro-format-keys key t) "\n")
    (insert (edmacro-format-keys key) " ")))

(defun edmacro-mode ()
  "\\<edmacro-mode-map>Keyboard Macro Editing mode.  Press
\\[edmacro-finish-edit] to save and exit.
To abort the edit, just kill this buffer with \\[kill-buffer] RET.

Press \\[edmacro-insert-key] to insert the name of any key by typing the key.

The editing buffer contains a \"Command:\" line and any number of
\"Key:\" lines at the top.  These are followed by a \"Macro:\" line
and the macro itself as spelled-out keystrokes: `C-x C-f foo RET'.

The \"Command:\" line specifies the command name to which the macro
is bound, or \"none\" for no command name.  Write \"last-kbd-macro\"
to refer to the current keyboard macro (as used by \\[call-last-kbd-macro]).

The \"Key:\" lines specify key sequences to which the macro is bound,
or \"none\" for no key bindings.

You can edit these lines to change the places where the new macro
is stored.


Format of keyboard macros during editing:

Text is divided into \"words\" separated by whitespace.  Except for
the words described below, the characters of each word go directly
as characters of the macro.  The whitespace that separates words
is ignored.  Whitespace in the macro must be written explicitly,
as in \"foo SPC bar RET\".

 * The special words RET, SPC, TAB, DEL, BS, LFD, ESC, and NUL represent
   special control characters.  The words must be written in uppercase.

 * A word in angle brackets, e.g., <return>, <down>, or <f1>, represents
   a function key.  (Note that in the standard configuration, the
   function key <return> and the control key RET are synonymous.)
   You can use angle brackets on the words RET, SPC, etc., but they
   are not required there.

 * Keys can be written by their ASCII code, using a backslash followed
   by up to six octal digits.  This is the only way to represent keys
   with codes above \\377.

 * One or more prefixes M- (meta), C- (control), S- (shift), A- (alt),
   H- (hyper), and s- (super) may precede a character or key notation.
   For function keys, the prefixes may go inside or outside of the
   brackets:  C-<down> = <C-down>.  The prefixes may be written in
   any order:  M-C-x = C-M-x.

   Prefixes are not allowed on multi-key words, e.g., C-abc, except
   that the Meta prefix is allowed on a sequence of digits and optional
   minus sign:  M--123 = M-- M-1 M-2 M-3.

 * The `^' notation for control characters also works:  ^M = C-m.

 * Double angle brackets enclose command names:  <<next-line>> is
   shorthand for M-x next-line RET.

 * Finally, REM or ;; causes the rest of the line to be ignored as a
   comment.

Any word may be prefixed by a multiplier in the form of a decimal
number and `*':  3*<right> = <right> <right> <right>, and
10*foo = foofoofoofoofoofoofoofoofoofoo.

Multiple text keys can normally be strung together to form a word,
but you may need to add whitespace if the word would look like one
of the above notations:  `; ; ;' is a keyboard macro with three
semicolons, but `;;;' is a comment.  Likewise, `\\ 1 2 3' is four
keys but `\\123' is a single key written in octal, and `< right >'
is seven keys but `<right>' is a single function key.  When in
doubt, use whitespace."
  (interactive)
  (error "This mode can be enabled only by `edit-kbd-macro'"))
(put 'edmacro-mode 'mode-class 'special)


(defun edmacro-int-char (int)
  (if (fboundp 'int-char)
      (int-char int)
    int))

(defvar edmacro-read-history nil)

;; Completing read on named keyboard macros only.
(defun edmacro-minibuf-read (prompt)
  (intern (completing-read
	   prompt obarray
	   (lambda (arg)
	     (and (commandp arg)
		  (vectorp (symbol-function arg))))
	   t nil 'edmacro-read-history)))


(defvar edmacro-char-to-word
  '((?\0 . "NUL")
    (?\r . "RET")
    (?\n . "LFD")
    (?\t . "TAB")
    (?\e . "ESC")
    (?\  . "SPC")
    (?\C-? . "DEL")))

(defvar edmacro-modifiers
  '(("C" . control)
    ("M" . meta)
    ("S" . shift)
    ("Sh" . shift)
    ("A" . alt)
    ("H" . hyper)
    ("s" . super)))

;;; Parsing a human-readable keyboard macro.

;; In XEmacs version of edmacro, edmacro-parse-keys always returns a
;; vector.  edmacro-format-keys accepts a vector (but works with a
;; string too).
(defun edmacro-parse-keys (string)
  (let* ((pos 0)
	 (case-fold-search nil)
	 res)
    (while (and (< pos (length string))
		(string-match "[^ \t\r\n\f]+" string pos))
      (let ((word (substring string (match-beginning 0) (match-end 0))))
	(setq pos (match-end 0))
	(if (or (equal word "REM") (string-match "^;;" word))
	    ;; Comment (discard to EOL) .
	    (setq pos (string-match "$" string pos))
	  (push (edmacro-parse-word word) res))))
    (mapvector 'identity (apply 'nconc (nreverse res)))))

;; Parse a "word".
(defun edmacro-parse-word (word)
  (let ((force-sym nil)
	(times 1)
	abbr)
    (when (string-match "\\([0-9]+\\)\\*." word)
      (setq times (string-to-int (substring word 0 (match-end 1))))
      (setq word (substring word (1+ (match-end 1)))))
    (when (string-match "^<\\([^<>]+\\)>$" word)
      (setq word (match-string 1 word))
      (setq force-sym t))
    (let* ((word-to-sym '(("NUL" . ?\0)
			  ("RET" . return)
			  ("LFD" . linefeed)
			  ("TAB" . tab)
			  ("ESC" . escape)
			  ("SPC" . space)
			  ("BS" . backspace)
			  ("DEL" . delete)))
	   (conv (lambda (arg)
		   ;; string-to-symbol-or-char converter
		   (if (= (length arg) 1)
		       (aref arg 0)
		     (if (string-match "^<\\([^>]+\\)>$" arg)
			 (setq arg (match-string 1 arg)))
		     (let ((match (assoc arg word-to-sym)))
		       (if match
			   (cdr match)
			 (intern arg))))))
	   (conv-chars (lambda (arg)
			 (let ((match (assoc arg edmacro-char-to-word)))
			   (if match
			       (cdr (assoc (cdr match) word-to-sym))
			     arg))))
	   (add
	    (cond
	     ((string-match "^\\\\[0-7]+" word)
	      ;; Octal value of character.
	      (list
	       (edmacro-int-char
		(hexl-octal-string-to-integer (substring word 1)))))
	     ((string-match "^<<.+>>$" word)
	      ;; Extended command.
	      (nconc
	       (list
		(if (eq (key-binding [(meta x)])
			'execute-extended-command)
		    '(meta x)
		  (or (car (where-is-internal
			    'execute-extended-command))
		      '(meta x))))
	       (mapcar conv-chars (concat (substring word 2 -2) "\r"))))
	     ((setq abbr (assoc word word-to-sym))
	      ;; Convert to symbol.
	      (list (cdr abbr)))
	     ((string-match "^\\^" word)
	      ;; ^X == C-x
	      (if (= (length word) 2)
		  `((control ,(aref word 1)))
		(mapcar 'identity word)))
	     ((string-match "^M--?[0-9]+$" word)
	      ;; Special case: M- followed by an optional hyphen and
	      ;; one or more digits
	      (mapcar (lambda (digit)
			(list 'meta digit))
		      (substring word 2)))
	     ((string-match "^\\([MCSsAH]\\|Sh\\)-" word)
	      ;; Parse C-* and stuff
	      (let ((pos1 0)
		    (r1 nil)
		    follow curpart prefix)
		(while (progn (setq curpart (substring word pos1))
			      (string-match "^\\([MCSsAH]\\|Sh\\)-"
					    curpart))
		  (setq prefix (assoc (match-string 1 curpart)
				      edmacro-modifiers))
		  (push (cdr prefix) r1)
		  (incf pos1 (1+ (length (car prefix)))))
		(setq follow (substring word pos1))
		(if (equal follow "")
		    ;; we've got something like "C-M-" -- just let it be,
		    ;; because of the way `edmacro-format-keys' works.
		    (mapcar 'identity word)
		  (list (nconc (nreverse r1) (list (funcall conv follow)))))))
	     (force-sym
	      ;; This must be a symbol
	      (list (intern word)))
	     (t
	      ;; Characters
	      (mapcar conv-chars word))))
	   (new nil))
	   (loop repeat times do (setq new (append add new)))
	   new)))

;; Convert the keypress events in vector x to keys, and return a
;; vector of keys.  If a list element is not a keypress event, ignore
;; it.  `events-to-keys' won't quite cut it here, as it is buggy.
(defun edmacro-events-to-keys (x &optional list)
  (let (new)
    (mapc (lambda (el)
	    (cond ((key-press-event-p el)
		   (push (let ((mods (event-modifiers el)))
			   (if mods
			       (append mods (list (event-key el)))
			     (event-key el)))
			 new))
		  ((or (characterp el)
		       (symbolp el)
		       (listp el))
		   (push el new))))
	  x)
    (setq new (nreverse new))
    (if list
	new
      (mapvector 'identity new))))

;; Collapse a list of keys into a list of function keys, if any.
(defun edmacro-fkeys (keys)
  (let (new k lookup)
    (while keys
      (setq k (nconc k (list (car keys))))
      (setq lookup (lookup-key function-key-map (mapvector 'identity k)))
      (cond ((vectorp lookup)
	     (push (mapcar 'identity lookup) new)
	     (setq k nil))
	    ((keymapp lookup)
	     nil)
	    ((null lookup)
	     (push k new)
	     (setq k nil))
	    (t
	     (setq k nil)))
      (pop keys))
    (when (keymapp lookup)
	(push k new))
    (apply 'nconc (nreverse new))))

;; Convert a character or symbol to string.
(defun edmacro-conv (char-or-sym add-<>)
  (let ((char-to-word '((?\0 . "NUL")
			(?\r . "RET")
			(?\n . "LFD")
			(?\t . "TAB")
			(?\e . "ESC")
			(?\  . "SPC")
			(?\C-? . "DEL")))
	(symbol-to-char '((return . ?\r)
			  (linefeed . ?\n)
			  (space . ?\ )
			  (delete . ?\C-?)
			  (tab . ?\t)
			  (escape . ?\e))))
    (if (symbolp char-or-sym)
	(if (= (length (symbol-name char-or-sym)) 1)
	    (setq char-or-sym (aref (symbol-name char-or-sym) 0))
	  (let ((found (assq char-or-sym symbol-to-char)))
	    (if found
		(setq char-or-sym (cdr found))))))
    ;; Return:
    (cons (symbolp char-or-sym)
	  (if (symbolp char-or-sym)
	      (if add-<>
		  (concat "<" (symbol-name char-or-sym) ">")
		(symbol-name char-or-sym))
	    (let ((found (assq char-or-sym char-to-word)))
	      (cond (found
		     (cdr found))
		    ((< char-or-sym 128)
		     (single-key-description char-or-sym))
		    ((and edmacro-eight-bits
			  (>= char-or-sym 128))
		     (char-to-string char-or-sym))
		    (t
		     (format "\\%o" (edmacro-int-char char-or-sym)))))))))

(defun edmacro-format-1 (keys command times togetherp)
  (let ((res "")
	(start keys)
	el)
    (while keys
      (when (not (or togetherp (eq start keys)))
	(callf concat res " "))
      (if (> times 1)
	  (setq res (concat (format "%d*" times) res)))
      (setq el (car keys))
      (callf concat res
	(cond ((listp el)
	       (let ((my ""))
		 (if (or
		      (let (cnv)
			(while el
			  (let ((found (find (car el) edmacro-modifiers
					     :key 'cdr)))
			    (callf concat my
			      (if found
				  (concat (car found) "-")
				(setq cnv (edmacro-conv (car el) nil))
				(cdr cnv))))
			  (pop el))
			(car cnv))
		      (> times 1))
		     (concat "<" my ">")
		   my)))
	      (t
	       (cdr (edmacro-conv el t)))))
      (and (cdr keys)
	   (memq (car keys) '(?- '- ?> ?^))
	   (callf concat res " "))
      (pop keys))
    (if command
	(callf concat res
	  (make-string (max (- 3 (/ (length res) tab-width)) 1) ?\t)
	  ";; "
	  (symbol-name command)
	  (if togetherp (format " * %d" (length start)))))
    res))

(defsubst edmacro-seq-equal (seq1 seq2)
  (while (and seq1 seq2
	      (equal (car seq1) (car seq2)))
    (pop seq1)
    (pop seq2))
  (not seq1))

;;; Formatting a keyboard macro as human-readable text.

(defun edmacro-format-keys (macro &optional verbose)
  ;; If we're dealing with events, convert them to symbols first;
  ;; Then, collapse them into function keys, if possible.
  (setq macro (edmacro-fkeys (edmacro-events-to-keys macro t)))
  (let ((res ""))
    (while macro
      (let (key lookup (times 1) self-insert-p)
	(loop
	 do (setq key (nconc key (list (car macro)))
		  macro (cdr macro)
		  lookup (lookup-key global-map (mapvector
						 'identity key)))
	 while (and macro lookup (not (commandp lookup))))
	;; keyboard macro
	(if (vectorp lookup)
	    (setq lookup nil))
	(if (and (eq lookup 'self-insert-command)
		 (= (length key) 1)
		 (not (memq (car key)
			    '(?\  ?\r ?\n space return linefeed tab))))
	    (while (and (< (length key) 23)
			(eq (lookup-key global-map (car macro))
			    'self-insert-command)
			(not (memq
			      (car macro)
			      '(?\  ?\r ?\n space return linefeed tab))))
	      (setq key (nconc key (list (car macro)))
		    macro (cdr macro)
		    self-insert-p t))
	  (let ((keysize (length key)))
	    (while (edmacro-seq-equal key macro)
	      (setq macro (nthcdr keysize macro))
	      (incf times))))
	(if (or self-insert-p
		(null (cdr key))
		(= times 1))
	    (callf concat res
	      (edmacro-format-1 key (if verbose lookup
				      nil)
				times self-insert-p)
	      (and macro (if verbose "\n" " ")))
	  (loop
	   repeat times
	   do
	   (callf concat res
	     (edmacro-format-1 key (if verbose lookup
				     nil)
			       1 self-insert-p)
	     (and macro (if verbose "\n" " ")))))))
    res))

(provide 'edmacro)

;;; edmacro.el ends here
