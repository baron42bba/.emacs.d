;;; DO NOT MODIFY THIS FILE
(if (featurep 'xemacs-devel-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "xemacs-devel/_pkg.el")

(package-provide 'xemacs-devel :version 1.72 :author-version "No-Upstream-Ver" :type 'single-file)

;;;***

;;;### (autoloads (checkdoc-minor-mode checkdoc-ispell-defun checkdoc-ispell-comments checkdoc-ispell-continue checkdoc-ispell-start checkdoc-ispell-message-text checkdoc-ispell-message-interactive checkdoc-ispell-interactive checkdoc-ispell-current-buffer checkdoc-ispell checkdoc-defun checkdoc-eval-defun checkdoc-message-text checkdoc-rogue-spaces checkdoc-continue checkdoc-start checkdoc-current-buffer checkdoc-eval-current-buffer checkdoc-message-interactive checkdoc-interactive checkdoc) "checkdoc" "xemacs-devel/checkdoc.el")

(autoload 'checkdoc "checkdoc" "\
Interactively check the entire buffer for style errors.
The current status of the check will be displayed in a buffer for
user to view as each check is completed." t nil)

(autoload 'checkdoc-interactive "checkdoc" "\
Interactively check the current buffer for doc string errors.
Prefix argument START-HERE will start the checking from the current
point, otherwise the check starts at the beginning of the current
buffer.  Allows navigation forward and backwards through document
errors.  Does not check for comment or space warnings.
Optional argument SHOWSTATUS indicates that we should update the
checkdoc status window instead of the usual behavior." t nil)

(autoload 'checkdoc-message-interactive "checkdoc" "\
Interactively check the current buffer for message string errors.
Prefix argument START-HERE will start the checking from the current
point, otherwise the check starts at the beginning of the current
buffer.  Allows navigation forward and backwards through document
errors.  Does not check for comment or space warnings.
Optional argument SHOWSTATUS indicates that we should update the
checkdoc status window instead of the usual behavior." t nil)

(autoload 'checkdoc-eval-current-buffer "checkdoc" "\
Evaluate and check documentation for the current buffer.
Evaluation is done first because good documentation for something that
doesn't work is just not useful.  Comments, doc strings, and rogue
spacing are all verified." t nil)

(autoload 'checkdoc-current-buffer "checkdoc" "\
Check current buffer for document, comment, error style, and rogue spaces.
With a prefix argument (in Lisp, the argument TAKE-NOTES),
store all errors found in a warnings buffer,
otherwise stop after the first error." t nil)

(autoload 'checkdoc-start "checkdoc" "\
Start scanning the current buffer for documentation string style errors.
Only documentation strings are checked.
Use `checkdoc-continue' to continue checking if an error cannot be fixed.
Prefix argument TAKE-NOTES means to collect all the warning messages into
a separate buffer." t nil)

(autoload 'checkdoc-continue "checkdoc" "\
Find the next doc string in the current buffer which has a style error.
Prefix argument TAKE-NOTES means to continue through the whole buffer and
save warnings in a separate buffer." t nil)

(autoload 'checkdoc-rogue-spaces "checkdoc" "\
Find extra spaces at the end of lines in the current file.
Prefix argument TAKE-NOTES non-nil means to save warnings in a
separate buffer.  Otherwise print a message.  This returns the error
if there is one.
Optional argument INTERACT permits more interactive fixing." t nil)

(autoload 'checkdoc-message-text "checkdoc" "\
Scan the buffer for occurrences of the error function, and verify text.
Optional argument TAKE-NOTES causes all errors to be logged." t nil)

(autoload 'checkdoc-eval-defun "checkdoc" "\
Evaluate the current form with `eval-defun' and check its documentation.
Evaluation is done first so the form will be read before the
documentation is checked.  If there is a documentation error, then the display
of what was evaluated will be overwritten by the diagnostic message." t nil)

(autoload 'checkdoc-defun "checkdoc" "\
Examine the doc string of the function or variable under point.
Call `error' if the doc string has problems.  If NO-ERROR is
non-nil, then do not call error, but call `message' instead.
If the doc string passes the test, then check the function for rogue white
space at the end of each line." t nil)

(autoload 'checkdoc-ispell "checkdoc" "\
Check the style and spelling of everything interactively.
Calls `checkdoc' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc'" t nil)

(autoload 'checkdoc-ispell-current-buffer "checkdoc" "\
Check the style and spelling of the current buffer.
Calls `checkdoc-current-buffer' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-current-buffer'" t nil)

(autoload 'checkdoc-ispell-interactive "checkdoc" "\
Check the style and spelling of the current buffer interactively.
Calls `checkdoc-interactive' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-interactive'" t nil)

(autoload 'checkdoc-ispell-message-interactive "checkdoc" "\
Check the style and spelling of message text interactively.
Calls `checkdoc-message-interactive' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-message-interactive'" t nil)

(autoload 'checkdoc-ispell-message-text "checkdoc" "\
Check the style and spelling of message text interactively.
Calls `checkdoc-message-text' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-message-text'" t nil)

(autoload 'checkdoc-ispell-start "checkdoc" "\
Check the style and spelling of the current buffer.
Calls `checkdoc-start' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-start'" t nil)

(autoload 'checkdoc-ispell-continue "checkdoc" "\
Check the style and spelling of the current buffer after point.
Calls `checkdoc-continue' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-continue'" t nil)

(autoload 'checkdoc-ispell-comments "checkdoc" "\
Check the style and spelling of the current buffer's comments.
Calls `checkdoc-comments' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-comments'" t nil)

(autoload 'checkdoc-ispell-defun "checkdoc" "\
Check the style and spelling of the current defun with Ispell.
Calls `checkdoc-defun' with spell-checking turned on.
Prefix argument TAKE-NOTES is the same as for `checkdoc-defun'" t nil)

(autoload 'checkdoc-minor-mode "checkdoc" "\
Toggle Checkdoc minor mode, a mode for checking Lisp doc strings.
With prefix ARG, turn Checkdoc minor mode on iff ARG is positive.

In Checkdoc minor mode, the usual bindings for `eval-defun' which is
bound to \\<checkdoc-minor-keymap> \\[checkdoc-eval-defun] and `checkdoc-eval-current-buffer' are overridden to include
checking of documentation strings.

\\{checkdoc-minor-keymap}" t nil)

;;;***

;;;### (autoloads (docref-setup) "docref" "xemacs-devel/docref.el")

(autoload 'docref-setup "docref" "\
Process docref cross-references in the current buffer.
See also \\(f@docref-subst)." t nil)

;;;***

;;;### (autoloads (turn-on-eldoc-mode eldoc-mode) "eldoc" "xemacs-devel/eldoc.el")

(defcustom eldoc-mode nil "*If non-nil, show the defined parameters for the elisp function near point.\n\nFor the emacs lisp function at the beginning of the sexp which point is\nwithin, show the defined parameters for the function in the echo area.\nThis information is extracted directly from the function or macro if it is\nin pure lisp.  If the emacs function is a subr, the parameters are obtained\nfrom the documentation string if possible.\n\nIf point is over a documented variable, print that variable's docstring\ninstead.\n\nThis variable is buffer-local." :type 'boolean :group 'eldoc)

(autoload 'eldoc-mode "eldoc" "\
*Enable or disable eldoc mode.
See documentation for the variable of the same name for more details.

If called interactively with no prefix argument, toggle current condition
of the mode.
If called with a positive or negative prefix argument, enable or disable
the mode, respectively." t nil)

(autoload 'turn-on-eldoc-mode "eldoc" "\
Unequivocally turn on eldoc-mode (see variable documentation)." t nil)

;;;***

;;;### (autoloads (elp-results elp-instrument-package elp-instrument-list elp-instrument-function) "elp" "xemacs-devel/elp.el")

(autoload 'elp-instrument-function "elp" "\
Instrument FUNSYM for profiling.
FUNSYM must be a symbol of a defined function." t nil)

(autoload 'elp-instrument-list "elp" "\
Instrument for profiling, all functions in `elp-function-list'.
Use optional LIST if provided instead." t nil)

(autoload 'elp-instrument-package "elp" "\
Instrument for profiling, all functions which start with PREFIX.
For example, to instrument all ELP functions, do the following:

    \\[elp-instrument-package] RET elp- RET" t nil)

(autoload 'elp-results "elp" "\
Display current profiling results.
If `elp-reset-after-results' is non-nil, then current profiling
information for all instrumented functions are reset after results are
displayed." t nil)

;;;***

;;;### (autoloads (eval-expr eval-expr-install) "eval-expr" "xemacs-devel/eval-expr.el")

(defvar eval-expr-error-message-delay 3 "\
*Amount of time, in seconds, to display in echo area before continuing.")

(defvar eval-expr-prompt "Eval: " "\
*Prompt used by eval-expr.")

(defvar eval-expr-honor-debug-on-error t "\
*If non-nil, do not trap evaluation errors.
Instead, allow errors to throw user into the debugger, provided
debug-on-error specifies that the particular error is a debuggable condition.")

(defvar eval-expr-print-level (default-value 'print-level) "\
*Like print-level, but affect results printed by `eval-expr' only.")

(defvar eval-expr-print-length (default-value 'print-length) "\
*Like print-length, but affect results printed by `eval-expr' only.")

(autoload 'eval-expr-install "eval-expr" "\
Replace standard eval-expression command with enhanced eval-expr." t nil)

(autoload 'eval-expr "eval-expr" "\
Evaluate EXPRESSION and print value in minibuffer, temp, or current buffer.
A temp output buffer is used if there is more than one line in the
evaluated result.
If invoked with a prefix arg, or second lisp argument EE::INSERT-VALUE is
non-nil, then insert final value into the current buffer at point.

Value is also consed on to front of the variable `values'." t nil)

;;;***

;;;### (autoloads (find-variable-at-point find-function-at-point find-function-on-key find-variable-other-frame find-variable-other-window find-variable find-variable-noselect find-function-other-frame find-function-other-window find-function find-function-noselect find-function-search-for-symbol) "find-func" "xemacs-devel/find-func.el")

(autoload 'find-function-search-for-symbol "find-func" "\
Search for SYMBOL in LIBRARY.
If VARIABLE-P is nil, `find-function-regexp' is used, otherwise
`find-variable-regexp' is used." nil nil)

(autoload 'find-function-noselect "find-func" "\
Return a pair (BUFFER . POINT) pointing to the definition of FUNCTION.

Finds the Emacs Lisp library containing the definition of FUNCTION
in a buffer and the point of the definition.  The buffer is
not selected.

If the file where FUNCTION is defined is not known, then it is
searched for in `find-function-source-path' if non nil, otherwise
in `load-path'." nil nil)

(autoload 'find-function "find-func" "\
Find the definition of the function near point in the current window.

Finds the Emacs Lisp library containing the definition of the function
near point (selected by `function-at-point') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where FUNCTION is defined is searched for in
`find-function-source-path', if non nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'." t nil)

(autoload 'find-function-other-window "find-func" "\
Find the definition of the function near point in another window.

See `find-function' for more details." t nil)

(autoload 'find-function-other-frame "find-func" "\
Find the definition of the function near point in another frame.

See `find-function' for more details." t nil)

(autoload 'find-variable-noselect "find-func" "\
Return a pair `(BUFFER . POINT)' pointing to the definition of SYMBOL.

Finds the Emacs Lisp library containing the definition of SYMBOL
in a buffer and the point of the definition.  The buffer is
not selected.

The library where VARIABLE is defined is searched for in FILE or
`find-function-source-path', if non nil, otherwise in `load-path'." nil nil)

(autoload 'find-variable "find-func" "\
Find the definition of the variable near point in the current window.

Finds the Emacs Lisp library containing the definition of the variable
near point (selected by `variable-at-point') in a buffer and
places point before the definition.  Point is saved in the buffer if
it is one of the current buffers.

The library where VARIABLE is defined is searched for in
`find-function-source-path', if non nil, otherwise in `load-path'.
See also `find-function-recenter-line' and `find-function-after-hook'." t nil)

(autoload 'find-variable-other-window "find-func" "\
Find the definition of the variable near point in another window.

See `find-variable' for more details." t nil)

(autoload 'find-variable-other-frame "find-func" "\
Find the definition of the variable near point in another frame.

See `find-variable' for more details." t nil)

(autoload 'find-function-on-key "find-func" "\
Find the function that KEY invokes.  KEY is a string.
Point is saved if FUNCTION is in the current buffer." t nil)

(autoload 'find-function-at-point "find-func" "\
Find directly the function at point in the other window." t nil)

(autoload 'find-variable-at-point "find-func" "\
Find directly the function at point in the other window." t nil)
(define-key ctl-x-4-map "F" 'find-function-other-window)
(define-key ctl-x-5-map "F" 'find-function-other-frame)
(define-key ctl-x-map "K" 'find-function-on-key)
(define-key ctl-x-map "V" 'find-variable)
(define-key ctl-x-4-map "V" 'find-variable-other-window)
(define-key ctl-x-5-map "V" 'find-variable-other-frame)
(define-key help-mode-map "F" 'find-function-at-point)
(define-key help-mode-map "V" 'find-variable-at-point)

;;;***

;;;### (autoloads (unhide-copyleft-region hide-copyleft-region) "hide-copyleft" "xemacs-devel/hide-copyleft.el")

(autoload 'hide-copyleft-region "hide-copyleft" "\
Make the legal drivel at the front of this file invisible.  Unhide it again
with C-u \\[hide-copyleft-region]." t nil)

(autoload 'unhide-copyleft-region "hide-copyleft" nil t nil)

;;;***

;;;### (autoloads (ielm) "ielm" "xemacs-devel/ielm.el")
(add-hook 'same-window-buffer-names "*ielm*")

(autoload 'ielm "ielm" "\
Interactively evaluate Emacs Lisp expressions.
Switches to the buffer `*ielm*', or creates it if it does not exist." t nil)

;;;***

;;;### (autoloads nil "patch-keywords" "xemacs-devel/patch-keywords.el")

(defgroup patch-review nil "Patch submission review." :group 'mail)

;;;***

;;;### (autoloads (patcher-insinuate-gnus patcher-mail-adapt-subproject patcher-mail-adapt patcher-mail patcher-mail-subproject patcher-version) "patcher" "xemacs-devel/patcher.el")

(autoload 'patcher-version "patcher" "\
Show the current version of Patcher." t nil)

(autoload 'patcher-mail-subproject "patcher" "\
Prepare a mail about a patch to apply on part of a project.
PROJECT is the name of the project (see the variables `patcher-projects'
and `patcher-subprojects').
SUBJECT is the subject of the mail.
FILES is a string listing one or more files, possibly with wild cards --
  essentially, part of a command line to be interpolated into the `diff'
  and maybe the `commit' commands issued by Patcher.

When called interactively, use a prefix (ARG) to override the value of
the diff command to use for this project.

This function is intended for one-time only subprojects.  Alternately, you
can define subprojects in the variable `patcher-subprojects' and continue
using `patcher-mail'.  If you call this function on a predefined subproject,
you will have the opportunity to modify the predefined list of files or
directories the subproject is composed of.

When you use this command instead of `patcher-mail', any commits issued
from the mail buffer (using \\<patcher-minor-mode-map>\\[patcher-commit-change]) will automatically include
the associated ChangeLogs, as well as the file(s) specified as part of
this command.

Please note that you can have multiple occurrences of a Patcher mail at
the same time, but not more than one at a time on the same project unless
you use `patcher-mail-subproject' and the sections of the project don't
overlap." t nil)

(autoload 'patcher-mail "patcher" "\
Prepare a mail about a patch to apply on a project.
PROJECT is the name of the project (see the variables `patcher-projects'
and `patcher-subprojects').
SUBJECT is the subject of the mail.

When called interactively, use a prefix (ARG) to override the value of
the diff command to use for this project.  Note that this is *not* the way
to restrict the diff to certain files.  If you want to work on a subset of
the project (e.g. some files, subdirectories etc), you have two
alternatives:

- for temporary subprojects, you can use the function
  `patcher-mail-subproject', which lets you specify the list of modified
  files / directories.
- otherwise, you can also define the subprojects in the variable
  `patcher-subprojects' and continue using this function.

Please note that you can have multiple occurrences of a Patcher mail at
the same time, but not more than one at a time on the same project unless
you use `patcher-mail-subproject' and the sections of the project don't
overlap." t nil)

(autoload 'patcher-mail-adapt "patcher" "\
Same as `patcher-mail', but for already started mails.
This function is mostly designed to adapt replies or followups probably
started with your usual MUA to Patcher.

Note two differences with `patcher-mail' however:
1. there is no SUBJECT argument to this function,
2. no prefix argument is available to override the diff command." t nil)

(autoload 'patcher-mail-adapt-subproject "patcher" "\
Same as `patcher-mail-subproject', but for already started mails.
This function is mostly designed to adapt replies or followups probably
started with your usual MUA to Patcher.

Note two differences with `patcher-mail-subproject' however:
1. there is no SUBJECT argument to this function,
2. no prefix argument is available to override the diff command." t nil)

(autoload 'patcher-insinuate-gnus "patcher" "\
This function plugs Patcher into Gnus.
It should be called from your gnusrc file." nil nil)

;;;***

;;;### (autoloads (pp-internal prettyexpand-all-sexp prettyexpand-sexp macroexpand-all-sexp macroexpand-sexp pp-plist pp-variable pp-function) "pretty-print" "xemacs-devel/pretty-print.el")

(autoload 'pp-function "pretty-print" "\
Pretty print the function definition of SYMBOL in a separate buffer" t nil)

(autoload 'pp-variable "pretty-print" "\
Pretty print the variable value of SYMBOL in a separate buffer" t nil)

(autoload 'pp-plist "pretty-print" "\
Pretty print the property list of SYMBOL in a separate buffer" t nil)

(autoload 'macroexpand-sexp "pretty-print" "\
Macro expand the sexpression following point. Pretty print expansion in a
temporary buffer. With prefix argument, replace the original
sexpression by its expansion in the current buffer." t nil)

(autoload 'macroexpand-all-sexp "pretty-print" "\
Macro expand recursively the sexpression following point. Pretty print
expansion in a temporary buffer. With prefix argument, replace the
original sexpression by its expansion in the current buffer." t nil)

(autoload 'prettyexpand-sexp "pretty-print" "\
Macro expand the sexpression following point. Pretty print expansion
in a temporary buffer. With prefix argument, replace the original
sexpression by its expansion in the current buffer.  
	However, calls to macros specified in the variable
`pp-shadow-expansion-list' are not expanded, in order to make the code
look nicer." t nil)

(autoload 'prettyexpand-all-sexp "pretty-print" "\
Macro expand recursively the sexpression following point. Pretty print
expansion in a temporary buffer. With prefix argument, replace the
original sexpression by its expansion in the current buffer.
	However, calls to macros specified in the variable
`pp-shadow-expansion-list' are not expanded, in order to make the code
look nicer." t nil)

(autoload 'pp-internal "pretty-print" "\
Pretty print FORM in in the current buffer.
Optional string TITLE is inserted before the pretty print." nil nil)

;;;***

;;;### (autoloads (compile-and-continue-profile compile-and-profile toggle-profiling profile-key-sequence profile-command profile-expression continue-profile profile save-profiling-info combine-profiling-info profile-results) "profile" "xemacs-devel/profile.el")

(autoload 'profile-results "profile" "\
Print profiling info INFO to STREAM in a pretty format.
If INFO is omitted, the current profiling info is retrieved using
 `get-profiling-info'.
If STREAM is omitted, the results will be displayed in a temp buffer
 using `with-output-to-temp-buffer'; otherwise, they will simply be
 printed into STREAM.  Use `standard-output' explicitly if you
 want standard output.
If SORT-BY is `call-count' (interactively, the prefix arg), display items
 sorted by call count rather than timing.  If `gc-usage' (interactively,
 use C-u C-u), sort by GC usage." t nil)

(autoload 'combine-profiling-info "profile" "\
Add up the profiling results accumulated during many profiling sessions.
See `profile'." nil nil)

(autoload 'save-profiling-info "profile" "\
Execute BODY, preserving the profiling info and profiling on-ness." nil 'macro)

(autoload 'profile "profile" "\
Profile FORMS and display results in a temporary buffer.
This clears out existing profiling info, turns on profiling, executes
the forms, turns off profiling, and displays the results.

If you want to accumulate the results of multiple profiling runs, you can
use `continue-profile', which does not clear out existing profiling info.

If you are looking for high-level interactive commands for profiling key
sequences, commands, and expressions, see `profile-key-sequence',
`profile-command', and `profile-expression'.

See also `toggle-profiling', which lets you easily profile any sequence
of commands.

If you need more control over what is profiled and what isn't, use the more
basic functions `clear-profiling-info', `start-profiling',
`stop-profiling', `profile-results', `get-profiling-info',
`combine-profiling-info' and `save-profiling-info'." nil 'macro)

(autoload 'continue-profile "profile" "\
Profile FORMS, combining the results with previous profile runs.
Display results in a temporary buffer.  Unlike `profile', this does
not clear out existing profile information first, and will leave profiling
on if it was already on when this macro was invoked." nil 'macro)

(autoload 'profile-expression "profile" "\
Eval EXPR, profiling the execution and displaying the results.
With prefix, combine results with results from a previous run." t nil)

(autoload 'profile-command "profile" "\
Run COMMAND, profiling the execution and displaying the results.
With prefix, combine results with results from a previous run." t nil)

(autoload 'profile-key-sequence "profile" "\
Dispatch the key sequence KEYS, profile the execution and show the results.
KEYS can be a vector of keypress events, a keypress event, or a character.
With prefix, combine results with results from a previous run." t nil)

(autoload 'toggle-profiling "profile" "\
Start profiling, or stop it and print results.
This lets you figure out where time is being spent when executing Lisp code." t nil)

(autoload 'compile-and-profile "profile" "\
Byte compile FORMS, profile the execution, and pretty-print the results." nil 'macro)

(autoload 'compile-and-continue-profile "profile" "\
Like `compile-and-profile' but combine results with previous profile runs." nil 'macro)

;;;***

;;;### (autoloads (reposition-window) "reposition" "xemacs-devel/reposition.el")

(autoload 'reposition-window "reposition" "\
Make the current definition and/or comment visible.
Further invocations move it to the top of the window or toggle the
visibility of comments that precede it.
  Point is left unchanged unless prefix ARG is supplied.
  If the definition is fully onscreen, it is moved to the top of the
window.  If it is partly offscreen, the window is scrolled to get the
definition (or as much as will fit) onscreen, unless point is in a comment
which is also partly offscreen, in which case the scrolling attempts to get
as much of the comment onscreen as possible.
  Initially `reposition-window' attempts to make both the definition and
preceding comments visible.  Further invocations toggle the visibility of
the comment lines.
  If ARG is non-nil, point may move in order to make the whole defun
visible (if only part could otherwise be made so), to make the defun line
visible (if point is in code and it could not be made so, or if only
comments, including the first comment line, are visible), or to make the
first comment line visible (if point is in a comment)." t nil)

;;;***

;;;### (autoloads (trace-function-background trace-function) "trace" "xemacs-devel/trace.el")

(defvar trace-buffer "*trace-output*" "\
*Trace output will by default go to that buffer.")

(autoload 'trace-function "trace" "\
Traces FUNCTION with trace output going to BUFFER.
For every call of FUNCTION Lisp-style trace messages that display argument
and return values will be inserted into BUFFER. This function generates the
trace advice for FUNCTION and activates it together with any other advice
there might be!! The trace BUFFER will popup whenever FUNCTION is called.
Do not use this to trace functions that switch buffers or do any other
display oriented stuff, use `trace-function-background' instead." t nil)

(autoload 'trace-function-background "trace" "\
Traces FUNCTION with trace output going quietly to BUFFER.
For every call of FUNCTION Lisp-style trace messages that display argument
and return values will be inserted into BUFFER. This function generates the
trace advice for FUNCTION and activates it together with any other advice
there might be!! Trace output will quietly go to BUFFER without changing
the window or buffer configuration at all." t nil)

;;;***

(provide 'xemacs-devel-autoloads)
