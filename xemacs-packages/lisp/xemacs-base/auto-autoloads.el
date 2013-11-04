;;; DO NOT MODIFY THIS FILE
(if (featurep 'xemacs-base-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "xemacs-base/_pkg.el")

(package-provide 'xemacs-base :version 2.03 :author-version "No-Upstream-Ver" :type 'regular)

(when (fboundp 'package-suppress) (package-suppress 'xemacs-base "regexp-opt" '(fboundp 'package-suppress)) (package-suppress 'xemacs-base "easy-mmode" '(fboundp 'package-suppress)))

;;;***

;;;### (autoloads (change-log-redate patch-to-change-log change-log-merge add-log-current-defun change-log-mode add-change-log-entry-other-window add-change-log-entry find-change-log prompt-for-change-log-name add-log-convert iso8601-time-string) "add-log" "xemacs-base/add-log.el")

(autoload 'iso8601-time-string "add-log" nil nil nil)

(autoload 'add-log-convert "add-log" "\
Convert the current buffer from the old ChangeLog format to new.
The old ChangeLogs (before XEmacs 20.2) were created with attribution
lines looking like this:

Mon Feb 10 22:20:16 1997  Hrvoje Niksic  <hniksic@xemacs.org>

The same line in new format looks like this:

1997-02-10  Hrvoje Niksic  <hniksic@xemacs.org>" t nil)

(autoload 'prompt-for-change-log-name "add-log" "\
Prompt for a change log name." nil nil)

(autoload 'find-change-log "add-log" "\
Find a change log file for \\[add-change-log-entry] and return the name.

Optional arg FILE-NAME specifies the file to use.
If FILE-NAME is nil, use the value of `change-log-default-name'.
If 'change-log-default-name' is nil, behave as though it were 'ChangeLog'
\(or whatever we use on this operating system).

If 'change-log-default-name' contains a leading directory component, then
simply find it in the current directory.  Otherwise, search in the current
directory and its successive parents for a file so named.

Once a file is found, `change-log-default-name' is set locally in the
current buffer to the complete file name.
Optional arg BUFFER-FILE overrides `buffer-file-name'." nil nil)

(autoload 'add-change-log-entry "add-log" "\
Find change log file, and add an entry for today and an item for this file.
Optional arg WHOAMI (interactive prefix) non-nil means prompt for user
name and site.

Second arg FILE-NAME is file name of the change log.
If nil, use the value of `change-log-default-name'.

Third arg OTHER-WINDOW non-nil means visit in other window.

Fourth arg NEW-ENTRY non-nil means always create a new entry at the front;
never append to an existing entry.  Option `add-log-keep-changes-together'
otherwise affects whether a new entry is created.

Option `add-log-always-start-new-record' non-nil means always create a
new record, even when the last record was made on the same date and by
the same person.

The change log file can start with a copyright notice and a copying
permission notice.  The first blank line indicates the end of these
notices.

Today's date is calculated according to `change-log-time-zone-rule' if
non-nil, otherwise in local time." t nil)

(autoload 'add-change-log-entry-other-window "add-log" "\
Find change log file in other window and add entry and item.
This is just like `add-change-log-entry' except that it displays
the change log file in another window." t nil)
(define-key ctl-x-4-map "a" 'add-change-log-entry-other-window)

(autoload 'change-log-mode "add-log" "Major mode for editing change logs; like Indented Text Mode.\nPrevents numeric backups and sets `left-margin' to 8 and `fill-column' to 74.\nNew log entries are usually made with \\[add-change-log-entry] or \\[add-change-log-entry-other-window].\nEach entry behaves as a paragraph, and the entries for one day as a page.\nRuns `change-log-mode-hook'." t nil)

(defvar add-log-lisp-like-modes '(emacs-lisp-mode lisp-mode scheme-mode dsssl-mode lisp-interaction-mode) "\
*Modes that look like Lisp to `add-log-current-defun'.")

(defvar add-log-c-like-modes '(c-mode c++-mode c++-c-mode objc-mode java-mode) "\
*Modes that look like C to `add-log-current-defun'.")

(defvar add-log-tex-like-modes '(TeX-mode plain-TeX-mode LaTeX-mode plain-tex-mode latex-mode) "\
*Modes that look like TeX to `add-log-current-defun'.")

(autoload 'add-log-current-defun "add-log" "\
Return name of function definition point is in, or nil.

Understands C, Lisp, LaTeX (\"functions\" are chapters, sections, ...),
Texinfo (@node titles) and Perl.

Other modes are handled by a heuristic that looks in the 10K before
point for uppercase headings starting in the first column or
identifiers followed by `:' or `='.  See variables
`add-log-current-defun-header-regexp' and
`add-log-current-defun-function'.

Has a preference of looking backwards." nil nil)

(autoload 'change-log-merge "add-log" "\
Merge the contents of ChangeLog file OTHER-LOG with this buffer.
Both must be found in Change Log mode (since the merging depends on
the appropriate motion commands).  OTHER-LOG can be either a file name
or a buffer.

Entries are inserted in chronological order.  Both the current and
old-style time formats for entries are supported." t nil)

(autoload 'patch-to-change-log "add-log" "\
Convert the unified diff in the current buffer into a ChangeLog.
DEVDIR (queried interactively) specifies the directory the diff was
made relative to.  The ChangeLog entries are added to the appropriate
ChangeLog files (generally in the same directory as the diffed file but
possibly in a parent directory), which are left as modified Emacs buffers
but not saved out to disk.  After running this, you should go to the various
buffers and annotate the entries appropriately.

To work on a region on the current buffer, narrow to that region first.

NOTE: This function handles diff output both from `cvs diff' and just
running `diff' directly, but *ONLY* unified-format (-u) diffs. #### Someone
should fix this to handle context diffs as well.

The following keys are allowed:
- :my-name defines the name to use in ChangeLog entries
  (defaults to `(or add-log-full-name (user-full-name))'),
- :my-email defines the email address to use in ChangeLog entries
  (defaults to `(or add-log-mailing-address (user-mail-address))'),
- :dry-run prevents `patch-to-changelog' from generating the ChangeLog
   entries: ChangeLog files are only loaded (defaults to nil),
- :keep-source-files prevents `patch-to-changelog' from killing the source
  file buffers after the ChangeLog skeleton is created
  (defaults to nil),
- :extent-property instructs `patch-to-changelog' to create extents
  containing the newly created ChangeLog entries, with that property set
  (defaults to nil),
- :extent-property-value is used in conjunction with :extent-property to
  specify a value for the extent property
  (defaults to nil)." t nil)

(autoload 'change-log-redate "add-log" "\
Fix any old-style date entries in the current log file to default format." t nil)
(add-to-list 'auto-mode-alist '("[Cc]hange.?[Ll]og?\\(?:.[0-9]+\\)?\\'" . change-log-mode))
(add-to-list 'auto-mode-alist '("\\$CHANGE_LOG\\$\\.TXT" . change-log-mode))

;;;***

;;;### (autoloads (defadvice ad-add-advice) "advice" "xemacs-base/advice.el")

(defvar ad-redefinition-action 'warn "\
*Defines what to do with redefinitions during Advice de/activation.
Redefinition occurs if a previously activated function that already has an
original definition associated with it gets redefined and then de/activated.
In such a case we can either accept the current definition as the new
original definition, discard the current definition and replace it with the
old original, or keep it and raise an error.  The values `accept', `discard',
`error' or `warn' govern what will be done.  `warn' is just like `accept' but
it additionally prints a warning message.  All other values will be
interpreted as `error'.")

(defvar ad-default-compilation-action 'maybe "\
*Defines whether to compile advised definitions during activation.
A value of `always' will result in unconditional compilation, `never' will
always avoid compilation, `maybe' will compile if the byte-compiler is already
loaded, and `like-original' will compile if the original definition of the
advised function is compiled or a built-in function. Every other value will
be interpreted as `maybe'. This variable will only be considered if the 
COMPILE argument of `ad-activate' was supplied as nil.")

(autoload 'ad-add-advice "advice" "\
Adds a piece of ADVICE to FUNCTION's list of advices in CLASS.
If FUNCTION already has one or more pieces of advice of the specified
CLASS then POSITION determines where the new piece will go.  The value
of POSITION can either be `first', `last' or a number where 0 corresponds
to `first'.  Numbers outside the range will be mapped to the closest
extreme position.  If there was already a piece of ADVICE with the same
name, then the position argument will be ignored and the old advice
will be overwritten with the new one.
    If the FUNCTION was not advised already, then its advice info will be 
initialized.  Redefining a piece of advice whose name is part of the cache-id
will clear the cache." nil nil)

(autoload 'defadvice "advice" "\
Defines a piece of advice for FUNCTION (a symbol).
The syntax of `defadvice' is as follows:

  (defadvice FUNCTION (CLASS NAME [POSITION] [ARGLIST] FLAG...)
    [DOCSTRING] [INTERACTIVE-FORM]
    BODY... )

FUNCTION ::= Name of the function to be advised.
CLASS ::= `before' | `around' | `after' | `activation' | `deactivation'.
NAME ::= Non-nil symbol that names this piece of advice.
POSITION ::= `first' | `last' | NUMBER. Optional, defaults to `first',
    see also `ad-add-advice'.
ARGLIST ::= An optional argument list to be used for the advised function
    instead of the argument list of the original.  The first one found in
    before/around/after-advices will be used.
FLAG ::= `protect'|`disable'|`activate'|`compile'|`preactivate'|`freeze'.
    All flags can be specified with unambiguous initial substrings.
DOCSTRING ::= Optional documentation for this piece of advice.
INTERACTIVE-FORM ::= Optional interactive form to be used for the advised
    function.  The first one found in before/around/after-advices will be used.
BODY ::= Any s-expression.

Semantics of the various flags:
`protect': The piece of advice will be protected against non-local exits in
any code that precedes it.  If any around-advice of a function is protected
then automatically all around-advices will be protected (the complete onion).

`activate': All advice of FUNCTION will be activated immediately if
FUNCTION has been properly defined prior to this application of `defadvice'.

`compile': In conjunction with `activate' specifies that the resulting
advised function should be compiled.

`disable': The defined advice will be disabled, hence, it will not be used 
during activation until somebody enables it.

`preactivate': Preactivates the advised FUNCTION at macro-expansion/compile
time.  This generates a compiled advised definition according to the current
advice state that will be used during activation if appropriate.  Only use
this if the `defadvice' gets actually compiled.

`freeze': Expands the `defadvice' into a redefining `defun/defmacro' according
to this particular single advice.  No other advice information will be saved.
Frozen advices cannot be undone, they behave like a hard redefinition of
the advised function.  `freeze' implies `activate' and `preactivate'.  The
documentation of the advised function can be dumped onto the `DOC' file
during preloading.

Look at the file `advice.el' for comprehensive documentation." nil 'macro)

;;;***

;;;### (autoloads (all-annotations annotation-list annotations-at annotations-in-region annotation-at annotationp delete-annotation make-annotation) "annotations" "xemacs-base/annotations.el")

(defvar make-annotation-hook nil "\
*Function or functions to run immediately after creating an annotation.")

(defvar before-delete-annotation-hook nil "\
*Function or functions to run immediately before deleting an annotation.")

(defvar after-delete-annotation-hook nil "\
*Function or functions to run immediately after deleting an annotation.")

(autoload 'make-annotation "annotations" "\
Create a marginal annotation, displayed using GLYPH, at position POS.
GLYPH may be either a glyph object or a string.  Use layout policy
LAYOUT and place the annotation in buffer BUFFER.  If POS is nil, point is
used.  If LAYOUT is nil, `whitespace' is used.  If BUFFER is nil, the
current buffer is used.  If WITH-EVENT is non-nil, then when an annotation
is activated, the triggering event is passed as the second arg to the
annotation function.  If D-GLYPH is non-nil then it is used as the glyph 
that will be displayed when button1 is down.  If RIGHTP is non-nil then
the glyph will be displayed on the right side of the buffer instead of the
left." nil nil)

(autoload 'delete-annotation "annotations" "\
Remove ANNOTATION from its buffer.  This does not modify the buffer text." nil nil)

(autoload 'annotationp "annotations" "\
T if OBJECT is an annotation." nil nil)

(autoload 'annotation-at "annotations" "\
Return the first annotation at POS in BUFFER.
BUFFER defaults to the current buffer.  POS defaults to point in BUFFER." nil nil)

(autoload 'annotations-in-region "annotations" "\
Return all annotations in BUFFER between START and END inclusively." nil nil)

(autoload 'annotations-at "annotations" "\
Return a list of all annotations at POS in BUFFER.
If BUFFER is nil, the current buffer is used.  If POS is nil, point is used." nil nil)

(autoload 'annotation-list "annotations" "\
Return a list of all annotations in BUFFER.
If BUFFER is nil, the current buffer is used." nil nil)

(autoload 'all-annotations "annotations" "\
Return a list of all annotations in existence." nil nil)

;;;***

;;;### (autoloads (describe-buffer-case-table) "case-table" "xemacs-base/case-table.el")

(autoload 'describe-buffer-case-table "case-table" "\
Describe the case table of the current buffer." t nil)

;;;***

;;;### (autoloads (command-history-mode list-command-history repeat-matching-complex-command) "chistory" "xemacs-base/chistory.el")

(autoload 'repeat-matching-complex-command "chistory" "\
Edit and re-evaluate complex command with name matching PATTERN.
Matching occurrences are displayed, most recent first, until you select
a form for evaluation.  If PATTERN is empty (or nil), every form in the
command history is offered.  The form is placed in the minibuffer for
editing and the result is evaluated." t nil)

(autoload 'list-command-history "chistory" "\
List history of commands typed to minibuffer.
The number of commands listed is controlled by `list-command-history-max'.
Calls value of `list-command-history-filter' (if non-nil) on each history
element to judge if that element should be excluded from the list.

The buffer is left in Command History mode." t nil)

(autoload 'command-history-mode "chistory" "\
Major mode for examining commands from `command-history'.
The number of commands listed is controlled by `list-command-history-max'.
The command history is filtered by `list-command-history-filter' if non-nil.
Use \\<command-history-map>\\[command-history-repeat] to repeat the command on the current line.

Otherwise much like Emacs-Lisp Mode except that there is no self-insertion
and digits provide prefix arguments.  Tab does not indent.
\\{command-history-map}
Calls the value of `command-history-hook' if that is non-nil.
The Command History listing is recomputed each time this mode is invoked." t nil)

;;;***

;;;### (autoloads (comint-dynamic-list-completions comint-dynamic-complete comint-run make-comint comint-mode) "comint" "xemacs-base/comint.el")

(autoload 'comint-mode "comint" "\
Major mode for interacting with an inferior interpreter.
Interpreter name is same as buffer name, sans the asterisks.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
Setting variable `comint-eol-on-send' means jump to the end of the line
before submitting new input.

This mode is customised to create major modes such as Inferior Lisp
mode, Shell mode, etc.  This can be done by setting the hooks
`comint-input-filter-functions', `comint-input-filter', `comint-input-sender'
and `comint-get-old-input' to appropriate functions, and the variable
`comint-prompt-regexp' to the appropriate regular expression.

An input history is maintained of size `comint-input-ring-size', and
can be accessed with the commands \\[comint-next-input], \\[comint-previous-input], and \\[comint-dynamic-list-input-ring].
Input ring history expansion can be achieved with the commands
\\[comint-replace-by-expanded-history] or \\[comint-magic-space].
Input ring expansion is controlled by the variable `comint-input-autoexpand',
and addition is controlled by the variable `comint-input-ignoredups'.

Commands with no default key bindings include `send-invisible',
`comint-dynamic-complete', `comint-dynamic-list-filename-completions', and
`comint-magic-space'.

Input to, and output from, the subprocess can cause the window to scroll to
the end of the buffer.  See variables `comint-output-filter-functions',
`comint-scroll-to-bottom-on-input', and `comint-scroll-to-bottom-on-output'.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

\\{comint-mode-map}

Entry to this mode runs the hooks on `comint-mode-hook'." t nil)

(autoload 'make-comint "comint" "\
Make a comint process NAME in a buffer, running PROGRAM.
The name of the buffer is made by surrounding NAME with `*'s.
PROGRAM should be either a string denoting an executable program to create
via `start-process', or a cons pair of the form (HOST . SERVICE) denoting a TCP
connection to be opened via `open-network-stream'.  If there is already a
running process in that buffer, it is not restarted.  Optional third arg
STARTFILE is the name of a file to send the contents of to the process.

If PROGRAM is a string, any more args are arguments to PROGRAM." nil nil)

(autoload 'comint-run "comint" "\
Run PROGRAM in a comint buffer and switch to it.
The buffer name is made by surrounding the file name of PROGRAM with `*'s.
The file name is used to make a symbol name, such as `comint-sh-hook', and any
hooks on this symbol are run in the buffer.
See `make-comint' and `comint-exec'." t nil)

(autoload 'comint-dynamic-complete "comint" "\
Dynamically perform completion at point.
Calls the functions in `comint-dynamic-complete-functions' to perform
completion until a function returns non-nil, at which point completion is
assumed to have occurred." t nil)

(autoload 'comint-dynamic-list-completions "comint" "\
List in help buffer sorted COMPLETIONS.
Typing SPC flushes the help buffer." nil nil)

;;;***

;;;### (autoloads (first-error previous-error next-error compilation-minor-mode compilation-shell-minor-mode compilation-mode grep-all-files-in-current-directory-and-below grep-all-files-in-current-directory grep-find grep compile) "compile" "xemacs-base/compile.el")

(defcustom compilation-mode-hook nil "*List of hook functions run by `compilation-mode' (see `run-hooks')." :type 'hook :group 'compilation)

(defcustom compilation-window-height nil "*Number of lines in a compilation window.  If nil, use Emacs default." :type '(choice (const nil) integer) :group 'compilation)

(defcustom compilation-buffer-name-function nil "Function to compute the name of a compilation buffer.\nThe function receives one argument, the name of the major mode of the\ncompilation buffer.  It should return a string.\nnil means compute the name with `(concat \"*\" (downcase major-mode) \"*\")'." :type 'function :group 'compilation)

(defcustom compilation-finish-function nil "*Function to call when a compilation process finishes.\nIt is called with two arguments: the compilation buffer, and a string\ndescribing how the process finished." :type 'function :group 'compilation)

(defcustom compilation-search-path '(nil) "*List of directories to search for source files named in error messages.\nElements should be directory names, not file names of directories.\nnil as an element means to try the default directory." :type '(repeat (choice (const :tag "Default" nil) directory)) :group 'compilation)

(autoload 'compile "compile" "\
Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `compilation-read-command' is
non-nil; otherwise uses `compile-command'.  With prefix arg, always prompts.

To run more than one compilation at once, start one and rename the
`*compilation*' buffer to some other name with \\[rename-buffer].
Then start the next one.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name." t nil)

(autoload 'grep "compile" "\
Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a grep command." t nil)

(autoload 'grep-find "compile" "\
Run grep via find, with user-specified args COMMAND-ARGS.
Collect output in a buffer.
While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command." t nil)

(autoload 'grep-all-files-in-current-directory "compile" "\
Run `grep' in all non-annoying files in the current directory.
`Non-annoying' excludes backup files, autosave files, CVS merge files, etc.
More specifically, this is controlled by `grep-all-files-omitted-expressions'.

This function does not recurse into subdirectories.  If you want this,
use \\[grep-all-files-in-current-directory-and-below]." t nil)

(autoload 'grep-all-files-in-current-directory-and-below "compile" "\
Run `grep' in all non-annoying files in the current directory and below.
`Non-annoying' excludes backup files, autosave files, CVS merge files, etc.
More specifically, this is controlled by `grep-all-files-omitted-expressions'.

This function recurses into subdirectories.  If you do not want this,
use \\[grep-all-files-in-current-directory]." t nil)

(defvar compilation-minor-mode-map (let ((map (make-sparse-keymap))) (set-keymap-name map 'compilation-minor-mode-map) (define-key map "" 'compile-goto-error) (define-key map "" 'compile-goto-error) (define-key map "" 'kill-compilation) (define-key map "Ó" 'compilation-next-error) (define-key map "" 'compilation-previous-error) (define-key map "˚" 'compilation-previous-file) (define-key map "˝" 'compilation-next-file) map) "\
Keymap for `compilation-minor-mode'.")

(defvar compilation-shell-minor-mode-map (let ((map (make-sparse-keymap))) (define-key map "ç" 'compile-goto-error) (define-key map "é" 'compilation-next-error) (define-key map "ê" 'compilation-previous-error) (define-key map "˚" 'compilation-previous-file) (define-key map "˝" 'compilation-next-file) map) "\
Keymap for `compilation-shell-minor-mode'.")

(autoload 'compilation-mode "compile" "\
Major mode for compilation log buffers.
\\<compilation-mode-map>To visit the source for a line-numbered error,
move point to the error message line and type \\[compile-goto-error],
or click on the line with \\[compile-mouse-goto-error].
There is a menu of commands on \\[compile-popup-menu].
To kill the compilation, type \\[kill-compilation].

Runs `compilation-mode-hook' with `run-hooks' (which see)." t nil)

(defvar compilation-shell-minor-mode nil "\
Non-nil when in compilation-shell-minor-mode.
In this minor mode, all the error-parsing commands of the
Compilation major mode are available but bound to keys that don't
collide with Shell mode.")

(defvar compilation-minor-mode nil "\
Non-nil when in compilation-minor-mode.
In this minor mode, all the error-parsing commands of the
Compilation major mode are available.")

(autoload 'compilation-shell-minor-mode "compile" "\
Toggle compilation shell minor mode.
With arg, turn compilation mode on if and only if arg is positive.
See `compilation-mode'.
Turning the mode on runs the normal hook `compilation-shell-minor-mode-hook'." t nil)

(add-minor-mode 'compilation-shell-minor-mode " Shell-Compile" compilation-shell-minor-mode-map 'view-minor-mode compilation-shell-minor-mode)

(autoload 'compilation-minor-mode "compile" "\
Toggle compilation minor mode.
With arg, turn compilation mode on if and only if arg is positive.
See `compilation-mode'.
! \\{compilation-mode-map}" t nil)

(add-minor-mode 'compilation-minor-mode " CMPL" compilation-minor-mode-map 'view-minor-mode compilation-minor-mode)

(autoload 'next-error "compile" "\
Visit next compilation error message and corresponding source code.

If all the error messages parsed so far have been processed already,
the message buffer is checked for new ones.

A prefix ARGP specifies how many error messages to move;
negative means move back to previous error messages.
Just \\[universal-argument] as a prefix means reparse the error message buffer
and start at the first error.

\\[next-error] normally uses the most recently started compilation or
grep buffer.  However, it can operate on any buffer with output from
the \\[compile] and \\[grep] commands, or, more generally, on any
buffer in Compilation mode or with Compilation Minor mode enabled.  To
specify use of a particular buffer for error messages, type
\\[next-error] in that buffer.

Once \\[next-error] has chosen the buffer for error messages,
it stays with that buffer until you use it in some other buffer which
uses Compilation mode or Compilation Minor mode.

See variables `compilation-parse-errors-function' and
`compilation-error-regexp-alist' for customization ideas." t nil)
(define-key ctl-x-map "`" 'next-error)

(autoload 'previous-error "compile" "\
Visit previous compilation error message and corresponding source code.

A prefix ARGP specifies how many error messages to move;
negative means move forward to next error messages.

This operates on the output from the \\[compile] and \\[grep] commands." t nil)

(autoload 'first-error "compile" "\
Reparse the error message buffer and start at the first error.
Visit corresponding source code.
This operates on the output from the \\[compile] command." t nil)

;;;***

;;;### (autoloads (toggle-debug-on-quit toggle-debug-on-signal toggle-debug-on-error cancel-debug-on-entry debug-on-entry debug) "debug" "xemacs-base/debug.el")

(autoload 'debug "debug" "\
Enter debugger.  To return, type \\<debugger-mode-map>`\\[debugger-continue]'.
Arguments are mainly for use when this is called from the internals
of the evaluator.

You may call with no args, or you may pass nil as the first arg and
any other args you like.  In that case, the list of args after the
first will be printed into the backtrace buffer." t nil)

(autoload 'debug-on-entry "debug" "\
Request FUNCTION to invoke debugger each time it is called.
If you tell the debugger to continue, FUNCTION's execution proceeds.
This works by modifying the definition of FUNCTION,
which must be written in Lisp, not predefined.
Use \\[cancel-debug-on-entry] to cancel the effect of this command.
Redefining FUNCTION also cancels it." t nil)

(autoload 'cancel-debug-on-entry "debug" "\
Undo effect of \\[debug-on-entry] on FUNCTION.
If argument is nil or an empty string, cancel for all functions." t nil)

(autoload 'toggle-debug-on-error "debug" "\
Toggle the status of debug-on-error.
With arg, set debug-on-error iff arg is positive." t nil)

(autoload 'toggle-debug-on-signal "debug" "\
Toggle the status of debug-on-signal.
With arg, set debug-on-signal iff arg is positive." t nil)

(autoload 'toggle-debug-on-quit "debug" "\
Toggle the status of debug-on-quit.
With arg, set debug-on-quit iff arg is positive." t nil)

;;;***

;;;### (autoloads (easy-mmode-define-navigation easy-mmode-defsyntax easy-mmode-defmap easy-mmode-define-keymap easy-mmode-define-global-mode define-minor-mode) "easy-mmode" "xemacs-base/easy-mmode.el")

(defalias 'easy-mmode-define-minor-mode 'define-minor-mode)

(autoload 'define-minor-mode "easy-mmode" "\
Define a new minor mode MODE.
This function defines the associated control variable MODE, keymap MODE-map,
toggle command MODE, and hook MODE-hook.

DOC is the documentation for the mode toggle command.
Optional INIT-VALUE is the initial value of the mode's variable.
Optional LIGHTER is displayed in the modeline when the mode is on.
Optional KEYMAP is the default (defvar) keymap bound to the mode keymap.
  If it is a list, it is passed to `easy-mmode-define-keymap'
  in order to build a valid keymap.  It's generally better to use
  a separate MODE-map variable than to use this argument.
The above three arguments can be skipped if keyword arguments are
used (see below).

BODY contains code that will be executed each time the mode is (de)activated.
  It will be executed after any toggling but before running the hooks.
  Before the actual body code, you can write
  keyword arguments (alternating keywords and values).
  These following keyword arguments are supported:
:group GROUP	Custom group name to use in all generated `defcustom' forms.
:global GLOBAL	If non-nil specifies that the minor mode is not meant to be
              	buffer-local, so don't make the variable MODE buffer-local.
		By default, the mode is buffer-local.
:init-value VAL	Same as the INIT-VALUE argument.
:lighter SPEC	Same as the LIGHTER argument.
:require SYM	Same as in `defcustom'.

For backwards compatibility, these hooks are run each time the mode is
\(de)activated.  When the mode is toggled, MODE-hook is always run before the
other hook.
MODE-hook: run if the mode is toggled.
MODE-on-hook: run if the mode is activated.
MODE-off-hook: run if the mode is deactivated.
 
\(defmacro easy-mmode-define-minor-mode
  (MODE DOC &optional INIT-VALUE LIGHTER KEYMAP &rest BODY)...)

For example, you could write
  (define-minor-mode foo-mode \"If enabled, foo on you!\"
    nil \"Foo \" foo-keymap
    :require 'foo :global t :group 'inconvenience
    ...BODY CODE...)" nil 'macro)

(autoload 'easy-mmode-define-global-mode "easy-mmode" "\
Make GLOBAL-MODE out of the buffer-local minor MODE.
TURN-ON is a function that will be called with no args in every buffer
  and that should try to turn MODE on if applicable for that buffer.
KEYS is a list of CL-style keyword arguments:
:group to specify the custom group." nil 'macro)

(autoload 'easy-mmode-define-keymap "easy-mmode" "\
Return a keymap built from bindings BS.
BS must be a list of (KEY . BINDING) where
KEY and BINDINGS are suitable for `define-key'.
Optional NAME is passed to `make-sparse-keymap'.
Optional map M can be used to modify an existing map.
ARGS is a list of additional keyword arguments." nil nil)

(autoload 'easy-mmode-defmap "easy-mmode" nil nil 'macro)

(autoload 'easy-mmode-defsyntax "easy-mmode" "\
Define variable ST as a syntax-table.
CSS contains a list of syntax specifications of the form (CHAR . SYNTAX)." nil 'macro)

(autoload 'easy-mmode-define-navigation "easy-mmode" "\
Define BASE-next and BASE-prev to navigate in the buffer.
RE determines the places the commands should move point to.
NAME should describe the entities matched by RE.  It is used to build
  the docstrings of the two functions.
BASE-next also tries to make sure that the whole entry is visible by
  searching for its end (by calling ENDFUN if provided or by looking for
  the next entry) and recentering if necessary.
ENDFUN should return the end position (with or without moving point)." nil 'macro)

;;;***

;;;### (autoloads (electric-buffer-list) "ebuff-menu" "xemacs-base/ebuff-menu.el")

(autoload 'electric-buffer-list "ebuff-menu" "\
Pops up a buffer describing the set of Emacs buffers.
Vaguely like ITS lunar select buffer; combining typeoutoid buffer
listing with menuoid buffer selection.

If the very next character typed is a space then the buffer list
window disappears.  Otherwise, one may move around in the buffer list
window, marking buffers to be selected, saved or deleted.

To exit and select a new buffer, type a space when the cursor is on
the appropriate line of the buffer-list window.  Other commands are
much like those of buffer-menu-mode.

Calls value of `electric-buffer-menu-mode-hook' on entry if non-nil.

Non-null optional arg FILES-ONLY means mention only file buffers.
When called from Lisp code, FILES-ONLY may be a regular expression,
in which case only buffers whose names match that expression are listed,
or an arbitrary predicate function.

\\{electric-buffer-menu-mode-map}" t nil)

;;;***

;;;### (autoloads (electric-command-history Electric-command-history-redo-expression) "echistory" "xemacs-base/echistory.el")

(autoload 'Electric-command-history-redo-expression "echistory" "\
Edit current history line in minibuffer and execute result.
With prefix arg NOCONFIRM, execute current line as-is without editing." t nil)

(autoload 'electric-command-history "echistory" "\
\\<electric-history-map>Major mode for examining and redoing commands from `command-history'.
This pops up a window with the Command History listing.
The number of command listed is controlled by `list-command-history-max'.
The command history is filtered by `list-command-history-filter' if non-nil.
Combines typeout Command History list window with menu like selection
of an expression from the history for re-evaluation in the *original* buffer.

The history displayed is filtered by `list-command-history-filter' if non-nil.

Like Emacs-Lisp mode except that characters do not insert themselves and
Tab and Linefeed do not indent.  Instead these commands are provided:
\\{electric-history-map}

Calls the value of `electric-command-history-hook' if that is non-nil.
The Command History listing is recomputed each time this mode is invoked." t nil)

;;;***

;;;### (autoloads (format-kbd-macro read-kbd-macro edit-named-kbd-macro edit-last-kbd-macro edit-kbd-macro) "edmacro" "xemacs-base/edmacro.el")
(define-key ctl-x-map "\C-k" 'edit-kbd-macro)

(autoload 'edit-kbd-macro "edmacro" "\
Edit a keyboard macro.
At the prompt, type any key sequence which is bound to a keyboard macro.
Or, type `C-x e' or RET to edit the last keyboard macro, `C-h l' to edit
the last 100 keystrokes as a keyboard macro, or `M-x' to edit a macro by
its command name.
With a prefix argument, format the macro in a more concise way." t nil)

(autoload 'edit-last-kbd-macro "edmacro" "\
Edit the most recently defined keyboard macro." t nil)

(autoload 'edit-named-kbd-macro "edmacro" "\
Edit a keyboard macro which has been given a name by `name-last-kbd-macro'." t nil)

(autoload 'read-kbd-macro "edmacro" "\
Read the region as a keyboard macro definition.
The region is interpreted as spelled-out keystrokes, e.g., \"M-x abc RET\".
See documentation for `edmacro-mode' for details.
The resulting macro is installed as the \"current\" keyboard macro.

In Lisp, may also be called with a single STRING argument in which case
the result is returned rather than being installed as the current macro.
The result will be a vector of keystrokes." t nil)

(autoload 'format-kbd-macro "edmacro" "\
Return the keyboard macro MACRO as a human-readable string.
This string is suitable for passing to `read-kbd-macro'.
Second argument VERBOSE means to put one command per line with comments.
If VERBOSE is nil, put everything on one line." nil nil)

;;;***

;;;### (autoloads (electric-helpify with-electric-help) "ehelp" "xemacs-base/ehelp.el")

(autoload 'with-electric-help "ehelp" "\
Pop up an \"electric\" help buffer.
The arguments are THUNK &optional BUFFER NOERASE MINHEIGHT.
THUNK is a function of no arguments which is called to initialize the
contents of BUFFER.  BUFFER defaults to `*Help*'.  BUFFER will be
erased before THUNK is called unless NOERASE is non-nil.  THUNK will
be called while BUFFER is current and with `standard-output' bound to
the buffer specified by BUFFER.

If THUNK returns nil, we display BUFFER starting at the top, and
shrink the window to fit.  If THUNK returns non-nil, we don't do those things.

After THUNK has been called, this function \"electrically\" pops up a window
in which BUFFER is displayed and allows the user to scroll through that buffer
in electric-help-mode. The window's height will be at least MINHEIGHT if
this value is non-nil.

If THUNK returns nil, we display BUFFER starting at the top, and
shrink the window to fit.  If THUNK returns non-nil, we don't do those
things.

When the user exits (with `electric-help-exit', or otherwise) the help
buffer's window disappears (i.e., we use `save-window-excursion')
BUFFER is put into `default-major-mode' (or `fundamental-mode') when we exit." nil nil)

(autoload 'electric-helpify "ehelp" nil nil nil)

;;;***

;;;### (autoloads (enriched-decode enriched-encode enriched-mode) "enriched" "xemacs-base/enriched.el")

(autoload 'enriched-mode "enriched" "\
Minor mode for editing text/enriched files.
These are files with embedded formatting information in the MIME standard
text/enriched format.
Turning the mode on runs `enriched-mode-hook'.

More information about Enriched mode is available in the file 
etc/enriched.doc  in the Emacs distribution directory.

Commands:

\\<enriched-mode-map>\\{enriched-mode-map}" t nil)

(autoload 'enriched-encode "enriched" nil nil nil)

(autoload 'enriched-decode "enriched" nil nil nil)

;;;***

;;;### (autoloads (setenv substitute-env-vars) "env" "xemacs-base/env.el")

(autoload 'substitute-env-vars "env" "\
Substitute environment variables referred to in STRING.
`$FOO' where FOO is an environment variable name means to substitute
the value of that variable.  The variable name should be terminated
with a character not a letter, digit or underscore; otherwise, enclose
the entire variable name in braces.  Use `$$' to insert a single
dollar sign." nil nil)

(autoload 'setenv "env" "\
Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or is
`nil', the environment variable VARIABLE will be removed.

UNSET, if non-nil, means to remove VARIABLE from the environment.
SUBSTITUTE-ENV-VARS, if non-nil, means to substitute environment
variables in VALUE using `substitute-env-vars'.

Interactively, a prefix argument means to unset the variable.
Interactively, the current value (if any) of the variable
appears at the front of the history list when you type in the new value.

This function works by modifying `process-environment'." t nil)

;;;***

;;;### (autoloads (facemenu-get-face list-colors-display list-text-properties-at facemenu-remove-special facemenu-remove-props facemenu-set-read-only facemenu-set-intangible facemenu-set-invisible facemenu-make-much-smaller facemenu-make-much-larger facemenu-make-smaller facemenu-make-larger facemenu-set-size-default facemenu-set-face-from-menu facemenu-set-background facemenu-set-foreground facemenu-set-face) "facemenu" "xemacs-base/facemenu.el")
(autoload 'facemenu-keymap "facemenu" nil t 'keymap)
(define-key ctl-x-map "F" 'facemenu-keymap)

(defvar facemenu-menu nil "\
Facemenu top-level menu keymap.")

(defvar facemenu-keymap (let ((map (make-sparse-keymap "Set face"))) (define-key map ?o 'facemenu-set-face) map) "\
Keymap for face-changing commands.
`Facemenu-update' fills in the keymap according to the bindings
requested in `facemenu-keybindings'.")

(defalias 'facemenu-keymap facemenu-keymap)

(autoload 'facemenu-set-face "facemenu" "\
Add FACE to the region or next character typed.
It will be added to the top of the face list; any faces lower on the list that
will not show through at all will be removed.

Interactively, the face to be used is read with the minibuffer.

If the region is active and there is no prefix argument,
this command sets the region to the requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification." t nil)

(autoload 'facemenu-set-foreground "facemenu" "\
Set the foreground color of the region or next character typed.
The color is prompted for.  A face named `fg:color' is used (or created).
If the region is active, it will be set to the requested face.  If
it is inactive (even if mark-even-if-inactive is set) the next
character that is typed (via `self-insert-command') will be set to
the selected face.  Moving point or switching buffers before
typing a character cancels the request." t nil)

(autoload 'facemenu-set-background "facemenu" "\
Set the background color of the region or next character typed.
The color is prompted for.  A face named `bg:color' is used (or created).
If the region is active, it will be set to the requested face.  If
it is inactive (even if mark-even-if-inactive is set) the next
character that is typed (via `self-insert-command') will be set to
the selected face.  Moving point or switching buffers before
typing a character cancels the request." t nil)

(autoload 'facemenu-set-face-from-menu "facemenu" "\
Set the face of the region or next character typed.
This function is designed to be called from a menu; the face to use
is the menu item's name.

If the region is active and there is no prefix argument,
this command sets the region to the requested face.

Otherwise, this command specifies the face for the next character
inserted.  Moving point or switching buffers before
typing a character to insert cancels the specification." t nil)

(autoload 'facemenu-set-size-default "facemenu" nil t nil)

(autoload 'facemenu-make-larger "facemenu" nil t nil)

(autoload 'facemenu-make-smaller "facemenu" nil t nil)

(autoload 'facemenu-make-much-larger "facemenu" nil t nil)

(autoload 'facemenu-make-much-smaller "facemenu" nil t nil)

(autoload 'facemenu-set-invisible "facemenu" "\
Make the region invisible.
This sets the `invisible' text property; it can be undone with
`facemenu-remove-special'." t nil)

(autoload 'facemenu-set-intangible "facemenu" "\
Make the region intangible: disallow moving into it.
This sets the `intangible' text property; it can be undone with
`facemenu-remove-special'." t nil)

(autoload 'facemenu-set-read-only "facemenu" "\
Make the region unmodifiable.
This sets the `read-only' text property; it can be undone with
`facemenu-remove-special'." t nil)

(autoload 'facemenu-remove-props "facemenu" "\
Remove all text properties that facemenu added to region." t nil)

(autoload 'facemenu-remove-special "facemenu" "\
Remove all the \"special\" text properties from the region.
These special properties include `invisible', `intangible' and `read-only'." t nil)

(autoload 'list-text-properties-at "facemenu" "\
Pop up a buffer listing text-properties at LOCATION." t nil)

(defalias 'facemenu-read-color 'read-color)

(autoload 'list-colors-display "facemenu" "\
Display names of defined colors, and show what they look like.
If the optional argument LIST is non-nil, it should be a list of
colors to display.  Otherwise, this command computes a list
of colors that the current display can handle." t nil)

(autoload 'facemenu-get-face "facemenu" "\
Make sure FACE exists.
If not, create it and add it to the appropriate menu.  Return the symbol.

If this function creates a face named `fg:color', then it sets the
foreground to that color.  Likewise, `bg:color' means to set the
background.  In either case, if the color is undefined, no color is
set and a warning is issued." nil nil)

;;;***

;;;### (autoloads (ffap-at-mouse ffap-menu find-file-at-point ffap-next ffap-next-guess) "ffap" "xemacs-base/ffap.el")

(autoload 'ffap-next-guess "ffap" "\
Move point to next file or URL, and return it as a string.
If nothing is found, leave point at limit and return nil.
Optional BACK argument makes search backwards.
Optional LIM argument limits the search.
Only considers strings that match `ffap-next-regexp'." nil nil)

(autoload 'ffap-next "ffap" "\
Search buffer for next file or URL, and run ffap.
Optional argument BACK says to search backwards.
Optional argument WRAP says to try wrapping around if necessary.
Interactively: use a single prefix to search backwards,
double prefix to wrap forward, triple to wrap backwards.
Actual search is done by `ffap-next-guess'." t nil)

(autoload 'find-file-at-point "ffap" "\
Find FILENAME, guessing a default from text around point.
If `ffap-url-regexp' is not nil, the FILENAME may also be an URL.
With a prefix, this command behaves exactly like `ffap-file-finder'.
If `ffap-require-prefix' is set, the prefix meaning is reversed.
See also the variables `ffap-dired-wildcards', `ffap-newfile-prompt',
and the functions `ffap-file-at-point' and `ffap-url-at-point'.

See <ftp://ftp.mathcs.emory.edu/pub/mic/emacs/> for latest version." t nil)

(defalias 'ffap 'find-file-at-point)

(autoload 'ffap-menu "ffap" "\
Put up a menu of files and urls mentioned in this buffer.
Then set mark, jump to choice, and try to fetch it.  The menu is
cached in `ffap-menu-alist', and rebuilt by `ffap-menu-rescan'.
The optional RESCAN argument (a prefix, interactively) forces
a rebuild.  Searches with `ffap-menu-regexp'." t nil)

(autoload 'ffap-at-mouse "ffap" "\
Find file or url guessed from text around mouse click.
Interactively, calls `ffap-at-mouse-fallback' if no guess is found.
Return value:
  * if a guess string is found, return it (after finding it)
  * if the fallback is called, return whatever it returns
  * otherwise, nil" t nil)

;;;***

;;;### (autoloads (constrain-to-field field-end field-beginning field-string-no-properties field-string delete-field make-field) "field" "xemacs-base/field.el")

(autoload 'make-field "field" "\
Make a field with value VALUE over the range [FROM, TO) in BUFFER.
If omitted, BUFFER defaults to the current buffer.
FROM and TO may be integers or markers.
The fifth argument, FRONT-ADVANCE, if non-nil, makes the front delimiter
advance when text is inserted there.
The sixth argument, REAR-ADVANCE, if non-nil, makes the rear delimiter
advance when text is inserted there." nil nil)

(autoload 'delete-field "field" "\
Delete the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS." nil nil)

(autoload 'field-string "field" "\
Return the contents of the field surrounding POS as a string.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS." nil nil)

(autoload 'field-string-no-properties "field" "\
Return the contents of the field around POS, without text-properties.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS." nil nil)

(autoload 'field-beginning "field" "\
Return the beginning of the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the beginning of its
field, then the beginning of the *previous* field is returned.
If LIMIT is non-nil, it is a buffer position; if the beginning of the field
is before LIMIT, then LIMIT will be returned instead." nil nil)

(autoload 'field-end "field" "\
Return the end of the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the end of its field,
then the end of the *following* field is returned.
If LIMIT is non-nil, it is a buffer position; if the end of the field
is after LIMIT, then LIMIT will be returned instead." nil nil)

(autoload 'constrain-to-field "field" "\
Return the position closest to NEW-POS that is in the same field as OLD-POS.

A field is a region of text with the same `field' property.
If NEW-POS is nil, then the current point is used instead, and set to the
constrained position if that is different.

If OLD-POS is at the boundary of two fields, then the allowable
positions for NEW-POS depend on the value of the optional argument
ESCAPE-FROM-EDGE: If ESCAPE-FROM-EDGE is nil, then NEW-POS is
constrained to the field that has the same `field' char-property
as any new characters inserted at OLD-POS, whereas if ESCAPE-FROM-EDGE
is non-nil, NEW-POS is constrained to the union of the two adjacent
fields.  Additionally, if two fields are separated by another field with
the special value `boundary', then any point within this special field is
also considered to be `on the boundary'.

If the optional argument ONLY-IN-LINE is non-nil and constraining
NEW-POS would move it to a different line, NEW-POS is returned
unconstrained.  This useful for commands that move by line, like
\\[next-line] or \\[beginning-of-line], which should generally respect field
boundaries only in the case where they can still move to the right line.

If the optional argument INHIBIT-CAPTURE-PROPERTY is non-nil, and OLD-POS has
a non-nil property of that name, then any field boundaries are ignored.

Field boundaries are not noticed if `inhibit-field-text-motion' is non-nil." nil nil)

;;;***

;;;### (autoloads (Helper-help Helper-describe-bindings) "helper" "xemacs-base/helper.el")

(autoload 'Helper-describe-bindings "helper" "\
Describe local key bindings of current mode." t nil)

(autoload 'Helper-help "helper" "\
Provide help for current mode." t nil)

;;;***

;;;### (autoloads (imenu imenu-add-menubar-index imenu-add-to-menubar) "imenu" "xemacs-base/imenu.el")

(defvar imenu-generic-expression nil "\
The regex pattern to use for creating a buffer index.

If non-nil this pattern is passed to `imenu--generic-function'
to create a buffer index.

The value should be an alist with elements that look like this:
 (MENU-TITLE REGEXP INDEX)
or like this:
 (MENU-TITLE REGEXP INDEX FUNCTION ARGUMENTS...)
with zero or more ARGUMENTS.  The former format creates a simple element in
the index alist when it matches; the latter creates a special element
of the form  (NAME FUNCTION POSITION-MARKER ARGUMENTS...)
with FUNCTION and ARGUMENTS beiong copied from `imenu-generic-expression'.

MENU-TITLE is a string used as the title for the submenu or nil if the
entries are not nested.

REGEXP is a regexp that should match a construct in the buffer that is
to be displayed in the menu; i.e., function or variable definitions,
etc.  It contains a substring which is the name to appear in the
menu.  See the info section on Regexps for more information.

INDEX points to the substring in REGEXP that contains the name (of the
function, variable or type) that is to appear in the menu.

The variable is buffer-local.

The variable `imenu-case-fold-search' determines whether or not the
regexp matches are case sensitive. and `imenu-syntax-alist' can be
used to alter the syntax table for the search.

For example, see the value of `lisp-imenu-generic-expression' used by
`lisp-mode' and `emacs-lisp-mode' with `imenu-syntax-alist' set
locally to give the characters which normally have \"punctuation\"
syntax \"word\" syntax during matching.")

(make-variable-buffer-local 'imenu-generic-expression)

(make-variable-buffer-local 'imenu-extract-index-name-function)

(autoload 'imenu-add-to-menubar "imenu" "\
Add an `imenu' entry to the menu bar for the current buffer.
NAME is a string used to name the menu bar item.
See the command `imenu' for more information." t nil)

(autoload 'imenu-add-menubar-index "imenu" "\
Add an Imenu \"Index\" entry on the menu bar for the current buffer.

A trivial interface to `imenu-add-to-menubar' suitable for use in a hook." t nil)

(autoload 'imenu "imenu" "\
Jump to a place in the buffer chosen using a buffer menu or mouse menu.
INDEX-ITEM specifies the position.  See `imenu-choose-buffer-index'
for more information." t nil)

;;;***

;;;### (autoloads (apply-macro-to-region-lines kbd-macro-query insert-kbd-macro assign-last-kbd-macro-to-key name-last-kbd-macro) "macros" "xemacs-base/macros.el")

(autoload 'name-last-kbd-macro "macros" "\
Assign a name to the last keyboard macro defined.
Argument SYMBOL is the name to define.
The symbol's function definition becomes the keyboard macro string.
Such a \"function\" cannot be called from Lisp, but it is a valid
editor command." t nil)

(autoload 'assign-last-kbd-macro-to-key "macros" "\
Assign to a key sequence the last keyboard macro defined.
Argument KEY is anything acceptable to `define-key'.." t nil)

(autoload 'insert-kbd-macro "macros" "\
Insert in buffer the definition of kbd macro NAME, as Lisp code.
Optional second arg KEYS means also record the keys it is on
\(this is the prefix argument, when calling interactively).

This Lisp code will, when executed, define the kbd macro with the same
definition it has now.  If you say to record the keys, the Lisp code
will also rebind those keys to the macro.  Only global key bindings
are recorded since executing this Lisp code always makes global
bindings.

To save a kbd macro, visit a file of Lisp code such as your `~/.emacs',
use this command, and then save the file." t nil)

(autoload 'kbd-macro-query "macros" "\
Query user during kbd macro execution.
With prefix argument, enters recursive edit,
 reading keyboard commands even within a kbd macro.
 You can give different commands each time the macro executes.
Without prefix argument, asks whether to continue running the macro.
Your options are: \\<query-replace-map>
\\[act]	Finish this iteration normally and continue with the next.
\\[skip]	Skip the rest of this iteration, and start the next.
\\[exit]	Stop the macro entirely right now.
\\[recenter]	Redisplay the frame, then ask again.
\\[edit]	Enter recursive edit; ask again when you exit from that." t nil)

(autoload 'apply-macro-to-region-lines "macros" "\
For each complete line between point and mark, move to the beginning
of the line, and run the last keyboard macro.

When called from lisp, this function takes two arguments TOP and
BOTTOM, describing the current region.  TOP must be before BOTTOM.
The optional third argument MACRO specifies a keyboard macro to
execute.

This is useful for quoting or unquoting included text, adding and
removing comments, or producing tables where the entries are regular.

For example, in Usenet articles, sections of text quoted from another
author are indented, or have each line start with `>'.  To quote a
section of text, define a keyboard macro which inserts `>', put point
and mark at opposite ends of the quoted section, and use
`\\[apply-macro-to-region-lines]' to mark the entire section.

Suppose you wanted to build a keyword table in C where each entry
looked like this:

    { \"foo\", foo_data, foo_function },
    { \"bar\", bar_data, bar_function },
    { \"baz\", baz_data, baz_function },

You could enter the names in this format:

    foo
    bar
    baz

and write a macro to massage a word into a table entry:

    \\C-x (
       \\M-d { \"\\C-y\", \\C-y_data, \\C-y_function },
    \\C-x )

and then select the region of un-tablified names and use
`\\[apply-macro-to-region-lines]' to build the table from the names.
" t nil)

;;;***

;;;### (autoloads (disable-command enable-command disabled-command-hook) "novice" "xemacs-base/novice.el")

(autoload 'disabled-command-hook "novice" nil nil nil)

(autoload 'enable-command "novice" "\
Allow COMMAND to be executed without special confirmation from now on.
The user's `custom-file' is altered so that this will apply
to future sessions." t nil)

(autoload 'disable-command "novice" "\
Require special confirmation to execute COMMAND from now on.
The user's `custom-file' is altered so that this will apply
to future sessions." t nil)

;;;***

;;;### (autoloads (outline-minor-mode outline-mode) "outline" "xemacs-base/outline.el")

(defvar outline-minor-mode nil "\
Non-nil if using Outline mode as a minor mode of some other mode.")

(make-variable-buffer-local 'outline-minor-mode)

(put 'outline-minor-mode 'permanent-local t)

(add-minor-mode 'outline-minor-mode " Outl")

(autoload 'outline-mode "outline" "\
Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines. 

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end 
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

Commands:\\<outline-mode-map>
\\[outline-next-visible-heading]   outline-next-visible-heading      move by visible headings
\\[outline-previous-visible-heading]   outline-previous-visible-heading
\\[outline-forward-same-level]   outline-forward-same-level        similar but skip subheadings
\\[outline-backward-same-level]   outline-backward-same-level
\\[outline-up-heading]   outline-up-heading		    move from subheading to heading

\\[hide-body]	make all text invisible (not headings).
\\[show-all]	make everything in buffer visible.

The remaining commands are used when point is on a heading line.
They apply to some of the body or subheadings of that heading.
\\[hide-subtree]   hide-subtree	make body and subheadings invisible.
\\[show-subtree]   show-subtree	make body and subheadings visible.
\\[show-children]   show-children	make direct subheadings visible.
		 No effect on body, or subheadings 2 or more levels down.
		 With arg N, affects subheadings N levels down.
\\[hide-entry]	   make immediately following body invisible.
\\[show-entry]	   make it visible.
\\[hide-leaves]	   make body under heading and under its subheadings invisible.
		     The subheadings remain visible.
\\[show-branches]  make all subheadings at all levels visible.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of `text-mode-hook' and then of
`outline-mode-hook', if they are non-nil." t nil)

(autoload 'outline-minor-mode "outline" "\
Toggle Outline minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.
See the command `outline-mode' for more information on this mode." t nil)

;;;***

;;;### (autoloads (read-passwd) "passwd" "xemacs-base/passwd.el")

(autoload 'read-passwd "passwd" "\
Prompts for a password in the minibuffer, and returns it as a string.
If PROMPT may be a prompt string or an alist of elements 
'(prompt . default).
If optional arg CONFIRM is true, then ask the user to type the password
again to confirm that they typed it correctly.
If optional arg DEFAULT is provided, then it is a string to insert as
the default choice (it is not, of course, displayed.)

If running under X, the keyboard will be grabbed (with XGrabKeyboard())
to reduce the possibility that eavesdropping is occuring.

When reading a password, all keys self-insert, except for:
\\<read-passwd-map>
	\\[read-passwd-erase-line]	Erase the entire line.
	\\[quoted-insert]	Insert the next character literally.
	\\[delete-backward-char]	Delete the previous character.
	\\[exit-minibuffer]	Accept what you have typed.
	\\[keyboard-quit]	Abort the command.

The returned value is always a newly-created string.  No additional copies
of the password remain after this function has returned.

NOTE: unless great care is taken, the typed password will exist in plaintext
form in the running image for an arbitrarily long time.  Priveleged users may
be able to extract it from memory.  If emacs crashes, it may appear in the
resultant core file.

Some steps you can take to prevent the password from being copied around:

 - as soon as you are done with the returned string, destroy it with
   (fillarray string 0).  The same goes for any default passwords
   or password histories.

 - do not copy the string, as with concat or substring - if you do, be
   sure to keep track of and destroy all copies.

 - do not insert the password into a buffer - if you do, be sure to 
   overwrite the buffer text before killing it, as with the functions 
   `passwd-erase-buffer' or `passwd-kill-buffer'.  Note that deleting
   the text from the buffer does NOT necessarily remove the text from
   memory.

 - be careful of the undo history - if you insert the password into a 
   buffer which has undo recording turned on, the password will be 
   copied onto the undo list, and thus recoverable.

 - do not pass it as an argument to a shell command - anyone will be
   able to see it if they run `ps' at the right time.

" nil nil)

;;;***

;;;### (autoloads (pp-eval-last-sexp pp-eval-expression pp pp-to-string) "pp" "xemacs-base/pp.el")

(defalias 'pprint 'pp)

(autoload 'pp-to-string "pp" "\
Return a string containing the pretty-printed representation of OBJECT,
any Lisp object.  Quoting characters are used when needed to make output
that `read' can handle, whenever this is possible." nil nil)

(autoload 'pp "pp" "\
Output the pretty-printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see)." nil nil)

(autoload 'pp-eval-expression "pp" "\
Evaluate EXPRESSION and pretty-print value into a new display buffer.
If the pretty-printed value fits on one line, the message line is used
instead.  Value is also consed on to front of variable  values 's
value." t nil)

(autoload 'pp-eval-last-sexp "pp" "\
Run `pp-eval-expression' on sexp before point (which see).
With argument, pretty-print output into current buffer.
Ignores leading comment characters." t nil)

;;;***

;;;### (autoloads (regexp-opt-depth regexp-opt) "regexp-opt" "xemacs-base/regexp-opt.el")

(autoload 'regexp-opt "regexp-opt" "\
Return a regexp to match a string in STRINGS.
Each string should be unique in STRINGS and should not contain any regexps,
quoted or not.  If optional PAREN is non-nil, ensure that the returned regexp
is enclosed by at least one regexp grouping construct.
The returned regexp is typically more efficient than the equivalent regexp:

 (let ((open (if PAREN \"\\\\(\" \"\")) (close (if PAREN \"\\\\)\" \"\")))
   (concat open (mapconcat 'regexp-quote STRINGS \"\\\\|\") close))

If PAREN is `words', then the resulting regexp is additionally surrounded
by \\=\\< and \\>." nil nil)

(autoload 'regexp-opt-depth "regexp-opt" "\
Return the depth of REGEXP.
This means the number of regexp grouping constructs (parenthesised expressions)
in REGEXP." nil nil)

;;;***

;;;### (autoloads (make-ring ringp) "ring" "xemacs-base/ring.el")

(autoload 'ringp "ring" "\
Returns t if X is a ring; nil otherwise." nil nil)

(define-compatible-function-alias 'ring-p 'ringp)

(autoload 'make-ring "ring" "\
Make a ring that can contain SIZE elements." nil nil)

;;;***

;;;### (autoloads (shell) "shell" "xemacs-base/shell.el")

(defcustom shell-dumb-shell-regexp "cmd\\(proxy\\)?\\.exe" "Regexp to match shells that don't save their command history, and\ndon't handle the backslash as a quote character.  For shells that\nmatch this regexp, Emacs will write out the command history when the\nshell finishes, and won't remove backslashes when it unquotes shell\narguments." :type 'regexp :group 'shell)

(defcustom shell-prompt-pattern "^[^#$%>\n]*[#$%>] *" "Regexp to match prompts in the inferior shell.\nDefaults to \"^[^#$%>\\n]*[#$%>] *\", which works pretty well.\nThis variable is used to initialise `comint-prompt-regexp' in the\nshell buffer.\n\nThis variable is only used if the variable\n`comint-use-prompt-regexp-instead-of-fields' is non-nil.\n\nThe pattern should probably not match more than one line.  If it does,\nShell mode may become confused trying to distinguish prompt from input\non lines which don't start with a prompt.\n\nThis is a fine thing to set in your `.emacs' file." :type 'regexp :group 'shell)

(autoload 'shell "shell" "\
Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL,
 or if there is no SHELL then from variable `shell-file-name',
 or, if all of the above are nil, \"/bin/sh\" is used.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)" t nil)
(add-hook 'same-window-buffer-names "*shell*")

;;;***

;;;### (autoloads (skeleton-pair-insert-maybe skeleton-insert skeleton-proxy skeleton-proxy-new define-skeleton) "skeleton" "xemacs-base/skeleton.el")

(defvar skeleton-filter 'identity "\
Function for transforming a skeleton proxy's aliases' variable value.")

(autoload 'define-skeleton "skeleton" "\
Define a user-configurable COMMAND that enters a statement skeleton.
DOCUMENTATION is that of the command, while the variable of the same name,
which contains the skeleton, has a documentation to that effect.
INTERACTOR and ELEMENT ... are as defined under `skeleton-insert'." nil 'macro)

(autoload 'skeleton-proxy-new "skeleton" "\
Insert skeleton defined by variable of same name (see `skeleton-insert').
Prefix ARG allows wrapping around words or regions (see `skeleton-insert').
If no ARG was given, but the region is visible, ARG defaults to -1 depending
on `skeleton-autowrap'.  An ARG of  M-0  will prevent this just for once.
This command can also be an abbrev expansion (3rd and 4th columns in
\\[edit-abbrevs]  buffer: \"\"  command-name).
 
When called as a function, optional first argument STR may also be a string
which will be the value of `str' whereas the skeleton's interactor is then
ignored." t nil)

(autoload 'skeleton-proxy "skeleton" "\
Insert skeleton defined by variable of same name (see `skeleton-insert').
Prefix ARG allows wrapping around words or regions (see `skeleton-insert').
If no ARG was given, but the region is visible, ARG defaults to -1 depending
on `skeleton-autowrap'.  An ARG of  M-0  will prevent this just for once.
This command can also be an abbrev expansion (3rd and 4th columns in
\\[edit-abbrevs]  buffer: \"\"  command-name).

When called as a function, optional first argument STR may also be a string
which will be the value of `str' whereas the skeleton's interactor is then
ignored." t nil)

(autoload 'skeleton-insert "skeleton" "\
Insert the complex statement skeleton SKELETON describes very concisely.

With optional third REGIONS wrap first interesting point (`_') in skeleton
around next REGIONS words, if REGIONS is positive.  If REGIONS is negative,
wrap REGIONS preceding interregions into first REGIONS interesting positions
\(successive `_'s) in skeleton.  An interregion is the stretch of text between
two contiguous marked points.  If you marked A B C [] (where [] is the cursor)
in alphabetical order, the 3 interregions are simply the last 3 regions.  But
if you marked B A [] C, the interregions are B-A, A-[], []-C.

Optional fourth STR is the value for the variable `str' within the skeleton.
When this is non-`nil' the interactor gets ignored, and this should be a valid
skeleton element.

SKELETON is made up as (INTERACTOR ELEMENT ...).  INTERACTOR may be nil if
not needed, a prompt-string or an expression for complex read functions.

If ELEMENT is a string or a character it gets inserted (see also
`skeleton-transformation').  Other possibilities are:

	\\n	go to next line and indent according to mode
	_	interesting point, interregion here, point after termination
	>	indent line (or interregion if > _) according to major mode
	&	do next ELEMENT if previous moved point
	|	do next ELEMENT if previous didn't move point
	-num	delete num preceding characters (see `skeleton-untabify')
	resume:	skipped, continue here if quit is signaled
	nil	skipped

Further elements can be defined via `skeleton-further-elements'.  ELEMENT may
itself be a SKELETON with an INTERACTOR.  The user is prompted repeatedly for
different inputs.  The SKELETON is processed as often as the user enters a
non-empty string.  \\[keyboard-quit] terminates skeleton insertion, but
continues after `resume:' and positions at `_' if any.  If INTERACTOR in such
a subskeleton is a prompt-string which contains a \".. %s ..\" it is
formatted with `skeleton-subprompt'.  Such an INTERACTOR may also a list of
strings with the subskeleton being repeated once for each string.

Quoted Lisp expressions are evaluated evaluated for their side-effect.
Other Lisp expressions are evaluated and the value treated as above.
Note that expressions may not return `t' since this implies an
endless loop.  Modes can define other symbols by locally setting them
to any valid skeleton element.  The following local variables are
available:

	str	first time: read a string according to INTERACTOR
		then: insert previously read string once more
	help	help-form during interaction with the user or `nil'
	input	initial input (string or cons with index) while reading str
	v1, v2	local variables for memorizing anything you want

When done with skeleton, but before going back to `_'-point call
`skeleton-end-hook' if that is non-`nil'." nil nil)

(autoload 'skeleton-pair-insert-maybe "skeleton" "\
Insert the character you type ARG times.

With no ARG, if `skeleton-pair' is non-nil, pairing can occur.  If the region
is visible the pair is wrapped around it depending on `skeleton-autowrap'.
Else, if `skeleton-pair-on-word' is non-nil or we are not before or inside a
word, and if `skeleton-pair-filter' returns nil, pairing is performed.

If a match is found in `skeleton-pair-alist', that is inserted, else
the defaults are used.  These are (), [], {}, <> and `' for the
symmetrical ones, and the same character twice for the others." t nil)

;;;***

;;;### (autoloads (reverse-region sort-columns sort-regexp-fields-numerically sort-regexp-fields sort-fields sort-float-fields sort-numeric-fields sort-pages sort-paragraphs sort-lines sort-subr) "sort" "xemacs-base/sort.el")

(autoload 'sort-subr "sort" "\
General text sorting routine to divide buffer into records and sort them.

We divide the accessible portion of the buffer into disjoint pieces
called sort records.  A portion of each sort record (perhaps all of
it) is designated as the sort key.  The records are rearranged in the
buffer in order by their sort keys.  The records may or may not be
contiguous.

Usually the records are rearranged in order of ascending sort key.
If REVERSE is non-nil, they are rearranged in order of descending sort key.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

The next four arguments are functions to be called to move point
across a sort record.  They will be called many times from within sort-subr.

NEXTRECFUN is called with point at the end of the previous record.
It moves point to the start of the next record.
It should move point to the end of the buffer if there are no more records.
The first record is assumed to start at the position of point when sort-subr
is called.

ENDRECFUN is called with point within the record.
It should move point to the end of the record.

STARTKEYFUN moves from the start of the record to the start of the key.
It may return either a non-nil value to be used as the key, or
else the key is the substring between the values of point after
STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
starts at the beginning of the record.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN.

COMPAREFUN compares the two keys.  It is called with two strings and
should return true if the first is \"less\" than the second, just as
for `sort'.  If nil or omitted, the default function accepts keys that
are numbers (compared numerically) or strings (compared lexicographically)." nil nil)

(autoload 'sort-lines "sort" "\
Sort lines in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order." t nil)

(autoload 'sort-paragraphs "sort" "\
Sort paragraphs in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order." t nil)

(autoload 'sort-pages "sort" "\
Sort pages in region alphabetically; argument means descending order.
Called from a program, there are three arguments:
REVERSE (non-nil means reverse order), BEG and END (region to sort).
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order." t nil)

(autoload 'sort-numeric-fields "sort" "\
Sort lines in region numerically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
Specified field must contain a number in each line of the region,
which may begin with \"0x\" or \"0\" for hexadecimal and octal values.
Otherwise, the number is interpreted according to sort-numeric-base.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.
If you want to sort floating-point numbers, try `sort-float-fields'." t nil)

(autoload 'sort-float-fields "sort" "\
Sort lines in region numerically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.  Specified field
must contain a floating point number in each line of the region.  With a
negative arg, sorts by the ARGth field counted from the right.  Called from a
program, there are three arguments: FIELD, BEG and END.  BEG and END specify
region to sort." t nil)

(autoload 'sort-fields "sort" "\
Sort lines in region lexicographically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order." t nil)

(autoload 'sort-regexp-fields "sort" "\
Sort the region as specified by RECORD-REGEXP and KEY.
RECORD-REGEXP specifies the textual units which should be sorted.
  For example, to sort lines RECORD-REGEXP would be \"^.*$\"
KEY specifies the part of each record (ie each match for RECORD-REGEXP)
  is to be used for sorting.
  If it is \"\\\\digit\" then the digit'th \"\\\\(...\\\\)\" match field from
  RECORD-REGEXP is used.
  If it is \"\\\\&\" then the whole record is used.
  Otherwise, it is a regular-expression for which to search within the record.
If a match for KEY is not found within a record then that record is ignored.

With a negative prefix arg sorts in reverse order.

The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

COMPAREFUN, if specified, should be a function of two arguments; it
will be passed the keys (as strings), and should return true if the
first is \"less\" than the second.  Otherwise the keys will be
compared lexicographically.

For example: to sort lines in the region by the first word on each line
 starting with the letter \"f\",
 RECORD-REGEXP would be \"^.*$\" and KEY would be \"\\\\=\\<f\\\\w*\\\\>\"" t nil)

(autoload 'sort-regexp-fields-numerically "sort" "\
Sort the region numerically as specified by RECORD-REGEXP and KEY.
RECORD-REGEXP specifies the textual units which should be sorted.
  For example, to sort lines RECORD-REGEXP would be \"^.*$\"
KEY specifies the part of each record (ie each match for RECORD-REGEXP)
  is to be used for sorting.
  If it is \"\\\\digit\" then the digit'th \"\\\\(...\\\\)\" match field from
  RECORD-REGEXP is used.
  If it is \"\\\\&\" then the whole record is used.
  Otherwise, it is a regular-expression for which to search within the record.
If a match for KEY is not found within a record then that record is ignored.

With a negative prefix arg sorts in reverse order.

The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

For example: to sort lines in the region by the first word on each line
 starting with the letter \"f\",
 RECORD-REGEXP would be \"^.*$\" and KEY would be \"\\\\=\\<f\\\\w*\\\\>\"" t nil)

(autoload 'sort-columns "sort" "\
Sort lines in region alphabetically by a certain range of columns.
For the purpose of this command, the region BEG...END includes
the entire line that point is in and the entire line the mark is in.
The column positions of point and mark bound the range of columns to sort on.
A prefix argument means sort into REVERSE order.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

Note that `sort-columns' rejects text that contains tabs,
because tabs could be split across the specified columns
and it doesn't know how to handle that.  Also, when possible,
it uses the `sort' utility program, which doesn't understand tabs.
Use \\[untabify] to convert tabs to spaces before sorting." t nil)

(autoload 'reverse-region "sort" "\
Reverse the order of lines in a region.
From a program takes two point or marker arguments, BEG and END." t nil)

;;;***

;;;### (autoloads (time-stamp-toggle-active time-stamp) "time-stamp" "xemacs-base/time-stamp.el")

(autoload 'time-stamp "time-stamp" "\
Update the time stamp string in the buffer.
If you put a time stamp template anywhere in the first 8 lines of a file,
it can be updated every time you save the file.  See the top of
`time-stamp.el' for a sample.  The template looks like one of the following:
    Time-stamp: <>
    Time-stamp: \" \"
The time stamp is written between the brackets or quotes, resulting in
    Time-stamp: <95/01/18 10:20:51 gildea>
Only does its thing if the variable  time-stamp-active  is non-nil.
Typically used on  write-file-hooks  for automatic time-stamping.
The format of the time stamp is determined by the variable  time-stamp-format.
The variables time-stamp-line-limit, time-stamp-start, and time-stamp-end
control finding the template." t nil)

(autoload 'time-stamp-toggle-active "time-stamp" "\
Toggle time-stamp-active, setting whether \\[time-stamp] updates a buffer.
With arg, turn time stamping on if and only if arg is positive." t nil)

;;;***

;;;### (autoloads (y-or-n-p-with-timeout with-timeout run-with-idle-timer run-with-timer run-at-time) "timer-funcs" "xemacs-base/timer-funcs.el")

(autoload 'run-at-time "timer-funcs" "\
Perform an action at time TIME.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
TIME should be a string recognized by `timer-parse-time', like \"11:23pm\",
nil meaning now, a number of seconds from now, a value from `current-time',
or t (with non-nil REPEAT) meaning the next integral multiple of REPEAT.
REPEAT may be an integer or floating point number.
The action is to call FUNCTION with arguments ARGS.

This function returns an itimer object which you can use in `delete-itimer'." t nil)

(autoload 'run-with-timer "timer-funcs" "\
Perform an action after a delay of SECS seconds.
Repeat the action every REPEAT seconds, if REPEAT is non-nil.
SECS and REPEAT may be integers or floating point numbers.
The action is to call FUNCTION with arguments ARGS.

This function returns an itimer object which you can use in `delete-itimer'." t nil)

(autoload 'run-with-idle-timer "timer-funcs" "\
Perform an action the next time XEmacs is idle for SECS seconds.
The action is to call FUNCTION with arguments ARGS.
SECS may be an integer or a floating point number.

If REPEAT is non-nil, do the action each time XEmacs has been idle for
exactly SECS seconds (that is, only once for each time XEmacs becomes idle).

This function returns an itimer object which you can use in `delete-itimer'." t nil)
(put 'with-timeout 'lisp-indent-function 1)

(autoload 'with-timeout "timer-funcs" "\
Run BODY, but if it doesn't finish in SECONDS seconds, give up.
If we give up, we run the TIMEOUT-FORMS and return the value of the last one.
The call should look like:
 (with-timeout (SECONDS TIMEOUT-FORMS...) BODY...)
The timeout is checked whenever XEmacs waits for some kind of external
event (such as keyboard input, input from subprocesses, or a certain time);
if the program loops without waiting in any way, the timeout will not
be detected." nil 'macro)

(autoload 'y-or-n-p-with-timeout "timer-funcs" "\
Like (y-or-n-p PROMPT), with a timeout.
If the user does not answer after SECONDS seconds, return DEFAULT-VALUE." nil nil)

;;;***

;;;### (autoloads nil "timezone" "xemacs-base/timezone.el")

(define-error 'invalid-date "Invalid date string")

;;;***

;;;### (autoloads (tq-create) "tq" "xemacs-base/tq.el")

(autoload 'tq-create "tq" "\
Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine." nil nil)

;;;***

;;;### (autoloads (xbm-button-create) "xbm-button" "xemacs-base/xbm-button.el")

(autoload 'xbm-button-create "xbm-button" "\
Returns a list of XBM image instantiators for a button displaying TEXT.
The list is of the form
   (UP DOWN DISABLED)
where UP, DOWN, and DISABLED are the up, down and disabled image
instantiators for the button.

BORDER-THICKNESS specifies how many pixels should be used for the
borders on the edges of the buttons.  It should be a positive integer,
or 0 to mean no border." nil nil)

;;;***

;;;### (autoloads (xpm-button-create) "xpm-button" "xemacs-base/xpm-button.el")

(autoload 'xpm-button-create "xpm-button" "\
Returns a list of XPM image instantiators for a button displaying TEXT.
The list is of the form
   (UP DOWN DISABLED)
where UP, DOWN, and DISABLED are the up, down and disabled image
instantiators for the button.

SHADOW-THICKNESS specifies how many pixels should be used for the
shadows on the edges of the buttons.  It should be a positive integer,
or 0 to mean no shadows on the edges.
FG-COLOR is the color used to display the text.  It should be a string.
BG-COLOR is the background color the text will be displayed upon.
It should be a string." nil nil)

;;;***

(provide 'xemacs-base-autoloads)
