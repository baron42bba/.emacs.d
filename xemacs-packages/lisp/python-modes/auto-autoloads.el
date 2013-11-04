;;; DO NOT MODIFY THIS FILE
(if (featurep 'python-modes-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "python-modes/_pkg.el")

(package-provide 'python-modes :version 1.07 :author-version "No-Upstream-Ver" :type 'single-file)

;;;***

;;;### (autoloads (pydoc-xrefs pydoc-topics pydoc-packages pydoc-modules pydoc-keywords pydoc-help pydoc-apropos pydoc-commands) "pydoc" "python-modes/pydoc.el")

(global-set-key "è" 'pydoc-commands)

(autoload 'pydoc-commands "pydoc" "\
Display a menu of commands for interacting with the pydoc Python documentation viewer library.
With optional prefix arg INIT-FLAG, reinitializes the pydoc completion
tables which includes the list of available Python modules.
See the documentation for `pydoc-menu-help' for a description of the
available commands." t nil)

(autoload 'pydoc-apropos "pydoc" "\
Call the Python module apropos function with optional ARGUMENT and display the output.
ARGUMENT defaults to an identifier near point.
The apropos function finds matches for ARGUMENT within the first line of
module or package doc strings." t nil)

(autoload 'pydoc-help "pydoc" "\
Call the Python help function with optional ARGUMENT and display the output.
ARGUMENT defaults to an identifier near point.
ARGUMENT is resolved as a name in any present Python namespace; use
single 'quotes' to send ARGUMENT as a Python literal string." t nil)

(autoload 'pydoc-keywords "pydoc" "\
Prompt with completion for a Python keyword ARGUMENT and display its documentation." t nil)

(autoload 'pydoc-modules "pydoc" "\
Prompt with completion for a Python module/package ARGUMENT and display its documentation." t nil)

(autoload 'pydoc-packages "pydoc" "\
Prompt with completion for a Python package ARGUMENT and display its documentation." t nil)

(autoload 'pydoc-topics "pydoc" "\
Prompt with completion for a Python topic ARGUMENT and display its documentation." t nil)

(autoload 'pydoc-xrefs "pydoc" "\
Display xref at point or prompt user with completion and display chosen xref.
Xrefs are terms which follow the `pydoc-xrefs-prefix' regular expression." t nil)

;;;***

;;;### (autoloads (py-shell python-mode) "python-mode" "python-modes/python-mode.el")

(autoload 'python-mode "python-mode" "\
Major mode for editing Python files.
To submit a problem report, enter `\\[py-submit-bug-report]' from a
`python-mode' buffer.  Do `\\[py-describe-mode]' for detailed
documentation.  To see what version of `python-mode' you are running,
enter `\\[py-version]'.

This mode knows about Python indentation, tokens, comments and
continuation lines.  Paragraphs are separated by blank lines only.

COMMANDS
\\{py-mode-map}
VARIABLES

py-indent-offset		indentation increment
py-block-comment-prefix		comment string used by `comment-region'
py-python-command		shell command to invoke Python interpreter
py-temp-directory		directory used for temp files (if needed)
py-beep-if-tab-change		ring the bell if `tab-width' is changed" t nil)

(let ((modes '(("jpython" . jpython-mode) ("jython" . jpython-mode) ("python" . python-mode)))) (while modes (when (not (assoc (car modes) interpreter-mode-alist)) (push (car modes) interpreter-mode-alist)) (setq modes (cdr modes))))

(when (not (or (rassq 'python-mode auto-mode-alist) (rassq 'jpython-mode auto-mode-alist))) (push '("\\.py$" . python-mode) auto-mode-alist))

(autoload 'py-shell "python-mode" "\
Start an interactive Python interpreter in another window.
This is like Shell mode, except that Python is running in the window
instead of a shell.  See the `Interactive Shell' and `Shell Mode'
sections of the Emacs manual for details, especially for the key
bindings active in the `*Python*' buffer.

With optional \\[universal-argument], the user is prompted for the
flags to pass to the Python interpreter.  This has no effect when this
command is used to switch to an existing process, only when a new
process is started.  If you use this, you will probably want to ensure
that the current arguments are retained (they will be included in the
prompt).  This argument is ignored when this function is called
programmatically, or when running in Emacs 19.34 or older.

Note: You can toggle between using the CPython interpreter and the
JPython interpreter by hitting \\[py-toggle-shells].  This toggles
buffer local variables which control whether all your subshell
interactions happen to the `*JPython*' or `*Python*' buffers (the
latter is the name used for the CPython buffer).

Warning: Don't use an interactive Python if you change sys.ps1 or
sys.ps2 from their default values, or if you're running code that
prints `>>> ' or `... ' at the start of a line.  `python-mode' can't
distinguish your output from Python's output, and assumes that `>>> '
at the start of a line is a prompt from Python.  Similarly, the Emacs
Shell mode code assumes that both `>>> ' and `... ' at the start of a
line are Python prompts.  Bad things can happen if you fool either
mode.

Warning:  If you do any editing *in* the process buffer *while* the
buffer is accepting output from Python, do NOT attempt to `undo' the
changes.  Some of the output (nowhere near the parts you changed!) may
be lost if you do.  This appears to be an Emacs bug, an unfortunate
interaction between undo and process filters; the same problem exists in
non-Python process buffers using the default (Emacs-supplied) process
filter." t nil)

;;;***

(provide 'python-modes-autoloads)
