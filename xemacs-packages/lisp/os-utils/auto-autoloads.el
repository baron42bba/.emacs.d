;;; DO NOT MODIFY THIS FILE
(if (featurep 'os-utils-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "os-utils/_pkg.el")

(package-provide 'os-utils :version 1.37 :author-version "No-Upstream-Ver" :type 'single-file)

;;;***

;;;### (autoloads (archive-mode) "arc-mode" "os-utils/arc-mode.el")

(autoload 'archive-mode "arc-mode" "\
Major mode for viewing an archive file in a dired-like way.
You can move around using the usual cursor motion commands.
Letters no longer insert themselves.
Type `e' to pull a file out of the archive and into its own buffer;
or click mouse-2 on the file's line in the archive mode buffer.

If you edit a sub-file of this archive (as with the `e' command) and
save it, the contents of that buffer will be saved back into the
archive.

\\{archive-mode-map}" nil nil)
(add-to-list 'auto-mode-alist '("\\.\\(?:arc\\|[ejw]ar\\|zip\\|lzh\\|zoo\\)\\'" . archive-mode))

;;;***

;;;### (autoloads (background) "background" "os-utils/background.el")

(autoload 'background "background" "\
Run COMMAND in the background like csh.  
A message is displayed when the job starts and finishes.  The buffer is in
comint mode, so you can send input and signals to the job.  The process object
is returned if anyone cares.  See also comint-mode and the variables
background-show and background-select.

Optional second argument BUFFER-NAME is a buffer to insert the output into.
If omitted, a buffer name is constructed from the command run." t nil)

;;;***

;;;### (autoloads (ftelnet) "ftelnet" "os-utils/ftelnet.el")
(add-hook 'same-window-regexps "^\\*telnet-.*\\*\\(\\|<[0-9]+>\\)")

(autoload 'ftelnet "ftelnet" "\
Open a network login connection to HOST via the `telnet' program.
Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer *telnet-HOST*
\(or *telnet-HOST:PORT* if using a nonstandard port number).
If a prefix argument is given and the buffer *telnet-HOST* already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument is a string or
buffer, it names the buffer to use.

The variable `ftelnet-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `ftelnet-explicit-args' is a list of arguments to give to the
telnet program when starting.  They are added after any arguments given in
INPUT-ARGS.

If the default value of `ftelnet-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `ftelnet-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `ftelnet-directory-tracking-mode' rather than simply setting the
variable." t nil)

;;;***

;;;### (autoloads (inferior-lisp) "inf-lisp" "os-utils/inf-lisp.el")

(defvar inferior-lisp-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'" "\
*What not to save on inferior Lisp's input history.
Input matching this regexp is not saved on the input history in Inferior Lisp
mode.  Default is whitespace followed by 0 or 1 single-letter colon-keyword 
\(as in :a, :c, etc.)")

(defvar inferior-lisp-program "lisp" "\
*Program name for invoking an inferior Lisp with for Inferior Lisp mode.")

(defvar inferior-lisp-load-command "(load \"%s\")\n" "\
*Format-string for building a Lisp expression to load a file.
This format string should use `%s' to substitute a file name
and should result in a Lisp expression that will command the inferior Lisp
to load that file.  The default works acceptably on most Lisps.
The string \"(progn (load \\\"%s\\\" :verbose nil :print t) (values))\\n\"
produces cosmetically superior output for this application,
but it works only in Common Lisp.")

(defvar inferior-lisp-prompt "^[^> \n]*>+:? *" "\
Regexp to recognise prompts in the Inferior Lisp mode.
Defaults to \"^[^> \\n]*>+:? *\", which works pretty good for Lucid, kcl,
and franz.  This variable is used to initialize `comint-prompt-regexp' in the 
Inferior Lisp buffer.

More precise choices:
Lucid Common Lisp: \"^\\\\(>\\\\|\\\\(->\\\\)+\\\\) *\"
franz: \"^\\\\(->\\\\|<[0-9]*>:\\\\) *\"
kcl: \"^>+ *\"

This is a fine thing to set in your .emacs file.")

(defvar inferior-lisp-mode-hook 'nil "\
*Hook for customizing Inferior Lisp mode.")

(autoload 'inferior-lisp "inf-lisp" "\
Run an inferior Lisp process, input and output via buffer `*inferior-lisp*'.
If there is a process already running in `*inferior-lisp*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-lisp-program').  Runs the hooks from
`inferior-lisp-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)" t nil)
(add-hook 'same-window-buffer-names "*inferior-lisp*")

(define-function 'run-lisp 'inferior-lisp)

;;;***

;;;### (autoloads (jka-compr-install toggle-auto-compression jka-compr-load) "jka-compr" "os-utils/jka-compr.el")

(autoload 'jka-compr-load "jka-compr" "\
Documented as original." nil nil)
(defun auto-compression-mode (&optional arg)
"\
Toggle automatic file compression and uncompression.
With prefix argument ARG, turn auto compression on if positive, else off.
Returns the new status of auto compression (non-nil means on)."
(interactive "P")
(if (not (fboundp 'jka-compr-installed-p))
(require 'jka-compr))
(toggle-auto-compression arg t))

(autoload 'toggle-auto-compression "jka-compr" "\
Toggle automatic file compression and uncompression.
With prefix argument ARG, turn auto compression on if positive, else off.
Returns the new status of auto compression (non-nil means on).
If the argument MESSAGE is non-nil, it means to print a message
saying whether the mode is now on or off." t nil)

(autoload 'jka-compr-install "jka-compr" "\
Install jka-compr.
This adds entries to `file-name-handler-alist' and `auto-mode-alist'
and `inhibit-first-line-modes-suffixes'." nil nil)

;;;***

;;;### (autoloads (ledit-from-lisp-mode ledit-mode) "ledit" "os-utils/ledit.el")

(defconst ledit-save-files t "\
*Non-nil means Ledit should save files before transferring to Lisp.")

(defconst ledit-go-to-lisp-string "%?lisp" "\
*Shell commands to execute to resume Lisp job.")

(defconst ledit-go-to-liszt-string "%?liszt" "\
*Shell commands to execute to resume Lisp compiler job.")

(autoload 'ledit-mode "ledit" "\
\\<ledit-mode-map>Major mode for editing text and stuffing it to a Lisp job.
Like Lisp mode, plus these special commands:
  \\[ledit-save-defun]	-- record defun at or after point
	   for later transmission to Lisp job.
  \\[ledit-save-region] -- record region for later transmission to Lisp job.
  \\[ledit-go-to-lisp] -- transfer to Lisp job and transmit saved text.
  \\[ledit-go-to-liszt] -- transfer to Liszt (Lisp compiler) job
	   and transmit saved text.
\\{ledit-mode-map}
To make Lisp mode automatically change to Ledit mode,
do (setq lisp-mode-hook 'ledit-from-lisp-mode)" t nil)

(autoload 'ledit-from-lisp-mode "ledit" nil nil nil)

;;;***

;;;### (autoloads (mchat-other-frame mchat) "mchat" "os-utils/mchat.el")

(autoload 'mchat "mchat" "\
Join MChat multicast group NAME at address ADDRESS. When called
interactively, you will be prompted for the group name, and if the group is
not registered, for the corresponding multicast address. Please refer to
`mchat-predefined-groups' for a list of known groups, and
`open-multicast-group' for the syntax of ADDRESS." t nil)

(autoload 'mchat-other-frame "mchat" "\
Like `mchat', but pop up a new frame." t nil)

;;;***

;;;### (autoloads (rlogin) "rlogin" "os-utils/rlogin.el")
(add-hook 'same-window-regexps "^\\*rlogin-.*\\*\\(\\|<[0-9]+>\\)")

(autoload 'rlogin "rlogin" "\
Open a network login connection via `rlogin' with args INPUT-ARGS.
INPUT-ARGS should start with a host name; it may also contain
other arguments for `rlogin'.

Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer `*rlogin-HOST*'
\(or `*rlogin-USER@HOST*' if the remote username differs).
If a prefix argument is given and the buffer `*rlogin-HOST*' already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument BUFFER is
a string or buffer, it specifies the buffer to use.

The variable `rlogin-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `rlogin-explicit-args' is a list of arguments to give to
the rlogin when starting.  They are added after any arguments given in
INPUT-ARGS.

If the default value of `rlogin-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `rlogin-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `rlogin-directory-tracking-mode' rather than simply setting the
variable." t nil)

;;;***

;;;### (autoloads (spell-string spell-region spell-word spell-buffer) "spell" "os-utils/spell.el")

(put 'spell-filter 'risky-local-variable t)

(autoload 'spell-buffer "spell" "\
Check spelling of every word in the buffer.
For each incorrect word, you are asked for the correct spelling
and then put into a query-replace to fix some or all occurrences.
If you do not want to change a word, just give the same word
as its \"correct\" spelling; then the query replace is skipped." t nil)

(autoload 'spell-word "spell" "\
Check spelling of word at or before point.
If it is not correct, ask user for the correct spelling
and `query-replace' the entire buffer to substitute it." t nil)

(autoload 'spell-region "spell" "\
Like `spell-buffer' but applies only to region.
Used in a program, applies from START to END.
DESCRIPTION is an optional string naming the unit being checked:
for example, \"word\"." t nil)

(autoload 'spell-string "spell" "\
Check spelling of string supplied as argument." t nil)

;;;***

;;;### (autoloads (ssh) "ssh" "os-utils/ssh.el")
(add-hook 'same-window-regexps "^\\*ssh-.*\\*\\(\\|<[0-9]+>\\)")

(autoload 'ssh "ssh" "\
Open a network login connection via `ssh' with args INPUT-ARGS.
INPUT-ARGS should start with a host name; it may also contain
other arguments for `ssh'.

Input is sent line-at-a-time to the remote connection.

Communication with the remote host is recorded in a buffer `*ssh-HOST*'
\(or `*ssh-USER@HOST*' if the remote username differs).
If a prefix argument is given and the buffer `*ssh-HOST*' already exists,
a new buffer with a different connection will be made.

When called from a program, if the optional second argument BUFFER is
a string or buffer, it specifies the buffer to use.

The variable `ssh-program' contains the name of the actual program to
run.  It can be a relative or absolute path.

The variable `ssh-explicit-args' is a list of arguments to give to
the ssh when starting.  They are prepended to any arguments given in
INPUT-ARGS.

If the default value of `ssh-directory-tracking-mode' is t, then the
default directory in that buffer is set to a remote (FTP) file name to
access your home directory on the remote machine.  Occasionally this causes
an error, if you cannot access the home directory on that machine.  This
error is harmless as long as you don't try to use that default directory.

If `ssh-directory-tracking-mode' is neither t nor nil, then the default
directory is initially set up to your (local) home directory.
This is useful if the remote machine and your local machine
share the same files via NFS.  This is the default.

If you wish to change directory tracking styles during a session, use the
function `ssh-directory-tracking-mode' rather than simply setting the
variable." t nil)

;;;***

;;;### (autoloads (tar-mode) "tar-mode" "os-utils/tar-mode.el")

(autoload 'tar-mode "tar-mode" "\
Major mode for viewing a tar file as a dired-like listing of its contents.
You can move around using the usual cursor motion commands. 
Letters no longer insert themselves.
Type `e' to pull a file out of the tar file and into its own buffer;
or click mouse-2 on the file's line in the Tar mode buffer.
Type `c' to copy an entry from the tar file into another file on disk.

If you edit a sub-file of this archive (as with the `e' command) and 
save it with Control-x Control-s, the contents of that buffer will be 
saved back into the tar-file buffer; in this way you can edit a file 
inside of a tar archive without extracting it and re-archiving it.

See also: variables `tar-update-datestamp' and `tar-anal-blocksize'.
\\{tar-mode-map}" nil nil)

(defvar tar-regexp "\\.tar$" "\
The regular expression used to identify tar file names.
Note that this regular expression must not match compressed tar file
names; if it does, tar-mode will attempt to parse the compressed tar
file as an uncompressed tar file, which will generate an error.  This
is not a problem, as other modules that handle compression will
uncompress the buffer and call `tar-mode' appropriately.")

(setq auto-mode-alist (cons (cons tar-regexp 'tar-mode) auto-mode-alist))

;;;***

;;;### (autoloads (rsh telnet) "telnet" "os-utils/telnet.el")
(add-hook 'same-window-regexps "\\*telnet-.*\\*\\(\\|<[0-9]+>\\)")

(autoload 'telnet "telnet" "\
Open a network login connection to host named HOST (a string).
With a prefix argument, prompts for the port name or number as well.
Communication with HOST is recorded in a buffer `*PROGRAM-HOST*'
where PROGRAM is the telnet program being used.  This program
is controlled by the contents of the global variable `telnet-host-properties',
falling back on the value of the global variable `telnet-program'.
Normally input is edited in Emacs and sent a line at a time.
See also `\\[rsh]'." t nil)
(add-hook 'same-window-regexps "\\*rsh-[^-]*\\*\\(\\|<[0-9]*>\\)")

(autoload 'rsh "telnet" "\
Open a network login connection to host named HOST (a string).
Communication with HOST is recorded in a buffer `*rsh-HOST*'.
Normally input is edited in Emacs and sent a line at a time.
See also `\\[telnet]'." t nil)

;;;***

;;;### (autoloads (terminal-emulator) "terminal" "os-utils/terminal.el")

(autoload 'terminal-emulator "terminal" "\
Under a display-terminal emulator in BUFFER, run PROGRAM on arguments ARGS.
ARGS is a list of argument-strings.  Remaining arguments are WIDTH and HEIGHT.
BUFFER's contents are made an image of the display generated by that program,
and any input typed when BUFFER is the current Emacs buffer is sent to that
program an keyboard input.

Interactively, BUFFER defaults to \"*terminal*\" and PROGRAM and ARGS
are parsed from an input-string using your usual shell.
WIDTH and HEIGHT are determined from the size of the current window
-- WIDTH will be one less than the window's width, HEIGHT will be its height.

To switch buffers and leave the emulator, or to give commands
to the emulator itself (as opposed to the program running under it),
type Control-^.  The following character is an emulator command.
Type Control-^ twice to send it to the subprogram.
This escape character may be changed using the variable `terminal-escape-char'.

`Meta' characters may not currently be sent through the terminal emulator.

Here is a list of some of the variables which control the behaviour
of the emulator -- see their documentation for more information:
terminal-escape-char, terminal-scrolling, terminal-more-processing,
terminal-redisplay-interval.

This function calls the value of terminal-mode-hook if that exists
and is non-nil after the terminal buffer has been set up and the
subprocess started.

Presently with `termcap' only; if somebody sends us code to make this
work with `terminfo' we will try to use it." t nil)

;;;***

(provide 'os-utils-autoloads)
