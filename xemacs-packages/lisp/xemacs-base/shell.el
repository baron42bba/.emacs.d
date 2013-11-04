;;; shell.el --- specialized comint.el for running the shell

;; Copyright (C) 1988, 93, 94, 95, 96, 1997, 2000 Free Software Foundation, Inc.

;; Author: Olin Shivers <shivers@cs.cmu.edu>
;;	Simon Marshall <simon@gnu.org>
;; Maintainer: Pete Ware <ware@cis.ohio-state.edu>
;; Keywords: processes shell terminal csh tcsh bash sh

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

;;; Synched up with: FSF 21.3

;;; Commentary:

;; Please send me bug reports, bug fixes, and extensions, so that I can
;; merge them into the master source.
;;     - Olin Shivers (shivers@cs.cmu.edu)
;;     - Simon Marshall (simon@gnu.org)

;; This file defines a shell-in-a-buffer package (shell mode) built on
;; top of comint mode.  This is actually cmushell with things renamed
;; to replace its counterpart in Emacs 18.  cmushell is more
;; featureful, robust, and uniform than the Emacs 18 version.

;; Since this mode is built on top of the general command-interpreter-in-
;; a-buffer mode (comint mode), it shares a common base functionality,
;; and a common set of bindings, with all modes derived from comint mode.
;; This makes these modes easier to use.

;; For documentation on the functionality provided by comint mode, and
;; the hooks available for customising it, see the file comint.el.
;; For further information on shell mode, see the comments below.

;; Needs fixin:
;; When sending text from a source file to a subprocess, the process-mark can
;; move off the window, so you can lose sight of the process interactions.
;; Maybe I should ensure the process mark is in the window when I send
;; text to the process? Switch selectable?

;; YOUR .EMACS FILE
;;=============================================================================
;; Some suggestions for your .emacs file.
;;
;; ;; Define M-# to run some strange command:
;; (eval-after-load "shell"
;;  '(define-key shell-mode-map "\M-#" 'shells-dynamic-spell))

;; Brief Command Documentation:
;;============================================================================
;; Comint Mode Commands: (common to shell and all comint-derived modes)
;;
;; m-p	    comint-previous-input    	    Cycle backwards in input history
;; m-n	    comint-next-input  	    	    Cycle forwards
;; m-r     comint-previous-matching-input  Previous input matching a regexp
;; m-s     comint-next-matching-input      Next input that matches
;; m-c-l   comint-show-output		    Show last batch of process output
;; return  comint-send-input
;; c-d	    comint-delchar-or-maybe-eof	    Delete char unless at end of buff.
;; c-c c-a comint-bol                      Beginning of line; skip prompt
;; c-c c-u comint-kill-input	    	    ^u
;; c-c c-w backward-kill-word    	    ^w
;; c-c c-c comint-interrupt-subjob 	    ^c
;; c-c c-z comint-stop-subjob	    	    ^z
;; c-c c-\ comint-quit-subjob	    	    ^\
;; c-c c-o comint-kill-output		    Delete last batch of process output
;; c-c c-r comint-show-output		    Show last batch of process output
;; c-c c-h comint-dynamic-list-input-ring  List input history
;;         send-invisible                  Read line w/o echo & send to proc
;;         comint-continue-subjob	    Useful if you accidentally suspend
;;					        top-level job
;; comint-mode-hook is the comint mode hook.

;; Shell Mode Commands:
;;         shell			Fires up the shell process
;; tab     comint-dynamic-complete	Complete filename/command/history
;; m-?     comint-dynamic-list-filename-completions
;;					List completions in help buffer
;; m-c-f   shell-forward-command	Forward a shell command
;; m-c-b   shell-backward-command	Backward a shell command
;; 	   dirs			        Resync the buffer's dir stack
;; 	   dirtrack-toggle		Turn dir tracking on/off
;;         comint-strip-ctrl-m		Remove trailing ^Ms from output
;;
;; The shell mode hook is shell-mode-hook
;; comint-prompt-regexp is initialised to shell-prompt-pattern, for backwards
;; compatibility.

;; Read the rest of this file for more information.

;;; Code:

(require 'comint)

;;; Customization and Buffer Variables

(defgroup shell nil
  "Running shell from within Emacs buffers"
  :group 'processes
  :group 'unix)

(defgroup shell-directories nil
  "Directory support in shell mode"
  :group 'shell)

(defgroup shell-faces nil
  "Faces in shell buffers"
  :group 'shell)

;;;###autoload
(defcustom shell-dumb-shell-regexp "cmd\\(proxy\\)?\\.exe"
  "Regexp to match shells that don't save their command history, and
don't handle the backslash as a quote character.  For shells that
match this regexp, Emacs will write out the command history when the
shell finishes, and won't remove backslashes when it unquotes shell
arguments."
  :type 'regexp
  :group 'shell)

;;;###autoload
(defcustom shell-prompt-pattern "^[^#$%>\n]*[#$%>] *"
  "Regexp to match prompts in the inferior shell.
Defaults to \"^[^#$%>\\n]*[#$%>] *\", which works pretty well.
This variable is used to initialise `comint-prompt-regexp' in the
shell buffer.

This variable is only used if the variable
`comint-use-prompt-regexp-instead-of-fields' is non-nil.

The pattern should probably not match more than one line.  If it does,
Shell mode may become confused trying to distinguish prompt from input
on lines which don't start with a prompt.

This is a fine thing to set in your `.emacs' file."
  :type 'regexp
  :group 'shell)

(defcustom shell-completion-fignore nil
  "*List of suffixes to be disregarded during file/command completion.
This variable is used to initialize `comint-completion-fignore' in the shell
buffer.  The default is nil, for compatibility with most shells.
Some people like (\"~\" \"#\" \"%\").

This is a fine thing to set in your `.emacs' file."
  :type '(repeat (string :tag "Suffix"))
  :group 'shell)

;jwz: turned this off; it's way too broken.
(defvar shell-delimiter-argument-list nil ;'(?\| ?& ?< ?> ?\( ?\) ?\;
  "List of characters to recognise as separate arguments.
This variable is used to initialize `comint-delimiter-argument-list' in the
shell buffer.  The value may depend on the operating system or shell.

This is a fine thing to set in your `.emacs' file.")

(defvar shell-file-name-chars
  (if (memq system-type '(ms-dos windows-nt cygwin))
      "~/A-Za-z0-9_^$!#%&{}@`'.,:()-"
    "~/A-Za-z0-9+@:_.$#%,={}-")
  "String of characters valid in a file name.
This variable is used to initialize `comint-file-name-chars' in the
shell buffer.  The value may depend on the operating system or shell.

This is a fine thing to set in your `.emacs' file.")

(defvar shell-file-name-quote-list
  (if (memq system-type '(ms-dos windows-nt))
      nil
    (append shell-delimiter-argument-list '(?\  ?\* ?\! ?\" ?\' ?\` ?\#)))
  "List of characters to quote when in a file name.
This variable is used to initialize `comint-file-name-quote-list' in the
shell buffer.  The value may depend on the operating system or shell.

This is a fine thing to set in your `.emacs' file.")

(defvar shell-dynamic-complete-functions
  '(comint-replace-by-expanded-history
    shell-dynamic-complete-environment-variable
    shell-dynamic-complete-command
    shell-replace-by-expanded-directory
    comint-dynamic-complete-filename)
  "List of functions called to perform completion.
This variable is used to initialise `comint-dynamic-complete-functions' in the
shell buffer.

This is a fine thing to set in your `.emacs' file.")

(defcustom shell-command-regexp "[^;&|\n]+"
  "*Regexp to match a single command within a pipeline.
This is used for directory tracking and does not do a perfect job."
  :type 'regexp
  :group 'shell)

(defcustom shell-command-separator-regexp "[;&|\n \t]*"
  "*Regexp to match what separates commands within a pipeline.
This is used for directory tracking and does not do a perfect job."
:type 'regexp
:group 'shell)

(defcustom shell-completion-execonly t
  "*If non-nil, use executable files only for completion candidates.
This mirrors the optional behavior of tcsh.

Detecting executability of files may slow command completion considerably."
  :type 'boolean
  :group 'shell)

(defcustom shell-multiple-shells nil
  "*If non-nil, each time shell mode is invoked, a new shell is made"
  :type 'boolean
  :group 'shell)

(defcustom shell-popd-regexp "popd"
  "*Regexp to match subshell commands equivalent to popd."
  :type 'regexp
  :group 'shell-directories)

(defcustom shell-pushd-regexp "pushd"
  "*Regexp to match subshell commands equivalent to pushd."
  :type 'regexp
  :group 'shell-directories)

(defcustom shell-pushd-tohome nil
  "*If non-nil, make pushd with no arg behave as \"pushd ~\" (like cd).
This mirrors the optional behavior of tcsh."
  :type 'boolean
  :group 'shell-directories)

(defcustom shell-pushd-dextract nil
  "*If non-nil, make \"pushd +n\" pop the nth dir to the stack top.
This mirrors the optional behavior of tcsh."
  :type 'boolean
  :group 'shell-directories)

(defcustom shell-pushd-dunique nil
  "*If non-nil, make pushd only add unique directories to the stack.
This mirrors the optional behavior of tcsh."
  :type 'boolean
  :group 'shell-directories)

(defcustom shell-cd-regexp "cd"
  "*Regexp to match subshell commands equivalent to cd."
  :type 'regexp
  :group 'shell-directories)

(defcustom shell-chdrive-regexp
  (if (memq system-type '(ms-dos windows-nt))
      ; NetWare allows the five chars between upper and lower alphabetics.
      "[]a-zA-Z^_`\\[\\\\]:"
    nil)
  "*If non-nil, is regexp used to track drive changes."
  :type '(choice regexp
		 (const nil))
  :group 'shell-directories)

(defcustom shell-dirtrack-verbose t
  "*If non-nil, show the directory stack following directory change.
This is effective only if directory tracking is enabled."
  :type 'boolean
  :group 'shell-directories)

(defcustom shell-dirtrack-other-regexp nil
  "*Regexp to match commands for which we call `shell-dirtrack-process-other-func'."
  :type '(choice regexp
		 (const nil))
  :group 'shell-directories)

;; @todo make this a full blown hook?  ?A hook which stops when a func
;; returns t? Or nil?
(defcustom shell-dirtrack-process-other-func nil
  "*Function to allow external processing of directory changing commands
which match `shell-dirtrack-other-regexp'.  This allows users to process
arbitrary directory changing commands.  This function takes two arguments:
  CMD  - the first word (cmd part) of the current shell command line.
  ARG1 - the rest of the current shell command line.
See `shell-process-popd', etc., for an example of things for which this
function might be used.  The motivation for this was to provide a
pushd-like command which expands ARG1 from an abbrev-table."
  :type 'function
  :group 'shell-directories)

(defcustom explicit-shell-file-name nil
  "*If non-nil, is file name to use for explicitly requested inferior shell."
  :type '(choice (const :tag "None" nil) file)
  :group 'shell)

(defcustom explicit-ash-args '("-i")
  "*List of arguments to pass to \"ash\" on startup in `shell-mode'."
  :type '(repeat (string :tag "Argument"))
  :group 'shell)

(defcustom explicit-bash-args
  ;; Tell bash not to use readline, except for bash 1.x which doesn't grook --noediting.
  ;; Bash 1.x has -nolineediting, but process-send-eof cannot terminate bash if we use it.
  (let* ((prog (or (and (boundp 'explicit-shell-file-name) explicit-shell-file-name)
		   (getenv "ESHELL") shell-file-name))
	 (name (file-name-nondirectory prog)))
    (if (and (not purify-flag)
	     (equal name "bash")
	     (file-executable-p prog)
	     (string-match "bad option"
			   (shell-command-to-string (concat prog " --noediting"))))
	'("-i")
      '("--noediting" "-i")))
  "*Args passed to inferior shell by M-x shell, if the shell is bash.
Value is a list of strings, which may be nil."
  :type '(repeat (string :tag "Argument"))
  :group 'shell)

(defcustom explicit-csh-args
  (if (eq system-type 'hpux)
      ;; -T persuades HP's csh not to think it is smarter
      ;; than us about what terminal modes to use.
      '("-i" "-T")
    '("-i"))
  "*Args passed to inferior shell by M-x shell, if the shell is csh.
Value is a list of strings, which may be nil."
  :type '(repeat (string :tag "Argument"))
  :group 'shell)

(defcustom explicit-ksh-args '("-i")
  "*List of arguments to pass to \"ksh\" on startup in `shell-mode'."
  :type '(repeat (string :tag "Argument"))
  :group 'shell)

(defcustom explicit-pdsh-args '("-i")
  "*List of arguments to pass to \"pdksh\" on startup in `shell-mode'."
  :type '(repeat (string :tag "Argument"))
  :group 'shell)

(defcustom explicit-tcsh-args '("-i")
  "*List of arguments to pass to \"tcsh\" on startup in `shell-mode'."
  :type '(repeat (string :tag "Argument"))
  :group 'shell)

(defcustom explicit-zsh-args '("-i")
  "*List of arguments to pass to \"zsh\" on startup in `shell-mode'."
  :type '(repeat (string :tag "Argument"))
  :group 'shell)

(defcustom shell-input-autoexpand 'history
  "*If non-nil, expand input command history references on completion.
This mirrors the optional behavior of tcsh (its autoexpand and histlit).

If the value is `input', then the expansion is seen on input.
If the value is `history', then the expansion is only when inserting
into the buffer's input ring.  See also `comint-magic-space' and
`comint-dynamic-complete'.

This variable supplies a default for `comint-input-autoexpand',
for Shell mode only."
  :type '(choice (const :tag "off" nil)
                (const input)
                (const history)
                (const :tag "on" t))
  :group 'shell)

(defvar shell-dirstack nil
  "List of directories saved by pushd in this buffer's shell.
Thus, this does not include the shell's current directory.")

(defvar shell-dirtrackp t
  "Non-nil in a shell buffer means directory tracking is enabled.")

(defvar shell-last-dir nil
  "Keep track of last directory for ksh `cd -' command.")

(defvar shell-dirstack-query nil
  "Command used by `shell-resync-dir' to query the shell.")

(defvar shell-mode-map nil)
(if (not shell-mode-map)
    (let ((map (make-keymap)))
      (set-keymap-parents map (list comint-mode-map))
      (set-keymap-name map 'shell-mode-map)
      (define-key map "\C-c\C-f" 'shell-forward-command)
      (define-key map "\C-c\C-b" 'shell-backward-command)
      (define-key map "\t" 'comint-dynamic-complete)
      (define-key map "\M-?"  'comint-dynamic-list-filename-completions)
      ;; XEmacs: this is a pretty common operation for those of us
      ;; who use directory aliases ...  someone shoot me if they
      ;; don't like this binding.  Another possibility is C-c C-s
      ;; but that's way awkward.
      ;; July-5-1997, Bang! -slb
      (unless (featurep 'infodock)
	(define-key map "\M-\C-m" 'shell-resync-dirs))
      (setq shell-mode-map map)))

(defcustom shell-mode-hook nil
  "*Hook for customising Shell mode."
  :type 'hook
  :group 'shell)

;; font-locking
(defcustom shell-prompt-face 'shell-prompt-face
  "Face for shell prompts."
  :type 'face
  :group 'shell-faces)
(defcustom shell-option-face 'shell-option-face
  "Face for command line options."
  :type 'face
  :group 'shell-faces)
(defcustom shell-output-face 'shell-output-face
  "Face for generic shell output."
  :type 'face
  :group 'shell-faces)
(defcustom shell-output-2-face 'shell-output-2-face
  "Face for grep-like output."
  :type 'face
  :group 'shell-faces)
(defcustom shell-output-3-face 'shell-output-3-face
  "Face for [N] output where N is a number."
  :type 'face
  :group 'shell-faces)

(make-face shell-prompt-face)
(make-face shell-option-face)
(make-face shell-output-face)
(make-face shell-output-2-face)
(make-face shell-output-3-face)

(defun shell-font-lock-mode-hook ()
  (or (face-differs-from-default-p shell-prompt-face)
      (copy-face 'font-lock-keyword-face shell-prompt-face))
  (or (face-differs-from-default-p shell-option-face)
      (copy-face 'font-lock-comment-face shell-option-face))
  (or (face-differs-from-default-p shell-output-face)
      (copy-face 'italic shell-output-face))
  (or (face-differs-from-default-p shell-output-2-face)
      (copy-face 'font-lock-string-face shell-output-2-face))
  (or (face-differs-from-default-p shell-output-3-face)
      (copy-face 'font-lock-string-face shell-output-3-face))
  ;; we only need to do this once
  (remove-hook 'font-lock-mode-hook 'shell-font-lock-mode-hook))
(add-hook 'font-lock-mode-hook 'shell-font-lock-mode-hook)

(defvar shell-prompt-pattern-for-font-lock nil
  "If non-nil, pattern to use to font-lock the prompt.
When nil, shell-prompt-pattern will be used.  Set this to a regular
expression if you want the font-locked pattern to be different then
the shell's prompt pattern.")

(defvar shell-font-lock-keywords
  (list '(eval . (cons (if shell-prompt-pattern-for-font-lock
			   shell-prompt-pattern-for-font-lock
			 shell-prompt-pattern)
		       shell-prompt-face))
	'("[ \t]\\([+-][^ \t\n>]+\\)" 1 shell-option-face)
	'("^[^ \t\n]+:.*" . shell-output-2-face)
	'("^\\[[1-9][0-9]*\\]" . shell-output-3-face)
	'("^[^\n]+.*$" . shell-output-face))
  "Additional expressions to highlight in Shell mode.")
(put 'shell-mode 'font-lock-defaults '(shell-font-lock-keywords t))

;;; Basic Procedures

(put 'shell-mode 'mode-class 'special)

(define-derived-mode shell-mode comint-mode "Shell"
  "Major mode for interacting with an inferior shell.
\\<shell-mode-map>\\[comint-send-input] after the end of the process' output sends the text from
    the end of process to the end of the current line.
\\[comint-send-input] before end of process output copies the current line minus the
    prompt to the end of the buffer and sends it (\\[comint-copy-old-input] just copies
    the current line).
\\[send-invisible] reads a line of text without echoing it, and sends it to
    the shell.  This is useful for entering passwords.  Or, add the function
    `comint-watch-for-password-prompt' to `comint-output-filter-functions'.

If you want to make multiple shell buffers, rename the `*shell*' buffer
using \\[rename-buffer] or \\[rename-uniquely] and start a new shell.

If you want to make shell buffers limited in length, add the function
`comint-truncate-buffer' to `comint-output-filter-functions'.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

`cd', `pushd' and `popd' commands given to the shell are watched by Emacs to
keep this buffer's default directory the same as the shell's working directory.
While directory tracking is enabled, the shell's working directory is displayed
by \\[list-buffers] or \\[mouse-buffer-menu] in the `File' field.
\\[dirs] queries the shell and resyncs Emacs' idea of what the current
    directory stack is.
\\[dirtrack-toggle] turns directory tracking on and off.

\\{shell-mode-map}
Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`shell-mode-hook' (in that order).  Before each input, the hooks on
`comint-input-filter-functions' are run.  After each shell output, the hooks
on `comint-output-filter-functions' are run.

Variable `shell-multiple-shells' will automatically generate a new shell each
time it is invoked.

Variables `shell-cd-regexp', `shell-chdrive-regexp', `shell-pushd-regexp'
and `shell-popd-regexp' are used to match their respective commands,
while `shell-pushd-tohome', `shell-pushd-dextract' and `shell-pushd-dunique'
control the behavior of the relevant command.

Variables `comint-completion-autolist', `comint-completion-addsuffix',
`comint-completion-recexact' and `comint-completion-fignore' control the
behavior of file name, command name and variable name completion.  Variable
`shell-completion-execonly' controls the behavior of command name completion.
Variable `shell-completion-fignore' is used to initialise the value of
`comint-completion-fignore'.

Variables `comint-input-ring-file-name' and `comint-input-autoexpand' control
the initialisation of the input ring history, and history expansion.

Variables `comint-output-filter-functions', a hook, and
`comint-scroll-to-bottom-on-input' and `comint-scroll-to-bottom-on-output'
control whether input and output cause the window to scroll to the end of the
buffer."
  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp shell-prompt-pattern)
  (setq comint-completion-fignore shell-completion-fignore)
  (make-local-variable 'comint-delimiter-argument-list)
  (setq comint-delimiter-argument-list shell-delimiter-argument-list)
  (setq comint-file-name-chars shell-file-name-chars)
  (make-local-variable 'comint-after-partial-filename-command)
  (setq comint-after-partial-filename-command 'shell-after-partial-filename)
  (make-local-variable 'comint-get-current-command)
  (setq comint-get-current-command 'shell-get-current-command)
  (make-local-variable 'comint-dynamic-complete-command-command)
  (setq comint-dynamic-complete-command-command 'shell-dynamic-complete-command)
  (setq comint-file-name-quote-list shell-file-name-quote-list)
  (setq comint-dynamic-complete-functions shell-dynamic-complete-functions)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(shell-font-lock-keywords t))
  (make-local-variable 'shell-dirstack)
  (setq shell-dirstack nil)
  (make-local-variable 'shell-last-dir)
  (setq shell-last-dir nil)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-dirtrackp t)
  (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil t)
  (setq comint-input-autoexpand shell-input-autoexpand)
  ;; This is not really correct, since the shell buffer does not really
  ;; edit this directory.  But it is useful in the buffer list and menus.
  (make-local-variable 'list-buffers-directory)
  (setq list-buffers-directory (expand-file-name default-directory))
  ;; shell-dependent assignments.
  (when (ring-empty-p comint-input-ring)
    (let ((shell (file-name-nondirectory (car
		   (process-command (get-buffer-process (current-buffer)))))))
      (setq comint-input-ring-file-name
	    (or (getenv "HISTFILE")
		(cond ((string-equal shell "bash") "~/.bash_history")
		      ((string-equal shell "ksh") "~/.sh_history")
		      (t "~/.history"))))
      (if (or (equal comint-input-ring-file-name "")
	      (equal (file-truename comint-input-ring-file-name)
		     (file-truename "/dev/null")))
	  (setq comint-input-ring-file-name nil))
      ;; Arrange to write out the input ring on exit, if the shell doesn't
      ;; do this itself.
      (if (and comint-input-ring-file-name
	       (string-match shell-dumb-shell-regexp shell))
	  (set-process-sentinel (get-buffer-process (current-buffer))
				#'shell-write-history-on-exit))
      (setq shell-dirstack-query
	    (cond ((string-equal shell "sh") "pwd")
		  ((string-equal shell "ksh") "echo $PWD ~-")
		  (t "dirs"))))
    (comint-read-input-ring t)
    (shell-dirstack-message)))

(defun shell-write-history-on-exit (process event)
  "Called when the shell process is stopped.

Writes the input history to a history file
`comint-input-ring-file-name' using `comint-write-input-ring'
and inserts a short message in the shell buffer.

This function is a sentinel watching the shell interpreter process.
Sentinels will always get the two parameters PROCESS and EVENT."
  ;; Write history.
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s\n" process event))))))

;;;###autoload
(defun shell (&optional buffer)
  "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
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

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
	 (read-buffer "Shell buffer: " "*shell*"))))
  (let* ((buffer-name (cond ((null buffer) "*shell*")
			    ((stringp buffer) buffer)
			    (t (buffer-name buffer))))
	 (buffer (if shell-multiple-shells (generate-new-buffer buffer-name)
		   (get-buffer-create buffer-name))))
    ;; Pop to buffer, so that the buffer's window will be correctly set
    ;; when we call comint (so that comint sets the COLUMNS env var properly).
    (pop-to-buffer buffer)
    (cond ((not (comint-check-proc buffer))
	   (let* ((prog (or explicit-shell-file-name
			    (getenv "ESHELL")
			    (getenv "SHELL")
			    shell-file-name
			    "/bin/sh"))
		  (name (file-name-nondirectory prog))
		  (startfile (concat "~/.emacs_" name))
		  (xargs-name (intern-soft (concat "explicit-" name "-args"))))
	     (if (not (file-exists-p startfile))
		 (setq startfile (concat "~/.emacs.d/.emacs_" name)))
	     (setq buffer (set-buffer (apply 'make-comint-1 buffer
					     prog
					     (if (file-exists-p startfile)
						 startfile)
					     (if (and xargs-name
						      (boundp xargs-name))
						 (symbol-value xargs-name)
					       '("-i")))))
	     (shell-mode))))
    buffer))

;;; Don't do this when shell.el is loaded, only while dumping.
;;;###autoload (add-hook 'same-window-buffer-names "*shell*")

;;; Directory tracking
;;;
;;; This code provides the shell mode input sentinel
;;;     SHELL-DIRECTORY-TRACKER
;;; that tracks cd, pushd, and popd commands issued to the shell, and
;;; changes the current directory of the shell buffer accordingly.
;;;
;;; This is basically a fragile hack, although it's more accurate than
;;; the version in Emacs 18's shell.el. It has the following failings:
;;; 1. It doesn't know about the cdpath shell variable.
;;; 2. It cannot infallibly deal with command sequences, though it does well
;;;    with these and with ignoring commands forked in another shell with ()s.
;;; 3. More generally, any complex command is going to throw it. Otherwise,
;;;    you'd have to build an entire shell interpreter in emacs lisp.  Failing
;;;    that, there's no way to catch shell commands where cd's are buried
;;;    inside conditional expressions, aliases, and so forth.
;;;
;;; The whole approach is a crock. Shell aliases mess it up. File sourcing
;;; messes it up. You run other processes under the shell; these each have
;;; separate working directories, and some have commands for manipulating
;;; their w.d.'s (e.g., the lcd command in ftp). Some of these programs have
;;; commands that do *not* affect the current w.d. at all, but look like they
;;; do (e.g., the cd command in ftp).  In shells that allow you job
;;; control, you can switch between jobs, all having different w.d.'s. So
;;; simply saying %3 can shift your w.d..
;;;
;;; The solution is to relax, not stress out about it, and settle for
;;; a hack that works pretty well in typical circumstances. Remember
;;; that a half-assed solution is more in keeping with the spirit of Unix,
;;; anyway. Blech.
;;;
;;; One good hack not implemented here for users of programmable shells
;;; is to program up the shell w.d. manipulation commands to output
;;; a coded command sequence to the tty. Something like
;;;     ESC | <cwd> |
;;; where <cwd> is the new current working directory. Then trash the
;;; directory tracking machinery currently used in this package, and
;;; replace it with a process filter that watches for and strips out
;;; these messages.

(defun shell-directory-tracker (str)
  "Tracks cd, pushd and popd commands issued to the shell.
This function is called on each input passed to the shell.
It watches for cd, pushd and popd commands and sets the buffer's
default directory to track these commands.

You may toggle this tracking on and off with \\[dirtrack-toggle].
If emacs gets confused, you can resync with the shell with \\[dirs].

See variables `shell-cd-regexp', `shell-chdrive-regexp', `shell-pushd-regexp',
`shell-popd-regexp', `shell-dirtrack-other-regexp', and 
`shell-dirtrack-process-other-func', while `shell-pushd-tohome', 
`shell-pushd-dextract', and `shell-pushd-dunique' control the behavior of 
the relevant command.

Environment variables are expanded, see function `substitute-in-file-name'."
  (if shell-dirtrackp
      ;; We fail gracefully if we think the command will fail in the shell.
      (condition-case chdir-failure
	  (let ((start (progn (string-match
			       (concat "^" shell-command-separator-regexp)
			       str) ; skip whitespace
			      (match-end 0)))
		end cmd arg1)
	    (while (string-match shell-command-regexp str start)
	      (setq end (match-end 0)
		    cmd (comint-arguments (substring str start end) 0 0)
		    arg1 (comint-arguments (substring str start end) 1 1))
	      (if arg1
		  (setq arg1 (shell-unquote-argument arg1)))
	      (cond ((string-match (concat "\\`\\(" shell-popd-regexp
					   "\\)\\($\\|[ \t]\\)")
				   cmd)
		     (shell-process-popd (comint-substitute-in-file-name arg1)))
		    ((string-match (concat "\\`\\(" shell-pushd-regexp
					   "\\)\\($\\|[ \t]\\)")
				   cmd)
		     (shell-process-pushd (comint-substitute-in-file-name arg1)))
		    ((string-match (concat "\\`\\(" shell-cd-regexp
					   "\\)\\($\\|[ \t]\\)")
				   cmd)
		     (shell-process-cd (comint-substitute-in-file-name arg1)))
		    ((and shell-chdrive-regexp
			  (string-match (concat "\\`\\(" shell-chdrive-regexp
						"\\)\\($\\|[ \t]\\)")
					cmd))
		     (shell-process-cd (comint-substitute-in-file-name cmd)))
		    ((and shell-dirtrack-other-regexp
			  shell-dirtrack-process-other-func
			  (string-match (concat "\\`\\(" 
						shell-dirtrack-other-regexp
						"\\)\\($\\|[ \t]\\)")
					cmd))
		     (funcall shell-dirtrack-process-other-func
			      ;; pass cmd so one "other-func" can handle
			      ;; multiple commands.
			      cmd
			      ;; leave arg1 unchanged since we don't know
			      ;; what `shell-dirtrack-process-other-func'
			      ;; will want to do with it
			      arg1)))
	      (setq start (progn (string-match
				  shell-command-separator-regexp str end)
				 ;; skip again
				 (match-end 0)))))
	(error "Couldn't cd"))))

;; Like `cd', but prepends comint-file-name-prefix to absolute names.
(defun shell-cd-1 (dir dirstack)
  (if shell-dirtrackp
      (setq list-buffers-directory (file-name-as-directory
				    (expand-file-name dir))))
  (condition-case nil
      (progn (if (file-name-absolute-p dir)
                 ;;(cd-absolute (concat comint-file-name-prefix dir))
		 (cd-absolute dir)
	       (cd dir))
             (setq shell-dirstack dirstack)
             (shell-dirstack-message))
    (file-error (message "Couldn't cd"))))

(defun shell-unquote-argument (string)
  "Remove all kinds of shell quoting from STRING."
  (save-match-data
    (let ((idx 0) next inside
	  (quote-chars
	   (if (string-match shell-dumb-shell-regexp
			     (file-name-nondirectory
			      (car (process-command (get-buffer-process (current-buffer))))))
	       "['`\"]"
	     "[\\'`\"]")))
      (while (and (< idx (length string))
		  (setq next (string-match quote-chars string next)))
	(cond ((= (aref string next) ?\\)
	       (setq string (replace-match "" nil nil string))
	       (setq next (1+ next)))
	      ((and inside (= (aref string next) inside))
	       (setq string (replace-match "" nil nil string))
	       (setq inside nil))
	      (inside
	       (setq next (1+ next)))
	      (t
	       (setq inside (aref string next))
	       (setq string (replace-match "" nil nil string)))))
      string)))

;;; popd [+n]
(defun shell-process-popd (arg)
  (let ((num (or (shell-extract-num arg) 0)))
    (cond ((and num (= num 0) shell-dirstack)
	   (shell-cd-1 (car shell-dirstack) (cdr shell-dirstack)))
	  ((and num (> num 0) (<= num (length shell-dirstack)))
	   (let* ((ds (cons nil shell-dirstack))
		  (cell (nthcdr (1- num) ds)))
	     (rplacd cell (cdr (cdr cell)))
	     (setq shell-dirstack (cdr ds))
	     (shell-dirstack-message)))
	  (t
	   (error "Couldn't popd")))))

;; Return DIR prefixed with comint-file-name-prefix as appropriate.
(defun shell-prefixed-directory-name (dir)
  (if (= (length comint-file-name-prefix) 0)
      dir
    (if (file-name-absolute-p dir)
	;; The name is absolute, so prepend the prefix.
	(concat comint-file-name-prefix dir)
      ;; For relative name we assume default-directory already has the prefix.
      (expand-file-name dir))))

;;; cd [dir]
(defun shell-process-cd (arg)
  (let ((new-dir (cond ((zerop (length arg)) (concat comint-file-name-prefix
						     "~"))
		       ((string-equal "-" arg) shell-last-dir)
		       (t (shell-prefixed-directory-name arg)))))
    (setq shell-last-dir default-directory)
    (shell-cd-1 new-dir shell-dirstack)))

;;; pushd [+n | dir]
(defun shell-process-pushd (arg)
  (let ((num (shell-extract-num arg)))
    (cond ((zerop (length arg))
	   ;; no arg -- swap pwd and car of stack unless shell-pushd-tohome
	   (cond (shell-pushd-tohome
		  (shell-process-pushd (concat comint-file-name-prefix "~")))
		 (shell-dirstack
		  (let ((old default-directory))
		    (shell-cd-1 (car shell-dirstack)
				(cons old (cdr shell-dirstack)))))
		 (t
		  (message "Directory stack empty."))))
	  ((numberp num)
	   ;; pushd +n
	   (cond ((> num (length shell-dirstack))
		  (message "Directory stack not that deep."))
		 ((= num 0)
		  (error (message "Couldn't cd")))
		 (shell-pushd-dextract
		  (let ((dir (nth (1- num) shell-dirstack)))
		    (shell-process-popd arg)
		    (shell-process-pushd default-directory)
		    (shell-cd-1 dir shell-dirstack)))
		 (t
		  (let* ((ds (cons default-directory shell-dirstack))
			 (dslen (length ds))
			 (front (nthcdr num ds))
			 (back (reverse (nthcdr (- dslen num) (reverse ds))))
			 (new-ds (append front back)))
		    (shell-cd-1 (car new-ds) (cdr new-ds))))))
	  (t
	   ;; pushd <dir>
	   (let ((old-wd default-directory))
             (shell-cd-1 (shell-prefixed-directory-name arg)
                         (if (or (null shell-pushd-dunique)
                                 (not (member old-wd shell-dirstack)))
                             (cons old-wd shell-dirstack)
                             shell-dirstack)))))))

;; If STR is of the form +n, for n>0, return n. Otherwise, nil.
(defun shell-extract-num (str)
  (and (string-match "^\\+[1-9][0-9]*$" str)
       (string-to-int str)))


(defun shell-dirtrack-toggle ()
  "Turn directory tracking on and off in a shell buffer."
  (interactive)
  (if (setq shell-dirtrackp (not shell-dirtrackp))
      (setq list-buffers-directory default-directory)
    (setq list-buffers-directory nil))
  (message "Directory tracking %s" (if shell-dirtrackp "ON" "OFF")))

;;; For your typing convenience:
(defalias 'dirtrack-toggle 'shell-dirtrack-toggle)

(defun shell-cd (dir)
  "Do normal `cd' to DIR, and set `list-buffers-directory'."
  (if shell-dirtrackp
      (setq list-buffers-directory (file-name-as-directory
				    (expand-file-name dir))))
  (cd dir))

(defun shell-resync-dirs ()
  "Resync the buffer's idea of the current directory stack.
This command queries the shell with the command bound to
`shell-dirstack-query' (default \"dirs\"), reads the next
line output and parses it to form the new directory stack.
DON'T issue this command unless the buffer is at a shell prompt.
Also, note that if some other subprocess decides to do output
immediately after the query, its output will be taken as the
new directory stack -- you lose. If this happens, just do the
command again."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
	 (pmark (process-mark proc)))
    (goto-char pmark)
    (insert shell-dirstack-query) (insert "\n")
    (sit-for 0) ; force redisplay
    (comint-send-string proc shell-dirstack-query)
    (comint-send-string proc "\n")
    (set-marker pmark (point))
    (let ((pt (point))) ; wait for 1 line
      ;; This extra newline prevents the user's pending input from spoofing us.
      (insert "\n") (backward-char 1)
      (while (not (looking-at ".+\n"))
	(accept-process-output proc)
	(goto-char pt)
	;; kludge to cope with shells that have "stty echo" turned on.
	;; of course this will lose if there is only one dir on the stack
	;; and it is named "dirs"...  -jwz
	(if (looking-at "^dirs\r?\n") (delete-region (point) (match-end 0)))
	))
    (goto-char pmark) (delete-char 1) ; remove the extra newline
    ;; That's the dirlist. grab it & parse it.
    (let* ((dl (buffer-substring (match-beginning 0) (1- (match-end 0))))
	   (dl-len (length dl))
	   (ds '())			; new dir stack
	   (i 0))
      (while (< i dl-len)
	;; regexp = optional whitespace, (non-whitespace), optional whitespace
	(string-match "\\s *\\(\\S +\\)\\s *" dl i) ; pick off next dir
	(setq ds (cons (concat comint-file-name-prefix
			       (substring dl (match-beginning 1)
					  (match-end 1)))
		       ds))
	(setq i (match-end 0)))
      (let ((ds (reverse ds)))
        (shell-cd-1 (car ds) (cdr ds))))))

;;; For your typing convenience:
(defalias 'dirs 'shell-resync-dirs)

;; XEmacs addition
(defvar shell-dirstack-message-hook nil
  "Hook to run after a cd, pushd or popd event")

;;; Show the current dirstack on the message line.
;;; Pretty up dirs a bit by changing "/usr/jqr/foo" to "~/foo".
;;; (This isn't necessary if the dirlisting is generated with a simple "dirs".)
;;; All the commands that mung the buffer's dirstack finish by calling
;;; this guy.
(defun shell-dirstack-message ()
  (let* ((msg "")
         (ds (cons default-directory shell-dirstack))
         (home (format "^%s\\(/\\|$\\)" (regexp-quote (user-home-directory))))
         (prefix (and comint-file-name-prefix
		      ;; XEmacs addition: don't turn "/foo" into "foo" !!
		      (not (= 0 (length comint-file-name-prefix)))
                      (format "^%s\\(/\\|$\\)"
                              (regexp-quote comint-file-name-prefix)))))
    (while ds
      (let ((dir (car ds)))
	(if (string-match home dir)
	    (setq dir (concat "~/" (substring dir (match-end 0)))))
	;; Strip off comint-file-name-prefix if present.
	(and prefix (string-match prefix dir)
	     (setq dir (substring dir (match-end 0)))
             (setcar ds dir)
             )
	(setq msg (concat msg dir " "))
	(setq ds (cdr ds))))
    (run-hooks 'shell-dirstack-message-hook)
    (message "%s" msg)))

;; This was mostly copied from shell-resync-dirs.
(defun shell-snarf-envar (var)
  "Return as a string the shell's value of environment variable VAR."
  (let* ((cmd (format "printenv '%s'\n" var))
	 (proc (get-buffer-process (current-buffer)))
	 (pmark (process-mark proc)))
    (goto-char pmark)
    (insert cmd)
    (sit-for 0)				; force redisplay
    (comint-send-string proc cmd)
    (set-marker pmark (point))
    (let ((pt (point)))			; wait for 1 line
      ;; This extra newline prevents the user's pending input from spoofing us.
      (insert "\n") (backward-char 1)
      (while (not (looking-at ".+\n"))
	(accept-process-output proc)
	(goto-char pt)))
    (goto-char pmark) (delete-char 1)	; remove the extra newline
    (buffer-substring (match-beginning 0) (1- (match-end 0)))))

(defun shell-copy-environment-variable (variable)
  "Copy the environment variable VARIABLE from the subshell to Emacs.
This command reads the value of the specified environment variable
in the shell, and sets the same environment variable in Emacs
\(what `getenv' in Emacs would return) to that value.
That value will affect any new subprocesses that you subsequently start
from Emacs."
  (interactive (list (read-envvar-name "\
Copy Shell environment variable to Emacs: ")))
  (setenv variable (shell-snarf-envar variable)))

(defun shell-forward-command (&optional arg)
  "Move forward across ARG shell command(s).  Does not cross lines.
See `shell-command-regexp'."
  (interactive "p")
  (let ((limit (save-excursion (end-of-line nil) (point))))
    (if (re-search-forward (concat shell-command-regexp "\\([;&|][\t ]*\\)+")
			   limit 'move arg)
	(skip-syntax-backward " "))))


(defun shell-backward-command (&optional arg)
  "Move backward across ARG shell command(s).  Does not cross lines.
See `shell-command-regexp'."
  (interactive "p")
  (let ((limit (save-excursion (comint-bol nil) (point))))
    (when (> limit (point))
      (setq limit (line-beginning-position)))
    (skip-syntax-backward " " limit)
    (if (re-search-backward
	 (format "[;&|]+[\t ]*\\(%s\\)" shell-command-regexp) limit 'move arg)
	(progn (goto-char (match-beginning 1))
	       (skip-chars-forward ";&|")))))

(defun shell-dynamic-complete-command ()
  "Dynamically complete the command at point.
This function is similar to `comint-dynamic-complete-filename', except that it
searches `exec-path' (minus the trailing emacs library path) for completion
candidates.  Note that this may not be the same as the shell's idea of the
path.

Completion is dependent on the value of `shell-completion-execonly', plus
those that effect file completion.  See `shell-dynamic-complete-as-command'.

Returns t if successful."
  (interactive)
  (let ((filename (comint-match-partial-filename)))
    (if (and filename
	     (save-match-data (not (string-match "[~/]" filename)))
	     (eq (match-beginning 0)
		 (save-excursion (shell-backward-command 1) (point))))
	(prog2 (message "Completing command name...")
	    (shell-dynamic-complete-as-command)))))


(defun shell-dynamic-complete-as-command ()
  "Dynamically complete at point as a command.
See `shell-dynamic-complete-filename'.  Returns t if successful."
  (let* ((filename (or (comint-match-partial-filename) ""))
	 (pathnondir (file-name-nondirectory filename))
	 (paths (cdr (reverse exec-path)))
	 (cwd (file-name-as-directory (expand-file-name default-directory)))
	 (ignored-extensions
	  (and comint-completion-fignore
	       (mapconcat (function (lambda (x) (concat (regexp-quote x) "$")))
			  comint-completion-fignore "\\|")))
	 (path "") (comps-in-path ()) (file "") (filepath "") (completions ()))
    ;; Go thru each path in the search path, finding completions.
    (while paths
      (setq path (file-name-as-directory (comint-directory (or (car paths) ".")))
	    comps-in-path (and (file-accessible-directory-p path)
			       (file-name-all-completions pathnondir path)))
      ;; Go thru each completion found, to see whether it should be used.
      (while comps-in-path
	(setq file (car comps-in-path)
	      filepath (concat path file))
	(if (and (not (member file completions))
		 (not (and ignored-extensions
			   (string-match ignored-extensions file)))
		 (or (string-equal path cwd)
		     (not (file-directory-p filepath)))
		 (or (null shell-completion-execonly)
		     (file-executable-p filepath)))
	    (setq completions (cons file completions)))
	(setq comps-in-path (cdr comps-in-path)))
      (setq paths (cdr paths)))
    ;; OK, we've got a list of completions.
    (let ((success (let ((comint-completion-addsuffix nil))
		     (comint-dynamic-simple-complete pathnondir completions))))
      (if (and (memq success '(sole shortest)) comint-completion-addsuffix
	       (not (file-directory-p (comint-match-partial-filename))))
	  (insert " "))
      success)))


(defun shell-match-partial-variable ()
  "Return the shell variable at point, or nil if none is found."
  (save-excursion
    (let ((limit (point)))
      (if (re-search-backward "[^A-Za-z0-9_{}]" nil 'move)
	  (or (looking-at "\\$") (forward-char 1)))
      ;; Anchor the search forwards.
      (if (or (eolp) (looking-at "[^A-Za-z0-9_{}$]"))
	  nil
	(re-search-forward "\\$?{?[A-Za-z0-9_]*}?" limit)
	(buffer-substring (match-beginning 0) (match-end 0))))))


(defun shell-dynamic-complete-environment-variable ()
  "Dynamically complete the environment variable at point.
Completes if after a variable, i.e., if it starts with a \"$\".
See `shell-dynamic-complete-as-environment-variable'.

This function is similar to `comint-dynamic-complete-filename', except that it
searches `process-environment' for completion candidates.  Note that this may
not be the same as the interpreter's idea of variable names.  The main problem
with this type of completion is that `process-environment' is the environment
which Emacs started with.  Emacs does not track changes to the environment made
by the interpreter.  Perhaps it would be more accurate if this function was
called `shell-dynamic-complete-process-environment-variable'.

Returns non-nil if successful."
  (interactive)
  (let ((variable (shell-match-partial-variable)))
    (if (and variable (string-match "^\\$" variable))
	(prog2 (message "Completing variable name...")
	    (shell-dynamic-complete-as-environment-variable)))))


(defun shell-dynamic-complete-as-environment-variable ()
  "Dynamically complete at point as an environment variable.
Used by `shell-dynamic-complete-environment-variable'.
Uses `comint-dynamic-simple-complete'."
  (let* ((var (or (shell-match-partial-variable) ""))
	 (variable (substring var (or (string-match "[^$({]\\|$" var) 0)))
	 (variables (mapcar (function (lambda (x)
					(substring x 0 (string-match "=" x))))
			    process-environment))
	 (addsuffix comint-completion-addsuffix)
	 (comint-completion-addsuffix nil)
	 (success (comint-dynamic-simple-complete variable variables)))
    (if (memq success '(sole shortest))
	(let* ((var (shell-match-partial-variable))
	       (variable (substring var (string-match "[^$({]" var)))
	       (protection (cond ((string-match "{" var) "}")
				 ((string-match "(" var) ")")
				 (t "")))
	       (suffix (cond ((null addsuffix) "")
			     ((file-directory-p
			       (comint-directory (getenv variable))) "/")
			     (t " "))))
	  (insert protection suffix)))
    success))


(defun shell-replace-by-expanded-directory ()
  "Expand directory stack reference before point.
Directory stack references are of the form \"=digit\" or \"=-\".
See `default-directory' and `shell-dirstack'.

Returns t if successful."
  (interactive)
  (if (comint-match-partial-filename)
      (save-excursion
	(goto-char (match-beginning 0))
	(let ((stack (cons default-directory shell-dirstack))
	      (index (cond ((looking-at "=-/?")
			    (length shell-dirstack))
			   ((looking-at "=\\([0-9]+\\)/?")
			    (string-to-number
			     (buffer-substring
			      (match-beginning 1) (match-end 1)))))))
	  (cond ((null index)
		 nil)
		((>= index (length stack))
		 (error "Directory stack not that deep"))
		(t
		 (replace-match (file-name-as-directory (nth index stack)) t t)
		 (message "Directory item: %d" index)
		 t))))))

(provide 'shell)

;;; shell.el ends here
