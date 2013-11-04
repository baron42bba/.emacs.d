;;; DO NOT MODIFY THIS FILE
(if (featurep 'prog-modes-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "prog-modes/_pkg.el")

(package-provide 'prog-modes :version 2.07 :author-version "No-Upstream-Ver" :type 'single-file)

;;;***

;;;### (autoloads (asm-mode) "asm-mode" "prog-modes/asm-mode.el")

(autoload 'asm-mode "asm-mode" "\
Major mode for editing typical assembler code.
Features a private abbrev table and the following bindings:

\\[asm-colon]	outdent a preceding label, tab to next tab stop.
\\[tab-to-tab-stop]	tab to next tab stop.
\\[asm-newline]	newline, then tab to next tab stop.
\\[asm-comment]	smart placement of assembler comments.

The character used for making comments is set by the variable
`asm-comment-char' (which defaults to `?;').

Alternatively, you may set this variable in `asm-mode-set-comment-hook',
which is called near the beginning of mode initialization.

Turning on Asm mode runs the hook `asm-mode-hook' at the end of initialization.

Special commands:
\\{asm-mode-map}
" t nil)
(add-to-list 'auto-mode-alist '("\\.[sS]\\'" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.asm\\'" . asm-mode))

;;;***

;;;### (autoloads (autoconf-mode) "autoconf-mode" "prog-modes/autoconf-mode.el")

(autoload 'autoconf-mode "autoconf-mode" "\
A major-mode to edit autoconf input files like configure.in
\\{autoconf-mode-map}
" t nil)
(add-to-list 'auto-mode-alist '("\\.m4\\'" . autoconf-mode))
(add-to-list 'auto-mode-alist '("configure\\.\\(in\\|ac\\)\\'" . autoconf-mode))

;;;***

;;;### (autoloads (awk-mode) "awk-mode" "prog-modes/awk-mode.el")

(autoload 'awk-mode "awk-mode" "\
Major mode for editing AWK code.
This is much like C mode except for the syntax of comments.  It uses
the same keymap as C mode and has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on AWK mode calls the value of the variable `awk-mode-hook'
with no args, if that value is non-nil." t nil)
(add-to-list 'auto-mode-alist '("\\.awk\\'" . awk-mode))
(add-to-list 'interpreter-mode-alist '("awk\\b" . awk-mode))

;;;***

;;;### (autoloads (common-lisp-indent-function) "cl-indent" "prog-modes/cl-indent.el")

(autoload 'common-lisp-indent-function "cl-indent" nil nil nil)

;;;***

;;;### (autoloads nil "diff-mode" "prog-modes/diff-mode.el")
(autoload 'diff-mode "diff-mode" nil t)
(autoload 'diff-minor-mode "diff-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))

;;;***

;;;### (autoloads (eiffel-mode) "eiffel" "prog-modes/eiffel.el")

(autoload 'eiffel-mode "eiffel" "\
Major mode for editing Eiffel programs.
\\[indent-for-tab-command] indents the current Eiffel line correctly and
\\[reindent-then-newline-and-indent] causes the current and next line to be
properly indented.

Key definitions:
\\{eiffel-mode-map}

If variable `eif-use-gnu-eiffel' is non-nil (default t) then support
for using GNU SmallEiffel is enabled.  Run \\[eif-customize] to see
compilation and indentation variables that can be customized." t nil)
(add-to-list 'auto-mode-alist '("\\.e\\'" . eiffel-mode))

;;;***

;;;### (autoloads (icon-mode) "icon" "prog-modes/icon.el")

(autoload 'icon-mode "icon" "\
Major mode for editing Icon code.
Expression and list commands understand all Icon brackets.
Tab indents for Icon code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{icon-mode-map}
Variables controlling indentation style:
 icon-tab-always-indent
    Non-nil means TAB in Icon mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 icon-auto-newline
    Non-nil means automatically newline before and after braces
    inserted in Icon code.
 icon-indent-level
    Indentation of Icon statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 icon-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 icon-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to `icon-continued-statement-offset'.
 icon-brace-offset
    Extra indentation for line if it starts with an open brace.
 icon-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.

Turning on Icon mode calls the value of the variable `icon-mode-hook'
with no args, if that value is non-nil." t nil)
(add-to-list 'auto-mode-alist '("\\.icn\\'" . icon-mode))

;;;***

;;;### (autoloads (javascript-shell javascript-mode) "javascript-mode" "prog-modes/javascript-mode.el")

(autoload 'javascript-mode "javascript-mode" "\
Major mode for editing JavaScript code.

See the documentation for `c++-mode': JavaScript mode is an extension of it.
Use the hook `javascript-mode-hook' to execute custom code when entering
JavaScript mode.

\\{javascript-mode-map}" t nil)

(autoload 'javascript-shell "javascript-mode" "\
Run a JavaScript shell as an inferior process.

Use the `javascript-shell-command' variable to set the command and
`javascript-shell-command-args' for its arguments to specify the
command line that invokes your preferred JavaScript shell.

Free JavaScript shell implementations are available for example from
<http://www.mozilla.org/js/>.

Usage examples:        command    arguments
 Mozilla SpiderMonkey  jsshell
 Mozilla Rhino         java       -jar /path/to/js.jar" t nil)
(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.pac$" . javascript-mode))

;;;***

;;;### (autoloads (ksh-mode) "ksh-mode" "prog-modes/ksh-mode.el")

(autoload 'ksh-mode "ksh-mode" "\
ksh-mode $Revision: 1.4 $ - Major mode for editing (Bourne, Korn or Bourne again)
shell scripts.
Special key bindings and commands:
\\{ksh-mode-map}
Variables controlling indentation style:
ksh-indent
    Indentation of ksh statements with respect to containing block.
    Default value is 2.
ksh-case-indent
    Additional indentation for statements under case items.
    Default value is nil which will align the statements one position 
    past the \")\" of the pattern.
ksh-case-item-offset
    Additional indentation for case items within a case statement.
    Default value is 2.
ksh-group-offset
    Additional indentation for keywords \"do\" and \"then\".
    Default value is -2.
ksh-brace-offset
    Additional indentation of \"{\" under functions or brace groupings.
    Default value is 0.
ksh-multiline-offset
   Additional indentation of line that is preceded of a line ending with a
   \\ to make it continue on next line.
ksh-tab-always-indent
    Controls the operation of the TAB key. If t (the default), always
    reindent the current line.  If nil, indent the current line only if
    point is at the left margin or in the line's indentation; otherwise
    insert a tab.
ksh-match-and-tell
    If non-nil echo in the minibuffer the matching compound command
    for the \"done\", \"}\", \"fi\", or \"esac\". Default value is t.

ksh-align-to-keyword
    Controls whether nested constructs align from the keyword or
    the current indentation. If non-nil, indentation will be relative to
    the column the keyword starts. If nil, indentation will be relative to
    the current indentation of the line the keyword is on.
    The default value is non-nil.

ksh-comment-regexp
  Regular expression used to recognize comments. Customize to support
  ksh-like languages. Default value is \"\\s *#\".

Style Guide.
 By setting
    (setq ksh-indent default-tab-width)
    (setq ksh-group-offset 0)

    The following style is obtained:

    if [ -z $foo ]
	    then
		    bar    # <-- ksh-group-offset is additive to ksh-indent
		    foo
    fi

 By setting
    (setq ksh-indent default-tab-width)
    (setq ksh-group-offset (- 0 ksh-indent))

    The following style is obtained:

    if [ -z $foo ]
    then
	    bar
	    foo
    fi

 By setting
    (setq ksh-case-item-offset 1)
    (setq ksh-case-indent nil)

    The following style is obtained:

    case x in *
     foo) bar           # <-- ksh-case-item-offset
          baz;;         # <-- ksh-case-indent aligns with \")\"
     foobar) foo
             bar;;
    esac

 By setting
    (setq ksh-case-item-offset 1)
    (setq ksh-case-indent 6)

    The following style is obtained:

    case x in *
     foo) bar           # <-- ksh-case-item-offset
           baz;;        # <-- ksh-case-indent
     foobar) foo
           bar;;
    esac
    

Installation:

 (setq ksh-mode-hook
      (function (lambda ()
         (font-lock-mode 1)             ;; font-lock the buffer
         (setq ksh-indent 8)
	 (setq ksh-group-offset -8)
	 (setq ksh-brace-offset -8)   
         (setq ksh-tab-always-indent t)
         (setq ksh-match-and-tell t)
         (setq ksh-align-to-keyword t)	;; Turn on keyword alignment
	 )))" t nil)

;;;***

;;;### (autoloads (lua-mode) "lua-mode" "prog-modes/lua-mode.el")

(autoload 'lua-mode "lua-mode" "\
Major mode for editing lua scripts.
The following keys are bound:
\\{lua-mode-map}
" t nil)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;;***

;;;### (autoloads (m4-mode) "m4-mode" "prog-modes/m4-mode.el")

(autoload 'm4-mode "m4-mode" "\
A major-mode to edit m4 macro files
\\{m4-mode-map}
" t nil)

;;;***

;;;### (autoloads (makefile-mode) "make-mode" "prog-modes/make-mode.el")

(autoload 'makefile-mode "make-mode" "\
Major mode for editing Makefiles.
This function ends by invoking the function(s) `makefile-mode-hook'.

\\{makefile-mode-map}

In the browser, use the following keys:

\\{makefile-browser-map}

Makefile mode can be configured by modifying the following variables:

makefile-browser-buffer-name:
    Name of the macro- and target browser buffer.

makefile-target-colon:
    The string that gets appended to all target names
    inserted by `makefile-insert-target'.
    \":\" or \"::\" are quite common values.

makefile-macro-assign:
   The string that gets appended to all macro names
   inserted by `makefile-insert-macro'.
   The normal value should be \" = \", since this is what
   standard make expects.  However, newer makes such as dmake
   allow a larger variety of different macro assignments, so you
   might prefer to use \" += \" or \" := \" .

makefile-tab-after-target-colon:
   If you want a TAB (instead of a space) to be appended after the
   target colon, then set this to a non-nil value.

makefile-browser-leftmost-column:
   Number of blanks to the left of the browser selection mark.

makefile-browser-cursor-column:
   Column in which the cursor is positioned when it moves
   up or down in the browser.

makefile-browser-selected-mark:
   String used to mark selected entries in the browser.

makefile-browser-unselected-mark:
   String used to mark unselected entries in the browser.

makefile-browser-auto-advance-after-selection-p:
   If this variable is set to a non-nil value the cursor
   will automagically advance to the next line after an item
   has been selected in the browser.

makefile-pickup-everything-picks-up-filenames-p:
   If this variable is set to a non-nil value then
   `makefile-pickup-everything' also picks up filenames as targets
   (i.e. it calls `makefile-pickup-filenames-as-targets'), otherwise
   filenames are omitted.

makefile-cleanup-continuations-p:
   If this variable is set to a non-nil value then Makefile mode
   will assure that no line in the file ends with a backslash
   (the continuation character) followed by any whitespace.
   This is done by silently removing the trailing whitespace, leaving
   the backslash itself intact.
   IMPORTANT: Please note that enabling this option causes Makefile mode
   to MODIFY A FILE WITHOUT YOUR CONFIRMATION when \"it seems necessary\".

makefile-browser-hook:
   A function or list of functions to be called just before the
   browser is entered. This is executed in the makefile buffer.

makefile-special-targets-list:
   List of special targets. You will be offered to complete
   on one of those in the minibuffer whenever you enter a `.'.
   at the beginning of a line in Makefile mode." t nil)
(add-to-list 'auto-mode-alist '("\\.ma?ke?\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\(GNU\\)?[Mm]akefile\\(\\.in\\)*\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.am\\'" . makefile-mode))
(add-to-list 'interpreter-mode-alist '("make" . makefile-mode))

;;;***

;;;### (autoloads (mode-compile-kill mode-compile mode-compile-submit-bug-report) "mode-compile" "prog-modes/mode-compile.el")

(defcustom mode-compile-make-program "make" "*The `make' program used to process makefiles.\n\nIf you have GNU make installed with name \"gmake\" use it." :type 'string :group 'compilation)

(defcustom mode-compile-ignore-makefile-backups t "*Tell mode compile to ignore makefiles backup files when selecting the Makefile to use." :type 'boolean :group 'compilation)

(defvar mode-compile-default-make-options "-k" "\
Default options to give to `make'.")

(defcustom mode-compile-make-options (eval mode-compile-default-make-options) "*Options to give to `make'.\nThis could be any form evaluating to a string.\n\nSome people asked me a way to modify the make options every time a\ncompilation command is launched, do that:\n (defun my-mode-compile-ask-make-options()\n   \"*Hook called by mode-compile, asking for make options.\"\n   (interactive)\n   (read-string \"Make options: \"\n                mode-compile-default-make-options))\n (setq mode-compile-make-options\n           'my-mode-compile-ask-make-options)" :type '(choice string (sexp :tag "Form evaluating to a string")) :group 'compilation)

(defcustom mode-compile-preferred-default-makerule 'none "*Default makerule you would like to see in minibuffer as a default choice\nwhen selecting the make rule to build.\n\nPossible values are:\n'none    -- let mode-compile deciding for you.\n'all     -- try hard to show you the \"all\" rule.\n'default -- try hard to show you the \"default\" rule.\n'file    -- try to show you the name of the file which will be\n            result of compilation.\nThe 'none action is taken as default is something fail." :type '(radio :tag "Symbol" (const :tag "None - Let mode compile made the choice" :value none) (const :tag "All - Show the \"all\" rule" :value all) (const :tag "Default - Show the \"default\" rule" :value default) (const :tag "File - Show the \"result file name\" rule" :value file)) :group 'compilation)

(defcustom mode-compile-ignore-makerule-regexp nil "*Makefile rules which must be ignored when building completion list.\n\nFor example if you want to remove all `files rules' set\nit to: \"\\\\.\\\\([aoc]\\\\|s[ao][.0-9]*\\\\)\". " :type '(choice (const :tag "none" :value nil) (const :tag "The `all files' rule" :value "\\.\\([aoc]\\|s[ao][.0-9]*\\)") regexp) :group 'compilation)

(defcustom mode-compile-save-all-p nil "*Non-nil means save ALL the modified buffers without asking\nbefore launching compilation command." :type 'boolean :group 'compilation)

(defcustom mode-compile-always-save-buffer-p nil "*Non-nil means save the current buffer without asking\nbefore launching compilation command." :type 'boolean :group 'compilation)

(defcustom mode-compile-never-edit-command-p nil "*Non-nil means never ask to user to edit the compile command." :type 'boolean :group 'compilation)

(defcustom mode-compile-other-frame-p nil "*Non-nil means compile in another frame.\n\nA new Emacs FRAME is created and the compilation command is executed\nin this other frame.  To specify the frame parameters see also\nvariable `mode-compile-frame-parameters-alist'." :type 'boolean :group 'compilation-frame)

(defcustom mode-compile-before-compile-hook nil "Hook to be run before compile command is executed\nwhen `mode-compile' is invoked." :type 'hook :group 'compilation)

(defcustom mode-compile-after-compile-hook nil "Hook to be run after compile command is executed\nwhen `mode-compile' is invoked." :type 'hook :group 'compilation)

(defcustom mode-compile-before-kill-hook nil "Hook to be run before killing compile command is executed\nwhen `mode-compile-kill' is invoked." :type 'hook :group 'compilation)

(defcustom mode-compile-after-kill-hook nil "Hook to be run after killing compile command is executed\nwhen `mode-compile-kill' is invoked." :type 'hook :group 'compilation)

(defvar mode-compile-chosen-compiler nil "\
*Global variable containing the name of the compiler
which will be used for compiling without makefile.

 Could be used in combination with
 (cc|c++|ada|f77)-default-compiler-options
to automatically choose the compiler specific options.

example:
 (defun my-compiler-get-options()
   (cond
    ((string= mode-compile-chosen-compiler \"gcc\")
      \"-Wall -pedantic-errors\")
    ((string= mode-compile-chosen-compiler \"cc\")
      \"cc options whatever they are...\")
    (t
     (message \"Don't know this compiler: %s\" mode-compile-chosen-compiler)
     (read-string
      (format \"Options for %s compiler: \" mode-compile-chosen-compiler)))))

  (setq cc-default-compiler-options 'my-compiler-get-options)")

(defcustom mode-compile-expert-p nil "*Non nil means `mode-compile' will not speaks too much.\n\nSee also variable variable mode-compile-reading-time." :type 'boolean :group 'compilation)

(defcustom mode-compile-reading-time 1 "*Seconds to wait in verbose mode after printing a message.\n\nIn verbose mode mode-compile print too much messages that it is\nalmost impossible to read them. Just setting this delay leave you the\ntime to read all the messages. If you don't want any delay set it to\n`0'.\n\nSee also function sit-for." :type 'integer :group 'compilation)

(defcustom emacs-lisp-byte-compile-dir-interactive-p t "*Non-nil means when byte-compiling a directory ask for each file\nneeding to be recompiled or not." :type 'boolean :group 'compilation-elisp)

(defconst mode-compile-version "2.28" "\
Current version of mode-compile package.

mode-compile.el,v 2.28 2003/04/01 13:52:47 boubaker Exp
Please send bugs-fixes/contributions/comments to boubaker@cena.fr")

(autoload 'mode-compile-submit-bug-report "mode-compile" "\
*Submit via mail a bug report on mode-compile v2.27." t nil)

(autoload 'mode-compile "mode-compile" "\
*Compile the file in the current buffer with a dynamically built command.

The command is built according to the current major mode the function
was invoked from.

Running this command preceded by universal-argument (\\[universal-argument])
allows remote compilation, the user is prompted for a host name to run the
compilation command on.

Currently know how to compile in:
 `c-mode' ,              -- function cc-compile.
 `java-mode' ,           -- function java-compile.
 `c++-mode',             -- function c++-compile.
 `ada-mode',             -- function ada-compile.
 `fortran-mode',         -- function f77-compile.
 `emacs-lisp-mode'       -- function elisp-compile.
 `lisp-interaction-mode' -- function elisp-compile.
 `makefile-mode'         -- function makefile-compile.
 `dired-mode'            -- function dired-compile.
 `sh-mode'               -- function sh-compile.
 `csh-mode'              -- function csh-compile.
 `zsh-mode'              -- function zsh-compile.
 `perl-mode'             -- function perl-compile.
 `cperl-mode'            -- function perl-compile.
 `tcl-mode'              -- function tcl-compile.
 `python-mode'           -- function python-compile.
 `fundamental-mode'      -- function guess-compile.
 `text-mode'             -- function guess-compile.
 `indented-text-mode'    -- function guess-compile.
 `compilation-mode'      -- function default-compile.
 The function `guess-compile' is called when mode is unknown.

The variable `mode-compile-modes-alist' contain description of known
modes.  The hooks variables `mode-compile-before-compile-hook' and
`mode-compile-after-compile-hook' are run just before and after
invoking the compile command of the mode.

Use the command `mode-compile-kill' (\\[mode-compile-kill]) to abort a
running compilation.

Bound on \\[mode-compile]." t nil)

(autoload 'mode-compile-kill "mode-compile" "\
*Kill the running compilation launched by `mode-compile' (\\[mode-compile]) command.

The compilation command is killed according to the current major mode
the function was invoked from.

Currently know how to kill compilations from:
 `c-mode' ,              -- function kill-compilation.
 `java-mode' ,           -- function kill-compilation.
 `c++-mode' ,            -- function kill-compilation.
 `ada-mode' ,            -- function kill-compilation.
 `fortran-mode' ,        -- function kill-compilation.
 `emacs-lisp-mode'       -- function keyboard-quit.
 `lisp-interaction-mode' -- function keyboard-quit.
 `makefile-mode'         -- function kill-compilation.
 `dired-mode'            -- function kill-compilation.
 `sh-mode'               -- function kill-compilation.
 `csh-mode'              -- function kill-compilation.
 `zsh-mode'              -- function kill-compilation.
 `perl-mode'             -- function kill-compilation.
 `cperl-mode'            -- function kill-compilation.
 `tcl-mode'              -- function kill-compilation.
 `python-mode'           -- function kill-compilation.
 `fundamental-mode'      -- Bound dynamically.
 `text-mode'             -- Bound dynamically.
 `indented-text-mode'    -- Bound dynamically.
 `compilation-mode'      -- function kill-compilation.

The variable `mode-compile-modes-alist' contain description of ALL
known modes.  The hooks variables `mode-compile-before-kill-hook' and
`mode-compile-after-kill-hook' are run just before and after invoking
the kill compile command of the mode.

Bound on \\[mode-compile-kill]." t nil)

;;;***

;;;### (autoloads (modula-2-mode) "modula2" "prog-modes/modula2.el")

(autoload 'modula-2-mode "modula2" "\
This is a mode intended to support program development in Modula-2.
All control constructs of Modula-2 can be reached by typing C-c
followed by the first character of the construct.
\\<m2-mode-map>
  \\[m2-begin] begin         \\[m2-case] case
  \\[m2-definition] definition    \\[m2-else] else
  \\[m2-for] for           \\[m2-header] header
  \\[m2-if] if            \\[m2-module] module
  \\[m2-loop] loop          \\[m2-or] or
  \\[m2-procedure] procedure     Control-c Control-w with
  \\[m2-record] record        \\[m2-stdio] stdio
  \\[m2-type] type          \\[m2-until] until
  \\[m2-var] var           \\[m2-while] while
  \\[m2-export] export        \\[m2-import] import
  \\[m2-begin-comment] begin-comment \\[m2-end-comment] end-comment
  \\[suspend-emacs] suspend Emacs     \\[m2-toggle] toggle
  \\[m2-compile] compile           \\[m2-next-error] next-error
  \\[m2-link] link

   `m2-indent' controls the number of spaces for each indentation.
   `m2-compile-command' holds the command to compile a Modula-2 program.
   `m2-link-command' holds the command to link a Modula-2 program." t nil)

;;;***

;;;### (autoloads (pascal-mode) "pascal" "prog-modes/pascal.el")

(autoload 'pascal-mode "pascal" "\
Major mode for editing Pascal code. \\<pascal-mode-map>
TAB indents for Pascal code.  Delete converts tabs to spaces as it moves back.

\\[pascal-complete-word] completes the word around current point with respect to position in code
\\[pascal-show-completions] shows all possible completions at this point.

Other useful functions are:

\\[pascal-mark-defun]	- Mark function.
\\[pascal-insert-block]	- insert begin ... end;
\\[pascal-star-comment]	- insert (* ... *)
\\[pascal-comment-area]	- Put marked area in a comment, fixing nested comments.
\\[pascal-uncomment-area]	- Uncomment an area commented with \\[pascal-comment-area].
\\[pascal-beg-of-defun]	- Move to beginning of current function.
\\[pascal-end-of-defun]	- Move to end of current function.
\\[pascal-goto-defun]	- Goto function prompted for in the minibuffer.
\\[pascal-outline]	- Enter pascal-outline-mode (see also pascal-outline).

Variables controlling indentation/edit style:

 pascal-indent-level      (default 3)
    Indentation of Pascal statements with respect to containing block.
 pascal-case-indent       (default 2)
    Indentation for case statements.
 pascal-auto-newline      (default nil)
    Non-nil means automatically newline after semicolons and the punctuation
    mark after an end.
 pascal-tab-always-indent (default t)
    Non-nil means TAB in Pascal mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 pascal-auto-endcomments  (default t)
    Non-nil means a comment { ... } is set after the ends which ends cases and
    functions. The name of the function or case will be set between the braces.
 pascal-auto-lineup       (default t)
    List of contexts where auto lineup of :'s or ='s should be done.

See also the user variables pascal-type-keywords, pascal-start-keywords and
pascal-separator-keywords.

Turning on Pascal mode calls the value of the variable pascal-mode-hook with
no args, if that value is non-nil." t nil)
(add-to-list 'auto-mode-alist '("\\.p\\(?:as\\)?\\'" . pascal-mode))

;;;***

;;;### (autoloads nil "php-mode" "prog-modes/php-mode.el")

(defcustom php-file-patterns (list "\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'") "*List of file patterns for which to automatically invoke php-mode." :type '(repeat (regexp :tag "Pattern")) :group 'php)
(autoload 'php-mode "php-mode" nil t)

(let ((php-file-patterns-temp php-file-patterns)) (while php-file-patterns-temp (add-to-list 'auto-mode-alist (cons (car php-file-patterns-temp) 'php-mode)) (setq php-file-patterns-temp (cdr php-file-patterns-temp))))

;;;***

;;;### (autoloads (postscript-mode) "postscript" "prog-modes/postscript.el")

(autoload 'postscript-mode "postscript" "\
Major mode for editing PostScript files.

\\[ps-execute-buffer] will send the contents of the buffer to the NeWS
server using psh(1).  \\[ps-execute-region] sends the current region.
\\[ps-shell] starts an interactive psh(1) window which will be used for
subsequent \\[ps-execute-buffer] or \\[ps-execute-region] commands.

In this mode, TAB and \\[indent-region] attempt to indent code
based on the position of {}, [], and begin/end pairs.  The variable
ps-indent-level controls the amount of indentation used inside
arrays and begin/end pairs.  

\\{ps-mode-map}

\\[postscript-mode] calls the value of the variable postscript-mode-hook 
with no args, if that value is non-nil." t nil)
(add-to-list 'auto-mode-alist '("\\.c?ps\\'" . postscript-mode))

;;;***

;;;### (autoloads (run-prolog inferior-prolog-mode prolog-mode) "prolog" "prog-modes/prolog.el")

(autoload 'prolog-mode "prolog" "\
Major mode for editing Prolog code for Prologs.
Blank lines and `%%...' separate paragraphs.  `%'s start comments.
Commands:
\\{prolog-mode-map}
Entry to this mode calls the value of `prolog-mode-hook'
if that value is non-nil." t nil)

(autoload 'inferior-prolog-mode "prolog" "\
Major mode for interacting with an inferior Prolog process.

The following commands are available:
\\{inferior-prolog-mode-map}

Entry to this mode calls the value of `prolog-mode-hook' with no arguments,
if that value is non-nil.  Likewise with the value of `comint-mode-hook'.
`prolog-mode-hook' is called after `comint-mode-hook'.

You can send text to the inferior Prolog from other buffers
using the commands `process-send-region', `process-send-string' and
\\[prolog-consult-region].

Commands:
Tab indents for Prolog; with argument, shifts rest
 of expression rigidly with the current line.
Paragraphs are separated only by blank lines and '%%'.
'%'s start comments.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
\\[comint-kill-input] and \\[backward-kill-word] are kill commands, imitating normal Unix input editing.
\\[comint-interrupt-subjob] interrupts the shell or its current subjob if any.
\\[comint-stop-subjob] stops. \\[comint-quit-subjob] sends quit signal." t nil)

(autoload 'run-prolog "prolog" "\
Run an inferior Prolog process, input and output via buffer *prolog*." t nil)
(add-to-list 'auto-mode-alist '("\\.prolog\\'" . prolog-mode))

;;;***

;;;### (autoloads (rexx-mode) "rexx-mode" "prog-modes/rexx-mode.el")

(autoload 'rexx-mode "rexx-mode" "\
Major mode for editing REXX code.
\\{rexx-mode-map}

Variables controlling indentation style:
 rexx-indent
	The basic indentation for do-blocks.
 rexx-end-indent
	The relative offset of the \"end\" statement. 0 places it in the
	same column as the statements of the block. Setting it to the same
	value as rexx-indent places the \"end\" under the do-line.
 rexx-cont-indent
	The indention for lines following \"then\", \"else\" and \",\"
	(continued) lines.
 rexx-tab-always-indent
	Non-nil means TAB in REXX mode should always reindent the current 
	line, regardless of where in the line the point is when the TAB
	command is used.

If you have set rexx-end-indent to a nonzero value, you probably want to
remap RETURN to rexx-indent-newline-indent. It makes sure that lines
indents correctly when you press RETURN.

An extensive abbreviation table consisting of all the keywords of REXX are
supplied. Expanded keywords are converted into upper case making it
easier to distinguish them. To use this feature the buffer must be in
abbrev-mode. (See example below.)

Turning on REXX mode calls the value of the variable rexx-mode-hook with
no args, if that value is non-nil.

For example:
\(setq rexx-mode-hook '(lambda ()
			(setq rexx-indent 4)
			(setq rexx-end-indent 4)
			(setq rexx-cont-indent 4)
			(local-set-key \"\\C-m\" 'rexx-indent-newline-indent)
			(abbrev-mode 1)
			))

will make the END aligned with the DO/SELECT. It will indent blocks and
IF-statements four steps and make sure that the END jumps into the
correct position when RETURN is pressed. Finally it will use the abbrev
table to convert all REXX keywords into upper case." t nil)
(add-to-list 'interpreter-mode-alist '("rexx" . rexx-mode))

;;;***

;;;### (autoloads (rpm-spec-mode) "rpm-spec-mode" "prog-modes/rpm-spec-mode.el")

(autoload 'rpm-spec-mode "rpm-spec-mode" "\
Major mode for editing RPM spec files.
This is much like C mode except for the syntax of comments.  It uses
the same keymap as C mode and has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on RPM spec mode calls the value of the variable `rpm-spec-mode-hook'
with no args, if that value is non-nil." t nil)
(add-to-list 'auto-mode-alist '("\\.spec\\(\\.in\\)?$" . rpm-spec-mode))

;;;***

;;;### (autoloads (simula-mode) "simula" "prog-modes/simula.el")

(autoload 'simula-mode "simula" "\
Major mode for editing SIMULA code.
\\{simula-mode-map}
Variables controlling indentation style:
 simula-tab-always-indent
    Non-nil means TAB in SIMULA mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 simula-indent-level
    Indentation of SIMULA statements with respect to containing block.
 simula-substatement-offset
    Extra indentation after DO, THEN, ELSE, WHEN and OTHERWISE.
 simula-continued-statement-offset 3
    Extra indentation for lines not starting a statement or substatement,
    e.g. a nested FOR-loop.  If value is a list, each line in a multiple-
    line continued statement will have the car of the list extra indentation
    with respect to the previous line of the statement.
 simula-label-offset -4711
    Offset of SIMULA label lines relative to usual indentation.
 simula-if-indent '(0 . 0)
    Extra indentation of THEN and ELSE with respect to the starting IF.
    Value is a cons cell, the car is extra THEN indentation and the cdr
    extra ELSE indentation.  IF after ELSE is indented as the starting IF.
 simula-inspect-indent '(0 . 0)
    Extra indentation of WHEN and OTHERWISE with respect to the
    corresponding INSPECT.  Value is a cons cell, the car is
    extra WHEN indentation and the cdr extra OTHERWISE indentation.
 simula-electric-indent nil
    If this variable is non-nil, `simula-indent-line'
    will check the previous line to see if it has to be reindented.
 simula-abbrev-keyword 'upcase
    Determine how SIMULA keywords will be expanded.  Value is one of
    the symbols `upcase', `downcase', `capitalize', (as in) `abbrev-table',
    or nil if they should not be changed.
 simula-abbrev-stdproc 'abbrev-table
    Determine how standard SIMULA procedure and class names will be
    expanded. Value is one of the symbols `upcase', `downcase', `capitalize',
    (as in) `abbrev-table', or nil if they should not be changed.

Turning on SIMULA mode calls the value of the variable simula-mode-hook
with no arguments, if that value is non-nil

Warning: simula-mode-hook should not read in an abbrev file without calling
the function simula-install-standard-abbrevs afterwards, preferably not
at all." t nil)

;;;***

;;;### (autoloads (sql-linter sql-db2 sql-interbase sql-postgres sql-ms sql-ingres sql-solid sql-mysql sql-sqlite sql-informix sql-sybase sql-oracle sql-product-interactive sql-mode sql-help sql-add-product-keywords) "sql" "prog-modes/sql.el")

(autoload 'sql-add-product-keywords "sql" "\
Add highlighting KEYWORDS for SQL PRODUCT.

PRODUCT should be a symbol, the name of a sql product, such as
`oracle'.  KEYWORDS should be a list; see the variable
`font-lock-keywords'.  By default they are added at the beginning
of the current highlighting list.  If optional argument APPEND is
`set', they are used to replace the current highlighting list.
If APPEND is any other non-nil value, they are added at the end
of the current highlighting list.

For example:

 (sql-add-product-keywords 'ms
  '((\"\\\\b\\\\w+_t\\\\b\" . font-lock-type-face)))

adds a fontification pattern to fontify identifiers ending in
`_t' as data types." nil nil)

(autoload 'sql-help "sql" "\
Show short help for the SQL modes.

Use an entry function to open an interactive SQL buffer.  This buffer is
usually named `*SQL*'.  The name of the major mode is SQLi.

Use the following commands to start a specific SQL interpreter:

    PostGres: \\[sql-postgres]
    MySQL: \\[sql-mysql]
    SQLite: \\[sql-sqlite]

Other non-free SQL implementations are also supported:

    Solid: \\[sql-solid]
    Oracle: \\[sql-oracle]
    Informix: \\[sql-informix]
    Sybase: \\[sql-sybase]
    Ingres: \\[sql-ingres]
    Microsoft: \\[sql-ms]
    DB2: \\[sql-db2]
    Interbase: \\[sql-interbase]
    Linter: \\[sql-linter]

But we urge you to choose a free implementation instead of these.

Once you have the SQLi buffer, you can enter SQL statements in the
buffer.  The output generated is appended to the buffer and a new prompt
is generated.  See the In/Out menu in the SQLi buffer for some functions
that help you navigate through the buffer, the input history, etc.

If you have a really complex SQL statement or if you are writing a
procedure, you can do this in a separate buffer.  Put the new buffer in
`sql-mode' by calling \\[sql-mode].  The name of this buffer can be
anything.  The name of the major mode is SQL.

In this SQL buffer (SQL mode), you can send the region or the entire
buffer to the interactive SQL buffer (SQLi mode).  The results are
appended to the SQLi buffer without disturbing your SQL buffer." t nil)

(autoload 'sql-mode "sql" "\
Major mode to edit SQL.

You can send SQL statements to the SQLi buffer using
\\[sql-send-region].  Such a buffer must exist before you can do this.
See `sql-help' on how to create SQLi buffers.

\\{sql-mode-map}
Customization: Entry to this mode runs the `sql-mode-hook'.

When you put a buffer in SQL mode, the buffer stores the last SQLi
buffer created as its destination in the variable `sql-buffer'.  This
will be the buffer \\[sql-send-region] sends the region to.  If this
SQLi buffer is killed, \\[sql-send-region] is no longer able to
determine where the strings should be sent to.  You can set the
value of `sql-buffer' using \\[sql-set-sqli-buffer].

For information on how to create multiple SQLi buffers, see
`sql-interactive-mode'.

Note that SQL doesn't have an escape character unless you specify
one.  If you specify backslash as escape character in SQL,
you must tell Emacs.  Here's how to do that in your `~/.emacs' file:

\(add-hook 'sql-mode-hook
          (lambda ()
	    (modify-syntax-entry ?\\\\ \".\" sql-mode-syntax-table)))" t nil)

(autoload 'sql-product-interactive "sql" "\
Run product interpreter as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-oracle "sql" "\
Run sqlplus by Oracle as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-oracle-program'.  Login uses
the variables `sql-user', `sql-password', and `sql-database' as
defaults, if set.  Additional command line parameters can be stored in
the list `sql-oracle-options'.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-oracle].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-sybase "sql" "\
Run isql by SyBase as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-sybase-program'.  Login uses
the variables `sql-server', `sql-user', `sql-password', and
`sql-database' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-sybase-options'.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-sybase].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-informix "sql" "\
Run dbaccess by Informix as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-informix-program'.  Login uses
the variable `sql-database' as default, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-informix].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-sqlite "sql" "\
Run sqlite as an inferior process.

SQLite is free software.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-sqlite-program'.  Login uses
the variables `sql-user', `sql-password', `sql-database', and
`sql-server' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-sqlite-options'.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-sqlite].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-mysql "sql" "\
Run mysql by TcX as an inferior process.

Mysql versions 3.23 and up are free software.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-mysql-program'.  Login uses
the variables `sql-user', `sql-password', `sql-database', and
`sql-server' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-mysql-options'.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-mysql].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-solid "sql" "\
Run solsql by Solid as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-solid-program'.  Login uses
the variables `sql-user', `sql-password', and `sql-server' as
defaults, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-solid].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-ingres "sql" "\
Run sql by Ingres as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-ingres-program'.  Login uses
the variable `sql-database' as default, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-ingres].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-ms "sql" "\
Run osql by Microsoft as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-ms-program'.  Login uses the
variables `sql-user', `sql-password', `sql-database', and `sql-server'
as defaults, if set.  Additional command line parameters can be stored
in the list `sql-ms-options'.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-ms].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-postgres "sql" "\
Run psql by Postgres as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-postgres-program'.  Login uses
the variables `sql-database' and `sql-server' as default, if set.
Additional command line parameters can be stored in the list
`sql-postgres-options'.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-postgres].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.  If your output lines end with ^M,
your might try undecided-dos as a coding system.  If this doesn't help,
Try to set `comint-output-filter-functions' like this:

\(setq comint-output-filter-functions (append comint-output-filter-functions
					     '(comint-strip-ctrl-m)))

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-interbase "sql" "\
Run isql by Interbase as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-interbase-program'.  Login
uses the variables `sql-user', `sql-password', and `sql-database' as
defaults, if set.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-interbase].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-db2 "sql" "\
Run db2 by IBM as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-db2-program'.  There is not
automatic login.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

If you use \\[sql-accumulate-and-indent] to send multiline commands to
db2, newlines will be escaped if necessary.  If you don't want that, set
`comint-input-sender' back to `comint-simple-send' by writing an after
advice.  See the elisp manual for more information.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-db2].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)

(autoload 'sql-linter "sql" "\
Run inl by RELEX as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-linter-program' - usually `inl'.
Login uses the variables `sql-user', `sql-password', `sql-database' and
`sql-server' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-linter-options'. Run inl -h to get help on
parameters.

`sql-database' is used to set the LINTER_MBX environment variable for
local connections, `sql-server' refers to the server name from the
`nodetab' file for the network connection (dbc_tcp or friends must run
for this to work).  If `sql-password' is an empty string, inl will use
an empty password.

The buffer is put in sql-interactive-mode, giving commands for sending
input.  See `sql-interactive-mode'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)" t nil)
(add-to-list 'auto-mode-alist '("\\.sql$" . sql-mode))

;;;***

;;;### (autoloads (tcl-help-on-word inferior-tcl tcl-mode) "tcl" "prog-modes/tcl.el")

(autoload 'tcl-mode "tcl" "\
Major mode for editing Tcl code.
Expression and list commands understand all Tcl brackets.
Tab indents for Tcl code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Variables controlling indentation style:
  `tcl-indent-level'
    Indentation of Tcl statements within surrounding block.
  `tcl-continued-indent-level'
    Indentation of continuation line relative to first line of command.

Variables controlling user interaction with mode (see variable
documentation for details):
  `tcl-tab-always-indent'
    Controls action of TAB key.
  `tcl-auto-newline'
    Non-nil means automatically newline before and after braces, brackets,
    and semicolons inserted in Tcl code.
  `tcl-electric-hash-style'
    Controls action of `#' key.
  `tcl-use-hairy-comment-detector'
    If t, use more complicated, but slower, comment detector.
    This variable is only used in Emacs 19.
  `tcl-use-smart-word-finder'
    If not nil, use a smarter, Tcl-specific way to find the current
    word when looking up help on a Tcl command.

Turning on Tcl mode calls the value of the variable `tcl-mode-hook'
with no args, if that value is non-nil.  Read the documentation for
`tcl-mode-hook' to see what kinds of interesting hook functions
already exist.

Commands:
\\{tcl-mode-map}" t nil)

(autoload 'inferior-tcl "tcl" "\
Run inferior Tcl process.
Prefix arg means enter program name interactively.
See documentation for function `inferior-tcl-mode' for more information." t nil)

(autoload 'tcl-help-on-word "tcl" "\
Get help on Tcl command.  Default is word at point.
Prefix argument means invert sense of `tcl-use-smart-word-finder'." t nil)
(add-to-list 'auto-mode-alist '("\\.\\(?:tcl\\|exp\\)\\'" . tcl-mode))
(add-to-list 'interpreter-mode-alist '("^#!.*\\b\\(scope\\|wish\\|tcl\\|tclsh\\|expect\\)" . tcl-mode))

;;;***

;;;### (autoloads (teco-command) "teco" "prog-modes/teco.el")

(autoload 'teco-command "teco" "\
Read and execute a Teco command string." t nil)

;;;***

;;;### (autoloads (uil-mode) "uil-mode" "prog-modes/uil-mode.el")

(autoload 'uil-mode "uil-mode" "\
Major mode for editing UIL files.
This is much like C mode except for the syntax of comments.  It uses
the same keymap as C mode and has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on uil mode calls the value of the variable `uil-mode-hook'
with no args, if that value is non-nil." t nil)
(add-to-list 'auto-mode-alist '("\\.uil$" . uil-mode))

;;;***

;;;### (autoloads (verilog-mode verilog-customize verilog-version) "verilog-mode" "prog-modes/verilog-mode.el")
(add-to-list 'auto-mode-alist '("\\.vh?\\'" . verilog-mode))
(add-to-list 'auto-mode-alist '("\\.dv\\'" . verilog-mode))
(add-to-list 'auto-mode-alist '("\\.vlog\\'" . verilog-mode))
(add-to-list 'auto-mode-alist '("\\.verilog\\'" . verilog-mode))

(autoload 'verilog-version "verilog-mode" "\
Inform caller of the version of this file." t nil)

(autoload 'verilog-customize "verilog-mode" "\
Link to customize screen for Verilog." t nil)

(autoload 'verilog-mode "verilog-mode" "\
Major mode for editing Verilog code.
\\<verilog-mode-map>

NEWLINE, TAB indents for Verilog code.
Delete converts tabs to spaces as it moves back.
Supports highlighting.

Variables controlling indentation/edit style:

 variable `verilog-indent-level'      (default 3)
    Indentation of Verilog statements with respect to containing block.
 `verilog-indent-level-module'        (default 3)
    Absolute indentation of Module level Verilog statements.
    Set to 0 to get initial and always statements lined up
    on the left side of your screen.
 `verilog-indent-level-declaration'   (default 3)
    Indentation of declarations with respect to containing block.
    Set to 0 to get them list right under containing block.
 `verilog-indent-level-behavioral'    (default 3)
    Indentation of first begin in a task or function block
    Set to 0 to get such code to lined up underneath the task or function keyword
 `verilog-indent-level-directive'     (default 1)
    Indentation of `ifdef/`endif blocks
 `verilog-cexp-indent'              (default 1)
    Indentation of Verilog statements broken across lines i.e.:
    if (a)
     begin
 `verilog-case-indent'              (default 2)
    Indentation for case statements.
 `verilog-auto-newline'             (default nil)
    Non-nil means automatically newline after semicolons and the punctuation
    mark after an end.
 `verilog-auto-indent-on-newline'   (default t)
    Non-nil means automatically indent line after newline
 `verilog-tab-always-indent'        (default t)
    Non-nil means TAB in Verilog mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 `verilog-indent-begin-after-if'    (default t)
    Non-nil means to indent begin statements following a preceding
    if, else, while, for and repeat statements, if any.  otherwise,
    the begin is lined up with the preceding token.  If t, you get:
      if (a)
         begin // amount of indent based on `verilog-cexp-indent'
    otherwise you get:
      if (a)
      begin
 `verilog-auto-endcomments'         (default t)
    Non-nil means a comment /* ... */ is set after the ends which ends
      cases, tasks, functions and modules.
    The type and name of the object will be set between the braces.
 `verilog-minimum-comment-distance' (default 10)
    Minimum distance (in lines) between begin and end required before a comment
    will be inserted.  Setting this variable to zero results in every
    end acquiring a comment; the default avoids too many redundant
    comments in tight quarters.
 `verilog-auto-lineup'              (default `(all))
    List of contexts where auto lineup of :'s or ='s should be done.

Turning on Verilog mode calls the value of the variable `verilog-mode-hook' with
no args, if that value is non-nil.
Other useful functions are:
\\[verilog-complete-word]	-complete word with appropriate possibilities
   (functions, verilog keywords...)
\\[verilog-comment-region]	- Put marked area in a comment, fixing
   nested comments.
\\[verilog-uncomment-region]	- Uncomment an area commented with \\[verilog-comment-region].
\\[verilog-insert-block]	- insert begin ... end;
\\[verilog-star-comment]	- insert /* ... */
\\[verilog-mark-defun]	- Mark function.
\\[verilog-beg-of-defun]	- Move to beginning of current function.
\\[verilog-end-of-defun]	- Move to end of current function.
\\[verilog-label-be]	- Label matching begin ... end, fork ... join
  and case ... endcase statements;

\\[verilog-sk-always]  Insert a always @(AS) begin .. end block
\\[verilog-sk-begin]  Insert a begin .. end block
\\[verilog-sk-case]  Insert a case block, prompting for details
\\[verilog-sk-else]  Insert an else begin .. end block
\\[verilog-sk-for]  Insert a for (...) begin .. end block, prompting for details
\\[verilog-sk-generate]  Insert a generate .. endgenerate block
\\[verilog-sk-header]  Insert a nice header block at the top of file
\\[verilog-sk-initial]  Insert an initial begin .. end block
\\[verilog-sk-fork]  Insert a fork begin .. end .. join block
\\[verilog-sk-module]  Insert a module .. (/*AUTOARG*/);.. endmodule block
\\[verilog-sk-primitive]  Insert a primitive .. (.. );.. endprimitive block
\\[verilog-sk-interface]  Insert an interface .. (.. );.. endinterface block
\\[verilog-sk-repeat]  Insert a repeat (..) begin .. end block
\\[verilog-sk-specify]  Insert a specify .. endspecify block
\\[verilog-sk-task]  Insert a task .. begin .. end endtask block
\\[verilog-sk-while]  Insert a while (...) begin .. end block, prompting for details
\\[verilog-sk-casex]  Insert a casex (...) item: begin.. end endcase block, prompting for details
\\[verilog-sk-casez]  Insert a casez (...) item: begin.. end endcase block, prompting for details
\\[verilog-sk-if]  Insert an if (..) begin .. end block
\\[verilog-sk-else-if]  Insert an else if (..) begin .. end block
\\[verilog-sk-comment]  Insert a comment block
\\[verilog-sk-assign]  Insert an assign .. = ..; statement
\\[verilog-sk-function]  Insert a function .. begin .. end endfunction block
\\[verilog-sk-input]  Insert an input declaration, prompting for details
\\[verilog-sk-output]  Insert an output declaration, prompting for details
\\[verilog-sk-state-machine]  Insert a state machine definition, prompting for details!
\\[verilog-sk-inout]  Insert an inout declaration, prompting for details
\\[verilog-sk-wire]  Insert a wire declaration, prompting for details
\\[verilog-sk-reg]  Insert a register declaration, prompting for details" t nil)

;;;***

;;;### (autoloads (vrml-mode) "vrml-mode" "prog-modes/vrml-mode.el")

(autoload 'vrml-mode "vrml-mode" "\
Major mode for editing VRML code.
Expression and list commands understand all VRML brackets.
Tab indents for VRML code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Variables controlling indentation style:
  vrml-indent-level
    Indentation of VRML statements within surrounding block.

Variables controlling user interaction with mode (see variable
documentation for details):
  vrml-tab-always-indent
    Controls action of TAB key.
  vrml-auto-newline
    Non-nil means automatically newline before and after braces
    inserted in VRML code.

Turning on VRML mode calls the value of the variable `vrml-mode-hook'
with no args, if that value is non-nil.  Read the documentation for
`vrml-mode-hook' to see what kinds of interesting hook functions
already exist.

Commands:
\\{vrml-mode-map}" t nil)
(add-to-list 'auto-mode-alist '("\\.wrl\\'" . vrml-mode))

;;;***

(provide 'prog-modes-autoloads)
