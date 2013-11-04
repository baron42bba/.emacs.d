;;; DO NOT MODIFY THIS FILE
(if (featurep 'sh-script-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "sh-script/_pkg.el")

(package-provide 'sh-script :version 1.21 :author-version "2.0f" :type 'regular)

;;;***

;;;### (autoloads (executable-make-buffer-file-executable-if-script-p executable-self-display executable-set-magic executable-interpret executable-find executable-command-find-posix-p) "executable" "sh-script/executable.el")

(autoload 'executable-command-find-posix-p "executable" "\
Check if PROGRAM handles arguments Posix-style.
If PROGRAM is non-nil, use that instead of \"find\"." nil nil)

(autoload 'executable-find "executable" "\
Search for COMMAND in `exec-path' and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'." nil nil)

(autoload 'executable-interpret "executable" "\
Run script with user-specified args, and collect output in a buffer.
While script runs asynchronously, you can use the \\[next-error]
command to find the next error.  The buffer is also in `comint-mode' and
`compilation-shell-minor-mode', so that you can answer any prompts." t nil)

(autoload 'executable-set-magic "executable" "\
Set this buffer's interpreter to INTERPRETER with optional ARGUMENT.
The variables `executable-magicless-file-regexp', `executable-prefix',
`executable-insert', `executable-query' and `executable-chmod' control
when and how magic numbers are inserted or replaced and scripts made
executable." t nil)

(autoload 'executable-self-display "executable" "\
Turn a text file into a self-displaying Un*x command.
The magic number of such a command displays all lines but itself." t nil)

(autoload 'executable-make-buffer-file-executable-if-script-p "executable" "\
Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes." nil nil)

;;;***

;;;### (autoloads (sh-mode) "sh-script" "sh-script/sh-script.el")

(put 'sh-mode 'mode-class 'special)

(autoload 'sh-mode "sh-script" "Major mode for editing shell scripts.\nThis mode works for many shells, since they all have roughly the same syntax,\nas far as commands, arguments, variables, pipes, comments etc. are concerned.\nUnless the file's magic number indicates the shell, your usual shell is\nassumed.  Since filenames rarely give a clue, they are not further analyzed.\n\nThis mode adapts to the variations between shells (see `sh-set-shell') by\nmeans of an inheritance based feature lookup (see `sh-feature').  This\nmechanism applies to all variables (including skeletons) that pertain to\nshell-specific features.\n\nThe default style of this mode is that of Rosenblatt's Korn shell book.\nThe syntax of the statements varies with the shell being used.  The\nfollowing commands are available, based on the current shell's syntax:\n\n\\[sh-case]	 case statement\n\\[sh-for]	 for loop\n\\[sh-function]	 function definition\n\\[sh-if]	 if statement\n\\[sh-indexed-loop]	 indexed loop from 1 to n\n\\[sh-while-getopts]	 while getopts loop\n\\[sh-repeat]	 repeat loop\n\\[sh-select]	 select loop\n\\[sh-until]	 until loop\n\\[sh-while]	 while loop\n\nFor sh and rc shells indentation commands are:\n\\[sh-show-indent]	Show the variable controlling this line's indentation.\n\\[sh-set-indent]	Set then variable controlling this line's indentation.\n\\[sh-learn-line-indent]	Change the indentation variable so this line\nwould indent to the way it currently is.\n\\[sh-learn-buffer-indent]  Set the indentation variables so the\nbuffer indents as it currently is indented.\n\n\n\\[backward-delete-char-untabify]	 Delete backward one position, even if it was a tab.\n\\[sh-newline-and-indent]	 Delete unquoted space and indent new line same as this one.\n\\[sh-end-of-command]	 Go to end of successive commands.\n\\[sh-beginning-of-command]	 Go to beginning of successive commands.\n\\[sh-set-shell]	 Set this buffer's shell, and maybe its magic number.\n\\[sh-execute-region]	 Have optional header and region be executed in a subshell.\n\n\\[sh-maybe-here-document]	 Without prefix, following an unquoted < inserts here document.\n{, (, [, ', \", `\n	Unless quoted with \\, insert the pairs {}, (), [], or '', \"\", ``.\n\nIf you generally program a shell different from your login shell you can\nset `sh-shell-file' accordingly.  If your shell's file name doesn't correctly\nindicate what shell it is use `sh-alias-alist' to translate.\n\nIf your shell gives error messages with line numbers, you can use \\[executable-interpret]\nwith your script for an edit-interpret-debug cycle." t nil)

(defalias 'shell-script-mode 'sh-mode)
(add-to-list 'auto-mode-alist '("/\\.\\(?:bash_\\|z\\)?\\(profile\\|login\\|logout\\)\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/\\.\\(?:[ckz]sh\\|bash\\|tcsh\\|es\\|xinit\\|startx\\)rc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/\\.\\(?:[kz]shenv\\|xsession\\)\\'" . sh-mode))
(setq auto-mode-alist (append auto-mode-alist '(("\\.m?spec$" . sh-mode))))
(add-to-list 'auto-mode-alist '("\\.\\(?:[ckz]?sh\\|shar\\)\\'" . sh-mode))
(add-to-list 'interpreter-mode-alist '("^#!.*csh" . sh-mode))
(add-to-list 'interpreter-mode-alist '("^#!.*sh\\b" . sh-mode))
(add-to-list 'interpreter-mode-alist '("^:" . sh-mode))

;;;***

(provide 'sh-script-autoloads)
