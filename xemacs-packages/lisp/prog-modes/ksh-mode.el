;; ksh-mode.el --- sh (ksh, bash) script editing mode for GNU Emacs.

;; Copyright (C) 1992-1999 Gary Ellison.

;; This file is compatible with GNU Emacs but is not part of the official
;; distribution.
;;
;; This program is free software; you can redistribute it and/or modify
;; it at your option.
;;   
;; Gary Ellison makes no representations about the suitability
;; of this software for any purpose.  It is provided "as is" without
;; express or implied warranty.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;; $Source: /usr/CVSroot/XEmacs/packages/xemacs-packages/prog-modes/ksh-mode.el,v $ --  
;;
;; LCD Archive Entry:
;; ksh-mode|Gary Ellison|gfe@interhack.net
;; |Mode for editing sh/ksh/bash scripts
;; |$Date: 2000/10/06 09:12:14 $|$Revision: 1.4 $|~/modes/ksh-mode.el.Z|

;; Author: Gary Ellison <gfe@interhack.net>
;;                   Interhack Posse
;;
;; Maintainer: Gary Ellison <gfe@interhack.net>
;; Created: Fri Jun 19
;; $Revision: 1.4 $
;; Keywords: shell, korn, bourne, sh, ksh, bash
;;
;; Delta On   $Date: 2000/10/06 09:12:14 $
;; Last Modified By: Gary Ellison
;; Status          : Highly Functional
;;

;;; Commentary:

;;
;; Description:
;;   sh, ksh, and bash script editing commands for emacs.
;; 
;; Installation:
;;   Put ksh-mode.el in some directory in your load-path.
;;   Refer to the installation section of ksh-mode's function definition.
;;
;; Usage:
;;   This major mode assists shell script writers with indentation
;;   control and control structure construct matching in much the same
;;   fashion as other programming language modes. Invoke describe-mode
;;   for more information.
;; 
;; Bugs:
;;   When the ksh-align-to-keyword is non-nil and the nester
;;   is a multi-command expression with a compound command
;;   the lines following the compound end will align incorrectly
;;   to the compound command instead of it's current indentation.
;;   The fix will probably require the detection of syntax elements
;;   in the nesting line.
;;   
;;   Function ending brace "}" must be on a separate line for indent-line
;;   to do the right thing.
;;
;;   Explicit function definition matching will proclaim in the minibuffer
;;   "No matching compound command" followed by "Matched ... "
;;
;;   indent-for-comment fails to recognize a comment starting in column 0,
;;   hence it moves the comment-start in comment-column.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HISTORY 
;; 13-May-99            Gary Ellison
;;    Removed some testing baggage which leaked into previous release.
;;
;; 29-Apr-99            Gary Ellison
;;    I moved and forgot to submit a forwarding address to the codeoffice.
;;    Minor mucks with font-lock.
;;
;; 28-Feb-97            Kevin Blake <kblake@nortel.ca>
;;    font-lock enhancements
;;
;; 4-Feb-97             Jim Berry <jberry@FRB.GOV>
;;    Converted from use of font-lock-keywords to font-lock-defaults.
;;
;; 6-Apr-96             Gary Ellison <gary.f.ellison@att.com>
;;    Depreciated font-lock-doc-string-face.
;;    Narly keywords inside strings bug fixed. 
;;
;; 8-Aug-95		Jack Repenning <jackr@sgi.com>
;;    Fix documentation of `ksh-align-to-keyword' to conform to the 23
;;    Feb default change.  Search for keywords obeying case, since the
;;    shell does.
;;
;; 23-Feb-1995		Gary Ellison	
;;    Merged Jonathan Stigelman <Stig@hackvan.com> into 2.5 souce.
;;
;; 23 Feb 1995          Jonathan Stigelman <Stig@hackvan.com>
;;    Reshuffled documentation to make the format more consistant with other
;;    elisp.  Added autoload and removed autoloading instructions from the
;;    ksh-mode docstring.  Changed default value for `ksh-align-to-keyword'
;;    to nil because it doesn't work properly.
;;
;; 2-Aug-1994		Gary Ellison	
;;    Last Modified: Mon Jun 13 16:52:55 1994 #29 (Gary Ellison)
;;    - Syntax table modifications to better support sexp navigation and
;;      parsing.
;;    - Fixed keyword regexps. Keywords were not being recoginized on the
;;      same line as " ' `.
;;
;; 13-Jun-1994		Gary Ellison	
;;    Last Modified: Wed Mar 30 14:12:26 1994 #28 (Gary Ellison)
;;    - Minor excursion problem fixed in ksh-indent-command.
;;
;; 30-Mar-1994		Gary Ellison	
;;    Last Modified: Fri Mar 25 15:42:29 1994 #25 (Gary Ellison)
;;    - Implement user customizable ksh-comment-regexp.
;;    - Make the keyword vs line indentation alignment customizable
;;      by calling ksh-align-to-keyword based on variable of same
;;      name. (If the code is obfuscated or convoluted I can attribute
;;      this to a severe head cold and not malice :)
;;
;; 25-Mar-1994		Gary Ellison	
;;    Last Modified: Fri Feb  4 13:06:30 1994 #23 (Gary Ellison)
;;    - Nest relative to the line indentation not the keywords
;;      column.
;;
;; 4-Feb-1994		Gary Ellison	
;;    Last Modified: Wed Nov 10 10:03:01 1993 #18 (Gary Ellison)
;;    - Add direct support for font-lock-mode. Thanks Espen Skoglund
;;      for the regular expressions.
;;
;; 10-Nov-1993		Gary Ellison	
;;    Last Modified: Tue Oct 12 15:23:06 1993 #17 (Gary Ellison)
;;    Fix message on ksh-match-and-tell to not get invalid format
;;    when a % appears in the string.
;;
;; 12-Oct-1993		Espen Skoglund <espensk@stud.cs.uit.no>.
;;    Last Modified: Tue Oct 12 15:03:01 1993 #16 (Gary Ellison)
;;    Apply Line continuation patch supplied by Espen Skoglund
;;
;; 1-Sep-1993		Gary Ellison	
;;    Last Modified: Tue Aug 17 17:18:18 1993 #14 (Gary Ellison)
;;    Get rid of this-line hack in ksh-get-nester-column.
;;
;; 17-Aug-1993		Gary Ellison	
;;    Last Modified: Mon Jun 21 14:00:43 1993 #13 (Gary Ellison)
;;    Code uses builtin current-indentation instead of lisp defun
;;    ksh-indentation-on-this-line (thanks to Tom Tromey).
;;    More and better doc strings.
;;
;; 5-Aug-1993		Tom Tromey <tromey@cns.caltech.edu>
;;    Last Modified: Thu Aug  5 11:09:12 1993 #12 (Tom Tromey)
;;    ksh-indent-region skips blank lines.  Uses let binding instead
;;    of setq.  No longer marks buffer modified if indentation
;;    doesn't change. 
;;
;; 21-Jun-1993		Gary Ellison	
;;    Last Modified: Mon Mar 29 15:05:34 1993 #11 (Gary Ellison)
;;    Use make-local-variables instead of make-variables-buffer-local
;;    ksh-indent now supports nil (keyword aligned) or number (offset)
;;    Support ksh-tab-always-indent feature
;;    Variables offsetting indentation renamed to better reflect their
;;    role.
;;    Integrate keyword completion feature supplied by
;;    Haavard Rue <hrue@imf.unit.no>.
;;
;; 29-Mar-1993		Gary Ellison	
;;    Last Modified: Tue Sep 29 16:14:02 1992 #10 (Gary Ellison)
;;    Integrate line continuation patch supplied by
;;    Haavard Rue <hrue@imf.unit.no>
;;    Name back to ksh-mode to avoid confusion with sh-mode
;;    by Thomas W. Strong, Jr. <strong+@cmu.edu>.
;;
;; 29-Sep-1992		Gary Ellison	
;;    Last Modified: Wed Sep  2 08:51:40 1992 #9 (Gary Ellison)
;;    Full support of ksh88 case items. 
;;    Align statements under "do" and "then" keywords one position 
;;    past the keyword.
;;
;; 2-Sep-1992		Gary Ellison	
;;    Last Modified: Tue Aug  4 14:34:35 1992 #8 (Gary Ellison)
;;    Use make-variable-buffer-local instead of make-local-variable
;;    Get rid of superflous ksh-default variables.
;;    Use end of word match \b for "then", "do", "else", "elif"
;;    Support process substitution lists and exclude ksh 88 case items
;;    Use default-tab-width for indentation defaults.
;;    Moved installation instructions to the mode level documentation 
;;    section.
;;    Fixed auto-mode-alist documentation.
;;
;; 24-Jul-1992		Gary Ellison	
;;    Last Modified: Fri Jul 24 09:45:11 1992 #7 (Gary Ellison)
;;    Modified ksh-indent-region to use marker versus fixed end point.
;;    comment-start-skip regexp no longer fooled by parameter substitution.
;;    Added constant ksh-mode-version.
;;
;; 21-Jul-1992		Gary Ellison	
;;    Last Modified: Tue Jul 21 15:53:57 1992 #6 (Gary Ellison)
;;    Indent with tabs instead of spaces.
;;    Can handle just about all styles.
;;    Anti-newline in REs.
;;    Word delim "\b" in REs
;;    More syntax entries.
;;    Variables with regexp suffix abbreviated to re
;;    Better } handling
;;    Implemented minimal indent-region-function
;;    Mode documentation corrected.
;;    Minor lisp source format changes.
;;    
;; 29-Jun-1992		Gary Ellison	
;;    Last Modified: Mon Jun 29 15:39:35 1992 #5 (Gary Ellison)
;;    Optimize line-to-string
;;    Implicit/Explicit functions aok
;;    More indentation variables
;;    Superfluous defun killed.
;;    renamed to sh-mode
;;    
;; 22-Jun-1992          Gary Ellison
;;    Last Modified: Mon Jun 22 15:01:14 1992 #4 (Gary Ellison)
;;    Cleanup pre att.emacs posting
;;
;; 19-Jun-1992          Gary Ellison
;;    Last Modified: Fri Jun 19 17:19:14 1992 #3 (Gary Ellison)
;;    Minimal case indent handling
;;
;; 19-Jun-1992          Gary Ellison
;;    Last Modified: Fri Jun 19 16:23:26 1992 #2 (Gary Ellison)
;;    Nesting handled except for case statement
;;
;; 19-Jun-1992          Gary Ellison
;;    Last Modified: Fri Jun 19 10:03:07 1992 #1 (Gary Ellison)
;;    Conception of this mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ksh-mode-version "$Revision: 1.4 $"
  "*Version numbers of this version of ksh-mode")

;;
;; Variables controlling indentation style
;;

(defvar ksh-indent 2 
  ;; perhaps c-basic-offset would be okay to use as a default, but using
  ;; default-tab-width as the default is ridiculous --Stig
  "*Indentation of ksh statements with respect to containing block. A value
of nil indicates compound list keyword \(\"do\" and \"then\"\) alignment.")
(defvar ksh-case-item-offset ksh-indent
  "*Additional indentation for case items within a case statement.")
(defvar ksh-case-indent nil
  "*Additional indentation for statements under case items.")
(defvar ksh-group-offset (- ksh-indent)
  "*Additional indentation for keywords \"do\" and \"then\".")
(defvar ksh-brace-offset 0
  "*Additional indentation of \"{\" under functions or brace groupings.")
(defvar ksh-multiline-offset 1
  "*Additional indentation of line that is preceded of a line ending with a
\\ to make it continue on next line.")
(defvar ksh-match-and-tell t
  "*If non-nil echo in the minibuffer the matching compound command
for the \"done\", \"}\", \"fi\", or \"esac\". ")
(defvar ksh-tab-always-indent t
  "*Controls the operation of the TAB key. If t (the default), always
reindent the current line.  If nil, indent the current line only if
point is at the left margin or in the line's indentation; otherwise
insert a tab.")

(defvar ksh-align-to-keyword nil
  ;; #### - this is broken, so it should be disabled by default --Stig
  "*Controls whether nested constructs align from the keyword or
the current indentation. If non-nil, indentation will be relative to
the column the keyword starts. If nil, indentation will be relative to
the current indentation of the line the keyword is on.
The default value is non-nil.  The non-nil case doesn't work very well.")

(defvar ksh-comment-regexp "^\\s *#"
  "*Regular expression used to recognize comments. Customize to support
ksh-like languages.")

(defun ksh-current-indentation ()
  nil)
;;
(fset 'ksh-current-indentation 'current-column)
;;
;; Variables controlling completion
(defvar ksh-completion-list '())
(make-variable-buffer-local 'ksh-completion-list)
(set-default 'ksh-completion-list  '())

;;
;; -type-  : type number, 0:misc, 1:variable, 2:function
;; -regexp-: regexp used to parse the script
;; -match- : used by match-beginning/end to pickup target
;;
(defvar ksh-completion-type-misc 0)
(defvar ksh-completion-regexp-var "\\([A-Za-z_0-9]+\\)=")
(defvar ksh-completion-type-var 1)
(defvar ksh-completion-match-var 1) 
(defvar ksh-completion-regexp-var2 "\\$\\({\\|{#\\)?\\([A-Za-z_0-9]+\\)[#%:}]?")
(defvar ksh-completion-match-var2 2)
(defvar ksh-completion-regexp-function
  "\\(function\\)?[ \t]*\\([A-Za-z_0-9]+\\)[ \t]*([ \t]*)")
(defvar ksh-completion-type-function 2)
(defvar ksh-completion-match-function 2)

;;
;; Variable controlling fontification
;;
;(defvar ksh-keywords '("for" "in" "do" "done" "select" "case" "esac" "if"
;"then" "elif" "else" "fi" "while" "until" "function" "time"
;"alias" "bg" "break" "continue" "cd" "exit" "echo" "fc" "fg" "getopts" "jobs"
;"kill" "let" "newgrp" "print" "pwd" "read" "readonly" "return" "set" "shift"
;"test" "times" "trap" "typeset" "ulimit" "umask" "unalias" "unset" "wait" "whence"))

(setq ksh-keywords "alias\\|b\\(g\\|reak\\)\\|c\\(ase\\|d\\|ontinue\\)\\|do\\(\\|ne\\)\\|e\\(cho\\|l\\(if\\|se\\)\\|sac\\|x\\(it\\|port\\)\\)\\|f\\([cgi]\\|or\\|unction\\)\\|getopts\\|i[fn]\\|jobs\\|kill\\|let\\|newgrp\\|p\\(rint\\|wd\\)\\|re\\(ad\\(\\|only\\)\\|turn\\)\\|s\\(e\\(lect\\|t\\)\\|hift\\)\\|t\\(est\\|hen\\|imes?\\|rap\\|ypeset\\)\\|u\\(limit\\|mask\\|n\\(alias\\|set\\|til\\)\\)\\|w\\(ait\\|h\\(ence\\|ile\\)\\)")

;;       '("\\<function[ \t]+\\([^(; \t]+\\)" 1 font-lock-function-name-face)
(defvar ksh-font-lock-keywords (purecopy
      (list
       ;; Fontify [[ ]] expressions
       '("\\(\\[.*\\]\\)"  1 font-lock-string-face t)
       ;; Fontify keywords
       (cons (concat
              "\\<\\("
              ksh-keywords
              "\\)\\>")
	     1)
       ;; Fontify function names
       '("\\<function[ \t]+\\([^(; \t]+\\)" 1 font-lock-function-name-face)
       '("\\(^[ \t]*[A-Za-z_][A-Za-z_0-9]*[ \t]*()\\)" 1 font-lock-function-name-face)
       ;; Fontify exports
       '("\\<export[ \t]+\\(-[a-z][ \t]+\\)?\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\>" 2 font-lock-variable-name-face)
       ;; Fontify aliases
       '("\\<alias[ \t]+\\(-[a-z][ \t]+\\)?\\<\\([A-Za-z_][A-Za-z0-9_]*\\)\\>" 2 font-lock-type-face)
       ;; Fontify $variables/parameters
       '("\\(\\$\\([A-Za-z_#-][A-Za-z0-9_]*\\|{[^}]+}\\|([^)]+)\\)\\)"
	 ;; The `if' is needed because the specific faces are unique to the
	 ;; two emacsen.
	 1 (if (string-match "XEmacs" emacs-version)
	       font-lock-preprocessor-face
	     font-lock-reference-face))
       ))
       "Expressions to font-lock in ksh-mode.")

(defvar ksh-mode-hook nil
  "Hook to run when entering ksh-mode.")

;; For the ksh-mode function, a small suggestion.

;;
;; config font-lock mode

;; First, some ugly stuff that is necessary to support GNU Emacs as
;; well as XEmacs.
;; Setting up things for font-lock
;;  (if (string-match "XEmacs" emacs-version)
;;      (progn
;;	(make-local-variable 'font-lock-keywords)
;;	(setq font-lock-keywords ksh-font-lock-keywords))
;;    ;; Emacs
;;    (make-local-variable 'font-lock-defaults)
;;    (setq font-lock-defaults '(ksh-font-lock-keywords nil t))
;;    )

(put 'ksh-mode 'font-lock-defaults '(ksh-font-lock-keywords))

;;
;; Context/indentation regular expressions
;; 
;; indenting expressions
;;
;(defconst ksh-then-do-re     "^[^#\n]*\\s\"*\\b\\(then\\|do\\)\\b"
(defconst ksh-then-do-re     "\\s *\\b\\(then\\|do\\)\\b"
  "*Regexp used to locate grouping keywords: \"then\" and \"do\"" )

(defconst ksh-do-re          "\\s *\\bdo\\(\\b\\|$\\)"
  "*Regexp used to match keyword: do")

(defconst ksh-then-re        "\\s *\\bthen\\(\\b\\|$\\)"
  "*Regexp used to match keyword: then")

;;
;; Structure starting/indenting keywords
;;
(defconst ksh-else-re           "\\s *\\belse\\(\\b\\|$\\)"
  "*Regexp used to match keyword: else")

(defconst ksh-elif-re           "\\s *\\belif\\(\\b\\|$\\)"
  "*Regexp used to match keyword: elif")

(defconst ksh-brace-re           "\\S>*{[ \t\n]"
  "*Regexp used to match syntactic entity: { ")

(defconst ksh-case-item-end-re           "\\S>*;;[ \t\n]"
  "*Regexp used to match case item end syntactic entity: ;;")

(defconst ksh-keywords-re
  "\\s *\\b\\(else\\|if\\|elif\\|case\\|while\\|for\\|until\\|select\\)\\b"
  "*Regexp used to detect compound command keywords: if, else, elif case, 
while, for, until, and select")


(defconst ksh-if-re         "\\s *\\b\\(if\\)\\b"
  "*Regexp used to match keyword: if")

(defconst ksh-iteration-keywords-re 
  "\\s *\\b\\(while\\|for\\|until\\|select\\)\\b"
  "*Match one of the keywords: while, until, for, select")

(defconst ksh-case-re           "\\s *\\bcase\\b"
  "*Regexp used to match keyword: case")

(defconst ksh-explicit-func-re
  "^\\s *\\(function\\s [a-zA-z_][a-zA-Z0-1_]*\\)\\b"
  "*Match an explicit function definition: function name")

(defconst ksh-implicit-func-re
  "^\\s *\\([a-zA-z_][a-zA-Z0-1_]*\\)\\s *()\\s *"
  "*Match an implicit function definition: name ()")

(defconst ksh-func-brace-re "^\\s *\\(.*{\\)[ \t\n]+"
  "*Match a implicit function definition brace: name { ")

;;
;; indenting 
(defconst ksh-case-item-re           "^[^#\n]*\\s\"*\\()\\)"
  "*Regexp used to match case-items including ksh88")

(defconst ksh-paren-re           "^[^#\n]*\\s\"*)[ \t\n]+"
  "*Regexp used to match compound list & case items")

;;
;; structure ending keyword regular expressions
(defconst ksh-fi-re            "\\s *\\bfi\\b"
  "*Regexp used to match keyword: fi")

(defconst ksh-esac-re          "\\s *\\besac\\b"
  "*Regexp used to match keyword: esac")

(defconst ksh-done-re          "\\s *\\bdone\\b"
  "*Regexp used to match keyword: done")

(defconst ksh-brace-end-re  "\\s *}\\s *"
  "*Regexp used to match function brace-groups")

(defconst ksh-multiline-re "^.*\\\\$"
  "*Regexp used to match a line with a statement using more lines.")

;;
;;
;; Create mode specific tables
(defvar ksh-mode-syntax-table nil
  "Syntax table used while in ksh mode.")
(if ksh-mode-syntax-table
    ()
  (setq ksh-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?\( "." ksh-mode-syntax-table)
  (modify-syntax-entry ?\) "." ksh-mode-syntax-table)
  (modify-syntax-entry ?{ "." ksh-mode-syntax-table)
  (modify-syntax-entry ?} "." ksh-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" ksh-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" ksh-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" ksh-mode-syntax-table)
  (modify-syntax-entry ?` "\"" ksh-mode-syntax-table)
  (modify-syntax-entry ?\n ">" ksh-mode-syntax-table)
  (modify-syntax-entry ?\f ">" ksh-mode-syntax-table)
  (modify-syntax-entry ?# "<" ksh-mode-syntax-table)
  (modify-syntax-entry ?_ "w" ksh-mode-syntax-table)
  (modify-syntax-entry ?< "." ksh-mode-syntax-table)
  (modify-syntax-entry ?> "." ksh-mode-syntax-table)
  (modify-syntax-entry ?& "." ksh-mode-syntax-table)
  (modify-syntax-entry ?| "." ksh-mode-syntax-table)
  (modify-syntax-entry ?$ "\\" ksh-mode-syntax-table)
  (modify-syntax-entry ?% "." ksh-mode-syntax-table)
  (modify-syntax-entry ?= "." ksh-mode-syntax-table)
  (modify-syntax-entry ?/ "." ksh-mode-syntax-table)
  (modify-syntax-entry ?+ "." ksh-mode-syntax-table)
  (modify-syntax-entry ?* "." ksh-mode-syntax-table)
  (modify-syntax-entry ?- "." ksh-mode-syntax-table)
  (modify-syntax-entry ?\; "." ksh-mode-syntax-table)
  (modify-syntax-entry ?: "." ksh-mode-syntax-table)
  )

(defvar ksh-mode-abbrev-table nil
  "Abbrev table used while in ksh mode.")
(define-abbrev-table 'ksh-mode-abbrev-table ())

(defvar ksh-mode-map nil 
  "Keymap used in ksh mode")

(if ksh-mode-map
    ()
  (setq ksh-mode-map (make-sparse-keymap))
  (define-key ksh-mode-map "\t"    'ksh-indent-command)
  (define-key ksh-mode-map "\t"    'ksh-indent-line)
  (define-key ksh-mode-map "\C-j"    'reindent-then-newline-and-indent)
  (define-key ksh-mode-map "\e\t"    'ksh-complete-symbol)
  (define-key ksh-mode-map "\C-c\t"    'ksh-completion-init-and-pickup)
  )


;;;###autoload
(defun ksh-mode ()
  "ksh-mode $Revision: 1.4 $ - Major mode for editing (Bourne, Korn or Bourne again)
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
  ksh-like languages. Default value is \"\^\\\\s *#\".

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
	 )))"
  ;;
  ;; and away we go
  (interactive)
  (kill-all-local-variables)
  (use-local-map ksh-mode-map)
  (setq major-mode 'ksh-mode)
  (setq mode-name "Ksh")
  (setq local-abbrev-table ksh-mode-abbrev-table)
  (set-syntax-table ksh-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ksh-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'ksh-indent-region)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  ;;
  ;; Let the user customize
  (run-hooks 'ksh-mode-hook)
  (if (not ksh-align-to-keyword)
      (ksh-align-to-keyword -1)
    )
  ) ;; defun

;;
;; Support functions

(defun ksh-align-to-keyword (&optional arg)
  "Toggle value of ksh-align-to-keyword and rebind the ksh-current-indentation
function. With arg, force alignment to keyword if and only if arg is positive."
  (interactive)
  (if (null arg)			;just toggle
      (cond ((not ksh-align-to-keyword)
	     (setq ksh-align-to-keyword t)
	     (fset 'ksh-current-indentation 'current-column))
	    (t
	     (setq ksh-align-to-keyword nil)
	     (fset 'ksh-current-indentation 'current-indentation))
	    )
    (cond ((natnump arg)
	   (setq ksh-align-to-keyword t)
	   (fset 'ksh-current-indentation 'current-column))
	  (t
	   (setq ksh-align-to-keyword nil)
	   (fset 'ksh-current-indentation 'current-indentation))
	  ))
  )

(defun ksh-current-line ()
  "Return the vertical position of point in the buffer.
Top line is 1."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0))
  )


(defun ksh-line-to-string ()
  "From point, construct a string from all characters on
current line"
  (skip-chars-forward " \t") ;; skip tabs as well as spaces
  (buffer-substring (point)
                    (progn
                      (end-of-line 1)
                      (point))))

(defun ksh-get-nest-level ()
  "Return a 2 element list (nest-level nest-line) describing where the
current line should nest."
  (let ((case-fold-search)
	(level))
    (save-excursion
      (forward-line -1)
      (while (and (not (bobp))
		  (null level))
	(if (and (not (looking-at "^\\s *$"))
 		 (not (save-excursion
 			(forward-line -1)
 			(beginning-of-line)
			(looking-at ksh-multiline-re)))
		 (not (looking-at ksh-comment-regexp)))
	    (setq level (cons (current-indentation)
			      (ksh-current-line)))
	  (forward-line -1)
	  );; if
	);; while
      (if (null level)
	  (cons (current-indentation) (ksh-current-line))
	level)
      )
    )
  )

(defun ksh-looking-at-compound-list ()
  "Return true if current line contains compound list initiating keyword"
  (or 
   (looking-at ksh-do-re)
   (looking-at ksh-then-re)
   ) ;; or
  ) ;; defun

(defun ksh-looking-at-case-item ()
  "Return true if current line is a case-item .vs. paren compound list"
  (save-excursion
    (beginning-of-line)
    ;;
    ;; Handle paren indentation constructs for this line
    (cond ((looking-at ksh-paren-re)
	   (goto-line (cdr (ksh-get-nest-level)))
	   ;;
	   ;; The question is whether this is really a case item or just
	   ;; parenthesized compound list.
	   (cond ((or (looking-at ksh-case-re)
		      (looking-at ksh-case-item-end-re)))
		 ;;
		 ;; turns out to be a parenthesized compound list
		 ;; so propigate the nil for cond
		 )
	   ))
    )
  ) ;; defun


(defun ksh-get-case-indent ()
  "Return the column of the closest open case statement"
  (save-excursion
    (let (
	  (nest-list (ksh-get-compound-level ksh-case-re ksh-esac-re (point)))
	  )
      (if (null nest-list)
	  (progn 
	    (if ksh-match-and-tell
		(message "No matching case for ;;"))
	    0)
	(car nest-list)))
    )
  )

(defun ksh-search-forward-sexp (sexp-re fence-post)
  "Search for an sexp. Return t on success with point at the
beginning of the sexp. Return nil on failure and restoring point
to it's original position"
  (let
      ((old-pnt (point))
       )
    (while (and (< (point) fence-post)
		(not (looking-at sexp-re)))
      (ksh-forward-sexp))
    
    (if (>= (point) fence-post)
	(progn (goto-char old-pnt)
	       nil)
      t)
    ))

(defun ksh-search-backward-sexp (sexp-re fence-post)
  (let
      ((old-pnt (point))
       (sentinal nil)
       )
    (while
	(progn
	  (if (not sentinal)
	      (backward-sexp 1))
	  (and (> (point) fence-post)
	       (not sentinal))
	  )
      (if (looking-at sexp-re)
	  (save-excursion			;avoid comment foolage
	    (let ((key-fence (point)))
	      (beginning-of-line)
	      (back-to-indentation)
	      (while (and (ksh-search-forward-sexp sexp-re key-fence)
			  (< (point) key-fence)))
	      
	      (if (= key-fence (point))
		  (setq sentinal t))
	      ))
	))
      
      (if (< (point) fence-post)
	  (progn (goto-char old-pnt)
		 nil)
	t)
      ))


;;
;; Functions which make this mode what it is
;;

(defun ksh-get-nester-column (nest-line)
  "Return the column to indent to with respect to nest-line taking 
into consideration keywords and other nesting constructs."
  (save-excursion 
    (let ((fence-post)
;	  (start-post)
	  (nester-column)
 	  (case-fold-search)
	  (start-line (ksh-current-line)))
      (cond
       ;;
       ;; Handle case item indentation constructs for this line
       ((ksh-looking-at-case-item)
	(save-excursion
	  (goto-line nest-line)
	  (back-to-indentation)
	  (let ((fence-post (ksh-eol-point)))
	    ;;
	    ;; Now know there is a case-item so detect whether
	    ;; it is first under case, just another case-item, or
	    ;; a case-item and case-item-end all rolled together.
	    ;;
	    (cond ((ksh-search-forward-sexp ksh-case-re fence-post)
		   (+ (ksh-current-indentation) ksh-case-item-offset))
		  
		  ((ksh-looking-at-case-item)
		   (current-indentation))
		  
		  ((looking-at ksh-case-item-end-re)
		   (end-of-line)
		   (+ (ksh-get-case-indent) ksh-case-item-offset))
		  )
	    )))
       (t;; Not a case-item.  What to do relative to the nest-line?
	(save-excursion
	  (goto-line nest-line)
;	  (setq start-post (point))
	  (setq fence-post (ksh-eol-point))
	  (setq nester-column
		(save-excursion
		  (cond
		   ;;
		   ;; Check if we are in a continued statement
		   ((and (looking-at ksh-multiline-re)
			 (save-excursion
			   (goto-line (1- start-line))
			   (looking-at ksh-multiline-re)))
		    (+ (current-indentation) ksh-multiline-offset))
		   ;; In order to locate the column of the keyword,
		   ;; which might be embedded within a case-item,
		   ;; it is necessary to iterate over sexp.
		   ((progn
		      (save-excursion
			(back-to-indentation)
			(if (ksh-search-forward-sexp ksh-keywords-re fence-post)
			    (progn
			      ;;
			      ;; special pun intended 'case'
			      (if (looking-at ksh-case-re)
				  (+ (ksh-current-indentation)
				     ksh-case-item-offset)
				(+ (ksh-current-indentation)
				   (if (null ksh-indent)
				       2 ksh-indent))))	
			  nil))
		      ))
		   ;;
		   ;;  handle then or do
		   ((progn
		      (save-excursion
			(back-to-indentation)
			(if (ksh-search-forward-sexp ksh-then-do-re fence-post)
			    (progn
			      (if (null ksh-indent)
				  (+ (ksh-current-indentation) 1)
				(+ (ksh-current-indentation) ksh-indent)))
			  nil))))

		   ((looking-at ksh-brace-re)
		    (+ (current-indentation)
		       (if (null ksh-indent)
			   2 ksh-indent)
		       ))
		   ;;
		   ;; Forces functions to first column
		   ((or (looking-at ksh-implicit-func-re)
			(looking-at ksh-explicit-func-re))
		    (if (looking-at ksh-func-brace-re)
			(if (null ksh-indent)
			    2 ksh-indent)
		      ksh-brace-offset))

		   ;;
		   ;; Need to first detect the end of a case-item
		   ((looking-at ksh-case-item-end-re)
		    (end-of-line)
		    (+ (ksh-get-case-indent) ksh-case-item-offset))
		   ;;
		   ;; Now detect first statement under a case item
		   ((ksh-looking-at-case-item)
		    (if (null ksh-case-indent)
			(progn
			  (re-search-forward ksh-case-item-re fence-post t)
			  (goto-char (match-end 1))
			  (+ (current-column) 1))
		      (+ (current-indentation) ksh-case-indent)))
		   
		   ;; This is hosed when using current-column
		   ;; and there is a multi-command expression as the
		   ;; nester.
		   (t (current-indentation)))
		  )
		));; excursion over
	;;
	;; Handle additional indentation constructs for this line
	(cond ((ksh-looking-at-compound-list)
	       (+ nester-column ksh-group-offset))
	      ((looking-at ksh-brace-re)
	       (+ nester-column ksh-brace-offset))
	      (t nester-column))
	);; Not a case-item
       )
      );;let
    );; excursion
  ) ;; defun

(defun ksh-indent-command ()
  "Indent current line relative to containing block and allow for
ksh-tab-always-indent customization"
  (interactive)
  (let (case-fold-search)
    (cond ((save-excursion
	     (skip-chars-backward " \t")
	     (bolp))
	   (ksh-indent-line))
	  (ksh-tab-always-indent
	   (save-excursion
	     (ksh-indent-line)))
	  (t (insert-tab))
	  ))
  )


(defun ksh-indent-line ()
  "Indent current line as far as it should go according
to the syntax/context"
  (interactive)
  (let (case-fold-search)
    (save-excursion
      (beginning-of-line)
      (if (bobp)
	  nil
	;;
	;; Align this line to current nesting level
	(let*
	    (
	     (level-list (ksh-get-nest-level)) ; Where to nest against
	     ;;           (last-line-level (car level-list))
	     (this-line-level (current-indentation))
	     (nester-column (ksh-get-nester-column (cdr level-list)))
	     (struct-match (ksh-match-structure-and-reindent))
	     )
	  (if struct-match
	      (setq nester-column struct-match))
	  (if (eq nester-column this-line-level)
	      nil
	    (beginning-of-line)
	    (let ((beg (point)))
	      (back-to-indentation)
	      (delete-region beg (point)))
	    (indent-to nester-column))
	  );; let*
	);; if
      );; excursion
    ;;
    ;; Position point on this line
    (let*
	(
	 (this-line-level (current-indentation))
	 (this-bol (ksh-bol-point))
	 (this-point (- (point) this-bol))
	 )
      (cond ((> this-line-level this-point);; point in initial white space
	     (back-to-indentation))
	    (t nil)
	    );; cond
      );; let*
    );; let
  );; defun


(defun ksh-match-indent-level (begin-re end-re)
  "Match the compound command and indent. Return nil on no match,
indentation to use for this line otherwise."
  (interactive)
  (let* ((case-fold-search)
	 (nest-list 
	  (save-excursion
	    (ksh-get-compound-level begin-re end-re (point))
	    ))
	 ) ;; bindings
    (if (null nest-list)
	(progn
	  (if ksh-match-and-tell
	      (message "No matching compound command"))
	  nil) ;; Propagate a miss.
      (let* (
	     (nest-level (car nest-list))
	     (match-line (cdr nest-list))
	     ) ;; bindings
	(if ksh-match-and-tell
	    (save-excursion
	      (goto-line match-line)
	      (message "Matched ... %s" (ksh-line-to-string))
	      ) ;; excursion
	  ) ;; if ksh-match-and-tell
	nest-level ;;Propagate a hit.
	) ;; let*
      ) ;; if
    ) ;; let*
  ) ;; defun ksh-match-indent-level

(defun ksh-match-structure-and-reindent ()
  "If the current line matches one of the indenting keywords
or one of the control structure ending keywords then reindent. Also
if ksh-match-and-tell is non-nil the matching structure will echo in
the minibuffer"
  (interactive)
  (let (case-fold-search)
    (save-excursion
      (beginning-of-line)
      (back-to-indentation)
      (cond ((looking-at ksh-else-re)
	     (ksh-match-indent-level ksh-if-re ksh-fi-re))
	    ((looking-at ksh-elif-re)
	     (ksh-match-indent-level ksh-if-re ksh-fi-re))
	    ((looking-at ksh-fi-re)
	     (ksh-match-indent-level ksh-if-re ksh-fi-re))
	    ((looking-at ksh-done-re)
	     (ksh-match-indent-level ksh-iteration-keywords-re ksh-done-re))
	    ((looking-at ksh-esac-re)
	     (ksh-match-indent-level ksh-case-re ksh-esac-re))
	    ;;
	    ((looking-at ksh-brace-end-re)
	     (cond
	      ((ksh-match-indent-level ksh-implicit-func-re ksh-brace-end-re))
	      ((ksh-match-indent-level ksh-explicit-func-re ksh-brace-end-re))
	      ((ksh-match-indent-level ksh-func-brace-re ksh-brace-end-re))
	      (t nil)))
	    (t nil)
	    );; cond
      )
    ))


(defun ksh-forward-sexp ()
  "Special incantation to march over syntax expressions and
avoid all sorts of nonsense"
  (if (char-equal ?< (char-syntax (char-after (point))))
      (end-of-line)
    (if (char-equal ?. (char-syntax (char-after (point))))
	(forward-char)
      (forward-sexp 1))
    )
  (if (eolp)
      (forward-line))
  (skip-chars-forward ") \t")		;damn case
  )


(defun ksh-get-compound-level
  (begin-re end-re anchor-point &optional balance-list)
  "Determine how much to indent this structure. Return a list (level line) 
of the matching compound command or nil if no match found."
  (let* 
      (;; Locate the next compound begin keyword bounded by point-min
       (match-point
	(if (and (ksh-search-backward-sexp begin-re (point-min))
		 (>= (point) (point-min))
		 )
	    (point)
	  0))

       (nest-column (if (zerop match-point)
			1 
		      (progn
			(goto-char match-point)
			(ksh-current-indentation))))
       (nest-list (cons 0 0))    ;; sentinel cons since cdr is >= 1
       )
    (if (zerop match-point)
	nil ;; graceful exit from recursion
      (progn
	(if (nlistp balance-list)
	    (setq balance-list (list)))
	;; Now search forward from matching start keyword for end keyword
	;; which will locate interceding compound commands
	(while (and (consp nest-list) (zerop (cdr nest-list))
		    (ksh-search-forward-sexp end-re anchor-point)
		    (> anchor-point (point))
		    )
	  (if (not (memq (point) balance-list))
	      (progn
		(setq balance-list (cons (point) balance-list))
		(goto-char match-point)  ;; beginning of compound cmd
		(setq nest-list
		      (ksh-get-compound-level begin-re end-re
					      anchor-point balance-list))
		)
	    (ksh-forward-sexp)
	    ))

	(cond ((consp nest-list)
	       (if (zerop (cdr nest-list))
		 (progn
		   (goto-char match-point)
		   (cons nest-column (ksh-current-line)))
		 nest-list))
	      (t nil)
	      )
	)
      )
    )
  )


(defun ksh-indent-region (start end)
  "From start to end, indent each line."
  ;; The algorithm is just moving through the region line by line with
  ;; the match noise turned off.  Only modifies nonempty lines.
  (save-excursion
    (let (ksh-match-and-tell
	  (endmark (copy-marker end)))
      
      (goto-char start)
      (beginning-of-line)
      (setq start (point))
      (while (> (marker-position endmark) start)
	(if (not (and (bolp) (eolp)))
	    (ksh-indent-line))
	(forward-line 1)
	(setq start (point)))

      (set-marker endmark nil)
      )
    )
  )

;;
;; Completion code supplied by Haavard Rue <hrue@imf.unit.no>.
;;
;;
;; add a completion with a given type to the list
;;
(defun ksh-addto-alist (completion type)
  (setq ksh-completion-list
	(append ksh-completion-list
		(list (cons completion type)))))
;;
;; init the list and pickup all 
;;
(defun ksh-completion-init-and-pickup ()
  (interactive)
  (let (case-fold-search)
    (ksh-completion-list-init)
    (ksh-pickup-all)))

;;
;; init the list
;;
(defun ksh-completion-list-init ()
  (interactive)
  (setq ksh-completion-list
	(list
	 (cons "if"  ksh-completion-type-misc)
	 (cons "while"  ksh-completion-type-misc)
	 (cons "until"  ksh-completion-type-misc)
	 (cons "select"  ksh-completion-type-misc)
	 (cons "for"  ksh-completion-type-misc)
	 (cons "continue"  ksh-completion-type-misc)
	 (cons "function"  ksh-completion-type-misc)
	 (cons "fi"  ksh-completion-type-misc)
	 (cons "case"  ksh-completion-type-misc)
	 (cons "esac"  ksh-completion-type-misc)
	 (cons "break"  ksh-completion-type-misc)
	 (cons "exit"  ksh-completion-type-misc)
	 (cons "done"  ksh-completion-type-misc)
	 (cons "do"  ksh-completion-type-misc))))

(defun ksh-eol-point ()
  (save-excursion
    (end-of-line)
    (point)))

(defun ksh-bol-point ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun ksh-pickup-all ()
  "Pickup all completions in buffer."
  (interactive)
  (ksh-pickup-completion-driver (point-min) (point-max) t))

(defun ksh-pickup-this-line ()
  "Pickup all completions in current line."
  (interactive)
  (ksh-pickup-completion-driver (ksh-bol-point) (ksh-eol-point) nil))

(defun ksh-pickup-completion-driver (pmin pmax message)
  "Driver routine for ksh-pickup-completion."
  (if message
      (message "pickup completion..."))
  (let* (
	 (i1
	  (ksh-pickup-completion  ksh-completion-regexp-var
				 ksh-completion-type-var
				 ksh-completion-match-var
				 pmin pmax))
	 (i2
	  (ksh-pickup-completion  ksh-completion-regexp-var2
				 ksh-completion-type-var
				 ksh-completion-match-var2
				 pmin pmax))
	 (i3
	  (ksh-pickup-completion  ksh-completion-regexp-function
				 ksh-completion-type-function
				 ksh-completion-match-function
				 pmin pmax)))
    (if message
	(message "pickup %d variables and %d functions." (+ i1 i2) i3))))

(defun ksh-pickup-completion (regexp type match pmin pmax)
  "Pickup completion in region and addit to the list, if not already
there." 
  (let ((i 0) kw obj)
    (save-excursion
      (goto-char pmin)
      (while (and
	      (re-search-forward regexp pmax t)
	      (match-beginning match)
	      (setq kw  (buffer-substring
			 (match-beginning match)
			 (match-end match))))
	(progn
	  (setq obj (assoc kw ksh-completion-list))
	  (if (or (equal nil obj)
		  (and (not (equal nil obj))
		       (not (= type (cdr obj)))))
	      (progn
		(setq i (1+ i))
		(ksh-addto-alist kw type))))))
    i))

(defun ksh-complete-symbol ()
  "Perform completion."
  (interactive)
  (let* ((case-fold-search)
	 (end (point))
         (beg (unwind-protect
                  (save-excursion
                    (backward-sexp 1)
                    (while (= (char-syntax (following-char)) ?\')
                      (forward-char 1))
                    (point))))
         (pattern (buffer-substring beg end))
	 (predicate 
	  ;;
	  ;; ` or $( mark a function
	  ;;
	  (save-excursion
	    (goto-char beg)
	    (if (or
		 (save-excursion
		   (backward-char 1)
		   (looking-at "`"))
		 (save-excursion
		   (backward-char 2)
		   (looking-at "\\$(")))
		(function (lambda (sym)
			    (equal (cdr sym) ksh-completion-type-function)))
	      ;;
	      ;; a $, ${ or ${# mark a variable
	      ;;
	      (if (or
		   (save-excursion
		     (backward-char 1)
		     (looking-at "\\$"))
		   (save-excursion
		     (backward-char 2)
		     (looking-at "\\${"))
		   (save-excursion
		     (backward-char 3)
		     (looking-at "\\${#")))
		  (function (lambda (sym)
			      (equal (cdr sym)
				     ksh-completion-type-var)))
		;;
		;; don't know. use 'em all
		;;
		(function (lambda (sym) t))))))
	 ;;
	 (completion (try-completion pattern ksh-completion-list predicate)))
    ;;
    (cond ((eq completion t))
	  ;;
	  ;; oops, what is this ?
	  ;;
          ((null completion)
           (message "Can't find completion for \"%s\"" pattern))
	  ;;
	  ;; insert
	  ;;
          ((not (string= pattern completion))
           (delete-region beg end)
           (insert completion))
	  ;;
	  ;; write possible completion in the minibuffer,
	  ;; use this instead of a separate buffer (usual)
	  ;;
          (t
           (let ((list (all-completions pattern ksh-completion-list predicate))
		 (string ""))
	     (while list
	       (progn
		 (setq string (concat string (format "%s " (car list))))
		 (setq list (cdr list))))
	     (message string))))))

(provide 'ksh-mode)
;;; ksh-mode.el ends here
