;; verilog-mode.el --- major mode for editing verilog source in Emacs
;;
;; $Id: verilog-mode.el,v 4.2 2004/12/03 23:05:24 mac Exp $

;; Copyright (C) 2003 Free Software Foundation, Inc.

;; Author: Michael McNamara (mac@verilog.com)
;; Senior Vice President of Technology, Verisity Design, Inc.
;; IEEE 1364 Verilog standards committee Chairman
;; (SureFire and Verisity merged October 1999)
;;      http://www.verisity.com
;; AUTO features, signal, modsig; by: Wilson Snyder
;;	(wsnyder@wsnyder.org or wsnyder@world.std.com)
;;	http://www.veripool.com
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This mode borrows heavily from the Pascal-mode and the cc-mode of emacs

;; USAGE
;; =====

;; A major mode for editing Verilog HDL source code. When you have
;; entered Verilog mode, you may get more info by pressing C-h m. You
;; may also get online help describing various functions by: C-h f
;; <Name of function you want described>

;; You can get step by step help in installing this file by going to
;; <http://www.verilog.com/emacs_install.html>

;; The short list of installation instructions are: To set up
;; automatic verilog mode, put this file in your load path, and put
;; the following in code (please un comment it first!) in your
;; .emacs, or in your site's site-load.el

; (autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
; (setq auto-mode-alist (cons  '("\\.v\\'" . verilog-mode) auto-mode-alist))
; (setq auto-mode-alist (cons  '("\\.dv\\'" . verilog-mode) auto-mode-alist))

;; If you want to customize Verilog mode to fit your needs better,
;; you may add these lines (the values of the variables presented
;; here are the defaults). Note also that if you use an emacs that
;; supports custom, it's probably better to use the custom menu to
;; edit these.
;;
;; Be sure to examine at the help for verilog-auto, and the other
;; verilog-auto-* functions for some major coding time savers.
;;
; ;; User customization for Verilog mode
; (setq verilog-indent-level             3
;       verilog-indent-level-module      3
;       verilog-indent-level-declaration 3
;       verilog-indent-level-behavioral  3
;       verilog-indent-level-directive   1
;       verilog-case-indent              2
;       verilog-auto-newline             t
;       verilog-auto-indent-on-newline   t
;       verilog-tab-always-indent        t
;       verilog-auto-endcomments         t
;       verilog-minimum-comment-distance 40
;       verilog-indent-begin-after-if    t
;       verilog-auto-lineup              '(all))

;; KNOWN BUGS / BUG REPORTS
;; =======================
;; This is beta code, and likely has bugs. Please report any and all
;; bugs to me at verilog-mode-bugs@surefirev.com.  Use
;; verilog-submit-bug-report to submit a report.
;; 

;;; History:
;;

;; 
;;; Code:

;; XEmacs additions
;;;###autoload(add-to-list 'auto-mode-alist '("\\.vh?\\'" . verilog-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.dv\\'" . verilog-mode))
;; APA: .f is probably controversial?
;; ;;;###autoload(add-to-list 'auto-mode-alist '("\\.f\\'" . verilog-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.vlog\\'" . verilog-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("\\.verilog\\'" . verilog-mode))

(provide 'verilog-mode)

;; This variable will always hold the version number of the mode
(defconst verilog-mode-version "Revision: 4.2"
  "Version of this verilog mode.")
(defconst verilog-running-on-xemacs (string-match "XEmacs" emacs-version))

;;;###autoload
(defun verilog-version ()
  "Inform caller of the version of this file."
  (interactive)
  (message (concat "Using verilog-mode version " (substring verilog-mode-version 12 -3 )) ))

;; Insure we have certain packages, and deal with it if we don't
(if (fboundp 'eval-when-compile)
    (eval-when-compile
      (condition-case nil
          (require 'imenu)
        (error nil))
      (condition-case nil
	  (require 'reporter)
        (error nil))
      (condition-case nil
          (require 'easymenu)
        (error nil))
      (condition-case nil
	  (load "skeleton") ;; bug in 19.28 through 19.30 skeleton.el, not provided.
        (error nil))
      (condition-case nil
          (if (fboundp 'when)
	      nil ;; fab
	    (defmacro when (var &rest body)
	      (` (cond ( (, var) (,@ body))))))
        (error nil))
      (condition-case nil
          (if (fboundp 'unless)
	      nil ;; fab
	    (defmacro unless (var &rest body)
	      (` (if (, var) nil (,@ body)))))
        (error nil))
      (condition-case nil
          (if (fboundp 'store-match-data)
	      nil ;; fab
	    (defmacro store-match-data (&rest args) nil))
        (error nil))
      (condition-case nil
	  (if (boundp 'current-menubar)
	      nil ;; great
	    (defmacro set-buffer-menubar (&rest args) nil)
	    (defmacro add-submenu (&rest args) nil))
	(error nil))
      (condition-case nil
	  (if (fboundp 'zmacs-activate-region)
	      nil ;; great
	    (defmacro zmacs-activate-region (&rest args) nil))
	(error nil))
      (condition-case nil
	  (if (fboundp 'char-before)
	      nil ;; great
	    (defmacro char-before (&rest body)
	      (` (char-after (1- (point))))))
	(error nil))
      ;; Requires to define variables that would be "free" warnings
      (condition-case nil
	  (require 'font-lock)
	(error nil))
      (condition-case nil
	  (require 'compile)
	(error nil))
      (condition-case nil
	  (require 'custom)
	(error nil))
      (condition-case nil
	  (require 'dinotrace)
	(error nil))
      (condition-case nil
	  (if (fboundp 'dinotrace-unannotate-all)
	      nil ;; great
	    (defun dinotrace-unannotate-all (&rest args) nil))
	(error nil))
      (condition-case nil
	  (if (fboundp 'customize-apropos)
	      nil ;; great
	    (defun customize-apropos (&rest args) nil))
	(error nil))
      (condition-case nil
	  (if (fboundp 'match-string-no-properties)
	      nil ;; great
	    (defsubst match-string-no-properties (match)
	      (buffer-substring-no-properties (match-beginning match) (match-end match))))
	(error nil))
      (if (and (featurep 'custom) (fboundp 'custom-declare-variable))
	  nil ;; We've got what we needed
	;; We have the old custom-library, hack around it!
	(defmacro defgroup (&rest args)  nil)
	(defmacro customize (&rest args)
	  (message "Sorry, Customize is not available with this version of emacs"))
	(defmacro defcustom (var value doc &rest args)
	  (` (defvar (, var) (, value) (, doc))))
	)
      (if (fboundp 'defface)
	  nil ; great!
	(defmacro defface (var value doc &rest args)
	  (` (make-face (, var))))
	)

      (if (and (featurep 'custom) (fboundp 'customize-group))
	  nil ;; We've got what we needed
	;; We have an intermediate custom-library, hack around it!
	(defmacro customize-group (var &rest args)
	  (`(customize (, var) )))
	)

      ))
(eval-and-compile
  (if (fboundp 'regexp-opt)
      (defun verilog-regexp-opt (strings &optional paren shy)
	(let ((open (if paren "\\(" "")) (close (if paren "\\)" "")))
	  (concat open (mapconcat 'regexp-quote strings "\\|") close)))
    (if (fboundp 'function-min-args)
	(if (= 3 (function-min-args `regexp-opt))
	    (defun verilog-regexp-opt (a b)
	      "Deal with differing number of required arguments for  `regexp-opt'.
         Call 'regexp-opt' on A and B."
	      (regexp-opt a b 't))
	  (defun verilog-regexp-opt (a b)
	    "Call 'regexp-opt' on A and B."
	    (regexp-opt a b))
	  )
      (defun verilog-regexp-opt (a b)
	"Call 'regexp-opt' on A and B."
	(regexp-opt a b))
      )
    ))

;; XEmacs change: workarounds not needed with xemacs-base > 1.81
(eval-when-compile
  (defalias 'verilog-regexp-opt 'regexp-opt))
(defalias 'verilog-regexp-opt 'regexp-opt)
;(unless (fboundp 'regexp-opt)
;  (defun regexp-opt (strings &optional paren)
;    (let ((open (if paren "\\(" "")) (close (if paren "\\)" "")))
;      (concat open (mapconcat 'regexp-quote strings "\\|") close))))
;(eval-when-compile
;  (defun verilog-regexp-opt (a b)
;    "Wrapper to deal with XEmacs and FSF emacs having similar functions with differing number of arguments."
;    (if (string-match "XEmacs" emacs-version)
;	(regexp-opt a b 't)
;      (regexp-opt a b)))
;  )
;(defun verilog-regexp-opt (a b)
;  "Deal with XEmacs & FSF both have `regexp-opt'; with different interface.
;Call 'regexp-opt' on A and B."
;  (if verilog-running-on-xemacs
;      (unwind-protect (regexp-opt a b 't))
;    (regexp-opt a b)))

;;;###autoload
(defun verilog-customize ()
  "Link to customize screen for Verilog."
  (interactive)
  (customize-group 'verilog-mode))

(defun verilog-font-customize ()
  "Link to customize fonts used for Verilog."
  (interactive)
  (customize-apropos "font-lock-*" 'faces))

(defgroup verilog-mode nil
  "Facilitates easy editing of Verilog source text"
  :group 'languages)

; (defgroup verilog-mode-fonts nil
;   "Facilitates easy customization fonts used in Verilog source text"
;   :link '(customize-apropos "font-lock-*" 'faces)
;  :group 'verilog-mode)

(defgroup verilog-mode-indent nil
  "Customize indentation and highlighting of verilog source text"
  :group 'verilog-mode)

(defgroup verilog-mode-actions nil
  "Customize actions on verilog source text"
  :group 'verilog-mode)

(defgroup verilog-mode-auto nil
  "Customize AUTO actions when expanding verilog source text"
  :group 'verilog-mode)

(defcustom verilog-linter "surelint --std --synth --style --sim --race --fsm --msglimit=none "
  "*Unix program and arguments to call to run a lint checker on verilog source.
Invoked when you type \\[compile].  \\[next-error] will take you to
   the next lint error as expected."
  :type 'string
  :group 'verilog-mode-actions)

(defcustom verilog-coverage "surecov --code --fsm --expr "
  "*Program and arguments to use to annotate for coverage verilog source."
  :type 'string
  :group 'verilog-mode-actions)

(defcustom verilog-simulator "verilog "
  "*Program and arguments to use to interpret verilog source."
  :type 'string
  :group 'verilog-mode-actions)

(defcustom verilog-compiler "vcs "
  "*Program and arguments to use to compile verilog source."
  :type 'string
  :group 'verilog-mode-actions)

(defvar verilog-tool 'verilog-linter)

(defcustom verilog-highlight-translate-off nil
  "*Non-nil means background-highlight code excluded from translation.
That is, all code between \"// synopsys translate_off\" and
\"// synopsys translate_on\" is highlighted using a different background color
\(face `verilog-font-lock-translate-off-face').
Note: this will slow down on-the-fly fontification (and thus editing).
NOTE: Activate the new setting in a Verilog buffer by re-fontifying it (menu
      entry \"Fontify Buffer\").  XEmacs: turn off and on font locking."
  :type 'boolean
  :group 'verilog-mode-indent)

(defcustom verilog-indent-level 3
  "*Indentation of Verilog statements with respect to containing block."
  :group 'verilog-mode-indent
  :type 'integer)

(defcustom verilog-indent-level-module 3
  "* Indentation of Module level Verilog statements.  (eg always, initial)
Set to 0 to get initial and always statements lined up
    on the left side of your screen."
  :group 'verilog-mode-indent
  :type 'integer)

(defcustom verilog-indent-level-declaration 3
  "*Indentation of declarations with respect to containing block.
Set to 0 to get them list right under containing block."
  :group 'verilog-mode-indent
  :type 'integer)

(defcustom verilog-indent-declaration-macros nil
  "*How to treat macro expansions in a declaration.
If nil, indent as:
  input [31:0] a;
  input        `CP;
  output       c;
If non nil, treat as
  input [31:0] a;
  input `CP    ;
  output       c;"
  :group 'verilog-mode-indent
  :type 'boolean)

(defcustom verilog-indent-lists t
  "*How to treat indenting items in a list.
If t (the default), indent as:
  always @( posedge a or
            reset ) begin
If nil, treat as
  always @( posedge a or
     reset ) begin"
  :group 'verilog-mode-indent
  :type 'boolean)

(defcustom verilog-indent-level-behavioral 3
  "*Absolute indentation of first begin in a task or function block.
Set to 0 to get such code to start at the left side of the screen."
  :group 'verilog-mode-indent
  :type 'integer)

(defcustom verilog-indent-level-directive 1
  "*Indentation to add to each level of `ifdef declarations.
Set to 0 to have all directives start at the left side of the screen."
  :group 'verilog-mode-indent
  :type 'integer)

(defcustom verilog-cexp-indent 2
  "*Indentation of Verilog statements split across lines."
  :group 'verilog-mode-indent
  :type 'integer)

(defcustom verilog-case-indent 2
  "*Indentation for case statements."
  :group 'verilog-mode-indent
  :type 'integer)

(defcustom verilog-auto-newline t
  "*True means automatically newline after semicolons."
  :group 'verilog-mode-indent
  :type 'boolean)

(defcustom verilog-auto-indent-on-newline t
  "*True means automatically indent line after newline."
  :group 'verilog-mode-indent
  :type 'boolean)

(defcustom verilog-tab-always-indent t
  "*True means TAB should always re-indent the current line.
Nil means TAB will only reindent when at the beginning of the line."
  :group 'verilog-mode-indent
  :type 'boolean)

(defcustom verilog-tab-to-comment nil
  "*True means TAB moves to the right hand column in preparation for a comment."
  :group 'verilog-mode-actions
  :type 'boolean)

(defcustom verilog-indent-begin-after-if t
  "*If true, indent begin statements following if, else, while, for and repeat.
Otherwise, line them up."
  :group 'verilog-mode-indent
  :type 'boolean )


(defcustom verilog-align-ifelse nil
  "*If true, align `else' under matching `if'.
Otherwise else is lined up with first character on line holding matching if."
  :group 'verilog-mode-indent
  :type 'boolean )

(defcustom verilog-minimum-comment-distance 10
  "*Minimum distance (in lines) between begin and end required before a comment.
Setting this variable to zero results in every end acquiring a comment; the
default avoids too many redundant comments in tight quarters"
  :group 'verilog-mode-indent
  :type 'integer)

(defcustom verilog-auto-endcomments t
  "*True means insert a comment /* ... */ after 'end's.
The name of the function or case will be set between the braces."
  :group 'verilog-mode-actions
  :type 'boolean )

(defcustom verilog-auto-read-includes nil
  "*True means do a `verilog-read-defines' and `verilog-read-includes'
before each AUTO expansion.  This makes it easier to embed defines and
includes, but can result in very slow reading times if there are many or
large include files."
  :group 'verilog-mode-actions
  :type 'boolean )

(defcustom verilog-auto-save-policy nil
  "*Non-nil indicates action to take when saving a Verilog buffer with AUTOs.
A value of `force' will always do a \\[verilog-auto] automatically if
needed on every save.  A value of `detect' will do \\[verilog-auto]
automatically when it thinks necessary.  A value of `ask' will query the
user when it thinks updating is needed.

You should not rely on the 'ask or 'detect policies, they are safeguards
only.  They do not detect when AUTOINSTs need to be updated because a
sub-module's port list has changed."
  :group 'verilog-mode-actions
  :type '(choice (const nil) (const ask) (const detect) (const force)))

(defvar verilog-auto-update-tick nil
  "Modification tick at which autos were last performed.")

(defvar verilog-auto-last-file-locals nil
  "Text from file-local-variables during last evaluation.")

(defvar verilog-error-regexp-add-didit nil)
(defvar verilog-error-regexp nil)
(setq verilog-error-regexp-add-didit nil
 verilog-error-regexp
  '(
	; SureLint
;;    ("[^\n]*\\[\\([^:]+\\):\\([0-9]+\\)\\]" 1 2)
	; Most SureFire tools
    ("\\(WARNING\\|ERROR\\|INFO\\)[^:]*: \\([^,]+\\), \\(line \\|\\)\\([0-9]+\\):" 2 4 )
    ("\
\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)\\([) \t]\\|\
:\\([^0-9\n]\\|\\([0-9]+:\\)\\)\\)" 1 2 5)
        ; xsim
	; Error! in file /homes/mac/Axis/Xsim/test.v at line 13 		[OBJ_NOT_DECLARED]
    ("\\(Error\\|Warning\\).*in file (\\([^ \t]+\\) at line *\\([0-9]+\\))" 2 3)
	; vcs
    ("\\(Error\\|Warning\\):[^(]*(\\([^ \t]+\\) line *\\([0-9]+\\))" 2 3)
    ("Warning:.*(port.*(\\([^ \t]+\\) line \\([0-9]+\\))" 1 2)
    ("\\(Error\\|Warning\\):[\n.]*\\([^ \t]+\\) *\\([0-9]+\\):" 2 3)
    ("syntax error:.*\n\\([^ \t]+\\) *\\([0-9]+\\):" 1 2)
        ; Verilator
    ("%?\\(Error\\|Warning\\)\\(-[^:]+\\|\\):[\n ]*\\([^ \t:]+\\):\\([0-9]+\\):" 3 4)
    ("%?\\(Error\\|Warning\\)\\(-[^:]+\\|\\):[\n ]*\\([^ \t:]+\\):\\([0-9]+\\):" 3 4)
       ; vxl
    ("\\(Error\\|Warning\\)!.*\n?.*\"\\([^\"]+\\)\", \\([0-9]+\\)" 2 3)
    ("([WE][0-9A-Z]+)[ \t]+\\([^ \t\n,]+\\)[, \t]+\\([0-9]+\\):.*$" 1 2)	       ; vxl
    ("([WE][0-9A-Z]+)[ \t]+\\([^ \t\n,]+\\)[, \t]+line[ \t]+\\([0-9]+\\):.*$" 1 2)
       ; Leda
    ("In file \\([^ \t]+\\)[ \t]+line[ \t]+\\([0-9]+\\):\n[^\n]*\n[^\n]*\n\\[\\(Warning\\|Error\\|Failure\\)\\][^\n]*" 1 2)
    )
;  "*List of regexps for verilog compilers, like verilint. See compilation-error-regexp-alist for the formatting."
)

(defvar verilog-error-font-lock-keywords
  '(
    ("[^\n]*\\[\\([^:]+\\):\\([0-9]+\\)\\]" 1 bold t)
    ("[^\n]*\\[\\([^:]+\\):\\([0-9]+\\)\\]" 2 bold t)

    ("\\(WARNING\\|ERROR\\|INFO\\): \\([^,]+\\), line \\([0-9]+\\):" 2 bold t)
    ("\\(WARNING\\|ERROR\\|INFO\\): \\([^,]+\\), line \\([0-9]+\\):" 3 bold t)

    ("\
\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)\\([) \t]\\|\
:\\([^0-9\n]\\|\\([0-9]+:\\)\\)\\)" 1 bold t)
    ("\
\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)\\([) \t]\\|\
:\\([^0-9\n]\\|\\([0-9]+:\\)\\)\\)" 1 bold t)

    ("\\(Error\\|Warning\\):[^(]*(\\([^ \t]+\\) line *\\([0-9]+\\))" 2 bold t)
    ("\\(Error\\|Warning\\):[^(]*(\\([^ \t]+\\) line *\\([0-9]+\\))" 3 bold t)

    ("%?\\(Error\\|Warning\\)\\(-[^:]+\\|\\):[\n ]*\\([^ \t:]+\\):\\([0-9]+\\):" 3 bold t)
    ("%?\\(Error\\|Warning\\)\\(-[^:]+\\|\\):[\n ]*\\([^ \t:]+\\):\\([0-9]+\\):" 4 bold t)

    ("Warning:.*(port.*(\\([^ \t]+\\) line \\([0-9]+\\))" 1 bold t)
    ("Warning:.*(port.*(\\([^ \t]+\\) line \\([0-9]+\\))" 1 bold t)

    ("\\(Error\\|Warning\\):[\n.]*\\([^ \t]+\\) *\\([0-9]+\\):" 2 bold t)
    ("\\(Error\\|Warning\\):[\n.]*\\([^ \t]+\\) *\\([0-9]+\\):" 3 bold t)

    ("syntax error:.*\n\\([^ \t]+\\) *\\([0-9]+\\):" 1 bold t)
    ("syntax error:.*\n\\([^ \t]+\\) *\\([0-9]+\\):" 2 bold t)
       ; vxl
    ("\\(Error\\|Warning\\)!.*\n?.*\"\\([^\"]+\\)\", \\([0-9]+\\)" 2 bold t)
    ("\\(Error\\|Warning\\)!.*\n?.*\"\\([^\"]+\\)\", \\([0-9]+\\)" 2 bold t)

    ("([WE][0-9A-Z]+)[ \t]+\\([^ \t\n,]+\\)[, \t]+\\([0-9]+\\):.*$" 1 bold t)
    ("([WE][0-9A-Z]+)[ \t]+\\([^ \t\n,]+\\)[, \t]+\\([0-9]+\\):.*$" 2 bold t)

    ("([WE][0-9A-Z]+)[ \t]+\\([^ \t\n,]+\\)[, \t]+line[ \t]+\\([0-9]+\\):.*$" 1 bold t)
    ("([WE][0-9A-Z]+)[ \t]+\\([^ \t\n,]+\\)[, \t]+line[ \t]+\\([0-9]+\\):.*$" 2 bold t)
       ; Leda
    ("In file \\([^ \t]+\\)[ \t]+line[ \t]+\\([0-9]+\\):\n[^\n]*\n[^\n]*\n\\[\\(Warning\\|Error\\|Failure\\)\\][^\n]*" 1 bold t)
    ("In file \\([^ \t]+\\)[ \t]+line[ \t]+\\([0-9]+\\):\n[^\n]*\n[^\n]*\n\\[\\(Warning\\|Error\\|Failure\\)\\][^\n]*" 2 bold t)
    )
  "*Keywords to also highlight in Verilog *compilation* buffers."
  )

(defcustom verilog-library-directories '(".")
  "*List of directories when looking for files for /*AUTOINST*/.
The directory may be relative to the current file, or absolute.
Environment variables are also expanded in the directory names.
Having at least the current directory is a good idea.

You might want these defined in each file; put at the *END* of your file
something like:

// Local Variables:
// verilog-library-directories:(\".\" \"subdir\" \"subdir2\")
// End:

Verilog-mode attempts to detect changes to this local variable, but they
are only insured to be correct when the file is first visited. Thus if you
have problems, use \\[find-alternate-file] RET to have these take effect.

See also verilog-library-files and verilog-library-extensions."
  :group 'verilog-mode-auto
  :type '(repeat file))

(defcustom verilog-library-files '(".")
  "*List of files to search for modules in when looking for files for
/*AUTOINST*/.  This is a complete path, usually to a technology file with
many standard cells defined in it.

You might want these defined in each file; put at the *END* of your file
something like:

// Local Variables:
// verilog-library-files:(\"/some/path/technology.v\" \"/some/path/tech2.v\")
// End:

Verilog-mode attempts to detect changes to this local variable, but they
are only insured to be correct when the file is first visited. Thus if you
have problems, use \\[find-alternate-file] RET to have these take effect.

See also verilog-library-directories."
  :group 'verilog-mode-auto
  :type '(repeat directory))

(defcustom verilog-library-extensions '(".v")
  "*List of extensions to use when looking for files for /*AUTOINST*/.
See also `verilog-library-directories'."
  :type '(repeat string)
  :group 'verilog-mode-auto)

(defcustom verilog-auto-sense-include-inputs nil
  "*If true, AUTOSENSE should include all inputs.
If nil, only inputs that are NOT output signals in the same block are
included."
  :type 'boolean
  :group 'verilog-mode-auto)

(defcustom verilog-auto-sense-defines-constant nil
  "*If true, AUTOSENSE should assume all defines represent constants.
When true, the defines will not be included in sensitivity lists.  To
maintain compatibility with other sites, this should be set at the bottom
of each verilog file that requires it, rather then being set globally."
  :type 'boolean
  :group 'verilog-mode-auto)

(defcustom verilog-auto-reset-widths nil
  "*If true, AUTORESET should determine the width of signals and use it
to widen the zero (32'h0 for example).  This is required by some lint
tools that aren't smart enough to ignore widths of the constant zero.
This is not on my default, as it results in ugly code when parameters
determine the MSB or LSB of a signal inside a AUTORESET."
  :type 'boolean
  :group 'verilog-mode-auto)

(defcustom verilog-assignment-delay ""
  "*Text used for delayes in delayed assignments.  Add a trailing space if set."
  :type 'string
  :group 'verilog-mode-auto)

(defcustom verilog-auto-inst-vector t
  "*If true, when creating default ports with AUTOINST, use bus subscripts.
If nil, skip the subscript when it matches the entire bus as declared in
the module (AUTOWIRE signals always are subscripted, you must manually
declare the wire to have the subscrips removed.)  Nil may speed up some
simulators, but is less general and harder to read, so avoid."
  :group 'verilog-mode-auto
  :type 'boolean )

(defcustom verilog-auto-inst-template-numbers nil
  "*If true, when creating templated ports with AUTOINST, add a comment.
The comment will add the line number of the template that was used for that
port declaration.  Setting this aids in debugging, but nil is suggested for
regular use to prevent large numbers of merge conflicts."
  :group 'verilog-mode-auto
  :type 'boolean )

(defcustom verilog-mode-hook   'verilog-set-compile-command
  "*Hook (List of functions) run after verilog mode is loaded."
  :type 'hook
  :group 'verilog-mode)

(defcustom verilog-before-auto-hook nil
  "*Hook run before `verilog-mode' updates AUTOs."
  :type 'hook
  :group 'verilog-mode-auto
)

(defcustom verilog-auto-hook nil
  "*Hook run after `verilog-mode' updates AUTOs."
  :type 'hook
  :group 'verilog-mode-auto)

(defvar verilog-auto-lineup '(all)
  "*List of contexts where auto lineup of :'s or ='s should be done.
Elements can be of type: 'declaration' or 'case', which will do auto
lineup in declarations or case-statements respectively.  The word 'all'
will do all lineups.  '(case declaration) for instance will do lineup
in case-statements and parameter list, while '(all) will do all
lineups.")

(defvar verilog-mode-abbrev-table nil
  "Abbrev table in use in Verilog-mode buffers.")

(defvar verilog-imenu-generic-expression
  '((nil "^\\s-*\\(\\(m\\(odule\\|acromodule\\)\\)\\|primitive\\)\\s-+\\([a-zA-Z0-9_.:]+\\)" 4)
    ("*Vars*" "^\\s-*\\(reg\\|wire\\)\\s-+\\(\\|\\[[^]]+\\]\\s-+\\)\\([A-Za-z0-9_]+\\)" 3))
  "Imenu expression for Verilog-mode.  See `imenu-generic-expression'.")

(defvar verilog-mode-abbrev-table nil
  "Abbrev table in use in Verilog-mode buffers.")

;;
;; provide a verilog-header function.
;; Customization variables:
;;
(defvar verilog-date-scientific-format nil
  "*If non-nil, dates are written in scientific format (e.g.  1997/09/17).
If nil, in European format (e.g.  17.09.1997).  The brain-dead American
format (e.g.  09/17/1997) is not supported.")

(defvar verilog-company nil
  "*Default name of Company for verilog header.
If set will become buffer local.")

(defvar verilog-project nil
  "*Default name of Project for verilog header.
If set will become buffer local.")

(define-abbrev-table 'verilog-mode-abbrev-table ())

(defvar verilog-mode-map ()
  "Keymap used in Verilog mode.")
(if verilog-mode-map
    ()
  (setq verilog-mode-map (make-sparse-keymap))
  (define-key verilog-mode-map ";"        'electric-verilog-semi)
  (define-key verilog-mode-map [(control 59)]    'electric-verilog-semi-with-comment)
  (define-key verilog-mode-map ":"        'electric-verilog-colon)
  (define-key verilog-mode-map "="        'electric-verilog-equal)
  (define-key verilog-mode-map "\`"       'electric-verilog-tick)
  (define-key verilog-mode-map "\t"       'electric-verilog-tab)
  (define-key verilog-mode-map "\r"       'electric-verilog-terminate-line)
  ;; backspace/delete key bindings
  (define-key verilog-mode-map [backspace]    'backward-delete-char-untabify)
  (unless (boundp 'delete-key-deletes-forward) ; XEmacs variable
    (define-key verilog-mode-map [delete]       'delete-char)
    (define-key verilog-mode-map [(meta delete)] 'kill-word))
  (define-key verilog-mode-map "\M-\C-b"  'electric-verilog-backward-sexp)
  (define-key verilog-mode-map "\M-\C-f"  'electric-verilog-forward-sexp)
  (define-key verilog-mode-map "\M-\r"    (function (lambda ()
		      (interactive) (electric-verilog-terminate-line 1))))
  (define-key verilog-mode-map "\M-\t"    'verilog-complete-word)
  (define-key verilog-mode-map "\M-?"     'verilog-show-completions)
  (define-key verilog-mode-map [(meta control h)] 'verilog-mark-defun)
  (define-key verilog-mode-map "\C-c\`"   'verilog-lint-off)
  (define-key verilog-mode-map "\C-c\C-r" 'verilog-label-be)
  (define-key verilog-mode-map "\C-c\C-i" 'verilog-pretty-declarations)
  (define-key verilog-mode-map "\C-c="    'verilog-pretty-expr)
  (define-key verilog-mode-map "\C-c\C-b" 'verilog-submit-bug-report)
  (define-key verilog-mode-map "\M-*"     'verilog-star-comment)
  (define-key verilog-mode-map "\C-c\C-c" 'verilog-comment-region)
  (define-key verilog-mode-map "\C-c\C-u" 'verilog-uncomment-region)
  (define-key verilog-mode-map "\M-\C-a"  'verilog-beg-of-defun)
  (define-key verilog-mode-map "\M-\C-e"  'verilog-end-of-defun)
  (define-key verilog-mode-map "\C-c\C-d" 'verilog-goto-defun)
  (define-key verilog-mode-map "\C-c\C-k" 'verilog-delete-auto)
  (define-key verilog-mode-map "\C-c\C-a" 'verilog-auto)
  (define-key verilog-mode-map "\C-c\C-s" 'verilog-auto-save-compile)
  (define-key verilog-mode-map "\C-c\C-e" 'verilog-expand-vector)
  (define-key verilog-mode-map "\C-c\C-h" 'verilog-header)
  )

;; menus
(defvar verilog-which-tool 1)
(defvar verilog-xemacs-menu
  '("Verilog"
    ("Choose Compilation Action"
     ["Lint"
      (progn
	(setq verilog-tool 'verilog-linter)
	(setq verilog-which-tool 1)
	(verilog-set-compile-command))
      :style radio
      :selected (= verilog-which-tool 1)]
     ["Coverage"
      (progn
	(setq verilog-tool 'verilog-coverage)
	(setq verilog-which-tool 2)
	(verilog-set-compile-command))
      :style radio
      :selected (= verilog-which-tool 2)]
     ["Simulator"
      (progn
	(setq verilog-tool 'verilog-simulator)
	(setq verilog-which-tool 3)
	(verilog-set-compile-command))
      :style radio
      :selected (= verilog-which-tool 3)]
     ["Compiler"
      (progn
	(setq verilog-tool 'verilog-compiler)
	(setq verilog-which-tool 4)
	(verilog-set-compile-command))
      :style radio
      :selected (= verilog-which-tool 4)]
     )
    ("Move"
     ["Beginning of function"		verilog-beg-of-defun t]
     ["End of function"			verilog-end-of-defun t]
     ["Mark function"			verilog-mark-defun t]
     ["Goto function"			verilog-goto-defun t]
     ["Move to beginning of block"	electric-verilog-backward-sexp t]
     ["Move to end of block"		electric-verilog-forward-sexp t]
     )
    ("Comments"
     ["Comment Region"			verilog-comment-region t]
     ["UnComment Region"			verilog-uncomment-region t]
     ["Multi-line comment insert"	verilog-star-comment t]
     ["Lint error to comment"		verilog-lint-off t]
     )
    "----"
    ["Compile"				compile t]
    ["AUTO, Save, Compile"		verilog-auto-save-compile t]
    ["Next Compile Error"		next-error t]
    ["Ignore Lint Warning at point"	verilog-lint-off t]
    "----"
    ["Line up declarations around point"	verilog-pretty-declarations t]
    ["Line up equations around point"		verilog-pretty-expr t]
    ["Redo/insert comments on every end"	verilog-label-be t]
    ["Expand [x:y] vector line"		verilog-expand-vector t]
    ["Insert begin-end block"		verilog-insert-block t]
    ["Complete word"			verilog-complete-word t]
    "----"
    ["Recompute AUTOs"			verilog-auto t]
    ["Kill AUTOs"			verilog-delete-auto t]
    ("AUTO Help..."
     ["AUTO General"			(describe-function 'verilog-auto) t]
     ["AUTO Library Path"		(describe-variable 'verilog-library-directories) t]
     ["AUTO Library Files"		(describe-variable 'verilog-library-files) t]
     ["AUTO Library Extensions"		(describe-variable 'verilog-library-extensions) t]
     ["AUTO `define Reading"		(describe-function 'verilog-read-defines) t]
     ["AUTO `include Reading"		(describe-function 'verilog-read-includes) t]
     ["AUTOARG"				(describe-function 'verilog-auto-arg) t]
     ["AUTOASCIIENUM"			(describe-function 'verilog-auto-ascii-enum) t]
     ["AUTOINOUTMODULE"			(describe-function 'verilog-auto-inout-module) t]
     ["AUTOINOUT"			(describe-function 'verilog-auto-inout) t]
     ["AUTOINPUT"			(describe-function 'verilog-auto-input) t]
     ["AUTOINST"			(describe-function 'verilog-auto-inst) t]
     ["AUTOOUTPUT"			(describe-function 'verilog-auto-output) t]
     ["AUTOOUTPUTEVERY"			(describe-function 'verilog-auto-output-every) t]
     ["AUTOREG"				(describe-function 'verilog-auto-reg) t]
     ["AUTOREGINPUT"			(describe-function 'verilog-auto-reg-input) t]
     ["AUTORESET"			(describe-function 'verilog-auto-reset) t]
     ["AUTOSENSE"			(describe-function 'verilog-auto-sense) t]
     ["AUTOWIRE"			(describe-function 'verilog-auto-wire) t]
     )
    "----"
    ["Submit bug report"		verilog-submit-bug-report t]
    ["Customize Verilog Mode..."	verilog-customize t]
    ["Customize Verilog Fonts & Colors"	verilog-font-customize t]
    )
  "Emacs menu for VERILOG mode."
  )
(unless verilog-running-on-xemacs
    (easy-menu-define verilog-menu verilog-mode-map "Menu for Verilog mode"
		      verilog-xemacs-menu))

(defvar verilog-mode-abbrev-table nil
  "Abbrev table in use in Verilog-mode buffers.")

(define-abbrev-table 'verilog-mode-abbrev-table ())

;; compilation program
(defun verilog-set-compile-command ()
  "Function to compute shell command to compile verilog.
Can be the path and arguments for your Verilog simulator:
  \"vcs -p123 -O\"
or a string like
  \"(cd /tmp; surecov %s)\".
In the former case, the path to the current buffer is concat'ed to the
value of verilog-tool; in the later, the path to the current buffer is
substituted for the %s.

Where __FILE__ appears in the string, the buffer-file-name of the current
buffer, without the directory portion, will be substituted."
  (interactive)
  (or (file-exists-p "makefile")	;If there is a makefile, use it
      (file-exists-p "Makefile")
      (progn (make-local-variable 'compile-command)
	     (setq compile-command
		   (if (string-match "%s" (eval verilog-tool))
		       (format (eval verilog-tool) (or buffer-file-name ""))
		     (concat (eval verilog-tool) " " (or buffer-file-name ""))))))
  (verilog-modify-compile-command))

(defun verilog-modify-compile-command ()
  "Replace meta-information in compile-command.
Where __FILE__ appears in the string, the buffer-file-name of the current
buffer, without the directory portion, will be substituted."
  (when (string-match "\\b__FILE__\\b" compile-command)
    (make-local-variable 'compile-command)
    (setq compile-command
	  (verilog-string-replace-matches
	   "\\b__FILE__\\b" (file-name-nondirectory (buffer-file-name))
	   t t compile-command))))

(defun verilog-error-regexp-add ()
  "Add the messages to the `compilation-error-regexp-alist'.
Called by `compilation-mode-hook'.  This allows \\[next-error] to find the errors."
  (if (not verilog-error-regexp-add-didit)
      (progn
	(setq verilog-error-regexp-add-didit t)
	(setq-default compilation-error-regexp-alist
		      (append (default-value 'compilation-error-regexp-alist)
			      verilog-error-regexp))
	;; Could be buffer local at this point; maybe also in let; change all three
	(setq compilation-error-regexp-alist (default-value 'compilation-error-regexp-alist))
	(set (make-local-variable 'compilation-error-regexp-alist)
	     (default-value 'compilation-error-regexp-alist))
	)))

(add-hook 'compilation-mode-hook 'verilog-error-regexp-add)

(defconst verilog-directive-re
  ;; "`case" "`default" "`define" "`define" "`else" "`endfor" "`endif"
  ;; "`endprotect" "`endswitch" "`endwhile" "`for" "`format" "`if" "`ifdef"
  ;; "`ifndef" "`include" "`let" "`protect" "`switch" "`timescale"
  ;; "`time_scale" "`undef" "`while"
  "\\<`\\(case\\|def\\(ault\\|ine\\(\\)?\\)\\|e\\(lse\\|nd\\(for\\|if\\|protect\\|switch\\|while\\)\\)\\|for\\(mat\\)?\\|i\\(f\\(def\\|ndef\\)?\\|nclude\\)\\|let\\|protect\\|switch\\|time\\(_scale\\|scale\\)\\|undef\\|while\\)\\>")

(defconst verilog-directive-begin
  "\\<`\\(for\\|i\\(f\\|fdef\\|fndef\\)\\|switch\\|while\\)\\>")

(defconst verilog-directive-middle
  "\\<`\\(else\\|default\\|case\\)\\>")

(defconst verilog-directive-end
  "`\\(endfor\\|endif\\|endswitch\\|endwhile\\)\\>")

(defconst verilog-directive-re-1
  (concat "[ \t]*"  verilog-directive-re))

;;
;; Regular expressions used to calculate indent, etc.
;;
(defconst verilog-symbol-re      "\\<[a-zA-Z_][a-zA-Z_0-9.]*\\>")
(defconst verilog-case-re        "\\(\\<case[xz]?\\>\\)")
;; Want to match
;; aa :
;; aa,bb :
;; a[34:32] :
;; a,
;;   b :
(defconst
  verilog-no-indent-begin-re
  "\\<\\(if\\|else\\|while\\|for\\|repeat\\|always\\|always_comb\\|always_ff\\|always_latch\\)\\>")
(defconst verilog-ends-re
  (concat
   "\\(\\<else\\>\\)\\|"		; 1
   "\\(\\<if\\>\\)\\|"			; 2
   "\\(\\<end\\>\\)\\|"			; 3
   "\\(\\<endcase\\>\\)\\|"		; 4
   "\\(\\<endfunction\\>\\)\\|"		; 5
   "\\(\\<endtask\\>\\)\\|"		; 6
   "\\(\\<endspecify\\>\\)\\|"		; 7
   "\\(\\<endtable\\>\\)\\|"		; 8
   "\\(\\<endgenerate\\>\\)\\|"         ; 9
   "\\(\\<join\\(_any\\|_none\\)?\\>\\)" ; 10
   ))

(defconst verilog-autoindent-lines-re
  ;; Matches to names in this list cause auto-end-commentation
  ;;
  ;; "macromodule" "module" "primitive" "interface" "end" "endcase" "endfunction"
  ;; "endtask" "endmodule" "endprimitive" "endinterface" "endspecify" "endtable" "join"
  ;; "begin" "else" "`else" "`ifdef" "`endif" "`define" "`undef" "`include"
  (concat "\\("
	  verilog-directive-re
	  "\\|\\(\\<begin\\>\\|"
	  "e\\(lse\\>\\|nd\\(\\>\\|case\\>\\|function\\>\\|module\\>\\|primitive\\>\\|interface\\>\\|package\\>"
	  "\\|specify\\>\\|ta\\(ble\\>\\|sk\\>\\)\\)\\)\\|"
	  "join\\(_any\\|_none\\)?\\>\\|m\\(acromodule\\>\\|odule\\>\\)\\|primitive\\>\\|interface\\>\\|package\\>\\)\\)" ))

(defconst verilog-enders-re
  (concat "\\(\\<endcase\\>\\)\\|" ; 1
	  "\\(\\<end\\>\\)\\|"     ; 2
	  "\\(\\<end"              ; 3, but not used
	  "\\("                    ; 4, but not used
	  "\\(function\\)\\|"      ; 5
	  "\\(task\\)\\|"          ; 6
	  "\\(module\\)\\|"        ; 7
	  "\\(primitive\\)\\|"     ; 8
	  "\\(interface\\)\\|"     ; 9
	  "\\(package\\)"          ; 10
	  "\\)\\>\\)"))
(defconst verilog-endcomment-reason-re
  (concat
   "\\(\\<fork\\>\\)\\|"
   "\\(\\<begin\\>\\)\\|"
   "\\(\\<if\\>\\)\\|"
   "\\(\\<else\\>\\)\\|"
   "\\(\\<end\\>.*\\<else\\>\\)\\|"
   "\\(\\<task\\>\\)\\|"
   "\\(\\<function\\>\\)\\|"
   "\\(\\<initial\\>\\)\\|"
   "\\(\\<interface\\>\\)\\|"
   "\\(\\<package\\>\\)\\|"
   "\\(\\<final\\>\\)\\|"
   "\\(\\<always\\>\\(\[ \t\]*@\\)?\\)\\|"
   "\\(\\<always_comb\\>\\(\[ \t\]*@\\)?\\)\\|"
   "\\(\\<always_ff\\>\\(\[ \t\]*@\\)?\\)\\|"
   "\\(\\<always_latch\\>\\(\[ \t\]*@\\)?\\)\\|"
   "\\(@\\)\\|"
   "\\(\\<while\\>\\)\\|"
   "\\(\\<for\\(ever\\)?\\>\\)\\|"
   "\\(\\<repeat\\>\\)\\|\\(\\<wait\\>\\)\\|"
   "#"))

(defconst verilog-named-block-re  "begin[ \t]*:")
(defconst verilog-beg-block-re
  ;; "begin" "case" "casex" "fork" "casez" "table" "specify" "function" "task"
  "\\(\\<\\(begin\\>\\|case\\(\\>\\|x\\>\\|z\\>\\)\\|generate\\|f\\(ork\\>\\|unction\\>\\)\\|specify\\>\\|ta\\(ble\\>\\|sk\\>\\)\\)\\)")

(defconst verilog-beg-block-re-1
  "\\<\\(begin\\)\\|\\(case[xz]?\\)\\|\\(fork\\)\\|\\(table\\)\\|\\(specify\\)\\|\\(function\\)\\|\\(task\\)\\|\\(generate\\)\\>")
(defconst verilog-end-block-re
  ;; "end" "join" "endcase" "endtable" "endspecify" "endtask" "endfunction" "endgenerate"
  "\\<\\(end\\(\\>\\|case\\>\\|function\\>\\|generate\\>\\|specify\\>\\|ta\\(ble\\>\\|sk\\>\\)\\)\\|join\\(_any\\|_none\\)?\\>\\)")

(defconst verilog-end-block-re-1 "\\(\\<end\\>\\)\\|\\(\\<endcase\\>\\)\\|\\(\\<join\\>\\)\\|\\(\\<endtable\\>\\)\\|\\(\\<endspecify\\>\\)\\|\\(\\<endfunction\\>\\)\\|\\(\\<endgenerate\\>\\)\\|\\(\\<endtask\\>\\)")
(defconst verilog-declaration-re
  (eval-when-compile
    (concat "\\<"
	    (verilog-regexp-opt
	     (list
	      "assign" "defparam" "event" "inout" "input" "integer" "localparam" "output"
	      "parameter" "real" "realtime" "reg" "supply" "supply0" "supply1" "time"
	      "tri" "tri0" "tri1" "triand" "trior" "trireg" "wand" "wire" "typedef"
	      "struct" "logic" "bit" "genvar" "wor") t )
	    "\\>")))
(defconst verilog-range-re "\\[[^]]*\\]")
(defconst verilog-macroexp-re "`\\sw+")
(defconst verilog-delay-re "#\\s-*\\(\\([0-9_]+\\('s?[hdxbo][0-9a-fA-F_xz]+\\)?\\)\\|\\(([^)]*)\\)\\|\\(\\sw+\\)\\)")
(defconst verilog-declaration-re-2-no-macro
  (concat "\\s-*" verilog-declaration-re
	  "\\s-*\\(\\(" verilog-range-re "\\)\\|\\(" verilog-delay-re "\\)"
;	  "\\|\\(" verilog-macroexp-re "\\)"
	  "\\)?"))
(defconst verilog-declaration-re-2-macro
  (concat "\\s-*" verilog-declaration-re
	  "\\s-*\\(\\(" verilog-range-re "\\)\\|\\(" verilog-delay-re "\\)"
	  "\\|\\(" verilog-macroexp-re "\\)"
	  "\\)?"))
(defconst verilog-declaration-re-1-macro (concat "^" verilog-declaration-re-2-macro))
(defconst verilog-declaration-re-1-no-macro (concat "^" verilog-declaration-re-2-no-macro))
(defconst verilog-defun-re
  ;;"module" "macromodule" "primitive"
  "\\(\\<\\(m\\(acromodule\\>\\|odule\\>\\)\\|interface\\>\\|package\\>\\|primitive\\>\\)\\)")
(defconst verilog-end-defun-re
  ;; "endmodule" "endprimitive"
"\\(\\<end\\(module\\>\\|primitive\\>\\|interface\\>\\|package\\>\\)\\)")
(defconst verilog-zero-indent-re
  (concat verilog-defun-re "\\|" verilog-end-defun-re))
(defconst verilog-directive-re
  ;; "`case" "`default" "`define" "`define" "`else" "`endfor" "`endif"
  ;; "`endprotect" "`endswitch" "`endwhile" "`for" "`format" "`if" "`ifdef"
  ;; "`ifndef" "`include" "`let" "`protect" "`switch" "`timescale"
  ;; "`time_scale" "`undef" "`while"
  "\\<`\\(case\\|def\\(ault\\|ine\\(\\)?\\)\\|e\\(lse\\|nd\\(for\\|if\\|protect\\|switch\\|while\\)\\)\\|for\\(mat\\)?\\|i\\(f\\(def\\|ndef\\)?\\|nclude\\)\\|let\\|protect\\|switch\\|time\\(_scale\\|scale\\)\\|undef\\|while\\)\\>")

(defconst verilog-directive-begin
  "\\<`\\(for\\|i\\(f\\|fdef\\|fndef\\)\\|switch\\|while\\)\\>")

(defconst verilog-directive-middle
  "\\<`\\(else\\|default\\|case\\)\\>")

(defconst verilog-directive-end
  "`\\(endfor\\|endif\\|endswitch\\|endwhile\\)\\>")

(defconst verilog-directive-re-1
  (concat "[ \t]*"  verilog-directive-re))

(defconst verilog-autoindent-lines-re
  ;; "macromodule" "module" "primitive" "end" "endcase" "endfunction"
  ;; "endtask" "endmodule" "endprimitive" "endspecify" "endtable" "join"
  ;; "begin" "else" "`else" "`ifdef" "`endif" "`define" "`undef" "`include"
  (concat "\\("
	  verilog-directive-re
	  "\\|\\(\\<begin\\>\\|e\\(lse\\>\\|nd\\(\\>\\|case\\>\\|function\\>\\|module\\>\\|primitive\\>\\|specify\\>\\|ta\\(ble\\>\\|sk\\>\\)\\)\\)\\|join\\>\\|m\\(acromodule\\>\\|odule\\>\\)\\|primitive\\>\\)\\)" ))

(defconst verilog-behavioral-block-beg-re
  (concat "\\(\\<initial\\>\\|\\<final\\>\\|\\<always\\>\\|\\<always_comb\\>\\|\\<always_ff\\>\\|"
	  "\\<always_latch\\>\\|\\<function\\>\\|\\<task\\>\\)"))
(defconst verilog-indent-reg
  (concat
   "\\(\\<begin\\>\\|\\<case[xz]?\\>\\|\\<specify\\>\\|\\<fork\\>\\|\\<table\\>\\)\\|"
   "\\(\\<end\\>\\|\\<join\\(_any\\|_none\\)?\\>\\|\\<endcase\\>\\|\\<endtable\\>\\|\\<endspecify\\>\\)\\|"
   "\\(\\<module\\>\\|\\<macromodule\\>\\|\\<primitive\\>\\|\\<interface\\>\\|\\<package\\>\\|\\<initial\\>\\|\\<final\\>\\|"
   "\\<always\\>\\|\\<always_comb\\>\\|\\<always_ff\\>\\|\\<always_latch\\>\\)\\|"
   "\\(\\<endmodule\\>\\|\\<endprimitive\\>\\|\\<endinterface\\>\\|\\<endpackage\\>\\)\\|"
   "\\(\\<generate\\>\\|\\<endgenerate\\>\\)\\|"
   "\\(\\<endtask\\>\\|\\<endfunction\\>\\)\\|"
   "\\(\\<function\\>\\|\\<task\\>\\)"
   ;;	  "\\|\\(\\<if\\>\\|\\<else\\>\\)"
   ))
(defconst verilog-indent-re
  (concat
   "\\(\\<\\(always\\(_latch\\|_ff\\|_comb\\)?\\>\\|begin\\>\\|case\\(\\>\\|x\\>\\|z\\>\\)\\|"
   "end\\(\\>\\|case\\>\\|function\\>\\|generate\\>\\|module\\>\\|primitive\\>\\|interface\\>\\|package\\>\\|specify\\>\\|ta\\(ble\\>\\|sk\\>\\)\\)"
   "\\|f\\(ork\\>\\|unction\\>\\)\\|generate\\|initial\\>\\|join\\(_any\\|_none\\)?\\>\\|m\\(acromodule\\>\\|odule\\>\\)"
   "\\|primitive\\>\\|interface\\>\\|package\\>\\|specify\\>\\|ta\\(ble\\>\\|sk\\>\\)\\)"
   "\\|" verilog-directive-re "\\)"))

(defconst verilog-defun-level-re
  ;; "module" "macromodule" "primitive" "initial" "final" "always" "always_comb" "always_ff" "always_latch" "endtask" "endfunction"
  "\\(\\<\\(always\\(_latch\\|_ff\\|_comb\\)?\\>\\|end\\(function\\>\\|task\\>\\)\\|final\\|initial\\>\\|m\\(acromodule\\>\\|odule\\>\\)\\|primitive\\>\\|interface\\>\\|package\\>\\)\\)")
(defconst verilog-cpp-level-re
 ;;"endmodule" "endprimitive"
  "\\(\\<end\\(module\\>\\|primitive\\>\\|interface\\>\\|package\\>\\)\\)")
(defconst verilog-behavioral-level-re
  ;; "function" "task"
  "\\(\\<\\(function\\>\\|task\\>\\)\\)")
(defconst verilog-complete-reg
  ;; "always" "always_latch" "always_ff" "always_comb" "initial" "final" "repeat" "case" "casex" "casez" "while" "if" "for" "forever" "else" "parameter"
  "\\<\\(always\\(_latch\\|_ff\\|_comb\\)?\\|case\\(\\|[xz]\\)\\|begin\\|else\\|generate\\|for\\(\\|ever\\)\\|final\\|i\\(f\\|nitial\\)\\|parameter\\|repeat\\|while\\)\\>")
(defconst verilog-end-statement-re
  (concat "\\(" verilog-beg-block-re "\\)\\|\\("
	  verilog-end-block-re "\\)"))
(defconst verilog-endcase-re
  (concat verilog-case-re "\\|"
	  "\\(endcase\\)\\|"
	  verilog-defun-re
	  ))
;; Strings used to mark beginning and end of excluded text
(defconst verilog-exclude-str-start "/* -----\\/----- EXCLUDED -----\\/-----")
(defconst verilog-exclude-str-end " -----/\\----- EXCLUDED -----/\\----- */")

(defconst verilog-keywords
  '( "`define" "`else" "`endif" "`ifdef"
     "`include" "`timescale" "`undef" 
     "alias" "always" "always_comb" "always_ff" "always_latch" "and"
     "assert" "assign" "assume" "automatic" "before" "begin" "bind"
     "bins" "binsof" "bit" "break" "buf" "bufif0" "bufif1" "byte"
     "case" "casex" "casez" "cell" "chandle" "class" "clocking" "cmos"
     "config" "const" "constraint" "context" "continue" "cover"
     "covergroup" "coverpoint" "cross" "deassign" "default" "defparam"
     "design" "disable" "dist" "do" "edge" "else" "end" "endcase"
     "endclass" "endclocking" "endconfig" "endfunction" "endgenerate"
     "endgroup" "endinterface" "endmodule" "endpackage" "endprimitive"
     "endprogram" "endproperty" "endspecify" "endsequence" "endtable"
     "endtask" "enum" "event" "expect" "export" "extends" "extern"
     "final" "first_match" "for" "force" "foreach" "forever" "fork"
     "forkjoin" "function" "generate" "genvar" "highz0" "highz1" "if"
     "iff" "ifnone" "ignore_bins" "illegal_bins" "import" "incdir"
     "include" "initial" "inout" "input" "inside" "instance" "int"
     "integer" "interface" "intersect" "join" "join_any" "join_none"
     "large" "liblist" "library" "local" "localparam" "logic"
     "longint" "macromodule" "matches" "medium" "modport" "module"
     "nand" "negedge" "new" "nmos" "nor" "noshowcancelled" "not"
     "notif0" "notif1" "null" "or" "output" "package" "packed"
     "parameter" "pmos" "posedge" "primitive" "priority" "program"
     "property" "protected" "pull0" "pull1" "pulldown" "pullup"
     "pulsestyle_onevent" "pulsestyle_ondetect" "pure" "rand" "randc"
     "randcase" "randsequence" "rcmos" "real" "realtime" "ref" "reg"
     "release" "repeat" "return" "rnmos" "rpmos" "rtran" "rtranif0"
     "rtranif1" "scalared" "sequence" "shortint" "shortreal"
     "showcancelled" "signed" "small" "solve" "specify" "specparam"
     "static" "string" "strong0" "strong1" "struct" "super" "supply0"
     "supply1" "table" "tagged" "task" "this" "throughout" "time"
     "timeprecision" "timeunit" "tran" "tranif0" "tranif1" "tri"
     "tri0" "tri1" "triand" "trior" "trireg" "type" "typedef" "union"
     "unique" "unsigned" "use" "var" "vectored" "virtual" "void"
     "wait" "wait_order" "wand" "weak0" "weak1" "while" "wildcard"
     "wire" "with" "within" "wor" "xnor" "xor"
 )
 "List of Verilog keywords.")


(defconst verilog-emacs-features
  (let ((major (and (boundp 'emacs-major-version)
		    emacs-major-version))
	(minor (and (boundp 'emacs-minor-version)
		    emacs-minor-version))
	flavor comments flock-syntax)
    ;; figure out version numbers if not already discovered
    (and (or (not major) (not minor))
	 (string-match "\\([0-9]+\\).\\([0-9]+\\)" emacs-version)
	 (setq major (string-to-int (substring emacs-version
					       (match-beginning 1)
					       (match-end 1)))
	       minor (string-to-int (substring emacs-version
					       (match-beginning 2)
					       (match-end 2)))))
    (if (not (and major minor))
	(error "Cannot figure out the major and minor version numbers"))
    ;; calculate the major version
    (cond
     ((= major 4)  (setq major 'v18))	;Epoch 4
     ((= major 18) (setq major 'v18))	;Emacs 18
     ((= major 19) (setq major 'v19	;Emacs 19
			 flavor (if (or (string-match "Lucid" emacs-version)
					(string-match "XEmacs" emacs-version))
				    'XEmacs 'FSF)))
     ((> major 19) (setq major 'v20
			 flavor (if (or (string-match "Lucid" emacs-version)
					(string-match "XEmacs" emacs-version))
				    'XEmacs 'FSF)))
     ;; I don't know
     (t (error "Cannot recognize major version number: %s" major)))
    ;; XEmacs 19 uses 8-bit modify-syntax-entry flags, as do all
    ;; patched Emacs 19, Emacs 18, Epoch 4's.  Only Emacs 19 uses a
    ;; 1-bit flag.  Let's be as smart as we can about figuring this
    ;; out.
    (if (or (eq major 'v20) (eq major 'v19))
	(let ((table (copy-syntax-table)))
	  (modify-syntax-entry ?a ". 12345678" table)
	  (cond
	   ;; XEmacs pre 20 and Emacs pre 19.30 use vectors for syntax tables.
	   ((vectorp table)
	    (if (= (logand (lsh (aref table ?a) -16) 255) 255)
		(setq comments '8-bit)
	      (setq comments '1-bit)))
	   ;; XEmacs 20 is known to be 8-bit
	   ((eq flavor 'XEmacs) (setq comments '8-bit))
	   ;; Emacs 19.30 and beyond are known to be 1-bit
	   ((eq flavor 'FSF) (setq comments '1-bit))
	   ;; Don't know what this is
	   (t (error "Couldn't figure out syntax table format"))
	   ))
      ;; Emacs 18 has no support for dual comments
      (setq comments 'no-dual-comments))
    ;; determine whether to use old or new font lock syntax
    ;; We can assume 8-bit syntax table emacsen aupport new syntax, otherwise
    ;; look for version > 19.30
    (setq flock-syntax
        (if (or (equal comments '8-bit)
                (equal major 'v20)
                (and (equal major 'v19) (> minor 30)))
            'flock-syntax-after-1930
          'flock-syntax-before-1930))
    ;; lets do some minimal sanity checking.
    (if (or
	 ;; Lemacs before 19.6 had bugs
	 (and (eq major 'v19) (eq flavor 'XEmacs) (< minor 6))
	 ;; Emacs 19 before 19.21 has known bugs
	 (and (eq major 'v19) (eq flavor 'FSF) (< minor 21))
	 )
	(with-output-to-temp-buffer "*verilog-mode warnings*"
	  (print (format
"The version of Emacs that you are running, %s,
has known bugs in its syntax parsing routines which will affect the
performance of verilog-mode. You should strongly consider upgrading to the
latest available version.  verilog-mode may continue to work, after a
fashion, but strange indentation errors could be encountered."
		     emacs-version))))
    ;; Emacs 18, with no patch is not too good
    (if (and (eq major 'v18) (eq comments 'no-dual-comments))
	(with-output-to-temp-buffer "*verilog-mode warnings*"
	  (print (format
"The version of Emacs 18 you are running, %s,
has known deficiencies in its ability to handle the dual verilog
(and C++) comments, (e.g. the // and /* */ comments). This will
not be much of a problem for you if you only use the /* */ comments,
but you really should strongly consider upgrading to one of the latest
Emacs 19's.  In Emacs 18, you may also experience performance degradations.
Emacs 19 has some new built-in routines which will speed things up for you.
Because of these inherent problems, verilog-mode is not supported
on emacs-18."
			    emacs-version))))
    ;; Emacs 18 with the syntax patches are no longer supported
    (if (and (eq major 'v18) (not (eq comments 'no-dual-comments)))
	(with-output-to-temp-buffer "*verilog-mode warnings*"
	  (print (format
"You are running a syntax patched Emacs 18 variant.  While this should
work for you, you may want to consider upgrading to Emacs 19.
The syntax patches are no longer supported either for verilog-mode."))))
    (list major comments flock-syntax))
  "A list of features extant in the Emacs you are using.
There are many flavors of Emacs out there, each with different
features supporting those needed by `verilog-mode'.  Here's the current
supported list, along with the values for this variable:

 Vanilla Emacs 18/Epoch 4:   (v18 no-dual-comments flock-syntax-before-1930)
 Emacs 18/Epoch 4 (patch2):  (v18 8-bit flock-syntax-after-1930)
 XEmacs (formerly Lucid) 19: (v19 8-bit flock-syntax-after-1930)
 XEmacs 20:                  (v20 8-bit flock-syntax-after-1930)
 Emacs 19.1-19.30:           (v19 8-bit flock-syntax-before-1930)
 Emacs 19.31-19.xx:          (v19 8-bit flock-syntax-after-1930)
 Emacs20        :            (v20 1-bit flock-syntax-after-1930).")

(defconst verilog-comment-start-regexp "//\\|/\\*"
  "Dual comment value for `comment-start-regexp'.")

(defun verilog-populate-syntax-table (table)
  "Populate the syntax TABLE."
  (modify-syntax-entry ?\\ "\\" table)
  (modify-syntax-entry ?+ "." table)
  (modify-syntax-entry ?- "." table)
  (modify-syntax-entry ?= "." table)
  (modify-syntax-entry ?% "." table)
  (modify-syntax-entry ?< "." table)
  (modify-syntax-entry ?> "." table)
  (modify-syntax-entry ?& "." table)
  (modify-syntax-entry ?| "." table)
  (modify-syntax-entry ?` "w" table)
  (modify-syntax-entry ?_ "w" table)
  (modify-syntax-entry ?\' "." table)
)

(defun verilog-setup-dual-comments (table)
  "Set up TABLE to handle block and line style comments."
  (cond
   ((memq '8-bit verilog-emacs-features)
    ;; XEmacs (formerly Lucid) has the best implementation
    (modify-syntax-entry ?/  ". 1456" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    )
   ((memq '1-bit verilog-emacs-features)
    ;; Emacs 19 does things differently, but we can work with it
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    )
   ))

(defvar verilog-mode-syntax-table nil
  "Syntax table used in `verilog-mode' buffers.")

(defconst verilog-font-lock-keywords nil
  "Default highlighting for Verilog mode.")

(defconst verilog-font-lock-keywords-1 nil
  "Subdued level highlighting for Verilog mode.")

(defconst verilog-font-lock-keywords-2 nil
  "Medium level highlighting for Verilog mode.
See also `verilog-font-lock-extra-types'.")

(defconst verilog-font-lock-keywords-3 nil
  "Gaudy level highlighting for Verilog mode.
See also `verilog-font-lock-extra-types'.")
(defvar
  verilog-font-lock-translate-off-face
  'verilog-font-lock-translate-off-face
  "Font to use for translated off regions")
(defface verilog-font-lock-translate-off-face
  '((((class color)
      (background light))
     (:background "gray90" :italic t ))
    (((class color)
      (background dark))
     (:background "gray10" :italic t ))
    (((class grayscale) (background light))
     (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :italic t))
    (t (:italis t)))
  "Font lock mode face used to background highlight translate-off regions."
  :group 'font-lock-highlighting-faces)

(defvar
  verilog-font-lock-p1800-face
  'verilog-font-lock-p1800-face
  "Font to use for p1800 keywords")
(defface verilog-font-p1800-face
  '((((class color)
      (background light))
     (:foreground "DarkOrange3" :bold t ))
    (((class color)
      (background dark))
     (:foreground "gray10" :bold t ))
    (t (:italic t)))
  "Font lock mode face used to highlight P1800 keywords."
  :group 'font-lock-highlighting-faces)

(let* ((verilog-type-font-keywords
	(eval-when-compile
	  (verilog-regexp-opt
	   '(
	     "and" "bit" "buf" "bufif0" "bufif1" "cmos" "defparam"
	     "event" "genvar" "inout" "input" "integer" "localparam"
	     "logic" "nand" "nmos" "not" "notif0" "notif1" "or"
	     "output" "parameter" "pmos" "pull0" "pull1" "pullup"
	     "rcmos" "real" "realtime" "reg" "rnmos" "rpmos" "rtran"
	     "rtranif0" "rtranif1" "signed" "struct" "supply"
	     "supply0" "supply1" "time" "tran" "tranif0" "tranif1"
	     "tri" "tri0" "tri1" "triand" "trior" "trireg" "typedef"
	     "vectored" "wand" "wire" "wor" "xnor" "xor"
	     ) nil  )))

       (verilog-pragma-keywords
	(eval-when-compile
	  (verilog-regexp-opt
	   '("surefire" "synopsys" "rtl_synthesis" "verilint" ) nil
	    )))

       (verilog-p1800-keywords
	(eval-when-compile
	  (verilog-regexp-opt
	   '("alias" "assert" "assume" "automatic" "before" "bind"
	     "bins" "binsof" "break" "byte" "cell" "chandle" "class"
	     "clocking" "config" "const" "constraint" "context" "continue"
	     "cover" "covergroup" "coverpoint" "cross" "deassign" "design"
	     "dist" "do" "edge" "endclass" "endclocking" "endconfig"
	     "endgroup" "endprogram" "endproperty" "endsequence" "enum"
	     "expect" "export" "extends" "extern" "first_match" "foreach"
	     "forkjoin" "genvar" "highz0" "highz1" "ifnone" "ignore_bins"
	     "illegal_bins" "import" "incdir" "include" "inside" "instance"
	     "int" "intersect" "large" "liblist" "library" "local" "longint"
	     "matches" "medium" "modport" "new" "noshowcancelled" "null"
	     "packed" "program" "property" "protected" "pull0" "pull1"
	     "pulsestyle_onevent" "pulsestyle_ondetect" "pure" "rand" "randc"
	     "randcase" "randsequence" "ref" "release" "return" "scalared"
	     "sequence" "shortint" "shortreal" "showcancelled" "small" "solve"
	     "specparam" "static" "string" "strong0" "strong1" "struct"
	     "super" "tagged" "this" "throughout" "timeprecision" "timeunit"
	     "type" "union" "unsigned" "use" "var" "virtual" "void"
	     "wait_order" "weak0" "weak1" "wildcard" "with" "within"
	     ) nil )))


       (verilog-font-keywords
	(eval-when-compile
	  (verilog-regexp-opt
	   '(
	     "assign" "begin" "case" "casex" "casez" "deassign"
	     "default" "disable" "else" "end" "endcase" "endfunction"
	     "endgenerate" "endinterface" "endmodule" "endprimitive"
	     "endspecify" "endtable" "endtask" "final" "for" "force" "return" "break" "continue"
	     "forever" "fork" "function" "generate" "if" "iff" "initial"
	     "interface" "join" "join_any" "join_none" "macromodule" "module" "negedge"
	     "package" "endpackage" "always" "always_comb" "always_ff"
	     "always_latch" "posedge" "primitive" "priority" "release"
	     "repeat" "specify" "table" "task" "unique" "wait" "while"

	     ) nil  ))))

  (setq verilog-font-lock-keywords
	(list
	 ;; Fontify all builtin keywords
	 (concat "\\<\\(" verilog-font-keywords "\\|"
		       ;; And user/system tasks and functions
		       "\\$[a-zA-Z][a-zA-Z0-9_\\$]*"
		       "\\)\\>")
	 ;; Fontify all types
	 (cons (concat "\\<\\(" verilog-type-font-keywords "\\)\\>")
	       'font-lock-type-face)
	 ;; Fontify IEEE-P1800 keywords
	 (cons (concat "\\<\\(" verilog-p1800-keywords "\\)\\>")
	       'verilog-font-p1800-face)

	 ))

  (setq verilog-font-lock-keywords-1
	(append verilog-font-lock-keywords
		(list
		 ;; Fontify module definitions
		 (list
		  "\\<\\(\\(macro\\)?module\\|primitive\\|interface\\|package\\|task\\)\\>\\s-*\\(\\sw+\\)"
		  '(1 font-lock-keyword-face)
		  '(3 font-lock-function-name-face 'prepend))
		 ;; Fontify function definitions
		 (list
		  (concat "\\<function\\>\\s-+\\(integer\\|real\\(time\\)?\\|time\\)\\s-+\\(\\sw+\\)" )
		       '(1 font-lock-keyword-face)
		       '(3 font-lock-reference-face prepend)
		       )
		 '("\\<function\\>\\s-+\\(\\[[^]]+\\]\\)\\s-+\\(\\sw+\\)"
		   (1 font-lock-keyword-face)
		   (2 font-lock-reference-face append)
		   )
		 '("\\<function\\>\\s-+\\(\\sw+\\)"
		   1 'font-lock-reference-face append)
		 )))

  (setq verilog-font-lock-keywords-2
	(append verilog-font-lock-keywords-1
		(list
		 ;; Fontify pragmas
		 (concat "\\(//\\s-*" verilog-pragma-keywords "\\s-.*\\)")
		 ;; Fontify escaped names
		 '("\\(\\\\\\S-*\\s-\\)"  0 font-lock-function-name-face)
		 ;; Fontify macro definitions/ uses
		 '("`\\s-*[A-Za-z][A-Za-z0-9_]*" 0 font-lock-function-name-face)
		 ;; Fontify delays/numbers
		 '("\\(@\\)\\|\\(#\\s-*\\(\\(\[0-9_.\]+\\('s?[hdxbo][0-9a-fA-F_xz]*\\)?\\)\\|\\(([^)]+)\\|\\sw+\\)\\)\\)"
		   0 font-lock-type-face append)
		 )))

  (setq verilog-font-lock-keywords-3
	(append verilog-font-lock-keywords-2
		(when verilog-highlight-translate-off
		  (list
		   ;; Fontify things in translate off regions
		   '(verilog-match-translate-off (0 'verilog-font-lock-translate-off-face prepend))
		   )))
  )
  )


;;
;;  Macros
;;

(defsubst verilog-string-replace-matches (from-string to-string fixedcase literal string)
  "Replace occurrences of FROM-STRING with TO-STRING.
FIXEDCASE and LITERAL as in `replace-match`.  STRING is what to replace.
The case (verilog-string-replace-matches \"o\" \"oo\" nil nil \"foobar\")
will break, as the o's continuously replace.  xa -> x works ok though."
  ;; Hopefully soon to a emacs built-in
  (let ((start 0))
    (while (string-match from-string string start)
      (setq string (replace-match to-string fixedcase literal string)
	    start (min (length string) (match-end 0))))
    string))

(defsubst verilog-string-remove-spaces (string)
  "Remove spaces surrounding STRING."
  (save-match-data
    (setq string (verilog-string-replace-matches "^\\s-+" "" nil nil string))
    (setq string (verilog-string-replace-matches "\\s-+$" "" nil nil string))
    string))

(defsubst verilog-re-search-forward (REGEXP BOUND NOERROR)
  ; checkdoc-params: (REGEXP BOUND NOERROR)
  "Like `re-search-forward', but skips over match in comments or strings."
  (store-match-data '(nil nil))
  (while (and
	  (re-search-forward REGEXP BOUND NOERROR)
	  (and (verilog-skip-forward-comment-or-string)
	       (progn
		 (store-match-data '(nil nil))
		 (if BOUND
		     (< (point) BOUND)
		   t)
		 ))))
  (match-end 0))

(defsubst verilog-re-search-backward (REGEXP BOUND NOERROR)
  ; checkdoc-params: (REGEXP BOUND NOERROR)
  "Like `re-search-backward', but skips over match in comments or strings."
  (store-match-data '(nil nil))
  (while (and
	  (re-search-backward REGEXP BOUND NOERROR)
	  (and (verilog-skip-backward-comment-or-string)
	       (progn
		 (store-match-data '(nil nil))
		 (if BOUND
		     (> (point) BOUND)
		   t)
		 ))))
  (match-end 0))

(defsubst verilog-re-search-forward-quick (regexp bound noerror)
  "Like `verilog-re-search-forward', including use of REGEXP BOUND and NOERROR,
but trashes match data and is faster for REGEXP that doesn't match often.
This may at some point use text properties to ignore comments,
so there may be a large up front penalty for the first search."
  (let (pt)
    (while (and (not pt)
		(re-search-forward regexp bound noerror))
      (if (not (verilog-inside-comment-p))
	  (setq pt (match-end 0))))
    pt))

(defsubst verilog-re-search-backward-quick (regexp bound noerror)
  ; checkdoc-params: (REGEXP BOUND NOERROR)
  "Like `verilog-re-search-backward', including use of REGEXP BOUND and NOERROR,
but trashes match data and is faster for REGEXP that doesn't match often.
This may at some point use text properties to ignore comments,
so there may be a large up front penalty for the first search."
  (let (pt)
    (while (and (not pt)
		(re-search-backward regexp bound noerror))
      (if (not (verilog-inside-comment-p))
	  (setq pt (match-end 0))))
    pt))

(defsubst verilog-get-beg-of-line (&optional arg)
  (save-excursion
    (beginning-of-line arg)
    (point)))

(defsubst verilog-get-end-of-line (&optional arg)
  (save-excursion
    (end-of-line arg)
    (point)))

(defun verilog-inside-comment-p ()
  "Check if point inside a nested comment."
  (save-excursion
    (let ((st-point (point)) hitbeg)
      (or (search-backward "//" (verilog-get-beg-of-line) t)
	  (if (progn
		;; This is for tricky case //*, we keep searching if /* is proceeded by // on same line
		(while (and (setq hitbeg (search-backward "/*" nil t))
			    (progn (forward-char 1) (search-backward "//" (verilog-get-beg-of-line) t))))
		hitbeg)
	      (not (search-forward "*/" st-point t)))))))

(defun verilog-declaration-end ()
  (search-forward ";"))

(defun verilog-point-text (&optional pointnum)
  "Return text describing where POINTNUM or current point is (for errors).
Use filename, if current buffer being edited shorten to just buffer name."
  (concat (or (and (equal (window-buffer (selected-window)) (current-buffer))
		   (buffer-name))
	      buffer-file-name
	      (buffer-name))
	  ":" (int-to-string (count-lines (point-min) (or pointnum (point))))))

(defun electric-verilog-backward-sexp ()
  "Move backward over a sexp."
  (interactive)
  ;; before that see if we are in a comment
  (verilog-backward-sexp)
)
(defun electric-verilog-forward-sexp ()
  "Move backward over a sexp."
  (interactive)
  ;; before that see if we are in a comment
  (verilog-forward-sexp)
)

(defun verilog-backward-sexp ()
  (let ((reg)
	(elsec 1)
	(found nil)
	(st (point))
	)
    (if (not (looking-at "\\<"))
	(forward-word -1))
    (cond
     ((verilog-skip-backward-comment-or-string)
      )
     ((looking-at "\\<else\\>")
      (setq reg (concat
		 verilog-end-block-re
		 "\\|\\(\\<else\\>\\)"
		 "\\|\\(\\<if\\>\\)"
		 ))
      (while (and (not found)
		  (verilog-re-search-backward reg nil 'move))
	(cond
	 ((match-end 1) ; endblock
	; try to leap back to matching outward block by striding across
	; indent level changing tokens then immediately
	; previous line governs indentation.
	  (verilog-leap-to-head))
	 ((match-end 2) ; else, we're in deep
	  (setq elsec (1+ elsec)))
	 ((match-end 3) ; found it
	  (setq elsec (1- elsec))
	  (if (= 0 elsec)
	      ;; Now previous line describes syntax
	      (setq found 't)
	    ))
	 )
	)
      )
     ((looking-at verilog-end-block-re)
      (verilog-leap-to-head))
     ((looking-at "\\(endmodule\\>\\)\\|\\(\\<endprimitive\\>\\)\\|\\(\\<endinterface\\>\\)\\|\\(\\<endpackage\\>\\)")
      (cond
       ((match-end 1)
	(verilog-re-search-backward "\\<\\(macro\\)?module\\>" nil 'move))
       ((match-end 2)
	(verilog-re-search-backward "\\<primitive\\>" nil 'move))
       ((match-end 3)
	(verilog-re-search-backward "\\<interface\\>" nil 'move))
       ((match-end 4)
	(verilog-re-search-backward "\\<package\\>" nil 'move))
       (t
	(goto-char st)
	(backward-sexp 1))))
     (t
      (goto-char st)
      (backward-sexp))
     ) ;; cond
    ))

(defun verilog-forward-sexp ()
  (let ((reg)
	(st (point)))
    (if (not (looking-at "\\<"))
	(forward-word -1))
    (cond
     ((verilog-skip-forward-comment-or-string)
      (verilog-forward-syntactic-ws)
      )
     ((looking-at verilog-beg-block-re-1);; begin|case|fork|table|specify|function|task|generate
      (cond
       ((match-end 1) ; end
	;; Search forward for matching begin
	(setq reg "\\(\\<begin\\>\\)\\|\\(\\<end\\>\\)" ))
       ((match-end 2) ; endcase
	;; Search forward for matching case
	(setq reg "\\(\\<case[xz]?\\>[^:]\\)\\|\\(\\<endcase\\>\\)" ))
       ((match-end 3) ; join
	;; Search forward for matching fork
	(setq reg "\\(\\<fork\\>\\)\\|\\(\\<join\\(_any\\|_none\\)?\\>\\)" ))
       ((match-end 4) ; endtable
	;; Search forward for matching table
	(setq reg "\\(\\<table\\>\\)\\|\\(\\<endtable\\>\\)" ))
       ((match-end 5) ; endspecify
	;; Search forward for matching specify
	(setq reg "\\(\\<specify\\>\\)\\|\\(\\<endspecify\\>\\)" ))
       ((match-end 6) ; endfunction
	;; Search forward for matching function
	(setq reg "\\(\\<function\\>\\)\\|\\(\\<endfunction\\>\\)" ))
       ((match-end 7) ; endspecify
	;; Search forward for matching task
	(setq reg "\\(\\<task\\>\\)\\|\\(\\<endtask\\>\\)" ))
       ((match-end 8) ; endgenerate
	;; Search forward for matching generate
	(setq reg "\\(\\<generate\\>\\)\\|\\(\\<endgenerate\\>\\)" ))
       )
      (if (forward-word 1)
	  (catch 'skip
	    (let ((nest 1))
	      (while (verilog-re-search-forward reg nil 'move)
		(cond
		 ((match-end 2) ; end
		  (setq nest (1- nest))
		  (if (= 0 nest)
		      (throw 'skip 1)))
		 ((match-end 1) ; begin
		  (setq nest (1+ nest)))))
	      )))
      )
     ((looking-at "\\(\\<\\(macro\\)?module\\>\\)\\|\\(\\<primitive\\>\\)\\|\\<interface\\>\\)\\|\\(\\<package\\>\\)")
      (cond
       ((match-end 1)
	(verilog-re-search-forward "\\<endmodule\\>" nil 'move))
       ((match-end 2)
	(verilog-re-search-forward "\\<endprimitive\\>" nil 'move))
       ((match-end 3)
	(verilog-re-search-forward "\\<endinterface\\>" nil 'move))
       ((match-end 4)
	(verilog-re-search-forward "\\<endpackage\\>" nil 'move))
       (t
	(goto-char st)
	(if (= (following-char) ?\) )
	    (forward-char 1)
	  (forward-sexp 1)))))
     (t
      (goto-char st)
      (if (= (following-char) ?\) )
	  (forward-char 1)
	(forward-sexp 1)))
     ) ;; cond
    ))

(defun verilog-declaration-beg ()
  (verilog-re-search-backward verilog-declaration-re (bobp) t))

(defsubst verilog-within-string ()
  (save-excursion
    (nth 3 (parse-partial-sexp (verilog-get-beg-of-line) (point)))))

(require 'font-lock)
(defvar verilog-need-fld 1)
(defvar font-lock-defaults-alist nil)	;In case we are XEmacs

(defun verilog-font-lock-init ()
  "Initialize fontification."
  ;; highlight keywords and standardized types, attributes, enumeration
  ;; values, and subprograms
  (setq verilog-font-lock-keywords-3
	(append verilog-font-lock-keywords-2
		(when verilog-highlight-translate-off
		  (list
		   ;; Fontify things in translate off regions
		   '(verilog-match-translate-off (0 'verilog-font-lock-translate-off-face prepend))
		   ))
	)
  )
  (put 'verilog-mode 'font-lock-defaults
       '((verilog-font-lock-keywords
	  verilog-font-lock-keywords-1
	  verilog-font-lock-keywords-2
	  verilog-font-lock-keywords-3
	  )
	 nil ;; nil means highlight strings & comments as well as keywords
	 nil ;; nil means keywords must match case
	 nil ;; syntax table handled elsewhere
	 verilog-beg-of-defun ;; function to move to beginning of reasonable region to highlight
	 ))
  (if verilog-need-fld
      (let ((verilog-mode-defaults
	     '((verilog-font-lock-keywords
		verilog-font-lock-keywords-1
		verilog-font-lock-keywords-2
		verilog-font-lock-keywords-3
		)
	       nil ;; nil means highlight strings & comments as well as keywords
	       nil ;; nil means keywords must match case
	       nil ;; syntax table handled elsewhere
	       verilog-beg-of-defun ;; function to move to beginning of reasonable region to highlight
	       )))
	(setq font-lock-defaults-alist
	      (append
	       font-lock-defaults-alist
	       (list (cons 'verilog-mode  verilog-mode-defaults))))
	(setq verilog-need-fld 0))))

;; initialize fontification for Verilog Mode
(verilog-font-lock-init)


;;
;;
;;  Mode
;;

;;;###autoload
(defun verilog-mode ()
"Major mode for editing Verilog code.
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
\\[verilog-complete-word]\t-complete word with appropriate possibilities
   (functions, verilog keywords...)
\\[verilog-comment-region]\t- Put marked area in a comment, fixing
   nested comments.
\\[verilog-uncomment-region]\t- Uncomment an area commented with \
\\[verilog-comment-region].
\\[verilog-insert-block]\t- insert begin ... end;
\\[verilog-star-comment]\t- insert /* ... */
\\[verilog-mark-defun]\t- Mark function.
\\[verilog-beg-of-defun]\t- Move to beginning of current function.
\\[verilog-end-of-defun]\t- Move to end of current function.
\\[verilog-label-be]\t- Label matching begin ... end, fork ... join
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
\\[verilog-sk-reg]  Insert a register declaration, prompting for details"
  (interactive)
  (kill-all-local-variables)
  (use-local-map verilog-mode-map)
  (setq major-mode 'verilog-mode)
  (setq mode-name "Verilog")
  (setq local-abbrev-table verilog-mode-abbrev-table)
  (setq verilog-mode-syntax-table (make-syntax-table))
  (verilog-populate-syntax-table verilog-mode-syntax-table)
  ;; add extra comment syntax
  (verilog-setup-dual-comments verilog-mode-syntax-table)
  (set-syntax-table verilog-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'verilog-indent-line-relative)
  (setq comment-indent-function 'verilog-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-multi-line)
  (make-local-variable 'comment-start-skip)
  (setq comment-start "// "
	comment-end ""
	comment-start-skip "/\\*+ *\\|// *"
	comment-multi-line nil)
  ;; Set up for compilation
  (setq verilog-which-tool 1)
  (setq verilog-tool 'verilog-linter)
  (verilog-set-compile-command)
  (when (boundp 'hack-local-variables-hook)  ;; Also modify any file-local-variables
    (add-hook 'hack-local-variables-hook 'verilog-modify-compile-command t))

  ;; Setting up things for font-lock
  (if verilog-running-on-xemacs
      (progn
        (if (and current-menubar
                 (not (assoc "Verilog" current-menubar)))
            (progn
             ;; (set-buffer-menubar (copy-sequence current-menubar))
              (add-submenu nil verilog-xemacs-menu))) ))
  ;; Stuff for GNU emacs
  (make-local-variable 'font-lock-defaults)
  ;;------------------------------------------------------------
  ;; now hook in 'verilog-colorize-include-files (eldo-mode.el&spice-mode.el)
  ;; all buffer local:
  (make-local-hook 'font-lock-mode-hook)
  (make-local-hook 'font-lock-after-fontify-buffer-hook); doesn't exist in emacs 20
  (add-hook 'font-lock-mode-hook 'verilog-colorize-include-files-buffer t t)
  (add-hook 'font-lock-after-fontify-buffer-hook 'verilog-colorize-include-files-buffer t t) ; not in emacs 20
  (make-local-hook 'after-change-functions)
  (add-hook 'after-change-functions 'verilog-colorize-include-files t t)

  ;; Tell imenu how to handle verilog.
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression verilog-imenu-generic-expression)
  ;; Stuff for autos
  (add-hook 'write-contents-hooks 'verilog-auto-save-check) ; already local
  (verilog-auto-reeval-locals t)   ; Save locals in case user changes them
  (run-hooks 'verilog-mode-hook))


;;
;;  Electric functions
;;
(defun electric-verilog-terminate-line (&optional arg)
  "Terminate line and indent next line.
With optional ARG, remove existing end of line comments."
  (interactive)
  ;; before that see if we are in a comment
  (let ((state
	 (save-excursion
	   (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 7 state)			; Inside // comment
      (if (eolp)
	  (progn
	    (delete-horizontal-space)
	    (newline))
	(progn
	  (newline)
	  (insert-string "// ")
	  (beginning-of-line)))
      (verilog-indent-line))
     ((nth 4 state)			; Inside any comment (hence /**/)
      (newline)
      (verilog-more-comment))
     ((eolp)
       ;; First, check if current line should be indented
       (if (save-excursion
             (delete-horizontal-space)
	     (beginning-of-line)
	     (skip-chars-forward " \t")
	     (if (looking-at verilog-autoindent-lines-re)
		 (let ((indent-str (verilog-indent-line)))
		   ;; Maybe we should set some endcomments
		   (if verilog-auto-endcomments
		       (verilog-set-auto-endcomments indent-str arg))
		   (end-of-line)
		   (delete-horizontal-space)
		   (if arg
		       ()
		     (newline))
		   nil)
	       (progn
		 (end-of-line)
		 (delete-horizontal-space)
		 't
		 )))
	   (newline)
	 (forward-line 1)
	 )
       ;; Indent next line
       (if verilog-auto-indent-on-newline
	   (verilog-indent-line))
       )
     (t
      (newline))
     )))

(defun electric-verilog-semi ()
  "Insert `;' character and reindent the line."
  (interactive)
  (insert last-command-char)
  (if (or (verilog-in-comment-or-string-p)
	  (verilog-in-escaped-name-p))
      ()
    (save-excursion
      (beginning-of-line)
      (verilog-forward-ws&directives)
      (verilog-indent-line))
    (if (and verilog-auto-newline
	     (not (verilog-parenthesis-depth)))
	(electric-verilog-terminate-line))))

(defun electric-verilog-semi-with-comment ()
  "Insert `;' character, reindent the line and indent for comment."
  (interactive)
  (insert "\;")
  (save-excursion
    (beginning-of-line)
    (verilog-indent-line))
  (indent-for-comment))

(defun electric-verilog-colon ()
  "Insert `:' and do all indentations except line indent on this line."
  (interactive)
  (insert last-command-char)
  ;; Do nothing if within string.
  (if (or
       (verilog-within-string)
       (not (verilog-in-case-region-p)))
      ()
    (save-excursion
      (let ((p (point))
	    (lim (progn (verilog-beg-of-statement) (point))))
	(goto-char p)
	(verilog-backward-case-item lim)
	(verilog-indent-line)))
;;    (let ((verilog-tab-always-indent nil))
;;      (verilog-indent-line))
    ))

(defun electric-verilog-equal ()
  "Insert `=', and do indentation if within block."
  (interactive)
  (insert last-command-char)
;; Could auto line up expressions, but not yet
;;  (if (eq (car (verilog-calculate-indent)) 'block)
;;      (let ((verilog-tab-always-indent nil))
;;	(verilog-indent-command)))
  )

(defun electric-verilog-tick ()
  "Insert back-tick, and indent to column 0 if this is a CPP directive."
  (interactive)
  (insert last-command-char)
  (save-excursion
    (if (progn
	  (beginning-of-line)
	  (looking-at verilog-directive-re-1))
	(verilog-indent-line))))

(defun electric-verilog-tab ()
  "Function called when TAB is pressed in Verilog mode."
  (interactive)
  ;; If verilog-tab-always-indent, indent the beginning of the line.
  (if (or verilog-tab-always-indent
	  (save-excursion
	    (skip-chars-backward " \t")
	    (bolp)))
      (let* ((oldpnt (point))
	     (boi-point
	      (save-excursion
		(beginning-of-line)
		(skip-chars-forward " \t")
		(let (type state )
		  (setq type (verilog-indent-line))
		  (setq state (car type))
		  (cond
		   ((eq state 'block)
		    (if (looking-at verilog-behavioral-block-beg-re )
			(error
			 (concat
			  (verilog-point-text)
			  ": The reserved word \""
			  (buffer-substring (match-beginning 0) (match-end 0))
			  "\" must be at the behavioral level!"))))
		   ))
		(back-to-indentation)
		(point))))
        (if (< (point) boi-point)
            (back-to-indentation)
	  (cond ((not verilog-tab-to-comment))
		((not (eolp))
		 (end-of-line))
		(t
		 (indent-for-comment)
		 (when (and (eolp) (= oldpnt (point)))
					; kill existing comment
		   (beginning-of-line)
		   (re-search-forward comment-start-skip oldpnt 'move)
		   (goto-char (match-beginning 0))
		   (skip-chars-backward " \t")
		   (kill-region (point) oldpnt)
		   ))))
	)
    (progn (insert "\t"))))



;;
;; Interactive functions
;;

(defun verilog-indent-buffer ()
  "Indent-region the entire buffer as Verilog code."
  (interactive)
  (verilog-mode)
  (indent-region (point-min) (point-max) nil))

(defun verilog-insert-block ()
  "Insert Verilog begin ... end; block in the code with right indentation."
  (interactive)
  (verilog-indent-line)
  (insert "begin")
  (electric-verilog-terminate-line)
  (save-excursion
    (electric-verilog-terminate-line)
    (insert "end")
    (beginning-of-line)
    (verilog-indent-line)))

(defun verilog-star-comment ()
  "Insert Verilog star comment at point."
  (interactive)
  (verilog-indent-line)
  (insert "/*")
  (save-excursion
    (newline)
    (insert " */"))
  (newline)
  (insert " * "))

(defun verilog-insert-indices (MAX)
  "Insert a set of indices at into the rectangle.
The upper left corner is defined by the current point.  Indices always
begin with 0 and extend to the MAX - 1.  If no prefix arg is given, the
user is prompted for a value.  The indices are surrounded by square brackets
[].  For example, the following code with the point located after the first
'a' gives:

    a = b                           a[  0] = b
    a = b                           a[  1] = b
    a = b                           a[  2] = b
    a = b                           a[  3] = b
    a = b   ==> insert-indices ==>  a[  4] = b
    a = b                           a[  5] = b
    a = b                           a[  6] = b
    a = b                           a[  7] = b
    a = b                           a[  8] = b"

  (interactive "NMAX?")
  (save-excursion
  (let ((n 0))
    (while (< n MAX)
      (save-excursion
      (insert (format "[%3d]" n)))
      (next-line 1)
      (setq n (1+ n))))))


(defun verilog-generate-numbers (MAX)
  "Insert a set of generated numbers into a rectangle.
The upper left corner is defined by point.  The numbers are padded to three
digits, starting with 000 and extending to (MAX - 1).  If no prefix argument
is supplied, then the user is prompted for the MAX number.  consider the
following code fragment:

    buf buf                           buf buf000
    buf buf                           buf buf001
    buf buf                           buf buf002
    buf buf                           buf buf003
    buf buf   ==> insert-indices ==>  buf buf004
    buf buf                           buf buf005
    buf buf                           buf buf006
    buf buf                           buf buf007
    buf buf                           buf buf008"

  (interactive "NMAX?")
  (save-excursion
  (let ((n 0))
    (while (< n MAX)
      (save-excursion
      (insert (format "%3.3d" n)))
      (next-line 1)
      (setq n (1+ n))))))

(defun verilog-mark-defun ()
  "Mark the current verilog function (or procedure).
This puts the mark at the end, and point at the beginning."
  (interactive)
  (push-mark (point))
  (verilog-end-of-defun)
  (push-mark (point))
  (verilog-beg-of-defun)
  (zmacs-activate-region))

(defun verilog-comment-region (start end)
  ; checkdoc-params: (start end)
  "Put the region into a Verilog comment.
The comments that are in this area are \"deformed\":
`*)' becomes `!(*' and `}' becomes `!{'.
These deformed comments are returned to normal if you use
\\[verilog-uncomment-region] to undo the commenting.

The commented area starts with `verilog-exclude-str-start', and ends with
`verilog-include-str-end'.  But if you change these variables,
\\[verilog-uncomment-region] won't recognize the comments."
  (interactive "r")
  (save-excursion
    ;; Insert start and endcomments
    (goto-char end)
    (if (and (save-excursion (skip-chars-forward " \t") (eolp))
	     (not (save-excursion (skip-chars-backward " \t") (bolp))))
	(forward-line 1)
      (beginning-of-line))
    (insert verilog-exclude-str-end)
    (setq end (point))
    (newline)
    (goto-char start)
    (beginning-of-line)
    (insert verilog-exclude-str-start)
    (newline)
    ;; Replace end-comments within commented area
    (goto-char end)
    (save-excursion
      (while (re-search-backward "\\*/" start t)
	(replace-match "*-/" t t)))
    (save-excursion
      (let ((s+1 (1+ start)))
	(while (re-search-backward "/\\*" s+1 t)
	  (replace-match "/-*" t t))))
    ))

(defun verilog-uncomment-region ()
  "Uncomment a commented area; change deformed comments back to normal.
This command does nothing if the pointer is not in a commented
area.  See also `verilog-comment-region'."
  (interactive)
  (save-excursion
    (let ((start (point))
	  (end (point)))
      ;; Find the boundaries of the comment
      (save-excursion
	(setq start (progn (search-backward verilog-exclude-str-start nil t)
			   (point)))
	(setq end (progn (search-forward verilog-exclude-str-end nil t)
			 (point))))
      ;; Check if we're really inside a comment
      (if (or (equal start (point)) (<= end (point)))
	  (message "Not standing within commented area.")
	(progn
	  ;; Remove endcomment
	  (goto-char end)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point))))
	  ;; Change comments back to normal
	  (save-excursion
	    (while (re-search-backward "\\*-/" start t)
	      (replace-match "*/" t t)))
	  (save-excursion
	    (while (re-search-backward "/-\\*" start t)
	      (replace-match "/*" t t)))
	  ;; Remove start comment
	  (goto-char start)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point)))))))))

(defun verilog-beg-of-defun ()
  "Move backward to the beginning of the current function or procedure."
  (interactive)
  (verilog-re-search-backward verilog-defun-re nil 'move))

(defun verilog-end-of-defun ()
  "Move forward to the end of the current function or procedure."
  (interactive)
  (verilog-re-search-forward verilog-end-defun-re nil 'move))

(defun verilog-get-beg-of-defun (&optional warn)
  (save-excursion
    (cond ((verilog-re-search-forward-quick verilog-defun-re nil t)
	   (point))
	  (t
	   (error "%s: Can't find module beginning" (verilog-point-text))
	   (point-max)))))
(defun verilog-get-end-of-defun (&optional warn)
  (save-excursion
    (cond ((verilog-re-search-forward-quick verilog-end-defun-re nil t)
	   (point))
	  (t
	   (error "%s: Can't find endmodule" (verilog-point-text))
	   (point-max)))))

(defun verilog-label-be (&optional arg)
  "Label matching begin ... end, fork ... join and case ... endcase statements.
With ARG, first kill any existing labels."
  (interactive)
  (let ((cnt 0)
	(oldpos (point))
	(b (progn
	     (verilog-beg-of-defun)
	     (point-marker)))
	(e (progn
	     (verilog-end-of-defun)
	     (point-marker)))
	)
    (goto-char (marker-position b))
    (if (> (- e b) 200)
	(message  "Relabeling module..."))
    (while (and
	    (> (marker-position e) (point))
	    (verilog-re-search-forward
	     (concat
	      "\\<end\\(\\(function\\)\\|\\(task\\)\\|\\(module\\)\\|\\(primitive\\)\\|\\(interface\\)\\|\\(package\\)\\|\\(case\\)\\)?\\>"
	      "\\|\\(`endif\\)\\|\\(`else\\)")
	     nil 'move))
      (goto-char (match-beginning 0))
      (let ((indent-str (verilog-indent-line)))
	(verilog-set-auto-endcomments indent-str 't)
	(end-of-line)
	(delete-horizontal-space)
	)
      (setq cnt (1+ cnt))
      (if (= 9 (% cnt 10))
	  (message "%d..." cnt))
      )
    (goto-char oldpos)
    (if (or
	 (> (- e b) 200)
	 (> cnt 20))
	(message  "%d lines auto commented" cnt))
    ))

(defun verilog-beg-of-statement ()
  "Move backward to beginning of statement."
  (interactive)
  (while (save-excursion
	   (and
	    (not (looking-at verilog-complete-reg))
	    (verilog-backward-syntactic-ws)
	    (not (or (bolp) (= (preceding-char) ?\;)))
	    ))
    (skip-chars-backward " \t")
    (verilog-backward-token))
  (let ((last (point)))
    (while (progn
	     (setq last (point))
	     (and (not (looking-at verilog-complete-reg))
		  (verilog-continued-line))))
    (goto-char last)
    (verilog-forward-syntactic-ws)))

(defun verilog-beg-of-statement-1 ()
  "Move backward to beginning of statement."
  (interactive)
  (let ((pt (point)))

    (while (and (not (looking-at verilog-complete-reg))
		(setq pt (point))
		(verilog-backward-token)
		(verilog-backward-syntactic-ws)
		(setq pt (point))
		(not (bolp))
		(not (= (preceding-char) ?\;))))
    (while (progn
	     (setq pt (point))
	     (and (not (looking-at verilog-complete-reg))
		  (not (= (preceding-char) ?\;))
		  (verilog-continued-line))))
    (goto-char pt)
    (verilog-forward-ws&directives)))

(defun verilog-end-of-statement ()
  "Move forward to end of current statement."
  (interactive)
  (let ((nest 0) pos)
    (or (looking-at verilog-beg-block-re)
	;; Skip to end of statement
	(setq pos (catch 'found
		    (while t
		      (forward-sexp 1)
		      (verilog-skip-forward-comment-or-string)
		      (cond ((looking-at "[ \t]*;")
			     (skip-chars-forward "^;")
			     (forward-char 1)
			     (throw 'found (point)))
			    ((save-excursion
			       (forward-sexp -1)
			       (looking-at verilog-beg-block-re))
			     (goto-char (match-beginning 0))
			     (throw 'found nil))
			    ((eobp)
			     (throw 'found (point))))))))
    (if (not pos)
	;; Skip a whole block
	(catch 'found
	  (while t
	    (verilog-re-search-forward verilog-end-statement-re nil 'move)
	    (setq nest (if (match-end 1)
			   (1+ nest)
			 (1- nest)))
	    (cond ((eobp)
		   (throw 'found (point)))
		  ((= 0 nest)
		   (throw 'found (verilog-end-of-statement))))))
      pos)))

(defun verilog-in-case-region-p ()
  "Return TRUE if in a case region;
more specifically, point @ in the line foo : @ begin"
  (interactive)
  (save-excursion
    (if (and
	 (progn (verilog-forward-syntactic-ws)
		(looking-at "\\<begin\\>"))
	 (progn (verilog-backward-syntactic-ws)
		(= (preceding-char) ?\:)))
	(catch 'found
	  (let ((nest 1))
	    (while t
	      (verilog-re-search-backward
	       (concat "\\(\\<module\\>\\)\\|\\(\\<case[xz]?\\>[^:]\\)\\|"
		       "\\(\\<endcase\\>\\)\\>")
	       nil 'move)
	      (cond
	       ((match-end 3)
		(setq nest (1+ nest)))
	       ((match-end 2)
		(if (= nest 1)
		(throw 'found 1))
		(setq nest (1- nest)))
	       (t
		(throw 'found (= nest 0)))
	       ))))
      nil)))

(defun verilog-in-fork-region-p ()
  "Return true if between a fork and join."
  (interactive)
  (let ((lim (save-excursion (verilog-beg-of-defun)  (point)))
	(nest 1)
	)
    (save-excursion
      (while (and
	      (/= nest 0)
	      (verilog-re-search-backward "\\<\\(fork\\)\\|\\(join\\(_any\\|_none\\)?\\)\\>" lim 'move)
	      (cond
	       ((match-end 1) ; fork
		(setq nest (1- nest)))
	       ((match-end 2) ; join
		(setq nest (1+ nest)))
	       ))
	))
    (= nest 0) )) ; return nest

(defun verilog-backward-case-item (lim)
  "Skip backward to nearest enclosing case item.
Limit search to point LIM."
  (interactive)
  (let ((str 'nil)
	(lim1
	 (progn
	   (save-excursion
	     (verilog-re-search-backward verilog-endcomment-reason-re
					 lim 'move)
	     (point)))))
    ;; Try to find the real :
    (if (save-excursion (search-backward ":" lim1 t))
	(let ((colon 0)
	      b e )
	  (while
	      (and
	       (< colon 1)
	       (verilog-re-search-backward "\\(\\[\\)\\|\\(\\]\\)\\|\\(:\\)"
					   lim1 'move))
	    (cond
	     ((match-end 1) ;; [
	      (setq colon (1+ colon))
	      (if (>= colon 0)
		  (error "%s: unbalanced [" (verilog-point-text))))
	     ((match-end 2) ;; ]
	      (setq colon (1- colon)))

	     ((match-end 3) ;; :
	      (setq colon (1+ colon)))
	     ))
	  ;; Skip back to beginning of case item
	  (skip-chars-backward "\t ")
	  (verilog-skip-backward-comment-or-string)
	  (setq e (point))
	  (setq b
		(progn
		  (if
		      (verilog-re-search-backward
		       "\\<\\(case[zx]?\\)\\>\\|;\\|\\<end\\>" nil 'move)
		      (progn
			(cond
			 ((match-end 1)
			  (goto-char (match-end 1))
			  (verilog-forward-ws&directives)
			  (if (looking-at "(")
			      (progn
				(forward-sexp)
				(verilog-forward-ws&directives)))
			  (point))
			 (t
			  (goto-char (match-end 0))
			  (verilog-forward-ws&directives)
			  (point))
			 ))
		    (error "Malformed case item")
		    )))
	  (setq str (buffer-substring b e))
	  (if
	      (setq e
		    (string-match
		     "[ \t]*\\(\\(\n\\)\\|\\(//\\)\\|\\(/\\*\\)\\)" str))
	      (setq str (concat (substring str 0 e) "...")))
	  str)
      'nil)))


;;
;; Other functions
;;

(defun kill-existing-comment ()
  "Kill auto comment on this line."
  (save-excursion
    (let* (
	   (e (progn
		(end-of-line)
		(point)))
	   (b (progn
		(beginning-of-line)
		(search-forward "//" e t))))
      (if b
	  (delete-region (- b 2) e)))))

(defconst verilog-directive-nest-re
  (concat "\\(`else\\>\\)\\|"
	  "\\(`endif\\>\\)\\|"
	  "\\(`if\\>\\)\\|"
	  "\\(`ifdef\\>\\)\\|"
	  "\\(`ifndef\\>\\)"))
(defun verilog-set-auto-endcomments (indent-str kill-existing-comment)
  "Add ending comment with given INDENT-STR.
With KILL-EXISTING-COMMENT, remove what was there before.
Insert `// case: 7 ' or `// NAME ' on this line if appropriate.
Insert `// case expr ' if this line ends a case block.
Insert `// ifdef FOO ' if this line ends code conditional on FOO.
Insert `// NAME ' if this line ends a function, task, module, primitive or interface named NAME."
  (save-excursion
    (cond
     (; Comment close preprocessor directives
      (and
       (looking-at "\\(`endif\\)\\|\\(`else\\)")
       (or  kill-existing-comment
	    (not (save-excursion
		   (end-of-line)
		   (search-backward "//" (verilog-get-beg-of-line) t)))))
      (let ((nest 1) b e
	    m
	    (else (if (match-end 2) "!" " "))
	    )
	(end-of-line)
	(if kill-existing-comment
	    (kill-existing-comment))
	(delete-horizontal-space)
	(save-excursion
	  (backward-sexp 1)
	  (while (and (/= nest 0)
		      (verilog-re-search-backward verilog-directive-nest-re nil 'move))
	    (cond
	     ((match-end 1) ; `else
	      (if (= nest 1)
		  (setq else "!")))
	     ((match-end 2) ; `endif
	      (setq nest (1+ nest)))
	     ((match-end 3) ; `if
	      (setq nest (1- nest)))
	     ((match-end 4) ; `ifdef
	      (setq nest (1- nest)))
	     ((match-end 5) ; `ifndef
	      (setq nest (1- nest)))
	     ))
	  (if (match-end 0)
	      (setq
	       m (buffer-substring
		  (match-beginning 0)
		  (match-end 0))
	       b (progn
		   (skip-chars-forward "^ \t")
		   (verilog-forward-syntactic-ws)
		   (point))
	       e (progn
		   (skip-chars-forward "a-zA-Z0-9_")
		   (point)
		   ))))
	(if b
	    (if (> (count-lines (point) b) verilog-minimum-comment-distance)
		(insert (concat " // " else m " " (buffer-substring b e))))
	  (progn
	    (insert " // unmatched `else or `endif")
	    (ding 't))
	  )))

     (; Comment close case/function/task/module and named block
      (and (looking-at "\\<end")
	   (or kill-existing-comment
	       (not (save-excursion
		      (end-of-line)
		      (search-backward "//" (verilog-get-beg-of-line) t)))))
      (let ((type (car indent-str)))
	(unless (eq type 'declaration)
	  (unless (looking-at (concat "\\(" verilog-enders-re "\\)[ \t]*:")) ;; ignore named ends
	    (if (looking-at verilog-enders-re)
	      (cond
	       (;- This is a case block; search back for the start of this case
		(match-end 1) ;; of verilog-enders-re

		(let ((err 't)
		      (str "UNMATCHED!!"))
		  (save-excursion
		    (verilog-leap-to-head)
		    (if (match-end 0)
			(progn
			  (goto-char (match-end 1))
			  (setq str (concat (buffer-substring (match-beginning 1) (match-end 1))
					    (verilog-get-expr)))
			  (setq err nil))))
		  (end-of-line)
		  (if kill-existing-comment
		      (kill-existing-comment))
		  (delete-horizontal-space)
		  (insert (concat " // " str ))
		  (if err (ding 't))
		  ))

	       (;- This is a begin..end block
		(match-end 2) ;; of verilog-enders-re
		(let ((str " // UNMATCHED !!")
		      (err 't)
		      (here (point))
		      there
		      cntx
		      )
		  (save-excursion
		    (verilog-leap-to-head)
		    (setq there (point))
		    (if (not (match-end 0))
			(progn
			  (goto-char here)
			  (end-of-line)
			  (if kill-existing-comment
			      (kill-existing-comment))
			  (delete-horizontal-space)
			  (insert str)
			  (ding 't)
			  )
		      (let ((lim
			     (save-excursion (verilog-beg-of-defun) (point)))
			    (here (point))
			    )
			(cond
			 (;-- handle named block differently
			  (looking-at verilog-named-block-re)
			  (search-forward ":")
			  (setq there (point))
			  (setq str (verilog-get-expr))
			  (setq err nil)
			  (setq str (concat " // block: " str )))

			 ((verilog-in-case-region-p) ;-- handle case item differently
			  (goto-char here)
			  (setq str (verilog-backward-case-item lim))
			  (setq there (point))
			  (setq err nil)
			  (setq str (concat " // case: " str )))

			 (;- try to find "reason" for this begin
			  (cond
			   (;
			    (eq here (progn
				       (verilog-backward-token)
				       (verilog-beg-of-statement-1)
				       (point)))
			    (setq err nil)
			    (setq str ""))
			   ((looking-at verilog-endcomment-reason-re)
			    (setq there (match-end 0))
			    (setq cntx (concat
					(buffer-substring (match-beginning 0) (match-end 0)) " "))
			    (cond
			     (;- begin
			      (match-end 2)
			      (setq err nil)
			      (save-excursion
				(if (and (verilog-continued-line)
					 (looking-at "\\<repeat\\>\\|\\<wait\\>\\|\\<always\\>"))
				    (progn
				      (goto-char (match-end 0))
				      (setq there (point))
				      (setq str
					    (concat " // "
						    (buffer-substring (match-beginning 0) (match-end 0)) " "
						    (verilog-get-expr))))
				  (setq str ""))))

			     (;- else
			      (match-end 4)
			      (let ((nest 0)
				    ( reg "\\(\\<begin\\>\\)\\|\\(\\<end\\>\\)\\|\\(\\<if\\>\\)")
				    )
				(catch 'skip
				  (while (verilog-re-search-backward reg nil 'move)
				    (cond
				     ((match-end 1) ; begin
				      (setq nest (1- nest)))
				     ((match-end 2)                       ; end
				      (setq nest (1+ nest)))
				     ((match-end 3)
				      (if (= 0 nest)
					  (progn
					    (goto-char (match-end 0))
					    (setq there (point))
					    (setq err nil)
					    (setq str (verilog-get-expr))
					    (setq str (concat " // else: !if" str ))
					    (throw 'skip 1))
					)))
				    ))))

			     (;- end else
			      (match-end 5)
			      (goto-char there)
			      (let ((nest 0)
				    ( reg "\\(\\<begin\\>\\)\\|\\(\\<end\\>\\)\\|\\(\\<if\\>\\)")
				    )
				(catch 'skip
				  (while (verilog-re-search-backward reg nil 'move)
				    (cond
				     ((match-end 1) ; begin
				      (setq nest (1- nest)))
				     ((match-end 2)                       ; end
				      (setq nest (1+ nest)))
				     ((match-end 3)
				      (if (= 0 nest)
					  (progn
					    (goto-char (match-end 0))
					    (setq there (point))
					    (setq err nil)
					    (setq str (verilog-get-expr))
					    (setq str (concat " // else: !if" str ))
					    (throw 'skip 1))
					)))
				    ))))

			     (;- task/function/initial et cetera
			      t
			      (match-end 0)
			      (goto-char (match-end 0))
			      (setq there (point))
			      (setq err nil)
			      (setq str (verilog-get-expr))
			      (setq str (concat " // " cntx str )))

			     (;-- otherwise...
			      (setq str " // auto-endcomment confused "))
			     ))

			   ((and
			     (verilog-in-case-region-p) ;-- handle case item differently
			     (progn
			       (setq there (point))
			       (goto-char here)
			       (setq str (verilog-backward-case-item lim))))
			    (setq err nil)
			    (setq str (concat " // case: " str )))

			   ((verilog-in-fork-region-p)
			    (setq err nil)
			    (setq str " // fork branch" ))

			   ((looking-at "\\<end\\>")
			    ;; HERE
			    (forward-word 1)
			    (verilog-forward-syntactic-ws)
			    (setq err nil)
			    (setq str (verilog-get-expr))
			    (setq str (concat " // " cntx str )))

			   ))))
		      (goto-char here)
		      (end-of-line)
		      (if kill-existing-comment
			  (kill-existing-comment))
		      (delete-horizontal-space)
		      (if (or err
			      (> (count-lines here there) verilog-minimum-comment-distance))
			  (insert str))
		      (if err (ding 't))
		      ))))

	       (;- this is end{function,generate,task,module,primative,table,generate}
		t
		(let (string reg (width nil))
		  (end-of-line)
		  (if kill-existing-comment
		      (save-match-data
		       (kill-existing-comment)))
		  (delete-horizontal-space)
		  (backward-sexp)
		  (cond
		   ((match-end 5) ;; of verilog-enders-re
		    (setq reg "\\(\\<function\\>\\)\\|\\(\\<\\(endfunction\\|task\\|\\(macro\\)?module\\|primitive\\)\\>\\)")
		    (setq width "\\(\\s-*\\(\\[[^]]*\\]\\)\\|\\(real\\(time\\)?\\)\\|\\(integer\\)\\|\\(time\\)\\)?")
		    )
		   ((match-end 6) ;; of verilog-enders-re
		    (setq reg "\\(\\<task\\>\\)\\|\\(\\<\\(endtask\\|function\\|\\(macro\\)?module\\|primitive\\)\\>\\)"))
		   ((match-end 7) ;; of verilog-enders-re
		    (setq reg "\\(\\<\\(macro\\)?module\\>\\)\\|\\<endmodule\\>"))
		   ((match-end 8) ;; of verilog-enders-re
		    (setq reg "\\(\\<primitive\\>\\)\\|\\(\\<\\(endprimitive\\|package\\|interface\\|\\(macro\\)?module\\)\\>\\)"))
		   ((match-end 9) ;; of verilog-enders-re
		    (setq reg "\\(\\<interface\\>\\)\\|\\(\\<\\(endinterface\\|package\\|primitive\\|\\(macro\\)?module\\)\\>\\)"))
		   ((match-end 10) ;; of verilog-enders-re
		    (setq reg "\\(\\<package\\>\\)\\|\\(\\<\\(endpackage\\|primitive\\|interface\\|\\(macro\\)?module\\)\\>\\)"))
		   )
		  (let (b e)
		    (save-excursion
		      (verilog-re-search-backward reg nil 'move)
		      (cond
		       ((match-end 1)
			(setq b (progn
				  (skip-chars-forward "^ \t")
				  (verilog-forward-ws&directives)
				  (if (and width (looking-at width))
				      (progn
					(goto-char (match-end 0))
					(verilog-forward-ws&directives)
					))
				  (point))
			      e (progn
				  (skip-chars-forward "a-zA-Z0-9_")
				  (point)))
			(setq string (buffer-substring b e)))
		       (t
			(ding 't)
			(setq string "unmatched end(function|task|module|primitive|interface|package)")))))
		  (end-of-line)
		  (insert (concat " // " string )))
		))))))))))

(defun verilog-get-expr()
  "Grab expression at point, e.g, case ( a | b & (c ^d))"
  (let* ((b (progn
	      (verilog-forward-syntactic-ws)
	      (skip-chars-forward " \t")
	      (point)))
	 (e (let ((par 1))
	      (cond
	       ((looking-at "@")
		(forward-char 1)
		(verilog-forward-syntactic-ws)
		(if (looking-at "(")
		    (progn
		      (forward-char 1)
		      (while (and (/= par 0)
				  (verilog-re-search-forward "\\((\\)\\|\\()\\)" nil 'move))
			(cond
			 ((match-end 1)
			  (setq par (1+ par)))
			 ((match-end 2)
			  (setq par (1- par)))))))
		(point))
	       ((looking-at "(")
		(forward-char 1)
		(while (and (/= par 0)
			    (verilog-re-search-forward "\\((\\)\\|\\()\\)" nil 'move))
		  (cond
		   ((match-end 1)
		    (setq par (1+ par)))
		   ((match-end 2)
		    (setq par (1- par)))))
		(point))
	       ((looking-at "\\[")
		(forward-char 1)
		(while (and (/= par 0)
			    (verilog-re-search-forward "\\(\\[\\)\\|\\(\\]\\)" nil 'move))
		  (cond
		   ((match-end 1)
		    (setq par (1+ par)))
		   ((match-end 2)
		    (setq par (1- par)))))
		(verilog-forward-syntactic-ws)
		(skip-chars-forward "^ \t\n")
		(point))
	       ((looking-at "/[/\\*]")
		b)
	       ('t
		(skip-chars-forward "^: \t\n")
		(point)
		))))
	 (str (buffer-substring b e)))
    (if (setq e (string-match "[ \t]*\\(\\(\n\\)\\|\\(//\\)\\|\\(/\\*\\)\\)" str))
	(setq str (concat (substring str 0 e) "...")))
    str))

(defun verilog-expand-vector ()
  "Take a signal vector on the current line and expand it to multiple lines.
Useful for creating tri's and other expanded fields."
  (interactive)
  (verilog-expand-vector-internal "[" "]"))

(defun verilog-expand-vector-internal (bra ket)
  "Given BRA, the start brace and KET, the end brace, expand one line into many lines."
  (save-excursion
    (forward-line 0)
    (let ((signal-string (buffer-substring (point)
					   (progn
					     (end-of-line) (point)))))
      (if (string-match (concat "\\(.*\\)"
				(regexp-quote bra)
				"\\([0-9]*\\)\\(:[0-9]*\\|\\)\\(::[0-9---]*\\|\\)"
				(regexp-quote ket)
				"\\(.*\\)$") signal-string)
	  (let* ((sig-head (match-string 1 signal-string))
		 (vec-start (string-to-int (match-string 2 signal-string)))
		 (vec-end (if (= (match-beginning 3) (match-end 3))
			      vec-start
			    (string-to-int (substring signal-string (1+ (match-beginning 3)) (match-end 3)))))
		 (vec-range (if (= (match-beginning 4) (match-end 4))
				1
			      (string-to-int (substring signal-string (+ 2 (match-beginning 4)) (match-end 4)))))
		 (sig-tail (match-string 5 signal-string))
		 vec)
	    ;; Decode vectors
	    (setq vec nil)
	    (if (< vec-range 0)
		(let ((tmp vec-start))
		  (setq vec-start vec-end
			vec-end tmp
			vec-range (- vec-range))))
	    (if (< vec-end vec-start)
		(while (<= vec-end vec-start)
		  (setq vec (append vec (list vec-start)))
		  (setq vec-start (- vec-start vec-range)))
	      (while (<= vec-start vec-end)
		(setq vec (append vec (list vec-start)))
		(setq vec-start (+ vec-start vec-range))))
	    ;;
	    ;; Delete current line
	    (delete-region (point) (progn (forward-line 0) (point)))
	    ;;
	    ;; Expand vector
	    (while vec
	      (insert (concat sig-head bra (int-to-string (car vec)) ket sig-tail "\n"))
	      (setq vec (cdr vec)))
	    (delete-char -1)
	    ;;
	    )))))

(defun verilog-strip-comments ()
  "Strip all comments from the verilog code."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "//" nil t)
    (let ((bpt (- (point) 2)))
      (end-of-line)
      (delete-region bpt (point))))
  ;;
  (goto-char (point-min))
  (while (re-search-forward "/\\*" nil t)
    (let ((bpt (- (point) 2)))
      (re-search-forward "\\*/")
      (delete-region bpt (point)))))

(defun verilog-one-line ()
  "Converts structural verilog instances to occupy one line."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\([^;]\\)[ \t]*\n[ \t]*" nil t)
	(replace-match "\\1 " nil nil)))

(defun verilog-linter-name ()
  "Return name of linter, either surelint or verilint."
  (let ((compile-word1 (verilog-string-replace-matches "\\s .*$" "" nil nil
						       compile-command))
	(lint-word1    (verilog-string-replace-matches "\\s .*$" "" nil nil
						       verilog-linter)))
    (cond ((equal compile-word1 "surelint") `surelint)
	  ((equal compile-word1 "verilint") `verilint)
	  ((equal lint-word1 "surelint")    `surelint)
	  ((equal lint-word1 "verilint")    `verilint)
	  (t `surelint))))  ;; back compatibility

(defun verilog-lint-off ()
  "Convert a Verilog linter warning line into a disable statement.
For example:
	pci_bfm_null.v, line  46: Unused input: pci_rst_
becomes a comment for the appropriate tool.

The first word of the `compile-command' or `verilog-linter'
variables are used to determine which product is being used.

See \\[verilog-surelint-off] and \\[verilog-verilint-off]."
  (interactive)
  (let ((linter (verilog-linter-name)))
    (cond ((equal linter `surelint)
	   (verilog-surelint-off))
	  ((equal linter `verilint)
	   (verilog-verilint-off))
	  (t (error "Linter name not set")))))

(defun verilog-surelint-off ()
  "Convert a SureLint warning line into a disable statement.
Run from Verilog source window; assumes there is a *compile* buffer
with point set appropriately.
For example:
 WARNING [STD-UDDONX]: xx.v, line 8: output out is never assigned.
becomes:
	// surefire lint_line_off UDDONX"
  (interactive)
  (save-excursion
    (switch-to-buffer compilation-last-buffer)
    (beginning-of-line)
    (when
	(looking-at "\\(INFO\\|WARNING\\|ERROR\\) \\[[^-]+-\\([^]]+\\)\\]: \\([^,]+\\), line \\([0-9]+\\): \\(.*\\)$")
      (let* ((code (match-string 2))
	     (file (match-string 3))
	     (line (match-string 4))
	     (buffer (get-file-buffer file))
	     dir filename)
	(unless buffer
	  (progn
	    (setq buffer
		  (and (file-exists-p file)
		       (find-file-noselect file)))
	    (or buffer
		(let* ((pop-up-windows t))
		  (let ((name (expand-file-name
			       (read-file-name
				(format "Find this error in: (default %s) "
					file)
				dir file t))))
		    (if (file-directory-p name)
			(setq name (expand-file-name filename name)))
		    (setq buffer
			  (and (file-exists-p name)
			       (find-file-noselect name))))))))
	(switch-to-buffer buffer)
	(goto-line (string-to-number line))
	(end-of-line)
	(catch 'already
	  (cond
	   ((verilog-in-slash-comment-p)
	    (re-search-backward "//")
	    (cond
	     ((looking-at "// surefire lint_off_line ")
	      (goto-char (match-end 0))
	      (let ((lim (save-excursion (end-of-line) (point))))
		(if (re-search-forward code lim 'move)
		    (throw 'already t)
		  (insert-string (concat " " code)))))
	     (t
	      )))
	   ((verilog-in-star-comment-p)
	    (re-search-backward "/\*")
	    (insert-string (format " // surefire lint_off_line %6s" code ))
	    )
	   (t
	    (insert-string (format " // surefire lint_off_line %6s" code ))
	    )))))))

(defun verilog-verilint-off ()
  "Convert a Verilint warning line into a disable statement.
For example:
	(W240)  pci_bfm_null.v, line  46: Unused input: pci_rst_
becomes:
	//Verilint 240 off // WARNING: Unused input"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\(.*\\)([WE]\\([0-9A-Z]+\\)).*,\\s +line\\s +[0-9]+:\\s +\\([^:\n]+\\):?.*$")
      (replace-match (format
		      ;; %3s makes numbers 1-999 line up nicely
		      "\\1//Verilint %3s off // WARNING: \\3"
		      (match-string 2)))
      (beginning-of-line)
      (verilog-indent-line))))

(defun verilog-auto-save-compile ()
  "Update automatics with \\[verilog-auto], save the buffer, and compile."
  (interactive)
  (verilog-auto)	; Always do it for safety
  (save-buffer)
  (compile compile-command))


;;
;; Indentation
;;
(defconst verilog-indent-alist
  '((block       . (+ ind verilog-indent-level))
    (case        . (+ ind verilog-case-indent))
    (cparenexp   . (+ ind verilog-indent-level))
    (cexp        . (+ ind verilog-cexp-indent))
    (defun       . verilog-indent-level-module)
    (declaration . verilog-indent-level-declaration)
    (directive   . (verilog-calculate-indent-directive))
    (tf          . verilog-indent-level)
    (behavioral  . (+ verilog-indent-level-behavioral verilog-indent-level-module))
    (statement   . ind)
    (cpp         . 0)
    (comment     . (verilog-comment-indent))
    (unknown     . 3)
    (string      . 0)))

(defun verilog-continued-line-1 (lim)
  "Return true if this is a continued line.
Set point to where line starts.  Limit search to point LIM."
  (let ((continued 't))
    (if (eq 0 (forward-line -1))
	(progn
	  (end-of-line)
	  (verilog-backward-ws&directives lim)
	  (if (bobp)
	      (setq continued nil)
	    (setq continued (verilog-backward-token))))
      (setq continued nil))
    continued))

(defun verilog-calculate-indent ()
  "Calculate the indent of the current Verilog line.
Examine previous lines.  Once a line is found that is definitive as to the
type of the current line, return that lines' indent level and its
type.  Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((starting_position (point))
	   (par 0)
	   (begin (looking-at "[ \t]*begin\\>"))
	   (lim (save-excursion (verilog-re-search-backward "\\(\\<begin\\>\\)\\|\\(\\<module\\>\\)" nil t)))
	   (type (catch 'nesting
		   ;; Keep working backwards until we can figure out
		   ;; what type of statement this is.
		   ;; Basically we need to figure out
		   ;; 1) if this is a continuation of the previous line;
		   ;; 2) are we in a block scope (begin..end)

		   ;; if we are in a comment, done.
		   (if (verilog-in-star-comment-p)   (throw 'nesting 'comment))

		   ;; if we are in a parenthesized list, and the user likes to indent
		   ;; these, return.
 		   (if (and verilog-indent-lists
			    (verilog-in-paren))
		       (progn (setq par 1) (throw 'nesting 'block)))
;		   (if (/= 0 (verilog-parenthesis-depth)) (progn (setq par 1) (throw 'nesting 'block)))

		   ;; if we have a directive, done.
		   (if (save-excursion
			 (beginning-of-line)
			 (looking-at verilog-directive-re-1))
		       (throw 'nesting 'directive))

		   ;; See if we are continuing a previous line
		   (while t
		     ;; trap out if we crawl off the top of the buffer
		     (if (bobp) (throw 'nesting 'cpp))

		     (if (verilog-continued-line-1 lim)
			 (let ((sp (point)))
			   (if (and
				(not (looking-at verilog-complete-reg))
				(verilog-continued-line-1 lim))
			       (progn (goto-char sp)
				      (throw 'nesting 'cexp))
			     (goto-char sp))

			   (if (and begin
				    (not verilog-indent-begin-after-if)
				    (looking-at verilog-no-indent-begin-re))
			       (progn
				 (beginning-of-line)
				 (skip-chars-forward " \t")
				 (throw 'nesting 'statement))
			     (progn
			       (throw 'nesting 'cexp))))
		       ;; not a continued line
		       (goto-char starting_position))

		     (if (looking-at "\\<else\\>")
			 ;; search back for governing if, striding across begin..end pairs
			 ;; appropriately
			 (let ((elsec 1))
			   (while (verilog-re-search-backward verilog-ends-re nil 'move)
			     (cond
			      ((match-end 1) ; else, we're in deep
			       (setq elsec (1+ elsec)))
			      ((match-end 2) ; if
			       (setq elsec (1- elsec))
			       (if (= 0 elsec)
				   (if verilog-align-ifelse
				       (throw 'nesting 'statement)
				     (progn ;; back up to first word on this line
				       (beginning-of-line)
				       (verilog-forward-syntactic-ws)
				       (throw 'nesting 'statement)))))
			      (t ; endblock
				; try to leap back to matching outward block by striding across
				; indent level changing tokens then immediately
				; previous line governs indentation.
			       (let (( reg) (nest 1))
;;	 verilog-ends =>  else|if|end|join(_any|_none|)|endcase|endtable|endspecify|endfunction|endtask|endgenerate
				 (cond
				  ((match-end 3) ; end
				   ;; Search back for matching begin
				   (setq reg "\\(\\<begin\\>\\)\\|\\(\\<end\\>\\)" ))
				  ((match-end 4) ; endcase
				   ;; Search back for matching case
				   (setq reg "\\(\\<case[xz]?\\>[^:]\\)\\|\\(\\<endcase\\>\\)" ))
				  ((match-end 5) ; endfunction
				   ;; Search back for matching function
				   (setq reg "\\(\\<function\\>\\)\\|\\(\\<endfunction\\>\\)" ))
				  ((match-end 6) ; endtask
				   ;; Search back for matching task
				   (setq reg "\\(\\<task\\>\\)\\|\\(\\<endtask\\>\\)" ))
				  ((match-end 7) ; endspecify
				   ;; Search back for matching specify
				   (setq reg "\\(\\<specify\\>\\)\\|\\(\\<endspecify\\>\\)" ))
				  ((match-end 8) ; endtable
				   ;; Search back for matching table
				   (setq reg "\\(\\<table\\>\\)\\|\\(\\<endtable\\>\\)" ))
				  ((match-end 9) ; endgenerate
				   ;; Search back for matching generate
				   (setq reg "\\(\\<generate\\>\\)\\|\\(\\<endgenerate\\>\\)" ))
				  ((match-end 10) ; join
				   ;; Search back for matching fork
				   (setq reg "\\(\\<fork\\>\\)\\|\\(\\<join\\(_any\\|none\\)?\\>\\)" ))
				  )
				 (catch 'skip
				   (while (verilog-re-search-backward reg nil 'move)
				     (cond
				      ((match-end 1) ; begin
				       (setq nest (1- nest))
				       (if (= 0 nest)
					   (throw 'skip 1)))
				      ((match-end 2) ; end
				       (setq nest (1+ nest)))))
				   )
				 ))
			      ))))
		     (throw 'nesting (verilog-calc-1))
		     ))))
      ;; Return type of block and indent level.
      (if (not type)
	  (setq type 'cpp))
      (if (> par 0)			; Unclosed Parenthesis
	  (list 'cparenexp par)
	(cond
	  ((eq type 'case)
	   (list type (verilog-case-indent-level)))
	  ((eq type 'statement)
	   (list type (current-column)))
	  ((eq type 'defun)
	   (list type 0))
	  (t
	   (list type (verilog-indent-level)))))
      )))

(defun verilog-calc-1 ()
  (catch 'nesting
    (while (verilog-re-search-backward verilog-indent-re nil 'move)
      (cond
       ((looking-at verilog-behavioral-level-re)
	(throw 'nesting 'behavioral))

       ((looking-at verilog-beg-block-re-1)
	(cond
	 ((match-end 2)  (throw 'nesting 'case))
	 (t              (throw 'nesting 'block))))

       ((looking-at verilog-end-block-re)
	(verilog-leap-to-head)
	(if (verilog-in-case-region-p)
	    (progn
	      (verilog-leap-to-case-head)
	      (if (looking-at verilog-case-re)
		  (throw 'nesting 'case)))))

       ((looking-at verilog-defun-level-re)
	(throw 'nesting 'defun))

       ((looking-at verilog-cpp-level-re)
	(throw 'nesting 'cpp))

       ((bobp)
	(throw 'nesting 'cpp))
       ))))

(defun verilog-calculate-indent-directive ()
  "Return indentation level for directive.
For speed, the searcher looks at the last directive, not the indent
of the appropriate enclosing block."
  (let ((base -1)	;; Indent of the line that determines our indentation
	(ind 0)	        ;; Relative offset caused by other directives (like `endif on same line as `else)
	)
    ;; Start at current location, scan back for another directive

    (save-excursion
      (beginning-of-line)
      (while (and (< base 0)
		  (verilog-re-search-backward verilog-directive-re nil t))
	(cond ((save-excursion (skip-chars-backward " \t") (bolp))
	       (setq base (current-indentation))
	       ))
	(cond ((and (looking-at verilog-directive-end) (< base 0))  ;; Only matters when not at BOL
	       (setq ind (- ind verilog-indent-level-directive)))
	      ((and (looking-at verilog-directive-middle) (>= base 0))  ;; Only matters when at BOL
	       (setq ind (+ ind verilog-indent-level-directive)))
	      ((looking-at verilog-directive-begin)
	       (setq ind (+ ind verilog-indent-level-directive)))))
      ;; Adjust indent to starting indent of critical line
      (setq ind (max 0 (+ ind base))))

    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (cond ((or (looking-at verilog-directive-middle)
		 (looking-at verilog-directive-end))
	     (setq ind (max 0 (- ind verilog-indent-level-directive))))))
   ind))

(defun verilog-leap-to-case-head ()
  (let ((nest 1))
    (while (/= 0 nest)
      (verilog-re-search-backward "\\(\\<case[xz]?\\>[^:]\\)\\|\\(\\<endcase\\>\\)" nil 'move)
      (cond
       ((match-end 1)
	(setq nest (1- nest)))
       ((match-end 2)
	(setq nest (1+ nest)))
       ((bobp)
	(ding 't)
	(setq nest 0))))))

(defun verilog-leap-to-head ()
  "Move point to the head of this block; jump from end to matching begin,
from endcase to matching case, and so on."
  (let (reg
	snest
	(nest 1))
    (cond
     ((looking-at "\\<end\\>")
      ;; Search back for matching begin
      (setq reg (concat "\\(\\<begin\\>\\)\\|\\(\\<end\\>\\)\\|"
			"\\(\\<endcase\\>\\)\\|\\(\\<join\\(_any\\|_none\\)?\\>\\)" )))

     ((looking-at "\\<endcase\\>")
      ;; Search back for matching case
      (setq reg "\\(\\<case[xz]?\\>\\)\\|\\(\\<endcase\\>\\)" ))
     ((looking-at "\\<join\\(_any\\|_none\\)?\\>")
      ;; Search back for matching fork
      (setq reg "\\(\\<fork\\>\\)\\|\\(\\<join\\(_any\\|_none\\)?\\>\\)" ))
     ((looking-at "\\<endtable\\>")
      ;; Search back for matching table
      (setq reg "\\(\\<table\\>\\)\\|\\(\\<endtable\\>\\)" ))
     ((looking-at "\\<endspecify\\>")
      ;; Search back for matching specify
      (setq reg "\\(\\<specify\\>\\)\\|\\(\\<endspecify\\>\\)" ))
     ((looking-at "\\<endfunction\\>")
      ;; Search back for matching function
      (setq reg "\\(\\<function\\>\\)\\|\\(\\<endfunction\\>\\)" ))
     ((looking-at "\\<endgenerate\\>")
      ;; Search back for matching generate
      (setq reg "\\(\\<generate\\>\\)\\|\\(\\<endgenerate\\>\\)" ))
     ((looking-at "\\<endtask\\>")
      ;; Search back for matching task
      (setq reg "\\(\\<task\\>\\)\\|\\(\\<endtask\\>\\)" ))
     )
    (catch 'skip
      (let (sreg)
	(while (verilog-re-search-backward reg nil 'move)
	  (cond
	   ((match-end 1) ; begin
	    (setq nest (1- nest))
	    (if (= 0 nest)
		;; Now previous line describes syntax
		(throw 'skip 1))
	    (if (and snest
		     (= snest nest))
		(setq reg sreg)))
	   ((match-end 2) ; end
	    (setq nest (1+ nest)))
	   ((match-end 3)
	    ;; endcase, jump to case
	    (setq snest nest)
	    (setq nest (1+ nest))
	    (setq sreg reg)
	    (setq reg "\\(\\<case[xz]?\\>[^:]\\)\\|\\(\\<endcase\\>\\)" ))
	   ((match-end 4)
	    ;; join, jump to fork
	    (setq snest nest)
	    (setq nest (1+ nest))
	    (setq sreg reg)
	    (setq reg "\\(\\<fork\\>\\)\\|\\(\\<join\\(_any\\|_none\\)?\\>\\)" ))
	   ))))))

(defun verilog-continued-line ()
  "Return true if this is a continued line.
Set point to where line starts"
  (let ((continued 't))
    (if (eq 0 (forward-line -1))
	(progn
	  (end-of-line)
	  (verilog-backward-ws&directives)
	  (if (bobp)
	      (setq continued nil)
	    (while (and continued
			(save-excursion
			  (skip-chars-backward " \t")
			  (not (bolp))))
	    (setq continued (verilog-backward-token))
	    ) ;; while
	    ))
      (setq continued nil))
    continued))

(defun verilog-backward-token ()
  "Step backward token, returning true if we are now at an end of line token."
  (verilog-backward-syntactic-ws)
  (cond
   ((bolp)
    nil)
   (;-- Anything ending in a ; is complete
    (= (preceding-char) ?\;)
    nil)
   (;-- Could be 'case (foo)' or 'always @(bar)' which is complete
    ;   also could be simply '@(foo)'
    ;   or foo u1 #(a=8)
    ;            (b, ... which ISN'T complete
    ;;;; Do we need this???
    (= (preceding-char) ?\))
    (progn
      (backward-char)
      (backward-up-list 1)
      (verilog-backward-syntactic-ws)
      (let ((back (point)))
	(forward-word -1)
	(cond
	 ((looking-at "\\<\\(always\\(_latch\\|_ff\\|_comb\\)?\\|case\\(\\|[xz]\\)\\|for\\(\\|ever\\)\\|i\\(f\\|nitial\\)\\|repeat\\|while\\)\\>")
	  (not (looking-at "\\<case[xz]?\\>[^:]")))
	 (t
	  (goto-char back)
	  (cond
	   ((= (preceding-char) ?\@)
	    (backward-char)
	    (save-excursion
	      (verilog-backward-token)
	      (not (looking-at "\\<\\(always\\(_latch\\|_ff\\|_comb\\)?\\|initial\\|while\\)\\>"))))
	   ((= (preceding-char) ?\#)
	    t)
	   (t t))
	  )))))

   (;-- any of begin|initial|while are complete statements; 'begin : foo' is also complete
    t
    (forward-word -1)
    (cond
     ((looking-at "\\(else\\)\\|\\(initial\\>\\)\\|\\(always\\(_latch\\|_ff\\|_comb\\)?\\>\\)")
      t)
     ((looking-at verilog-indent-reg)
      nil)
     (t
      (let
	  ((back (point)))
	(verilog-backward-syntactic-ws)
	(cond
	 ((= (preceding-char) ?\:)
	  (backward-char)
	  (verilog-backward-syntactic-ws)
	  (backward-sexp)
	  (if (looking-at "begin")
	      nil
	    t)
	  )
	 ((= (preceding-char) ?\#)
	  (backward-char)
	  t)
	 ((= (preceding-char) ?\`)
	  (backward-char)
	  t)

	 (t
	  (goto-char back)
	  t)
	 )))))))

(defun verilog-backward-syntactic-ws (&optional bound)
  "Backward skip over syntactic whitespace for Emacs 19.
Optional BOUND limits search."
  (save-restriction
    (let* ((bound (or bound (point-min))) (here bound) )
      (if (< bound (point))
	  (progn
	    (narrow-to-region bound (point))
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment (-(buffer-size)))
	      )))
      ))
  t)

(defun verilog-forward-syntactic-ws (&optional bound)
  "Forward skip over syntactic whitespace for Emacs 19.
Optional BOUND limits search."
  (save-restriction
    (let* ((bound (or bound (point-max)))
	   (here bound)
	   )
      (if (> bound (point))
	  (progn
	    (narrow-to-region (point) bound)
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment (buffer-size))
	      )))
      )))

(defun verilog-backward-ws&directives (&optional bound)
  "Backward skip over syntactic whitespace and compiler directives for Emacs 19.
Optional BOUND limits search."
  (save-restriction
    (let* ((bound (or bound (point-min)))
	   (here bound)
	   (p nil) )
      (if (< bound (point))
	  (progn
	    (let ((state
		   (save-excursion
		     (parse-partial-sexp (point-min) (point)))))
	      (cond
	       ((nth 7 state) ;; in // comment
		(verilog-re-search-backward "//" nil 'move)
                (skip-chars-backward "/"))
	       ((nth 4 state) ;; in /* */ comment
		(verilog-re-search-backward "/\*" nil 'move))))
	    (narrow-to-region bound (point))
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment (-(buffer-size)))
	      (setq p
		    (save-excursion
		      (beginning-of-line)
		      (cond
		       ((verilog-within-translate-off)
			(verilog-back-to-start-translate-off (point-min)))
		       ((looking-at verilog-directive-re-1)
			(point))
		       (t
			nil))))
	      (if p (goto-char p))
	      )))
      )))

(defun verilog-forward-ws&directives (&optional bound)
  "Forward skip over syntactic whitespace and compiler directives for Emacs 19.
Optional BOUND limits search."
  (save-restriction
    (let* ((bound (or bound (point-max)))
	   (here bound)
	   jump
	   )
      (if (> bound (point))
	  (progn
	    (let ((state
		   (save-excursion
		     (parse-partial-sexp (point-min) (point)))))
	      (cond
	       ((nth 7 state) ;; in // comment
		(verilog-re-search-forward "//" nil 'move))
	       ((nth 4 state) ;; in /* */ comment
		(verilog-re-search-forward "/\*" nil 'move))))
	    (narrow-to-region (point) bound)
	    (while (/= here (point))
	      (setq here (point)
		    jump nil)
	      (forward-comment (buffer-size))
	      (save-excursion
		(beginning-of-line)
		(if (looking-at verilog-directive-re-1)
		    (setq jump t)))
	      (if jump
		  (beginning-of-line 2))
	      )))
      )))

(defun verilog-in-comment-p ()
 "Return true if in a star or // comment."
 (let ((state
	(save-excursion
	  (parse-partial-sexp (point-min) (point)))))
   (or (nth 4 state) (nth 7 state))))

(defun verilog-in-star-comment-p ()
 "Return true if in a star comment."
 (let ((state
	(save-excursion
	  (parse-partial-sexp (point-min) (point)))))
   (nth 4 state)))

(defun verilog-in-slash-comment-p ()
 "Return true if in a slash comment."
 (let ((state
	(save-excursion
	  (parse-partial-sexp (point-min) (point)))))
   (nth 7 state)))

(defun verilog-in-comment-or-string-p ()
 "Return true if in a string or comment."
 (let ((state
	(save-excursion
	  (parse-partial-sexp (point-min) (point)))))
   (or (nth 3 state) (nth 4 state) (nth 7 state)))) ; Inside string or comment)

(defun verilog-in-escaped-name-p ()
 "Return true if in an escaped name."
 (save-excursion
   (backward-char)
   (skip-chars-backward "^ \t\n")
   (if (= (char-after (point) ) ?\\ )
       t
     nil)))

(defun verilog-in-paren ()
 "Return true if in a parenthetical expression."
 (let ((state
	(save-excursion
	  (parse-partial-sexp (point-min) (point)))))
   (/= 0 (nth 0 state))))

(defun verilog-parenthesis-depth ()
 "Return non zero if in parenthetical-expression."
 (save-excursion
   (nth 1 (parse-partial-sexp (point-min) (point)))))

(defun verilog-skip-forward-comment-or-string ()
 "Return true if in a string or comment."
 (let ((state
	(save-excursion
	  (parse-partial-sexp (point-min) (point)))))
   (cond
    ((nth 3 state)			;Inside string
     (goto-char (nth 3 state))
     t)
    ((nth 7 state)			;Inside // comment
     (forward-line 1)
     t)
    ((nth 4 state)			;Inside any comment (hence /**/)
     (search-forward "*/"))
    (t
     nil))))

(defun verilog-skip-backward-comment-or-string ()
 "Return true if in a string or comment."
 (let ((state
	(save-excursion
	  (parse-partial-sexp (point-min) (point)))))
   (cond
    ((nth 3 state)			;Inside string
     (search-backward "\"")
     t)
    ((nth 7 state)			;Inside // comment
     (search-backward "//")
     (skip-chars-backward "/")
     t)
    ((nth 4 state)			;Inside /* */ comment
     (search-backward "/*")
     t)
    (t
     nil))))

(defun verilog-skip-forward-comment-p ()
  "If in comment, move to end and return true."
  (let (state)
    (progn
      (setq state
	    (save-excursion
	      (parse-partial-sexp (point-min) (point))))
      (cond
       ((nth 3 state)
	t)
       ((nth 7 state)			;Inside // comment
	(end-of-line)
	(forward-char 1)
	t)
       ((nth 4 state)			;Inside any comment
	t)
       (t
	nil)))))

(defun verilog-indent-line-relative ()
  "Cheap version of indent line.
Only look at a few lines to determine indent level."
  (interactive)
  (let ((indent-str)
	(sp (point)))
    (if (looking-at "^[ \t]*$")
	(cond  ;- A blank line; No need to be too smart.
	 ((bobp)
	  (setq indent-str (list 'cpp 0)))
	 ((verilog-continued-line)
	  (let ((sp1 (point)))
	    (if (verilog-continued-line)
		(progn (goto-char sp)
		       (setq indent-str (list 'statement (verilog-indent-level))))
	      (goto-char sp1)
	      (setq indent-str (list 'block (verilog-indent-level)))))
	  (goto-char sp))
	 ((goto-char sp)
	  (setq indent-str (verilog-calculate-indent))))
      (progn (skip-chars-forward " \t")
	     (setq indent-str (verilog-calculate-indent))))
    (verilog-do-indent indent-str)))

(defun verilog-indent-line ()
  "Indent for special part of code."
  (verilog-do-indent (verilog-calculate-indent)))

(defun verilog-do-indent (indent-str)
  (let ((type (car indent-str))
	(ind (car (cdr indent-str))))
    (cond
     (; handle continued exp
      (eq type 'cexp)
      (let ((here (point)))
	(verilog-backward-syntactic-ws)
	(cond
	 ((or
	   (= (preceding-char) ?\,)
	   (= (preceding-char) ?\])
	   (save-excursion
	     (verilog-beg-of-statement-1)
	     (looking-at verilog-declaration-re)))
	  (let* ( fst
		  (val
		   (save-excursion
		     (backward-char 1)
		     (verilog-beg-of-statement-1)
		     (setq fst (point))
		     (if (looking-at verilog-declaration-re)
			 (progn ;; we have multiple words
			   (goto-char (match-end 0))
			   (skip-chars-forward " \t")
			   (cond
			    ((and verilog-indent-declaration-macros
				  (= (following-char) ?\`))
			     (progn
			       (forward-char 1)
			       (forward-word 1)
			       (skip-chars-forward " \t")))
			    ((= (following-char) ?\[)
			     (progn
			       (forward-char 1)
			       (backward-up-list -1)
			       (skip-chars-forward " \t")))
			    )
			   (current-column))
		       (progn
			 (goto-char fst)
			 (+ (current-column) verilog-cexp-indent))
		       ))))
	    (goto-char here)
	    (indent-line-to val))
	  )
	 ((= (preceding-char) ?\) )
	  (goto-char here)
	  (let ((val (eval (cdr (assoc type verilog-indent-alist)))))
	    (indent-line-to val)))
	 (t
	  (goto-char here)
	  (let ((val))
	    (verilog-beg-of-statement-1)
	    (if (verilog-re-search-forward "=[ \\t]*" here 'move)
		(setq val (current-column))
	      (setq val (eval (cdr (assoc type verilog-indent-alist)))))
	    (goto-char here)
	    (indent-line-to val)))
	 )))

     (; handle inside parenthetical expressions
      (eq type 'cparenexp)
      (let ((val (save-excursion
		   (backward-up-list 1)
		   (forward-char 1)
		   (skip-chars-forward " \t")
		   (current-column))))
	(indent-line-to val)))

     (;-- Handle the ends
      (looking-at verilog-end-block-re )
      (let ((val (if (eq type 'statement)
		     (- ind verilog-indent-level)
		   ind)))
	(indent-line-to val)))

     (;-- Case -- maybe line 'em up
      (and (eq type 'case) (not (looking-at "^[ \t]*$")))
      (progn
	(cond
	 ((looking-at "\\<endcase\\>")
	  (indent-line-to ind))
	 (t
	  (let ((val (eval (cdr (assoc type verilog-indent-alist)))))
	    (indent-line-to val))))))

     (;-- defun
      (and (eq type 'defun)
 	   (looking-at verilog-zero-indent-re))
      (indent-line-to 0))

     (;-- declaration
      (and (or
	    (eq type 'defun)
	    (eq type 'block))
	   (looking-at verilog-declaration-re))
      (verilog-indent-declaration ind))

     (;-- Everything else
      t
      (let ((val (eval (cdr (assoc type verilog-indent-alist)))))
	(indent-line-to val)))
     )
    (if (looking-at "[ \t]+$")
	(skip-chars-forward " \t"))
    indent-str				; Return indent data
    ))

(defun verilog-indent-level ()
  "Return the indent-level the current statement has."
  (save-excursion
    (let (par-pos)
      (beginning-of-line)
      (setq par-pos (verilog-parenthesis-depth))
      (while par-pos
	(goto-char par-pos)
	(beginning-of-line)
	(setq par-pos (verilog-parenthesis-depth)))
      (skip-chars-forward " \t")
      (current-column))))

(defun verilog-case-indent-level ()
  "Return the indent-level the current statement has.
Do not count named blocks or case-statements."
  (save-excursion
    (skip-chars-forward " \t")
    (cond
     ((looking-at verilog-named-block-re)
      (current-column))
     ((and (not (looking-at verilog-case-re))
	   (looking-at "^[^:;]+[ \t]*:"))
      (verilog-re-search-forward ":" nil t)
      (skip-chars-forward " \t")
      (current-column))
     (t
      (current-column)))))

(defun verilog-indent-comment ()
  "Indent current line as comment."
  (let* ((stcol
	  (cond
	   ((verilog-in-star-comment-p)
	    (save-excursion
	      (re-search-backward "/\\*" nil t)
	      (1+(current-column))))
	   (comment-column
	     comment-column )
	   (t
	    (save-excursion
	      (re-search-backward "//" nil t)
	      (current-column)))
	   )))
    (indent-line-to stcol)
    stcol))

(defun verilog-more-comment ()
  "Make more comment lines like the previous."
  (let* ((star 0)
	 (stcol
	  (cond
	   ((verilog-in-star-comment-p)
	    (save-excursion
	      (setq star 1)
	      (re-search-backward "/\\*" nil t)
	      (1+(current-column))))
	   (comment-column
	    comment-column )
	   (t
	    (save-excursion
	      (re-search-backward "//" nil t)
	      (current-column)))
	   )))
    (progn
      (indent-to stcol)
      (if (and star
	       (save-excursion
		 (forward-line -1)
		 (skip-chars-forward " \t")
		 (looking-at "\*")))
	  (insert "* ")))))

(defun verilog-comment-indent (&optional arg)
  "Return the column number the line should be indented to.
ARG is ignored, for `comment-indent-function' compatibility."
  (cond
   ((verilog-in-star-comment-p)
    (save-excursion
      (re-search-backward "/\\*" nil t)
      (1+(current-column))))
   ( comment-column
     comment-column )
   (t
    (save-excursion
      (re-search-backward "//" nil t)
      (current-column)))))

;;

(defun verilog-pretty-declarations ()
  "Line up declarations around point."
  (interactive)
  (save-excursion
    (if (progn
	  (verilog-beg-of-statement-1)
	  (looking-at verilog-declaration-re))
	(let* ((m1 (make-marker))
	       (e) (r)
	       (here (point))
	       (start
		(progn
		  (verilog-beg-of-statement-1)
		  (while (looking-at verilog-declaration-re)
		    (beginning-of-line)
		    (setq e (point))
		    (verilog-backward-syntactic-ws)
		    (backward-char)
		    (verilog-beg-of-statement-1)) ;Ack, need to grok `define
		  e))
	       (end
		(progn
		  (goto-char here)
		  (verilog-end-of-statement)
		  (setq e (point))	;Might be on last line
		  (verilog-forward-syntactic-ws)
		  (while (looking-at verilog-declaration-re)
		    (beginning-of-line)
		    (verilog-end-of-statement)
		    (setq e (point))
		    (verilog-forward-syntactic-ws))
		  e))
	       (edpos (set-marker (make-marker) end))
	       (ind)
	       (base-ind
		(progn
		  (goto-char start)
		  (verilog-do-indent (verilog-calculate-indent))
		  (verilog-forward-ws&directives)
		  (current-column)))
	       )
	  (goto-char end)
	  (goto-char start)
	  (if (> (- end start) 100)
	      (message "Lining up declarations..(please stand by)"))
	  ;; Get the beginning of line indent first
	  (while (progn (setq e (marker-position edpos))
			(< (point) e))
	    (indent-line-to base-ind)
	    (forward-line))
	  ;; Now find biggest prefix
	  (setq ind (verilog-get-lineup-indent start edpos))
	  ;; Now indent each line.
	  (goto-char start)
	  (while (progn (setq e (marker-position edpos))
			(setq r (- e (point)))
			(> r 0))
	    (setq e (point))
	    (message "%d" r)
	    (cond
	     ((or (and verilog-indent-declaration-macros
		       (looking-at verilog-declaration-re-1-macro))
		  (looking-at verilog-declaration-re-1-no-macro))
	      (let ((p (match-end 0)))
		(set-marker m1 p)
		(if (verilog-re-search-forward "[[#`]" p 'move)
		    (progn
		      (forward-char -1)
		      (just-one-space)
		      (goto-char (marker-position m1))
		      (just-one-space)
		      (indent-to ind))
		  (progn
		    (just-one-space)
		    (indent-to ind))
		  )))
	     ((verilog-continued-line-1 start)
	      (goto-char e)
	      (indent-line-to ind))
	     (t 	; Must be comment or white space
	      (goto-char e)
	      (verilog-forward-ws&directives)
	      (forward-line -1))
	     )
	    (forward-line 1))
	  (message "")))))

(defun verilog-pretty-expr (&optional myre)
  "Line up expressions around point"
  (interactive)
  (save-excursion
    (if (not myre)
	(setq myre (read-string "Regular Expression: (<=) ")))
    (if (string-equal myre "") (setq myre "<="))
    (setq myre (concat "\\(^.*\\)\\(" myre "\\)"))
    (beginning-of-line)
    (if (and (not(looking-at verilog-complete-reg))
	     (looking-at myre))
	(let* ((m1 (make-marker))
	       (e) (r)
	       (here (point))
	       (start
		(progn
		  (beginning-of-line)
		  (setq e (point))
		  (verilog-backward-syntactic-ws)
		  (beginning-of-line)
		  (while (and (not(looking-at (concat "^\\s-*" verilog-complete-reg)))
			      (looking-at myre))
		    (setq e (point))
		    (verilog-backward-syntactic-ws)
		    (beginning-of-line)
		    ) ;Ack, need to grok `define
		  e))
	       (end
		(progn
		  (goto-char here)
		  (end-of-line)
		  (setq e (point))	;Might be on last line
		  (verilog-forward-syntactic-ws)
		  (beginning-of-line)
		  (while (and (not(looking-at (concat "^\\s-*" verilog-complete-reg)))
			      (looking-at myre))
		    (end-of-line)
		    (setq e (point))
		    (verilog-forward-syntactic-ws)
		    (beginning-of-line)
		    )
		  e))
	       (edpos (set-marker (make-marker) end))
	       (ind)
	       )
	  (goto-char start)
	  (verilog-do-indent (verilog-calculate-indent))
	  (goto-char end)
	  ;;(insert "/*end*/")
	  (goto-char start)
	  ;;(insert "/*start*/")
	  (if (> (- end start) 100)
	      (message "Lining up expressions..(please stand by)"))
	  ;; Get the begining of line indent first
	  (while (progn (setq e (marker-position edpos))
			(< (point) e))
	    (beginning-of-line)
	    (verilog-just-one-space myre)
	    (forward-line))
	  ;; Now find biggest prefix
	  (setq ind (verilog-get-lineup-indent-2 myre start edpos))
	  ;; Now indent each line.
	  (goto-char start)
	  (while (progn (setq e (marker-position edpos))
			(setq r (- e (point)))
			(> r 0))
	    (setq e (point))
	    (message "%d" r)
	    (cond
	     ((looking-at myre)
	      (let ((p1 (match-end 1)))
		(set-marker m1 p1)
		(progn
		  (set-marker m1 p1)
		  (goto-char (marker-position m1))
		  (indent-to ind)
		  )))
	     ((verilog-continued-line-1 start)
	      (goto-char e)
	      (indent-line-to ind))
	     (t 	; Must be comment or white space
	      (goto-char e)
	      (verilog-forward-ws&directives)
	      (forward-line -1))
	     )
	    (forward-line 1))
	  (message "")
	  ))))

(defun verilog-just-one-space (myre)
  "Remove extra spaces around regular expression"
  (interactive)
  (save-excursion
    (if (and (not(looking-at verilog-complete-reg))
			      (looking-at myre))
	(let ((m1 (make-marker))
	      (p2 (match-end 2))
	      (p1 (match-end 1)))
	  (set-marker m1 p1)
	  (progn
	    (set-marker m1 p2)
	    (goto-char (marker-position m1))
	    (if (looking-at "\\s-")
		(just-one-space) )
	    (set-marker m1 p1)
	    (goto-char (marker-position m1))
	    (forward-char -1)
	    (if (looking-at "\\s-")
		(just-one-space))
	    )
	  ))
    (message "")))

(defun verilog-indent-declaration (baseind)
  "Indent current lines as declaration.
Line up the variable names based on previous declaration's indentation.
BASEIND is the base indent to offset everything."
  (interactive)
  (let ((pos (point-marker))
	(lim (save-excursion
	       (verilog-re-search-backward "\\(\\<begin\\>\\)\\|\\(\\<module\\>\\)" nil 'move)
	       (point)))
	(ind)
	(val)
	(m1 (make-marker))
	)
    ;; Use previous declaration (in this module) as template.
    (if (verilog-re-search-backward (or (and verilog-indent-declaration-macros
					     verilog-declaration-re-1-macro)
					verilog-declaration-re-1-no-macro) lim t)
	(progn
	  (goto-char (match-end 0))
	  (skip-chars-forward " \t")
	  (setq ind (current-column))
	  (goto-char pos)
	  (setq val (+ baseind (eval (cdr (assoc 'declaration verilog-indent-alist)))))
	  (indent-line-to val)
	  (if (and verilog-indent-declaration-macros
		   (looking-at verilog-declaration-re-2-macro))
	      (let ((p (match-end 0)))
		(set-marker m1 p)
		(if (verilog-re-search-forward "[[#`]" p 'move)
		    (progn
		      (forward-char -1)
		      (just-one-space)
		      (goto-char (marker-position m1))
		      (just-one-space)
		      (indent-to ind)
		      )
		  (if (/= (current-column) ind)
		      (progn
			(just-one-space)
			(indent-to ind))
		    )))
	    (if (looking-at verilog-declaration-re-2-no-macro)
		(let ((p (match-end 0)))
		  (set-marker m1 p)
		  (if (verilog-re-search-forward "[[`#]" p 'move)
		      (progn
			(forward-char -1)
			(just-one-space)
			(goto-char (marker-position m1))
			(just-one-space)
			(indent-to ind))
		    (if (/= (current-column) ind)
			(progn
			  (just-one-space)
			  (indent-to ind))
		      )))
	      )))
      (let ((val (+ baseind (eval (cdr (assoc 'declaration verilog-indent-alist))))))
	(indent-line-to val))
      )
    (goto-char pos)))

(defun verilog-get-lineup-indent (b edpos)
  "Return the indent level that will line up several lines within the region.
Region is defined by B and EDPOS."
  (save-excursion
    (let ((ind 0) e)
      (goto-char b)
      ;; Get rightmost position
      (while (progn (setq e (marker-position edpos))
		    (< (point) e))
	(if (verilog-re-search-forward (or (and verilog-indent-declaration-macros
						verilog-declaration-re-1-macro)
					   verilog-declaration-re-1-no-macro) e 'move)
	    (progn
	      (goto-char (match-end 0))
	      (verilog-backward-syntactic-ws)
	      (if (> (current-column) ind)
		  (setq ind (current-column)))
	      (goto-char (match-end 0)))))
      (if (> ind 0)
	  (1+ ind)
	;; No lineup-string found
	(goto-char b)
	(end-of-line)
	(skip-chars-backward " \t")
	(1+ (current-column))))))

(defun verilog-get-lineup-indent-2 (myre b edpos)
  "Return the indent level that will line up several lines within the region
from b to e nicely. The lineup string is str."
  (save-excursion
    (let ((ind 0) e)
      (goto-char b)
      ;; Get rightmost position
      (while (progn (setq e (marker-position edpos))
		    (< (point) e))
	(if (re-search-forward myre e 'move)
	    (progn
	      (goto-char (match-end 0))
	      (verilog-backward-syntactic-ws)
	      (if (> (current-column) ind)
		  (setq ind (current-column)))
	      (goto-char (match-end 0)))))
      (if (> ind 0)
	  (1+ ind)
	;; No lineup-string found
	(goto-char b)
	(end-of-line)
	(skip-chars-backward " \t")
	(1+ (current-column))))))

(defun verilog-comment-depth (type val)
  "A useful mode debugging aide.  TYPE and VAL are comments for insertion."
  (save-excursion
    (let
	((b (prog2
		(beginning-of-line)
		(point-marker)
	      (end-of-line)))
	 (e (point-marker)))
      (if (re-search-backward " /\\* \[#-\]# \[a-zA-Z\]+ \[0-9\]+ ## \\*/" b t)
	  (progn
	    (replace-match " /* -#  ## */")
	    (end-of-line))
	(progn
	  (end-of-line)
	  (insert " /* ##  ## */"))))
    (backward-char 6)
    (insert
     (format "%s %d" type val))))

;; 
;;
;; Completion
;;
(defvar verilog-str nil)
(defvar verilog-all nil)
(defvar verilog-pred nil)
(defvar verilog-buffer-to-use nil)
(defvar verilog-flag nil)
(defvar verilog-toggle-completions nil
  "*True means \\<verilog-mode-map>\\[verilog-complete-word] should try all possible completions one by one.
Repeated use of \\[verilog-complete-word] will show you all of them.
Normally, when there is more than one possible completion,
it displays a list of all possible completions.")


(defvar verilog-type-keywords
  '(
    "and" "buf" "bufif0" "bufif1" "cmos" "defparam" "inout" "input"
    "integer" "localparam" "logic" "nand" "nmos" "nor" "not" "notif0"
    "notif1" "or" "output" "parameter" "pmos" "pull0" "pull1" "pullup"
    "rcmos" "real" "realtime" "reg" "rnmos" "rpmos" "rtran" "rtranif0"
    "rtranif1" "time" "tran" "tranif0" "tranif1" "tri" "tri0" "tri1"
    "triand" "trior" "trireg" "wand" "wire" "wor" "xnor" "xor"
    )
  "*Keywords for types used when completing a word in a declaration or parmlist.
\(eg.  integer, real, reg...)")

(defvar verilog-cpp-keywords
  '("module" "macromodule" "primitive" "timescale" "define" "ifdef" "ifndef" "else"
    "endif")
  "*Keywords to complete when at first word of a line in declarative scope.
\(eg.  initial, always, begin, assign.)
The procedures and variables defined within the Verilog program
will be completed runtime and should not be added to this list.")

(defvar verilog-defun-keywords
  (append
   '(
     "always" "always_comb" "always_ff" "always_latch" "assign"
     "begin" "end" "generate" "endgenerate" "module" "endmodule"
     "specify" "endspecify" "function" "endfunction" "initial" "final"
     "task" "endtask" "primitive" "endprimitive"
     )
   verilog-type-keywords)
  "*Keywords to complete when at first word of a line in declarative scope.
\(eg.  initial, always, begin, assign.)
The procedures and variables defined within the Verilog program
will be completed runtime and should not be added to this list.")

(defvar verilog-block-keywords
  '(
    "begin" "break" "case" "continue" "else" "end" "endfunction"
    "endgenerate" "endinterface" "endpackage" "endspecify" "endtask"
    "for" "fork" "if" "join" "join_any" "join_none" "repeat" "return"
    "while")
  "*Keywords to complete when at first word of a line in behavioral scope.
\(eg.  begin, if, then, else, for, fork.)
The procedures and variables defined within the Verilog program
will be completed runtime and should not be added to this list.")

(defvar verilog-tf-keywords
  '("begin" "break" "fork" "join" "join_any" "join_none" "case" "end" "endtask" "endfunction" "if" "else" "for" "while" "repeat")
  "*Keywords to complete when at first word of a line in a task or function.
\(eg.  begin, if, then, else, for, fork.)
The procedures and variables defined within the Verilog program
will be completed runtime and should not be added to this list.")

(defvar verilog-case-keywords
  '("begin" "fork" "join" "join_any" "join_none" "case" "end" "endcase" "if" "else" "for" "repeat")
  "*Keywords to complete when at first word of a line in case scope.
\(eg.  begin, if, then, else, for, fork.)
The procedures and variables defined within the Verilog program
will be completed runtime and should not be added to this list.")

(defvar verilog-separator-keywords
  '("else" "then" "begin")
  "*Keywords to complete when NOT standing at the first word of a statement.
\(eg.  else, then.)
Variables and function names defined within the
Verilog program are completed runtime and should not be added to this list.")

(defun verilog-string-diff (str1 str2)
  "Return index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
	(if (or (> (1+ diff) (length str1))
		(> (1+ diff) (length str2)))
	    (throw 'done diff))
	(or (equal (aref str1 diff) (aref str2 diff))
	    (throw 'done diff))
	(setq diff (1+ diff))))))

;; Calculate all possible completions for functions if argument is `function',
;; completions for procedures if argument is `procedure' or both functions and
;; procedures otherwise.

(defun verilog-func-completion (type)
  "Build regular expression for module/task/function names.
TYPE is 'module, 'tf for task or function, or t if unknown."
  (if (string= verilog-str "")
      (setq verilog-str "[a-zA-Z_]"))
  (let ((verilog-str (concat (cond
			     ((eq type 'module) "\\<\\(module\\)\\s +")
			     ((eq type 'tf) "\\<\\(task\\|function\\)\\s +")
			     (t "\\<\\(task\\|function\\|module\\)\\s +"))
			    "\\<\\(" verilog-str "[a-zA-Z0-9_.]*\\)\\>"))
	match)

    (if (not (looking-at verilog-defun-re))
	(verilog-re-search-backward verilog-defun-re nil t))
    (forward-char 1)

    ;; Search through all reachable functions
    (goto-char (point-min))
    (while (verilog-re-search-forward verilog-str (point-max) t)
      (progn (setq match (buffer-substring (match-beginning 2)
					   (match-end 2)))
	     (if (or (null verilog-pred)
		     (funcall verilog-pred match))
		 (setq verilog-all (cons match verilog-all)))))
    (if (match-beginning 0)
	(goto-char (match-beginning 0)))))

(defun verilog-get-completion-decl (end)
  "Macro for searching through current declaration (var, type or const)
for matches of `str' and adding the occurence tp `all' through point END."
  (let ((re (or (and verilog-indent-declaration-macros
		     verilog-declaration-re-2-macro)
		verilog-declaration-re-2-no-macro))
	decl-end match)
    ;; Traverse lines
    (while (and (< (point) end)
		(verilog-re-search-forward re end t))
      ;; Traverse current line
      (setq decl-end (save-excursion (verilog-declaration-end)))
      (while (and (verilog-re-search-forward verilog-symbol-re decl-end t)
		  (not (match-end 1)))
	(setq match (buffer-substring (match-beginning 0) (match-end 0)))
	(if (string-match (concat "\\<" verilog-str) match)
	    (if (or (null verilog-pred)
		    (funcall verilog-pred match))
		(setq verilog-all (cons match verilog-all)))))
      (forward-line 1)
      )
    )
  verilog-all
  )

(defun verilog-type-completion ()
  "Calculate all possible completions for types."
  (let ((start (point))
	goon)
    ;; Search for all reachable type declarations
    (while (or (verilog-beg-of-defun)
	       (setq goon (not goon)))
      (save-excursion
	(if (and (< start (prog1 (save-excursion (verilog-end-of-defun)
						 (point))
			    (forward-char 1)))
		 (verilog-re-search-forward
		  "\\<type\\>\\|\\<\\(begin\\|function\\|procedure\\)\\>"
		  start t)
		 (not (match-end 1)))
	    ;; Check current type declaration
	    (verilog-get-completion-decl start))))))

(defun verilog-var-completion ()
  "Calculate all possible completions for variables (or constants)."
  (let ((start (point)))
    ;; Search for all reachable var declarations
    (verilog-beg-of-defun)
    (save-excursion
      ;; Check var declarations
      (verilog-get-completion-decl start))))

(defun verilog-keyword-completion (keyword-list)
  "Give list of all possible completions of keywords in KEYWORD-LIST."
  (mapcar '(lambda (s)
	     (if (string-match (concat "\\<" verilog-str) s)
		 (if (or (null verilog-pred)
			 (funcall verilog-pred s))
		     (setq verilog-all (cons s verilog-all)))))
	  keyword-list))


(defun verilog-completion (verilog-str verilog-pred verilog-flag)
  "Function passed to `completing-read', `try-completion' or `all-completions'.
Called to get completion on VERILOG-STR.  If VERILOG-PRED is non-nil, it
must be a function to be called for every match to check if this should
really be a match.  If VERILOG-FLAG is t, the function returns a list of all
possible completions.  If VERILOG-FLAG is nil it returns a string, the
longest possible completion, or t if STR is an exact match.  If VERILOG-FLAG
is 'lambda, the function returns t if STR is an exact match, nil
otherwise."
  (save-excursion
    (let ((verilog-all nil))
      ;; Set buffer to use for searching labels. This should be set
      ;; within functions which use verilog-completions
      (set-buffer verilog-buffer-to-use)

      ;; Determine what should be completed
      (let ((state (car (verilog-calculate-indent))))
	(cond ((eq state 'defun)
	       (save-excursion (verilog-var-completion))
	       (verilog-func-completion 'module)
	       (verilog-keyword-completion verilog-defun-keywords))

	      ((eq state 'behavioral)
	       (save-excursion (verilog-var-completion))
	       (verilog-func-completion 'module)
	       (verilog-keyword-completion verilog-defun-keywords))

	      ((eq state 'block)
	       (save-excursion (verilog-var-completion))
	       (verilog-func-completion 'tf)
	       (verilog-keyword-completion verilog-block-keywords))

	      ((eq state 'case)
	       (save-excursion (verilog-var-completion))
	       (verilog-func-completion 'tf)
	       (verilog-keyword-completion verilog-case-keywords))

	      ((eq state 'tf)
	       (save-excursion (verilog-var-completion))
	       (verilog-func-completion 'tf)
	       (verilog-keyword-completion verilog-tf-keywords))

	      ((eq state 'cpp)
	       (save-excursion (verilog-var-completion))
	       (verilog-keyword-completion verilog-cpp-keywords))

	      ((eq state 'cparenexp)
	       (save-excursion (verilog-var-completion)))

	      (t;--Anywhere else
	       (save-excursion (verilog-var-completion))
	       (verilog-func-completion 'both)
	       (verilog-keyword-completion verilog-separator-keywords))))

      ;; Now we have built a list of all matches. Give response to caller
      (verilog-completion-response))))

(defun verilog-completion-response ()
  (cond ((or (equal verilog-flag 'lambda) (null verilog-flag))
	 ;; This was not called by all-completions
	 (if (null verilog-all)
	     ;; Return nil if there was no matching label
	     nil
	   ;; Get longest string common in the labels
	   (let* ((elm (cdr verilog-all))
		  (match (car verilog-all))
		  (min (length match))
		  tmp)
	     (if (string= match verilog-str)
		 ;; Return t if first match was an exact match
		 (setq match t)
	       (while (not (null elm))
		 ;; Find longest common string
		 (if (< (setq tmp (verilog-string-diff match (car elm))) min)
		     (progn
		       (setq min tmp)
		       (setq match (substring match 0 min))))
		 ;; Terminate with match=t if this is an exact match
		 (if (string= (car elm) verilog-str)
		     (progn
		       (setq match t)
		       (setq elm nil))
		   (setq elm (cdr elm)))))
	     ;; If this is a test just for exact match, return nil ot t
	     (if (and (equal verilog-flag 'lambda) (not (equal match 't)))
		 nil
	       match))))
	;; If flag is t, this was called by all-completions. Return
	;; list of all possible completions
	(verilog-flag
	 verilog-all)))

(defvar verilog-last-word-numb 0)
(defvar verilog-last-word-shown nil)
(defvar verilog-last-completions nil)

(defun verilog-complete-word ()
  "Complete word at current point.
\(See also `verilog-toggle-completions', `verilog-type-keywords',
`verilog-start-keywords' and `verilog-separator-keywords'.)"
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
	 (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
	 (verilog-str (buffer-substring b e))
	 ;; The following variable is used in verilog-completion
	 (verilog-buffer-to-use (current-buffer))
	 (allcomp (if (and verilog-toggle-completions
			   (string= verilog-last-word-shown verilog-str))
		      verilog-last-completions
		    (all-completions verilog-str 'verilog-completion)))
	 (match (if verilog-toggle-completions
		    "" (try-completion
			verilog-str (mapcar '(lambda (elm)
					      (cons elm 0)) allcomp)))))
    ;; Delete old string
    (delete-region b e)

    ;; Toggle-completions inserts whole labels
    (if verilog-toggle-completions
	(progn
	  ;; Update entry number in list
	  (setq verilog-last-completions allcomp
		verilog-last-word-numb
		(if (>= verilog-last-word-numb (1- (length allcomp)))
		    0
		  (1+ verilog-last-word-numb)))
	  (setq verilog-last-word-shown (elt allcomp verilog-last-word-numb))
	  ;; Display next match or same string if no match was found
	  (if (not (null allcomp))
	      (insert "" verilog-last-word-shown)
	    (insert "" verilog-str)
	    (message "(No match)")))
      ;; The other form of completion does not necessarily do that.

      ;; Insert match if found, or the original string if no match
      (if (or (null match) (equal match 't))
	  (progn (insert "" verilog-str)
		 (message "(No match)"))
	(insert "" match))
      ;; Give message about current status of completion
      (cond ((equal match 't)
	     (if (not (null (cdr allcomp)))
		 (message "(Complete but not unique)")
	       (message "(Sole completion)")))
	    ;; Display buffer if the current completion didn't help
	    ;; on completing the label.
	    ((and (not (null (cdr allcomp))) (= (length verilog-str)
						(length match)))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list allcomp))
	     ;; Wait for a key press. Then delete *Completion*  window
	     (momentary-string-display "" (point))
	     (delete-window (get-buffer-window (get-buffer "*Completions*")))
	     )))))

(defun verilog-show-completions ()
  "Show all possible completions at current point."
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward "a-zA-Z0-9_") (point)))
	 (e (save-excursion (skip-chars-forward "a-zA-Z0-9_") (point)))
	 (verilog-str (buffer-substring b e))
	 ;; The following variable is used in verilog-completion
	 (verilog-buffer-to-use (current-buffer))
	 (allcomp (if (and verilog-toggle-completions
			   (string= verilog-last-word-shown verilog-str))
		      verilog-last-completions
		    (all-completions verilog-str 'verilog-completion))))
    ;; Show possible completions in a temporary buffer.
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list allcomp))
    ;; Wait for a key press. Then delete *Completion*  window
    (momentary-string-display "" (point))
    (delete-window (get-buffer-window (get-buffer "*Completions*")))))


(defun verilog-get-default-symbol ()
  "Return symbol around current point as a string."
  (save-excursion
    (buffer-substring (progn
			(skip-chars-backward " \t")
			(skip-chars-backward "a-zA-Z0-9_")
			(point))
		      (progn
			(skip-chars-forward "a-zA-Z0-9_")
			(point)))))

(defun verilog-build-defun-re (str &optional arg)
  "Return function/task/module starting with STR as regular expression.
With optional second ARG non-nil, STR is the complete name of the instruction."
  (if arg
      (concat "^\\(function\\|task\\|module\\)[ \t]+\\(" str "\\)\\>")
    (concat "^\\(function\\|task\\|module\\)[ \t]+\\(" str "[a-zA-Z0-9_]*\\)\\>")))

(defun verilog-comp-defun (verilog-str verilog-pred verilog-flag)
  "Function passed to `completing-read', `try-completion' or `all-completions'.
Returns a completion on any function name based on VERILOG-STR prefix.  If
VERILOG-PRED is non-nil, it must be a function to be called for every match
to check if this should really be a match.  If VERILOG-FLAG is t, the
function returns a list of all possible completions.  If it is nil it
returns a string, the longest possible completion, or t if VERILOG-STR is
an exact match.  If VERILOG-FLAG is 'lambda, the function returns t if
VERILOG-STR is an exact match, nil otherwise."
  (save-excursion
    (let ((verilog-all nil)
	  match)

      ;; Set buffer to use for searching labels. This should be set
      ;; within functions which use verilog-completions
      (set-buffer verilog-buffer-to-use)

      (let ((verilog-str verilog-str))
	;; Build regular expression for functions
	(if (string= verilog-str "")
	    (setq verilog-str (verilog-build-defun-re "[a-zA-Z_]"))
	  (setq verilog-str (verilog-build-defun-re verilog-str)))
	(goto-char (point-min))

	;; Build a list of all possible completions
	(while (verilog-re-search-forward verilog-str nil t)
	  (setq match (buffer-substring (match-beginning 2) (match-end 2)))
	  (if (or (null verilog-pred)
		  (funcall verilog-pred match))
	      (setq verilog-all (cons match verilog-all)))))

      ;; Now we have built a list of all matches. Give response to caller
      (verilog-completion-response))))

(defun verilog-goto-defun ()
  "Move to specified Verilog module/task/function.
The default is a name found in the buffer around point.
If search fails, other files are checked based on
`verilog-library-directories', `verilog-library-files',
and `verilog-library-extensions'."
  (interactive)
  (let* ((default (verilog-get-default-symbol))
	 ;; The following variable is used in verilog-comp-function
	 (verilog-buffer-to-use (current-buffer))
	 (label (if (not (string= default ""))
		    ;; Do completion with default
		    (completing-read (concat "Label: (default " default ") ")
				     'verilog-comp-defun nil nil "")
		  ;; There is no default value. Complete without it
		  (completing-read "Label: "
				   'verilog-comp-defun nil nil "")))
	 pt)
    ;; If there was no response on prompt, use default value
    (if (string= label "")
	(setq label default))
    ;; Goto right place in buffer if label is not an empty string
    (or (string= label "")
	(progn
	  (save-excursion
	    (goto-char (point-min))
	    (setq pt (re-search-forward (verilog-build-defun-re label t) nil t)))
	  (when pt
	    (goto-char pt)
	    (beginning-of-line))
	  pt)
	(verilog-goto-defun-file label)
	)))

;; Eliminate compile warning
(eval-when-compile
  (if (not (boundp 'occur-pos-list))
      (defvar occur-pos-list nil "Backward compatibility occur positions.")))

(defun verilog-showscopes ()
  "List all scopes in this module."
  (interactive)
  (let ((buffer (current-buffer))
	(linenum 1)
	(nlines 0)
	(first 1)
	(prevpos (point-min))
        (final-context-start (make-marker))
	(regexp "\\(module\\s-+\\w+\\s-*(\\)\\|\\(\\w+\\s-+\\w+\\s-*(\\)")
	)
    (with-output-to-temp-buffer "*Occur*"
      (save-excursion
	(message (format "Searching for %s ..." regexp))
	;; Find next match, but give up if prev match was at end of buffer.
	(while (and (not (= prevpos (point-max)))
		    (verilog-re-search-forward regexp nil t))
	  (goto-char (match-beginning 0))
	  (beginning-of-line)
	  (save-match-data
            (setq linenum (+ linenum (count-lines prevpos (point)))))
	  (setq prevpos (point))
	  (goto-char (match-end 0))
	  (let* ((start (save-excursion
			  (goto-char (match-beginning 0))
			  (forward-line (if (< nlines 0) nlines (- nlines)))
			  (point)))
		 (end (save-excursion
			(goto-char (match-end 0))
			(if (> nlines 0)
			    (forward-line (1+ nlines))
			    (forward-line 1))
			(point)))
		 (tag (format "%3d" linenum))
		 (empty (make-string (length tag) ?\ ))
		 tem)
	    (save-excursion
	      (setq tem (make-marker))
	      (set-marker tem (point))
	      (set-buffer standard-output)
	      (setq occur-pos-list (cons tem occur-pos-list))
	      (or first (zerop nlines)
		  (insert "--------\n"))
	      (setq first nil)
	      (insert-buffer-substring buffer start end)
	      (backward-char (- end start))
	      (setq tem (if (< nlines 0) (- nlines) nlines))
	      (while (> tem 0)
		(insert empty ?:)
		(forward-line 1)
		(setq tem (1- tem)))
	      (let ((this-linenum linenum))
		(set-marker final-context-start
			    (+ (point) (- (match-end 0) (match-beginning 0))))
		(while (< (point) final-context-start)
		  (if (null tag)
		      (setq tag (format "%3d" this-linenum)))
		  (insert tag ?:)))))))
      (set-buffer-modified-p nil))))


;; Highlight helper functions
(defconst verilog-directive-regexp "\\(translate\\|coverage\\|lint\\)_")
(defun verilog-within-translate-off ()
  "Return point if within translate-off region, else nil."
  (and (save-excursion
	 (re-search-backward
	  (concat "//\\s-*.*\\s-*" verilog-directive-regexp "\\(on\\|off\\)\\>")
	  nil t))
       (equal "off" (match-string 2))
       (point)))

(defun verilog-start-translate-off (limit)
  "Return point before translate-off directive if before LIMIT, else nil."
  (when (re-search-forward
	  (concat "//\\s-*.*\\s-*" verilog-directive-regexp "off\\>")
	  limit t)
    (match-beginning 0)))

(defun verilog-back-to-start-translate-off (limit)
  "Return point before translate-off directive if before LIMIT, else nil."
  (when (re-search-backward
	  (concat "//\\s-*.*\\s-*" verilog-directive-regexp "off\\>")
	  limit t)
    (match-beginning 0)))

(defun verilog-end-translate-off (limit)
  "Return point after translate-on directive if before LIMIT, else nil."

  (re-search-forward (concat
		      "//\\s-*.*\\s-*" verilog-directive-regexp "on\\>") limit t))

(defun verilog-match-translate-off (limit)
  "Match a translate-off block, setting `match-data' and returning t, else nil.
Bound search by LIMIT."
  (when (< (point) limit)
    (let ((start (or (verilog-within-translate-off)
		     (verilog-start-translate-off limit)))
	  (case-fold-search t))
      (when start
	(let ((end (or (verilog-end-translate-off limit) limit)))
	  (set-match-data (list start end))
	  (goto-char end))))))

(defun verilog-font-lock-match-item (limit)
  "Match, and move over, any declaration item after point.
Bound search by LIMIT.  Adapted from
`font-lock-match-c-style-declaration-item-and-skip-to-next'."
  (condition-case nil
      (save-restriction
	(narrow-to-region (point-min) limit)
	;; match item
	(when (looking-at "\\s-*\\([a-zA-Z]\\w*\\)")
	  (save-match-data
	    (goto-char (match-end 1))
	    ;; move to next item
	    (if (looking-at "\\(\\s-*,\\)")
		(goto-char (match-end 1))
	      (end-of-line) t))))
    (error t)))


;; Added by Subbu Meiyappan for Header

(defun verilog-header ()
  "Insert a standard Verilog file header."
  (interactive)
  (let ((start (point)))
  (insert "\
//-----------------------------------------------------------------------------
// Title         : <title>
// Project       : <project>
//-----------------------------------------------------------------------------
// File          : <filename>
// Author        : <author>
// Created       : <credate>
// Last modified : <moddate>
//-----------------------------------------------------------------------------
// Description :
// <description>
//-----------------------------------------------------------------------------
// Copyright (c) <copydate> by <company> This model is the confidential and
// proprietary property of <company> and the possession or use of this
// file requires a written license from <company>.
//------------------------------------------------------------------------------
// Modification history :
// <modhist>
//-----------------------------------------------------------------------------

")
    (goto-char start)
    (search-forward "<filename>")
    (replace-match (buffer-name) t t)
    (search-forward "<author>") (replace-match "" t t)
    (insert (user-full-name))
    (insert "  <" (user-login-name) "@" (system-name) ">")
    (search-forward "<credate>") (replace-match "" t t)
    (insert-date)
    (search-forward "<moddate>") (replace-match "" t t)
    (insert-date)
    (search-forward "<copydate>") (replace-match "" t t)
    (insert-year)
    (search-forward "<modhist>") (replace-match "" t t)
    (insert-date)
    (insert " : created")
    (goto-char start)
    (let (string)
      (setq string (read-string "title: "))
      (search-forward "<title>")
      (replace-match string t t)
      (setq string (read-string "project: " verilog-project))
      (make-variable-buffer-local 'verilog-project)
      (setq verilog-project string)
      (search-forward "<project>")
      (replace-match string t t)
      (setq string (read-string "Company: " verilog-company))
      (make-variable-buffer-local 'verilog-company)
      (setq verilog-company string)
      (search-forward "<company>")
      (replace-match string t t)
      (search-forward "<company>")
      (replace-match string t t)
      (search-forward "<company>")
      (replace-match string t t)
      (search-backward "<description>")
      (replace-match "" t t)
      )))

;; verilog-header Uses the insert-date function

(defun insert-date ()
  "Insert date from the system."
  (interactive)
  (let ((timpos))
    (setq timpos (point))
    (if verilog-date-scientific-format
	(shell-command  "date \"+@%Y/%m/%d\"" t)
      (shell-command  "date \"+@%d.%m.%Y\"" t))
    (search-forward "@")
    (delete-region timpos (point))
    (end-of-line))
    (delete-char 1))

(defun insert-year ()
  "Insert year from the system."
  (interactive)
  (let ((timpos))
    (setq timpos (point))
    (shell-command  "date \"+@%Y\"" t)
    (search-forward "@")
    (delete-region timpos (point))
    (end-of-line))
  (delete-char 1))


;;
;; Signal list parsing
;;

(defsubst verilog-modi-get-decls (modi)
  (verilog-modi-cache-results modi 'verilog-read-decls))

(defsubst verilog-modi-get-sub-decls (modi)
  (verilog-modi-cache-results modi 'verilog-read-sub-decls))

;; Signal reading for given module
;; Note these all take modi's - as returned from the verilog-modi-current function
(defsubst verilog-modi-get-outputs (modi)
  (aref (verilog-modi-get-decls modi) 0))
(defsubst verilog-modi-get-inouts (modi)
  (aref (verilog-modi-get-decls modi) 1))
(defsubst verilog-modi-get-inputs (modi)
  (aref (verilog-modi-get-decls modi) 2))
(defsubst verilog-modi-get-wires (modi)
  (aref (verilog-modi-get-decls modi) 3))
(defsubst verilog-modi-get-regs (modi)
  (aref (verilog-modi-get-decls modi) 4))
(defsubst verilog-modi-get-assigns (modi)
  (aref (verilog-modi-get-decls modi) 5))
(defsubst verilog-modi-get-consts (modi)
  (aref (verilog-modi-get-decls modi) 6))
(defsubst verilog-modi-get-sub-outputs (modi)
  (aref (verilog-modi-get-sub-decls modi) 0))
(defsubst verilog-modi-get-sub-inouts (modi)
  (aref (verilog-modi-get-sub-decls modi) 1))
(defsubst verilog-modi-get-sub-inputs (modi)
  (aref (verilog-modi-get-sub-decls modi) 2))

;; Elements of a signal list
(defsubst verilog-sig-name (sig)
  (car sig))
(defsubst verilog-sig-bits (sig)
  (nth 1 sig))
(defsubst verilog-sig-comment (sig)
  (nth 2 sig))
(defsubst verilog-sig-memory (sig)
  (nth 3 sig))
(defsubst verilog-sig-enum (sig)
  (nth 4 sig))
(defsubst verilog-sig-signed (sig)
  (nth 5 sig))
(defsubst verilog-sig-width (sig)
  (verilog-make-width-expression (verilog-sig-bits sig)))

(defsubst verilog-alw-get-inputs (sigs)
  (nth 2 sigs))
(defsubst verilog-alw-get-outputs (sigs)
  (nth 0 sigs))
(defsubst verilog-alw-get-uses-delayed (sigs)
  (nth 3 sigs))


(defun verilog-signals-not-in (in-list not-list)
  "Return list of signals in IN-LIST that aren't also in NOT-LIST.
Signals must be in standard (base vector) form."
  (let (out-list)
    (while in-list
      (if (not (assoc (car (car in-list)) not-list))
	  (setq out-list (cons (car in-list) out-list)))
      (setq in-list (cdr in-list)))
    (nreverse out-list)))
;;(verilog-signals-not-in '(("A" "") ("B" "") ("DEL" "[2:3]")) '(("DEL" "") ("EXT" "")))

(defun verilog-signals-in (in-list other-list)
  "Return list of signals in IN-LIST that are also in other-list.
Signals must be in standard (base vector) form."
  (let (out-list)
    (while in-list
      (if (assoc (car (car in-list)) other-list)
	  (setq out-list (cons (car in-list) out-list)))
      (setq in-list (cdr in-list)))
    (nreverse out-list)))
;;(verilog-signals-in '(("A" "") ("B" "") ("DEL" "[2:3]")) '(("DEL" "") ("EXT" "")))

(defun verilog-signals-memory (in-list)
  "Return list of signals in IN-LIST that are memoried (multidimensional)."
  (let (out-list)
    (while in-list
      (if (nth 3 (car in-list))
	  (setq out-list (cons (car in-list) out-list)))
      (setq in-list (cdr in-list)))
    out-list))
;;(verilog-signals-memory '(("A" nil nil "[3:0]")) '(("B" nil nil nil)))

(defun verilog-signals-sort-compare (a b)
  "Compare signal A and B for sorting."
  (string< (car a) (car b)))

(defun verilog-signals-not-params (in-list)
  "Return list of signals in IN-LIST that aren't parameters or
numeric constants."
  (let (out-list)
    (while in-list
      (unless (boundp (intern (concat "vh-" (car (car in-list)))))
	(setq out-list (cons (car in-list) out-list)))
      (setq in-list (cdr in-list)))
    (nreverse out-list)))

(defun verilog-signals-combine-bus (in-list)
  "Return a list of signals in IN-LIST, with busses combined.
Duplicate signals are also removed.  For example A[2] and A[1] become A[2:1]."
  (let (combo
	out-list
	sig highbit lowbit		; Temp information about current signal
	sv-name sv-highbit sv-lowbit	; Details about signal we are forming
	sv-comment sv-memory sv-enum sv-signed sv-busstring
	bus)
    ;; Shove signals so duplicated signals will be adjacent
    (setq in-list (sort in-list `verilog-signals-sort-compare))
    (while in-list
      (setq sig (car in-list))
      ;; No current signal; form from existing details
      (unless sv-name
	(setq sv-name    (verilog-sig-name sig)
	      sv-highbit nil
	      sv-busstring nil
	      sv-comment (verilog-sig-comment sig)
	      sv-memory  (verilog-sig-memory sig)
	      sv-enum    (verilog-sig-enum sig)
	      sv-signed  (verilog-sig-signed sig)
	      combo ""
	      ))
      ;; Extract bus details
      (setq bus (verilog-sig-bits sig))
      (cond ((and bus
		  (or (and (string-match "\\[\\([0-9]+\\):\\([0-9]+\\)\\]" bus)
			   (setq highbit (string-to-int (match-string 1 bus))
				 lowbit  (string-to-int (match-string 2 bus))))
		      (and (string-match "\\[\\([0-9]+\\)\\]" bus)
			   (setq highbit (string-to-int (match-string 1 bus))
				 lowbit  highbit))))
	     ;; Combine bits in bus
	     (if sv-highbit
		 (setq sv-highbit (max highbit sv-highbit)
		       sv-lowbit  (min lowbit  sv-lowbit))
	       (setq sv-highbit highbit
		     sv-lowbit  lowbit)))
	    (bus
	     ;; String, probably something like `preproc:0
	     (setq sv-busstring bus)))
      ;; Peek ahead to next signal
      (setq in-list (cdr in-list))
      (setq sig (car in-list))
      (cond ((and sig (equal sv-name (verilog-sig-name sig)))
	     ;; Combine with this signal
	     (if (and sv-busstring (not (equal sv-busstring (verilog-sig-bits sig))))
		 (message (concat "Warning, can't merge into single bus "
				  sv-name bus
				  ", the AUTOs may be wrong")))
	     (if (verilog-sig-comment sig) (setq combo ", ..."))
	     (setq sv-memory (or sv-memory (verilog-sig-memory sig))
		   sv-enum   (or sv-enum   (verilog-sig-enum sig))
		   sv-signed (or sv-signed (verilog-sig-signed sig))))
	    ;; Doesn't match next signal, add to que, zero in prep for next
	    ;; Note sig may also be nil for the last signal in the list
	    (t
	     (setq out-list
		   (cons (list sv-name
			       (or sv-busstring
				   (if sv-highbit
				       (concat "[" (int-to-string sv-highbit) ":" (int-to-string sv-lowbit) "]")))
			       (concat sv-comment combo)
			       sv-memory sv-enum sv-signed)
			 out-list)
		   sv-name nil)))
      )
    ;;
    out-list))

;;
;; Port/Wire/Etc Reading
;;

(defun verilog-read-inst-module ()
  "Return module_name when point is inside instantiation."
  (save-excursion
    (verilog-backward-open-paren)
    ;; Skip over instantiation name
    (verilog-re-search-backward-quick "\\b[a-zA-Z0-9`_\$]" nil nil)
    (skip-chars-backward "a-zA-Z0-9`_$")
    (verilog-re-search-backward-quick "\\(\\b[a-zA-Z0-9`_\$]\\|)\\)" nil nil)  ; ) isn't word boundary
    ;; Check for parameterized instantiations
    (when (looking-at ")")
      (verilog-backward-open-paren)
      (verilog-re-search-backward-quick "\\b[a-zA-Z0-9`_\$]" nil nil))
    (skip-chars-backward "a-zA-Z0-9'_$")
    (looking-at "[a-zA-Z0-9`_\$]+")
    ;; Important: don't use match string, this must work with emacs 19 font-lock on
    (buffer-substring-no-properties (match-beginning 0) (match-end 0))))

(defun verilog-read-inst-name ()
  "Return instance_name when point is inside instantiation."
  (save-excursion
    (verilog-backward-open-paren)
    (verilog-re-search-backward-quick "\\b[a-zA-Z0-9`_\$]" nil nil)
    (skip-chars-backward "a-zA-Z0-9`_$")
    (looking-at "[a-zA-Z0-9`_\$]+")
    ;; Important: don't use match string, this must work with emacs 19 font-lock on
    (buffer-substring-no-properties (match-beginning 0) (match-end 0))))

(defun verilog-read-module-name ()
  "Return module name when after its ( or ;."
  (save-excursion
    (re-search-backward "[(;]")
    (verilog-re-search-backward-quick "\\b[a-zA-Z0-9`_\$]" nil nil)
    (skip-chars-backward "a-zA-Z0-9`_$")
    (looking-at "[a-zA-Z0-9`_\$]+")
    ;; Important: don't use match string, this must work with emacs 19 font-lock on
    (buffer-substring-no-properties (match-beginning 0) (match-end 0))))

(defun verilog-read-auto-params (num-param &optional max-param)
  "Return parameter list inside auto.
Optional NUM-PARAM and MAX-PARAM check for a specific number of parameters."
  (let ((olist))
    (save-excursion
      ;; /*AUTOPUNT("parameter", "parameter")*/
      (search-backward "(")
      (while (looking-at "(?\\s *\"\\([^\"]*\\)\"\\s *,?")
	(setq olist (cons (match-string 1) olist))
	(goto-char (match-end 0))))
    (or (eq nil num-param)
	(<= num-param (length olist))
	(error "%s: Expected %d parameters" (verilog-point-text) num-param))
    (if (eq max-param nil) (setq max-param num-param))
    (or (eq nil max-param)
	(>= max-param (length olist))
	(error "%s: Expected <= %d parameters" (verilog-point-text) max-param))
    (nreverse olist)))

(defun verilog-read-decls ()
  "Compute signal declaration information for the current module at point.
Return a array of [outputs inouts inputs wire reg assign const]."
  (let ((end-mod-point (or (verilog-get-end-of-defun t) (point-max)))
	(functask 0) (paren 0)
	sigs-in sigs-out sigs-inout sigs-wire sigs-reg sigs-assign sigs-const
	vec expect-signal keywd newsig rvalue enum io signed)
    (save-excursion
      (verilog-beg-of-defun)
      (setq sigs-const (verilog-read-auto-constants (point) end-mod-point))
      (while (< (point) end-mod-point)
	;;(if dbg (setq dbg (cons (format "Pt %s  Vec %s   Kwd'%s'\n" (point) vec keywd) dbg)))
	(cond
	 ((looking-at "//")
	  (if (looking-at "[^\n]*synopsys\\s +enum\\s +\\([a-zA-Z0-9_]+\\)")
	      (setq enum (match-string 1)))
	  (search-forward "\n"))
	 ((looking-at "/\\*")
	  (forward-char 2)
	  (if (looking-at "[^*]*synopsys\\s +enum\\s +\\([a-zA-Z0-9_]+\\)")
	      (setq enum (match-string 1)))
	  (or (search-forward "*/")
	      (error "%s: Unmatched /* */, at char %d" (verilog-point-text) (point))))
	 ((eq ?\" (following-char))
	  (or (re-search-forward "[^\\]\"" nil t)	;; don't forward-char first, since we look for a non backslash first
	      (error "%s: Unmatched quotes, at char %d" (verilog-point-text) (point))))
	 ((eq ?\; (following-char))
	  (setq vec nil  io nil  expect-signal nil  newsig nil  paren 0  rvalue nil)
	  (forward-char 1))
	 ((eq ?= (following-char))
	  (setq rvalue t newsig nil)
	  (forward-char 1))
	 ((and rvalue
	       (cond ((and (eq ?, (following-char))
			   (eq paren 0))
		      (setq rvalue nil)
		      (forward-char 1)
		      t)
		     ;; ,'s can occur inside {} & funcs
		     ((looking-at "[{(]")
		      (setq paren (1+ paren))
		      (forward-char 1)
		      t)
		     ((looking-at "[})]")
		      (setq paren (1- paren))
		      (forward-char 1)
		      t)
		     )))
	 ((looking-at "\\s-*\\(\\[[^]]+\\]\\)")
	  (goto-char (match-end 0))
	  (cond (newsig	; Memory, not just width.  Patch last signal added's memory (nth 3)
		 (setcar (cdr (cdr (cdr newsig))) (match-string 1)))
		(t ;; Bit width
		 (setq vec (verilog-string-replace-matches
			    "\\s-+" "" nil nil (match-string 1))))))
	 ;; Normal or escaped identifier -- note we remember the \ if escaped
	 ((looking-at "\\s-*\\([a-zA-Z0-9`_$]+\\|\\\\[^ \t\n]+\\)")
	  (goto-char (match-end 0))
	  (setq keywd (match-string 1))
	  (when (string-match "^\\\\" keywd)
	    (setq keywd (concat keywd " ")))  ;; Escaped ID needs space at end
	  (cond ((equal keywd "input")
		 (setq vec nil enum nil  newsig nil  signed nil  io t  expect-signal 'sigs-in))
		((equal keywd "output")
		 (setq vec nil enum nil  newsig nil  signed nil  io t  expect-signal 'sigs-out))
		((equal keywd "inout")
		 (setq vec nil enum nil  newsig nil  signed nil  io t  expect-signal 'sigs-inout))
 		((or (equal keywd "wire")
 		     (equal keywd "tri"))
		 (unless io (setq vec nil  enum nil  signed nil  expect-signal 'sigs-wire)))
 		((or (equal keywd "reg")
 		     (equal keywd "trireg"))
		 (unless io (setq vec nil  enum nil  signed nil  expect-signal 'sigs-reg)))
		((equal keywd "assign")
		 (setq vec nil  enum nil  signed nil  expect-signal 'sigs-assign))
		((or (equal keywd "supply0")
		     (equal keywd "supply1")
		     (equal keywd "supply")
		     (equal keywd "parameter")
		     (equal keywd "localparam"))
		 (unless io (setq vec nil  enum nil  signed nil  expect-signal 'sigs-const)))
		((equal keywd "signed")
		 (setq signed "signed"))
		((or (equal keywd "function")
		     (equal keywd "task"))
		 (setq functask (1+ functask)))
		((or (equal keywd "endfunction")
		     (equal keywd "endtask"))
		 (setq functask (1- functask)))
		((and expect-signal
		      (eq functask 0)
		      (not (member keywd verilog-keywords))
		      (not rvalue))
		 ;; Add new signal to expect-signal's variable
		 (setq newsig (list keywd vec nil nil enum signed))
		 (set expect-signal (cons newsig
					  (symbol-value expect-signal))))))
	 (t
	  (forward-char 1)))
	(skip-syntax-forward " "))
      ;; Return arguments
      (vector (nreverse sigs-out)
	      (nreverse sigs-inout)
	      (nreverse sigs-in)
	      (nreverse sigs-wire)
	      (nreverse sigs-reg)
	      (nreverse sigs-assign)
	      (nreverse sigs-const)
	      ))))

(defvar sigs-in nil) ; Prevent compile warning
(defvar sigs-inout nil) ; Prevent compile warning
(defvar sigs-out nil) ; Prevent compile warning

(defun verilog-read-sub-decls-line (submodi comment)
  "For read-sub-decl, read lines of port defs until none match anymore.
Return the list of signals found, using submodi to look up each port."
  (let (done port portdata sig vec)
    (save-excursion
      (forward-line 1)
      (while (not done)
	;; Get port name
	(cond ((looking-at "\\s-*\\.\\s-*\\([a-zA-Z0-9`_$]*\\)\\s-*(\\s-*")
	       (setq port (match-string 1))
	       (goto-char (match-end 0)))
	      ((looking-at "\\s-*\\.\\s-*\\(\\\\[^ \t\n]*\\)\\s-*(\\s-*")
	       (setq port (concat (match-string 1) " ")) ;; escaped id's need trailing space
	       (goto-char (match-end 0)))
	      ((looking-at "\\s-*\\.[^(]*(")
	       (setq port nil) ;; skip this line
	       (goto-char (match-end 0)))
	      (t
	       (setq port nil  done t))) ;; Unknown, ignore rest of line
	;; Get signal name
	(when port
	  (cond ((looking-at "\\(\\\\[^ \t\n]*\\)\\s-*)")
		 (setq sig (concat (match-string 1) " ") ;; escaped id's need trailing space
		       vec nil))
		; We intentionally ignore (non-escaped) signals with .s in them
		; this prevents AUTOWIRE etc from noticing hierarchical sigs.
		((looking-at "\\([^[({).]*\\)\\s-*)")
		 (setq sig (verilog-string-remove-spaces (match-string 1))
		       vec nil))
		((looking-at "\\([^[({).]*\\)\\s-*\\(\\[[^]]+\\]\\)\\s-*)")
		 (setq sig (verilog-string-remove-spaces (match-string 1))
		       vec (verilog-symbol-detick-denumber (match-string 2))))
		(t
		 (setq sig nil)))
	  ;; Process signals
	  (when sig
	    (setq port (verilog-symbol-detick-denumber port))
	    (setq sig  (verilog-symbol-detick-denumber sig))
	    (unless (or (not sig)
			(equal sig ""))  ;; Ignore .foo(1'b1) assignments
	      (cond ((setq portdata (assoc port (verilog-modi-get-inouts submodi)))
		     (setq sigs-inout (cons (list sig vec (concat "To/From " comment) nil nil
		   				  (verilog-sig-signed portdata)) sigs-inout)))
		    ((setq portdata (assoc port (verilog-modi-get-outputs submodi)))
		     (setq sigs-out   (cons (list sig vec (concat "From " comment) nil nil
						  (verilog-sig-signed portdata)) sigs-out)))
		    ((setq portdata (assoc port (verilog-modi-get-inputs submodi)))
		     (setq sigs-in    (cons (list sig vec (concat "To " comment) nil nil
						  (verilog-sig-signed portdata)) sigs-in)))
		    ;; (t  -- warning pin isn't defined.)   ; Leave for lint tool
		    )
	      )))
	;;
	(forward-line 1)))))

(defun verilog-read-sub-decls ()
  "Parse signals going to modules under this module.
Return a array of [ outputs inouts inputs ] signals for modules that are
instantiated in this module.  For example if declare A A (.B(SIG)) and SIG
is a output, then SIG will be included in the list.

This only works on instantiations created with /*AUTOINST*/ converted by
\\[verilog-auto-instant].  Otherwise, it would have to read in the whole
component library to determine connectivity of the design."
  (save-excursion
    (let ((end-mod-point (verilog-get-end-of-defun t))
	  st-point end-inst-point
	  ;; below 3 modified by verilog-read-sub-decls-line
	  sigs-out sigs-inout sigs-in)
      (verilog-beg-of-defun)
      (while (search-forward "/*AUTOINST*/" end-mod-point t)
	(save-excursion
	  (goto-char (match-beginning 0))
	  (unless (verilog-inside-comment-p)
	    ;; Attempt to snarf a comment
	    (let* ((submod (verilog-read-inst-module))
		   (inst (verilog-read-inst-name))
		   (comment (concat inst " of " submod ".v")) submodi)
	      (when (setq submodi (verilog-modi-lookup submod t))
		;; This could have used a list created by verilog-auto-instant
		;; However I want it to be runnable even on user's manually added signals
		(verilog-backward-open-paren)
		(setq end-inst-point (save-excursion (forward-sexp 1) (point))
		      st-point (point))
		(while (re-search-forward "^\\s *// Outputs" end-inst-point t)
		  (verilog-read-sub-decls-line submodi comment)) ;; Modifies sigs-out
		(goto-char st-point)
		(while (re-search-forward "\\s *// Inouts" end-inst-point t)
		  (verilog-read-sub-decls-line submodi comment)) ;; Modifies sigs-inout
		(goto-char st-point)
		(while (re-search-forward "\\s *// Inputs" end-inst-point t)
		  (verilog-read-sub-decls-line submodi comment)) ;; Modifies sigs-in
		)))))
      ;; Combine duplicate bits
      ;;(setq rr (vector sigs-out sigs-inout sigs-in))
      (vector (verilog-signals-combine-bus (nreverse sigs-out))
	      (verilog-signals-combine-bus (nreverse sigs-inout))
	      (verilog-signals-combine-bus (nreverse sigs-in))))))

(defun verilog-read-inst-pins ()
  "Return a array of [ pins ] for the current instantiation at point.
For example if declare A A (.B(SIG)) then B will be included in the list."
  (save-excursion
    (let ((end-mod-point (point))	;; presume at /*AUTOINST*/ point
	  pins pin)
      (verilog-backward-open-paren)
      (while (re-search-forward "\\.\\([^( \t\n]*\\)\\s-*(" end-mod-point t)
	(setq pin (match-string 1))
	(unless (verilog-inside-comment-p)
	  (setq pins (cons (list pin) pins))))
      (vector pins))))

(defun verilog-read-arg-pins ()
  "Return a array of [ pins ] for the current argument declaration at point."
  (save-excursion
    (let ((end-mod-point (point))	;; presume at /*AUTOARG*/ point
	  pins pin)
      (verilog-backward-open-paren)
      (while (re-search-forward "\\([a-zA-Z0-9$_.%`]+\\)" end-mod-point t)
	(setq pin (match-string 1))
	(unless (verilog-inside-comment-p)
	  (setq pins (cons (list pin) pins))))
      (vector pins))))

(defun verilog-read-auto-constants (beg end-mod-point)
  "Return a list of AUTO_CONSTANTs used in the region from BEG to END-MOD-POINT."
  ;; Insert new
  (save-excursion
    (let (sig-list tpl-end-pt)
      (goto-char beg)
      (while (re-search-forward "\\<AUTO_CONSTANT" end-mod-point t)
	(if (not (looking-at "\\s *("))
	    (error "%s: Missing () after AUTO_CONSTANT" (verilog-point-text)))
	(search-forward "(" end-mod-point)
	(setq tpl-end-pt (save-excursion
			   (backward-char 1)
			   (forward-sexp 1)   ;; Moves to paren that closes argdecl's
			   (backward-char 1)
			   (point)))
	(while (re-search-forward "\\s-*\\([\"a-zA-Z0-9$_.%`]+\\)\\s-*,*" tpl-end-pt t)
	  (setq sig-list (cons (list (match-string 1) nil nil) sig-list))))
      sig-list)))

(defun verilog-read-auto-lisp (start end)
  "Look for and evaluate a AUTO_LISP between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\<AUTO_LISP(" end t)
      (backward-char)
      (let* ((beg-pt (prog1 (point)
		       (forward-sexp 1)))	;; Closing paren
	     (end-pt (point)))
	(eval-region beg-pt end-pt nil)))))

(eval-when-compile
  ;; These are passed in a let, not global
  (if (not (boundp 'sigs-in))
      (defvar sigs-in nil) (defvar sigs-out nil)
      (defvar got-sig nil) (defvar got-rvalue nil) (defvar uses-delayed nil)))

(defun verilog-read-always-signals-recurse
  (exit-keywd rvalue ignore-next)
  "Recursive routine for parentheses/bracket matching.
EXIT-KEYWD is expression to stop at, nil if top level.
RVALUE is true if at right hand side of equal.
IGNORE-NEXT is true to ignore next token, fake from inside case statement."
  (let* ((semi-rvalue (equal "endcase" exit-keywd)) ;; true if after a ; we are looking for rvalue
	 keywd last-keywd sig-tolk sig-last-tolk gotend end-else-check)
    ;;(if dbg (setq dbg (concat dbg (format "Recursion %S %S %S\n" exit-keywd rvalue ignore-next))))
    (while (not (or (eobp) gotend))
      (cond
       ((looking-at "//")
	(search-forward "\n"))
       ((looking-at "/\\*")
	(or (search-forward "*/")
	    (error "%s: Unmatched /* */, at char %d" (verilog-point-text) (point))))
       (t (setq keywd (buffer-substring-no-properties
		       (point)
		       (save-excursion (when (eq 0 (skip-chars-forward "a-zA-Z0-9$_.%`"))
					 (forward-char 1))
				       (point)))
		sig-last-tolk sig-tolk
		sig-tolk nil)
	  ;;(if dbg (setq dbg (concat dbg (format "\tPt %S %S\t%S %S %S\n" (point) keywd rvalue ignore-next end-else-check))))
	  (cond
	   ((equal keywd "\"")
	    (or (re-search-forward "[^\\]\"" nil t)
		(error "%s: Unmatched quotes, at char %d" (verilog-point-text) (point))))
	   ;; else at top level loop, keep parsing
	   ((and end-else-check (equal keywd "else"))
	    ;;(if dbg (setq dbg (concat dbg (format "\tif-check-else %s\n" keywd))))
	    ;; no forward movement, want to see else in lower loop
	    (setq end-else-check nil))
	   ;; End at top level loop
	   ((and end-else-check (looking-at "[^ \t\n]"))
	    ;;(if dbg (setq dbg (concat dbg (format "\tif-check-else-other %s\n" keywd))))
	    (setq gotend t))
	   ;; Final statement?
	   ((and exit-keywd (equal keywd exit-keywd))
	    (setq gotend t)
	    (forward-char (length keywd)))
	   ;; Standard tokens...
	   ((equal keywd ";")
	    (setq ignore-next nil rvalue semi-rvalue)
	    ;; Final statement at top level loop?
	    (when (not exit-keywd)
	      ;;(if dbg (setq dbg (concat dbg (format "\ttop-end-check %s\n" keywd))))
	      (setq end-else-check t))
	    (forward-char 1))
	   ((equal keywd "'")
	    (if (looking-at "'s?[hdxbo][0-9a-fA-F_xz? \t]*")
		(goto-char (match-end 0))
	      (forward-char 1)))
	   ((equal keywd ":")	;; Case statement, begin/end label, x?y:z
	    (cond ((equal "endcase" exit-keywd)  ;; case x: y=z; statement next
		   (setq ignore-next nil rvalue nil))
		  ((not rvalue)	;; begin label
		   (setq ignore-next t rvalue nil)))
	    (forward-char 1))
	   ((equal keywd "=")
	    (if (eq (char-before) ?< )
		(setq uses-delayed 1))
	    (setq ignore-next nil rvalue t)
	    (forward-char 1))
	   ((equal keywd "?")
	    (forward-char 1)
	    (verilog-read-always-signals-recurse ":" rvalue nil))
	   ((equal keywd "[")
	    (forward-char 1)
	    (verilog-read-always-signals-recurse "]" t nil))
	   ((equal keywd "(")
	    (forward-char 1)
	    (cond (sig-last-tolk	;; Function call; zap last signal
		   (setq got-sig nil)))
	    (cond ((equal last-keywd "for")
		   (verilog-read-always-signals-recurse ";" nil nil)
		   (verilog-read-always-signals-recurse ";" t nil)
		   (verilog-read-always-signals-recurse ")" nil nil))
		  (t (verilog-read-always-signals-recurse ")" t nil))))
	   ((equal keywd "begin")
	    (skip-syntax-forward "w_")
	    (verilog-read-always-signals-recurse "end" nil nil)
	    ;;(if dbg (setq dbg (concat dbg (format "\tgot-end %s\n" exit-keywd))))
	    (setq ignore-next nil rvalue semi-rvalue)
	    (if (not exit-keywd) (setq end-else-check t)))
	   ((or (equal keywd "case")
		(equal keywd "casex")
		(equal keywd "casez"))
	    (skip-syntax-forward "w_")
	    (verilog-read-always-signals-recurse "endcase" t nil)
	    (setq ignore-next nil rvalue semi-rvalue)
	    (if (not exit-keywd) (setq gotend t)))	;; top level begin/end
	   ((string-match "^[$`a-zA-Z_]" keywd)	;; not exactly word constituent
	    (cond ((equal keywd "`ifdef")
		   (setq ignore-next t))
		  ((or ignore-next
		       (member keywd verilog-keywords)
		       (string-match "^\\$" keywd))	;; PLI task
		   (setq ignore-next nil))
		  (t
		   (setq keywd (verilog-symbol-detick-denumber keywd))
		   (if got-sig (if got-rvalue
				   (setq sigs-in (cons got-sig sigs-in))
				 (setq sigs-out (cons got-sig sigs-out))))
		   (setq got-rvalue rvalue
			 got-sig (if (or (not keywd)
					 (assoc keywd (if got-rvalue sigs-in sigs-out)))
				     nil (list keywd nil nil))
			 sig-tolk t)))
	    (skip-chars-forward "a-zA-Z0-9$_.%`"))
	   (t
	    (forward-char 1)))
	  ;; End of non-comment token
	  (setq last-keywd keywd)
	  ))
      (skip-syntax-forward " "))
    ;;(if dbg (setq dbg (concat dbg (format "ENDRecursion %s\n" exit-keywd))))
    ))

(defun verilog-read-always-signals ()
  "Parse always block at point and return list of (outputs inout inputs)."
  ;; Insert new
  (save-excursion
    (let* (;;(dbg "")
	   sigs-in sigs-out
	   got-sig got-rvalue
	   uses-delayed)	;; Found signal/rvalue; push if not function
      (search-forward ")")
      (verilog-read-always-signals-recurse nil nil nil)
      ;; Return what was found
      (if got-sig (if got-rvalue
		      (setq sigs-in (cons got-sig sigs-in))
		    (setq sigs-out (cons got-sig sigs-out))))
      ;;(if dbg (message dbg))
      (list sigs-out nil sigs-in uses-delayed))))

(defun verilog-read-instants ()
  "Parse module at point and return list of ( ( file instance ) ... )."
  (verilog-beg-of-defun)
  (let* ((end-mod-point (verilog-get-end-of-defun t))
	 (state nil)
	 (instants-list nil))
    (save-excursion
      (while (< (point) end-mod-point)
	;; Stay at level 0, no comments
	(while (progn
		 (setq state (parse-partial-sexp (point) end-mod-point 0 t nil))
		 (or (> (car state) 0)	; in parens
		     (nth 5 state)		; comment
		     ))
	  (forward-line 1))
	(beginning-of-line)
	(if (looking-at "^\\s-*\\([a-zA-Z0-9`_$]+\\)\\s-+\\([a-zA-Z0-9`_$]+\\)\\s-*(")
	    ;;(if (looking-at "^\\(.+\\)$")
	    (let ((module (match-string 1))
		  (instant (match-string 2)))
	      (if (not (member module verilog-keywords))
		  (setq instants-list (cons (list module instant) instants-list)))))
	(forward-line 1)
	))
    instants-list))


(defun verilog-read-auto-template (module)
  "Look for a auto_template for the instantiation of the given MODULE.
If found returns the signal name connections.  Return nil or list of
 ( (signal_name connection_name)... )"
  (save-excursion
    ;; Find beginning
    (let (tpl-sig-list tpl-wild-list tpl-end-pt rep (lineno 0) (templateno 0))
      (cond ((or
	       (re-search-backward (concat "^\\s-*/?\\*?\\s-*" module "\\s-+AUTO_TEMPLATE") nil t)
	       (progn
		 (goto-char (point-min))
		 (re-search-forward (concat "^\\s-*/?\\*?\\s-*" module "\\s-+AUTO_TEMPLATE") nil t)))
	     (search-forward "(")
	     (when verilog-auto-inst-template-numbers
	       (save-excursion
		 (goto-char (point-min))
		 (while (search-forward "AUTO_TEMPLATE" nil t)
		   (setq templateno (1+ templateno)))))
	     (setq tpl-end-pt (save-excursion
				(backward-char 1)
				(forward-sexp 1)   ;; Moves to paren that closes argdecl's
				(backward-char 1)
				(point)))
	     ;;
	     (while (< (point) tpl-end-pt)
	       (cond ((looking-at "\\s-*\\.\\([a-zA-Z0-9`_$]+\\)\\s-*(\\(.*\\))\\s-*\\(,\\|)\\s-*;\\)")
		      (setq tpl-sig-list (cons (list
						(match-string-no-properties 1)
						(match-string-no-properties 2)
						templateno lineno)
					       tpl-sig-list))
		      (goto-char (match-end 0)))
		     ;; Regexp form??
		     ((looking-at
		       ;; Regexp bug in xemacs disallows ][ inside [], and wants + last
		       "\\s-*\\.\\(\\([a-zA-Z0-9`_$+@^.*?|---]+\\|[][]\\|\\\\[()|]\\)+\\)\\s-*(\\(.*\\))\\s-*\\(,\\|)\\s-*;\\)")
		      (setq rep (match-string-no-properties 3))
		      (goto-char (match-end 0))
		      (setq tpl-wild-list
			    (cons (list
				   (concat "^"
					   (verilog-string-replace-matches "@" "\\\\([0-9]+\\\\)" nil nil
									   (match-string 1))
					   "$")
				   rep
				   templateno lineno)
				  tpl-wild-list)))
		     ((looking-at "[ \t\f]+")
		      (goto-char (match-end 0)))
		     ((looking-at "\n")
		      (setq lineno (1+ lineno))
		      (goto-char (match-end 0)))
		     ((looking-at "//")
		      (search-forward "\n"))
		     ((looking-at "/\\*")
		      (forward-char 2)
		      (or (search-forward "*/")
			  (error "%s: Unmatched /* */, at char %d" (verilog-point-text) (point))))
		     (t
		      (error "%s: AUTO_TEMPLATE parsing error: %s"
			     (verilog-point-text)
			     (progn (looking-at ".*$") (match-string 0))))
		     ))
	     ;;
	     (list tpl-sig-list tpl-wild-list)
	     )))))
;;(progn (find-file "auto-template.v") (verilog-read-auto-template "ptl_entry"))

(defun verilog-set-define (defname defvalue &optional buffer enumname)
  "In BUFFER, set the definition DEFNAME to the DEFVALUE."
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (let ((mac (intern (concat "vh-" defname))))
      ;;(message "Define %s=%s" defname defvalue) (sleep-for 1)
      (set (make-variable-buffer-local mac) defvalue))
    (if enumname
	(let ((enumvar (intern (concat "venum-" enumname))))
	  ;;(message "Define %s=%s" defname defvalue) (sleep-for 1)
	  (make-variable-buffer-local enumvar)
	  (add-to-list enumvar defname)))
    ))

(defun verilog-read-defines (&optional filename recurse)
  "Read `defines and parameters for the current file, or optional FILENAME.
If the filename is provided, `verilog-library-directories' and
`verilog-library-extensions' will be used to resolve it.  If optional
RECURSE is non-nil, recurse through `includes.

Parameters must be simple assignments to constants, or have their own
\"parameter\" label rather then a list of parameters.  Thus:

    parameter X = 5, Y = 10;	// Ok
    parameter X = {1'b1, 2'h2};	// Ok
    parameter X = {1'b1, 2'h2}, Y = 10;	// Bad, make into 2 parameter lines

Defines must be simple text substitutions, one on a line, starting
at the beginning of the line.  Any ifdefs or multiline comments around the
define are ignored.

Defines are stored inside Emacs variables using the name vh-{definename}.

This function is useful for setting vh-* variables.  The file variables
feature can be used to set defines that `verilog-mode' can see; put at the
*END* of your file something like:

// Local Variables:
// vh-macro:\"macro_definition\"
// End:

If macros are defined earlier in the same file and you want their values,
you can read them automatically (provided `enable-local-eval' is on):

// Local Variables:
// eval:(verilog-read-defines)
// eval:(verilog-read-defines \"group_standard_includes.v\")
// End:

Note these are only read when the file is first visited, you must use
\\[find-alternate-file] RET  to have these take effect after editing them!

If you want to disable the \"Process `eval' or hook local variables\"
warning message, you need to add to your .emacs file:

(setq enable-local-eval t)
"
  (let ((origbuf (current-buffer)))
    (save-excursion
      (when filename
	(let ((fns (verilog-library-filenames filename (buffer-file-name))))
	  (if fns
	      (set-buffer (find-file-noselect (car fns)))
	    (error (concat (verilog-point-text)
			   ": Can't find verilog-read-defines file: " filename)))))
      (when recurse
	(goto-char (point-min))
	(while (re-search-forward "^\\s-*`include\\s-+\\([^ \t\n]+\\)" nil t)
	  (let ((inc (verilog-string-replace-matches "\"" "" nil nil (match-string-no-properties 1))))
	    (unless (verilog-inside-comment-p)
	      (verilog-read-defines inc recurse)))))
      ;; Read `defines
      ;; note we don't use verilog-re... it's faster this way, and that
      ;; function has problems when comments are at the end of the define
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*`define\\s-+\\([a-zA-Z0-9_$]+\\)\\s-+\\(.*\\)$" nil t)
	(let ((defname (match-string-no-properties 1))
	      (defvalue (match-string-no-properties 2)))
	  (setq defvalue (verilog-string-replace-matches "\\s-*/[/*].*$" "" nil nil defvalue))
	  (verilog-set-define defname defvalue origbuf)))
      ;; Hack: Read parameters
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\s-*\\(parameter\\|localparam\\)\\(\\(\\s-*\\[[^]]*\\]\\|\\)\\s-+\\([a-zA-Z0-9_$]+\\)\\s-*=\\s-*\\([^;,]*\\),?\\|\\)\\s-*" nil t)
	(let ((var (match-string-no-properties 4))
	      (val (match-string-no-properties 5))
	      enumname)
	  ;; The primary way of getting defines is verilog-read-decls
	  ;; However, that isn't called yet for included files, so we'll add another scheme
	  (if (looking-at "[^\n]*synopsys\\s +enum\\s +\\([a-zA-Z0-9_]+\\)")
	      (setq enumname (match-string-no-properties 1)))
	  (if var
	    (verilog-set-define var val origbuf enumname))
	  (forward-comment 999)
	  (while (looking-at "\\s-*,?\\s-*\\([a-zA-Z0-9_$]+\\)\\s-*=\\s-*\\([^;,]*\\),?\\s-*")
	    (verilog-set-define (match-string-no-properties 1) (match-string-no-properties 2) origbuf enumname)
	    (goto-char (match-end 0))
	    (forward-comment 999))))
      )))

(defun verilog-read-includes ()
  "Read `includes for the current file.
This will find all of the `includes which are at the beginning of lines,
ignoring any ifdefs or multiline comments around them.
`verilog-read-defines' is then performed on the current and each included
file.

It is often useful put at the *END* of your file something like:

// Local Variables:
// eval:(verilog-read-includes)
// End:

Note includes are only read when the file is first visited, you must use
\\[find-alternate-file] RET  to have these take effect after editing them!

It is good to get in the habit of including all needed files in each .v
file that needs it, rather then waiting for compile time.  This will aid
this process, Verilint, and readability.  To prevent defining the same
variable over and over when many modules are compiled together, put a test
around the inside each include file:

foo.v (a include):
	`ifdef _FOO_V	// include if not already included
	`else
	`define _FOO_V
	... contents of file
	`endif // _FOO_V"
;;slow:  (verilog-read-defines nil t))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*`include\\s-+\\([^ \t\n]+\\)" nil t)
      (let ((inc (verilog-string-replace-matches "\"" "" nil nil (match-string 1))))
	(verilog-read-defines inc)))))

(defun verilog-read-signals (&optional start end)
  "Return a simple list of all possible signals in the file.
Bounded by optional region from START to END.  Overly aggressive but fast.
Some macros and such are also found and included.  For dinotrace.el"
  (let (sigs-all keywd)
    (progn;save-excursion
      (goto-char (or start (point-min)))
      (setq end (or end (point-max)))
      (while (re-search-forward "[\"/a-zA-Z_.%`]" end t)
	(forward-char -1)
	(cond
	 ((looking-at "//")
	  (search-forward "\n"))
	 ((looking-at "/\\*")
	  (search-forward "*/"))
	 ((eq ?\" (following-char))
	  (re-search-forward "[^\\]\""))	;; don't forward-char first, since we look for a non backslash first
	 ((looking-at "\\s-*\\([a-zA-Z0-9$_.%`]+\\)")
	  (goto-char (match-end 0))
	  (setq keywd (match-string-no-properties 1))
	  (or (member keywd verilog-keywords)
	      (member keywd sigs-all)
	      (setq sigs-all (cons keywd sigs-all))))
	 (t (forward-char 1)))
	)
      ;; Return list
      sigs-all)))


;;
;; Module name lookup
;;

(defun verilog-module-inside-filename-p (module filename)
  "Return point if MODULE is specified inside FILENAME, else nil.
Allows version control to check out the file if need be."
  (and (or (file-exists-p filename)
	   (and
	    (condition-case nil
		(fboundp 'vc-backend)
	      (error nil))
	    (vc-backend filename)))
       (let (pt)
	 (save-excursion
	   (set-buffer (find-file-noselect filename))
	   (goto-char (point-min))
	   (while (and
		   ;; It may be tempting to look for verilog-defun-re, don't, it slows things down a lot!
		   (verilog-re-search-forward-quick "module" nil t)
		   (verilog-re-search-forward-quick "[(;]" nil t))
	     (if (equal module (verilog-read-module-name))
		 (setq pt (point))))
	   pt))))

(defun verilog-is-number (symbol)
  "Return true if SYMBOL is number-like."
  (or (string-match "^[0-9 \t:]+$" symbol)
      (string-match "^[---]*[0-9]+$" symbol)
      (string-match "^[0-9 \t]+'s?[hdxbo][0-9a-fA-F_xz? \t]*$" symbol)
      ))

(defun verilog-symbol-detick (symbol wing-it)
  "Return a expanded SYMBOL name without any defines.
If the variable vh-{symbol} is defined, return that value.
If undefined, and WING-IT, return just SYMBOL without the tick, else nil."
  (while (and symbol (string-match "^`" symbol))
    (setq symbol (substring symbol 1))
    (setq symbol
	  (if (boundp (intern (concat "vh-" symbol)))
	      ;; Emacs has a bug where boundp on a buffer-local variable in only one
	      ;; buffer returns t in another.  This can confuse, so check for nil.
	      (let ((val (eval (intern (concat "vh-" symbol)))))
		(if (eq val nil)
		    (if wing-it symbol nil)
		  val))
	    (if wing-it symbol nil))))
  symbol)
;;(verilog-symbol-detick "`mod" nil)

(defun verilog-symbol-detick-denumber (symbol)
  "Return symbol with defines converted and any numbers dropped to nil."
  (when (string-match "^`" symbol)
    ;; This only will work if the define is a simple signal, not
    ;; something like a[b].  Sorry, it should be substituted into the parser
    (setq symbol
	  (verilog-string-replace-matches
	   "\[[^0-9: \t]+\]" "" nil nil
	   (or (verilog-symbol-detick symbol nil)
	       (if verilog-auto-sense-defines-constant
		   "0"
		 symbol)))))
  (if (verilog-is-number symbol)
      nil
    symbol))

(defun verilog-expand-dirnames (&optional dirnames)
  "Return a list of existing directories given a list of wildcarded dirnames
or just the existing dirnames themselves if there are no wildcards."
  (interactive)
  (unless dirnames (error "verilog-library-directories should include at least '.'"))
  (setq dirnames (reverse dirnames))	; not nreverse
  (let ((dirlist nil)
	pattern dirfile dirfiles dirname root filename rest)
    (while dirnames
      (setq dirname (substitute-in-file-name (car dirnames))
	    dirnames (cdr dirnames))
      (cond ((string-match (concat "^\\(\\|[/\\]*[^*?]*[/\\]\\)"  ;; root
				   "\\([^/\\]*[*?][^/\\]*\\)"	  ;; filename with *?
				   "\\(.*\\)")			  ;; rest
			   dirname)
	     (setq root (match-string 1 dirname)
		   filename (match-string 2 dirname)
		   rest (match-string 3 dirname)
		   pattern filename)
	     ;; now replace those * and ? with .+ and .
	     ;; use ^ and /> to get only whole file names
	     ;;verilog-string-replace-matches
	     (setq pattern (verilog-string-replace-matches "[*]" ".+" nil nil pattern)
		   pattern (verilog-string-replace-matches "[?]" "." nil nil pattern)

		   ;; Unfortunately allows abc/*/rtl to match abc/rtl
		   ;; because abc/.. shows up in dirfiles.  Solutions welcome.
		   dirfiles (directory-files root t pattern nil))
	     (while dirfiles
	       (setq dirfile (expand-file-name (concat (car dirfiles) rest))
		     dirfiles (cdr dirfiles))
	       (if (file-directory-p dirfile)
		   (setq dirlist (cons dirfile dirlist))))
	     )
	    ;; Defaults
	    (t
	     (if (file-directory-p dirname)
		 (setq dirlist (cons dirname dirlist))))
	    ))
    dirlist))
;;(verilog-expand-dirnames (list "." ".." "nonexist" "../*" "/home/wsnyder/*/v"))

(defun verilog-library-filenames (filename current)
  "Return a search path to find the given FILENAME name.
Uses the CURRENT filename, `verilog-library-directories' and
`verilog-library-extensions' variables to build the path."
  (let ((ckdir (verilog-expand-dirnames verilog-library-directories))
	fn outlist)
    (while ckdir
      (setq fn (expand-file-name
		filename
		(expand-file-name (car ckdir) (file-name-directory current))))
      (if (file-exists-p fn)
	  (setq outlist (cons fn outlist)))
      (setq ckdir (cdr ckdir)))
    (nreverse outlist)))

(defun verilog-module-filenames (module current)
  "Return a search path to find the given MODULE name.
Uses the CURRENT filename, `verilog-library-extensions',
`verilog-library-directories' and `verilog-library-files'
variables to build the path."
  ;; Return search locations for it
  (append (list current)	; first, current buffer
 	  (let ((ext verilog-library-extensions) flist)
 	    (while ext
 	      (setq flist
 		    (append (verilog-library-filenames
 			     (concat module (car ext)) current) flist)
		    ext (cdr ext)))
	    flist)
	  verilog-library-files	 ; finally, any libraries
	  ))

;;
;; Module Information
;;
;; Many of these functions work on "modi" a module information structure
;; A modi is:  [module-name-string file-name begin-point]

(defvar verilog-cache-enabled t
  "If true, enable caching of signals, etc.  Set to nil for debugging to make things SLOW!")

(defvar verilog-modi-cache-list nil
  "Cache of ((Module Function) Buf-Tick Buf-Modtime Func-Returns)...
For speeding up verilog-modi-get-* commands.
Buffer-local.")

(defvar verilog-modi-cache-preserve-tick nil
  "Modification tick after which the cache is still considered valid.
Use verilog-preserve-cache's to set")
(defvar verilog-modi-cache-preserve-buffer nil
  "Modification tick after which the cache is still considered valid.
Use verilog-preserve-cache's to set")

(defun verilog-modi-current ()
  "Return the modi structure for the module currently at point."
  (let* (name pt)
    ;; read current module's name
    (save-excursion
      (verilog-re-search-backward-quick verilog-defun-re nil nil)
      (verilog-re-search-forward-quick "(" nil nil)
      (setq name (verilog-read-module-name))
      (setq pt (point)))
    ;; return
    (vector name (or (buffer-file-name) (current-buffer)) pt)))

(defvar verilog-modi-lookup-last-mod nil "Cache of last module looked up.")
(defvar verilog-modi-lookup-last-current nil "Cache of last current looked up.")
(defvar verilog-modi-lookup-last-modi nil "Cache of last modi returned.")

(defun verilog-modi-lookup (module allow-cache)
  "Find the file and point at which MODULE is defined.
If ALLOW-CACHE is set, check and remember cache of previous lookups.
Return modi if successful, else print message."
  (let* ((current (or (buffer-file-name) (current-buffer))))
    (cond ((and (equal verilog-modi-lookup-last-mod module)
		(equal verilog-modi-lookup-last-current current)
		verilog-modi-lookup-last-modi
		verilog-cache-enabled
		allow-cache)
	   ;; ok as is
	   )
	  (t (let* ((realmod (verilog-symbol-detick module t))
		    (orig-filenames (verilog-module-filenames realmod current))
		    (filenames orig-filenames)
		    pt)
	       (while (and filenames (not pt))
		 (if (not (setq pt (verilog-module-inside-filename-p realmod (car filenames))))
		     (setq filenames (cdr filenames))))
	       (cond (pt (setq verilog-modi-lookup-last-modi
			       (vector realmod (car filenames) pt)))
		     (t (setq verilog-modi-lookup-last-modi nil)
			(error (concat (verilog-point-text)
				       ": Can't locate " module " module definition"
				       (if (not (equal module realmod))
					   (concat " (Expanded macro to " realmod ")")
					 "")
				       "\n    Check the verilog-library-directories variable."
				       "\n    I looked in (if not listed, doesn't exist):\n\t" (mapconcat 'concat orig-filenames "\n\t"))))
		     )
	       (setq verilog-modi-lookup-last-mod module
		     verilog-modi-lookup-last-current current))))
    verilog-modi-lookup-last-modi
    ))

(defsubst verilog-modi-name (modi)
  (aref modi 0))
(defsubst verilog-modi-file-or-buffer (modi)
  (aref modi 1))
(defsubst verilog-modi-point (modi)
  (aref modi 2))

(defun verilog-modi-filename (modi)
  "Filename of modi, or name of buffer if its never been saved"
  (if (bufferp (verilog-modi-file-or-buffer modi))
      (or (buffer-file-name (verilog-modi-file-or-buffer modi))
	  (buffer-name (verilog-modi-file-or-buffer modi)))
    (verilog-modi-file-or-buffer modi)))

(defun verilog-modi-goto (modi)
  "Move point/buffer to specified MODI."
  (or modi (error "Passed unfound modi to goto, check earlier"))
  (set-buffer (if (bufferp (verilog-modi-file-or-buffer modi))
		  (verilog-modi-file-or-buffer modi)
		(find-file-noselect (verilog-modi-file-or-buffer modi))))
  (or (equal major-mode `verilog-mode)	;; Put into verilog mode to get syntax
      (verilog-mode))
  (goto-char (verilog-modi-point modi)))

(defun verilog-goto-defun-file (module)
  "Move point to the file at which a given MODULE is defined."
  (interactive "sGoto File for Module: ")
  (let* ((modi (verilog-modi-lookup module nil)))
    (when modi
      (verilog-modi-goto modi)
      (switch-to-buffer (current-buffer)))))

(defun verilog-modi-cache-results (modi function)
  "Run on MODI the given FUNCTION.  Locate the module in a file.
Cache the output of function so next call may have faster access."
  (let (func-returns fass)
    (save-excursion
      (verilog-modi-goto modi)
      (if (and (setq fass (assoc (list (verilog-modi-name modi) function)
				 verilog-modi-cache-list))
	       ;; Destroy caching when incorrect; Modified or file changed
	       (not (and verilog-cache-enabled
			 (or (equal (buffer-modified-tick) (nth 1 fass))
			     (and verilog-modi-cache-preserve-tick
				  (<= verilog-modi-cache-preserve-tick  (nth 1 fass))
				  (equal  verilog-modi-cache-preserve-buffer (current-buffer))))
			 (equal (visited-file-modtime) (nth 2 fass)))))
	  (setq verilog-modi-cache-list nil
		fass nil))
      (cond (fass
	     ;; Found
	     (setq func-returns (nth 3 fass)))
	    (t
	     ;; Read from file
	     ;; Clear then restore any hilighting to make emacs19 happy
	     (let ((fontlocked (when (and (memq 'v19 verilog-emacs-features)
					  (boundp 'font-lock-mode)
					  font-lock-mode)
				 (font-lock-mode nil)
				 t)))
	       (setq func-returns (funcall function))
	       (when fontlocked (font-lock-mode t)))
	     ;; Cache for next time
	     (make-variable-buffer-local 'verilog-modi-cache-list)
	     (setq verilog-modi-cache-list
		   (cons (list (list (verilog-modi-name modi) function)
			       (buffer-modified-tick)
			       (visited-file-modtime)
			       func-returns)
			 verilog-modi-cache-list)))
	    ))
      ;;
      func-returns))

(defun verilog-modi-cache-add (modi function element sig-list)
  "Add function return results to the module cache.
Update MODI's cache for given FUNCTION so that the return ELEMENT of that
function now contains the additional SIG-LIST parameters."
  (let (fass)
    (save-excursion
      (verilog-modi-goto modi)
      (if (setq fass (assoc (list (verilog-modi-name modi) function)
			    verilog-modi-cache-list))
	  (let ((func-returns (nth 3 fass)))
	    (aset func-returns element
		  (append sig-list (aref func-returns element))))))))

(defmacro verilog-preserve-cache (&rest body)
  "Execute the BODY forms, allowing cache preservation within BODY.
This means that changes to the buffer will not result in the cache being
flushed.  If the changes affect the modsig state, they must call the
modsig-cache-add-* function, else the results of later calls may be
incorrect.  Without this, changes are assumed to be adding/removing signals
and invalidating the cache."
  `(let ((verilog-modi-cache-preserve-tick (buffer-modified-tick))
	 (verilog-modi-cache-preserve-buffer (current-buffer)))
     (progn ,@body)))


(defun verilog-signals-matching-enum (in-list enum)
  "Return all signals in IN-LIST matching the given ENUM."
  (let (out-list)
    (while in-list
      (if (equal (verilog-sig-enum (car in-list)) enum)
	  (setq out-list (cons (car in-list) out-list)))
      (setq in-list (cdr in-list)))
    ;; New scheme
    (let* ((enumvar (intern (concat "venum-" enum)))
	   (enumlist (and (boundp enumvar) (eval enumvar))))
      (while enumlist
	(add-to-list 'out-list (list (car enumlist)))
	(setq enumlist (cdr enumlist))))
    (nreverse out-list)))

;; Combined
(defun verilog-modi-get-signals (modi)
  (append
   (verilog-modi-get-outputs modi)
   (verilog-modi-get-inouts modi)
   (verilog-modi-get-inputs modi)
   (verilog-modi-get-wires modi)
   (verilog-modi-get-regs modi)
   (verilog-modi-get-assigns modi)
   (verilog-modi-get-consts modi)))

(defun verilog-modi-get-ports (modi)
  (append
   (verilog-modi-get-outputs modi)
   (verilog-modi-get-inouts modi)
   (verilog-modi-get-inputs modi)))

(defsubst verilog-modi-cache-add-outputs (modi sig-list)
  (verilog-modi-cache-add modi 'verilog-read-decls 0 sig-list))
(defsubst verilog-modi-cache-add-inouts (modi sig-list)
  (verilog-modi-cache-add modi 'verilog-read-decls 1 sig-list))
(defsubst verilog-modi-cache-add-inputs (modi sig-list)
  (verilog-modi-cache-add modi 'verilog-read-decls 2 sig-list))
(defsubst verilog-modi-cache-add-wires (modi sig-list)
  (verilog-modi-cache-add modi 'verilog-read-decls 3 sig-list))
(defsubst verilog-modi-cache-add-regs (modi sig-list)
  (verilog-modi-cache-add modi 'verilog-read-decls 4 sig-list))

(defun verilog-signals-from-signame (signame-list)
  "Return signals in standard form from SIGNAME-LIST, a simple list of signal names."
  (mapcar (function (lambda (name) (list name nil nil)))
	  signame-list))

;;
;; Auto creation utilities
;;

(defun verilog-auto-search-do (search-for func)
  "Search for the given auto text SEARCH-FOR, and perform FUNC where it occurs."
  (goto-char (point-min))
  (while (search-forward search-for nil t)
    (if (not (save-excursion
	       (goto-char (match-beginning 0))
	       (verilog-inside-comment-p)))
	(funcall func))))

(defun verilog-auto-re-search-do (search-for func)
  "Search for the given auto text SEARCH-FOR, and perform FUNC where it occurs."
  (goto-char (point-min))
  (while (re-search-forward search-for nil t)
    (if (not (save-excursion
	       (goto-char (match-beginning 0))
	       (verilog-inside-comment-p)))
	(funcall func))))

(defun verilog-insert-definition (sigs type indent-pt &optional dont-sort)
  "Print out a definition for a list of SIGS of the given TYPE,
with appropriate INDENT-PT indentation.  Sort unless DONT-SORT.
TYPE is normally wire/reg/output."
  (or dont-sort
      (setq sigs (sort (copy-alist sigs) `verilog-signals-sort-compare)))
  (while sigs
    (let ((sig (car sigs)))
      (indent-to indent-pt)
      (insert type)
      (when (verilog-sig-signed sig)
	(insert " " (verilog-sig-signed sig)))
      (when (verilog-sig-bits sig)
	(insert " " (verilog-sig-bits sig)))
      (indent-to (max 24 (+ indent-pt 16)))
      (insert (concat (verilog-sig-name sig) ";"))
      (if (or (not (verilog-sig-comment sig))
	      (equal "" (verilog-sig-comment sig)))
	  (insert "\n")
	(indent-to (max 48 (+ indent-pt 40)))
	(insert (concat "// " (verilog-sig-comment sig) "\n")))
      (setq sigs (cdr sigs)))))

(eval-when-compile
  (if (not (boundp 'indent-pt))
      (defvar indent-pt nil "Local used by insert-indent")))

(defun verilog-insert-indent (&rest stuff)
  "Indent to position stored in local `indent-pt' variable, then insert STUFF.
Presumes that any newlines end a list element."
  (let ((need-indent t))
    (while stuff
      (if need-indent (indent-to indent-pt))
      (setq need-indent nil)
      (insert (car stuff))
      (setq need-indent (string-match "\n$" (car stuff))
	    stuff (cdr stuff)))))
;;(let ((indent-pt 10)) (verilog-insert-indent "hello\n" "addon" "there\n"))

(defun verilog-get-list (start end)
  "Return the elements of a comma separated list."
  (interactive)
  (let ((my-list (list))
	my-string)
    (save-excursion
      (while (< (point) end)
	(when (re-search-forward "\\([^,{]+\\)" end t)
	  (setq my-string (verilog-string-remove-spaces (match-string 1)))
	  (setq my-list (nconc my-list (list my-string) ))
	  (goto-char (match-end 0))))
      my-list)))

(defun verilog-make-width-expression (range-exp)
  "Return an expression calculating the length of a range [x:y]."
  (interactive)
  ;; strip off the []
  (cond ((not range-exp)
	 "1")
	(t
	 (if (string-match "^\\[\\(.*\\)\\]$" range-exp)
	     (setq range-exp (match-string 1 range-exp)))
	 (cond ((not range-exp)
		"1")
	       ((string-match "^\\s *\\([0-9]+\\)\\s *:\\s *\\([0-9]+\\)\\s *$" range-exp)
		(int-to-string (1+ (abs (- (string-to-int (match-string 1 range-exp))
					   (string-to-int (match-string 2 range-exp)))))))
	       ((string-match "^\\(.*\\)\\s *:\\s *\\(.*\\)\\s *$" range-exp)
		(concat "(1+(" (match-string 1 range-exp)
			")-(" (match-string 2 range-exp) "))"))
	       (t nil)))))
;;(verilog-make-width-expression "`A:`B")


;;
;; Auto deletion
;;

(defun verilog-delete-autos-lined ()
  "Delete autos that occupy multiple lines, between begin and end comments."
  (let ((pt (point)))
    (forward-line 1)
    (when (and
	   (looking-at "\\s-*// Beginning")
	   (search-forward "// End of automatic" nil t))
      ;; End exists
      (end-of-line)
      (delete-region pt (point))
      (forward-line 1))
  ))

(defun verilog-backward-open-paren ()
  "Find the open parenthesis that match the current point,
ignore other open parenthesis with matching close parens"
  (let ((parens 1))
    (while (> parens 0)
      (unless (verilog-re-search-backward-quick "[()]" nil t)
	(error "%s: Mismatching ()" (verilog-point-text)))
      (cond ((looking-at ")")
	     (setq parens (1+ parens)))
	    ((looking-at "(")
	     (setq parens (1- parens)))))))

(defun verilog-delete-to-paren ()
  "Delete the automatic inst/sense/arg created by autos.
Deletion stops at the matching end parenthesis."
  (delete-region (point)
		 (save-excursion
		   (verilog-backward-open-paren)
		   (forward-sexp 1)   ;; Moves to paren that closes argdecl's
		   (backward-char 1)
		   (point))))

(defun verilog-delete-auto ()
  "Delete the automatic outputs, regs, and wires created by \\[verilog-auto].
Use \\[verilog-auto] to re-insert the updated AUTOs."
  (interactive)
  (save-excursion
    (if (buffer-file-name)
	(find-file-noselect (buffer-file-name)))	;; To check we have latest version
    ;; Remove those that have multi-line insertions
    (verilog-auto-re-search-do "/\\*AUTO\\(OUTPUTEVERY\\|CONCATCOMMENT\\|WIRE\\|REG\\|DEFINEVALUE\\|REGINPUT\\|INPUT\\|OUTPUT\\|INOUT\\|RESET\\)\\*/"
			       'verilog-delete-autos-lined)
    ;; Remove those that have multi-line insertions with parameters
    (verilog-auto-re-search-do "/\\*AUTO\\(INOUTMODULE\\|ASCIIENUM\\)([^)]*)\\*/"
			       'verilog-delete-autos-lined)
    ;; Remove those that are in parenthesis
    (verilog-auto-re-search-do "/\\*\\(AS\\|AUTO\\(ARG\\|CONCATWIDTH\\|INST\\|SENSE\\)\\)\\*/"
			       'verilog-delete-to-paren)

    ;; Remove template comments ... anywhere in case was pasted after AUTOINST removed
    (goto-char (point-min))
    (while (re-search-forward "\\s-*// Templated[ \tLT0-9]*$" nil t)
      (replace-match ""))))

;;
;; Auto save
;;

(defun verilog-auto-save-check ()
  "On saving see if we need auto update."
  (cond ((not verilog-auto-save-policy)) ; disabled
	((not (save-excursion
		(save-match-data
		  (let ((case-fold-search nil))
		    (goto-char (point-min))
		    (re-search-forward "AUTO" nil t))))))
	((eq verilog-auto-save-policy 'force)
	 (verilog-auto))
	((not (buffer-modified-p)))
	((eq verilog-auto-update-tick (buffer-modified-tick))) ; up-to-date
	((eq verilog-auto-save-policy 'detect)
	 (verilog-auto))
	(t
	 (when (yes-or-no-p "AUTO statements not recomputed, do it now? ")
	   (verilog-auto))
	 ;; Don't ask again if didn't update
	 (set (make-local-variable 'verilog-auto-update-tick) (buffer-modified-tick))
	 ))
  nil)	;; Always return nil -- we don't write the file ourselves

(defun verilog-auto-read-locals ()
  "Return file local variable segment at bottom of file."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward "Local Variables:" nil t)
	(buffer-substring-no-properties (point) (point-max))
      "")))

(defun verilog-auto-reeval-locals (&optional just-cache)
  "Read file local variable segment at bottom of file if it's changed since last read."
  (make-variable-buffer-local 'verilog-auto-last-file-locals)
  (let ((curlocal (verilog-auto-read-locals)))
    (when (not (equal verilog-auto-last-file-locals curlocal))
      (unless just-cache (hack-local-variables))
      (setq verilog-auto-last-file-locals curlocal)
      t)))

;;
;; Auto creation
;;

(defun verilog-auto-arg-ports (sigs message indent-pt)
  "Print a list of ports for a AUTOINST.
Takes SIGS list, adds MESSAGE to front and inserts each at INDENT-PT."
  (when sigs
    (insert "\n")
    (indent-to indent-pt)
    (insert message)
    (insert "\n")
    (indent-to indent-pt)
    (while sigs
      (cond ((> (+ 2 (current-column) (length (verilog-sig-name (car sigs)))) fill-column)
	     (insert "\n")
	     (indent-to indent-pt)))
      (insert (verilog-sig-name (car sigs)) ", ")
      (setq sigs (cdr sigs)))))

(defun verilog-auto-arg ()
  "Expand AUTOARG statements.
Replace the argument declarations at the beginning of the
module with ones automatically derived from input and output
statements.  This can be dangerous if the module is instantiated
using position-based connections, so use only name-based when
instantiating the resulting module.  Long lines are split based
on the `fill-column', see \\[set-fill-column].

Limitations:
   Concatenation and outputting partial busses is not supported.

For example:

	module ex_arg (/*AUTOARG*/);
	  input i;
	  output o;
	endmodule

Typing \\[verilog-auto] will make this into:

	module ex_arg (/*AUTOARG*/
	  // Outputs
	  o,
	  // Inputs
	  i
	);
	  input i;
	  output o;
	endmodule

Any ports declared between the ( and /*AUTOARG*/ are presumed to be
predeclared and are not redeclared by AUTOARG.  You need to know whether to
put a comma just before the AUTOARG or not, based upon whether there will be
ports in the AUTOARG or not; this is not determined for you.  Avoid
declaring ports manually, as it makes code harder to maintain."
  (save-excursion
    (let ((modi (verilog-modi-current))
	  (skip-pins (aref (verilog-read-arg-pins) 0))
	  (pt (point)))
      (verilog-auto-arg-ports (verilog-signals-not-in
			       (verilog-modi-get-outputs modi)
			       skip-pins)
			      "// Outputs"
			      verilog-indent-level-declaration)
      (verilog-auto-arg-ports (verilog-signals-not-in
			       (verilog-modi-get-inouts modi)
			       skip-pins)
			      "// Inouts"
			      verilog-indent-level-declaration)
      (verilog-auto-arg-ports (verilog-signals-not-in
			       (verilog-modi-get-inputs modi)
			       skip-pins)
			      "// Inputs"
			      verilog-indent-level-declaration)
      (save-excursion
	(if (re-search-backward "," pt t)
	    (delete-char 2)))
      (unless (eq (char-before) ?/ )
	(insert "\n"))
      (indent-to verilog-indent-level-declaration)
      )))

(defun verilog-auto-inst-port-map (port-st)
  nil)

(defvar vector-skip-list nil) ; Prevent compile warning
(defvar vl-cell-type nil "See verilog-auto-inst") ; Prevent compile warning
(defvar vl-cell-name nil "See verilog-auto-inst") ; Prevent compile warning
(defvar vl-width 0)
(defvar vl-name 0)
(defun verilog-auto-inst-port (port-st indent-pt tpl-list tpl-num)
  "Print out a instantiation connection for this PORT-ST.
Insert to INDENT-PT, use template TPL-LIST.
@ are instantiation numbers, replaced with TPL-NUM.
@\"(expression @)\" are evaluated, with @ as a variable."
  (let* ((port (verilog-sig-name port-st))
	 (tpl-ass (or (assoc port (car tpl-list))
		      (verilog-auto-inst-port-map port-st)))
	 ;; vl-* are documented for user use
	 (vl-name (verilog-sig-name port-st))
	 (vl-width (verilog-sig-width port-st))
	 (vl-bits (if (or verilog-auto-inst-vector
			  (not (assoc port vector-skip-list))
			  (not (equal (verilog-sig-bits port-st)
				      (verilog-sig-bits (assoc port vector-skip-list)))))
		      (or (verilog-sig-bits port-st) "")
		    ""))
	 ;; Default if not found
	 (tpl-net (concat port vl-bits)))
    ;; Find template
    (cond (tpl-ass	    ; Template of exact port name
	   (setq tpl-net (nth 1 tpl-ass)))
	  ((nth 1 tpl-list) ; Wildcards in template, search them
	   (let ((wildcards (nth 1 tpl-list)))
	     (while wildcards
	       (when (string-match (nth 0 (car wildcards)) port)
		 (setq tpl-ass (car wildcards)  ; so allow @ parsing
		       tpl-net (replace-match (nth 1 (car wildcards))
					      t nil port)))
	       (setq wildcards (cdr wildcards))))))
    ;; Parse Templated variable
    (when tpl-ass
      ;; Evaluate @"(lispcode)"
      (when (string-match "@\".*[^\\]\"" tpl-net)
	(while (string-match "@\"\\(\\([^\\\"]*\\(\\\\.\\)*\\)*\\)\"" tpl-net)
	  (setq tpl-net
		(concat
		 (substring tpl-net 0 (match-beginning 0))
		 (save-match-data
		   (let* ((expr (match-string 1 tpl-net))
			  (value
			   (progn
			     (setq expr (verilog-string-replace-matches "\\\\\"" "\"" nil nil expr))
			     (setq expr (verilog-string-replace-matches "@" tpl-num nil nil expr))
			     (prin1 (eval (car (read-from-string expr)))
				    (lambda (ch) ())))))
		     (if (numberp value) (setq value (number-to-string value)))
		     value
		     ))
		 (substring tpl-net (match-end 0))))))
      ;; Replace @ and [] magic variables in final output
      (setq tpl-net (verilog-string-replace-matches "@" tpl-num nil nil tpl-net))
      (setq tpl-net (verilog-string-replace-matches "\\[\\]" vl-bits nil nil tpl-net))
      )
    (indent-to indent-pt)
    (insert "." port)
    (indent-to 40)
    (insert "(" tpl-net "),")
    (when tpl-ass
      (indent-to 64)
      (insert " // Templated")
      (when verilog-auto-inst-template-numbers
	(insert " T" (int-to-string (nth 2 tpl-ass))
		" L" (int-to-string (nth 3 tpl-ass)))))
    (insert "\n")))
;;(verilog-auto-inst-port (list "foo" "[5:0]") 10 (list (list "foo" "a@\"(% (+ @ 1) 4)\"a")) "3")
;;(x "incom[@\"(+ (* 8 @) 7)\":@\"(* 8 @)\"]")
;;(x ".out (outgo[@\"(concat (+ (* 8 @) 7) \\\":\\\" ( * 8 @))\"]));")

(defun verilog-auto-inst ()
  "Expand AUTOINST statements, as part of \\[verilog-auto].
Replace the argument calls inside an instantiation with ones
automatically derived from the module header of the instantiated netlist.

Limitations:
  Module names must be resolvable to filenames by adding a
  verilog-library-extension, and being found in the same directory, or
  by changing the variable `verilog-library-directories' or
  `verilog-library-files'.  Macros `modname are translated through the
  vh-{name} Emacs variable, if that is not found, it just ignores the `.

  In templates you must have one signal per line, ending in a ), or ));,
  and have proper () nesting, including a final ); to end the template.

For example, first take the submodule inst.v:

	module inst (o,i)
	   output [31:0] o;
	   input i;
	   wire [31:0] o = {32{i}};
	endmodule

This is then used in a upper level module:

	module ex_inst (o,i)
	   output o;
	   input i;
	   inst inst (/*AUTOINST*/);
	endmodule

Typing \\[verilog-auto] will make this into:

	module ex_inst (o,i)
	   output o;
	   input i;
	   inst inst (/*AUTOINST*/
		      // Outputs
		      .ov			(ov[31:0]),
		      // Inputs
		      .i			(i));
	endmodule

Where the list of inputs and outputs came from the inst module.

Exceptions:

Unless you are instantiating a module multiple times, or the module is
something trivial like a adder, DO NOT CHANGE SIGNAL NAMES ACROSS HIERARCHY.
It just makes for unmaintainable code.  To sanitize signal names, try
vrename from http://www.veripool.com

When you need to violate this suggestion there are several ways to list
exceptions.

Any ports defined before the /*AUTOINST*/ are not included in the list of
automatics.  This is similar to making a template as described below, but
is restricted to simple connections just like you normally make.  Also note
that any signals before the AUTOINST will only be picked up by AUTOWIRE if
you have the appropriate // Input or // Output comment, and exactly the
same line formatting as AUTOINST itself uses.

	   inst inst (// Inputs
		      .i			(my_i_dont_mess_with_it),
		      /*AUTOINST*/
		      // Outputs
		      .ov			(ov[31:0]));


Auto Templates:

For multiple instantiations based upon a single template, create a
commented out template:
	/* psm_mas AUTO_TEMPLATE (
		.PTL_MAPVALIDX		(PTL_MAPVALID[@]),
		.PTL_MAPVALIDP1X	(PTL_MAPVALID[@\"(% (+ 1 @) 4)\"]),
		.PTL_BUS		(PTL_BUSNEW[]),
		);
	*/

Templates go ABOVE the instantiation(s).  When a instantiation is expanded
`verilog-mode' simply searches up for the closest template.  Thus you can have
multiple templates for the same module, just alternate between the template
for a instantiation and the instantiation itself.

The @ character should be replaced by the instantiation number; the first
digits found in the cell's instantiation name.  The module name must be the
same as the name of the module in the instantiation name, and the code
\"AUTO_TEMPLATE\" must be in these exact words and capitalized.  Only
signals that must be different for each instantiation need to be listed.

The above template will convert:

	psm_mas ms2m (/*AUTOINST*/);

Typing \\[verilog-auto] will make this into:

	psm_mas ms2m (/*AUTOINST*/
	    // Outputs
	    .INSTDATAOUT		(INSTDATAOUT),
	    .PTL_MAPVALIDX		(PTL_MAPVALID[2]),
	    .PTL_MAPVALIDP1X		(PTL_MAPVALID[3]),
  	    .PTL_BUS			(PTL_BUSNEW[3:0]),
	    ....

Note the @ character was replaced with the 2 from \"ms2m\".  Also, if a
signal wasn't in the template, it is assumed to be a direct connection.

A [] in a template (with nothing else inside the brackets) will be replaced
by the same bus subscript as it is being connected to, or \"\" (nothing) if
it is a single bit signal.  See PTL_BUS becoming PTL_BUSNEW above.

Regexp templates:

  A template entry of the form
	    .pci_req\\([0-9]+\\)_l	(pci_req_jtag_[\\1]),

  will apply a Emacs style regular expression search for any port beginning
  in pci_req followed by numbers and ending in _l and connecting that to
  the pci_req_jtag_[] net, with the bus subscript coming from what matches
  inside the first set of \\( \\).  Thus pci_req2_l becomes pci_req_jtag_[2].

  Since \\([0-9]+\\) is so common and ugly to read, a @ does the same thing
  (Note a @ in replacement text is completely different -- still use \\1
  there!)  Thus this is the same as the above template:

	    .pci_req@_l		(pci_req_jtag_[\\1]),

  Here's another example to remove the _l, if naming conventions specify _
  alone to mean active low.  Note the use of [] to keep the bus subscript:
	    .\\(.*\\)_l		(\\1_[]),

  If you have a complicated template, set `verilog-auto-inst-template-numbers'
  to see which regexps are matching.  Don't leave that mode set after
  debugging is completed though, it will result in lots of extra differences
  and merge conflicts.

Lisp templates:

  First any regular expression template is expanded.

  If the syntax @\"( ... )\" is found, the expression in quotes will be
  evaluated as a Lisp expression, with @ replaced by the instantiation
  number.  The MAPVALIDP1X example above would put @+1 modulo 4 into the
  brackets.  Quote all double-quotes inside the expression with a leading
  backslash (\\\").  There are special variables defined that are useful
  in these Lisp functions:
	vl-name        Name portion of the input/output port
	vl-bits        Bus bits portion of the input/output port ('[2:0]')
	vl-width       Width of the input/output port ('3' for [2:0])
                       May be a (...) expression if bits isn't a constant.
	vl-cell-type   Module name/type of the cell ('psm_mas')
	vl-cell-name   Instance name of the cell ('ms2m')

  Normal Lisp variables may be used in expressions.  See
  `verilog-read-defines' which can set vh-{definename} variables for use
  here.  Also, any comments of the form:

	/*AUTO_LISP(setq foo 1)*/

  will evaluate any Lisp expression inside the parenthesis between the
  beginning of the buffer and the point of the AUTOINST.  This allows
  functions to be defined or variables to be changed between instantiations.

  After the evaluation is completed, @ substitution and [] substitution
  occur."
  (save-excursion
    ;; Find beginning
    (let* ((pt (point))
	   (indent-pt (save-excursion (verilog-backward-open-paren)
				      (1+ (current-column))))
	   (modi (verilog-modi-current))
	   (vector-skip-list (unless verilog-auto-inst-vector
			       (verilog-modi-get-signals modi)))
	   submod submodi inst skip-pins tpl-list tpl-num)
      ;; Find module name that is instantiated
      (setq submod  (verilog-read-inst-module)
	    inst (verilog-read-inst-name)
	    vl-cell-type submod
	    vl-cell-name inst
	    skip-pins (aref (verilog-read-inst-pins) 0))

      ;; Parse any AUTO_LISP() before here
      (verilog-read-auto-lisp (point-min) pt)

      ;; Lookup position, etc of submodule
      ;; Note this may raise an error
      (when (setq submodi (verilog-modi-lookup submod t))
	;; If there's a number in the instantiation, it may be a argument to the
	;; automatic variable instantiation program.
	(setq tpl-num (if (string-match "\\([0-9]+\\)" inst)
			  (substring inst (match-beginning 1) (match-end 1))
			"")
	      tpl-list (verilog-read-auto-template submod))
	;; Find submodule's signals and dump
	(insert "\n")
	(let ((sig-list (verilog-signals-not-in
			 (verilog-modi-get-outputs submodi)
			 skip-pins)))
	  (when sig-list
	    (indent-to indent-pt)
	    (insert "// Outputs\n")	;; Note these are searched for in verilog-read-sub-decl
	    (mapcar (function (lambda (port)
				(verilog-auto-inst-port port indent-pt tpl-list tpl-num)))
		    sig-list)))
	(let ((sig-list (verilog-signals-not-in
			 (verilog-modi-get-inouts submodi)
			 skip-pins)))
	  (when sig-list
	    (indent-to indent-pt)
	    (insert "// Inouts\n")
	    (mapcar (function (lambda (port)
				(verilog-auto-inst-port port indent-pt tpl-list tpl-num)))
		    sig-list)))
	(let ((sig-list (verilog-signals-not-in
			 (verilog-modi-get-inputs submodi)
			 skip-pins)))
	  (when sig-list
	    (indent-to indent-pt)
	    (insert "// Inputs\n")
	    (mapcar (function (lambda (port)
				(verilog-auto-inst-port port indent-pt tpl-list tpl-num)))
		    sig-list)))
	;; Kill extra semi
	(save-excursion
	  (cond ((re-search-backward "," pt t)
		 (delete-char 1)
		 (insert ");")
		 (search-forward "\n")	;; Added by inst-port
		 (delete-backward-char 1)
		 (if (search-forward ")" nil t) ;; From user, moved up a line
		     (delete-backward-char 1))
		 (if (search-forward ";" nil t) ;; Don't error if user had syntax error and forgot it
		     (delete-backward-char 1))
		 )
		(t
		 (delete-backward-char 1)	;; Newline Inserted above
		 )))
	))))

(defun verilog-auto-reg ()
  "Expand AUTOREG statements, as part of \\[verilog-auto].
Make reg statements for any output that isn't already declared,
and isn't a wire output from a block.

Limitations:
  This ONLY detects outputs of AUTOINSTants (see verilog-read-sub-decl).
  This does NOT work on memories, declare those yourself.

A simple example:

	module ex_reg (o,i)
	   output o;
	   input i;
	   /*AUTOREG*/
	   always o = i;
	endmodule

Typing \\[verilog-auto] will make this into:

	module ex_reg (o,i)
	   output o;
	   input i;
	   /*AUTOREG*/
	   // Beginning of automatic regs (for this module's undeclared outputs)
	   reg			o;
	   // End of automatics
	   always o = i;
	endmodule"
  (save-excursion
    ;; Point must be at insertion point.
    (let* ((indent-pt (current-indentation))
	   (modi (verilog-modi-current))
	   (sig-list (verilog-signals-not-in
		      (verilog-modi-get-outputs modi)
		      (append (verilog-modi-get-wires modi)
			      (verilog-modi-get-regs modi)
			      (verilog-modi-get-assigns modi)
			      (verilog-modi-get-consts modi)
			      (verilog-modi-get-sub-outputs modi)
			      (verilog-modi-get-sub-inouts modi)
			      ))))
      (forward-line 1)
      (verilog-insert-indent "// Beginning of automatic regs (for this module's undeclared outputs)\n")
      (verilog-insert-definition sig-list "reg" indent-pt)
      (verilog-modi-cache-add-regs modi sig-list)
      (verilog-insert-indent "// End of automatics\n")
      )))

(defun verilog-auto-reg-input ()
  "Expand AUTOREGINPUT statements, as part of \\[verilog-auto].
Make reg statements instantiation inputs that aren't already declared.
This is useful for making a top level shell for testing the module that is
to be instantiated.

Limitations:
  This ONLY detects inputs of AUTOINSTants (see verilog-read-sub-decl).
  This does NOT work on memories, declare those yourself.

A simple example (see `verilog-auto-inst' for what else is going on here):

	module ex_reg_input (o,i)
	   output o;
	   input i;
	   /*AUTOREGINPUT*/
           inst inst (/*AUTOINST*/);
	endmodule

Typing \\[verilog-auto] will make this into:

	module ex_reg_input (o,i)
	   output o;
	   input i;
	   /*AUTOREGINPUT*/
	   // Beginning of automatic reg inputs (for undeclared ...
	   reg [31:0]		iv;		// From inst of inst.v
	   // End of automatics
	   inst inst (/*AUTOINST*/
		      // Outputs
		      .o			(o[31:0]),
		      // Inputs
		      .iv			(iv));
	endmodule"
  (save-excursion
    ;; Point must be at insertion point.
    (let* ((indent-pt (current-indentation))
	   (modi (verilog-modi-current))
	   (sig-list (verilog-signals-combine-bus
		      (verilog-signals-not-in
		       (append (verilog-modi-get-sub-inputs modi)
			       (verilog-modi-get-sub-inouts modi))
		       (verilog-modi-get-signals modi)
		       ))))
      (forward-line 1)
      (verilog-insert-indent "// Beginning of automatic reg inputs (for undeclared instantiated-module inputs)\n")
      (verilog-insert-definition sig-list "reg" indent-pt)
      (verilog-modi-cache-add-wires modi sig-list)
      (verilog-insert-indent "// End of automatics\n")
      )))

(defun verilog-auto-wire ()
  "Expand AUTOWIRE statements, as part of \\[verilog-auto].
Make wire statements for instantiations outputs that aren't
already declared.

Limitations:
  This ONLY detects outputs of AUTOINSTants (see verilog-read-sub-decl).
  This does NOT work on memories, declare those yourself.

A simple example (see `verilog-auto-inst' for what else is going on here):

	module ex_wire (o,i)
	   output o;
	   input i;
	   /*AUTOWIRE*/
           inst inst (/*AUTOINST*/);
	endmodule

Typing \\[verilog-auto] will make this into:

	module ex_wire (o,i)
	   output o;
	   input i;
	   /*AUTOWIRE*/
	   // Beginning of automatic wires
	   wire [31:0]		ov;	// From inst of inst.v
	   // End of automatics
	   inst inst (/*AUTOINST*/
		      // Outputs
		      .ov	(ov[31:0]),
		      // Inputs
		      .i	(i));
	   wire o = | ov;
	endmodule"
  (save-excursion
    ;; Point must be at insertion point.
    (let* ((indent-pt (current-indentation))
	   (modi (verilog-modi-current))
	   (sig-list (verilog-signals-combine-bus
		      (verilog-signals-not-in
		       (append (verilog-modi-get-sub-outputs modi)
			       (verilog-modi-get-sub-inouts modi))
		       (verilog-modi-get-signals modi)
		       ))))
      (forward-line 1)
      (verilog-insert-indent "// Beginning of automatic wires (for undeclared instantiated-module outputs)\n")
      (verilog-insert-definition sig-list "wire" indent-pt)
      (verilog-modi-cache-add-wires modi sig-list)
      (verilog-insert-indent "// End of automatics\n")
      (when nil	;; Too slow on huge modules, plus makes everyone's module change
	(beginning-of-line)
	(setq pnt (point))
	(verilog-pretty-declarations)
	(goto-char pnt)
	(verilog-pretty-expr "//"))
      )))

(defun verilog-auto-output ()
  "Expand AUTOOUTPUT statements, as part of \\[verilog-auto].
Make output statements for any output signal from an /*AUTOINST*/ that
isn't used elsewhere inside the module.  This is useful for modules which
only instantiate other modules.

Limitations:
  This ONLY detects outputs of AUTOINSTants (see verilog-read-sub-decl).

  If any concatenation, or bit-subscripts are missing in the AUTOINSTant's
  instantiation, all bets are off.  (For example due to a AUTO_TEMPLATE).

A simple example (see `verilog-auto-inst' for what else is going on here):

	module ex_output (ov,i)
	   input i;
	   /*AUTOOUTPUT*/
	   inst inst (/*AUTOINST*/);
	endmodule

Typing \\[verilog-auto] will make this into:

	module ex_output (ov,i)
	   input i;
	   /*AUTOOUTPUT*/
	   // Beginning of automatic outputs (from unused autoinst outputs)
	   output [31:0]	ov;			// From inst of inst.v
	   // End of automatics
	   inst inst (/*AUTOINST*/
		      // Outputs
		      .ov			(ov[31:0]),
		      // Inputs
		      .i			(i));
	endmodule"
  (save-excursion
    ;; Point must be at insertion point.
    (let* ((indent-pt (current-indentation))
	   (modi (verilog-modi-current))
	   (sig-list (verilog-signals-not-in
		      (verilog-modi-get-sub-outputs modi)
		      (append (verilog-modi-get-outputs modi)
			      (verilog-modi-get-inouts modi)
			      (verilog-modi-get-sub-inputs modi)
			      (verilog-modi-get-sub-inouts modi)
			      ))))
      (forward-line 1)
      (verilog-insert-indent "// Beginning of automatic outputs (from unused autoinst outputs)\n")
      (verilog-insert-definition sig-list "output" indent-pt)
      (verilog-modi-cache-add-outputs modi sig-list)
      (verilog-insert-indent "// End of automatics\n")
      )))

(defun verilog-auto-output-every ()
  "Expand AUTOOUTPUTEVERY statements, as part of \\[verilog-auto].
Make output statements for any signals that aren't primary inputs or
outputs already.  This makes every signal in the design a output.  This is
useful to get Synopsys to preserve every signal in the design, since it
won't optimize away the outputs.

A simple example:

	module ex_output_every (o,i,tempa,tempb)
	   output o;
	   input i;
	   /*AUTOOUTPUTEVERY*/
	   wire tempa = i;
	   wire tempb = tempa;
	   wire o = tempb;
	endmodule

Typing \\[verilog-auto] will make this into:

	module ex_output_every (o,i,tempa,tempb)
	   output o;
	   input i;
	   /*AUTOOUTPUTEVERY*/
	   // Beginning of automatic outputs (every signal)
	   output		tempb;
	   output		tempa;
	   // End of automatics
	   wire tempa = i;
	   wire tempb = tempa;
	   wire o = tempb;
	endmodule"
  (save-excursion
    ;;Point must be at insertion point
    (let* ((indent-pt (current-indentation))
	   (modi (verilog-modi-current))
	   (sig-list (verilog-signals-combine-bus
		      (verilog-signals-not-in
		       (verilog-modi-get-signals modi)
		       (verilog-modi-get-ports modi)
		       ))))
      (forward-line 1)
      (verilog-insert-indent "// Beginning of automatic outputs (every signal)\n")
      (verilog-insert-definition sig-list "output" indent-pt)
      (verilog-modi-cache-add-outputs modi sig-list)
      (verilog-insert-indent "// End of automatics\n")
      )))

(defun verilog-auto-input ()
  "Expand AUTOINPUT statements, as part of \\[verilog-auto].
Make input statements for any input signal into an /*AUTOINST*/ that
isn't declared elsewhere inside the module.  This is useful for modules which
only instantiate other modules.

Limitations:
  This ONLY detects outputs of AUTOINSTants (see verilog-read-sub-decl).

  If any concatenation, or bit-subscripts are missing in the AUTOINSTant's
  instantiation, all bets are off.  (For example due to a AUTO_TEMPLATE).

A simple example (see `verilog-auto-inst' for what else is going on here):

	module ex_input (ov,i)
	   output [31:0] ov;
	   /*AUTOINPUT*/
	   inst inst (/*AUTOINST*/);
	endmodule

Typing \\[verilog-auto] will make this into:

	module ex_input (ov,i)
	   output [31:0] ov;
	   /*AUTOINPUT*/
	   // Beginning of automatic inputs (from unused autoinst inputs)
	   input		i;			// From inst of inst.v
	   // End of automatics
	   inst inst (/*AUTOINST*/
		      // Outputs
		      .ov			(ov[31:0]),
		      // Inputs
		      .i			(i));
	endmodule"
  (save-excursion
    (let* ((indent-pt (current-indentation))
	   (modi (verilog-modi-current))
	   (sig-list (verilog-signals-not-in
		      (verilog-modi-get-sub-inputs modi)
		      (append (verilog-modi-get-inputs modi)
			      (verilog-modi-get-inouts modi)
			      (verilog-modi-get-wires modi)
			      (verilog-modi-get-regs modi)
			      (verilog-modi-get-consts modi)
			      (verilog-modi-get-sub-outputs modi)
			      (verilog-modi-get-sub-inouts modi)
			      ))))
      (forward-line 1)
      (verilog-insert-indent "// Beginning of automatic inputs (from unused autoinst inputs)\n")
      (verilog-insert-definition sig-list "input" indent-pt)
      (verilog-modi-cache-add-inputs modi sig-list)
      (verilog-insert-indent "// End of automatics\n")
      )))

(defun verilog-auto-inout ()
  "Expand AUTOINOUT statements, as part of \\[verilog-auto].
Make inout statements for any inout signal in an /*AUTOINST*/ that
isn't declared elsewhere inside the module.

Limitations:
  This ONLY detects outputs of AUTOINSTants (see verilog-read-sub-decl).

  If any concatenation, or bit-subscripts are missing in the AUTOINSTant's
  instantiation, all bets are off.  (For example due to a AUTO_TEMPLATE).

A simple example (see `verilog-auto-inst' for what else is going on here):

	module ex_inout (ov,i)
	   input i;
	   /*AUTOINOUT*/
	   inst inst (/*AUTOINST*/);
	endmodule

Typing \\[verilog-auto] will make this into:

	module ex_inout (ov,i)
	   input i;
	   /*AUTOINOUT*/
	   // Beginning of automatic inouts (from unused autoinst inouts)
	   inout [31:0]	ov;			// From inst of inst.v
	   // End of automatics
	   inst inst (/*AUTOINST*/
		      // Inouts
		      .ov			(ov[31:0]),
		      // Inputs
		      .i			(i));
	endmodule"
  (save-excursion
    ;; Point must be at insertion point.
    (let* ((indent-pt (current-indentation))
	   (modi (verilog-modi-current))
	   (sig-list (verilog-signals-not-in
		      (verilog-modi-get-sub-inouts modi)
		      (append (verilog-modi-get-outputs modi)
			      (verilog-modi-get-inouts modi)
			      (verilog-modi-get-inputs modi)
			      (verilog-modi-get-sub-inputs modi)
			      (verilog-modi-get-sub-outputs modi)
			      ))))
      (forward-line 1)
      (verilog-insert-indent "// Beginning of automatic inouts (from unused autoinst inouts)\n")
      (verilog-insert-definition sig-list "inout" indent-pt)
      (verilog-modi-cache-add-inouts modi sig-list)
      (verilog-insert-indent "// End of automatics\n")
      )))

(defun verilog-auto-inout-module ()
  "Expand AUTOINOUTMODULE statements, as part of \\[verilog-auto].
Take input/output/inout statements from the specified module and insert
into the current module.  This is useful for making null templates and
shell modules which need to have identical I/O with another module.  Any
I/O which are already defined in this module will not be redefined.

Limitations:
  Concatenation and outputting partial busses is not supported.
  Module names must be resolvable to filenames.  See \\[verilog-auto-inst].
  Signals are not inserted in the same order as in the original module,
  though they will appear to be in the same order to a AUTOINST
  instantiating either module.

A simple example:

	module ex_shell (/*AUTOARG*/)
	   /*AUTOINOUTMODULE(\"ex_main\")*/
	endmodule

	module ex_main (i,o,io)
          input i;
          output o;
          inout io;
        endmodule

Typing \\[verilog-auto] will make this into:

	module ex_shell (/*AUTOARG*/i,o,io)
	   /*AUTOINOUTMODULE(\"ex_main\")*/
           // Beginning of automatic in/out/inouts (from specific module)
           input i;
           output o;
           inout io;
	   // End of automatics
	endmodule"
  (save-excursion
    (let* ((submod (car (verilog-read-auto-params 1))) submodi)
      ;; Lookup position, etc of co-module
      ;; Note this may raise an error
      (when (setq submodi (verilog-modi-lookup submod t))
	(let* ((indent-pt (current-indentation))
	       (modi (verilog-modi-current))
	       (sig-list-i  (verilog-signals-not-in
			     (verilog-modi-get-inputs submodi)
			     (append (verilog-modi-get-inputs modi))))
	       (sig-list-o  (verilog-signals-not-in
			     (verilog-modi-get-outputs submodi)
			     (append (verilog-modi-get-outputs modi))))
	       (sig-list-io (verilog-signals-not-in
			     (verilog-modi-get-inouts submodi)
			     (append (verilog-modi-get-inouts modi)))))
	  (forward-line 1)
	  (verilog-insert-indent "// Beginning of automatic in/out/inouts (from specific module)\n")
	  ;; Don't sort them so a upper AUTOINST will match the main module
	  (verilog-insert-definition sig-list-o  "output" indent-pt t)
	  (verilog-insert-definition sig-list-io "inout" indent-pt t)
	  (verilog-insert-definition sig-list-i  "input" indent-pt t)
	  (verilog-modi-cache-add-inputs modi sig-list-i)
	  (verilog-modi-cache-add-outputs modi sig-list-o)
	  (verilog-modi-cache-add-inouts modi sig-list-io)
	  (verilog-insert-indent "// End of automatics\n")
	  )))))

(defun verilog-auto-sense ()
  "Expand AUTOSENSE statements, as part of \\[verilog-auto].
Replace the always (/*AUTOSENSE*/) sensitivity list (/*AS*/ for short)
with one automatically derived from all inputs declared in the always
statement.  Signals that are generated within the same always block are NOT
placed into the sensitivity list (see `verilog-auto-sense-include-inputs').
Long lines are split based on the `fill-column', see \\[set-fill-column].

Limitations:
  Verilog does not allow memories (multidimensional arrays) in sensitivity
  lists.  AUTOSENSE will thus exclude them, and add a /*memory or*/ comment.

Constant signals:
  AUTOSENSE cannot always determine if a `define is a constant or a signal
  (it could be in a include file for example).  If a `define or other signal
  is put into the AUTOSENSE list and is not desired, use the AUTO_CONSTANT
  declaration anywhere in the module (parenthesis are required):

	/* AUTO_CONSTANT ( `this_is_really_constant_dont_autosense_it ) */

  Better yet, use a parameter, which will be understood to be constant
  automatically.

OOps!
  If AUTOSENSE makes a mistake, please report it.  (First try putting
  a begin/end after your always!) As a workaround, if a signal that
  shouldn't be in the sensitivity list was, use the AUTO_CONSTANT above.
  If a signal should be in the sensitivity list wasn't, placing it before
  the /*AUTOSENSE*/ comment will prevent it from being deleted when the
  autos are updated (or added if it occurs there already).

A simple example:

	   always @ (/*AUTOSENSE*/) begin
	      /* AUTO_CONSTANT (`constant) */
	      outin = ina | inb | `constant;
	      out = outin;
	   end

Typing \\[verilog-auto] will make this into:

	   always @ (/*AUTOSENSE*/ina or inb) begin
	      /* AUTO_CONSTANT (`constant) */
	      outin = ina | inb | `constant;
	      out = outin;
	   end"
  (save-excursion
    ;; Find beginning
    (let* ((start-pt (save-excursion
		       (verilog-re-search-backward "(" nil t)
		       (point)))
	   (indent-pt (save-excursion
			(or (and (goto-char start-pt) (1+ (current-column)))
			    (current-indentation))))
	   (modi (verilog-modi-current))
	   (sig-memories (verilog-signals-memory (verilog-modi-get-regs modi)))
	   sigss sig-list not-first presense-sigs)
      ;; Read signals in always, eliminate outputs from sense list
      (setq presense-sigs (verilog-signals-from-signame
			   (save-excursion
			     (verilog-read-signals start-pt (point)))))
      (setq sigss (verilog-read-always-signals))
      (setq sig-list (verilog-signals-not-params
		      (verilog-signals-not-in (verilog-alw-get-inputs sigss)
					      (append (and (not verilog-auto-sense-include-inputs)
							   (verilog-alw-get-outputs sigss))
						      (verilog-modi-get-consts modi)
						      presense-sigs)
					      )))
      (when sig-memories
	(let ((tlen (length sig-list)))
	  (setq sig-list (verilog-signals-not-in sig-list sig-memories))
	  (if (not (eq tlen (length sig-list))) (insert " /*memory or*/ "))))
      (if (and presense-sigs  ;; Add a "or" if not "(.... or /*AUTOSENSE*/"
	       (save-excursion (goto-char (point))
			       (verilog-re-search-backward "[a-zA-Z0-9$_.%`]+" start-pt t)
			       (verilog-re-search-backward "\\s-" start-pt t)
			       (while (looking-at "\\s-`endif")
				 (verilog-re-search-backward "[a-zA-Z0-9$_.%`]+" start-pt t)
				 (verilog-re-search-backward "\\s-" start-pt t))
			       (not (looking-at "\\s-or\\b"))))
	  (setq not-first t))
      (setq sig-list (sort sig-list `verilog-signals-sort-compare))
      (while sig-list
	(cond ((> (+ 4 (current-column) (length (verilog-sig-name (car sig-list)))) fill-column) ;+4 for width of or
	       (insert "\n")
	       (indent-to indent-pt)
	       (if not-first (insert "or ")))
	      (not-first (insert " or ")))
	(insert (verilog-sig-name (car sig-list)))
	(setq sig-list (cdr sig-list)
	      not-first t))
      )))

(defun verilog-auto-reset ()
  "Expand AUTORESET statements, as part of \\[verilog-auto].
Replace the /*AUTORESET*/ comment with code to initialize all
registers set elsewhere in the always block.

Limitations:
  AUTORESET will not clear memories.
  AUTORESET uses <= if there are any <= in the block, else it uses =.

/*AUTORESET*/ presumes that any signals mentioned between the previous
begin/case/if statement and the AUTORESET comment are being reset manually
and should not be automatically reset.  This includes ommitting any signals
used on the right hand side of assignments.

A simple example:

    always @(posedge clk or negedge reset_l) begin
        if (!reset_l) begin
            c <= 1;
            /*AUTORESET*/
        end
        else begin
            a <= in_a;
            b <= in_b;
            c <= in_c;
        end
    end

Typing \\[verilog-auto] will make this into:

    always @(posedge core_clk or negedge reset_l) begin
        if (!reset_l) begin
            c <= 1;
            /*AUTORESET*/
            // Beginning of autoreset for uninitialized flops
            a <= 0;
            b <= 0;
            // End of automatics
        end
        else begin
            a <= in_a;
            b <= in_b;
            c <= in_c;
        end
    end"

  (interactive)
  (save-excursion
    ;; Find beginning
    (let* ((indent-pt (current-indentation))
	   (modi (verilog-modi-current))
	   (all-list (verilog-modi-get-signals modi))
	   sigss sig-list prereset-sigs assignment-str)
      ;; Read signals in always, eliminate outputs from reset list
      (setq prereset-sigs (verilog-signals-from-signame
			   (save-excursion
			     (verilog-read-signals
			      (save-excursion
				(verilog-re-search-backward "\\(@\\|\\<begin\\>\\|\\<if\\>\\|\\<case\\>\\)" nil t)
				(point))
			      (point)))))
      (save-excursion
	(verilog-re-search-backward "@" nil t)
        (setq sigss (verilog-read-always-signals)))
      (setq assignment-str (if (verilog-alw-get-uses-delayed sigss)
			       (concat " <= " verilog-assignment-delay)
			     " = "))
      (setq sig-list (verilog-signals-not-in (verilog-alw-get-outputs sigss)
					     prereset-sigs))
      (setq sig-list (sort sig-list `verilog-signals-sort-compare))
      (insert "\n");
      (indent-to indent-pt)
      (insert "// Beginning of autoreset for uninitialized flops\n");
      (indent-to indent-pt)
      (while sig-list
	(let* ((defn-sig (if verilog-auto-reset-widths
			     (assoc (verilog-sig-name (car sig-list)) all-list)))
	       (width (if defn-sig (verilog-sig-width defn-sig))))
	  (insert (verilog-sig-name (car sig-list)))
	  (cond ((not width)
		 (insert assignment-str "0;\n"))
		((string-match "^[0-9]+$" width)
		 (insert assignment-str (verilog-sig-width defn-sig) "'h0;\n" ))
		(t
		 (insert assignment-str "{" (verilog-sig-width defn-sig) "{1'b0}};\n" )))
	  (indent-to indent-pt)
	  (setq sig-list (cdr sig-list))))
      (insert "// End of automatics")
      )))

(defun verilog-enum-ascii (signm elim-regexp)
  "Convert a enum name SIGNM to a ascii string for insertion.
Remove user provided prefix ELIM-REGEXP."
  (or elim-regexp (setq elim-regexp "_ DONT MATCH IT_"))
  (let ((case-fold-search t))
    ;; All upper becomes all lower for readability
    (downcase (verilog-string-replace-matches elim-regexp "" nil nil signm))))

(defun verilog-auto-ascii-enum ()
  "Expand AUTOASCIIENUM statements, as part of \\[verilog-auto].
Create a register to contain the ASCII decode of a enumerated signal type.
This will allow trace viewers to show the ASCII name of states.

First, parameters are built into a enumeration using the synopsys enum
  comment.  The comment must be between the keyword and the symbol.
  (Annoying, but that's what Synopsys's dc_shell FSM reader requires.)

Next, registers which that enum applies to are also tagged with the same
  enum.  Synopsys also suggests labeling state vectors, but `verilog-mode'
  doesn't care.

Finally, a AUTOASCIIENUM command is used.
  The first parameter is the name of the signal to be decoded.

  The second parameter is the name to store the ASCII code into.  For the
  signal foo, I suggest the name _foo__ascii, where the leading _ indicates
  a signal that is just for simulation, and the magic characters _ascii
  tell viewers like Dinotrace to display in ASCII format.

  The final optional parameter is a string which will be removed from the
  state names.


A simple example:

   //== State enumeration
   parameter [2:0] // synopsys enum state_info
		   SM_IDLE =  3'b000,
		   SM_SEND =  3'b001,
		   SM_WAIT1 = 3'b010;
   //== State variables
   reg [2:0]	/* synopsys enum state_info */
		state_r;		/* synopsys state_vector state_r */
   reg [2:0]	/* synopsys enum state_info */
		state_e1;

   //== ASCII state decoding

   /*AUTOASCIIENUM(\"state_r\", \"_stateascii_r\", \"sm_\")*/

Typing \\[verilog-auto] will make this into:

   ... same front matter ...

   /*AUTOASCIIENUM(\"state_r\", \"_stateascii_r\", \"sm_\")*/
   // Beginning of automatic ASCII enum decoding
   reg [39:0]		_stateascii_r;		// Decode of state_r
   always @(state_r) begin
      case ({state_r})
	SM_IDLE:  _stateascii_r = \"idle \";
	SM_SEND:  _stateascii_r = \"send \";
	SM_WAIT1: _stateascii_r = \"wait1\";
	default:  _stateascii_r = \"%Erro\";
      endcase
   end
   // End of automatics"
  (save-excursion
    (let* ((params (verilog-read-auto-params 2 3))
	   (undecode-name (nth 0 params))
	   (ascii-name (nth 1 params))
	   (elim-regexp (nth 2 params))
	   ;;
	   (indent-pt (current-indentation))
	   (modi (verilog-modi-current))
	   ;;
	   (sig-list-consts (verilog-modi-get-consts modi))
	   (sig-list-all  (append (verilog-modi-get-regs modi)
				  (verilog-modi-get-outputs modi)
				  (verilog-modi-get-inouts modi)
				  (verilog-modi-get-inputs modi)
				  (verilog-modi-get-wires modi)))
	   ;;
	   (undecode-sig (or (assoc undecode-name sig-list-all)
			     (error "%s: Signal %s not found in design" (verilog-point-text) undecode-name)))
	   (undecode-enum (or (verilog-sig-enum undecode-sig)
			      (error "%s: Signal %s does not have a enum tag" (verilog-point-text) undecode-name)))
	   ;;
	   (enum-sigs (or (verilog-signals-matching-enum sig-list-consts undecode-enum)
			  (error "%s: No state definitions for %s" (verilog-point-text) undecode-enum)))
	   ;;
	   (enum-chars 0)
	   (ascii-chars 0))
      ;;
      ;; Find number of ascii chars needed
      (let ((tmp-sigs enum-sigs))
	(while tmp-sigs
	  (setq enum-chars (max enum-chars (length (verilog-sig-name (car tmp-sigs))))
		ascii-chars (max ascii-chars (length (verilog-enum-ascii
						      (verilog-sig-name (car tmp-sigs))
						      elim-regexp)))
		tmp-sigs (cdr tmp-sigs))))
      ;;
      (forward-line 1)
      (verilog-insert-indent "// Beginning of automatic ASCII enum decoding\n")
      (let ((decode-sig-list (list (list ascii-name (format "[%d:0]" (- (* ascii-chars 8) 1))
					 (concat "Decode of " undecode-name) nil nil))))
	(verilog-insert-definition decode-sig-list "reg" indent-pt)
	(verilog-modi-cache-add-regs modi decode-sig-list))
      ;;
      (verilog-insert-indent "always @(" undecode-name ") begin\n")
      (setq indent-pt (+ indent-pt verilog-indent-level))
      (indent-to indent-pt)
      (insert "case ({" undecode-name "})\n")
      (setq indent-pt (+ indent-pt verilog-case-indent))
      ;;
      (let ((tmp-sigs enum-sigs)
	    (chrfmt (format "%%-%ds %s = \"%%-%ds\";\n" (1+ (max 8 enum-chars))
			    ascii-name ascii-chars))
	    (errname (substring "%Error" 0 (min 6 ascii-chars))))
	(while tmp-sigs
	  (verilog-insert-indent
	   (format chrfmt (concat (verilog-sig-name (car tmp-sigs)) ":")
		   (verilog-enum-ascii (verilog-sig-name (car tmp-sigs))
				       elim-regexp)))
	  (setq tmp-sigs (cdr tmp-sigs)))
	(verilog-insert-indent (format chrfmt "default:" errname)))
      ;;
      (setq indent-pt (- indent-pt verilog-case-indent))
      (verilog-insert-indent "endcase\n")
      (setq indent-pt (- indent-pt verilog-indent-level))
      (verilog-insert-indent "end\n"
			     "// End of automatics\n")
      )))

(defun verilog-auto-templated-rel ()
  "Replace Templated relative line numbers with absolute line numbers.
Internal use only. This hacks around the line numbers in AUTOINST Templates
being different from the final output's line numbering."
  (let ((templateno 0) (template-line (list 0)))
    ;; Find line number each template is on
    (goto-char (point-min))
    (while (search-forward "AUTO_TEMPLATE" nil t)
      (setq templateno (1+ templateno))
      (setq template-line (cons (count-lines (point-min) (point)) template-line)))
    (setq template-line (nreverse template-line))
    ;; Replate T# L# with absolute line number
    (goto-char (point-min))
    (while (re-search-forward " Templated T\\([0-9]+\\) L\\([0-9]+\\)" nil t)
      (replace-match (concat " Templated "
			     (int-to-string (+ (nth (string-to-int (match-string 1))
						    template-line)
					       (string-to-int (match-string 2)))))
		     t t))))


;;
;; Auto top level
;;

(defun verilog-auto ()
  "Expand AUTO statements.
Look for any /*AUTO...*/ commands in the code, as used in
instantiations or argument headers.  Update the list of signals
following the /*AUTO...*/ command.

Use \\[verilog-delete-auto] to remove the AUTOs.

The hooks `verilog-before-auto-hook' and `verilog-auto-hook' are
called before and after this function, respectively.

For example:
	module (/*AUTOARG*/)
	/*AUTOINPUT*/
	/*AUTOOUTPUT*/
	/*AUTOWIRE*/
	/*AUTOREG*/
	somesub sub (/*AUTOINST*/);

You can also update the AUTOs from the shell using:
	emacs --batch $FILENAME_V -f verilog-auto -f save-buffer

Using \\[describe-function], see also:
   `verilog-auto-arg'          for AUTOARG module instantiations
   `verilog-auto-ascii-enum'   for AUTOASCIIENUM enumeration decoding
   `verilog-auto-inout-module' for AUTOINOUTMODULE copying i/o from elsewhere
   `verilog-auto-inout'        for AUTOINOUT making hierarchy inouts
   `verilog-auto-input'        for AUTOINPUT making hierarchy inputs
   `verilog-auto-inst'         for AUTOINST argument declarations
   `verilog-auto-output'       for AUTOOUTPUT making hierarchy outputs
   `verilog-auto-output-every' for AUTOOUTPUTEVERY making all outputs
   `verilog-auto-reg'          for AUTOREG registers
   `verilog-auto-reg-input'    for AUTOREGINPUT instantiation registers
   `verilog-auto-reset'        for AUTORESET flop resets
   `verilog-auto-sense'        for AUTOSENSE always sensitivity lists
   `verilog-auto-wire'         for AUTOWIRE instantiation wires

   `verilog-read-defines'      for reading `define values
   `verilog-read-includes'     for reading `includes

If you have bugs with these autos, try contacting the AUTOAUTHOR
Wilson Snyder (wsnyder@wsnyder.org or wsnyder@world.std.com)"
  (interactive)
  (unless noninteractive (message "Updating AUTOs..."))
  (if (featurep 'dinotrace)
      (dinotrace-unannotate-all))
  (let ((oldbuf (if (not (buffer-modified-p))
		    (buffer-string)))
	;; Before version 20, match-string with font-lock returns a
	;; vector that is not equal to the string.  IE if on "input"
	;; nil==(equal "input" (progn (looking-at "input") (match-string 0)))
	(fontlocked (when (and ;(memq 'v19 verilog-emacs-features)
			       (boundp 'font-lock-mode)
			       font-lock-mode)
		      (font-lock-mode nil)
		      t)))
    (save-excursion
      (run-hooks 'verilog-before-auto-hook)
      ;; Try to save the user from needing to revert-file to reread file local-variables
      (verilog-auto-reeval-locals)
      ;; These two may seem obvious to do always, but on large includes it can be way too slow
      (when verilog-auto-read-includes
	(verilog-read-includes)
	(verilog-read-defines))
      ;; This particular ordering is important
      ;; INST: Lower modules correct, no internal dependencies, FIRST
      (verilog-preserve-cache
       ;; Clear existing autos else we'll be screwed by existing ones
       (verilog-delete-auto)
       ;;
       (verilog-auto-search-do "/*AUTOINST*/" 'verilog-auto-inst)
       ;; Doesn't matter when done, but combine it with a common changer
       (verilog-auto-re-search-do "/\\*\\(AUTOSENSE\\|AS\\)\\*/" 'verilog-auto-sense)
       (verilog-auto-re-search-do "/\\*AUTORESET\\*/" 'verilog-auto-reset)
       ;; Must be done before autoin/out as creates a reg
       (verilog-auto-re-search-do "/\\*AUTOASCIIENUM([^)]*)\\*/" 'verilog-auto-ascii-enum)
       ;;
       ;; first in/outs from other files
       (verilog-auto-re-search-do "/\\*AUTOINOUTMODULE([^)]*)\\*/" 'verilog-auto-inout-module)
       ;; next in/outs which need previous sucked inputs first
       (verilog-auto-search-do "/*AUTOOUTPUT*/" 'verilog-auto-output)
       (verilog-auto-search-do "/*AUTOINPUT*/" 'verilog-auto-input)
       (verilog-auto-search-do "/*AUTOINOUT*/" 'verilog-auto-inout)
       ;; Wires/regs must be after inputs/outputs
       (verilog-auto-search-do "/*AUTOWIRE*/" 'verilog-auto-wire)
       (verilog-auto-search-do "/*AUTOREG*/" 'verilog-auto-reg)
       (verilog-auto-search-do "/*AUTOREGINPUT*/" 'verilog-auto-reg-input)
       ;; outputevery needs autooutputs done first
       (verilog-auto-search-do "/*AUTOOUTPUTEVERY*/" 'verilog-auto-output-every)
       ;; Must be after all inputs outputs are generated
       (verilog-auto-search-do "/*AUTOARG*/" 'verilog-auto-arg)
       ;; Fix line numbers (comments only)
       (verilog-auto-templated-rel)
       )
      ;;
      (run-hooks 'verilog-auto-hook)
      ;;
      (set (make-local-variable 'verilog-auto-update-tick) (buffer-modified-tick))
      ;;
      ;; If end result is same as when started, clear modified flag
      (cond ((and oldbuf (equal oldbuf (buffer-string)))
	     (set-buffer-modified-p nil)
	     (unless noninteractive (message "Updating AUTOs...done (no changes)")))
	    (t (unless noninteractive (message "Updating AUTOs...done"))))
      ;; Restore font-lock
      (when fontlocked (font-lock-mode t))
      )))


;;
;; Skeleton based code insertion
;;
(defvar verilog-template-map nil
  "Keymap used in Verilog mode for smart template operations.")

(let ((verilog-mp (make-sparse-keymap)))
  (define-key verilog-mp "a" 'verilog-sk-always)
  (define-key verilog-mp "b" 'verilog-sk-begin)
  (define-key verilog-mp "c" 'verilog-sk-case)
  (define-key verilog-mp "e" 'verilog-sk-else)
  (define-key verilog-mp "f" 'verilog-sk-for)
  (define-key verilog-mp "g" 'verilog-sk-generate)
  (define-key verilog-mp "h" 'verilog-sk-header)
  (define-key verilog-mp "i" 'verilog-sk-initial)
  (define-key verilog-mp "j" 'verilog-sk-fork)
  (define-key verilog-mp "m" 'verilog-sk-module)
  (define-key verilog-mp "p" 'verilog-sk-primitive)
  (define-key verilog-mp "r" 'verilog-sk-repeat)
  (define-key verilog-mp "s" 'verilog-sk-specify)
  (define-key verilog-mp "t" 'verilog-sk-task)
  (define-key verilog-mp "w" 'verilog-sk-while)
  (define-key verilog-mp "x" 'verilog-sk-casex)
  (define-key verilog-mp "z" 'verilog-sk-casez)
  (define-key verilog-mp "?" 'verilog-sk-if)
  (define-key verilog-mp ":" 'verilog-sk-else-if)
  (define-key verilog-mp "/" 'verilog-sk-comment)
  (define-key verilog-mp "A" 'verilog-sk-assign)
  (define-key verilog-mp "F" 'verilog-sk-function)
  (define-key verilog-mp "I" 'verilog-sk-input)
  (define-key verilog-mp "O" 'verilog-sk-output)
  (define-key verilog-mp "S" 'verilog-sk-state-machine)
  (define-key verilog-mp "=" 'verilog-sk-inout)
  (define-key verilog-mp "W" 'verilog-sk-wire)
  (define-key verilog-mp "R" 'verilog-sk-reg)
  (setq verilog-template-map verilog-mp))

;;
;; Place the templates into Verilog Mode.  They may be inserted under any key.
;; C-c C-t will be the default.  If you use templates a lot, you
;; may want to consider moving the binding to another key in your .emacs
;; file.
;;
;(define-key verilog-mode-map "\C-ct" verilog-template-map)
(define-key verilog-mode-map "\C-c\C-t" verilog-template-map)

;;; ---- statement skeletons ------------------------------------------

(define-skeleton verilog-sk-prompt-condition
  "Prompt for the loop condition."
  "[condition]: " str )

(define-skeleton verilog-sk-prompt-init
  "Prompt for the loop init statement."
  "[initial statement]: " str )

(define-skeleton verilog-sk-prompt-inc
  "Prompt for the loop increment statement."
  "[increment statement]: " str )

(define-skeleton verilog-sk-prompt-name
  "Prompt for the name of something."
  "[name]: " str)

(define-skeleton verilog-sk-prompt-clock
  "Prompt for the name of something."
  "name and edge of clock(s): " str)

(defvar verilog-sk-reset nil)
(defun verilog-sk-prompt-reset ()
  "Prompt for the name of a state machine reset."
  (setq verilog-sk-reset (read-input "name of reset: " "rst")))


(define-skeleton verilog-sk-prompt-state-selector
  "Prompt for the name of a state machine selector."
  "name of selector (eg {a,b,c,d}): " str )

(define-skeleton verilog-sk-prompt-output
  "Prompt for the name of something."
  "output: " str)

(define-skeleton verilog-sk-prompt-msb
  "Prompt for least signifcant bit specification."
  "msb:" str & ?: & (verilog-sk-prompt-lsb) | -1 )

(define-skeleton verilog-sk-prompt-lsb
  "Prompt for least signifcant bit specification."
  "lsb:" str )

(defvar verilog-sk-p nil)
(define-skeleton verilog-sk-prompt-width
  "Prompt for a width specification."
  ()
  (progn (setq verilog-sk-p (point)) nil)
  (verilog-sk-prompt-msb)
  (if (> (point) verilog-sk-p) "] " " "))

(defun verilog-sk-header ()
  "Insert a descriptive header at the top of the file."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (verilog-sk-header-tmpl)))

(define-skeleton verilog-sk-header-tmpl
  "Insert a comment block containing the module title, author, etc."
  "[Description]: "
  "//                              -*- Mode: Verilog -*-"
  "\n// Filename        : " (buffer-name)
  "\n// Description     : " str
  "\n// Author          : " (user-full-name)
  "\n// Created On      : " (current-time-string)
  "\n// Last Modified By: ."
  "\n// Last Modified On: ."
  "\n// Update Count    : 0"
  "\n// Status          : Unknown, Use with caution!"
  "\n")

(define-skeleton verilog-sk-module
  "Insert a module definition."
  ()
  > "module " (verilog-sk-prompt-name) " (/*AUTOARG*/ ) ;" \n
  > _ \n
  > (- verilog-indent-level-behavioral) "endmodule" (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-primitive
  "Insert a task definition."
  ()
  > "primitive " (verilog-sk-prompt-name) " ( " (verilog-sk-prompt-output) ("input:" ", " str ) " );"\n
  > _ \n
  > (- verilog-indent-level-behavioral) "endprimitive" (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-task
  "Insert a task definition."
  ()
  > "task " (verilog-sk-prompt-name) & ?; \n
  > _ \n
  > "begin" \n
  > \n
  > (- verilog-indent-level-behavioral) "end" \n
  > (- verilog-indent-level-behavioral) "endtask" (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-function
  "Insert a function definition."
  ()
  > "function [" (verilog-sk-prompt-width) | -1 (verilog-sk-prompt-name) ?; \n
  > _ \n
  > "begin" \n
  > \n
  > (- verilog-indent-level-behavioral) "end" \n
  > (- verilog-indent-level-behavioral) "endfunction" (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-always
  "Insert always block.  Uses the minibuffer to prompt
for sensitivity list."
  ()
  > "always @ ( /*AUTOSENSE*/ ) begin\n"
  > _ \n
  > (- verilog-indent-level-behavioral) "end" \n >
  )

(define-skeleton verilog-sk-initial
  "Insert an initial block."
  ()
  > "initial begin\n"
  > _ \n
  > (- verilog-indent-level-behavioral) "end" \n > )

(define-skeleton verilog-sk-specify
  "Insert specify block.  "
  ()
  > "specify\n"
  > _ \n
  > (- verilog-indent-level-behavioral) "endspecify" \n > )

(define-skeleton verilog-sk-generate
  "Insert generate block.  "
  ()
  > "generate\n"
  > _ \n
  > (- verilog-indent-level-behavioral) "endgenerate" \n > )

(define-skeleton verilog-sk-begin
  "Insert begin end block.  Uses the minibuffer to prompt for name"
  ()
  > "begin" (verilog-sk-prompt-name) \n
  > _ \n
  > (- verilog-indent-level-behavioral) "end"
)

(define-skeleton verilog-sk-fork
  "Insert an fork join block."
  ()
  > "fork\n"
  > "begin" \n
  > _ \n
  > (- verilog-indent-level-behavioral) "end" \n
  > "begin" \n
  > \n
  > (- verilog-indent-level-behavioral) "end" \n
  > (- verilog-indent-level-behavioral) "join" \n
  > )


(define-skeleton verilog-sk-case
  "Build skeleton case statement, prompting for the selector expression,
and the case items."
  "[selector expression]: "
  > "case (" str ") " \n
  > ("case selector: " str ": begin" \n > _ \n > (- verilog-indent-level-behavioral) "end" \n )
  resume: >  (- verilog-case-indent) "endcase" (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-casex
  "Build skeleton casex statement, prompting for the selector expression,
and the case items."
  "[selector expression]: "
  > "casex (" str ") " \n
  > ("case selector: " str ": begin" \n > _ \n > (- verilog-indent-level-behavioral) "end" \n )
  resume: >  (- verilog-case-indent) "endcase" (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-casez
  "Build skeleton casez statement, prompting for the selector expression,
and the case items."
  "[selector expression]: "
  > "casez (" str ") " \n
  > ("case selector: " str ": begin" \n > _ \n > (- verilog-indent-level-behavioral) "end" \n )
  resume: >  (- verilog-case-indent) "endcase" (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-if
  "Insert a skeleton if statement."
  > "if (" (verilog-sk-prompt-condition) & ")" " begin" \n
  > _ \n
  > (- verilog-indent-level-behavioral) "end " \n )

(define-skeleton verilog-sk-else-if
  "Insert a skeleton else if statement."
  > (verilog-indent-line) "else if ("
  (progn (setq verilog-sk-p (point)) nil) (verilog-sk-prompt-condition) (if (> (point) verilog-sk-p) ") " -1 ) & " begin" \n
  > _ \n
  > "end" (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-datadef
  "Common routine to get data definition"
  ()
  (verilog-sk-prompt-width) | -1 ("name (RET to end):" str ", ") -2 ";" \n)

(define-skeleton verilog-sk-input
  "Insert an input definition."
  ()
  > "input  [" (verilog-sk-datadef))

(define-skeleton verilog-sk-output
  "Insert an output definition."
  ()
  > "output [" (verilog-sk-datadef))

(define-skeleton verilog-sk-inout
  "Insert an inout definition."
  ()
  > "inout  [" (verilog-sk-datadef))

(define-skeleton verilog-sk-reg
  "Insert a reg definition."
  ()
  > "reg    [" (verilog-sk-datadef))

(define-skeleton verilog-sk-wire
  "Insert a wire definition."
  ()
  > "wire   [" (verilog-sk-datadef))

(define-skeleton verilog-sk-assign
  "Insert a skeleton assign statement."
  ()
  > "assign " (verilog-sk-prompt-name) " = " _ ";" \n)

(define-skeleton verilog-sk-while
  "Insert a skeleton while loop statement."
  ()
  > "while ("  (verilog-sk-prompt-condition)  ") begin" \n
  > _ \n
  > (- verilog-indent-level-behavioral) "end " (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-repeat
  "Insert a skeleton repeat loop statement."
  ()
  > "repeat ("  (verilog-sk-prompt-condition)  ") begin" \n
  > _ \n
  > (- verilog-indent-level-behavioral) "end " (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-for
  "Insert a skeleton while loop statement."
  ()
  > "for ("
  (verilog-sk-prompt-init) "; "
  (verilog-sk-prompt-condition) "; "
  (verilog-sk-prompt-inc)
  ") begin" \n
  > _ \n
  > (- verilog-indent-level-behavioral) "end " (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-comment
  "Inserts three comment lines, making a display comment."
  ()
  > "/*\n"
  > "* " _ \n
  > "*/")

(define-skeleton verilog-sk-state-machine
  "Insert a state machine definition."
  "Name of state variable: "
  '(setq input "state")
  > "// State registers for " str | -23 \n
  '(setq verilog-sk-state str)
  > "reg [" (verilog-sk-prompt-width) | -1 verilog-sk-state ", next_" verilog-sk-state ?; \n
  '(setq input nil)
  > \n
  > "// State FF for " verilog-sk-state \n
  > "always @ ( " (read-string "clock:" "posedge clk") " or " (verilog-sk-prompt-reset) " ) begin" \n
  > "if ( " verilog-sk-reset " ) " verilog-sk-state " = 0; else" \n
  > verilog-sk-state " = next_" verilog-sk-state ?; \n
  > (- verilog-indent-level-behavioral) "end" (progn (electric-verilog-terminate-line) nil)
  > \n
  > "// Next State Logic for " verilog-sk-state \n
  > "always @ ( /*AUTOSENSE*/ ) begin\n"
  > "case (" (verilog-sk-prompt-state-selector) ") " \n
  > ("case selector: " str ": begin" \n > "next_" verilog-sk-state " = " _ ";" \n > (- verilog-indent-level-behavioral) "end" \n )
  resume: >  (- verilog-case-indent) "endcase" (progn (electric-verilog-terminate-line) nil)
  > (- verilog-indent-level-behavioral) "end" (progn (electric-verilog-terminate-line) nil))

;; Eliminate compile warning
(eval-when-compile
  (if (not (boundp 'mode-popup-menu))
      (defvar mode-popup-menu nil "Compatibility with XEmacs.")))

;; ---- add menu 'Statements' in Verilog mode (MH)
(defun verilog-add-statement-menu ()
  "Add the menu 'Statements' to the menu bar in Verilog mode."
  (easy-menu-define verilog-stmt-menu verilog-mode-map
		    "Menu for statement templates in Verilog."
		    '("Statements"
		      ["Header"		verilog-sk-header  t]
		      ["Comment"	verilog-sk-comment t]
		      "----"
		      ["Module"		verilog-sk-module t]
		      ["Primitive"	verilog-sk-primitive t]
		      "----"
		      ["Input"		verilog-sk-input t]
		      ["Output"		verilog-sk-output t]
		      ["Inout"		verilog-sk-inout t]
		      ["Wire"		verilog-sk-wire t]
		      ["Reg"		verilog-sk-reg t]
		      "----"
		      ["Initial"	verilog-sk-initial t]
		      ["Always" 	verilog-sk-always t]
		      ["Function"	verilog-sk-function t]
		      ["Task" 		verilog-sk-task t]
		      ["Specify"	verilog-sk-specify t]
		      ["Generate"	verilog-sk-generate t]
		      "----"
		      ["Begin"		verilog-sk-begin t]
		      ["If" 		verilog-sk-if t]
		      ["(if) else"	verilog-sk-else-if t]
		      ["For" 		verilog-sk-for t]
		      ["While" 		verilog-sk-while t]
		      ["Fork" 		verilog-sk-fork t]
		      ["Repeat" 	verilog-sk-repeat t]
		      ["Case" 		verilog-sk-case t]
		      ["Casex" 		verilog-sk-casex t]
		      ["Casez" 		verilog-sk-casez t]
		      ))
  (if verilog-running-on-xemacs
      (progn
	(easy-menu-add verilog-stmt-menu)
	(setq mode-popup-menu (cons "Verilog Mode" verilog-stmt-menu)))))

(add-hook 'verilog-mode-hook 'verilog-add-statement-menu)



;;
;; Include file loading with mouse/return event
;;
;; idea & first impl.: M. Rouat (eldo-mode.el)
;; second (emacs/xemacs) impl.: G. Van der Plas (spice-mode.el)

(if verilog-running-on-xemacs
    (require 'overlay)
  (require 'lucid)) ;; what else can we do ??

(defconst verilog-include-file-regexp
  "^`include\\s-+\"\\([^\n\"]*\\)\""
  "Regexp that matches the include file")

(defvar verilog-mode-mouse-map nil
  "Map containing mouse bindings for verilog-mode.")

(if verilog-mode-mouse-map
    ()
  (let ((map (make-sparse-keymap))) ; as described in info pages, make a map
    (set-keymap-parent map verilog-mode-map)
    ;; mouse button bindings
    (define-key map "\r"            'verilog-load-file-at-point)
    (if verilog-running-on-xemacs
	(define-key map 'button2    'verilog-load-file-at-mouse);ffap-at-mouse ?
      (define-key map [mouse-2]     'verilog-load-file-at-mouse))
    (if verilog-running-on-xemacs
	(define-key map 'Sh-button2 'mouse-yank) ; you wanna paste don't you ?
      (define-key map [S-mouse-2]   'mouse-yank-at-click))
    (setq verilog-mode-mouse-map map))) ;; copy complete map now

;; create set-extent-keymap procedure when it does not exist
(eval-and-compile
  (unless (fboundp 'set-extent-keymap)
    (defun set-extent-keymap (extent keymap)
      "fallback version of set-extent-keymap (for emacs 2[01])"
      (set-extent-property extent 'local-map keymap))))

(defun verilog-colorize-include-files (beg end old-len)
  "This function colorises included files when the mouse passes over them.
Clicking on the middle-mouse button loads them in a buffer (as in dired)."
  (save-excursion
    (save-match-data
      (let (end-point)
	(goto-char end)
	(setq end-point (verilog-get-end-of-line))
	(goto-char beg)
	(beginning-of-line)  ; scan entire line !
	;; delete overlays existing on this line
	(let ((overlays (overlays-in (point) end-point)))
	  (while overlays
	    (if (and
		 (overlay-get (car overlays) 'detachable)
		 (overlay-get (car overlays) 'verilog-include-file))
		(delete-overlay (car overlays)))
	    (setq overlays (cdr overlays)))) ; let
	;; make new ones, could reuse deleted one ?
	(while (search-forward-regexp verilog-include-file-regexp end-point t)
	  (let (extent)
	    (goto-char (match-beginning 1))
	    (or (extent-at (point) (buffer-name) 'mouse-face) ;; not yet extended
		(progn
		  (setq extent (make-extent (match-beginning 1) (match-end 1)))
		  (set-extent-property extent 'start-closed 't)
		  (set-extent-property extent 'end-closed 't)
		  (set-extent-property extent 'detachable 't)
		  (set-extent-property extent 'verilog-include-file 't)
		  (set-extent-property extent 'mouse-face 'highlight)
		  (set-extent-keymap extent verilog-mode-mouse-map)))))))))


(defun verilog-colorize-include-files-buffer ()
  (interactive)
  ;; delete overlays
  (let ((overlays (overlays-in (point-min) (point-max))))
    (while overlays
      (if (and
	   (overlay-get (car overlays) 'detachable)
	   (overlay-get (car overlays) 'verilog-include-file))
	  (delete-overlay (car overlays)))
      (setq overlays (cdr overlays)))) ; let
  ;; remake overlays
  (verilog-colorize-include-files (point-min) (point-max) nil))

;; ffap-at-mouse isn't useful for verilog mode. It uses library paths.
;; so define this function to do more or less the same as ffap-at-mouse
;; but first resolve filename...
(defun verilog-load-file-at-mouse (event)
  "loads file under button 2 click. Files are checked based on
`verilog-library-directories'."
  (interactive "@e")
  (save-excursion ;; implement a verilog specific ffap-at-mouse
    (mouse-set-point event)
    (beginning-of-line)
    (if (looking-at verilog-include-file-regexp)
	(if (and (car (verilog-library-filenames
		       (match-string 1) (buffer-file-name)))
		 (file-readable-p (car (verilog-library-filenames
					(match-string 1) (buffer-file-name)))))
	    (find-file (car (verilog-library-filenames
			     (match-string 1) (buffer-file-name))))
	  (progn
	    (message
	     "File '%s' isn't readable, use shift-mouse2 to paste in this field"
	     (match-string 1))))
      )))

;; ffap isn't useable for verilog mode. It uses library paths.
;; so define this function to do more or less the same as ffap
;; but first resolve filename...
(defun verilog-load-file-at-point ()
  "loads file under point. Files are checked based on
`verilog-library-directories'."
  (interactive)
  (save-excursion ;; implement a verilog specific ffap
    (beginning-of-line)
    (if (looking-at verilog-include-file-regexp)
	(if (and
	     (car (verilog-library-filenames
		   (match-string 1) (buffer-file-name)))
	     (file-readable-p (car (verilog-library-filenames
				    (match-string 1) (buffer-file-name)))))
	    (find-file (car (verilog-library-filenames
			     (match-string 1) (buffer-file-name))))))
      ))


;;
;; Bug reporting
;;

(defun verilog-submit-bug-report ()
  "Submit via mail a bug report on lazy-lock.el."
  (interactive)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     "verilog-mode-bugs@surefirev.com"
     (concat "verilog-mode v" (substring verilog-mode-version 12 -3))
     '(
       verilog-align-ifelse
       verilog-auto-endcomments
       verilog-auto-hook
       verilog-auto-indent-on-newline
       verilog-auto-inst-vector
       verilog-auto-inst-template-numbers
       verilog-auto-lineup
       verilog-auto-newline
       verilog-auto-save-policy
       verilog-auto-sense-defines-constant
       verilog-auto-sense-include-inputs
       verilog-before-auto-hook
       verilog-case-indent
       verilog-cexp-indent
       verilog-compiler
       verilog-coverage
       verilog-highlight-translate-off
       verilog-indent-begin-after-if
       verilog-indent-declaration-macros
       verilog-indent-level
       verilog-indent-level-behavioral
       verilog-indent-level-declaration
       verilog-indent-level-directive
       verilog-indent-level-module
       verilog-indent-lists
       verilog-library-directories
       verilog-library-extensions
       verilog-library-files
       verilog-linter
       verilog-minimum-comment-distance
       verilog-mode-hook
       verilog-simulator
       verilog-tab-always-indent
       verilog-tab-to-comment
       )
     nil nil
     (concat "Hi Mac,

I want to report a bug.  I've read the `Bugs' section of `Info' on
Emacs, so I know how to make a clear and unambiguous report.  To get
to that Info section, I typed

M-x info RET m " invocation-name " RET m bugs RET

Before I go further, I want to say that Verilog mode has changed my life.
I save so much time, my files are colored nicely, my co workers respect
my coding ability... until now.  I'd really appreciate anything you
could do to help me out with this minor deficiency in the product.

To reproduce the bug, start a fresh Emacs via " invocation-name "
-no-init-file -no-site-file'.  In a new buffer, in verilog mode, type
the code included below.

If you have bugs with the AUTO functions, please CC the AUTOAUTHOR
Wilson Snyder (wsnyder@wsnyder.org or wsnyder@world.std.com)

Given those lines, I expected [[Fill in here]] to happen;
but instead, [[Fill in here]] happens!.

== The code: =="))))

;; Local Variables:
;; checkdoc-permit-comma-termination-flag:t
;; checkdoc-force-docstrings-flag:nil
;; End:

;;; verilog-mode.el ends here
