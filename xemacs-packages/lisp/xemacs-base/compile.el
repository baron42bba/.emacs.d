;;; compile.el --- run compiler as inferior of Emacs, parse error messages

;; Copyright (C) 1985, 86, 87, 93, 94, 1995, 1996 Free Software Foundation, Inc.
;; Copyright (C) 1995 Tinker Systems and INS Engineering Corp.
;; Copyright (C) 2001 Ben Wing.

;; Author: Roland McGrath <roland@prep.ai.mit.edu>
;; Maintainer: XEmacs Development Team
;; Keywords: tools, processes

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
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

;;; Synched up with: FSF 19.34, with a lot of divergence.

;;; Commentary:

;; This package provides the compile and grep facilities documented in
;; the Emacs user's manual.

;;; Code:

(defgroup compilation nil
  "Run compiler as inferior of Emacs, parse error messages."
  :group 'programming
  :group 'tools
  :group 'processes)


;;;###autoload
(defcustom compilation-mode-hook nil
  "*List of hook functions run by `compilation-mode' (see `run-hooks')."
  :type 'hook
  :group 'compilation)

;;;###autoload
(defcustom compilation-window-height nil
  "*Number of lines in a compilation window.  If nil, use Emacs default."
  :type '(choice (const nil)
		 integer)
  :group 'compilation)

;; XEmacs change
(defvar compilation-error-list 'invalid ; only valid buffer-local
  "List of error message descriptors for visiting erring functions.
Each error descriptor is a cons (or nil).  Its car is a marker pointing to
an error message.  If its cdr is a marker, it points to the text of the
line the message is about.  If its cdr is a cons, it is a list
\(\(DIRECTORY . FILE\) LINE [COLUMN]\).  Or its cdr may be nil if that
error is not interesting.

The value may be t instead of a list; this means that the buffer of
error messages should be reparsed the next time the list of errors is wanted.

Some other commands (like `diff') use this list to control the error
message tracking facilities; if you change its structure, you should make
sure you also change those packages.  Perhaps it is better not to change
it at all.")

(defvar compilation-old-error-list nil
  "Value of `compilation-error-list' after errors were parsed.")

(defvar compilation-parse-errors-function 'compilation-parse-errors
  "Function to call to parse error messages from a compilation.
It takes args LIMIT-SEARCH and FIND-AT-LEAST.
If LIMIT-SEARCH is non-nil, don't bother parsing past that location.
If FIND-AT-LEAST is non-nil, don't bother parsing after finding that
many new errors.
It should read in the source files which have errors and set
`compilation-error-list' to a list with an element for each error message
found.  See that variable for more info.")

;;;###autoload
(defcustom compilation-buffer-name-function nil
  "Function to compute the name of a compilation buffer.
The function receives one argument, the name of the major mode of the
compilation buffer.  It should return a string.
nil means compute the name with `(concat \"*\" (downcase major-mode) \"*\")'."
  :type 'function
  :group 'compilation)

;; XEmacs change
(defcustom compilation-shell-minor-mode-menubar-menu-name "Errors"
  "*Name of the menubar menu which displays error-navigation items."
  :type 'string
  :group 'compilation)

;;;###autoload
(defcustom compilation-finish-function nil
  "*Function to call when a compilation process finishes.
It is called with two arguments: the compilation buffer, and a string
describing how the process finished."
  :type 'function
  :group 'compilation)

(defvar compilation-last-buffer nil
  "The most recent compilation buffer.
A buffer becomes most recent when its compilation is started
or when it is used with \\[next-error] or \\[compile-goto-error].")

(defvar compilation-in-progress nil
  "List of compilation processes now running.")
(or (assq 'compilation-in-progress minor-mode-alist)
    (setq minor-mode-alist (cons '(compilation-in-progress " Compiling")
				 minor-mode-alist)))

;; XEmacs change
(defcustom compilation-always-signal-completion nil
  "*Always give an audible signal upon compilation completion.
By default that signal is only given if the bottom of the compilation
buffer is not visible in its window."
  :type 'boolean
  :group 'compilation)

(defvar compilation-parsing-end nil
  "Position of end of buffer when last error messages were parsed.")

(defvar compilation-error-message "No more errors"
  "Message to print when no more matches are found.")

(defvar compilation-num-errors-found)

(defun compilation-build-compilation-error-regexp-alist ()
  "Set the regular expressions used for parsing compiler
errors based on the compilers listed in the variable
`compilation-error-regexp-systems-list'.  Updates the
variable `compilation-error-regexp-alist'."
  (interactive)
  (setq compilation-error-regexp-alist
        (apply 'append
               (mapcar
                '(lambda (elt)
                   (if (or (not (consp compilation-error-regexp-systems-list))
                           (and (consp (car elt))
                                (intersection (car elt)
                                              compilation-error-regexp-systems-list))
                           (memq (car elt) compilation-error-regexp-systems-list))
                       (cdr elt)
                     nil))
                compilation-error-regexp-alist-alist))))

(defvar compilation-error-regexp-alist-alist
  '(
    ;; NOTE!  See also grep-regexp-alist, below.

    ;; 4.3BSD grep, cc, lint pass 1:
    ;; 	/usr/src/foo/foo.c(8): warning: w may be used before set
    ;; or GNU utilities:
    ;; 	foo.c:8: error message
    ;; or HP-UX 7.0 fc:
    ;; 	foo.f          :16    some horrible error message
    ;; or GNU utilities with column (GNAT 1.82):
    ;;   foo.adb:2:1: Unit name does not match file name
    ;; 
    ;; We'll insist that the number be followed by a colon or closing
    ;; paren, because otherwise this matches just about anything
    ;; containing a number with spaces around it.
   ((4bsd gnu)
    ("\
\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\)\\([) \t]\\|\
:\\([^0-9\n]\\|\\([0-9]+:\\)\\)\\)" 1 2 5)
    )

    ;; Microsoft C/C++:
    ;;  keyboard.c(537) : warning C4005: 'min' : macro redefinition
    ;;  d:\tmp\test.c(23) : error C2143: syntax error : missing ';' before 'if'
   (msft
    ("\\(\\([a-zA-Z]:\\)?[^:( \t\n-]+\\)[:(][ \t]*\\([0-9]+\\)[:) \t]" 1 3)
    )

    ;; Borland C++, C++Builder:
    ;;  Error ping.c 15: Unable to open include file 'sys/types.h'
    ;;  Warning ping.c 68: Call to function 'func' with no prototype
    ;; Or with Borland C++ Builder 6.0:
    ;;  Error E2209 ping.c 15: Unable to open include file 'sys/types.h'
    ;;  Warning W8065 ping.c 68: Call to function 'func' with no prototype
   (borland
    ("\\(Error\\|Warning\\) \\([EW][0-9]+ \\)?\\([a-zA-Z]?:?[^:( \t\n]+\\)\
 \\([0-9]+\\)\\([) \t]\\|:[^0-9\n]\\)" 3 4)
    )

    ;; 4.3BSD lint pass 2
    ;; 	strcmp: variable # of args. llib-lc(359)  ::  /usr/src/foo/foo.c(8)
   (4bsd
    (".*[ \t:]\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(](+[ \t]*\\([0-9]+\\))[:) \t]*$"
     1 2)
    )

    ;; 4.3BSD lint pass 3
    ;; 	bloofle defined( /users/wolfgang/foo.c(4) ), but never used
    ;; This used to be
    ;; ("[ \t(]+\\([a-zA-Z]?:?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]+" 1 2)
    ;; which is regexp Impressionism - it matches almost anything!
   (4bsd
    (".*([ \t]*\\([a-zA-Z]?:?[^:( \t\n]+\\)[:(][ \t]*\\([0-9]+\\))" 1 2)
    )

    ;; MIPS lint pass<n>; looks good for SunPro lint also
    ;;  TrimMask (255) in solomon.c may be indistinguishable from TrimMasks (93) in solomon.c due to truncation
   (mips
    ("[^\n ]+ (\\([0-9]+\\)) in \\([^ \n]+\\)" 2 1)
    ;;  name defined but never used: LinInt in cmap_calc.c(199)
    (".*in \\([^(\n]+\\)(\\([0-9]+\\))$" 1 2)
    )

    ;; Ultrix 3.0 f77:
    ;;  fort: Severe: addstf.f, line 82: Missing operator or delimiter symbol
    ;; Some SGI cc version:
    ;;  cfe: Warning 835: foo.c, line 2: something
   ((sgi ultrix)
    ("\\(cfe\\|fort\\): [^:\n]*: \\([^ \n]*\\), line \\([0-9]+\\):" 2 3)
    )
    ;; SGI Mipspro 7.3 compilers
    ;; cc-1020 CC: ERROR File = CUI_App.h, Line = 735
   (sgimipspro
    ("^cc-[0-9]* \\(cc\\|CC\\|f77\\): \\(REMARK\\|WARNING\\|ERROR\\) File = \\(.*\\), Line = \\([0-9]*\\)" 3 4 ))
    ;;  Error on line 3 of t.f: Execution error unclassifiable statement
    ;; Unknown who does this:
    ;;  Line 45 of "foo.c": bloofle undefined
    ;; Absoft FORTRAN 77 Compiler 3.1.3
    ;;  error on line 19 of fplot.f: spelling error?
    ;;  warning on line 17 of fplot.f: data type is undefined for variable d
   (of
    ("\\(.* on \\)?[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+\
of[ \t]+\"?\\([a-zA-Z]?:?[^\":\n]+\\)\"?:" 3 2)
    )

    ;; Apollo cc, 4.3BSD fc:
    ;;	"foo.f", line 3: Error: syntax error near end of statement
    ;; IBM RS6000:
    ;;  "vvouch.c", line 19.5: 1506-046 (S) Syntax error.
    ;; Unknown compiler:
    ;;  File "foobar.ml", lines 5-8, characters 20-155: blah blah
    ;; Microtec mcc68k:
    ;;  "foo.c", line 32 pos 1; (E) syntax error; unexpected symbol: "lossage"
    ;; GNAT (as of July 94):
    ;;  "foo.adb", line 2(11): warning: file name does not match ...
    ;; IBM AIX xlc compiler:
    ;;  "src/swapping.c", line 30.34: 1506-342 (W) "/*" detected in comment.
   (comma
    (".*\"\\([^,\" \n\t]+\\)\", lines? \
\\([0-9]+\\)\\([\(.]\\([0-9]+\\)\)?\\)?[:., (-]" 1 2 4)
    )

    ;; Python:
    ;;  File "foobar.py", line 5, blah blah
   (python
    ("^File \"\\([^,\" \n\t]+\\)\", line \\([0-9]+\\)," 1 2)
    )

    ;; Caml compiler:
    ;;  File "foobar.ml", lines 5-8, characters 20-155: blah blah
   (caml
    ("^File \"\\([^,\" \n\t]+\\)\", lines? \\([0-9]+\\)[-0-9]*, characters? \\([0-9]+\\)" 1 2 3)
    )

    ;; MIPS RISC CC - the one distributed with Ultrix:
    ;;	ccom: Error: foo.c, line 2: syntax error
    ;; DEC AXP OSF/1 cc
    ;;  /usr/lib/cmplrs/cc/cfe: Error: foo.c: 1: blah blah 
   ((mips ultrix)
    ("[a-z0-9/]+: \\([eE]rror\\|[wW]arning\\): \\([^,\" \n\t]+\\)[,:] \\(line \\)?\\([0-9]+\\):" 2 4)
    )

    ;; IBM AIX PS/2 C version 1.1:
    ;;	****** Error number 140 in line 8 of file errors.c ******
   (aix
    (".*in line \\([0-9]+\\) of file \\([^ \n]+[^. \n]\\)\\.? " 2 1)
    )
    ;; IBM AIX lint is too painful to do right this way.  File name
    ;; prefixes entire sections rather than being on each line.

    ;; Lucid Compiler, lcc 3.x
    ;; E, file.cc(35,52) Illegal operation on pointers
   (lcc
    ("[EW], \\([^(\n]*\\)(\\([0-9]+\\),[ \t]*\\([0-9]+\\)" 1 2 3)
    )

    ;; GNU messages with program name and optional column number.
   (gnu
    ("[a-zA-Z]?:?[^0-9 \n\t:]+:[ \t]*\\([^ \n\t:]+\\):\
\\([0-9]+\\):\\(\\([0-9]+\\)[: \t]\\)?" 1 2 4)
    )

    ;; GNU messages with program name and optional column number
    ;; and a severity letter after that.  nsgmls makes them.
   (gnu
    ("[^0-9 \n\t:]+:[ \t]*\\([^ \n\t:]+\\):\
\\([0-9]+\\):\\(\\([0-9]+\\):\\)?[A-Za-z]:" 1 2 4)
    )

    ;; jwz:
    ;; IRIX 5.2
    ;; cfe: Warning 712: foo.c, line 2: illegal combination of pointer and ...
   (sgi
    ("[^\n]* \\([^ \n,\"]+\\), line \\([0-9]+\\):" 1 2)
    )
    ;; IRIX 5.2
    ;; cfe: Warning 600: xfe.c: 170: Not in a conditional directive while ...
   (sgi
    ("[^\n]*: \\([^ \n,\"]+\\): \\([0-9]+\\):" 1 2)
    )

    ;; Cray C compiler error messages
   (cray
    ("\\(cc\\| cft\\)-[0-9]+ c\\(c\\|f77\\): ERROR \\([^,\n]+, \\)* File = \\([^,\n]+\\), Line = \\([0-9]+\\)" 4 5)
    )

    ;; IBM C/C++ Tools 2.01:
    ;;  foo.c(2:0) : informational EDC0804: Function foo is not referenced.
    ;;  foo.c(3:8) : warning EDC0833: Implicit return statement encountered.
    ;;  foo.c(5:5) : error EDC0350: Syntax error.
   (ibm
    ("\\([^( \n\t]+\\)(\\([0-9]+\\):\\([0-9]+\\)) : " 1 2 3)
    )

    ;; IAR Systems C Compiler:
    ;;  "foo.c",3  Error[32]: Error message
    ;;  "foo.c",3  Warning[32]: Error message
   (iar
    ("\"\\(.*\\)\",\\([0-9]+\\)\\s-+\\(Error\\|Warning\\)\\[[0-9]+\\]:" 1 2)
    )

    ;; Sun ada (VADS, Solaris):
    ;;  /home3/xdhar/rcds_rc/main.a, line 361, char 6:syntax error: "," inserted
   (ada
    ("\\([^, \n\t]+\\), line \\([0-9]+\\), char \\([0-9]+\\)[:., \(-]" 1 2 3)
    )

    ;; Perl -w:
    ;; syntax error at automake line 922, near "':'"
    ;; Perl debugging traces
    ;; store::odrecall('File_A', 'x2') called at store.pm line 90
   (perl
    (".* at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 1 2)
    )

    ;; Oracle pro*c:
    ;; Semantic error at line 528, column 5, file erosacqdb.pc:
   (oracle
    ("Semantic error at line \\([0-9]+\\), column \\([0-9]+\\), file \\(.*\\):"
     3 1 2)
    )

    ;; EPC F90 compiler:
    ;; Error 24 at (2:progran.f90) : syntax error
   (epc
    ("Error [0-9]+ at (\\([0-9]*\\):\\([^)\n]+\\))" 2 1)
    )

    ;; SGI IRIX MipsPro 7.3 compilers:
    ;; cc-1070 cc: ERROR File = linkl.c, Line = 38
    (sgimipspro
     (".*: ERROR File = \\(.+\\), Line = \\([0-9]+\\)" 1 2)
     (".*: WARNING File = \\(.+\\), Line = \\([0-9]+\\)" 1 2)
     )

    ;; Sun F90 error messages:
    ;; cf90-113 f90comp: ERROR NSE, File = Hoved.f90, Line = 16, Column = 3
    (sun
     (".* ERROR [a-zA-Z0-9 ]+, File = \\(.+\\), Line = \\([0-9]+\\), Column = \\([0-9]+\\)"
      1 2 3)
     )

    ;; RXP - GPL XML validator at http://www.cogsci.ed.ac.uk/~richard/rxp.html:
    ;; Error: Mismatched end tag: expected </geroup>, got </group>
    ;; in unnamed entity at line 71 char 8 of file:///home/reto/test/group.xml
    (rxp
     ("Error:.*\n.* line \\([0-9]+\\) char \\([0-9]+\\) of file://\\(.+\\)"
      3 1 2)
     )
    ;; Warning: Start tag for undeclared element geroup
    ;; in unnamed entity at line 4 char 8 of file:///home/reto/test/group.xml
    (rxp
     ("Warning:.*\n.* line \\([0-9]+\\) char \\([0-9]+\\) of file://\\(.+\\)"
      3 1 2)
     )

    ;; See http://ant.apache.org/faq.html
    ;; Ant Java: works for jikes
    (ant
     ("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[0-9]+:[0-9]+:" 1 2 3)
     )

    ;; Ant Java: works for javac
    (ant
     ("^\\s-*\\[[^]]*\\]\\s-*\\(.+\\):\\([0-9]+\\):" 1 2)
     )

    ;; Ant / cygwin:
    ;; file:G:/foobar/dev/build-myproj.xml:54: Compiler Adapter 'javac' can't be found.
   (ant
    ("file:\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\):[ \t]*\\([0-9]+\\)[: \t]" 1 3)
    )
   )
  "Alist of (system regexp-alist) for building
`compilation-error-regexp-alist'.  SYSTEM is either a system identifier,
or a list of system identifiers.  See the variable 
`compilation-error-regexp-systems-list'")

(defvar compilation-error-regexp-alist nil
 "Alist that specifies how to match errors in compiler output.
Each elt has the form (REGEXP FILE-IDX LINE-IDX [COLUMN-IDX FILE-FORMAT...])
If REGEXP matches constrained to the beginning of the line, the
FILE-IDX'th subexpression gives the file name, and the LINE-IDX'th
subexpression gives the line number.  If COLUMN-IDX is given, the
COLUMN-IDX'th subexpression gives the column number on that line.  If
any FILE-FORMAT is given, each is a format string to produce a file name
to try; %s in the string is replaced by the text matching the
FILE-IDX'th subexpression.  Note previously REGEXP was not constrained
to the beginning of the line, so old patterns without leading `^' or `\\n'
may now require a leading `.*'.

Note that this now gets set by the function 
`compilation-build-compilation-error-regexp-alist' using the 
value of the variable `compilation-error-regexp-alist-alist'")

(defcustom compilation-error-regexp-systems-list 'all
  "*This is either the symbol `all', or a list of systems for which
compilation error regexps should be included in
`compilation-error-regexp-alist'.  You must run the function
`compilation-build-compilation-error-regexp-alist' after changing
the value of this variable for the change to take effect.

The list of known systems is:
  4bsd:       Using 4bsd
  ada:        Ada compilers
  aix:        the operating system
  ant:        Ant Java
  borland:    Using Borland
  caml:       Using caml compiler
  comma:      Using tool that says \"foo.c\", line 12
  cray:       Using Cray
  epc:        Using EPC F90 compiler
  gnu:        but of course
  iar:        Using IAR systems compiler
  ibm:        IBM C compilers
  lcc:        Lucid compilers
  mips:       Using Mips
  msft:       Using microsoft
  of:         Using tool that says line xx of foo.c
  oracle:     Using Oracle pro*c
  perl:       perl -w
  python:     Using python
  rxp:        Using RPX GPL XML validator
  sgi:        Using SGI
  sgimipspro: Using SGI Mipspro 7.3
  sun:        Using Sun F90 compiler
  ultrix:     Using DEC unixoid operating system

See also the variable `compilation-error-regexp-alist-alist'."
  :type '(choice (const all)
	  (set :menu-tag "Pick"
	       (const 4bsd)
	       (const ada)
	       (const aix)
	       (const ant)
	       (const borland)
	       (const caml)
	       (const comma)
	       (const cray)
	       (const epc)
	       (const gnu)
	       (const iar)
	       (const ibm)
	       (const lcc)
	       (const mips)
	       (const msft)
	       (const of)
	       (const oracle)
	       (const perl)
	       (const python)
	       (const rxp)
	       (const sgi)
	       (const sgimipspro)
	       (const sun)
	       (const ultrix)
	       ))
  :set (lambda (symbol value)
         (set-default symbol value)
         (compilation-build-compilation-error-regexp-alist))
  :group 'compilation)

(compilation-build-compilation-error-regexp-alist)

(defcustom compilation-error-screen-columns t
  "*If non-nil, column numbers in error messages are screen columns.
Otherwise they are interpreted as character positions, with
each character occupying one column.
The default is to use screen columns, which requires that the compilation
program and Emacs agree about the display width of the characters,
especially the TAB character."
  :type 'boolean
  :group 'compilation
  :version "20.4")

(defcustom compilation-read-command t
  "*Non-nil means \\[compile] reads the compilation command to use.
Otherwise, \\[compile] just uses the value of `compile-command'."
  :type 'boolean
  :group 'compilation)

(defcustom compilation-ask-about-save t
  "*Non-nil means \\[compile] asks which buffers to save before compiling.
Otherwise, it saves all modified buffers without asking."
  :type 'boolean
  :group 'compilation)

(defcustom compilation-ask-about-kill t
  "*If not nil, M-x compile asks to kill a running compilation.
Otherwise, it kills it without asking."
  :type 'boolean
  :group 'compilation)

;; Note: the character class after the optional drive letter does not
;; include a space to support file names with blanks.
(defvar grep-regexp-alist
  '(("^\\([a-zA-Z]?:?[^:(\t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2))
  "Regexp used to match grep hits.  See `compilation-error-regexp-alist'.")

(defcustom grep-command "grep -n "
  "*Last grep command used in \\[grep]; default for next grep."
  :type 'string
  :group 'compilation)

;; The system null device.
;; #### Backward compatibility hack; recent (21.4.x and 21.5.x) XEmacsen
;; have `null-device', but 21.1 may not.
(defvar grep-null-device
  (if (boundp 'null-device) null-device "/dev/null")
  "The system null device.")

(defvar grep-find-use-xargs
  ;; XEmacs change: test xargs compatibility too, revert to non-print0 xargs
  ;; if any xargs is available.
  (if (condition-case nil
	  (and
	   (equal
	    (call-process "find" nil nil nil grep-null-device "-print0") 0)
	   (equal (call-process "xargs" nil nil nil "-0" "-e") 0))
	(error nil))
      'gnu
    (condition-case nil (equal (call-process "xargs") 0) (error nil)))
  "Whether \\[grep-find] uses the `xargs' utility by default.

If nil, it uses `find -exec'; if `gnu', it uses `find -print0' and `xargs -0';
if not nil and not `gnu', it uses `find -print' and `xargs'.

This variable's value takes effect when `compile.el' is loaded
by influencing the default value for the variable `grep-find-command'.")

(defvar grep-find-command
  (cond ((eq grep-find-use-xargs 'gnu)
	 (format "find . -type f -print0 | xargs -0 -e %s" grep-command))
	(grep-find-use-xargs
	 (format "find . -type f -print | xargs %s" grep-command))
	(t (cons (format "find . -type f -exec %s {} /dev/null \\;"
			 grep-command)
		 (+ 22 (length grep-command)))))
  "The default find command for \\[grep-find].")

;; XEmacs addition: all grep-all-files stuff

(defvar grep-all-files-history nil)

(defcustom grep-all-files-omitted-expressions
  '("*~" "#*" ".#*" ",*" "*.elc" "*.obj" "*.o" "*.exe" "*.dll" "*.lib" "*.a"
    "*.dvi" "*.class" "*.bin" "*.orig" "*.rej")
  "List of expressions matching files to be omitted in `grep-all-files-...'.
Each entry should be a simple name or a shell wildcard expression."
  :type '(repeat string)
  :group 'compilation)

(defcustom grep-all-files-omitted-directories '("CVS" "RCS" "SCCS")
  "List of directories not to recurse into in `grep-all-files-...'.
Each entry should be a simple name or a shell wildcard expression."
  :type '(repeat string)
  :group 'compilation)

;;;###autoload
(defcustom compilation-search-path '(nil)
  "*List of directories to search for source files named in error messages.
Elements should be directory names, not file names of directories.
nil as an element means to try the default directory."
  :type '(repeat (choice (const :tag "Default" nil)
			 directory))
  :group 'compilation)

(defcustom compile-command "make -k "
  "*Last shell command used to do a compilation; default for next compilation.

Sometimes it is useful for files to supply local values for this variable.
You might also use mode hooks to specify it in certain modes, like this:

    (add-hook 'c-mode-common-hook
	      (lambda ()
		(or (file-exists-p \"makefile\") (file-exists-p \"Makefile\")
		    (set (make-local-variable 'compile-command)
			 (concat \"make -k \" buffer-file-name)))))"
  :type 'string
  :group 'compilation)

(defvar compilation-enter-directory-regexp
  "[^\n]*: Entering directory `\\([^\n]*\\)'$"
  "Regular expression matching lines that indicate a new current directory.
This must contain one \\(, \\) pair around the directory name.

The default value matches lines printed by the `-w' option of GNU Make.")

(defvar compilation-leave-directory-regexp
  "[^\n]*: Leaving directory `\\([^\n]*\\)'$"
  "Regular expression matching lines that indicate restoring current directory.
This may contain one \\(, \\) pair around the name of the directory
being moved from.  If it does not, the last directory entered \(by a
line matching `compilation-enter-directory-regexp'\) is assumed.

The default value matches lines printed by the `-w' option of GNU Make.")

(defvar compilation-directory-stack nil
  "Stack of previous directories for `compilation-leave-directory-regexp'.
The head element is the directory the compilation was started in.")

(defvar compilation-exit-message-function nil "\
If non-nil, called when a compilation process dies to return a status message.
This should be a function of three arguments: process status, exit status,
and exit message; it returns a cons (MESSAGE . MODELINE) of the strings to
write into the compilation buffer, and to put in its mode line.")

;; History of compile commands.
(defvar compile-history nil)
;; History of grep commands.
(defvar grep-history nil)
(defvar grep-find-history nil)

;; XEmacs
(defvar compilation-font-lock-keywords (purecopy
  (list
   '("^[-_.\"A-Za-z0-9/+]+\\(:\\|, line \\)[0-9]+: \\([wW]arning:\\).*$" .
     font-lock-keyword-face)
   '("^[-_.\"A-Za-z0-9/+]+\\(: *\\|, line \\)[0-9]+:.*$" . font-lock-function-name-face)
   '("^[^:\n]+-[a-zA-Z][^:\n]+$" . font-lock-doc-string-face)
   '("\\(^[-_.\"A-Za-z0-9/+]+\\)\\(: *\\|, line \\)[0-9]+" 1 font-lock-string-face t)
   '("^[-_.\"A-Za-z0-9/+]+\\(: *[0-9]+\\|, line [0-9]+\\)" 1 bold t)
   ))
  "Additional expressions to highlight in Compilation mode.")

;FSF's version.  Ours looks better.
;(defvar compilation-mode-font-lock-keywords
;  ;; This regexp needs a bit of rewriting.  What is the third grouping for?
;  '(("^\\([a-zA-Z]?:?[^ \n:]*:\\([0-9]+:\\)+\\)\\(.*\\)$"
;     1 font-lock-function-name-face))
;;;  ("^\\([^\n:]*:\\([0-9]+:\\)+\\)\\(.*\\)$" 0 font-lock-keyword-face keep)

;; XEmacs change
(put 'compilation-mode 'font-lock-defaults
     '(compilation-font-lock-keywords t))

(defcustom compilation-mouse-motion-initiate-parsing nil
  "*Should mouse motion over the compilation buffer initiate parsing?
When set to a non-nil value, mouse motion over the compilation/grep
buffer may initiate parsing of the error messages or grep hits.
When this is nil, errors and grep matches will no longer be 
highlighted until they have been parsed, but may still be selected
with the center mouse button.  This will then initiate parsing
and jump to the corresponding source line."
  :type 'boolean
  :group 'compilation)

;;;###autoload
(defun compile (command)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `compilation-read-command' is
non-nil; otherwise uses `compile-command'.  With prefix arg, always prompts.

To run more than one compilation at once, start one and rename the
\`*compilation*' buffer to some other name with \\[rename-buffer].
Then start the next one.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name."
  (interactive
   (if (or compilation-read-command current-prefix-arg)
       ;; XEmacs change
       (list (read-shell-command "Compile command: "
                                 compile-command
                                 ;; #### minibuffer code should do this
                                 (if (equal (car compile-history)
                                            compile-command)
                                     '(compile-history . 1)
                                     'compile-history)))
       (list compile-command)))
  (setq compile-command command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compile-internal compile-command "No more errors"))

;; run compile with the default command line
(defun recompile ()
  "Re-compile the program including the current buffer."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compile-internal compile-command "No more errors"))

;;;###autoload
(defun grep (command-args)
  "Run grep, with user-specified args, and collect output in a buffer.
While grep runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a grep command."
  (interactive
   ;; XEmacs change
   (list (read-shell-command "Run grep (like this): "
			     grep-command 'grep-history)))
  (let ((buf (compile-internal (concat command-args " " grep-null-device)
			       "No more grep hits" "grep"
			       ;; Give it a simpler regexp to match.
			       nil grep-regexp-alist)))
    (save-excursion
      (set-buffer buf)
      (set (make-local-variable 'compilation-exit-message-function)
	   ;; XEmacs change
	   (lambda (proc msg)
	     (let ((code (process-exit-status proc)))
	       (if (eq (process-status proc) 'exit)
		   (cond ((zerop code)
			  (cons (format "finished (%d matches found)\n"
					;; stolen from count-matches,
					;; should be refactored by
					;; count-matches returning
					;; count.
					(let ((count 0) opoint)
					  (save-excursion
					    (goto-char (point-min))
					    (while (and (not (eobp))
							(progn (setq opoint (point))
							       (re-search-forward (caar grep-regexp-alist) nil t)))
					      (if (= opoint (point))
						  (forward-char 1)
						(setq count (1+ count))))
					    count)))
				"matched"))
			 ((= code 1)
			  '("finished with no matches found\n" . "no match"))
			 (t
			  (cons msg code)))
		 (cons msg code))))))))

;;;###autoload
(defun grep-find (command-args)
  "Run grep via find, with user-specified args COMMAND-ARGS.
Collect output in a buffer.
While find runs asynchronously, you can use the \\[next-error] command
to find the text that grep hits refer to.

This command uses a special history list for its arguments, so you can
easily repeat a find command."
  (interactive
   ;; XEmacs change..
   (list (read-shell-command "Run find (like this): "
			       grep-find-command 'grep-find-history)))
  (let ((grep-null-device nil))		; see grep
    (grep command-args)))

;; XEmacs addition
(defalias 'find-grep 'grep-find)

;; XEmacs addition: all grep-all-files stuff

(defun construct-grep-all-files-command (find-segment grep-segment)
  (let ((omit-annoying
	 (mapconcat #'(lambda (wildcard)
			(concat "-name '" wildcard "' -or "))
		    grep-all-files-omitted-expressions
		    "")))
    (cond ((eq grep-find-use-xargs 'gnu)
	   (format "find . %s %s -type f -print0 | xargs -0 -e %s"
		   find-segment omit-annoying grep-segment))
	  (grep-find-use-xargs
	   (format "find . %s %s -type f -print | xargs %s"
		   find-segment omit-annoying grep-segment))
	  (t
	   (format "find . %s %s -type f -exec %s {} /dev/null \\;"
		   find-segment omit-annoying grep-segment)))))

;;;###autoload
(defun grep-all-files-in-current-directory (command)
  "Run `grep' in all non-annoying files in the current directory.
`Non-annoying' excludes backup files, autosave files, CVS merge files, etc.
More specifically, this is controlled by `grep-all-files-omitted-expressions'.

This function does not recurse into subdirectories.  If you want this,
use \\[grep-all-files-in-current-directory-and-below]."
  (interactive
   (progn
     (list (read-shell-command "Run grep (like this): "
			       grep-command 'grep-all-files-history))))
  (grep (construct-grep-all-files-command
	 "-name . -or -type d -prune -or" command)))

;;;###autoload
(defun grep-all-files-in-current-directory-and-below (command)
  "Run `grep' in all non-annoying files in the current directory and below.
`Non-annoying' excludes backup files, autosave files, CVS merge files, etc.
More specifically, this is controlled by `grep-all-files-omitted-expressions'.

This function recurses into subdirectories.  If you do not want this,
use \\[grep-all-files-in-current-directory]."
  (interactive
   (progn
     (list (read-shell-command "Run grep (like this): "
			       grep-command 'grep-all-files-history))))
  (grep (construct-grep-all-files-command
	 ;; prune all specified directories.
	 (mapconcat #'(lambda (wildcard)
			(concat "-name '" wildcard "' -prune -or "))
		    grep-all-files-omitted-directories
		    "")
	 command)))

(defun compile-internal (command error-message
				 &optional name-of-mode parser regexp-alist
				 name-function)
  "Run compilation command COMMAND (low level interface).
ERROR-MESSAGE is a string to print if the user asks to see another error
and there are no more errors.  Third argument NAME-OF-MODE is the name
to display as the major mode in the compilation buffer.

Fourth arg PARSER is the error parser function (nil means the default).  Fifth
arg REGEXP-ALIST is the error message regexp alist to use (nil means the
default).  Sixth arg NAME-FUNCTION is a function called to name the buffer (nil
means the default).  The defaults for these variables are the global values of
\`compilation-parse-errors-function', `compilation-error-regexp-alist', and
\`compilation-buffer-name-function', respectively.

Returns the compilation buffer created."
  (let (outbuf)
    (save-excursion
      (or name-of-mode
	  (setq name-of-mode "Compilation"))
      (setq outbuf
	    (get-buffer-create
	     (funcall (or name-function compilation-buffer-name-function
			  (function (lambda (mode)
				      (concat "*" (downcase mode) "*"))))
		      name-of-mode)))
      (set-buffer outbuf)
      (let ((comp-proc (get-buffer-process (current-buffer))))
	(if comp-proc
	    (if (or (not (eq (process-status comp-proc) 'run))
                    (or
                     (not compilation-ask-about-kill)
                     (yes-or-no-p
                      (format "A %s process is running; kill it? "
                              name-of-mode))))
		(condition-case ()
		    (progn
		      (interrupt-process comp-proc)
		      (sit-for 1)
		      (delete-process comp-proc))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once"
		     (buffer-name))
	      )))
      ;; In case the compilation buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables))
    (let ((regexp-alist (or regexp-alist compilation-error-regexp-alist))
	  (parser (or parser compilation-parse-errors-function))
	  (thisdir default-directory)
	  ;; XEmacs change
	  (buffer-save (current-buffer))
	  outwin)

      ;; XEmacs change
      ;; Pop up the compilation buffer.
      (setq outwin (display-buffer outbuf))
      
      (unwind-protect
       (progn
	;; Clear out the compilation buffer and make it writable.
	;; Change its default-directory to the directory where the compilation
	;; will happen, and insert a `cd' command to indicate this.
	(set-buffer outbuf)
	(setq buffer-read-only nil)
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(buffer-enable-undo (current-buffer))
	(setq default-directory thisdir)
	(insert "cd " thisdir "\n" command "\n")
	(set-buffer-modified-p nil)

	;; XEmacs change
	;; set it so the window will scroll to show compile output
	(save-window-excursion
	  (select-window outwin)
	  (goto-char (point-max)))
	(setq outwin (display-buffer outbuf))
	
	;; XEmacs change
	(compilation-mode name-of-mode)
	;; (setq buffer-read-only t)  ;;; Non-ergonomic.
	;; XEmacs change
	(set (make-local-variable 'compile-command) command)
	(set (make-local-variable 'compilation-parse-errors-function) parser)
	(set (make-local-variable 'compilation-error-message) error-message)
	(set (make-local-variable 'compilation-error-regexp-alist) regexp-alist)
	(setq default-directory thisdir
	      compilation-directory-stack (list default-directory))
	(set-window-start outwin (point-min))
	(setq mode-name name-of-mode)
	;; XEmacs change
;	(or (eq outwin (selected-window))
;	    (set-window-point outwin (point-min)))
	(compilation-set-window-height outwin)
	;; Start the compilation.
	(if (fboundp 'start-process)
	    (let* ((process-environment (cons "EMACS=t" process-environment))
		   (proc (start-process-shell-command (downcase mode-name)
						      outbuf
						      command)))
	      (set-process-sentinel proc 'compilation-sentinel)
	      (set-process-filter proc 'compilation-filter)
	      ;; Causes problems; please investigate before using:
	      ;(process-send-eof proc)
	      (set-marker (process-mark proc) (point) outbuf)
	      (setq compilation-in-progress
		    (cons proc compilation-in-progress)))
	  ;; No asynchronous processes available.
	  (display-message
	   'progress
	   (format "Executing `%s'..." command))
	  ; FSF
	  ; (setq mode-line-process ":run")
	  ; (force-mode-line-update)
	  (sit-for 0)			;; Force redisplay
	  (let ((status (call-process shell-file-name nil outbuf nil
				      shell-command-switch command))))
	  (display-message
	   'progress
	   (format "Executing `%s'...done" command))))
       (set-buffer buffer-save)))

    ;; Make it so the next C-x ` will use this buffer.
    (setq compilation-last-buffer outbuf)))

;; Set the height of WINDOW according to compilation-window-height.
(defun compilation-set-window-height (window)
  (and compilation-window-height
       ;; Check if the window is split horizontally.
       ;; Emacs checks window width versus frame-width:
       ;;   (= (window-width window) (frame-width (window-frame window)))
       ;; But XEmacs must take into account a possible left or right
       ;; toolbar:
       (and (window-leftmost-p (selected-window))
	    (window-rightmost-p (selected-window)))
       ;; If window is alone in its frame, aside from a minibuffer,
       ;; don't change its height.
       (not (eq window (frame-root-window (window-frame window))))
       ;; This save-excursion prevents us from changing the current buffer,
       ;; which might not be the same as the selected window's buffer.
       (save-excursion
	 (let ((w (selected-window)))
	   (unwind-protect
	       (progn
		 (select-window window)
		 (enlarge-window (- compilation-window-height
				    (window-height))))
	     (select-window w))))))

;;;###autoload
(defvar compilation-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-name map 'compilation-minor-mode-map)
    (define-key map "\C-c\C-c" 'compile-goto-error)
    (define-key map "\C-m" 'compile-goto-error)
    (define-key map "\C-c\C-k" 'kill-compilation)
    (define-key map "\M-n" 'compilation-next-error)
    (define-key map "\M-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    map)
  "Keymap for `compilation-minor-mode'.")

;;;###autoload
(defvar compilation-shell-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-\C-m" 'compile-goto-error)
    (define-key map "\M-\C-n" 'compilation-next-error)
    (define-key map "\M-\C-p" 'compilation-previous-error)
    (define-key map "\M-{" 'compilation-previous-file)
    (define-key map "\M-}" 'compilation-next-file)
    map)
  "Keymap for `compilation-shell-minor-mode'.")

(defvar compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parents map (list compilation-minor-mode-map))
    (set-keymap-name map 'compilation-mode-map)
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map 'button2 'compile-mouse-goto-error)
    map)
  "Keymap for compilation log buffers.
`compilation-minor-mode-map' is a parent of this.")

;;; XEmacs menus

(defun compilation-errors-exist-p (&optional buffer)
  "Whether we are in a state where the `next-error' command will work,
that is, whether there exist (or may exist) error targets in the *compile*
or *grep* buffers."
  (or buffer
      (setq buffer (condition-case nil
			  (compilation-find-buffer)
			(error nil))))
  (and buffer
       (compilation-buffer-p buffer)
       (save-excursion
	 (set-buffer buffer)
	 ;; Has errors on the list, or needs to be parsed.
	 ;; But don't parse it now!
	 (or (not (null compilation-error-list))
	     (< compilation-parsing-end (point-max))))))

;; XEmacs change
(defvar compilation-shell-minor-mode-menubar-menu
  '("Errors"
    :filter compile-menu-filter
    ["Stop"		comint-interrupt-subjob t]
    "---"
    ["First Error"	first-error		(compilation-errors-exist-p)]
    ["Previous Error"	previous-error		(compilation-errors-exist-p)]
    ["Next Error" 	next-error		(compilation-errors-exist-p)]
    ))

(defvar Compilation-mode-popup-menu
  '("Compilation Mode Commands"
    :filter compile-menu-filter
    ["Compile..."	compile t]
    ["Recompile"	recompile t]
    ["Kill Compilation"	kill-compilation (get-buffer-process (current-buffer))]
    "---"
    ["Goto Error"	compile-goto-error	(compilation-errors-exist-p)]
    ["Next Error" 	next-error		(compilation-errors-exist-p)]
    ["Previous Error"	previous-error		(compilation-errors-exist-p)]
    ["First Error"	first-error		(compilation-errors-exist-p)]
    ))

(defvar Compilation-mode-menubar-menu
  (cons "Compile" (cdr Compilation-mode-popup-menu)))
  
(defvar grep-mode-popup-menu
  '("Grep Mode Commands"
    :filter grep-menu-filter
    ["Grep..."		grep t]
    ["Repeat Grep"	recompile t]
    ["Kill Grep"	kill-compilation (get-buffer-process (current-buffer))]
    "---"
    ["Goto Match" compile-goto-error (default-value 'compilation-error-list)]
    ["Next Match"	next-error (default-value 'compilation-error-list)]
    ["Previous Match"	previous-error (default-value 'compilation-error-list)]
    ["First Match"	first-error (default-value 'compilation-error-list)]
    ))

(defvar grep-mode-menubar-menu
  (cons "Grep" (cdr grep-mode-popup-menu)))
  
(defun compile-menu-filter-1 (menu history-list item-name command-name)
  (let ((submenu (mapcar #'(lambda (string)
			     (vector string
				     (list command-name string)
				     t))
			  history-list))
	(existing (assoc item-name menu)))
    (if existing
	(progn
	  (setcdr existing submenu)
	  menu)
      (nconc menu (list (cons item-name submenu))))))

(defun compile-menu-filter (menu)
  (compile-menu-filter-1 menu compile-history "Compile History" 'compile))

(defun grep-menu-filter (menu)
  (compile-menu-filter-1 menu grep-history "Grep History" 'grep))

;;;###autoload
(defun compilation-mode (&optional name-of-mode)
  "Major mode for compilation log buffers.
\\<compilation-mode-map>To visit the source for a line-numbered error,
move point to the error message line and type \\[compile-goto-error],
or click on the line with \\[compile-mouse-goto-error].
There is a menu of commands on \\[compile-popup-menu].
To kill the compilation, type \\[kill-compilation].

Runs `compilation-mode-hook' with `run-hooks' (which see)."
  (interactive)
  (kill-all-local-variables)
  (use-local-map compilation-mode-map)
  (setq major-mode 'compilation-mode
	mode-name "Compilation")
  (compilation-setup)
  (font-lock-set-defaults)
  (if (not name-of-mode) nil
    (let ((sym (intern (concat name-of-mode "-mode-popup-menu"))))
      (if (boundp sym)
	  (setq mode-popup-menu (symbol-value sym))))
    (if (featurep 'menubar)
	(progn
	  ;; make a local copy of the menubar, so our modes don't
	  ;; change the global menubar
	  (set-buffer-menubar current-menubar)
	  (let ((sym (intern (concat name-of-mode "-mode-menubar-menu"))))
	    (if (boundp sym)
		(add-submenu nil (symbol-value sym)))))))
  (run-hooks 'compilation-mode-hook))

;; XEmacs addition, hacked by Mly
(defun compilation-mode-motion-hook (event)
  (mode-motion-highlight-internal
    event
    #'beginning-of-line
    #'(lambda ()
        (let* ((p (point))
               (e (progn (end-of-line) (point)))
               (l (progn
                    (if (and compilation-mouse-motion-initiate-parsing
                             (or (eq compilation-error-list 't)
                                 (>= p compilation-parsing-end)))
                        ;; #### Does it suck too badly to have mouse-movement
                        ;; #### over a buffer parse errors in that buffer??
                        (save-window-excursion
                          (compile-reinitialize-errors nil p)))
                    (if (and compilation-error-list
                             (<= (car (car compilation-error-list)) p))
                        ;; Perhaps save time by only searching tail
                        compilation-error-list
                        compilation-old-error-list))))
          (if (catch 'found
                (while l
                  (let ((x (marker-position (car (car l)))))
                    (cond ((< x p)
                           (setq l (cdr l)))
                          ((<= x e)
                           (throw 'found t))
                          (t
                           (throw 'found nil)))))
                nil)
              (goto-char e)
              (goto-char p))))))

(defun compilation-setup ()
  "Prepare the buffer for the compilation parsing commands to work."
  ;; Make the buffer's mode line show process state.
  (setq mode-line-process '(":%s"))
  (set (make-local-variable 'compilation-error-list) nil)
  (set (make-local-variable 'compilation-old-error-list) nil)
  (set (make-local-variable 'compilation-parsing-end) 1)
  (set (make-local-variable 'compilation-directory-stack) nil)
  (make-local-variable 'compilation-error-screen-columns)
  (setq compilation-last-buffer (current-buffer))
  ;; XEmacs change: highlight lines, install menubar.
  (require 'mode-motion)
  (setq mode-motion-hook 'compilation-mode-motion-hook)
  (make-local-hook 'mouse-track-click-hook)
  (add-hook 'mouse-track-click-hook 'compile-mouse-maybe-goto-error t t))

;;;###autoload
(defvar compilation-shell-minor-mode nil
  "Non-nil when in compilation-shell-minor-mode.
In this minor mode, all the error-parsing commands of the
Compilation major mode are available but bound to keys that don't
collide with Shell mode.")
(make-variable-buffer-local 'compilation-shell-minor-mode)

;;;###autoload
(defvar compilation-minor-mode nil
  "Non-nil when in compilation-minor-mode.
In this minor mode, all the error-parsing commands of the
Compilation major mode are available.")
(make-variable-buffer-local 'compilation-minor-mode)

;;;###autoload
(defun compilation-shell-minor-mode (&optional arg)
  "Toggle compilation shell minor mode.
With arg, turn compilation mode on if and only if arg is positive.
See `compilation-mode'.
Turning the mode on runs the normal hook `compilation-shell-minor-mode-hook'."
  (interactive "P")
  (if (setq compilation-shell-minor-mode (if (null arg)
					     (null compilation-shell-minor-mode)
					   (> (prefix-numeric-value arg) 0)))
      (let ((mode-line-process))
	(compilation-setup)
	(run-hooks 'compilation-shell-minor-mode-hook)
	(make-local-variable 'current-menubar)
	(add-submenu nil
		     (cons compilation-shell-minor-mode-menubar-menu-name
			   (cdr compilation-shell-minor-mode-menubar-menu))))
    (delete-menu-item (list compilation-shell-minor-mode-menubar-menu-name))))

;;;###autoload
(add-minor-mode 'compilation-shell-minor-mode
		" Shell-Compile"
		compilation-shell-minor-mode-map
		'view-minor-mode
		compilation-shell-minor-mode)

;;;###autoload
(defun compilation-minor-mode (&optional arg)
  "Toggle compilation minor mode.
With arg, turn compilation mode on if and only if arg is positive.
See `compilation-mode'.
! \\{compilation-mode-map}"
  (interactive "P")
  (if (setq compilation-minor-mode (if (null arg)
				       (null compilation-minor-mode)
				     (> (prefix-numeric-value arg) 0)))
      (progn
	(compilation-setup)
	(run-hooks 'compilation-minor-mode-hook))))

;;;###autoload
(add-minor-mode 'compilation-minor-mode
		" CMPL"
		compilation-minor-mode-map
		'view-minor-mode
		compilation-minor-mode)

(defun compilation-handle-exit (process-status exit-status msg)
  "Write msg in the current buffer and hack its mode-line-process."
  (let ((buffer-read-only nil)
	(status (if compilation-exit-message-function
		    (funcall compilation-exit-message-function
			     process-status exit-status msg)
		  (cons msg exit-status)))
	(omax (point-max))
	(opoint (point)))
    ;; Record where we put the message, so we can ignore it
    ;; later on.
    (goto-char omax)
    (insert ?\n mode-name " " (car status))
    (forward-char -1)
    (insert " at " (substring (current-time-string) 0 19))
    (forward-char 1)
    (setq mode-line-process (format ":%s [%s]" process-status (cdr status)))
    ;; Force mode line redisplay soon.
    (force-mode-line-update)
    (if (and opoint (< opoint omax))
	(goto-char opoint))
    (if compilation-finish-function
	(funcall compilation-finish-function (current-buffer) msg))))

;; Called when compilation process changes state.
(defun compilation-sentinel (proc msg)
  "Sentinel for compilation buffers."
  ;; XEmacs change
  (let* ((buffer (process-buffer proc))
	 (window (get-buffer-window buffer)))
    (if (memq (process-status proc) '(signal exit))
	(progn
	  (if (null (buffer-name buffer))
	      ;; buffer killed
	      (set-process-buffer proc nil)
	    (let ((obuf (current-buffer))
		  omax opoint estatus)
	      ;; save-excursion isn't the right thing if
	      ;; process-buffer is current-buffer
	      (unwind-protect
		  (progn
		    ;; Write something in the compilation buffer
		    ;; and hack its mode line.
		    (set-buffer buffer)
		    (let ((buffer-read-only nil)
			  (status (if compilation-exit-message-function
				      (funcall compilation-exit-message-function
					       proc msg)
				    (cons msg (process-exit-status proc)))))
		      (setq omax (point-max)
			    opoint (point))
		      (goto-char omax)
		      ;; Record where we put the message, so we can ignore it
		      ;; later on.
		      (insert ?\n mode-name " " (car status))
		      (forward-char -1)
		      (insert " at " (substring (current-time-string) 0 19))
		      (forward-char 1)
		      (setq mode-line-process
			    (concat ":"
				    (symbol-name (process-status proc))
				    (if (zerop (process-exit-status proc))
					" OK"
					(setq estatus
					      (format " [exit-status %d]"
						      (process-exit-status proc))))
				    ))
		      ;; XEmacs - tedium should let you know when it's ended...
		      (if (and (not compilation-always-signal-completion)
			       window
			       (pos-visible-in-window-p (point-max) window))
			  nil		; assume that the user will see it...
			(ding t 'ready)
			(display-message
			 'progress
			 (format "Compilation process completed%s."
				 (or estatus " successfully")
				 )))
		      ;; Since the buffer and mode line will show that the
		      ;; process is dead, we can delete it now.  Otherwise it
		      ;; will stay around until M-x list-processes.
		      (delete-process proc)
		      ;; Force mode line redisplay soon.
		      (redraw-modeline))
		    (if (and opoint (< opoint omax))
			(goto-char opoint))
		    (if compilation-finish-function
			(funcall compilation-finish-function buffer msg)))
		(set-buffer obuf))))
	  (setq compilation-in-progress (delq proc compilation-in-progress))
	  ))))

(defun compilation-filter (proc string)
  "Process filter for compilation buffers.
Just inserts the text, but uses `insert-before-markers'."
  (if (buffer-name (process-buffer proc))
      (save-excursion
	(set-buffer (process-buffer proc))
	(let ((buffer-read-only nil))
	  (save-excursion
	    (goto-char (process-mark proc))
	    (insert-before-markers string)
	    (run-hooks 'compilation-filter-hook)
	    (set-marker (process-mark proc) (point)))))))

(defun compile-error-at-point ()
  "Return the cdr of `compilation-old-error-list' for error containing point."
  (compile-reinitialize-errors nil (point))
  (let ((errors compilation-old-error-list))
    (while (and errors
		(> (point) (car (car errors))))
      (setq errors (cdr errors)))
    errors))

(defsubst compilation-buffer-p (buffer)
  (save-excursion
    (set-buffer buffer)
    (or compilation-shell-minor-mode compilation-minor-mode
	(eq major-mode 'compilation-mode))))

(defun compilation-next-error (n)
  "Move point to the next error in the compilation buffer.
Prefix arg N says how many error messages to move forwards (or
backwards, if negative).
Does NOT find the source line like \\[next-error]."
  (interactive "p")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (setq compilation-last-buffer (current-buffer))

  (let ((errors (compile-error-at-point)))

    ;; Move to the error after the one containing point.
    (goto-char (car (if (< n 0)
			(let ((i 0)
			      (e compilation-old-error-list))
			  ;; See how many cdrs away ERRORS is from the start.
			  (while (not (eq e errors))
			    (setq i (1+ i)
				  e (cdr e)))
			  (if (> (- n) i)
			      (error "Moved back past first error")
			    (nth (+ i n) compilation-old-error-list)))
		      (let ((compilation-error-list (cdr errors)))
			(compile-reinitialize-errors nil nil n)
			(if compilation-error-list
			    (nth (1- n) compilation-error-list)
			  (error "Moved past last error"))))))))

(defun compilation-previous-error (n)
  "Move point to the previous error in the compilation buffer.
Prefix arg N says how many error messages to move backwards (or
forwards, if negative).
Does NOT find the source line like \\[next-error]."
  (interactive "p")
  (compilation-next-error (- n)))


;; Given an elt of `compilation-error-list', return an object representing
;; the referenced file which is equal to (but not necessarily eq to) what
;; this function would return for another error in the same file.
(defsubst compilation-error-filedata (data)
  (setq data (cdr data))
  (if (markerp data)
      (marker-buffer data)
    (car data)))

;; Return a string describing a value from compilation-error-filedata.
;; This value is not necessarily useful as a file name, but should be
;; indicative to the user of what file's errors are being referred to.
(defsubst compilation-error-filedata-file-name (filedata)
  (if (bufferp filedata)
      (buffer-file-name filedata)
    (car filedata)))

(defun compilation-next-file (n)
  "Move point to the next error for a different file than the current one."
  (interactive "p")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (setq compilation-last-buffer (current-buffer))

  (let ((reversed (< n 0))
	errors filedata)

    (if (not reversed)
	(setq errors (or (compile-error-at-point)
			 (error "Moved past last error")))

      ;; Get a reversed list of the errors up through the one containing point.
      (compile-reinitialize-errors nil (point))
      (setq errors (reverse compilation-old-error-list)
	    n (- n))

      ;; Ignore errors after point.  (car ERRORS) will be the error
      ;; containing point, (cadr ERRORS) the one before it.
      (while (and errors
		  (< (point) (car (car errors))))
	(setq errors (cdr errors))))

    (while (> n 0)
      (setq filedata (compilation-error-filedata (car errors)))

      ;; Skip past the following errors for this file.
      (while (equal filedata
		    (compilation-error-filedata
		     (car (or errors
			      (if reversed
				  (error "%s the first erring file"
					 (compilation-error-filedata-file-name
					  filedata))
				(let ((compilation-error-list nil))
				  ;; Parse some more.
				  (compile-reinitialize-errors nil nil 2)
				  (setq errors compilation-error-list)))
			      (error "%s is the last erring file"
				     (compilation-error-filedata-file-name
				      filedata))))))
	(setq errors (cdr errors)))

      (setq n (1- n)))

    ;; Move to the following error.
    (goto-char (car (car (or errors
			     (if reversed
				 (error "This is the first erring file")
			       (let ((compilation-error-list nil))
				 ;; Parse the last one.
				 (compile-reinitialize-errors nil nil 1)
				 compilation-error-list))))))))

(defun compilation-previous-file (n)
  "Move point to the previous error for a different file than the current one."
  (interactive "p")
  (compilation-next-file (- n)))

(defun kill-compilation ()
  "Kill the process made by the \\[compile] command."
  (interactive)
  (let ((buffer (compilation-find-buffer)))
    (if (get-buffer-process buffer)
	(interrupt-process (get-buffer-process buffer))
      (error "The compilation process is not running"))))


;; Parse any new errors in the compilation buffer,
;; or reparse from the beginning if the user has asked for that.
(defun compile-reinitialize-errors (reparse
                                    &optional limit-search find-at-least)
  (save-excursion
    ;; XEmacs change: Below we made a change to possibly change the
    ;; selected window.  If we don't save and restore the old window
    ;; then if we get an error such as 'no more errors' we'll end up
    ;; in the compilation buffer.
    (save-window-excursion
      (set-buffer compilation-last-buffer)
      ;; If we are out of errors, or if user says "reparse",
      ;; discard the info we have, to force reparsing.
      (if (or (eq compilation-error-list t)
	      reparse)
	  (compilation-forget-errors))
      (if (and compilation-error-list
	       (or (not limit-search)
		 (> compilation-parsing-end limit-search))
	     (or (not find-at-least)
		 (>= (length compilation-error-list) find-at-least)))
	;; Since compilation-error-list is non-nil, it points to a specific
	;; error the user wanted.  So don't move it around.
	nil

      ;; XEmacs change: if the compilation buffer is already visible
      ;; in a window, use that instead of thrashing the display.
      (let ((w (get-buffer-window compilation-last-buffer)))
	(if w
	    (select-window w)
	  (switch-to-buffer compilation-last-buffer)))

      ;; This was here for a long time (before my rewrite); why? --roland
      ;;(switch-to-buffer compilation-last-buffer)
      (set-buffer-modified-p nil)
      (if (< compilation-parsing-end (point-max))
	  ;; compilation-error-list might be non-nil if we have a non-nil
	  ;; LIMIT-SEARCH or FIND-AT-LEAST arg.  In that case its value
	  ;; records the current position in the error list, and we must
	  ;; preserve that after reparsing.
	  (let ((error-list-pos compilation-error-list))
	    (funcall compilation-parse-errors-function
		     limit-search
		     (and find-at-least
			  ;; We only need enough new parsed errors to reach
			  ;; FIND-AT-LEAST errors past the current
			  ;; position.
			  (- find-at-least (length compilation-error-list))))
	    ;; Remember the entire list for compilation-forget-errors.  If
	    ;; this is an incremental parse, append to previous list.  If
	    ;; we are parsing anew, compilation-forget-errors cleared
	    ;; compilation-old-error-list above.
	    (setq compilation-old-error-list
		  (nconc compilation-old-error-list compilation-error-list))
	    (if error-list-pos
		;; We started in the middle of an existing list of parsed
		;; errors before parsing more; restore that position.
		(setq compilation-error-list error-list-pos))
	    ))))))

;; XEmacs addition
;; FSF has added this by 19.34, but it is highly complex, why? -sb
(defun compile-mouse-goto-error (event)
  "Visit the source for the error under the mouse.
Use this command in a compilation log buffer."
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (compile-goto-error))

(defun compile-goto-error (&optional argp)
  "Visit the source for the error message point is on.
Use this command in a compilation log buffer.  Sets the mark at point there.
\\[universal-argument] as a prefix arg means to reparse the buffer's error messages first;
other kinds of prefix arguments are ignored."
  (interactive "P")
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (setq compilation-last-buffer (current-buffer))
  (compile-reinitialize-errors (consp argp) (point))

  ;; Move to bol; the marker for the error on this line will point there.
  (beginning-of-line)

  ;; Move compilation-error-list to the elt of compilation-old-error-list
  ;; we want.
  (setq compilation-error-list compilation-old-error-list)
  (while (and compilation-error-list
	      (> (point) (car (car compilation-error-list))))
    (setq compilation-error-list (cdr compilation-error-list)))

  ;; Move to another window, so that next-error's window changes
  ;; result in the desired setup.
  (or (one-window-p)
      (progn
	(other-window -1)
	;; other-window changed the selected buffer,
	;; but we didn't want to do that.
	(set-buffer compilation-last-buffer)))

  (push-mark)
  (next-error 1))

;; XEmacs addition
(defun compile-mouse-maybe-goto-error (event &optional click-count)
  (interactive "e")
  (if (equal (event-button event) 2)
      (let ((buffer (current-buffer))
	    (point (point))
	    (config (current-window-configuration)))
	(condition-case nil
	    (progn
	      (compile-mouse-goto-error event)
	      t)
	  (error
	   (set-window-configuration config)
	   (set-buffer buffer)
	   (goto-char point)
	   nil)))))

;; Return a compilation buffer.
;; If the current buffer is a compilation buffer, return it.
;; If compilation-last-buffer is set to a live buffer, use that.
;; Otherwise, look for a compilation buffer and signal an error
;; if there are none.
(defun compilation-find-buffer (&optional other-buffer)
  (if (and (not other-buffer)
	   (compilation-buffer-p (current-buffer)))
      ;; The current buffer is a compilation buffer.
      (current-buffer)
    (if (and compilation-last-buffer (buffer-name compilation-last-buffer)
	     (compilation-buffer-p compilation-last-buffer)
	     (or (not other-buffer) (not (eq compilation-last-buffer
					     (current-buffer)))))
	compilation-last-buffer
      (let ((buffers (buffer-list)))
	(while (and buffers (or (not (compilation-buffer-p (car buffers)))
				(and other-buffer
				     (eq (car buffers) (current-buffer)))))
	  (setq buffers (cdr buffers)))
	(if buffers
	    (car buffers)
	  (or (and other-buffer
		   (compilation-buffer-p (current-buffer))
		   ;; The current buffer is a compilation buffer.
		   (progn
		     (if other-buffer
			 (message "This is the only compilation buffer."))
		     (current-buffer)))
	      (error "No compilation started!")))))))

;;;###autoload
(defun next-error (&optional argp)
  "Visit next compilation error message and corresponding source code.

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
\`compilation-error-regexp-alist' for customization ideas."
  (interactive "P")
  (setq compilation-last-buffer (compilation-find-buffer))
  (compilation-goto-locus (compilation-next-error-locus
			   ;; We want to pass a number here only if
			   ;; we got a numeric prefix arg, not just C-u.
			   (and (not (consp argp))
				(prefix-numeric-value argp))
			   (consp argp))))
;;;###autoload (define-key ctl-x-map "`" 'next-error)

;; XEmacs change
;;;###autoload
(defun previous-error (&optional argp)
  "Visit previous compilation error message and corresponding source code.

A prefix ARGP specifies how many error messages to move;
negative means move forward to next error messages.

This operates on the output from the \\[compile] and \\[grep] commands."
  (interactive "P")
  (next-error (cond ((null argp) -1)
		    ((numberp argp) (- argp))
		    (t argp))))

;;;###autoload
(defun first-error ()
  "Reparse the error message buffer and start at the first error.
Visit corresponding source code.
This operates on the output from the \\[compile] command."
  (interactive)
  (next-error '(4)))

(defun compilation-next-error-locus (&optional move reparse silent)
  "Visit next compilation error and return locus in corresponding source code.
This operates on the output from the \\[compile] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones.

Returns a cons (ERROR . SOURCE) of two markers: ERROR is a marker at the
location of the error message in the compilation buffer, and SOURCE is a
marker at the location in the source code indicated by the error message.

Optional first arg MOVE says how many error messages to move forwards (or
backwards, if negative); default is 1.  Optional second arg REPARSE, if
non-nil, says to reparse the error message buffer and reset to the first
error (plus MOVE - 1).  If optional third argument SILENT is non-nil, return
nil instead of raising an error if there are no more errors.

The current buffer should be the desired compilation output buffer."
  (or move (setq move 1))
  (compile-reinitialize-errors reparse nil (and (not reparse)
						(if (< move 1) 0 (1- move))))
  (let (next-errors next-error)
    (catch 'no-next-error
      (save-excursion
	(set-buffer compilation-last-buffer)
	;; compilation-error-list points to the "current" error.
	(setq next-errors
	      (if (> move 0)
		  (nthcdr (1- move)
			  compilation-error-list)
                ;; Zero or negative arg; we need to move back in the list.
                (let ((n (1- move))
                      (i 0)
                      (e compilation-old-error-list))
                  ;; See how many cdrs away the current error is from the start.
                  (while (not (eq e compilation-error-list))
                    (setq i (1+ i)
                          e (cdr e)))
                  (if (> (- n) i)
                      (error "Moved back past first error")
		    (nthcdr (+ i n) compilation-old-error-list))))
	      next-error (car next-errors))
	(while
	    (if (null next-error)
		(progn
		  (and move (/= move 1)
		       (error (if (> move 0)
				  "Moved past last error"
                                "Moved back past first error")))
;; Forget existing error messages if compilation has finished.
;;; XEmacs change by Barry Warsaw.
;;; Without this, if you get a "no more errors" error, then you can't do
;;; previous-error or goto-error until you kill the buffer.
;		  (if (not (and (get-buffer-process (current-buffer))
;				(eq (process-status
;				     (get-buffer-process
;				      (current-buffer)))
;				    'run)))
;		      (compilation-forget-errors))
		  (if silent
		      (throw 'no-next-error nil)
		    (error (concat compilation-error-message
				   (and (get-buffer-process (current-buffer))
					(eq (process-status
					     (get-buffer-process
					      (current-buffer)))
					    'run)
					" yet")))))
	      (setq compilation-error-list (cdr next-errors))
	      (if (null (cdr next-error))
		  ;; This error is boring.  Go to the next.
		  t
		(or (markerp (cdr next-error))
		    ;; This error has a filename/lineno pair.
		    ;; Find the file and turn it into a marker.
		    (let* ((fileinfo (car (cdr next-error)))
			   (cbuf (current-buffer)) ;XEmacs addition
			   (buffer (apply 'compilation-find-file
					  (car next-error) fileinfo)))
		      (if (null buffer)
			  ;; We can't find this error's file.
			  ;; Remove all errors in the same file.
			  (progn
			    (setq next-errors compilation-old-error-list)
			    (while next-errors
			      (and (consp (cdr (car next-errors)))
				   (equal (car (cdr (car next-errors)))
					  fileinfo)
				   (progn
				     (set-marker (car (car next-errors)) nil)
				     (setcdr (car next-errors) nil)))
			      (setq next-errors (cdr next-errors)))
			    ;; Look for the next error.
			    t)
			;; We found the file.  Get a marker for this error.
			;; compilation-old-error-list and
			;; compilation-error-screen-columns are buffer-local
			;; so we must be careful to extract their value
			;; before switching to the source file buffer.
			(let ((errors compilation-old-error-list)
			      (columns compilation-error-screen-columns)
			      (last-line (nth 1 (cdr next-error)))
			      (column (nth 2 (cdr next-error))))
			  (set-buffer buffer)
			  (save-excursion
			    (save-restriction
			      (widen)
			      (goto-line last-line)
			      (if (and column (> column 0))
				  ;; Columns in error msgs are 1-origin.
				  (if columns
				      (move-to-column (1- column))
				    (forward-char (1- column)))
				(beginning-of-line))
			      (setcdr next-error (point-marker))
			      ;; Make all the other error messages referring
			      ;; to the same file have markers into the buffer.
			      (while errors
				(and (consp (cdr (car errors)))
				     (equal (car (cdr (car errors))) fileinfo)
				     (let* ((this (nth 1 (cdr (car errors))))
					    (column (nth 2 (cdr (car errors))))
					    (lines (- this last-line)))
				       (if (eq selective-display t)
					   ;; When selective-display is t,
					   ;; each C-m is a line boundary,
					   ;; as well as each newline.
					   (if (< lines 0)
					       (re-search-backward "[\n\C-m]"
								   nil 'end
								   (- lines))
					     (re-search-forward "[\n\C-m]"
								nil 'end
								lines))
					 (forward-line lines))
				       (if (and column (> column 1))
					   (if columns
					       (move-to-column (1- column))
					     (forward-char (1- column)))
					 (beginning-of-line))
				       (setq last-line this)
				       (setcdr (car errors) (point-marker))))
				(setq errors (cdr errors)))))
			  ;; XEmacs addition
			  (set-buffer cbuf)))))
                ;; If we didn't get a marker for this error, or this
                ;; marker's buffer was killed, go on to the next one.
                (or (not (markerp (cdr next-error)))
                    (not (marker-buffer (cdr next-error))))))
	  (setq next-errors compilation-error-list
		next-error (car next-errors)))

	;; XEmacs -- move this inside save-excursion
	;; Skip over multiple error messages for the same source location,
	;; so the next C-x ` won't go to an error in the same place.
	(while (and compilation-error-list
		    (equal (cdr (car compilation-error-list))
			   (cdr next-error)))
	  (setq compilation-error-list (cdr compilation-error-list)))
	))

    ;; XEmacs change: If a new window has to be displayed, select the other
    ;; window to avoid swapping the position of the compilation error buffer.
    (and next-error (get-buffer-window (marker-buffer (car next-error)))
         (progn
           (select-window (get-buffer-window (marker-buffer (car next-error))))
           (other-window -1)))
	  
    ;; We now have a marker for the position of the error source code.
    ;; NEXT-ERROR is a cons (ERROR . SOURCE) of two markers.
    next-error))

(defun compilation-goto-locus (next-error)
  "Jump to an error locus returned by `compilation-next-error-locus'.
Takes one argument, a cons (ERROR . SOURCE) of two markers.
Selects a window with point at SOURCE, with another window displaying ERROR."
;; XEmacs: this code is horrendous, and makes windows do all sorts of
;; weird things when you're using separate frames for the compilation
;; and source buffer.
;  (if (and (window-dedicated-p (selected-window))
;	   (eq (selected-window) (frame-root-window)))
;      (switch-to-buffer-other-frame (marker-buffer (cdr next-error)))
;    (switch-to-buffer (marker-buffer (cdr next-error))))
;  (goto-char (cdr next-error))
;  ;; If narrowing got in the way of
;  ;; going to the right place, widen.
;  (or (= (point) (marker-position (cdr next-error)))
;      (progn
;        (widen)
;        (goto-char (cdr next-error))))
;
;  ;; Show compilation buffer in other window, scrolled to this error.
;  (let* ((pop-up-windows t)
;	 (w (or (get-buffer-window (marker-buffer (car next-error)) 'visible)
;		(display-buffer (marker-buffer (car next-error))))))
;    (set-window-point w (car next-error))
;    (set-window-start w (car next-error))
;    (compilation-set-window-height w)))

  (let* ((pop-up-windows t)
	 (compilation-buffer (marker-buffer (car next-error)))
	 (source-buffer (marker-buffer (cdr next-error)))
	 ;; make sure compilation buffer is visible ...
	 (compilation-window
	 ;; Use an existing window if it is in a visible frame.
	  (or (get-buffer-window compilation-buffer 'visible)
	      ;; Pop up a window.
	      (display-buffer compilation-buffer))))

    ;; now, make the compilation buffer **STAY WHERE IT IS** and
    ;; make sure the source buffer is visible

    (select-window compilation-window)
    (pop-to-buffer source-buffer)

    ;; now put things aright in the compilation window.
    (set-window-point compilation-window (car next-error))
    (set-window-start compilation-window (car next-error))
    (compilation-set-window-height compilation-window)

    ;; now put things aright in the source window.

    (set-buffer source-buffer)
    (goto-char (cdr next-error))
    ;; If narrowing got in the way of
    ;; going to the right place, widen.
    (or (= (point) (marker-position (cdr next-error)))
	(progn
	  (widen)
	  (goto-char (cdr next-error))))))
    

;; helper routine to compilation-find-file
(defun compare-file-to-buffer (filename buffer)
  "compare BUFFER to FILENAME and see if they are really the same. Returns nil
if not, BUFFER if they are"
  (interactive "FCompare FILE : 
bTo BUFFER : ")
  ;; (print (format "%s %s " buffer filename))
  (if (and (stringp filename) (bufferp buffer))
      (save-excursion
	(set-buffer buffer)
	(if (equal (file-truename (buffer-file-name))
		   (file-truename filename))
	    buffer
	  nil))
    nil)
)

;; Find a buffer for file FILENAME.
;; Search the directories in compilation-search-path.
;; A nil in compilation-search-path means to try the
;; current directory, which is passed in DIR.
;; If FILENAME is not found at all, ask the user where to find it.
;; Pop up the buffer containing MARKER and scroll to MARKER if we ask the user.
(defun compilation-find-file (marker filename dir &rest formats)
    (or formats (setq formats '("%s")))
    (let* ((dirs compilation-search-path)
	   (true-file-name (file-name-nondirectory filename))
	   (buffer (get-buffer true-file-name)) 
	   thisdir fmts name)
      (flet ((push-file-name-history (name)
	       (setq file-name-history
		     (if minibuffer-history-uniquify
			 (cons name (remove name file-name-history))
		       (cons name file-name-history)))))
	;; first see if buffer is there already
	(if (compare-file-to-buffer buffer filename)
	    (switch-to-buffer buffer)
	  (progn
	    (setq buffer nil) ; its the wrong buffer, so forget it
	    (if (file-name-absolute-p filename)
		;; The file name is absolute.  Use its explicit directory as
		;; the first in the search path, and strip it from FILENAME.
		(setq filename (abbreviate-file-name (expand-file-name filename))
		      dirs (cons (file-name-directory filename) dirs)
		      filename (file-name-nondirectory filename)))
	    ;; Now search the path.
	    (while (and dirs (null buffer))
	      (setq thisdir (or (car dirs) dir)
		    fmts formats)
	      ;; For each directory, try each format string.
	      (while (and fmts (null buffer))
		(setq name (expand-file-name (format (car fmts) filename) thisdir)
		      buffer (and (file-exists-p name)
				  (progn
				    (push-file-name-history name)
				    (find-file-noselect name)))
		    fmts (cdr fmts)))
	    (setq dirs (cdr dirs)))
	  (or buffer
	      ;; The file doesn't exist.
	      ;; Ask the user where to find it.
	      ;; If he hits C-g, then the next time he does
	      ;; next-error, he'll skip past it.
	      (let* ((pop-up-windows t)
		     (w (display-buffer (marker-buffer marker))))
		(set-window-point w marker)
		(set-window-start w marker)
		(let ((name (expand-file-name
			     (read-file-name
			      (format "Find this error in: (default %s) "
				      filename)
			      dir filename t))))
		  (if (file-directory-p name)
		    (setq name (expand-file-name filename name)))
		  (and (file-exists-p name)
		       (progn
			 (push-file-name-history name)
			 (find-file-noselect name)))))))))))

;; Set compilation-error-list to nil, and unchain the markers that point to the
;; error messages and their text, so that they no longer slow down gap motion.
;; This would happen anyway at the next garbage collection, but it is better to
;; do it right away.
(defun compilation-forget-errors ()
  (while compilation-old-error-list
    (let ((next-error (car compilation-old-error-list)))
      (set-marker (car next-error) nil)
      (if (markerp (cdr next-error))
	  (set-marker (cdr next-error) nil)))
    (setq compilation-old-error-list (cdr compilation-old-error-list)))
  (setq compilation-error-list nil
	compilation-directory-stack nil
	compilation-parsing-end 1))


(defun count-regexp-groupings (regexp)
  "Return the number of \\( ... \\) groupings in REGEXP (a string)."
  (let ((groupings 0)
	(len (length regexp))
	(i 0)
	c)
    (while (< i len)
      (setq c (aref regexp i)
	    i (1+ i))
      (cond ((= c ?\[)
	     ;; Find the end of this [...].
	     (while (and (< i len)
			 (not (= (aref regexp i) ?\])))
	       (setq i (1+ i))))
	    ((= c ?\\)
	     (if (< i len)
		 (progn
		   (setq c (aref regexp i)
			 i (1+ i))
		   (if (= c ?\))
		       ;; We found the end of a grouping,
		       ;; so bump our counter.
		       (setq groupings (1+ groupings))))))))
    groupings))

(defun compilation-parse-errors (limit-search find-at-least)
  "Parse the current buffer as grep, cc or lint error messages.
See variable `compilation-parse-errors-function' for the interface it uses."
  (setq compilation-error-list nil)
  (display-message 'progress "Parsing error messages...")
  (let (;;text-buffer -- unused
	orig orig-expanded parent-expanded
	regexp enter-group leave-group error-group
	alist subexpr error-regexp-groups
	(found-desired nil)
	(compilation-num-errors-found 0)
        (message-freq (max 1 (/ (count-lines (point-min) (point-max)) 50))))

    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(progn
	  (forward-line 2)
	  ;; Move back so point is before the newline.
	  ;; This matters because some error regexps use \n instead of ^
	  ;; to be faster.
	  (forward-char -1)))

    ;; Compile all the regexps we want to search for into one.
    (setq regexp (concat "\\(" compilation-enter-directory-regexp "\\)\\|"
			 "\\(" compilation-leave-directory-regexp "\\)\\|"
			 "\\(" (mapconcat (function
					   (lambda (elt)
					     (concat "\\(" (car elt) "\\)")))
					  compilation-error-regexp-alist
					  "\\|") "\\)"))

    ;; Find out how many \(...\) groupings are in each of the regexps, and set
    ;; *-GROUP to the grouping containing each constituent regexp (whose
    ;; subgroups will come immediately thereafter) of the big regexp we have
    ;; just constructed.
    (setq enter-group 1
	  leave-group (+ enter-group
			 (count-regexp-groupings
			  compilation-enter-directory-regexp)
			 1)
	  error-group (+ leave-group
			 (count-regexp-groupings
			  compilation-leave-directory-regexp)
			 1))

    ;; Compile an alist (IDX FILE LINE [COL]), where IDX is the number of
    ;; the subexpression for an entire error-regexp, and FILE and LINE (and
    ;; possibly COL) are the numbers for the subexpressions giving the file
    ;; name and line number (and possibly column number).
    (setq alist (or compilation-error-regexp-alist
		    (error "compilation-error-regexp-alist is empty!"))
	  subexpr (1+ error-group))
    (while alist
      (setq error-regexp-groups
	    (cons (list subexpr
			(+ subexpr (nth 1 (car alist)))
			(+ subexpr (nth 2 (car alist)))
			;;#### This is buggy in FSFmacs
			(let ((col (nth 3 (car alist))))
			  (and col
			       (+ subexpr col))))
		  error-regexp-groups))
      (setq subexpr (+ subexpr 1 (count-regexp-groupings (car (car alist)))))
      (setq alist (cdr alist)))

    ;; Set up now the expanded, abbreviated directory variables
    ;; that compile-abbreviate-directory will need, so we can
    ;; compute them just once here.
    (setq orig (abbreviate-file-name default-directory)
	  orig-expanded (abbreviate-file-name
			 (file-truename default-directory))
	  parent-expanded (abbreviate-file-name
			   (expand-file-name "../" orig-expanded)))

    (while (and (not found-desired)
		;; We don't just pass LIMIT-SEARCH to re-search-forward
		;; because we want to find matches containing LIMIT-SEARCH
		;; but which extend past it.
		;; Instead of using re-search-forward,
		;; we use this loop which tries only at each line.
		(progn
		  (while (and (not (eobp))
			      (not (looking-at regexp)))
		    (forward-line 1))
		  (not (eobp))))

      ;; Move to the end of the match we just found.
      (goto-char (match-end 0))

      ;; Figure out which constituent regexp matched.
      (cond ((match-beginning enter-group)
	     ;; The match was the enter-directory regexp.
	     (let ((dir
		    (file-name-as-directory
		     (expand-file-name
		      (buffer-substring (match-beginning (+ enter-group 1))
					(match-end (+ enter-group 1)))))))
	       ;; The directory name in the "entering" message
	       ;; is a truename.  Try to convert it to a form
	       ;; like what the user typed in.
	       (setq dir
		     (compile-abbreviate-directory dir orig orig-expanded
						   parent-expanded))
	       (setq compilation-directory-stack
		     (cons dir compilation-directory-stack))
	       (and (file-directory-p dir)
		    (setq default-directory dir)))

	     (and limit-search (>= (point) limit-search)
		  ;; The user wanted a specific error, and we're past it.
		  ;; We do this check here (and in the leave-group case)
		  ;; rather than at the end of the loop because if the last
		  ;; thing seen is an error message, we must carefully
		  ;; discard the last error when it is the first in a new
		  ;; file (see below in the error-group case).
		  (setq found-desired t)))
	    
	    ((match-beginning leave-group)
	     ;; The match was the leave-directory regexp.
	     (let ((beg (match-beginning (+ leave-group 1)))
		   (stack compilation-directory-stack))
	       (if beg
		   (let ((dir
			  (file-name-as-directory
			   (expand-file-name
			    (buffer-substring beg
					      (match-end (+ leave-group
							    1)))))))
		     ;; The directory name in the "leaving" message
		     ;; is a truename.  Try to convert it to a form
		     ;; like what the user typed in.
		     (setq dir
			   (compile-abbreviate-directory dir orig orig-expanded
							 parent-expanded))
		     (while (and stack
				 (not (string-equal (car stack) dir)))
		       (setq stack (cdr stack)))))
	       (setq compilation-directory-stack (cdr stack))
	       (setq stack (car compilation-directory-stack))
	       (if stack
		   (setq default-directory stack))
	       )

	     (and limit-search (>= (point) limit-search)
		  ;; The user wanted a specific error, and we're past it.
		  ;; We do this check here (and in the enter-group case)
		  ;; rather than at the end of the loop because if the last
		  ;; thing seen is an error message, we must carefully
		  ;; discard the last error when it is the first in a new
		  ;; file (see below in the error-group case).
		  (setq found-desired t)))
	    
	    ((match-beginning error-group)
	     ;; The match was the composite error regexp.
	     ;; Find out which individual regexp matched.
	     (setq alist error-regexp-groups)
	     (while (and alist
			 (null (match-beginning (car (car alist)))))
	       (setq alist (cdr alist)))
	     (if alist
		 (setq alist (car alist))
	       (error "compilation-parse-errors: impossible regexp match!"))
	     
	     ;; Extract the file name and line number from the error message.
	     (let ((beginning-of-match (match-beginning 0)) ;looking-at nukes
		   (filename (buffer-substring (match-beginning (nth 1 alist))
					       (match-end (nth 1 alist))))
		   (linenum (string-to-int
			     (buffer-substring
			      (match-beginning (nth 2 alist))
			      (match-end (nth 2 alist)))))
		   (column (and (nth 3 alist)
				(match-beginning (nth 3 alist))
				(string-to-int
				 (buffer-substring
				  (match-beginning (nth 3 alist))
				  (match-end (nth 3 alist)))))))

	       ;; Check for a comint-file-name-prefix and prepend it if
	       ;; appropriate.  (This is very useful for
	       ;; compilation-minor-mode in an rlogin-mode buffer.)
	       (and (boundp 'comint-file-name-prefix)
		    ;; If the file name is relative, default-directory will
		    ;; already contain the comint-file-name-prefix (done by
		    ;; compile-abbreviate-directory).
		    (file-name-absolute-p filename)
		    (setq filename (concat comint-file-name-prefix filename)))

	       ;; Some compilers (e.g. Sun's java compiler, reportedly)
	       ;; produce bogus file names like "./bar//foo.c" for the file
	       ;; "bar/foo.c"; expand-file-name will collapse these into
	       ;; "/foo.c" and fail to find the appropriate file.  So we look
	       ;; for doubled slashes in the file name and fix them up in the
	       ;; buffer.
	       (when (fboundp 'command-line-normalize-file-name)
		 (setq filename (command-line-normalize-file-name filename)))
	       (setq filename (cons filename (cons default-directory
						   (nthcdr 4 alist))))
				     

	       ;; Locate the erring file and line.
	       ;; Cons a new elt onto compilation-error-list,
	       ;; giving a marker for the current compilation buffer
	       ;; location, and the file and line number of the error.
	       (save-excursion
		 ;; Save as the start of the error the beginning of the
		 ;; line containing the match unless the match starts at a
		 ;; newline, in which case the beginning of the next line.
		 (goto-char beginning-of-match)
		 (forward-line (if (eolp) 1 0))
		 (let ((this (cons (point-marker)
				   (list filename linenum column))))
		   ;; Don't add the same source line more than once.
		   (if (equal (cdr this) (cdr (car compilation-error-list)))
		       nil
		     (setq compilation-error-list
			   (cons this
				 compilation-error-list))
		     (setq compilation-num-errors-found
			   (1+ compilation-num-errors-found)))))
	       (and (or (and find-at-least (> compilation-num-errors-found
					      find-at-least))
			(and limit-search (>= (point) limit-search)))
		    ;; We have found as many new errors as the user wants,
		    ;; or past the buffer position he indicated.  We
		    ;; continue to parse until we have seen all the
		    ;; consecutive errors in the same file, so the error
                    ;; positions will be recorded as markers in this buffer
                    ;; that might change.
		    (cdr compilation-error-list) ; Must check at least two.
		    (not (equal (car (cdr (nth 0 compilation-error-list)))
				(car (cdr (nth 1 compilation-error-list)))))
		    (progn
		      ;; Discard the error just parsed, so that the next
		      ;; parsing run can get it and the following errors in
		      ;; the same file all at once.  If we didn't do this, we
		      ;; would have the same problem we are trying to avoid
		      ;; with the test above, just delayed until the next run!
		      (setq compilation-error-list
			    (cdr compilation-error-list))
		      (goto-char beginning-of-match)
		      (setq found-desired t)))
	       )
	     )
	    (t
	     (error "compilation-parse-errors: known groups didn't match!")))

      (when (= (% compilation-num-errors-found message-freq) 0)
        (display-message
	 'progress
	 (format "Parsing error messages...%d (%.0f%% of buffer)"
                 compilation-num-errors-found
                 ;; Use floating-point because (* 100 (point)) frequently
                 ;; exceeds the range of Emacs Lisp integers.
                 (/ (* 100.0 (point)) (point-max)))))

;;; This is broken - it foils the logic above which is supposed to ensure
;;; that all errors for the current file are found before we quit. 
;      (and limit-search (>= (point) limit-search)
;	   ;; The user wanted a specific error, and we're past it.
;	   (setq found-desired t))
      )
    (setq compilation-parsing-end (if found-desired
				      (point)
				    ;; We have searched the whole buffer.
				    (point-max))))
  (setq compilation-error-list (nreverse compilation-error-list))
  (display-message 'progress "Parsing error messages...done"))

(defun compile-buffer-substring (index)
  "Get substring matched by INDEXth subexpression."
  (if index
      (let ((beg (match-beginning index)))
	(if beg (buffer-substring beg (match-end index))))))

;; If directory DIR is a subdir of ORIG or of ORIG's parent,
;; return a relative name for it starting from ORIG or its parent.
;; ORIG-EXPANDED is an expanded version of ORIG.
;; PARENT-EXPANDED is an expanded version of ORIG's parent.
;; Those two args could be computed here, but we run faster by
;; having the caller compute them just once.
(defun compile-abbreviate-directory (dir orig orig-expanded parent-expanded)
  ;; Apply canonical abbreviations to DIR first thing.
  ;; Those abbreviations are already done in the other arguments passed.
  (setq dir (abbreviate-file-name dir))

  ;; Check for a comint-file-name-prefix and prepend it if appropriate.
  ;; (This is very useful for compilation-minor-mode in an rlogin-mode
  ;; buffer.)
  (if (boundp 'comint-file-name-prefix)
      (setq dir (concat comint-file-name-prefix dir)))

  (if (and (> (length dir) (length orig-expanded))
	   (string= orig-expanded
		    (substring dir 0 (length orig-expanded))))
      (setq dir
	    (concat orig
		    (substring dir (length orig-expanded)))))
  (if (and (> (length dir) (length parent-expanded))
	   (string= parent-expanded
		    (substring dir 0 (length parent-expanded))))
    (setq dir
	  (concat (file-name-directory
		   (directory-file-name orig))
		  (substring dir (length parent-expanded)))))
  dir)

(provide 'compile)

;;; compile.el ends here
