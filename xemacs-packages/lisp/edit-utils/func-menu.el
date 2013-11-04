;;; func-menu.el --- Jump to a function within a buffer.

;; Author:     David Hughes <d.hughes@videonetworks.com>
;; Maintainer: The XEmacs Development Team <xemacs-beta@xemacs.org>
;; Version: 2.66
;; Keywords: tools, c, lisp

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Installation:
;; =============
;; (add-hook 'find-file-hooks 'fume-setup-buffer)
;; (add-hook 'Manual-mode-hook 'turn-on-fume-mode)
;;
;; #### NOTE: if you've been using func-menu the old way (before fume-mode was
;; written), please note that `mouse-function-menu' is bound to M-Button3
;; (not Shift-Button3) in this mode. Nothing prevents you from keeping your
;; old binding though.
;;
;;
;; Description:
;; ============
;; Suppose you have a file with a lot of functions in it. Well, this package
;; makes it easy to jump to any of those functions. The names of the
;; functions in the current buffer are automatically put into a menubar menu,
;; you select one of the function-names and the point is moved to that very
;; function. The mark is pushed on the mark-ring, so you can easily go back
;; to where you were. Alternatively, you can use enter the name of the
;; desired function via the minibuffer which offers completing read input. In
;; addition, the name of the function before point is optionally displayed in
;; the modeline.
;;
;; Support for non X Window versions of Emacs:
;; ===========================================
;; This package can also be used for non X versions of Emacs. In this case,
;; only modeline display and completing read input from the minibuffer are
;; possible.
;;
;; Modes supported:
;; ================
;; Ada, Assembly, BibTex, C++, C, Dired, Ehdm, ELisp, FORTRAN, Ksh, Latex,
;; Lelisp, Makefile, Maple, Modula2, Modula3, Outline, Objective-C, Pascal,
;; Perl, Postscript, Prolog, PVS, Python, SGML, Scheme, Tcl, Verilog,
;; Manual, Ruby, JavaScript
;;
;; Acknowledgements:
;; =================
;;
;; Manual mode support
;; fume-goto-\(next|previous\)-function functions
;; func-menu minor mode
;; Didier Verna <didier@xemacs.org>
;;
;; Protect integer concatenation with int-to-string
;; Anthony Rossini <arossini@biostat.washington.edu>
;;
;; Patch for jde-mode
;; Tony Lam <tonyl@Eng.Sun.COM>
;;
;; Patches to FORTRAN support
;; Bruce Ravel <ravel@phys.washington.edu>
;;
;; Idea for fume-goto-function-hook
;; Mark Gates <mgates@ncsa.uiuc.edu>
;;
;; Patch for backward compatibility with backquote
;; Bill Dubuque <wgd@martigny.ai.mit.edu>
;;
;; Objective-C support
;; Guillaume Laurent <glaurent@worldnet.fr>
;;
;; Fix to fume-function-name-regexp-c
;; Jonathan Edwards <edwards@intranet.com>
;;
;; Speedup for fume-cc-inside-comment
;; Peter Pezaris <pez@dwwc.com>
;;
;; Made menu placement more flexible and rewrote doc strings
;; Bob Weiner <weiner@altrasoft.com>
;;
;; Fortran90 regexp
;; John Turner <turner@xdiv.lanl.gov>
;;
;; Patch to error trap in fume-rescan-buffer
;; Andy Piper <andyp@parallax.co.uk>
;;
;; Java support
;; Bob Weiner <weiner@altrasoft.com>
;; Heddy Boubaker <boubaker@dgac.fr>
;;
;; Patch for fume-rescan-buffer{-trigger}
;; Christoph Wedler <wedler@vivaldi.fmi.uni-passau.de>
;;
;; Patch for fume-tickle-f-to-b
;; Michael Sperber <sperber@informatik.uni-tuebingen.de>
;;
;; Cleanup suggestions
;; Jonathan Stigelman <stig@hackvan.com>
;;
;; Idea for jumping directly with a mouse click
;; Marc Paquette <Marc.Paquette@Softimage.COM>
;;
;; Prolog mode additions based on functions for Postscript mode
;; Laszlo Teleki <laszlo@ipb.uni-bonn.de>
;;
;; Idea for displaying function name in modeline
;; Paul Filipski <filipski@blackhawk.com>
;;
;; Fame mode support
;; Cooper Vertz <cooper@prod2.imsi.com>
;;
;; Made fume-match-find-next-function-name iterative, not recursive, to avoid
;; blowing out the emacs stack on big files with lots of prototypes.
;; Joe Marshall <jrm@odi.com>
;;
;; Verilog support
;; Matt Sale <mdsale@icdc.delcoelect.com>
;;
;; Minibuffer interface & Pascal support
;; Espen Skoglund <espensk@stud.cs.uit.no>
;;
;; Python support
;; Shuichi Koga <skoga@virginia.edu>
;;
;; JavaScript support
;; Ville Skyttä <scop@xemacs.org>
;;
;; Maple support
;; Luc Tancredi <Luc.Tancredi@sophia.inria.fr>
;;
;; Combined Tcl and C++ function finder
;; Andy Piper <ajp@eng.cam.ac.uk>
;;
;; Perl Support
;; Alex Rezinsky <alexr@msil.sps.mot.com>
;; Michael Lamoureux <lamour@engin.umich.edu>
;;
;; Ruby Support
;; Stefan Kamphausen <mail@skamphausen.de>
;; Albert Davidson Chou <Al_Chou@CyberDude.com>
;; Claus Brunzema <mail@cbrunzema.de>
;;
;; Suggested mouse interface
;; Raymond L. Toy <toy@soho.crd.ge.com>
;;
;; Dired support
;; Improved modula support
;; Numerous code cleanups
;; Norbert Kiesel <norbert@i3.informatik.rwth-aachen.de>
;;
;; Makefile support
;; Suggested multi-choice sublisting
;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
;;
;; Suggestions for menubar entry
;; Andy Piper <ajp@eng.cam.ac.uk>
;;
;; Ada support
;; Scott Evans  <gse@ocsystems.com>
;; Mike Konerman <konerman@SSA.crane.navy.mil>
;; Michael Polo <mikep@polo.mn.org> <mikep@cfsmo.honeywell.com>
;;
;; Scheme, BibTeX, Ehdm & PVS support
;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;
;; Modula support
;; Geoffrey Wyant <gwyant@cloyd.east.sun.com>
;;
;; SGML support; submenu indexing
;; Thomas Plass <thomas.plass@mid-heidelberg.de>
;;
;; Extensions to fume-function-name-regexp-lisp
;; Vladimir Alexiev <vladimir@cs.ualberta.ca>
;; Kari Heinola <kph@dpe.fi>
;; Milo A. Chan <chan@jpmorgan.com>
;; Jack Repenning <jackr@step7.informix.com>
;; Cedric Beust <Cedric.Beust@sophia.inria.fr>
;; Joachim Krumnow <krumnow@srsir02.ext.sap-ag.de>
;;
;; ksh support
;; Philippe Bondono <bondono@vnet.ibm.com>
;;
;; FORTRAN support
;; Paul Emsley <paule@chem.gla.ac.uk>
;; Raymond L. Toy <toy@soho.crd.ge.com>
;; Richard Cognot <cognot@elfgrc.co.uk>
;; Greg Sjaardema <gdsjaar@sandia.gov>
;;
;; Latex support
;; Wolfgang Mettbach <wolle@uni-paderborn.de>
;; Paolo Frasconi <paolo@mcculloch.ing.unifi.it>
;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
;;
;; Assembly support
;; Bob Weiner <weiner@altrasoft.com>
;;
;; Removal of cl dependencies
;; Russell Ritchie <russell@gssec.bt.co.uk>
;;
;; C++ mode enhancemencements for func-menu
;; Andy Piper      <ajp@eng.cam.ac.uk>
;; Kevin R. Powell <powell@csl.ncsa.uiuc.edu>
;; Mats Lidell     <mats.lidell@eua.ericsson.se>
;; Mike Battaglia  <mbattagl@spd.dsccc.com>
;; Oliver Schittko <schittko@fokus.gmd.de>
;; Tom Murray      <tmurray@hpindck.cup.hp.com>
;; Russell Ritchie <russell@gssec.bt.co.uk>
;;
;; Tcl mode additions for func-menu
;; Andy Piper <ajp@eng.cam.ac.uk>
;; Jean-Michel Augusto <augusto@eurecom.fr>
;; Dr P.G. Sjoerdsma <pgs1002@esc.cam.ac.uk>
;;
;; Postscript mode additions for func-menu
;; Leigh Klotz <klotz@adoc.xerox.com>
;;
;; Suggestions for popup menu positioning
;; Marc Gemis <makke@wins.uia.ac.be>
;;
;; Original FSF package
;; Ake Stenhoff <etxaksf@aom.ericsson.se>

;;; Code:

(defmacro fume-bomb-proof (&rest forms)
  (` (condition-case () (progn (,@ forms)) (t nil))))

(eval-when-compile
  (byte-compiler-options
    (optimize t)
    (warnings (- free-vars unresolved)))
  ;; For older XEmacs, Win-Emacs
  (cond ((not (fume-bomb-proof (require 'custom)))
         (defmacro defgroup (sym def description &rest args)
           (` (defvar (, sym) (, def) (, description))))
         (defmacro defcustom (sym def description &rest args)
           (` (defvar (, sym) (, def) (, description)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;  Environment Initialisation  ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst fume-version "2.66")

(defconst fume-developer
  "The XEmacs Development Team <xemacs-beta@xemacs.org>")

(defun fume-about ()
  (interactive)
  (sit-for 0)
  (display-message
   'no-log
   (format "Func-Menu version %s" fume-version)))

(defconst fume-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(defmacro fume-defvar-local (var value &optional doc)
  "Defines VAR as an advertised variable.
Performs a defvar, then executes `make-variable-buffer-local' on
the variable.  Also sets the `permanent-local' property, so that
`kill-all-local-variables' (called by major-mode setting commands)
won't destroy func-menu control variables."
  (` (progn
       (if (, doc)
           (defvar (, var) (, value) (, doc))
         (defvar (, var) (, value)))
       (make-variable-buffer-local '(, var))
       (put '(, var) 'permanent-local t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;  Backward compatibility hacks for older versions of XEmacs  ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(or (fboundp 'display-message)
    (defun display-message (msgtype msg) (message msg)))

(or (fboundp 'defalias)
    ;; poor man's defalias
    (defun defalias (sym newdef)
      "Set SYMBOL's function definition to NEWVAL, and return NEWVAL.
Associates the function with the current load file, if any."
      (fset sym (symbol-function newdef))))

(or (fboundp 'selected-frame)
    (defalias 'selected-frame 'selected-screen))

(if (fboundp 'locate-window-from-coordinates)
    ;; Older versions of XEmacs need a more robust version of 'event-window'
    (defun fume-event-window (event)
      (or (event-window event)
          (locate-window-from-coordinates
           (selected-frame) (list (event-x event) (event-y event)))
          (locate-window-from-coordinates
           (selected-frame) (list (event-x event) (1- (event-y event))))))
  ;; In post 19.11 versions of XEmacs 'event-window' now works acceptably
  (defalias 'fume-event-window 'event-window))

(or (fboundp 'shrink-window-if-larger-than-buffer)
    ;; Win-Emacs doesn't have this
    (defun shrink-window-if-larger-than-buffer (&optional window reqd-height)
      "Shrink WINDOW to the smallest no of lines needed to display its buffer,
or to optional REQUIRED-HEIGHT if and only if that is larger.  Does nothing if
the buffer contains more lines than the present window height."
      (interactive)
      (let* ((OriginalWindow (selected-window))
             (TargetWindow (select-window (or window OriginalWindow))))
        (or (one-window-p t)
            (and reqd-height (>= reqd-height (window-height)))
            (< (window-height) (1+ (count-lines (point-min) (point-max))))
            (let ((calc-reqd-height
                   (if truncate-lines
                       (1+ (count-lines (point-min) (point-max)))
                     (save-excursion
                       (let ((count 0)
                             linew
                             (windw (window-width)))
                         (goto-char (point-min))
                         (while (not (eobp))
                           (setq linew (1+ (progn (end-of-line)
                                                  (current-column)))
                                 count (+ count
                                          (/ linew windw)
                                          (min (% linew windw) 1)))
                           (beginning-of-line 2))
                         count)))))
              (setq reqd-height (1+ (max calc-reqd-height
                                         (1- window-min-height)
                                         (or reqd-height 0))))
              (if (> (window-height) reqd-height)
                  (let* (wc spare bonus share wins shrunkwins)
                    (walk-windows
                     '(lambda (w)
                        (select-window w)
                        (if (or (eq w TargetWindow)
                                (> (1+ (count-lines (point-min) (point-max)))
                                   (1- (window-height w))))
                            (setq wins (cons w wins))
                          (if (= (1+ (count-lines (point-min) (point-max)))
                                 (1- (window-height w)))
                              (setq shrunkwins (cons w shrunkwins)))))
                     'nomini)
                    (setq wc (1- (length wins))
                          spare (- (window-height TargetWindow) reqd-height)
                          share (if (> wc 0) (/ spare wc))
                          bonus (if (> wc 0) (% spare wc))
                          shrunkwins (if (zerop wc) nil shrunkwins)
                          wins (mapcar (function
                                        (lambda (w)
                                          (cons w (list
                                                   (if (eq w TargetWindow)
                                                       reqd-height
                                                     (+ (window-height w)
                                                        share
                                                        (if (zerop bonus)
                                                            0
                                                          (setq bonus
                                                                (1- bonus))
                                                          1)))
                                                   (window-start w)))))
                                       wins))
                    (let (ok (trys 2))
                      (while (and (not ok) (> trys 0))
                        (setq trys (1- trys))
                        (mapcar
                         (function
                          (lambda (info)
                            (select-window (car info))
                            (enlarge-window
                             (- (car (cdr info)) (window-height)))))
                         wins)
                        (setq ok t)
                        (mapcar
                         (function
                          (lambda (info)
                            (setq ok
                                  (and ok
                                       (<= (abs (- (car (cdr info))
                                                   (window-height
                                                    (car info))))
                                           1)))))
                         wins)))
                    (mapcar
                     (function
                      (lambda (info)
                        (select-window (car info))
                        (if (eq (car info) TargetWindow)
                            (shrink-window
                             (- (window-height TargetWindow) reqd-height)))
                        (set-window-start (car info) (car (cdr (cdr info))))))
                     wins)
                    (mapcar
                     (function
                      (lambda (w)
                        (select-window w)
                        (if (< (1+ (count-lines (point-min) (point-max)))
                               (1- (window-height w)))
                            (shrink-window-if-larger-than-buffer))))
                     shrunkwins)))))
        (select-window OriginalWindow))))

(defconst fume-modeline-buffer-identification
  (if (boundp 'modeline-buffer-identification)
      'modeline-buffer-identification
    'mode-line-buffer-identification))

(defconst fume-use-local-post-command-hook
  (boundp 'local-post-command-hook))

(cond ((fboundp 'add-submenu)
       (defconst fume-add-submenu 'add-submenu)
       (defun fume-munge-menu-args (menu-name submenu before)
         (list fume-menu-path (cons menu-name submenu) before)))
      (t
       (defconst fume-add-submenu 'add-menu)
       (defun fume-munge-menu-args (menu-name submenu before)
         (list fume-menu-path menu-name submenu before))))

(defun fume-add-submenu (menu-name submenu before)
  (apply fume-add-submenu (fume-munge-menu-args menu-name submenu before)))

(defconst fume-not-tty
  (or (and (fboundp 'device-type) (not (eq 'tty (device-type))))
      (and (symbol-value 'window-system) t))) ; obsolete test

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;  Customizable Variables  ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup fume nil
  "Jump to a function within a buffer."
  :tag "Func Menu"
  :group 'tools
  :group 'c
  :group 'lisp)

(defcustom fume-auto-position-popup t
  "*Setting of t (default) means popup the Function menu at the mouse cursor,
nil means at a window corner."
  :type 'boolean
  :group 'fume)

(fume-defvar-local fume-display-in-modeline-p t
  "*Setting of t (default) means display the function name in the modeline,
nil disables.
Non-nil, non-t setting replaces the normal `modeline-buffer-identification'
which is useful when the modeline display is already full.

This is a buffer-local variable.")

(defvar fume-buffer-name "*Function List*"
  "Name of buffer used to list functions when `fume-list-functions' is called.")

(defcustom fume-menubar-menu-name "Functions"
  "*Name of the menubar menu which displays current buffer function definitions."
  :type 'string
  :group 'fume)

;; Bob Weiner <weiner@altrasoft.com>
(defvar fume-menu-path nil
  "Path to the menubar menu under which the function menu should be installed.
Nil (default) means install it on the menubar itself.  Otherwise, this should
be a list of strings, each of which names a successively deeper menu under
which the new menu should be located.")

(defcustom fume-menubar-menu-location "Buffers"
  "*Name of menu before which function menu should appear (default=\"Buffers\").
Nil means display it after the last non-right-justified menubar item."
  :type '(choice (const :tag "Last" nil) (string :format "%v"))
  :group 'fume)

(defcustom fume-max-items 24
  "*Maximum number of elements in a function menu or submenu (default = 24)."
  :type 'integer
  :group 'fume)

(defcustom fume-fn-window-position 3
  "*Number of lines from top of window at which to show a function (default = 3)
If nil, display function start from the centre of the window."
  :type '(choice (const :tag "Center" nil) integer)
  :group 'fume)

(defcustom fume-index-method 3
  "*Set this to the method number you want used.

Methods currently supported:
0 = if you want submenu names to be numbered
1 = if you want submenu range indicated by first character
2 = if you want submenu range indicated by first 12 characters
3 = if you want submenu range indicated by as many characters as needed"
  :type '(radio (const :tag "Numbered" 0)
		(const :tag "Indicated by first character" 1)
		(const :tag "Indicated by first 12 characters" 2)
		(const :tag "Indicated by as many characters as needed" 3))
  :group 'fume)

(defcustom fume-scanning-message "Scanning buffer... (%3d%%)"
  "*Message string format displayed during manual buffer function scans.
Nil means inhibit such messages."
  :type '(choice (const :tag "None" nil) string)
  :group 'fume)

(defcustom fume-rescanning-message nil
  "*Message string format displayed during automatic buffer function scans.
For example, \"Re-Scanning buffer...\".  Nil (default) means inhibit such
messages."
  :type '(choice (const :tag "None" nil) string)
  :group 'fume)

(defvar fume-rescan-trigger-counter-buffer-size 10000
  "Used to tune the frequency of automatic function scans of the buffer
(default = 10000).
The function `fume-rescan-buffer-trigger' executes only when the value of the
variable `fume-rescan-trigger-counter' reaches zero, whereupon it gets reset
to the maximum of a) buffer-size/`fume-rescan-trigger-counter-buffer-size'
               or b) `fume-rescan-trigger-counter-min'.")

(defvar fume-rescan-trigger-counter-min 50
  "Used to tune the frequency of automatic function scans of the buffer
(default = 50).
The function `fume-rescan-buffer-trigger' executes only when the value of the
variable `fume-rescan-trigger-counter' reaches zero, whereupon it gets reset
to the maximum of a) buffer-size/`fume-rescan-trigger-counter-buffer-size'
               or b) `fume-rescan-trigger-counter-min'.")

(fume-defvar-local
 fume-sort-function 'fume-sort-by-name
 "*The function used to sort the function menu entries
(default = fume-sort-by-name).
Nil means display in order of occurrence in the buffer; this is faster since
no sorting is done.

The function should take two arguments and return t if the first
element should appear before the second.  The arguments are cons cells:
\(NAME . POSITION).  See `fume-sort-by-name' for an example.")

(fume-defvar-local
 fume-rescan-buffer-hook nil
 "*Buffer local hook to call at the end of each buffer rescan.")

;; This hook is provided for outl-mouse and must not be made buffer local as
;; this appears to break outl-mouse for some reason.
;;
(defvar fume-found-function-hook nil
  "*Hook to call after every function match.")

;; Idea for jumping directly with a mouse click
;; Marc Paquette <Marc.Paquette@Softimage.COM>
;;
(defvar fume-no-prompt-on-valid-default nil
  "*Non-nil means `fume-prompt-function-goto' should jump to default function
definition without prompting.
Default value is nil.")

;; Idea from Mark Gates <mgates@ncsa.uiuc.edu>
;;
(defvar fume-goto-function-hook nil
  "*Function of with one argument (position)
called if `fume-fn-window-position' is nil or invalid.  Default is nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;  Buffer local variables  ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fume-defvar-local
 fume-auto-rescan-buffer-p t
 "Buffer local variable which if t (default) enables automatic buffer rescanning

Usage:
By default, `fume-auto-rescan-buffer-p' is set to t. If a particular
major-mode slows down too much as a result of automatic rescanning
by func-menu, add something like the following:

   (defun remove-func-menu-auto-rescan ()
      (setq fume-auto-rescan-buffer-p nil))

   (add-hook 'foo-mode-hook 'remove-func-menu-auto-rescan)")

(fume-defvar-local
 fume-funclist nil
 "The latest list of function names in the buffer.")

(fume-defvar-local
 fume-function-marks
 "The latest list of function positions in the buffer.")

(fume-defvar-local
 fume-function-name-regexp nil
 "Regexp which matches names within function definitions.
These names are displayed in the function menu.")

(fume-defvar-local
 fume-find-next-function-name-method nil
 "The function to use to find the next function name in the buffer.")

(fume-defvar-local
 fume-modeline-funclist nil
 "The latest list of function names in the buffer.
If point is within a function definition, a name from this list may be
displayed in the modeline.")

(fume-defvar-local
 fume-funclist-dirty-p nil
 "Flags whether the buffer is in need of a fresh scan.")

(fume-defvar-local
 fume-rescan-inhibit-p nil
 "Internal variable only.  DO NOT TOUCH.")

(fume-defvar-local
 fume-rescan-trigger-counter 0
 "Used in large buffers to optimise automatic function rescanning frequency.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;  Mode specific regexp's and hooks  ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Every fume-function-name-regexp-<language> should uniquely identify a
;; function for that language.

;; Lisp
;;
;; David Hughes <d.hughes@videonetworks.com>
;; Vladimir Alexiev <vladimir@cs.ualberta.ca>
(defvar fume-function-name-regexp-lisp
  (concat
   "^[ \t]*"                            ; Allow whitespace   |(or (fboundp 'foo)
                                        ;  for the construct |    (defun foo ()
   "(\\(def\\(?:un\\*?\\|subst\\*?"	; APA: Make regexp more correct.
   "\\|macro\\*?\\|advice\\|group\\|type\\|struct"
   "\\|class\\|ine-condition\\)\\)"     ; excluding defvar, defconst, but not defgroup
   "\\s-+"                              ; At least one whitespace
   "'?[#:?%@!*/A-Za-z0-9_+<>-]+"        ; Allow (defalias 'foo 'bar)
   "\\s-*"                              ; Whitespace
   "\\(nil\\|(\\)"                      ; nil or (arg list
   )
  "Expression to get Lisp function names.")

;; C
;;
;; Danny Bar-Dov <danny@acet02.amil.co.il>
;; Bob Weiner    <weiner@altrasoft.com> added #define macro support.
(defvar fume-function-name-regexp-c
  (concat
   "^\\([a-zA-Z0-9_]+\\|#define\\)\\s-?"; type specs; there can be no
   "\\([a-zA-Z0-9_*]+\\s-+\\)?"         ; more than 3 tokens, right?
   "\\([a-zA-Z0-9_*]+\\s-+\\)?"
   "\\([*&]+\\s-*\\)?"                  ; pointer
   "\\([a-zA-Z0-9_*]+\\)[ \t\n]*("      ; name
   )
  "Expression to get C function names.")

;; C++
;;
;; Andy Piper      <ajp@eng.cam.ac.uk>
;; Kevin R. Powell <powell@csl.ncsa.uiuc.edu>
;; Mats Lidell     <mats.lidell@eua.ericsson.se>
;; Mike Battaglia  <mbattagl@spd.dsccc.com>
;; Oliver Schittko <schittko@fokus.gmd.de>
;; Tom Murray      <tmurray@hpindck.cup.hp.com>
;; Bob Weiner      <weiner@altrasoft.com> added #define macro support.
;; Didier Verna    <didier@xemacs.org> avoid matches on macro def last line
(defvar fume-function-name-regexp-c++
  (cons
   (concat
    "\\(\\`\\|[^\\]\n\\)" ; avoid matching on last line of macro definition
    "\\(#define\\s-+\\|"
    "\\(template\\s-+<[^>]+>\\s-+\\)?"           ; template formals
    "\\([a-zA-Z0-9_*&<,>:]+\\s-+\\)?"            ; type specs; there can be no
    "\\([a-zA-Z0-9_*&<,>\"]+\\s-+\\)?"           ; more than 3 tokens, right?
    "\\([a-zA-Z0-9_*&<,>]+\\s-+\\)?\\)"
    "\\(\\([a-zA-Z0-9_&~:<,>*]\\|\\(\\s +::\\s +\\)\\)+\\)"
    "\\(o?perator\\s *.[^(]*\\)?\\(\\s-\\|\n\\)*(" ; name
    ) 7)
  "Expression to get C++ function names.")

;; Objective-C
;; Guillaume Laurent <glaurent@worldnet.fr>
(defvar fume-function-name-regexp-objc
  (concat
   "\\("
   "^[a-zA-Z0-9]+\\s-?"                 ; type specs; there can be no
   "\\([a-zA-Z0-9_*]+\\s-+\\)?"         ; more than 3 tokens, right?
   "\\([a-zA-Z0-9_*]+\\s-+\\)?"
   "\\([*&]+\\s-*\\)?"                  ; pointer
   "\\([a-zA-Z0-9_*]+\\)[ \t\n]*("      ; name
   "\\)"
   "\\|"
   "\\(^[-+]\\s-*\\(([a-zA-Z0-9_* \t]+)\\)?\\(\\s-*\\)[a-zA-Z0-9_]+\\)"
   )
  "Expression to get Objective-C function names.")

;; FORTRAN
;;
;; Paul Emsley <paule@chem.gla.ac.uk>
;; Raymond L. Toy <toy@soho.crd.ge.com>
;; Richard Cognot <cognot@elfgrc.co.uk>
;; Greg Sjaardema <gdsjaar@sandia.gov>
;; Bruce Ravel <ravel@phys.washington.edu>
(defvar fume-function-name-regexp-fortran
  (concat
   ;; >= six spaces
   "^      \\s-*"
   ;; type specs
   "[a-zA-Z0-9* ]*\\s-*"
   ;; continuation lines
   "\\(\n     [^ 0]\\s-*\\)*"
   ;; function or subroutine
   "\\(entry\\|ENTRY\\|function\\|FUNCTION\\|subroutine\\|SUBROUTINE\\|block\\s-*data\\|BLOCK\\s-*DATA\\)\\s-*"
   ;; continuation lines
   "\\(\n     [^ 0]\\s-*\\)*"
   )
  "Expression to get Fortran 77 function and subroutine names.")

;; John Turner <turner@xdiv.lanl.gov>
(defvar fume-function-name-regexp-fortran90
  (concat
   ;; Start of big "Or" clause.
   "\\("
   ;; Modules -- must start at beginning of line -- this eliminates
   ;; "module procedure" statements, which are indented.
   "^[Mm][Oo][Dd][Uu][Ll][Ee]\\s-*"
   ;; Or.
   "\\|"
   ;; Derived type definitions. This makes use of my convention to
   ;; never leave a space between "type" and "(" when I actually use
   ;; a derived type.
   "^\\s-*[Tt][Yy][Pp][Ee][^(]\\s-*"
   ;; Or.
   "\\|"
   ;; White space at beginning.
   "^\\s-*"
   ;; Type specs -- not used, because it would also include "end
   ;; function" lines and I don't use type specs before function
   ;; statements anyway. Uncomment this line if you want that behavior.
   ;; "[a-zA-Z0-9*]*"
   ;;
   ;; Entry, function, subroutine, program.
   "\\("
   "[Ee][Nn][Tt][Rr][Yy]\\|"
   "[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]\\|"
   "[Ss][Uu][Bb][Rr][Oo][Uu][Tt][Ii][Nn][Ee]\\|"
   "[Pp][Rr][Oo][Gg][Rr][Aa][Mm]"
   "\\)\\s-*"
   ;; End of big "Or" clause.
   "\\)"
  )
  "Expression to get Fortran 90 module, type, function, and subroutine names.")

;; Modula
(defvar fume-function-name-regexp-modula
  "^\\s-*PROCEDURE\\s-+[A-Za-z0-9_-]+"
  "Expression to get Modula function names.")

;; Maple
;;
;; Luc Tancredi <Luc.Tancredi@sophia.inria.fr>
(defvar fume-function-name-regexp-maple
  "^[ \t]*[a-zA-Z0-9_]+[ \t]*:=[ \t]*proc[ \t]*("
  "Expression to get Maple function/procedure names.")

;; Tcl
;;
;; Andy Piper <ajp@eng.cam.ac.uk>
;; Jean-Michel Augusto <augusto@eureecom.fr>
;; Dr P.G. Sjoerdsma <pgs1002@esc.cam.ac.uk>
(defvar fume-function-name-regexp-tcl
  (cons "^\\s *proc\\s +\\(\\S-+\\)\\s *{" 1)
  "Expression to get Tcl function names.")

;; Java
;;
;; Heddy Boubaker <boubaker@dgac.fr>
(defvar fume-function-name-regexp-java
  "\\s-+\\([A-Za-z_][A-Za-z0-9_]+\\)[\n \t\r]*\\((\\)"
  "Expression to get Java method names.")

;; Perl
;;
;; Alex Rezinsky <alexr@msil.sps.mot.com>
;; Michael Lamoureux <lamour@engin.umich.edu>
;; Remco Wouts <remco@xray.bmc.uu.se>
;; Ville Skyttä <scop@xemacs.org>
(defvar fume-function-name-regexp-perl
  (concat
   "^\\(package\\|sub\\)[ \t]+"     ;; Package or sub?
   "\\(\\([A-Za-z0-9_:]+\\)::\\)?"  ;; Package prefix, like in "sub Foo::bar"
   "\\([A-Za-z0-9_]+\\)"            ;; Function name.
   )
  "Expression to get Perl function names.")

(defcustom fume-perl-fully-qualified-names t
  "*If non-nil, use fully qualified names for Perl functions."
  :type 'boolean
  :group 'fume)

;; Ruby
;; Stefan Kamphausen <mail@skamphausen.de>
;; Albert Davidson Chou <Al_Chou@CyberDude.com>
;; Claus Brunzema <mail@cbrunzema.de>
(defvar fume-function-name-regexp-ruby
  "^\\s-*\\(class\\|def\\)+\\s-*\\([^(\n ]+\\)")

;; Python support
;; Shuichi Koga <skoga@virginia.edu>
;;
(defvar fume-function-name-regexp-python
  "^\\s-*\\(class\\|def\\)+\\s-*\\([A-Za-z0-9_]+\\)\\s-*[(:]"
  "Expression to get Python class and function names.")

(defcustom fume-python-fully-qualified-names t
  "*If non-nil, include class name for python functions."
:type 'boolean
:group 'fume)

;; JavaScript
;;
(defvar fume-function-name-regexp-javascript
   (concat
    "^\\("
    "\\|"
    "\\([^/]\\|/[^/\\*]\\|/\\*\\([^\\*]\\|\\*[^/]\\)*\\*/\\)[ \t;{]"
    "\\)"
    "function[ \t]+"
    "\\([a-zA-Z_\\$][a-zA-Z0-9_\\$]*\\)"
    ;; "[ \t\n]*("
    )
  "Expression to get JavaScript function names.")

;; Postscript
;;
;; Leigh L. Klotz <klotz@adoc.xerox.com>
(defvar fume-function-name-regexp-postscript
  "^/[^][ \t{}<>]*"
  "Expression to get PostScript function names.")

;; Prolog
;;
;; Laszlo Teleki <laszlo@ipb.uni-bonn.de>
(defvar fume-function-name-regexp-prolog
  "^[a-z][a-zA-Z0-9_]+"
  "Expression to get Prolog fact and clause names.")

;; Ehdm
;;
;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defvar fume-function-name-regexp-ehdm
  (concat
   "[A-Za-z0-9_]*:[ ]*"
   "\\([Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]\\|"
   "[Ll][Ee][Mm][Mm][Aa]\\|"
   "[Aa][Xx][Ii][Oo][Mm]\\|"
   "[Pp][Rr][Oo][Vv][Ee]\\|"
   "[Tt][Hh][Ee][Oo][Rr][Ee][Mm]"
   "\\)"
   )
  "*Expression to get Ehdm function, theorems, axioms, lemmas, and proofs.")

;; PVS
;;
;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defvar fume-function-name-regexp-pvs
  (concat
   "\\([A-Za-z0-9_]*:[ ]*"
   "\\([Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]\\|"
   "[Ll][Ee][Mm][Mm][Aa]\\|"
   "[Aa][Xx][Ii][Oo][Mm]\\|"
   "[Tt][Hh][Ee][Oo][Rr][Ee][Mm]\\|"
   "[Ff][Or][Rr][Mm][Uu][La][Aa]"
   "\\|"
   "\\[.*\\]"
   "\\)\\)\\|"
   "[A-Za-z0-9_]*(.*)[ ]*:"
   )
  "*Expression to get PVS functions, theorems, axioms, and lemmas.")

;; Tex, LaTex
;;
;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
;; Paolo Frasconi <paolo@mcculloch.ing.unifi.it>
(fume-defvar-local fume-tex-chapter 0)
(fume-defvar-local fume-tex-section 0)
(fume-defvar-local fume-tex-subsection 0)
(fume-defvar-local fume-tex-subsubsection 0)

(defun fume-tex-rescan-buffer-hook ()
  (setq fume-tex-chapter 0
        fume-tex-section 0
        fume-tex-subsection 0
        fume-tex-subsubsection 0))

(defun fume-tweak-tex-mode ()
  (setq fume-sort-function nil)
  (add-hook 'fume-rescan-buffer-hook 'fume-tex-rescan-buffer-hook))

(add-hook 'tex-mode-hook 'fume-tweak-tex-mode)
;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(add-hook 'TeX-mode-hook 'fume-tweak-tex-mode)
;; Wolfgang Mettbach <wolle@uni-paderborn.de>
(add-hook 'latex-mode-hook 'fume-tweak-tex-mode)
(add-hook 'LaTeX-mode-hook 'fume-tweak-tex-mode)

;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
(defvar fume-section-name-regexp-latex
  (concat
   "^\\s-*\\\\\\("
   "\\(sub\\)*section\\|chapter\\)"
   "\\*?\\(\\[[^]]*\\]\\)?{\\([^}]*\\)}"
   )
  "Expression to get LaTeX section names.")

;; ksh
;;
;; Philippe Bondono <bondono@vnet.ibm.com>
(defvar fume-function-name-regexp-ksh
  (concat
   "\\(^\\s-*function\\s-+[A-Za-z_][A-Za-z_0-9]*\\)"
   "\\|"
   "\\(^\\s-*[A-Za-z_][A-Za-z_0-9]*\\s-*()\\)")
  "Expression to get ksh function names.")

;; Scheme
;;
;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defvar fume-function-name-regexp-scheme
  "^(define[-a-z]* [ ]*"
  "Expression to get Scheme function names.")

;; BibTeX
;;
;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
(defvar fume-function-name-regexp-bibtex
  ;; "^@[A-Za-z]*[({]\\([A-Za-z0-9:;&-]*\\),"
  ;; Christoph Wedler <wedler@fmi.uni-passau.de>
  ;; According to the LaTeX Companion, this should be
  "^@[A-Za-z]*[({]\\([A-Za-z][^ \t\n\"#%'(),={}]*\\),"
  "Expression to get BibTeX citation headers.")

;; SGML
;;
;; Thomas Plass <thomas.plass@mid-heidelberg.de>
(defvar fume-function-name-regexp-sgml
  "<!\\(element\\|entity\\)[ \t\n]+%?[ \t\n]*\\([A-Za-z][-A-Za-z.0-9]*\\)"
  "Expression to find declaration of SGML element or entity.")

;; Ada
;;
;; Mike Konerman <konerman@SSA.crane.navy.mil>
;; Michael Polo <mikep@polo.mn.org> <mikep@cfsmo.honeywell.com>
(defvar fume-function-name-regexp-ada
  (cons "^[ \t]*\\([pP][rR][oO][cC][eE][dD][uU][rR][eE]\\|[fF][uU][nN][cC][tT][iI][oO][nN]\\|[tT][aA][sS][kK][ \t\n]*[bB][oO][dD][yY]\\)[ \n\t]+\\([a-zA-Z0-9_]+\\|\"[^\"]\"\\)" 2)
  "Expression to find declaration of Ada function.")

;; ignore prototypes, `renames', `is new' to eliminate clutter
;;
;; Scott Evans <gse@ocsystems.com>
(defvar fume-function-name-regexp-ada-ignore
  "[ \n\t]*\\(([^()]+)[ \n\t]*\\)?\\(return[ \t\n]+[^ \t\n;]+[ \n\t]*\\)?\\(;\\|is[ \n\t]+new[ \n\t]\\|renames\\)"
  "Ignore if Ada function name matches this string.")

;; Makefiles
;;
;; David Hughes <d.hughes@videonetworks.com>
;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
(defvar fume-function-name-regexp-make
  "^\\(\\(\\$\\s(\\)?\\(\\w\\|\\s_\\|\\.\\)+\\(:sh\\)?\\(\\s)\\)?\\)\\s *\\(::?\\|\\+?=\\)"
  "Expression to get Makefile target names.

Add the following line to your .emacs if you want Makefile support.
Note, this can be very slow for large or non-trivial Makefiles.

(add-hook 'makefile-mode-hook 'fume-add-menubar-entry)")

;; Directory Listings
;;
;; Norbert Kiesel <norbert@i3.informatik.rwth-aachen.de>
;; regexp stolen from font-lock-mode
(defvar fume-function-name-regexp-dired
  "^. +d.*\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\) +[0-9]+ +[0-9:]+ \\(.*\\)$"
  "Expression to get directory names.")

;; Pascal
;;
;; Espen Skoglund <espensk@stud.cs.uit.no>
(defvar fume-function-name-regexp-pascal
  "^\\(function\\|procedure\\)[ \t]+\\([_a-zA-Z][_a-zA-Z0-9]*\\)"
  "Expression to get function/procedure names in Pascal.")

;; Fame
;;
;; Cooper Vertz <cooper@prod2.imsi.com>
(defvar fume-function-name-regexp-fame
  "^\\(function\\|procedure\\)[ \t]+\\([#\\$%_a-zA-Z][#\\$%_a-zA-Z0-9]*\\)"
  "Expression to get function/procedure names in fame.")

;; Verilog
;;
;; Matt Sale <mdsale@icdc.delcoelect.com>
(defvar fume-function-name-regexp-verilog
  "^\\(task\\|function\\|module\\|primitive\\)[ \t]+\\([A-Za-z0-9_+-]*\\)[ \t]*(?"
  "Expression to get Verilog module names.")

;; IDL (Interactive Data Language)
;;
;; Lubos Pochman <lubos@rsinc.com>
;; Carsten Dominik <dominik@astro.uva.nl>
(defvar fume-function-name-regexp-idlwave
  (cons "^\\s *\\([pP][rR][oO]\\|[fF][uU][nN][cC][tT][iI][oO][nN]\\)\\s +\\([A-Za-z][A-Za-z0-9_$]*\\(::[A-Za-z][A-Za-z0-9_$]*\\)?\\)" 2)
  "Expression to get IDL function, procedure and method names.")

;; Assembly
(defvar fume-function-name-regexp-asm
  "^\\([a-zA-Z_.$][a-zA-Z0-9_.$]*\\)[ \t]*:"
  "Expression to get assembly label names.")

;; Manual pages
;;
;; Didier Verna <didier@xemacs.org>
(defvar fume-function-name-regexp-manual "^[A-Z][A-Z ]+$"
  "*Expression to get Manual page sections.")

;; Autoconf 2.5x
;;
;; Andrew W. Nosenko <awn@bcs.zp.ua>
(defvar fume-function-name-regexp-autoconf "^\\(?:AC_DEFUN\\|m4_define\\)(\\[?\\([A-Za-z0-9_()]+\\)"
  "*Expression to get Autoconf macro definitions.")


;; This is where the mode specific regexp's are hooked in
;;
(defvar fume-function-name-regexp-alist
  '(;; Lisp
    (emacs-lisp-mode              . fume-function-name-regexp-lisp)
    (common-lisp-mode             . fume-function-name-regexp-lisp)
    (fi:common-lisp-mode          . fume-function-name-regexp-lisp)
    (fi:emacs-lisp-mode           . fume-function-name-regexp-lisp)
    (fi:franz-lisp-mode           . fume-function-name-regexp-lisp)
    (fi:inferior-common-lisp-mode . fume-function-name-regexp-lisp)
    (fi:inferior-franz-lisp-mode  . fume-function-name-regexp-lisp)
    (fi:lisp-listener-mode        . fume-function-name-regexp-lisp)
    (lisp-mode                    . fume-function-name-regexp-lisp)
    (lisp-interaction-mode        . fume-function-name-regexp-lisp)

    ;; C
    (c-mode      . fume-function-name-regexp-c)
    (elec-c-mode . fume-function-name-regexp-c)

    ;; C++
    (c++-mode   . fume-function-name-regexp-c++)
    (c++-c-mode . fume-function-name-regexp-c++)

    ;; Objective-C
    (objc-mode . fume-function-name-regexp-objc)

    ;; Fortran
    (fortran-mode . fume-function-name-regexp-fortran)
    (f90-mode     . fume-function-name-regexp-fortran90)

    ;; Modula
    (modula-2-mode . fume-function-name-regexp-modula)
    (modula-3-mode . fume-function-name-regexp-modula)

    ;; Maple
    (maple-mode . fume-function-name-regexp-maple)

    ;; Perl
    (perl-mode . fume-function-name-regexp-perl)

    ;; Ruby
    (ruby-mode . fume-function-name-regexp-ruby)

    ;; Java
    (java-mode . fume-function-name-regexp-java)
    (jde-mode  . fume-function-name-regexp-java)

    ;; Python
    (alice-mode  . fume-function-name-regexp-python)
    (python-mode . fume-function-name-regexp-python)

    ;; JavaScript
    (javascript-mode . fume-function-name-regexp-javascript)

    ;; Postscript
    (postscript-mode . fume-function-name-regexp-postscript)

    ;; Prolog
    (prolog-mode . fume-function-name-regexp-prolog)

    ;; Tcl
    (tcl-mode . fume-function-name-regexp-tcl)

    ;; ksh
    (ksh-mode . fume-function-name-regexp-ksh)

    ;; LaTeX
    (latex-mode . fume-section-name-regexp-latex)
    (LaTeX-mode . fume-section-name-regexp-latex)

    ;; Scheme
    (scheme-mode . fume-function-name-regexp-scheme)

    ;; BibTeX
    (bibtex-mode . fume-function-name-regexp-bibtex)

    ;; Ehdm & PVS
    (ehdm-mode . fume-function-name-regexp-ehdm)
    (pvs-mode  . fume-function-name-regexp-pvs)

    ;; SGML
    (sgml-mode . fume-function-name-regexp-sgml)

    ;; Ada
    (ada-mode . fume-function-name-regexp-ada)

    ;; Makefiles
    (makefile-mode . fume-function-name-regexp-make)

    ;; Dired
    (dired-mode . fume-function-name-regexp-dired)

    ;; Pascal
    (pascal-mode . fume-function-name-regexp-pascal)

    ;; Fame
    (fame-mode . fume-function-name-regexp-fame)

    ;; Verilog
    (verilog-mode . fume-function-name-regexp-verilog)

    ;; IDL (Interactive Data Language)
    (idlwave-mode . fume-function-name-regexp-idlwave)

    ;; Assembly
    (asm-mode . fume-function-name-regexp-asm)

    ;; Manual
    (Manual-mode . fume-function-name-regexp-manual)

    ;; Autoconf
    (autoconf-mode . fume-function-name-regexp-autoconf)
    )

  "The connection between a mode and the regexp that matches function names.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;  Mode specific finding functions  ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default routine : Note, most modes will need a specialised routine
;;
(defun fume-find-next-function-name (buffer)
  "Search for the next function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((char (progn
                    (if (string-match
                         "[({[]"
                         (char-to-string (char-after (1- (point)))))
                        (backward-char)
                      (forward-word -1))
                    (save-excursion
                      (goto-char (scan-sexps (point) 1))
                      (skip-chars-forward "[ \t\n]")
                      (following-char)))))
        ;; Skip this function name if it is a prototype declaration.
        (if (and (eq char ?\;) (not (eq major-mode 'emacs-lisp-mode)))
            (fume-find-next-function-name buffer)
          ;; Get the function name and position
          (let (beg)
            (forward-sexp -1)
            (setq beg (point))
            (forward-sexp)
            (cons (buffer-substring beg (point)) beg))))))

;; General purpose sexp find function
;;
(defun fume-find-next-sexp (buffer)
  "Search for the next sexp type function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (save-excursion (forward-sexp -1) (point))))
        (cons (buffer-substring beg (point)) beg))))

;; Specialised routine to get the next Ehdm entity in the buffer.
;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;
(defun fume-find-next-ehdm-entity (buffer)
  "Search for the next Ehdm entity in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (cons (buffer-substring beg end) beg))))

;; Specialised routine to get the next PVS entity in the buffer.
;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;
(defun fume-find-next-pvs-entity (buffer)
  "Search for the next PVS entity in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (goto-char (1- end))
        (if (looking-at ":")
            (setq end (1- end)))
        (cons (buffer-substring beg end) beg))))

;; Specialised routine to get the next C function name in the buffer.
;; Modified 16/12/96: Jerome Bertorelle <bertorel@telspace.alcatel.fr>
;;
(defun fume-find-next-c-function-name (buffer)
  "Search for the next C function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((char (progn
                    (backward-up-list 1)
                    (save-excursion
                      (goto-char (scan-sexps (point) 1))
                      (skip-chars-forward "[ \t\n]")
                      (following-char)))))
        ;; Skip this function name if it is a prototype declaration.
        (if (eq char ?\;)
            (fume-find-next-c-function-name buffer)
          (let (beg
                name)
            ;; Get the function name and position
            (forward-sexp -1)
            (setq beg (point))
            (forward-sexp)
            (setq name (buffer-substring beg (point)))
            ;; ghastly crock for DEFUN declarations
            (cond ((string-match "^DEFUN\\s-*" name)
                   (forward-word 1)
                   (forward-word -1)
                   (setq beg (point))
                   (cond ((re-search-forward "\"," nil t)
                          (re-search-backward "\"," nil t)
                          (setq name
                                (format "%s %s"
                                        name
                                        (buffer-substring beg (point))))))))
            ;; kludge to avoid `void' etc in menu
            (if (string-match
                "\\`\\(void\\|if\\|else if\\|else\\|switch\\|for\\|while\\)\\'"
		name)
                (fume-find-next-c-function-name buffer)
              (cons name beg)))))))

;; Peter Pezaris <pez@dwwc.com>
;;
(defun fume-cc-inside-comment ()
  (memq (buffer-syntactic-context) '(comment block-comment)))

;; <jrm@odi.com>
;; <ajp@eng.cam.ac.uk>
;; <djh@videonetworks.com>
;; <schittko@fokus.gmd.de>
;;
(defun fume-match-find-next-function-name (buffer)
  "General next function name in BUFFER finder using match.
The regexp is assumed to be a two item list the car of which is the regexp
to use, and the cdr of which is the match position of the function name."
  (set-buffer buffer)
  (let ((r (car fume-function-name-regexp))
        (p (cdr fume-function-name-regexp)))
    (catch 'found
      (while (re-search-forward r nil t)
        (catch 'skip
          ;; Reject comments
          (if (fume-cc-inside-comment) (throw 'skip t))
          ;; Reject typedef's
          (save-excursion
            (re-search-backward r nil t)
            (if (string= "typedef" (fume-what-looking-at)) (throw 'skip t))
            (re-search-forward r nil t))
          (backward-up-list 1)
          ;; Reject class member inits
          (let ((c (save-excursion (back-to-indentation) (char-after (point)))))
            (if (or (char-equal c ?\,) (char-equal c ?\:)) (throw 'skip t)))
          ;; Reject prototypes
          (save-excursion
            (goto-char (scan-sexps (point) 1))
            (skip-chars-forward "[ \t()]*")
            (if (char-equal (following-char) ?\;) (throw 'skip t)))
          (throw
           'found
           (cons (buffer-substring (setq p (match-beginning p)) (point)) p))))
      nil)))

;; Objective-C
;; Guillaume Laurent <glaurent@worldnet.fr>
(defun fume-find-next-objc-function-or-method-name (buffer)
  "Search for the next Objective-C function or method in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (if (string-match "^[-+]\\s-*" (match-string 0)) ; this is a method decl.
          (let (beg name)
            (backward-sexp)
            (setq beg (point))
            (forward-sexp)
            (if (char-equal (following-char) ?\:) (forward-char))
            (setq name (buffer-substring beg (point)))
            (cons name beg))
        (let ((char (progn ; else this is a regular C func
                      (backward-up-list 1)
                      (save-excursion
                        (goto-char (scan-sexps (point) 1))
                        (skip-chars-forward "[ \t\n]")
                        (following-char)))))
          ;; Skip this function name if it is a prototype declaration.
          (if (eq char ?\;)
              (fume-find-next-function-name buffer)
            (let (beg
                  name)
              ;; Get the function name and position
              (forward-sexp -1)
              (setq beg (point))
              (forward-sexp)
              (setq name (buffer-substring beg (point)))
              ;; kludge to avoid `void' in menu
              (if (string-match "^void\\s-*" name)
                  (fume-find-next-function-name buffer)
                (cons name beg))))))))

;; Specialised routine to find the next Perl function
;;
;; This gets package names wrong in some cases, eg:
;;
;;   package Foo;
;;   {
;;   package Bar;
;;   sub baz {}
;;   }
;;   sub quux {}  # This is Foo::quux, not Bar::quux !
;;
(defun fume-find-next-perl-function-name (buffer)
  "Search for the next Perl function in BUFFER.
See also `fume-perl-fully-qualified-names'."
  (set-buffer buffer)
  (when (not (boundp 'fume-perl-current-package))
    (make-variable-buffer-local 'fume-perl-current-package)
    (setq fume-perl-current-package nil))
  (if (re-search-forward fume-function-name-regexp nil t)
      (save-excursion
	(let ((retpnt (match-beginning 1))
	      (type (match-string 1))
	      (pkg-with-colons (match-string 2))
	      (pkg (match-string 3))
	      (name (match-string 4)))
	  (cond ((string= type "package")
		 (setq name (concat (or pkg-with-colons "") name))
		 (setq fume-perl-current-package name))
		((string= type "sub")
		 (when fume-perl-fully-qualified-names
		   (setq name
			 (format "%s::%s"
				 (or pkg fume-perl-current-package "main")
				 name)))
		 (cons name retpnt)))))
    ;; Reset current package when no more functions found.
    (setq fume-perl-current-package nil)))

;; Specialised routine to find the next Ruby function
;;
(defun fume-find-next-ruby-function-name (buffer)
  "Search for the next Ruby function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (save-excursion
        (let* ((retpnt (match-beginning 2))
               (retname (buffer-substring retpnt (match-end 2))))
          (goto-char (match-beginning 0))
          (cond ((looking-at "^\\s-*def")
                 (re-search-backward
                  "\n?\\s-*class\\s-*\\([A-Z][A-Za-z0-9_]*\\)\\s-*" nil t)
		 (let* ((classname (buffer-substring
				    (match-beginning 1) (match-end 1))))
		   (if (not (string-match (concat "^" classname "\\.") retname))
		       (setq retname (concat classname "." retname))))))
	  (cons retname retpnt)))))

;; Specialised routine to find the next Java function
;; Bob Weiner <weiner@altrasoft.com>
;; Heddy Boubaker <boubaker@dgac.fr>
;;
(defun fume-find-next-java-function-name (buffer)
  "Search for the next Java function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 1))
            (end (match-end       1)))
        (goto-char (match-beginning 2))
        (forward-sexp)
        (if (and (looking-at "[^;(]*{")
                 (not (fume-cc-inside-comment)))
            ;; This is a method definition and we're not in a comment
            (let ((str (buffer-substring beg end)))
              ;; Bob Weiner <weiner@altrasoft.com> added exact match
              ;; delimiters so function names that happen to contain
              ;; any of these terms are not eliminated.  The old version
              ;; would ignore "notify()" since it contained "if".
              (or
               (string-match
                "\\`\\(if\\|else if\\|else\\|switch\\|catch\\|for\\|while\\|synchronized\\)\\'"
                str)
               ;; These constructs look like method definitions but are not
               (cons str beg)))
          (fume-find-next-java-function-name buffer)))))

;; Specialised routine to find the next Python function
;; Shuichi Koga <skoga@virginia.edu>
;;
(defun fume-find-next-python-function-name (buffer)
  "Search for the next Python function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (save-excursion
        (let* ((retpnt (match-beginning 2))
               (retname (buffer-substring retpnt (match-end 2))))
	  (when fume-python-fully-qualified-names
	    (goto-char (match-beginning 0))
	    (cond ((looking-at "\\s-+def")
		   (re-search-backward
		    "^class\\s-*\\([A-Za-z0-9_]+\\)\\s-*[(:]" nil t)
		   (setq retname
			 (concat
			  (buffer-substring (match-beginning 1) (match-end 1))
			  "."
			  retname)))))
	    (cons retname retpnt)))))

;; Specialised routine to find the next JavaScript function
;;
(defun fume-find-next-javascript-function-name (buffer)
  "Search for the next JavaScript function in BUFFER."
  (fume-find-next-sexp buffer))

;; Specialised routine to find the next Modula function or subroutine.
;;
(defun fume-find-next-modula-function-name (buffer)
  "Search for the next Modula function in BUFFER."
  (fume-find-next-sexp buffer))

;; Specialised routine to find the next directory.
;; Norbert Kiesel <norbert@i3.informatik.rwth-aachen.de>
;;
(defun fume-find-next-directory-name (buffer)
  "Search for the next directory in dired BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        (cons (buffer-substring beg end) beg))))

;; Specialised routine to find the next Fortran function or subroutine
;;
(defun fume-find-next-fortran-function-name (buffer)
  "Search for the next Fortran function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((pos (point))
            ;; name may have "_" but must start with a letter
            (name-regexp "\\s-+[a-zA-Z]+[_a-zA-Z0-9* ]*")
            (eol (save-excursion (end-of-line 1) (point))))
        (skip-chars-backward " \t")
        (if (re-search-forward name-regexp eol t)
            ;; name is ok; so return it
            (cons (buffer-substring pos (point)) pos)
          ;; rubbish found; skip to next function
          (fume-find-next-fortran-function-name buffer)))))

;; Specialised routine to get the next postscript function name in the buffer
;; Leigh L. Klotz <klotz@adoc.xerox.com>
;;
(defun fume-find-next-postscript-function-name (buffer)
  "Search for the next PostScript function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (progn (beginning-of-line 1) (point))))
        (forward-sexp)
        ;; keep including sexps as long as they
        ;; start with / or [.
        (if (looking-at "\\s-+\\(/\\|\\[\\)")
            (forward-sexp))
        (cons (buffer-substring beg (point)) beg))))

;; Specialised routine to get the next prolog fact/clause name in the buffer
;; Laszlo Teleki <laszlo@ipb.uni-bonn.de>
;;
(defun fume-find-next-prolog-function-name (buffer)
  "Search for the next Prolog fact or clause in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (progn (beginning-of-line 1) (point))))
        (forward-sexp)
        (cons (buffer-substring beg (point)) beg))))

;; Specialized routine to get the next Maple function name in the buffer
;; Luc Tancredi <Luc.Tancredi@sophia.inria.fr>
;;
(defun fume-find-next-maple-function-name (buffer)
  "Search for the next Maple function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (progn (backward-up-list 1) (forward-sexp -2) (point))))
        (forward-sexp)
        (cons (buffer-substring beg (point)) beg))))

;; Specialised routine to get the next latex section name in the buffer
;; Philippe Queinnec <queinnec@cenatls.cena.dgac.fr>
;; Paolo Frasconi <paolo@mcculloch.ing.unifi.it>
;; Anthony Rossini <arossini@biostat.washington.edu>
;;
(defun fume-find-next-latex-section-name (buffer)
  "Search for the next LaTeX section in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let* ((secname (buffer-substring (match-beginning 1) (match-end 1)))
             (beg (match-beginning 4))
             (name (buffer-substring beg (match-end 4))))
        (cond ((string= secname "chapter")
               (setq fume-tex-chapter (1+ fume-tex-chapter)
                     fume-tex-section 0
                     fume-tex-subsection 0
                     fume-tex-subsubsection 0
                     name (concat (int-to-string fume-tex-chapter)
                                  " " (upcase name))))
              ((string= secname "section")
               (setq fume-tex-section (1+ fume-tex-section)
                     name (concat
                           (if (> fume-tex-chapter 0)
                               (concat (int-to-string fume-tex-chapter) ".") "")
                           (int-to-string fume-tex-section) " " name)
                     fume-tex-subsection 0
                     fume-tex-subsubsection 0))
              ((string= secname "subsection")
               (setq fume-tex-subsection (1+ fume-tex-subsection)
                     name (concat
                           (if (> fume-tex-chapter 0)
                               (concat (int-to-string fume-tex-chapter) ".") "")
                           (int-to-string fume-tex-section) "."
                           (int-to-string fume-tex-subsection) " " name)
                     fume-tex-subsubsection 0))
              ((string= secname "subsubsection")
               (setq fume-tex-subsubsection (1+ fume-tex-subsubsection)
                     name (concat
                           (if (> fume-tex-chapter 0)
                               (concat (int-to-string fume-tex-chapter) ".") "")
                           (int-to-string fume-tex-section) "."
                           (int-to-string fume-tex-subsection) "."
                           (int-to-string fume-tex-subsubsection) " " name)))
              ((string= secname "subsubsection")
               (setq name (concat "   " name))))
        (cons name beg))))

;; Specialised routine to get the next ksh function in the buffer
;; Philippe Bondono <bondono@vnet.ibm.com>
;;
(defun fume-find-next-ksh-function-name (buffer)
  "Search for the ksh type function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (let (name
            (beg (match-beginning 0)))
        (cond ((re-search-backward "\\(^\\|\\s-\\)function\\s-" beg t)
               (re-search-forward
                "\\(function\\s-+\\)\\([A-Za-z_][A-Za-z_0-9]*\\)" nil t)
               (setq beg (match-beginning 2)
                     name (buffer-substring beg (match-end 2))))
              (t
               (re-search-backward
                "\\(^\\|\\s-\\)\\([A-Za-z_][A-Za-z_0-9]*\\)" beg t)
               (setq beg (match-beginning 2)
                     name (buffer-substring beg (match-end 2)))))
        (if (null name)
            (fume-find-next-ksh-function-name buffer)
          (end-of-line)
          (cons name beg)))))

;; Specialised routine to get the next Scheme function in the buffer
;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;
(defun fume-find-next-scheme-function (buffer)
  "Search for the next Scheme function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (progn (if (looking-at "(") (forward-char 1)) (point)))
            (end (save-excursion (forward-sexp) (point))))
        (cons (buffer-substring beg end) beg))))

;; Specialised routine to get the next BibTeX citation in the buffer
;; C. Michael Holloway <c.m.holloway@larc.nasa.gov>
;;
(defun fume-find-next-bibtex-citation (buffer)
  "Search for the next BibTeX citation in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        (cons (buffer-substring beg end) beg))))

;; Specialised routine to get the next SGML declaration in the buffer
;; Thomas Plass <thomas.plass@mid-heidelberg.de>
;;
(defun fume-find-next-sgml-element-name (buffer)
  "Search for the next SGML declaration in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((type (buffer-substring (match-beginning 1) (match-end 1)))
            (beg (match-beginning 2))
            (name (buffer-substring (match-beginning 2) (match-end 2))))
        (if (string= (downcase type) "element")
            (setq name (format "%-17s%3s" name "EL"))
          (setq name (format "%-17s%3s" name "ENT")))
        (cons name beg))))

;; Specialised routine to get the next ada function in the buffer
;; Michael Polo <mikep@polo.mn.org> <mikep@cfsmo.honeywell.com>
;;
(defun fume-find-next-ada-function-name (buffer)
  "Search for the next Ada function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward (car fume-function-name-regexp-ada) nil t)
      (let ((beg (match-beginning (cdr fume-function-name-regexp-ada)))
            (end (match-end (cdr fume-function-name-regexp-ada))))

        (if (looking-at fume-function-name-regexp-ada-ignore)
            (fume-find-next-ada-function-name buffer)
          (cons (buffer-substring beg end) beg)))))

;; Makefiles
;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
;;
(defun fume-find-next-function-name-make (buffer)
  "Search for the next make item in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        (cons (buffer-substring beg end) beg))))

;; Find next pascal function in the buffer
;; Espen Skoglund <espensk@stud.cs.uit.no>
;;
(defun fume-find-next-pascal-function-name (buffer)
  "Search for the next Pascal procedure in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        (cons (buffer-substring beg end) beg))))

;; Verilog support
;; Matt Sale <mdsale@icdc.delcoelect.com>
;;
(defun fume-find-next-verilog-function-name (buffer)
  "Search for the next Verilog module in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 2))
            (end (match-end 2)))
        (cons (buffer-substring beg end) beg))))

;; Specialised routine to get the next IDL function in the buffer
;;
;; Lubos Pochman <lubos@rsinc.com>
;; Carsten Dominik <dominik@astro.uva.nl>
(defun fume-find-next-idlwave-function-name (buffer)
  "Search for the next IDL function in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward (car fume-function-name-regexp-idlwave) nil t)
      (let ((beg (match-beginning (cdr fume-function-name-regexp-idlwave)))
            (end (match-end (cdr fume-function-name-regexp-idlwave))))
        (cons (buffer-substring beg end) beg))))

;; Assembly
;; Bob Weiner <weiner@altrasoft.com>
;;
(defun fume-find-next-asm-function-name (buffer)
  "Search for the next assembler function in BUFFER."
  (set-buffer buffer)
  ;; Search for the function
  (if (re-search-forward fume-function-name-regexp nil t)
      (cons (buffer-substring (match-beginning 1) (match-end 1))
            (match-beginning 1))))

;; Specialised routine to get the next manual section name in the buffer
;; Didier Verna <didier@xemacs.org>
;;
(defun fume-find-next-manual-section (buffer)
  "Search for the next manual section name in BUFFER."
  (set-buffer buffer)
  (if (re-search-forward fume-function-name-regexp nil t)
      (let ((beg (match-beginning 0))
	    (end (match-end 0)))
	(cons (buffer-substring beg end) beg))))

;; This is where you can hook in other languages which may need a different
;; method to scan for function names. Otherwise, the default defun used is
;; fume-find-next-function-name which is suitable for sexp-based languages
;; such as C, C++ and elisp.
;;
(defconst fume-find-function-name-method-alist
  '((ada-mode        . fume-find-next-ada-function-name)
    (alice-mode      . fume-find-next-python-function-name)
    (asm-mode        . fume-find-next-asm-function-name)
    (bibtex-mode     . fume-find-next-bibtex-citation)
    (c++-mode        . fume-match-find-next-function-name)
    (c++-c-mode      . fume-match-find-next-function-name)
    (c-mode          . fume-find-next-c-function-name)
    (objc-mode       . fume-find-next-objc-function-or-method-name)
    (dired-mode      . fume-find-next-directory-name)
    (ehdm-mode       . fume-find-next-ehdm-entity)
    (fame-mode       . fume-find-next-pascal-function-name)
    (fortran-mode    . fume-find-next-fortran-function-name)
    (f90-mode        . fume-find-next-fortran-function-name)
    (ksh-mode        . fume-find-next-ksh-function-name)
    (latex-mode      . fume-find-next-latex-section-name)
    (LaTeX-mode      . fume-find-next-latex-section-name)
    (makefile-mode   . fume-find-next-function-name-make)
    (maple-mode      . fume-find-next-maple-function-name)
    (modula-2-mode   . fume-find-next-modula-function-name)
    (modula-3-mode   . fume-find-next-modula-function-name)
    (pascal-mode     . fume-find-next-pascal-function-name)
    (perl-mode       . fume-find-next-perl-function-name)
    (ruby-mode       . fume-find-next-ruby-function-name)
    (java-mode       . fume-find-next-java-function-name)
    (jde-mode        . fume-find-next-java-function-name)
    (postscript-mode . fume-find-next-postscript-function-name)
    (prolog-mode .     fume-find-next-prolog-function-name)
    (pvs-mode        . fume-find-next-pvs-entity)
    (python-mode     . fume-find-next-python-function-name)
    (javascript-mode . fume-find-next-javascript-function-name)
    (scheme-mode     . fume-find-next-scheme-function)
    (sgml-mode       . fume-find-next-sgml-element-name)
    (tcl-mode        . fume-match-find-next-function-name)
    (verilog-mode    . fume-find-next-verilog-function-name)
    (idlwave-mode    . fume-find-next-idlwave-function-name)
    (Manual-mode     . fume-find-next-manual-section)
    )

  "The connection between a mode and the defun that finds function names.
If no connection is in this alist for a given mode, a default method is used")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;  General utility functions  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; modeline refresh routine
;;
(or (fboundp 'redraw-modeline)
    (defun redraw-modeline () (set-buffer-modified-p (buffer-modified-p))))

;; Smart mouse positioning
;;
(if (fboundp 'window-edges)             ; old method
    (defun fume-set-mouse-position ()
      (set-mouse-position
       (selected-frame)
       (nth 0 (window-edges)) (nth 1 (window-edges))))
  (defun fume-set-mouse-position ()     ; new method
    (set-mouse-position
     (selected-window)
     (nth 0 (window-pixel-edges))
     (nth 1 (window-pixel-edges)))))

;; Sets `fume-function-name-regexp' to something appropriate for the current
;; mode for this buffer.
;;
(defun fume-set-defaults ()
  "Return nil if unsuccessful in setting up buffer-local defaults.
Otherwise returns fume-function-name-regexp"
  (setq fume-function-name-regexp
        (symbol-value
         (cdr-safe (assoc major-mode fume-function-name-regexp-alist))))
  (if fume-function-name-regexp
      (setq fume-find-next-function-name-method
            (or (cdr-safe (assoc major-mode
                                 fume-find-function-name-method-alist))
                'fume-find-next-function-name)))
  fume-function-name-regexp)

(defun fume-add-menubar-entry (&optional force)
  (interactive)
  (if force
      (save-window-excursion (function-menu t))
    (enqueue-eval-event 'fume-do-add-menubar-entry (current-buffer))))

(defun fume-do-add-menubar-entry (buffer)
  (and (buffer-live-p buffer)
       (save-excursion
	 (save-window-excursion
	   (set-buffer buffer)
	   (function-menu t)))))

(defun fume-remove-menubar-entry ()
  (interactive)
  (cond ((and fume-running-xemacs current-menubar)
         (delete-menu-item (list fume-menubar-menu-name))
         ;; force update of the menubar
         (redraw-modeline))))

(defun fume-update-menubar-entry ()
  "Return t if menubar was updated, nil otherwise."
  (and fume-running-xemacs
       fume-not-tty
       (assoc fume-menubar-menu-name current-menubar)
       (save-window-excursion (function-menu t))
       t))

(defun fume-replace-str (s r b e) (concat (substring s 0 b) r (substring s e)))

(defun fume-substitute (s old new &optional all)
  (let (case-fold-search b (l (length new)))
    (while (and (string-match old s b)
                (setq b (+ (match-beginning 0) l)
                      s (fume-replace-str s new (- b l) (match-end 0)))
                all))
    s))

(defun fume-trim-string (s &optional c)
  (setq c (or c " \t") s (fume-substitute s (format "^[%s]*" c) ""))
  (fume-substitute s (format "[%s]*$" c) ""))

(defvar fume-syntax-table nil)

(defun fume-what-looking-at (&optional check-primary-selection-p)
  (or (and check-primary-selection-p
           primary-selection-extent
           (fume-bomb-proof
            (prog1 (buffer-substring (region-beginning) (region-end))
              (and zmacs-regions (zmacs-deactivate-region) (sit-for 0)))))
      (let (name
            (orig-syntax-table (syntax-table)))
        (if fume-syntax-table
            ()
          (setq fume-syntax-table (make-syntax-table))
          (modify-syntax-entry ?: "w" fume-syntax-table))
        (unwind-protect
            (progn
              (set-syntax-table fume-syntax-table)
              (save-excursion
                (while (looking-at "\\sw\\|\\s_") (forward-char 1))
                (if (re-search-backward "\\sw\\|\\s_" nil t)
                    (let ((beg (progn (forward-char 1) (point))))
                      (forward-sexp -1)
                      (while (looking-at "\\s'") (forward-char 1))
                      (setq name (buffer-substring beg (point)))))))
          (set-syntax-table orig-syntax-table)
          name))))

(defun fume-function-before-point ()
  "Find function name that point is in.
The trick is to start from the end."
  (if (or fume-modeline-funclist (fume-rescan-buffer) fume-modeline-funclist)
      (let ((p (point)))
        (save-excursion
          (catch 'found
            (mapcar (function
                     (lambda (x)
                       (goto-char (cdr x))
                       (beginning-of-line 1)
                       (if (>= p (point)) (throw 'found (car x)))))
                    fume-modeline-funclist) nil)))))

;; Routines to add a buffer local post command hook
;;
(defun fume-post-command-hook-p (hook)
  (memq hook (if fume-use-local-post-command-hook
                 local-post-command-hook
               post-command-hook)))

(defun fume-add-post-command-hook (hook &optional append)
  (or (fume-post-command-hook-p hook)
      (cond (fume-use-local-post-command-hook
             (add-hook 'local-post-command-hook hook append))
            ((fboundp 'make-local-hook)
             (make-local-hook 'post-command-hook)
             (add-hook 'post-command-hook hook append t))
            (t
             ;; NOT make-variable-buffer-local
             (make-local-variable 'post-command-hook)
             (add-hook 'post-command-hook hook append)))))

(defun fume-remove-post-command-hook (hook)
  (and (fume-post-command-hook-p hook)
       (cond (fume-use-local-post-command-hook
              (remove-hook 'local-post-command-hook hook))
             ((fboundp 'make-local-hook)
              (remove-hook 'post-command-hook hook t))
             (t
              (remove-hook 'post-command-hook hook)))))

;; Routine to install the modeline feature
;;
(defun fume-maybe-install-modeline-feature ()
  (cond ((and fume-display-in-modeline-p (fume-set-defaults))
         (or fume-modeline-funclist
             (fume-post-command-hook-p 'fume-tickle-modeline)
             (fume-rescan-buffer))
         (fume-add-post-command-hook 'fume-tickle-modeline)
         (fume-remove-post-command-hook 'fume-maybe-install-modeline-feature)
         (fume-tickle-modeline-1)
         (fume-tickle-modeline)
         t                              ; return success flag
         )))

(defun fume-toggle-modeline-display ()
  "Toggle whether func-menu displays function names in the modeline."
  (interactive)
  (setq fume-display-in-modeline-p (not fume-display-in-modeline-p))
  (if (interactive-p) (fume-tickle-modeline)))

(defun fume-tickle-modeline ()
  "Routine to display function before point in the modeline."
  (let ((fname (and fume-display-in-modeline-p (fume-function-before-point))))
    (set fume-modeline-buffer-identification
         (if (not (and fume-display-in-modeline-p fname))
             fume-modeline-buffer-identification-0
           (setq fname (replace-in-string fname "%" "%%")
                 fname (format "'%s'" (fume-trim-string fname)))
           (if (eq fume-display-in-modeline-p t)
               (list fume-modeline-buffer-identification-1 " " fname)
             fname))))
  (cond ((not fume-display-in-modeline-p)
         (fume-remove-post-command-hook 'fume-tickle-modeline)
         (fume-add-post-command-hook 'fume-maybe-install-modeline-feature)))
  ;; force update of the modeline
  (redraw-modeline))

(fume-defvar-local fume-modeline-buffer-identification-0 nil
  "Storage for original modeline-buffer-identification")

(fume-defvar-local fume-modeline-buffer-identification-1 nil
  "Storage for munged modeline-buffer-identification")

(defun fume-tickle-f-to-b (str)
  "Change modeline format of \"XEmacs: %f\" to \"XEmacs: %b\" in order to make
extra room for the function name which is going to be appended to the
`modeline-buffer-identification' component of `modeline-format'."
  (cond ((consp str)
         (if (extentp (car str))
             (cons (car str) (fume-tickle-f-to-b (cdr str)))
           (mapcar (function fume-tickle-f-to-b) str)))
        ((not (stringp str)) str)
        ((string-match "%[0-9]*f" str)
         (let ((newstr (copy-sequence str)))
           (aset newstr (1- (match-end 0)) (string-to-char "b"))
           newstr))
        (t str)))

(defun fume-tickle-modeline-1 ()
  (or fume-modeline-buffer-identification-0
      (setq fume-modeline-buffer-identification-0
            (symbol-value fume-modeline-buffer-identification)))
  (setq fume-modeline-buffer-identification-1
        (fume-tickle-f-to-b fume-modeline-buffer-identification-0)))

(defun fume-toggle-auto-rescanning ()
  "Toggle automatic rescanning of the buffer."
  (interactive)
  (message "Func-Menu buffer auto-rescanning turned %s"
           (if (setq fume-auto-rescan-buffer-p (not fume-auto-rescan-buffer-p))
               "ON" "OFF"))
  (sit-for 0))

(if (fboundp 'copy-tree)                ; not built-in in all emacsen
    (defalias 'fume-shallow-copy-list 'copy-tree)
  (defun fume-shallow-copy-list (list)
    "Create a shallow separate copy of a list."
    (mapcar (function (lambda (i) (cons (car i) (cdr i)))) list)))

(defun fume-sort-by-name (item1 item2)
  "Sort function to sort items depending on their function name.
An item looks like (NAME . POSITION)."
  (or (string-lessp (car item1) (car item2))
      (string-equal (car item1) (car item2))))

(defun fume-sort-by-position (item1 item2)
  "Sort function to sort items depending on their position."
  (<= (cdr item1) (cdr item2)))

(defun fume-relative-position ()
  "Calculate relative position in buffer."
  (let ((pos (point))
        (total (buffer-size)))
    (if (> total 50000)
        ;; Avoid overflow from multiplying by 100!
        (/ (1- pos) (max (/ total 100) 1))
      (/ (* 100 (1- pos))
         (max total 1)))))

;; Example (fume-split '(1 2 3 4 5 6 7 8) 3)-> '((1 2 3) (4 5 6) (7 8))
;;
(defun fume-split (list n)
  "Split LIST into sublists of max length N."
  (let ((i 0)
        result
        sublist
        (remain list))
    (while remain
      (if (= n (setq sublist (cons (car remain) sublist)
                     remain (cdr remain)
                     i (1+ i)))
          ;; We have finished a sublist
          (setq result (cons (nreverse sublist) result)
                sublist nil
                i 0)))
    ;; There might be a sublist (if the length of LIST mod n is != 0)
    ;; that has to be added to the result list.
    (if sublist
        (setq result (cons (nreverse sublist) result)))
    (nreverse result)))

;; Routines to create indexes for submenus
;;

;; Method 0
;;
(defun fume-index-sublist-method-0 (sublist count)
  (concat "Function sublist #" count))

;; Method 1
;; Thomas Plass <thomas.plass@mid-heidelberg.de>
;;
(defun fume-index-sublist-method-1 (sublist &rest count)
  (interactive)
  (let ((s (substring (car (car sublist)) 0 1))
        (e (substring (car (nth (1- (length sublist)) sublist)) 0 1)))
    (format "Function sublist (%s%s)"
            s (if (string-equal s e) "<>" (format "<>-%s<>" e)))))

;; Method 2
;; Paul Filipski & Anthony Girardin <{filipski,girardin}@blackhawk.com>
;;
(defun fume-index-sublist-method-2 (sublist &rest count)
  (let ((s (substring (car (car sublist))
                      0
                      (min (length (car (car sublist))) 12)))
        (e (substring (car (nth (1- (length sublist)) sublist))
                      0
                      (min (length (car (nth (1- (length sublist)) sublist)))
                           12))))
    (format "%s%s" s (if (string-equal s e) "<>" (format "<> ... %s<>" e)))))

;; Method 3
;;
(defun fume-index-sublist-method-3-1 (sublist ix limit)
  (let ((s1 (substring (car (car sublist)) 0 (min limit ix)))
        (s2 (substring
             (car (nth (1- (length sublist)) sublist))
             0 (min (length (car (nth (1- (length sublist)) sublist))) ix))))
    (cons s1 s2)))

(defun fume-index-sublist-method-3 (sublist &rest count)
  (let* ((cmplength 12)
         (limit (length (car (car sublist))))
         (result (fume-index-sublist-method-3-1 sublist cmplength limit))
         (str1 (car result))
         (str2 (cdr result)))
    (while (and (string-equal str1 str2) (< cmplength limit))
      (setq cmplength (1+ cmplength)
            result (fume-index-sublist-method-3-1 sublist cmplength limit)
            str1 (car result)
            str2 (cdr result)))
    (cond ((not (string-equal str1 str2))
           (format "%s<> ... %s<>" str1 str2))
          ((< cmplength limit)
           (format "%s<>" str1))
          (t
           (format "%s ..." str1)))))

;; Buffer rescanning
;;
(defun fume-rescan-buffer-trigger ()
  "Automatically spots when a buffer rescan becomes necessary."
  (if fume-auto-rescan-buffer-p
      (if (> fume-rescan-trigger-counter 0)
          (setq fume-rescan-trigger-counter (1- fume-rescan-trigger-counter))
        (setq fume-rescan-trigger-counter
              (max fume-rescan-trigger-counter-min
                   (/ (buffer-size) fume-rescan-trigger-counter-buffer-size)))
        (if (or fume-funclist-dirty-p
                (save-excursion
                  (let (find fnam)
                    (fume-bomb-proof
                     (and fume-function-name-regexp
                          (setq fnam (fume-function-before-point))
                          (setq find (symbol-value
                                      'fume-find-next-function-name-method))
                          (progn (end-of-line 1)
                                 (re-search-backward
                                  fume-function-name-regexp nil t))
                          (if (eq find 'fume-find-next-latex-section-name)
                              (let ((lnam
                                     (car (fume-find-next-latex-section-name
                                           (current-buffer)))))
                                (fume-tex-rescan-buffer-hook)
                                (not (string-equal
                                      (substring fnam
                                                 (string-match " " fnam))
                                      (substring lnam
                                                 (string-match " " lnam)))))
                            (not (string-equal
                                  fnam
                                  (car (funcall find (current-buffer)))))))))))
            (let ((fume-scanning-message nil))
              (fume-rescan-buffer))))))

(defun fume-install-rescan-buffer-trigger ()
  (cond ((not (fume-post-command-hook-p 'fume-rescan-buffer-trigger))
         (fume-add-post-command-hook 'fume-rescan-buffer-trigger 'append)
         ;; Make narrow-to-region tickle func-menu
         (or (fboundp 'fume-narrow-to-region)
             (fset 'fume-narrow-to-region
                   (symbol-function 'narrow-to-region)))
         (defun narrow-to-region (b e)
           "Restrict editing in this buffer to the current region.
The rest of the text becomes temporarily invisible and untouchable
but is not deleted; if you save the buffer in a file, the invisible
text is included in the file.  C-x n w makes all visible again.
See also `save-restriction'.

When calling from a program, pass two arguments; positions (integers
or markers) bounding the text that should remain visible"
           (interactive "r")
           (fume-narrow-to-region b e)
           (if fume-funclist (setq fume-funclist-dirty-p t)))
         ;; Make widen tickle func-menu
         (or (fboundp 'fume-widen)
             (fset 'fume-widen (symbol-function 'widen)))
         (defun widen ()
           "Remove restrictions (narrowing) from current buffer.
This allows the buffer's full text to be seen and edited."
           (interactive)
           (fume-widen)
           (if fume-funclist
               (setq fume-funclist-dirty-p t
                     fume-rescan-trigger-counter 0))))))

(defun fume-rescan-buffer (&optional popmenu)
  "Rescans the buffer for function names.
If optional arg POPMENU is non-nil, brings up the `function-menu'."
  (interactive)
  (let ((fnam)
        (flst '())
        (buffer-to-scan (current-buffer))
        (find (symbol-value 'fume-find-next-function-name-method)))
    (save-excursion
      (goto-char (point-min))
      (cond (fume-scanning-message
             (display-message 'progress (format fume-scanning-message 0)))
            (fume-rescanning-message
             (display-message 'progress fume-rescanning-message)))
      (while (setq fnam
                   (condition-case ()
                       (funcall find buffer-to-scan)
                     (error
                      ;; test for more possible fns after this error trap
                      (if (consp fume-function-name-regexp)
                          (save-excursion
                            (re-search-forward
                             (car fume-function-name-regexp) nil t))
                        (and fume-function-name-regexp
                             (save-excursion
                               (re-search-forward
                                fume-function-name-regexp nil t)))))))
        (cond ((listp fnam)
               (setcar fnam (fume-trim-string (car fnam))) ; trim trailing crud
               (setq flst (cons fnam flst))
               (if fume-found-function-hook
                   (save-excursion (run-hooks 'fume-found-function-hook)))))
        (if fume-scanning-message
            (display-message
             'progress
             (format fume-scanning-message (fume-relative-position)))))
      (cond (fume-scanning-message
             (display-message
              'progress (format "%s done" (format fume-scanning-message 100))))
            (fume-rescanning-message
             (display-message
              'progress (format "%s done" fume-rescanning-message))))
      ;; make a copy of flst sorted by position in buffer
      (setq fume-modeline-funclist
            (nreverse
             (sort (fume-shallow-copy-list flst) 'fume-sort-by-position)))
      ;; make a copy of the function positions
      (setq fume-function-marks (mapcar 'cdr fume-modeline-funclist))
      (if fume-sort-function
          (setq fume-funclist (sort flst fume-sort-function))
        (setq fume-funclist (nreverse flst)))
      (if fume-rescan-buffer-hook
          (run-hooks 'fume-rescan-buffer-hook))))
  (if popmenu
      (function-menu)
    (let ((fume-rescan-inhibit-p t))
      (fume-update-menubar-entry)))
  ;; Reset dirty flag
  (setq fume-funclist-dirty-p nil))

(defun fume-scan-buffer ()
  (or fume-funclist (progn (fume-set-defaults) (fume-rescan-buffer))))

;; Routine to position cursor
;;
(defun fume-goto-function (fn pos)
  "Position cursor at function FN at location POS."
  (let ((orig-pos (point))
        (case-fold-search nil)
        (match-fn (cond ((string-match "DEFUN " fn) ; Emacs DEFUN declaration
                         (substring fn (match-end 0)))
                        ((string-match "^[ \t]*" fn) ; strip leading spaces
                         (substring fn (match-end 0)))
                        (t
                         fn))))

    (save-excursion
      (goto-char pos)
      (or (fume-bomb-proof (looking-at (regexp-quote match-fn)))
          (let ((fume-scanning-message nil))
            (fume-rescan-buffer)
            (setq pos (cdr-safe (assoc fn fume-funclist))))))

    (if pos
        (progn
          (goto-char pos)
          ;; possibly set mark
          (or (= orig-pos (point))
              (push-mark orig-pos (null fume-scanning-message)))
          (cond ((numberp fume-fn-window-position)
                 (set-window-start
                  (selected-window)
                  (save-excursion
                    (beginning-of-line
                     (- 1 (min (- (window-height) 2) fume-fn-window-position)))
                    (point))))
                ((fboundp fume-goto-function-hook)
                 (funcall fume-goto-function-hook pos))
                (t
                 (recenter))))
      (ding)
      (message "%s not found" fn)
      (function-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;  The main entry points for this package  ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interface to function-menu for mouse bindings only
;;
(defun mouse-function-menu (event)
  "Wrapper for mouse button bindings for function-menu."
  (interactive "e")
  (let ((currwin (selected-window)))
    (condition-case ()
        (progn
          (select-window (fume-event-window event))
          (let ((fume-auto-position-popup nil))
            (call-interactively 'function-menu)))
      (error (select-window currwin)))))

;; Interface for Key bindings
;;
;;;###autoload
(defun function-menu (&optional use-menubar return-only menu-item-function)
  "Pop up a menu of buffer local functions and related commands.
Each function menu entry jumps to its named function.  Prior to a jump, a
mark is set so that {C-u \\[set-mark-command]} will move point back to the
old location.

With optional prefix argument USE-MENUBAR, add the menu to the current
menubar.  With optional second argument RETURN-ONLY, return the menu
of functions but do not display it.

With optional third argument MENU-ITEM-FUNCTION, use this as the function
called by each menu item (default = 'fume-goto-function).  This function
must take two arguments, function menu item name (a string) and the position
\(an integer) within the buffer to leave point when displaying this menu item."
  (interactive "P")

  (setq use-menubar
        (and use-menubar fume-running-xemacs fume-not-tty current-menubar t))

  (catch 'no-functions
    (or (fume-set-defaults)
        (if (not (interactive-p))
            (throw 'no-functions t)
          (error "func-menu does not support the mode \"%s\"" mode-name)))

    ;; Create a list for this buffer only if there isn't any.
    (or fume-funclist
        (if fume-rescan-inhibit-p
            (fume-remove-menubar-entry)
          (fume-rescan-buffer)))
    (or fume-funclist
        (if (not (interactive-p))
            (throw 'no-functions t)
          (error "No functions found in this buffer")))

    ;; Rescan buffer trigger
    (fume-install-rescan-buffer-trigger)

    ;; Function name in modeline
    (fume-maybe-install-modeline-feature)

    (or menu-item-function (setq menu-item-function 'fume-goto-function))
    ;; The rest of this routine works only for (Lucid) XEmacs
    (cond (fume-running-xemacs
           ;; Create the menu
           (let* ((count 0)
                  (index-method
                   (intern-soft (format "fume-index-sublist-method-%d"
                                        fume-index-method)))
                  function-menu
                  (function-menu-items
                   (mapcar
                    (function
                     (lambda (sublist)
                       (setq count (1+ count))
                       (cons
                        (format "%s" (funcall index-method sublist count))
                        (mapcar
                         (function
                          (lambda (menu)
                            (vector
                             (fume-trim-string (format "%s" (car menu)) "&")
                             (list menu-item-function (car menu) (cdr menu))
                             t)))
                         sublist))))
                    (fume-split fume-funclist fume-max-items))))

             (or (> count 1)
                 (setq function-menu-items (cdr (car function-menu-items))))

             (if return-only
                 nil
               (setq function-menu
                     (` ((,@ function-menu-items)
                         "----"
                         ["Display full list of functions"
                          fume-list-functions t]
                         ;; Bill Dubuque <wgd@martigny.ai.mit.edu>
                         ;; This doesn't work with the old backquote.el
                         ;;[(, (concat "Rescan buffer :  " (buffer-name)))
                         ;; (fume-rescan-buffer (, (null use-menubar))) t]
                         (, (vector
                             (concat "Rescan buffer :  " (buffer-name))
                             (list 'fume-rescan-buffer (null use-menubar))
                             t))
                         "----"
			 ["Go to next function"
			  fume-goto-next-function t]
			 ["Go to previous function"
			  fume-goto-previous-function t]
                         "----"
                         ["Toggle modeline display"
                          fume-toggle-modeline-display t]
                         ["Toggle buffer auto rescanning"
                          fume-toggle-auto-rescanning t]
                         ["About Func-Menu" fume-about t])))

               (cond (use-menubar
                      (fume-remove-menubar-entry)
                      (set-buffer-menubar (copy-sequence current-menubar))
                      (fume-add-submenu
                       fume-menubar-menu-name
                       (` ((,@ function-menu)
                           "----"
                           ["Remove Function Menu from menubar"
                            fume-remove-menubar-entry t]))
                       fume-menubar-menu-location))

                     ((and fume-not-tty ; trap tty segmentation faults...
                           (not (popup-up-p)))
                      (or (fume-update-menubar-entry)
                          (setq function-menu
                                (cons
                                 ["Put Function Menu into menubar"
                                  (function-menu t) t]
                                 (cons "----" function-menu))))

                      (if fume-auto-position-popup
                          (fume-set-mouse-position))

                      (popup-menu
                       (cons fume-menubar-menu-name function-menu)))))

             ;; Return basic function menu for display by another function
             function-menu-items)))))

(defun fume-mouse-function-goto (event)
  "Goto function clicked on or prompt with completion in the minibuffer."
  (interactive "@e")
  (let ((orig-pos (point))
	(event-point (event-point event)))
    (if (not event-point)
	(message "Please click on some text.")
      (goto-char event-point)
      (let ((fume-no-prompt-on-valid-default t))
	(fume-prompt-function-goto))
      (or (= orig-pos (point))
	  (push-mark orig-pos (null fume-scanning-message))))
    ))

(defun fume-goto-next-function ()
  "Goto next function in current buffer."
  (interactive)
  (let ((where (point))
	;; note that `fume-function-marks' is in reverse order
	(marks (reverse fume-function-marks)))
    (while (and marks (<= (car marks) where))
      (pop marks))
    (and marks (goto-char (car marks)))
    ))

(defun fume-goto-previous-function ()
  "Goto previous function in current buffer."
  (interactive)
  (let ((where (point))
	;; note that `fume-function-marks' is already in reverse order
	(marks fume-function-marks))
    (while (and marks (>= (car marks) where))
      (pop marks))
    (and marks (goto-char (car marks)))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;  Keyboard access to func-menu for tty users  ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Internal variables only
;;
(defvar fume-list-srcbuffer nil)
(defvar fume-list-reused-win-p nil)
(defvar fume-list-trampled-buffer nil)

;; Espen Skoglund <espensk@stud.cs.uit.no>
;; David Hughes <d.hughes@videonetworks.com>
;;
(defun fume-prompt-function-goto (&optional other-window-p prompt)
  "Goto a function prompted for with completion in the minibuffer.
With optional prefix argument OTHER-WINDOW-P, jump to the function in a
different window.  PROMPT (useful for wrapper functions) must be a format
string with two `%s' elements."
  (interactive "P")
  (let* ((default-name (fume-what-looking-at t))
         (OrigBuffer (current-buffer))
         (flistMode (eq major-mode 'fume-list-mode))
         (no-prompt (or flistMode fume-no-prompt-on-valid-default))
         (TargetBuffer (if flistMode fume-list-srcbuffer OrigBuffer)))
    (switch-to-buffer TargetBuffer)
    (fume-scan-buffer) ;; Create funclist and set defaults if required
    (let* (;; verify default-name is a valid function name
           (default-exists-p (assoc default-name fume-funclist))
           ;; Prompt for function name in minibuffer, unless there is a valid
           ;; function name at point & fume-no-prompt-on-valid-default set to t
           (function-name
            (if (and default-exists-p no-prompt)
                ""
              (let ((this-command last-command)) ; preserve last-command
                (completing-read
                 (format (or prompt "Goto function%s%s: ")
                         (if other-window-p " other window" "")
                         (if default-exists-p
                             (concat " (" default-name ")")
                           ""))
                 fume-funclist nil t))))
           ;; Use default function name if just RET was pressed
           (function-name (if (and default-exists-p (string= "" function-name))
                              default-name
                            function-name)))
      (switch-to-buffer OrigBuffer)
      ;; Goto function or just return if function name is empty string
      (cond ((not (string= "" function-name))
             (if other-window-p
                 (cond ((prog1 (one-window-p)
                          (if (not (windowp other-window-p))
                              (switch-to-buffer-other-window TargetBuffer)
                            (select-window other-window-p)
                            (switch-to-buffer TargetBuffer)))
                        (other-window 1)
                        (shrink-window-if-larger-than-buffer)
                        (other-window 1)))
               (switch-to-buffer TargetBuffer))
             (fume-goto-function
              function-name (cdr (assoc function-name fume-funclist))))))))

(defun fume-prompt-function-goto-one-window ()
  (interactive)
  (delete-other-windows)
  (fume-prompt-function-goto))

(defun fume-prompt-function-goto-other-window ()
  (interactive)
  (fume-prompt-function-goto t))

(defun fume-list-functions-show-fn-other-window (&optional window)
  (interactive)
  (beginning-of-line)
  (select-window
   (prog1 (selected-window) (fume-prompt-function-goto (or window t)))))

(defun fume-list-functions-show-prev-fn-other-window (&optional window)
  (interactive)
  (forward-line -1)
  (fume-list-functions-show-fn-other-window window))

(defun fume-list-functions-show-next-fn-other-window (&optional window)
  (interactive)
  (forward-line 1)
  (beginning-of-line)
  (fume-list-functions-show-fn-other-window window))

(defun fume-list-functions-help ()
  (interactive)
  (fume-about)
  (sit-for 1)
  (display-message
   'prompt
   (format "SPC=%s, p=%s, n=%s, o=%s, G=%s, RET=%s,    q=%s"
           "this"
           "previous"
           "next"
           "other win"
           "one win"
           "this win"
           "quit")))

(defun fume-list-functions-quit ()
  (interactive)
  (if (eq major-mode 'fume-list-mode)
      (kill-buffer (current-buffer)))
  (if fume-list-reused-win-p
      (fume-bomb-proof (switch-to-buffer fume-list-trampled-buffer))
    (or (one-window-p)
        (delete-window (selected-window))))
  (if (not (eq (current-buffer) fume-list-srcbuffer))
      (condition-case ()
          (select-window (get-buffer-window fume-list-srcbuffer))
        (error (fume-bomb-proof (switch-to-buffer fume-list-srcbuffer))))))

(defun fume-list-mouse-select (event)
  (interactive "e")
  (let (ws cb cp (wc (current-window-configuration)))
    (mouse-set-point event)
    (fume-prompt-function-goto-other-window)
    (setq ws (save-excursion
               (beginning-of-line (- 1 fume-fn-window-position)) (point))
          cb (current-buffer)
          cp (point))
    (set-window-configuration wc)
    (switch-to-buffer cb)
    (set-window-start (selected-window) ws)
    (goto-char cp)))

(defvar fume-list-mode-map nil)
(or fume-list-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "q"    'fume-list-functions-quit)
      (define-key map "h"    'fume-list-functions-help)
      (define-key map "?"    'fume-list-functions-help)
      (define-key map "g"    'fume-prompt-function-goto)
      (define-key map "\C-m" 'fume-prompt-function-goto)
      (define-key map "G"    'fume-prompt-function-goto-one-window)
      (define-key map "o"    'fume-prompt-function-goto-other-window)
      (define-key map " "    'fume-list-functions-show-fn-other-window)
      (define-key map "p"    'fume-list-functions-show-prev-fn-other-window)
      (define-key map "n"    'fume-list-functions-show-next-fn-other-window)
      (if fume-not-tty
          (define-key map [(button2)] 'fume-list-mouse-select))
      (setq fume-list-mode-map map)))

(defvar fume-list-mode-hook nil "*Hook to run after fume-list-mode entered.")

(defun fume-list-functions (&optional this-window)
  "Creates a temporary buffer listing functions found in the current buffer."
  (interactive "P")
  (fume-scan-buffer) ;; Create funclist and set defaults if required
  (let ((func-near-point (fume-function-before-point)))
    (if func-near-point
        (setq func-near-point (fume-substitute func-near-point "\\[" "\\[" t)
              func-near-point (fume-substitute func-near-point "\\]" "\\]" t)
              func-near-point (format "^%s$" func-near-point)))
    (or fume-function-name-regexp
        (fume-maybe-install-modeline-feature)
        (error "Func-Menu is not operative in this buffer"))
    (save-excursion
      (let ((srcbuffer (current-buffer)))
        (set-buffer (get-buffer-create fume-buffer-name))
        (let (buffer-read-only) (erase-buffer))
	(fume-list-mode)
        (setq fume-list-srcbuffer srcbuffer
              fume-list-reused-win-p (not (one-window-p)))))
    (or fume-funclist (fume-rescan-buffer))
    (if fume-funclist
        (mapcar
         (function
          (lambda (p)
            (save-excursion
              (set-buffer fume-buffer-name)
              (let (buffer-read-only)
                (goto-char (point-max))
                (insert (concat (if (< (point-min) (point)) "\n") (car p)))
                (set-buffer-modified-p nil)
                (goto-char (point-min))))))
         fume-funclist))
    (cond ((interactive-p)
           (if current-prefix-arg
               (switch-to-buffer fume-buffer-name)
             (setq fume-list-trampled-buffer (window-buffer (next-window)))
             (switch-to-buffer-other-window fume-buffer-name)
             (or fume-list-reused-win-p
                 (shrink-window-if-larger-than-buffer)))
           (cond (func-near-point
                  (re-search-forward func-near-point nil t)
                  (beginning-of-line)))
           (fume-list-functions-help)))))

(defun fume-list-mode ()
  "Major mode for jumping from a list of buffer functions to a function
definition.

\\{fume-list-mode-map}"
  (use-local-map fume-list-mode-map)
  (setq buffer-read-only t
	mode-name "Func-Menu"
	major-mode 'fume-list-mode)
  (if fume-not-tty
      (setq mode-motion-hook 'mode-motion-highlight-symbol))
  (run-hooks 'fume-list-mode-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;  func-menu minor mode  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom fume-mode-line-string " Func"
  "*String to use in the modeline when func-menu minor mode is active."
  :group 'fume
  :type 'string)

(defcustom fume-mode-hook nil
  "*Hooks to run after setting up func-menu minor mode."
  :group 'fume
  :type 'hook)

(defvar fume-mode-map
  (let ((map (make-sparse-keymap 'fume-minor-mode-map)))
    (define-key map [(control c) (control f) f] 'function-menu)
    (define-key map [(control c) (control f) l] 'fume-list-functions)
    (define-key map [(control c) (control f) r] 'fume-rescan-buffer)
    (define-key map [(control c) (control f) R] 'fume-toggle-auto-rescanning)
    (define-key map [(control c) (control f) p] 'fume-goto-previous-function)
    (define-key map [(control c) (control f) n] 'fume-goto-next-function)
    (define-key map [(control c) (control f) g] 'fume-prompt-function-goto)
    (define-key map [(control c) (control f) D] 'fume-toggle-modeline-display)
    (define-key map [(control c) (control f) h] 'fume-about)
    (define-key map [(control c) (control f) ??] 'fume-about)
    (define-key map '(meta button3) 'mouse-function-menu)
    (define-key map '(meta button1) 'fume-mouse-function-goto)
    map)
  ;; fume minor mode keymap.
  )

(make-variable-buffer-local
 (defvar fume-mode nil))

;;;###autoload
(defun fume-mode (arg)
  "Toggle the func menu minor mode on/off.
With a prefix numeric value or ARG > 0, turn it on.
The variable `fume-mode-line-string' defines what you will see in the
modeline when the mode is on.

Note: if you had been using

\\{fume-mode-map}"
  (interactive "P")
  (let ((turn-it-on (if (null arg) (not fume-mode)
		      (> (prefix-numeric-value arg) 0))))
    (cond ((and turn-it-on (not fume-mode)) ;; turn it on
	   (function-menu t)
	   (setq fume-mode t)
	   (run-hooks 'fume-mode-hook))
	  ((and (not turn-it-on) fume-mode) ;; turn it off
	   (fume-remove-menubar-entry)
	   (setq fume-mode nil)))
    (redraw-modeline)))

(add-minor-mode 'fume-mode fume-mode-line-string fume-mode-map)

;;;###autoload
(defun turn-on-fume-mode ()
  "Unconditionaly turn the func menu minor mode on."
  (interactive)
  (fume-mode 1))

(defun turn-off-fume-mode ()
  "Unconditionaly turn the func menu minor mode off."
  (interactive)
  (fume-mode 0))

;;;###autoload
(defun fume-setup-buffer (&optional force)
  "Setup the current buffer for func-menu.
This actually means prepare for turning `fume-mode' on.

If you've been using func-menu the old way (before `fume-mode' was
written), please note that `mouse-function-menu' is bound to M-Button3
(not Shift-Button3) in this mode.  Nothing prevents you from keeping your
old binding though.

This function is meant to be added to `find-file-hooks'.
If FORCE is non-nil, actually do the operation now; otherwise,
delay until the next time the event loop is processed (this is
so that Lisp routines that temporarily load a file, process it,
and then kill it will not be slowed down by `function-menu' processing)."
  (interactive)
  (if force
      (turn-on-fume-mode)
    (enqueue-eval-event 'fume-do-setup-buffer (current-buffer))))

(defun fume-do-setup-buffer (buffer)
  (and (buffer-live-p buffer)
       (save-excursion
	 (save-window-excursion
	   (set-buffer buffer)
	   (turn-on-fume-mode)))))

(provide 'func-menu)

;;; func-menu.el ends here
