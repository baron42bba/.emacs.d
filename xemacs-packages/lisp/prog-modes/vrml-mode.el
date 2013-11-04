;;; vrml-mode.el --- major mode for editing VRML (.wrl) files

;; Copyright (C) 1994 Free Software Foundation, Inc.
;; Copyright (C) 1996 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>
;; Keywords: languages vrml modes

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
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Mostly bastardized from tcl.el.

;; HOW TO INSTALL:
;; Put the following forms in your .emacs to enable autoloading of VRML
;; mode, and auto-recognition of ".wrl" files.
;;
;;   (autoload 'vrml-mode "vrml" "VRML mode." t)
;;   (setq auto-mode-alist (append '(("\\.wrl\\'" . vrml-mode))
;;				   auto-mode-alist))
;;

;;; Code:

;;
;; User variables.
;;

(defgroup vrml nil
  "Major mode for editing VRML (.wrl) files."
  :group 'languages)


(defcustom vrml-indent-level 3
  "*Indentation of VRML statements with respect to containing block."
  :type 'integer
  :group 'vrml)

(defcustom vrml-auto-newline nil
  "*Non-nil means automatically newline before and after braces
inserted in VRML code."
  :type 'boolean
  :group 'vrml)

(defcustom vrml-tab-always-indent t
  "*Control effect of TAB key.
If t (the default), always indent current line.
If nil and point is not in the indentation area at the beginning of
the line, a TAB is inserted.
Other values cause the first possible action from the following list
to take place:

  1. Move from beginning of line to correct indentation.
  2. Delete an empty comment.
  3. Move forward to start of comment, indenting if necessary.
  4. Move forward to end of line, indenting if necessary.
  5. Create an empty comment.
  6. Move backward to start of comment, indenting if necessary."
  :type '(choice (const :tag "on" t)
		 (const :tag "off" nil)
		 (sexp :format "%t\n" :tag "The Works" other))
  :group 'vrml)

(defcustom vrml-use-hairy-comment-detector t
  "*If not `nil', then the more complicated, but slower, comment
detecting function is used."
  :type 'boolean
  :group 'vrml)

(defvar vrml-mode-abbrev-table nil
  "Abbrev table used while in VRML mode.")
(define-abbrev-table 'vrml-mode-abbrev-table ())

(defvar vrml-mode-map ()
  "Keymap used in VRML mode.")
(if (null vrml-mode-map)
    (progn
      (setq vrml-mode-map (make-sparse-keymap))
      (set-keymap-name vrml-mode-map 'vrml-mode-map)
      (define-key vrml-mode-map "{" 'vrml-electric-brace)
      (define-key vrml-mode-map "}" 'vrml-electric-brace)
      (define-key vrml-mode-map "\e\C-q" 'indent-vrml-exp)
      (define-key vrml-mode-map "\t" 'vrml-indent-command)
      (define-key vrml-mode-map "\M-;" 'vrml-indent-for-comment)
      ))

(defvar vrml-mode-syntax-table nil
  "Syntax table in use in vrml-mode buffers.")

(if vrml-mode-syntax-table
    ()
  (setq vrml-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\n ">" vrml-mode-syntax-table)
  (modify-syntax-entry ?\f ">" vrml-mode-syntax-table)
  (modify-syntax-entry ?\# "<" vrml-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" vrml-mode-syntax-table)
  (modify-syntax-entry ?%  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?@  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?&  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?*  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?-  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?:  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?!  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?$  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?/  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?~  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?<  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?=  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?>  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?|  "_" vrml-mode-syntax-table)
  (modify-syntax-entry ?+ "." vrml-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" vrml-mode-syntax-table))

(defcustom vrml-mode-hook nil
  "Hook run on entry to VRML mode."
  :type 'hook
  :group 'vrml)

(defvar vrml-keyword-list
  '(
    ; shape nodes:
    "AsciiText" "Cone" "Cube" "Cylinder" "IndexedFaceSet" "IndexedLineSet"
    "PointSet" "Sphere"
    ; geometry and material nodes:
    "Coordinate3" "FontStyle" "Info" "LOD" "Material" "MaterialBinding"
    "Normal" "NormalBinding" "Texture2" "Texture2Transform"
    "TextureCoordinate2" "ShapeHints"
    ; transformation nodes:
    "MatrixTransform" "Rotation" "Scale" "Transform" "Translation"
    ;camera nodes:
    "OrthographicCamera" "PerspectiveCamera"
    ;lighting nodes:
    "DirectionalLight" "PointLight" "SpotLight"
    ;group nodes:
    "Group" "Separator" "Switch" "TransformSeparator" "WWWAnchor"
    ;other:
    "WWWInline"
    ;new VRML 2.0 nodes (#### not yet classified)
    "Anchor" "Appearance" "AudioClip" "Background" "Billboard" "Box"
    "Collision" "Color" "ColorInterpolator" "Coordinate"
    "CoordinateInterpolator" "CylinderSensor" "DiskSensor" "ElevationGrid"
    "Extrusion" "Fog" "FontStyle" "ImageTexture" "Inline" "MovieTexture"
    "NavigationInfo" "NormalInterpolator" "OrientationInterpolator"
    "PixelTexture" "PlaneSensor" "PositionInterpolator" "ProximitySensor"
    "ScalarInterpolator" "Script" "Shape" "Sound" "SphereSensor" "Text"
    "TextureTransform" "TextureCoordinate" "TimeSensor" "TouchSensor"
    "Viewpoint" "VisibilitySensor" "WorldInfo"
    ;VRML 2.0 node fields
    "eventIn" "eventOut" "field" "exposedField"
    ;misc. VRML 2.0 keywords (DEF, PROTO, EXTERNPROTO handled below)
    "USE" "ROUTE" "TO" "IS" "TRUE" "FALSE" "NULL"
))

(defconst vrml-font-lock-keywords
  (list
   ;; Names of functions (and other "defining things").
   (list "\\(DEF\\|PROTO\\|EXTERNPROTO\\)[ \t\n]+\\([^ \t\n]+\\)"
	 2 'font-lock-function-name-face)

   ;; Keywords.  Only recognized if surrounded by whitespace.
   ;; FIXME consider using "not word or symbol", not
   ;; "whitespace".
   (cons (concat "\\(\\s-\\|^\\)\\("
		 ;; FIXME Use regexp-quote? 
		 (mapconcat 'identity vrml-keyword-list "\\|")
		 "\\)\\(\\s-\\|$\\)")
	 2)
   )
  "Keywords to highlight for VRML.  See variable `font-lock-keywords'.")

;;;###autoload
(defun vrml-mode ()
  "Major mode for editing VRML code.
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
\\{vrml-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map vrml-mode-map)
  (setq major-mode 'vrml-mode)
  (setq mode-name "VRML")
  (setq local-abbrev-table vrml-mode-abbrev-table)
  (set-syntax-table vrml-mode-syntax-table)

  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (if (fboundp 'move-to-left-margin)
      (progn
	;; In FSF Emacs 19.29 / XEmacs 19.14, you aren't supposed to
	;; start these with a ^.
	(setq paragraph-start "$\\|")
	(setq paragraph-separate paragraph-start))
    (setq paragraph-start (concat "^$\\|" page-delimiter))
    (setq paragraph-separate paragraph-start))
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'vrml-do-fill-paragraph)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'vrml-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)

  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'outline-regexp)
  (setq outline-regexp "[^\n\^M]")
  (make-local-variable 'outline-level)
  (setq outline-level 'vrml-outline-level)

  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords vrml-font-lock-keywords)

  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  (make-local-variable 'defun-prompt-regexp)
  (setq defun-prompt-regexp "^[^ \t\n#}][^\n}]+}*[ \t]+")

  ;; Settings for new dabbrev code.
  (make-local-variable 'dabbrev-case-fold-search)
  (setq dabbrev-case-fold-search nil)
  (make-local-variable 'dabbrev-case-replace)
  (setq dabbrev-case-replace nil)
  (make-local-variable 'dabbrev-abbrev-skip-leading-regexp)
  (setq dabbrev-abbrev-skip-leading-regexp "[$!]")
  (make-local-variable 'dabbrev-abbrev-char-regexp)
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

  (run-hooks 'vrml-mode-hook))

;; This is used for closing braces.  If vrml-auto-newline is set, can
;; insert a newline both before and after the brace, depending on
;; context.  FIXME should this be configurable?  Does anyone use this?
(defun vrml-electric-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "p")
  ;; If auto-newlining and there is stuff on the same line, insert a
  ;; newline first.
  (if vrml-auto-newline
      (progn
	(if (save-excursion
	      (skip-chars-backward " \t")
	      (bolp))
	    ()
	  (vrml-indent-line)
	  (newline))
	;; In auto-newline case, must insert a newline after each
	;; brace.  So an explicit loop is needed.
	(while (> arg 0)
	  (insert last-command-char)
	  (vrml-indent-line)
	  (newline)
	  (setq arg (1- arg))))
    (self-insert-command arg))
  (vrml-indent-line))



(defun vrml-indent-command (&optional arg)
  "Indent current line as VRML code, or in some cases insert a tab character.
If vrml-tab-always-indent is t (the default), always indent current line.
If vrml-tab-always-indent is nil and point is not in the indentation
area at the beginning of the line, a TAB is inserted.
Other values of vrml-tab-always-indent cause the first possible action
from the following list to take place:

  1. Move from beginning of line to correct indentation.
  2. Delete an empty comment.
  3. Move forward to start of comment, indenting if necessary.
  4. Move forward to end of line, indenting if necessary.
  5. Create an empty comment.
  6. Move backward to start of comment, indenting if necessary."
  (interactive "p")
  (cond
   ((not vrml-tab-always-indent)
    ;; Indent if in indentation area, otherwise insert TAB.
    (if (<= (current-column) (current-indentation))
	(vrml-indent-line)
      (self-insert-command arg)))
   ((eq vrml-tab-always-indent t)
    ;; Always indent.
    (vrml-indent-line))
   (t
    ;; "Perl-mode" style TAB command.
    (let* ((ipoint (point))
	   (eolpoint (progn
		       (end-of-line)
		       (point)))
	   (comment-p (vrml-in-comment)))
      (cond
       ((= ipoint (save-excursion
		    (beginning-of-line)
		    (point)))
	(beginning-of-line)
	(vrml-indent-line)
	;; If indenting didn't leave us in column 0, go to the
	;; indentation.  Otherwise leave point at end of line.  This
	;; is a hack.
	(if (= (point) (save-excursion
			 (beginning-of-line)
			 (point)))
	    (end-of-line)
	  (back-to-indentation)))
       ((and comment-p (looking-at "[ \t]*$"))
	;; Empty comment, so delete it.  We also delete any ";"
	;; characters at the end of the line.  I think this is
	;; friendlier, but I don't know how other people will feel.
	(backward-char)
	(skip-chars-backward " \t;")
	(delete-region (point) eolpoint))
       ((and comment-p (< ipoint (point)))
	;; Before comment, so skip to it.
	(vrml-indent-line)
	(indent-for-comment))
       ((/= ipoint eolpoint)
	;; Go to end of line (since we're not there yet).
	(goto-char eolpoint)
	(vrml-indent-line))
       ((not comment-p)
	(vrml-indent-line)
	(vrml-indent-for-comment))
       (t
	;; Go to start of comment.  We don't leave point where it is
	;; because we want to skip comment-start-skip.
	(vrml-indent-line)
	(indent-for-comment)))))))

(defun vrml-indent-line ()
  "Indent current line as VRML code.
Return the amount the indentation changed by."
  (let ((indent (calculate-vrml-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((= (following-char) ?})
		  (setq indent (- indent vrml-indent-level)))
		 ((= (following-char) ?\])
		  (setq indent (- indent 1))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun calculate-vrml-indent (&optional parse-start)
  "Return appropriate indentation for current line as VRML code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let* ((indent-point (point))
	   (case-fold-search nil)
	   state
	   containing-sexp
	   found-next-line)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; Inside comment or string.  Return nil or t if should
	     ;; not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.
	     0)
	    (t
	     (goto-char containing-sexp)
	     (let* ((expr-start (point)))
	       ;; Find the first statement in the block and indent
	       ;; like it.  The first statement in the block might be
	       ;; on the same line, so what we do is skip all
	       ;; "virtually blank" lines, looking for a non-blank
	       ;; one.  A line is virtually blank if it only contains
	       ;; a comment and whitespace.  We do it this funky way
	       ;; because we want to know if we've found a statement
	       ;; on some line _after_ the line holding the sexp
	       ;; opener.
	       (goto-char containing-sexp)
	       (forward-char)
	       (if (and (< (point) indent-point)
			(looking-at "[ \t]*\\(#.*\\)?$"))
		   (progn
		     (forward-line)
		     (while (and (< (point) indent-point)
				 (looking-at "[ \t]*\\(#.*\\)?$"))
		       (setq found-next-line t)
		       (forward-line))))
	       (if (not (or (= (char-after containing-sexp) ?{)
			    (and (= (char-after containing-sexp) ?\[)
				 (save-excursion
				   (goto-char containing-sexp)
				   (skip-chars-backward " \t\n")
				   (forward-char -8)
				   (looking-at "children")))))
		   (progn
		     ;; Line is continuation line, or the sexp opener
		     ;; is not a curly brace, or we are looking at
		     ;; an `expr' expression (which must be split
		     ;; specially).  So indentation is column of first
		     ;; good spot after sexp opener.  If there is no
		     ;; nonempty line before the indentation point, we
		     ;; use the column of the character after the sexp
		     ;; opener.
		     (if (>= (point) indent-point)
			 (progn
			   (goto-char containing-sexp)
			   (forward-char))
		       (skip-chars-forward " \t"))
		     (current-column))
		 ;; After a curly brace, and not a continuation line.
		 ;; So take indentation from first good line after
		 ;; start of block, unless that line is on the same
		 ;; line as the opening brace.  In this case use the
		 ;; indentation of the opening brace's line, plus
		 ;; another indent step.  If we are in the body part
		 ;; of an "if" or "while" then the indentation is
		 ;; taken from the line holding the start of the
		 ;; statement.
		 (if (and (< (point) indent-point)
			  found-next-line)
		     (current-indentation)
		   (if t ; commands-p
		       (goto-char expr-start)
		     (goto-char containing-sexp))
		   (+ (current-indentation) vrml-indent-level)))))))))



(defun indent-vrml-exp ()
  "Indent each line of the VRML grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	outer-loop-done inner-loop-done state ostate
	this-indent last-sexp
	(next-depth 0)
	last-depth)
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq inner-loop-done nil)
	(while (and (not inner-loop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 ostate))
	      (vrml-indent-line))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq inner-loop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  ;; If this line had ..))) (((.. in it, pop out of the levels
	  ;; that ended anywhere in this line, even if the final depth
	  ;; doesn't indicate that they ended.
	  (while (> last-depth (nth 6 state))
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  ;; Add levels for any parens that were started in this line.
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack 
		      (or (car (cdr state))
			  (save-excursion
			    (forward-sexp -1)
			    (point)))))
	  (forward-line 1)
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		(setq this-indent (car indent-stack))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (calculate-vrml-indent
			  (if (car indent-stack)
			      (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))
		))
	    (cond ((not (numberp this-indent)))
		  ((= (following-char) ?})
		   (setq this-indent (- this-indent vrml-indent-level)))
		  ((= (following-char) ?\])
		   (setq this-indent (- this-indent 1))))
	    ;; Put chosen indentation into effect.
	    (or (null this-indent)
		(= (current-column) 
		   this-indent)
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to 
		   this-indent))))))))
  )

;;
;; Auto-fill support.
;;

(defun vrml-real-command-p ()
  "Return nil if point is not at the beginning of a command.
A command is the first word on an otherwise empty line, or the
first word following an opening brace."
  (save-excursion
    (skip-chars-backward " \t")
    (cond
     ((bobp) t)
     ((bolp)
      (backward-char)
      ;; Note -- continued comments are not supported here.  I
      ;; consider those to be a wart on the language.
      (not (eq ?\\ (preceding-char))))
     (t
      (memq (preceding-char) '(?{))))))

;; FIXME doesn't actually return t.  See last case.
(defun vrml-real-comment-p ()
  "Return t if point is just after the `#' beginning a real comment.
Does not check to see if previous char is actually `#'.
A real comment is either at the beginning of the buffer,
preceded only by whitespace on the line, or has a preceding
semicolon, opening brace, or opening bracket on the same line."
  (save-excursion
    (backward-char)
    (vrml-real-command-p)))

(defun vrml-hairy-scan-for-comment (state end always-stop)
  "Determine if point is in a comment.
Returns a list of the form `(FLAG . STATE)'.  STATE can be used
as input to future invocations.  FLAG is nil if not in comment,
t otherwise.  If in comment, leaves point at beginning of comment.
See also `vrml-simple-scan-for-comment', a simpler version that is
often right."
  (let ((bol (save-excursion
	       (goto-char end)
	       (beginning-of-line)
	       (point)))
	real-comment
	last-cstart)
    (while (and (not last-cstart) (< (point) end))
      (setq real-comment nil)		;In case we've looped around and it is
                                        ;set.
      (setq state (parse-partial-sexp (point) end nil nil state t))
      (if (nth 4 state)
	  (progn
	    ;; If ALWAYS-STOP is set, stop even if we don't have a
	    ;; real comment, or if the comment isn't on the same line
	    ;; as the end.
	    (if always-stop (setq last-cstart (point)))
	    ;; If we have a real comment, then set the comment
	    ;; starting point if we are on the same line as the ending
	    ;; location.
	    (setq real-comment (vrml-real-comment-p))
	    (if real-comment
		(progn
		  (and (> (point) bol) (setq last-cstart (point)))
		  ;; NOTE Emacs 19 has a misfeature whereby calling
		  ;; parse-partial-sexp with COMMENTSTOP set and with
		  ;; an initial list that says point is in a comment
		  ;; will cause an immediate return.  So we must skip
		  ;; over the comment ourselves.
		  (beginning-of-line 2)))
	    ;; Frob the state to make it look like we aren't in a
	    ;; comment.
	    (setcar (nthcdr 4 state) nil))))
    (and last-cstart
	 (goto-char last-cstart))
    (cons real-comment state)))

(defun vrml-hairy-in-comment ()
  "Return t if point is in a comment, and leave point at beginning
of comment."
  (let ((save (point)))
    (beginning-of-defun)
    (car (vrml-hairy-scan-for-comment nil save nil))))

(defun vrml-simple-in-comment ()
  "Return t if point is in comment, and leave point at beginning
of comment.  This is faster than `vrml-hairy-in-comment', but is
correct less often."
  (let ((save (point))
	comment)
    (beginning-of-line)
    (while (and (< (point) save) (not comment))
      (search-forward "#" save 'move)
      (setq comment (vrml-real-comment-p)))
    comment))

(defun vrml-in-comment ()
  "Return t if point is in comment, and leave point at beginning
of comment."
  (if vrml-use-hairy-comment-detector
      (vrml-hairy-in-comment)
    (vrml-simple-in-comment)))

(defun vrml-do-fill-paragraph (ignore)
  "fill-paragraph function for VRML mode.  Only fills in a comment."
  (let (in-comment col where)
    (save-excursion
      (end-of-line)
      (setq in-comment (vrml-in-comment))
      (if in-comment
	  (progn
	    (setq where (1+ (point)))
	    (setq col (1- (current-column))))))
    (and in-comment
	 (save-excursion
	   (back-to-indentation)
	   (= col (current-column)))
	 ;; In a comment.  Set the fill prefix, and find the paragraph
	 ;; boundaries by searching for lines that look like
	 ;; comment-only lines.
	 (let ((fill-prefix (buffer-substring (progn
						(beginning-of-line)
						(point))
					      where))
	       p-start p-end)
	   ;; Search backwards.
	   (save-excursion
	     (while (looking-at "^[ \t]*#")
	       (forward-line -1))
	     (forward-line)
	     (setq p-start (point)))

	   ;; Search forwards.
	   (save-excursion
	     (while (looking-at "^[ \t]*#")
	       (forward-line))
	     (setq p-end (point)))

	   ;; Narrow and do the fill.
	   (save-restriction
	     (narrow-to-region p-start p-end)
	     (fill-paragraph ignore)))))
  t)

(defun vrml-do-auto-fill ()
  "Auto-fill function for VRML mode.  Only auto-fills in a comment."
  (if (> (current-column) fill-column)
      (let ((fill-prefix "# ")
	    in-comment col)
	(save-excursion
	  (setq in-comment (vrml-in-comment))
	  (if in-comment
	      (setq col (1- (current-column)))))
	(if in-comment
	    (progn
	      (do-auto-fill)
	      (save-excursion
		(back-to-indentation)
		(delete-region (point) (save-excursion
					 (beginning-of-line)
					 (point)))
		(indent-to-column col)))))))

(defun vrml-indent-for-comment ()
  "Indent this line's comment to comment column, or insert an empty comment.
Is smart about syntax of VRML comments.
Parts of this were taken from indent-for-comment (simple.el)."
  (interactive "*")
  (end-of-line)
  (or (vrml-in-comment)
      (progn
	;; Not in a comment, so we have to insert one.  Create an
	;; empty comment (since there isn't one on this line).
	(skip-chars-backward " \t")
	(let ((eolpoint (point)))
	  (beginning-of-line)
	  (if (/= (point) eolpoint)
	      (progn
		(goto-char eolpoint)
		(insert
		 "# ")
		(backward-char))))))
  ;; Point is just after the "#" starting a comment.  Move it as
  ;; appropriate.
  (let* ((indent (funcall comment-indent-function))
	 (begpos (progn
		   (backward-char)
		   (point))))
    (if (/= begpos indent)
	(progn
	  (skip-chars-backward " \t" (save-excursion
				       (beginning-of-line)
				       (point)))
	  (delete-region (point) begpos)
	  (indent-to indent)))
    (looking-at comment-start-skip)	; Always true.
    (goto-char (match-end 0))
    ;; I don't like the effect of the next two.
    ;;(skip-chars-backward " \t" (match-beginning 0))
    ;;(skip-chars-backward "^ \t" (match-beginning 0))
    ))

;; XEmacs addition
;;;###autoload(add-to-list 'auto-mode-alist '("\\.wrl\\'" . vrml-mode))

;;; vrml-mode.el ends here
