;;; enriched.el --- read and save files in text/enriched format

;; Copyright (c) 1994, 1995, 1996 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@gnu.ai.mit.edu>
;; Adapted-by: Mike Sperber <sperber@informatik.uni-tuebingen.de>
;; Maintainer: XEmacs Development Team
;; Keywords: wp, faces

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

;;; Synched up with: FSF 20.2.

;;; Commentary:

;; This file implements reading, editing, and saving files with
;; text-properties such as faces, levels of indentation, and true line
;; breaks distinguished from newlines just used to fit text into the window.

;; The file format used is the MIME text/enriched format, which is a
;; standard format defined in internet RFC 1563.  All standard annotations
;; are supported except for <smaller> and <bigger>, which are currently not
;; possible to display.

;; A separate file, enriched.doc, contains further documentation and other
;; important information about this code.  It also serves as an example
;; file in text/enriched format.  It should be in the etc directory of your
;; emacs distribution.

;;; TODO for the XEmacs port:
;;
;; Currently XEmacs does not support default-text-properties.  The
;; original enriched.el uses this to set the left-margin,
;; right-margin, and justification properties to 'front-sticky.
;; If you know the Right Way to fix this, contact
;; Mike Sperber <sperber@informatik.uni-tuebingen.de>.

;;; Code:

(provide 'enriched)

;;;
;;; Variables controlling the display
;;;

(defgroup enriched nil
  "Read and save files in text/enriched format"
  :group 'wp)

(defcustom enriched-verbose t
  "*If non-nil, give status messages when reading and writing files."
  :type 'boolean
  :group 'enriched)

;;;
;;; Set up faces & display table
;;;

;; Emacs doesn't have a "fixed" face by default, since all faces currently
;; have to be fixed-width.  So we just pick one that looks different from the
;; default.
(defface fixed
  '((t (:bold t)))
  "Face used for text that must be shown in fixed width.
Currently, emacs can only display fixed-width fonts, but this may change.
This face is used for text specifically marked as fixed-width, for example
in text/enriched files."
  :group 'enriched)

(defface excerpt
  '((t (:italic t)))
  "Face used for text that is an excerpt from another document.
This is used in enriched-mode for text explicitly marked as an excerpt."
  :group 'enriched)

(defconst enriched-display-table
  ;; XEmacs change
  ;; (or (copy-sequence standard-display-table)
  ;;     (make-display-table)))
  (make-display-table))
(aset enriched-display-table ?\f (make-vector (1- (frame-width)) ?-))

(defconst enriched-par-props '(left-margin right-margin justification)
  "Text-properties that usually apply to whole paragraphs.
These are set front-sticky everywhere except at hard newlines.")

;;;
;;; Variables controlling the file format
;;;   (bidirectional)

(defconst enriched-initial-annotation
  (lambda ()
    (format "Content-Type: text/enriched\nText-Width: %d\n\n"
	    fill-column))
  "What to insert at the start of a text/enriched file.
If this is a string, it is inserted.  If it is a list, it should be a lambda
expression, which is evaluated to get the string to insert.")

(defconst enriched-annotation-format "<%s%s>"
  "General format of enriched-text annotations.")

(defconst enriched-annotation-regexp "<\\(/\\)?\\([-A-za-z0-9]+\\)>"
  "Regular expression matching enriched-text annotations.")

(defconst enriched-translations
  '((face          (bold-italic "bold" "italic")
		   (bold        "bold")
		   (italic      "italic")
		   (underline   "underline")
		   (fixed       "fixed")
		   (excerpt     "excerpt")
		   (default     )
		   (nil         enriched-encode-other-face))
    (left-margin   (4           "indent"))
    (right-margin  (4           "indentright"))
    (justification (none        "nofill")
		   (right       "flushright")
		   (left        "flushleft")
		   (full        "flushboth")
		   (center      "center"))
    (PARAMETER     (t           "param")) ; Argument of preceding annotation
    ;; The following are not part of the standard:
    (FUNCTION      (enriched-decode-foreground "x-color")
		   (enriched-decode-background "x-bg-color")
		   ;; XEmacs addition
		   (facemenu-make-larger "bigger")
		   (facemenu-make-smaller "smaller"))
    (read-only     (t           "x-read-only"))
    (unknown       (nil         format-annotate-value))
;   (font-size     (2           "bigger")       ; unimplemented
;                  (-2          "smaller"))
)
  "List of definitions of text/enriched annotations.
See `format-annotate-region' and `format-deannotate-region' for the definition
of this structure.")

(defconst enriched-ignore
  '(front-sticky rear-nonsticky hard)
  "Properties that are OK to ignore when saving text/enriched files.
Any property that is neither on this list nor dealt with by
`enriched-translations' will generate a warning.")

;;; Internal variables

(defvar enriched-mode nil
  "True if Enriched mode is in use.")
(make-variable-buffer-local 'enriched-mode)

(if (not (assq 'enriched-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(enriched-mode " Enriched")
		minor-mode-alist)))

(defcustom enriched-mode-hook nil
  "Functions to run when entering Enriched mode.
If you set variables in this hook, you should arrange for them to be restored
to their old values if you leave Enriched mode.  One way to do this is to add
them and their old values to `enriched-old-bindings'."
  :type 'hook
  :group 'enriched)

(defvar enriched-old-bindings nil
  "Store old variable values that we change when entering mode.
The value is a list of \(VAR VALUE VAR VALUE...).")
(make-variable-buffer-local 'enriched-old-bindings)

;;;
;;; Define the mode
;;;

;;;###autoload
(defun enriched-mode (&optional arg)
  "Minor mode for editing text/enriched files.
These are files with embedded formatting information in the MIME standard
text/enriched format.
Turning the mode on runs `enriched-mode-hook'.

More information about Enriched mode is available in the file 
etc/enriched.doc  in the Emacs distribution directory.

Commands:

\\<enriched-mode-map>\\{enriched-mode-map}"
  (interactive "P")
  (let ((mod (buffer-modified-p)))
    (cond ((or (<= (prefix-numeric-value arg) 0)
	       (and enriched-mode (null arg)))
	   ;; Turn mode off
	   (setq enriched-mode nil)
	   (setq buffer-file-format (delq 'text/enriched buffer-file-format))
	   ;; restore old variable values
	   (while enriched-old-bindings
	     (funcall 'set (car enriched-old-bindings)
		      (car (cdr enriched-old-bindings)))
	     (setq enriched-old-bindings (cdr (cdr enriched-old-bindings)))))

	  (enriched-mode nil)		; Mode already on; do nothing.

	  (t (setq enriched-mode t)	; Turn mode on
	     (add-to-list 'buffer-file-format 'text/enriched)
	     ;; Save old variable values before we change them.
	     ;; These will be restored if we exit Enriched mode.
	     (setq enriched-old-bindings
		   ;; XEmacs change
		   (list ; 'buffer-display-table buffer-display-table
			 'indent-line-function indent-line-function
			 'default-text-properties default-text-properties))
	     (make-local-variable 'indent-line-function)
	     (make-local-variable 'default-text-properties)
	     (setq indent-line-function 'indent-to-left-margin
 		   ;; XEmacs change
 		   ;; buffer-display-table  enriched-display-table
		   )
	     (use-hard-newlines 1 nil)
	     (let ((sticky (plist-get default-text-properties 'front-sticky))
		   (p enriched-par-props))
	       (while p
		 (add-to-list 'sticky (car p))
		 (setq p (cdr p)))
	       (if sticky
		   (setq default-text-properties
			 (plist-put default-text-properties
				    'front-sticky sticky))))
	     (run-hooks 'enriched-mode-hook)))
    (set-buffer-modified-p mod)
    ;; XEmacs change
    (redraw-modeline)))

;;;
;;; Keybindings
;;;

(defvar enriched-mode-map nil
  "Keymap for Enriched mode.")

(if (null enriched-mode-map)
    (fset 'enriched-mode-map (setq enriched-mode-map (make-sparse-keymap))))

(if (not (assq 'enriched-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons (cons 'enriched-mode enriched-mode-map)
		minor-mode-map-alist)))

(define-key enriched-mode-map "\C-a" 'beginning-of-line-text)
(define-key enriched-mode-map "\C-m" 'reindent-then-newline-and-indent)
(define-key enriched-mode-map "\C-j" 'reindent-then-newline-and-indent)
(define-key enriched-mode-map "\M-j" 'facemenu-justification-menu)
(define-key enriched-mode-map "\M-S" 'set-justification-center)
(define-key enriched-mode-map "\C-x\t" 'increase-left-margin)
(define-key enriched-mode-map "\C-c\C-l" 'set-left-margin)
(define-key enriched-mode-map "\C-c\C-r" 'set-right-margin)

;;;
;;; Some functions dealing with text-properties, especially indentation
;;;

(defun enriched-map-property-regions (prop func &optional from to)
  "Apply a function to regions of the buffer based on a text property.
For each contiguous region of the buffer for which the value of PROPERTY is
eq, the FUNCTION will be called.  Optional arguments FROM and TO specify the
region over which to scan.

The specified function receives three arguments: the VALUE of the property in
the region, and the START and END of each region."
  (save-excursion
    (save-restriction
      (if to (narrow-to-region (point-min) to))
      (goto-char (or from (point-min)))
      (let ((begin (point))
	    end
	    (marker (make-marker))
	    (val (get-text-property (point) prop)))
	(while (setq end (text-property-not-all begin (point-max) prop val))
	  (move-marker marker end)
	  (funcall func val begin (marker-position marker))
	  (setq begin (marker-position marker)
		val (get-text-property marker prop)))
	(if (< begin (point-max))
	    (funcall func val begin (point-max)))))))

(put 'enriched-map-property-regions 'lisp-indent-hook 1)

(defun enriched-insert-indentation (&optional from to)
  "Indent and justify each line in the region."
  (save-excursion
    (save-restriction
      (if to (narrow-to-region (point-min) to))
      (goto-char (or from (point-min)))
      (if (not (bolp)) (forward-line 1))
      (while (not (eobp))
	(if (eolp)
	    nil ; skip blank lines
	  (indent-to (current-left-margin))
	  (justify-current-line t nil t))
	(forward-line 1)))))

;;;
;;; Encoding Files
;;;

;;;###autoload
(defun enriched-encode (from to &optional orig-buf)
  (if enriched-verbose (message "Enriched: encoding document..."))
  (save-restriction
    (narrow-to-region from to)
    (delete-to-left-margin)
    (unjustify-region)
    (goto-char from)
    (format-replace-strings '(("<" . "<<")))
    (format-insert-annotations 
     (format-annotate-region from (point-max) enriched-translations
			     'enriched-make-annotation enriched-ignore))
    (goto-char from)
    (insert (if (stringp enriched-initial-annotation)
		enriched-initial-annotation
	      (save-excursion
		;; Eval this in the buffer we are annotating.  This
		;; fixes a bug which was saving incorrect File-Width
		;; information, since we were looking at local
		;; variables in the wrong buffer.
		(if orig-buf (set-buffer orig-buf))
		(funcall enriched-initial-annotation))))
    (enriched-map-property-regions 'hard
      (lambda (v b e)
	(if (and v (= ?\n (char-after b)))
	    (progn (goto-char b) (insert "\n"))))
      (point) nil)
    (if enriched-verbose (message nil))
    ;; Return new end.
    (point-max)))

(defun enriched-make-annotation (name positive)
  "Format an annotation called NAME.
If POSITIVE is non-nil, this is the opening annotation, if nil, this is the
matching close."
  (cond ((stringp name)
	 (format enriched-annotation-format (if positive "" "/") name))
	;; Otherwise it is an annotation with parameters, represented as a list
	(positive
	 (let ((item (car name))
	       (params (cdr name)))
	   (concat (format enriched-annotation-format "" item)
		   (mapconcat (lambda (i) (concat "<param>" i "</param>"))
			      params ""))))
	(t (format enriched-annotation-format "/" (car name)))))

;; XEmacs addition
(defun enriched-face-strip-size (face)
  "Create a symbol from the name of FACE devoid of size information,
i.e. remove all larger- and smaller- prefixes."
  (let* ((face-symbol (face-name face))
	 (face-name (symbol-name face-symbol))
	 (old-name face-name)
	 new-name)
    (while
	(not (string-equal
	      old-name
	      (setq new-name (replace-in-string old-name "^larger-" ""))))
      (setq old-name new-name))
    
    (while
	(not (string-equal
	      old-name
	      (setq new-name (replace-in-string old-name "^smaller-" ""))))
      (setq old-name new-name))
    
    (if (string-equal new-name face-name)
	face-symbol
      (intern new-name))))

(defun enriched-encode-other-face (old new)
  "Generate annotations for random face change.
One annotation each for foreground color, background color, italic, etc."
  (cons (and old (enriched-face-ans old))
	(and new (enriched-face-ans new))))
	    
(defun enriched-face-ans (face)
  "Return annotations specifying FACE."
  ;; XEmacs change (entire body of this function)
  (let ((face-name (symbol-name face)))
    (cond ((string-match "^fg:" face-name)
	   (list (list "x-color" (substring face-name 3))))
	  ((string-match "^bg:" face-name)
	   (list (list "x-bg-color" (substring face-name 3))))
	  ((or (string-match "^larger-" face-name)
	       (string-match "^smaller-" face-name))
	   (cdr (format-annotate-single-property-change
		 'face nil (enriched-face-strip-size face)
		 enriched-translations)))
	  (t
	   (let* ((fg (and (not (eq (face-foreground face)
				    (face-foreground 'default)))
			   (color-name (face-foreground face))))
		  (bg (and (not (eq (face-background face)
				    (face-background 'default)))
			   (color-name (face-background face))))
		  (ans '()))
	     (if fg (setq ans (cons (list "x-color" fg) ans)))
	     (if bg (setq ans (cons (list "x-bg-color" bg) ans)))
	     ans)))))

;; XEmacs addition
(defun enriched-size-annotation (n annotation)
  "Generate ANNOTATION N times."
  (let ((l '()))
    (while (not (zerop n))
      (setq l (cons annotation l))
      (setq n (1- n)))
    l))

;; XEmacs addition
(defun enriched-encode-size (old new)
  "Return annotations specifying SIZE."
  (let* ((old (or old 0))
	 (new (or new 0))
	 (closing-annotation
	  (enriched-size-annotation (abs old)
				    (if (> old 0) "bigger" "smaller")))
	 (opening-annotation
	  (enriched-size-annotation (abs new)
				    (if (> new 0) "bigger" "smaller"))))
    (cons closing-annotation
	  opening-annotation)))

;;;
;;; Decoding files
;;;

;;;###autoload
(defun enriched-decode (from to)
  (if enriched-verbose (message "Enriched: decoding document..."))
  (use-hard-newlines 1 'never)
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)

      ;; Deal with header
      (let ((file-width (enriched-get-file-width)))
	(enriched-remove-header)

	;; Deal with newlines
	(while (search-forward-regexp "\n\n+" nil t)
	  (if (current-justification)
	      (delete-char -1))
	  (set-hard-newline-properties (match-beginning 0) (point)))

	;; Translate annotations
	(format-deannotate-region from (point-max) enriched-translations
				  'enriched-next-annotation)

	;; Indent or fill the buffer
	(cond (file-width		; File was filled to this width
	       (setq fill-column file-width)
	       (if enriched-verbose (message "Indenting..."))
	       (enriched-insert-indentation))
	      (t			; File was not filled.
	       (if enriched-verbose (message "Filling paragraphs..."))
	       (fill-region (point-min) (point-max))))
	(if enriched-verbose (message nil)))
      (point-max))))

(defun enriched-next-annotation ()
  "Find and return next text/enriched annotation.
Any \"<<\" strings encountered are converted to \"<\".
Return value is \(begin end name positive-p), or nil if none was found."
  (while (and (search-forward "<" nil 1)
	      (progn (goto-char (match-beginning 0))
		     (not (looking-at enriched-annotation-regexp))))
    (forward-char 1)
    (if (= ?< (char-after (point)))
	(delete-char 1)
      ;; A single < that does not start an annotation is an error,
      ;; which we note and then ignore.
      (message "Warning: malformed annotation in file at %s" 
	       (1- (point)))))
  (if (not (eobp))
      (let* ((beg (match-beginning 0))
	     (end (match-end 0))
	     (name (downcase (buffer-substring 
			      (match-beginning 2) (match-end 2))))
	     (pos (not (match-beginning 1))))
	(list beg end name pos))))

(defun enriched-get-file-width ()
  "Look for file width information on this line."
  (save-excursion
    (if (search-forward "Text-Width: " (+ (point) 1000) t)
	(read (current-buffer)))))

(defun enriched-remove-header ()
  "Remove file-format header at point."
  (while (looking-at "^[-A-Za-z]+: .*\n")
    (delete-region (point) (match-end 0)))
  (if (looking-at "^\n")
      (delete-char 1)))

(defun enriched-decode-foreground (from to &optional color)
  ;; XEmacs change
  (let ((face (facemenu-get-face (intern (concat "fg:" color)))))
    (if (not face)
	(progn
	  (make-face face)
	  (message "Warning: Color \"%s\" can't be displayed." color)))
    (list from to 'face face)))

(defun enriched-decode-background (from to &optional color)
  ;; XEmacs change
  (let ((face (facemenu-get-face (intern (concat "bg:" color)))))
    (if (not face)
	(progn
	  (make-face face)
	  (message "Warning: Color \"%s\" can't be displayed." color)))
    (list from to 'face face)))

;;; enriched.el ends here
