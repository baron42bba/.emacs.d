;;; rtf-support.el --- MS Rich Text Format support functions

;; Copyright (C) 2000 Alastair J. Houghton

;; Authors:    1999-2001 Alastair J. Houghton <ajhoughton@lineone.net>
;; Keywords:   RTF Microsoft Windows NT
;; Version:    1.4

;; This file is part of XEmacs

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

;; If you want to actually *use* RTF, you probably want to take a look
;; at the functions `rtf-clip-region', `rtf-clip-buffer',
;; `rtf-export-region' and `rtf-export-buffer' or if you're more interested
;; in how it all works, `rtf-spool-region' and `rtf-spool-buffer'.

;; Some people like to rebind their global keymap so that they can do
;; rtf-clip-region directly from the keyboard.  An example follows:
;;
;;   (require 'rtf-support)
;;
;;   (define-key global-map '(control meta insert) 'rtf-clip-region)
;;
;; Note that the rtf-clip functions do *not* place things into the
;; XEmacs kill ring, just onto the clipboard.  XEmacs makes it appear as
;; if things on the clipboard are at the top of the kill ring via the
;; `interprogram-paste-function'.  
;;
;; It's probably best to treat all of these functions the way you'd
;; treat `htmlize' or `ps-spool-*'; whilst they probably are quick enough
;; to replace kill/yank/copy for day-to-day use, they do perform a fair bit
;; of processing and could theoretically take a long time or use up a lot
;; of memory executing...

;; Requires
(require 'cl)

(defconst rtf-version "1.4"
  "RTF-support version number.")

;;; Customisation support     

;;;###autoload
(defgroup rtf nil
  "Support RTF selections and spooling of RTF to a buffer."
  :group 'wp
  :tag "RTF")

;; This says whether to untabify the text before changing it to RTF
(defcustom rtf-untabify-p t
  "Set this to t to untabify the text before changing it to RTF."
  :type 'boolean
  :group 'rtf)

(defcustom rtf-gen-ruler-p nil
  "Set this to t to generate a set of RTF tabstops.
This is intended for RTF readers that cannot handle character-based tabs
properly (e.g. Microsoft Word). Note that this feature uses on-screen
character metrics, so tabstops generated like this tend to be somewhat
inaccurate.  It is usually better to untabify the text by setting
`rtf-untabify-p' rather than using this feature."
  :type 'boolean
  :group 'rtf)

(defcustom rtf-create-colours nil
  "Non-nil causes RTF output to contain new colours.
This is for compatibility with RTF readers that do not expect anything
but the standard set of colours (e.g. Microsoft Word prior to Word 2000).
If new colours are not being created, face colours are best-matched with
those in the default colour table `rtf-default-colour-table' using a
Euclidean distance metric."
  :type 'boolean
  :group 'rtf)

;; This is the default colour table for the RTF output.  It is set-up the
;; same way as Microsoft Word's default colour table, which means that
;; the colours won't cause too much bother.
(defcustom rtf-default-colour-table '((nil           . 0);; Auto
				      ((0 0 0)       . 1);; Black
				      ((0 0 255)     . 2);; Blue
				      ((0 255 255)   . 3);; Cyan
				      ((0 255 0)     . 4);; Green
				      ((255 0 255)   . 5);; Magenta
				      ((255 0 0)     . 6);; Red
				      ((255 255 0)   . 7);; Yellow
				      ((255 255 255) . 8);; White
				      ((0 0 128)     . 9);; Dark Blue
				      ((0 128 128)   . 10);; Dark Cyan
				      ((0 128 0)     . 11);; Dark Green
				      ((128 0 128)   . 12);; Dark Magenta
				      ((128 0 0)     . 13);; Dark Red
				      ((128 128 0)   . 14);; Dark Yellow
				      ((128 128 128) . 15);; Grey
				      ((192 192 192) . 16));; Light Grey
  "The default colour table to use in RTF output.
This is the palette of colours that is used when rtf-create-colours is nil;
by default it matches the standard set used in Microsoft Word.

It is only used when `rtf-create-colours' is nil, in which case Emacs face
colours are matched with available palette colours by minimising the
Euclidean distance between the selected palette colour and the face colour.

Ideally, the indices should be unique, although that isn't enforced here
because it's possible that someone might find a use for non-unique indices
(in conjunction with the various broken programs that are about)."
  :type '(repeat rtf-color-table-entry)
  :group 'rtf)

(define-widget 'rtf-color-table-entry 'default
  "Edit an RTF colour table entry."
  :format "%v"
  :value '(nil . 0)
  :value-create 'rtf-color-table-value-create
  :value-delete 'widget-children-value-delete
  :value-get 'rtf-color-table-value-get
  :value-set 'rtf-color-table-value-set
  :match '(lambda (widget value) t)
  :validate 'widget-children-validate
  :convert-widget 'widget-value-convert-widget)

(defun rtf-color-table-value-create (widget)
  "Create the components of an rtf-color-table-entry widget."
  (let ((value (widget-get widget :value))
	index color)
    (setq index (widget-create-child-value widget
					   '(integer
					     :tag "Index"
					     :size 6)
					   (cdr value)))
    (insert ?\ )
    (setq color (widget-create-child-value widget
					   '(rtf-color
					     :tag "Color")
					   (car value)))
    (insert ?\n)
    (widget-put widget :children (list index color))
    ))

(defun rtf-color-table-value-get (widget)
  "Retrieve the value of an rtf-color-table-entry widget."
  (let ((children (widget-get widget :children)))
    (if children
	(cons (widget-value (cadr children))
	      (widget-value (car children)))
      (widget-get widget :value))))

(defun rtf-color-table-value-set (widget value)
  "Set the value of an rtf-color-table-entry widget."
  (let ((children (widget-get widget :children)))
    (if children
	(progn
	  (widget-value-set (car children) (cdr value))
	  (widget-value-set (cadr children) (car value))))))

(define-widget 'rtf-color 'editable-field
  "Choose a color, either (R G B) or auto (with sample)."
  :format "%{%t%}: (%{  %}) %v"
  :size 15
  :tag "Color"
  :value nil
  :sample-face-get 'rtf-color-sample-face-get
  :notify 'rtf-color-notify
  :action 'rtf-color-action
  :error "Must be an RGB triple (R G B), or auto."
  :validate 'rtf-color-validate
  :match 'rtf-color-match
  :value-to-internal 'rtf-color-value-to-internal
  :value-to-external 'rtf-color-value-to-external)

(defun rtf-color-as-string (color)
  "Get a color as a string."
  (if (and color
	   (not (eq color 'rtf-invalid-color)))
      (format "#%2.2X%2.2X%2.2X" (car color) (cadr color) (caddr color))
    "#000000"))

(defun rtf-color-sample-face-get (widget)
  "Retrieve the sample face."
  (or (widget-get widget :sample-face)
      (let ((color (widget-value widget))
	    (face (make-face (gensym "sample-face-") nil t)))
	(widget-put widget :sample-face face)
	(if (rtf-color-match widget color)
	    (set-face-background face (rtf-color-as-string color))
	  (set-face-background face "#000000"))
	face)))

(defun rtf-color-action (widget &optional event)
  "Prompt for a colour."
  (let* ((tag (widget-apply widget :menu-tag-get))
	 (answer (read-string (concat tag ": ")
			      (rtf-color-value-to-internal
			       widget
			       (widget-value widget)))))
    (unless (zerop (length answer))
      (widget-value-set widget (rtf-color-value-to-external widget answer))
      (widget-setup)
      (widget-apply widget :notify widget event))))

(defun rtf-color-notify (widget child &optional event)
  "Update the sample, and notify the parent."
  (let* ((face (widget-apply widget :sample-face-get))
	 (color (widget-value widget)))
    (if (rtf-color-match widget color)
	(set-face-background face (rtf-color-as-string color))
      (set-face-background face "#000000"))
    (widget-default-notify widget child event)))

(defun rtf-color-validate (widget)
  "Validate this widget's value."
  (let ((color (widget-value widget)))
    (unless (rtf-color-match widget color)
      widget)))

(defun rtf-color-match (widget value)
  "Validate this value."
  (and (not (eq value 'rtf-invalid-color))
       (or (not value)
	   (and (listp value)
		(eq (length value) 3)))))

(defun rtf-color-value-to-internal (widget value)
  "Convert to internal representation (string)."
  (cond
   ((eq value 'rtf-invalid-color)
    "auto")
   (value
    (format "(%d %d %d)" (car value) (cadr value) (caddr value)))
   (t
    "auto")))

(defun rtf-color-value-to-external (widget value)
  "Convert to external representation."
  (unless (equal value "auto")
    (let ((val (condition-case nil
		   (read value)
		 (error nil))))
      (if (and (listp val)
	       (every '(lambda (x)
			 (and (integerp x)
			      (<= x 255)
			      (>= x 0)))
		      val))
	  val
	'rtf-invalid-color))
    ))

;;; Code proper:

;; This is the clipboard format ID
(defvar rtf-data-type nil
  "Contains the window-system data type for RTF.")

(unless rtf-data-type
  (setq rtf-data-type (register-selection-data-type "Rich Text Format")))

;; This function makes a string safe for inclusion in an RTF file
(defun rtf-safe (string)
  "Return a valid RTF string with the textual meaning of `string'.
This function makes various special characters safe by escaping them."
  (replace-in-string string "[{}\\\\]" "\\\\\\&" nil))

(defun rtf-map-chars (string)
  "Map some characters in an RTF string."
;; RAS:  07/11/03
;; adding support for form feed
  (replace-in-string (replace-in-string (replace-in-string (rtf-safe string) 
                                                           "" "\\page " t) 
                                        "\t" "\\tab" t)
		     "\n" "\\par\n" t)) 

(defun rtf-map-colour (emacs-colour)
  "Convert an Emacs colour triple to a more suitable form for RTF."
  (list
   (max (min (/ (nth 0 emacs-colour) 256) 255) 0)
   (max (min (/ (nth 1 emacs-colour) 256) 255) 0)
   (max (min (/ (nth 2 emacs-colour) 256) 255) 0)))

(defun rtf-match-colour (colour colours best-match)
  "Find a colour in the colours list.
If `best-match' is non-nil, it matches the closest colour, otherwise
it performs an exact match."
  (if best-match
      (let (current
	    curcol
	    (bestcol nil)
	    (bestdist nil)
	    dist)
	;; Remember to skip the "auto" colour
	(setq current (cdr colours))
	(while current
	  (setq curcol (car current))
	  (setq current (cdr current))
	  (let ((rd (- (nth 0 colour) (nth 0 (car curcol))))
		(gd (- (nth 1 colour) (nth 1 (car curcol))))
		(bd (- (nth 2 colour) (nth 2 (car curcol)))))
	    (setq dist (sqrt (+ (* rd rd) (* gd gd) (* bd bd)))))
	  (if (or (not bestdist) (< dist bestdist))
	      (progn
		(setq bestdist dist)
		(setq bestcol (cdr curcol)))))
	bestcol)
    (cdr (assoc colour colours))))

;; This function generates a ruler
(defun rtf-ruler (tab-twips nstops)
  "Generate the RTF for a set of tab-stops, starting at the left margin,
separated by `tab-twips' twips, with `nstops' stops."
  (let ((result nil)
	(stops nstops)
	(pos 0))
    (while (> stops 0)
      (setq stops (- stops 1))
      (setq pos (truncate (+ pos tab-twips)))
      (setq result (concat result "\\tx" (number-to-string pos))))
    result)
  )

;; This function shamelessly based upon Hrvoje's htmlize-faces-in-buffer
;; from htmlize.el
(defun rtf-faces-in-buffer ()
  "Return a list of the faces actually used by extents in the current buffer."
  (let (faces)
    (map-extents (lambda (extent ignored)
		   (let ((face (extent-face extent)))
		     (when (consp face)
		       (setq face (car face)))
		     (when (find-face face)
		       (pushnew face faces)))
		   nil)
		 nil nil nil nil nil 'face)
    (pushnew 'default faces)))


;; This function takes a region and generates RTF in the specified buffer

;;;###autoload
(defun rtf-spool-region (start end &optional dont-fontify)
  "Spool a buffer as Microsoft Rich Text Format text.
Like `ps-spool-region', although the rtf-support code doesn't keep
track of spooled regions to despool (because RTF isn't useful for
printing). Returns the buffer containing the RTF."
  (interactive "r")
  (when (and font-lock-mode (not dont-fontify))
    (font-lock-fontify-buffer))
  
  ;; Swap if necessary
  (if (< end start)
      (let ((tmp start))
	(setq start end)
	(setq end tmp)))
  
  ;; Create the new buffer
  (let ((rtf-buf (generate-new-buffer "*rtf*"))
	(tmp-buf nil)
	old-buf)
    (save-excursion
      (message "rtf-spool-region: building header...")
      
      ;; Build the RTF header first
      (insert-string "{\\rtf1\\ansi" rtf-buf)
      
      ;; Build the font table, colour table and stylesheet
      (let ((fonts nil)
	    (colours rtf-default-colour-table)
	    (styles nil)
	    (fnum 0) (cnum 16) (snum 0)
	    (font nil) (forecolour nil) (backcolour nil)
	    (style nil) (extstyle nil)
	    (faces-list (rtf-faces-in-buffer))
	    (style-map (make-hash-table :test 'equal))
	    (tab-twips 720)
	    (style-start nil))
	
	;; Enumerate the faces, breaking out lists
	(mapc '(lambda (cur-face)
		 ;; Extract font information
		 (setq font (font-name (face-property cur-face 'font)))
		 (let ((type (console-type)))
		   (cond
		    ((eq type 'x)
		     (setq font (split-string font "-"))
		     (setq font (list (nth 2 font)
				      (nth 3 font)
				      (nth 4 font)
				      (cond
				       ((eq (nth 8 font) "*") 10)
				       ((> (length (nth 8 font)) 0)
					(/ (string-to-number (nth 8 font)) 10))
				       (t 10)))))
		    (t
		     (setq font (split-string font ":"))
		     (setq font (list (nth 0 font)
				      (nth 1 font)
				      (nth 3 font)
				      (if (> (length (nth 2 font)) 0)
					  (string-to-number (nth 2 font))
					10))))))
		   
		 ;; Make a new font if necessary
		 (unless (assoc (nth 0 font) fonts)
		   (setq fnum (+ fnum 1))
		   (setq fonts (append fonts (list (cons
						    (nth 0 font) fnum)))))

		 ;; Make new colours if necessary
		 (setq forecolour (rtf-map-colour
				   (color-rgb-components
				    (face-property cur-face 'foreground))))
		 (setq backcolour (rtf-map-colour
				   (color-rgb-components
				    (face-property cur-face 'background))))

		 (if rtf-create-colours
		     (progn
		       (unless (assoc forecolour colours)
			 (setq cnum (+ cnum 1))
			 (setq colours (append colours
					       (list
						(cons forecolour cnum)))))
		
		       (unless (assoc backcolour colours)
			 (setq cnum (+ cnum 1))
			 (setq colours (append colours
					       (list
						(cons backcolour cnum)))))
		       ))
	  
		 ;; Sort-out bold, underlined, etc...
		 (setq extstyle nil)
		 (let ((font-type (nth 1 font)))
		   (if (string-match "Bold" font-type)
		       (setq extstyle (concat "\\b" extstyle)))
		   (if (string-match "Italic" font-type)
		       (setq extstyle (concat "\\i" extstyle)))
		   (if (face-underline-p cur-face)
		       (setq extstyle (concat "\\ul" extstyle)))
		   )
	     
		 ;; Make a new style for this face
		 (setq style (list (format "(Emacs) %s" cur-face)
				   (cdr (assoc (nth 0 font) fonts))
				   (nth 3 font)
				   (rtf-match-colour forecolour colours
						     (not rtf-create-colours))
				   (rtf-match-colour backcolour colours
						     (not rtf-create-colours))
				   extstyle
				   cur-face))

		 ;; If this was the default face, work-out how big a tab is
		 (unless rtf-untabify-p
		   (if (equal (nth 0 style) "(Emacs) default")
		       (let ((fwidth (font-instance-width
				      (specifier-instance
				       (face-property cur-face 'font)))))
			 (setq tab-twips (* (* fwidth 15) tab-width))
			 )))

		 (setq snum (+ snum 1))
		 (setq styles (append styles (list (cons style snum)))))
	      faces-list)

	;; OK - emit the font table
	(insert-string "{\\fonttbl" rtf-buf)

	(mapc '(lambda (font)
		 (insert-string (concat "\\f" (number-to-string (cdr font))
					"\\fmodern " (car font) ";") rtf-buf))
	      fonts)

	;; Now emit the colour table
	(insert-string "}\n{\\colortbl;" rtf-buf)

	(mapc '(lambda (colour)
		 (if (car colour)
		     (insert-string (concat
				     "\\red" (number-to-string
					      (first (car colour)))
				     "\\green" (number-to-string
						(second (car colour)))
				     "\\blue" (number-to-string
					       (third (car colour)))
				     ";") rtf-buf)))
	      colours)
			   
	;; Finally do the stylesheet
	(insert-string "}\n{\\stylesheet" rtf-buf)
	(setq style-start (point rtf-buf))
	
	(mapc
	 '(lambda (style)
	    (if (equal (first (car style)) "(Emacs) default")
		(let ((cur-pos (point rtf-buf)))
		  (goto-char style-start rtf-buf)
		  (insert-string (concat
				  "{\\s15"
				  "\\plain\\f" (number-to-string
						(second (car style)))
				  "\\fs" (number-to-string
					  (* (third (car style)) 2))
				  "\\cf" (number-to-string (fourth (car style)))
				  "\\cb" (number-to-string (fifth (car style)))
				  "\\lang1024"
				  (sixth (car style))
				  (when rtf-gen-ruler-p
				    (rtf-ruler tab-twips 30))
				  " Emacs Text;}"
				  "{\\*\\cs16"
				  " \\additive"
				  "\\f" (number-to-string
					 (second (car style)))
				  "\\fs" (number-to-string
					  (* (third (car style)) 2))
				  "\\cf" (number-to-string (fourth (car style)))
				  "\\cb" (number-to-string (fifth (car style)))
				  "\\lang1024"
				  (sixth (car style))
				  " Emacs Base Style;}") rtf-buf)
		  (goto-char (+ cur-pos (- (point rtf-buf) style-start)) rtf-buf)
		  (puthash nil (concat
				"\\cs16"
				"\\f" (number-to-string (second (car style)))
				"\\fs" (number-to-string (* (third
							     (car style)) 2))
				"\\cf" (number-to-string (fourth (car style)))
				"\\cb" (number-to-string (fifth (car style)))
				"\\lang1024" (sixth (car style)))
			   style-map))
	      (insert-string (concat
			      "{\\*\\cs" (number-to-string (+ 16 (cdr style)))
			      " \\additive"
			      "\\f" (number-to-string (second (car style)))
			      "\\fs" (number-to-string (* (third (car style)) 2))
			      "\\cf" (number-to-string (fourth (car style)))
			      "\\cb" (number-to-string (fifth (car style)))
			      "\\lang1024"
			      (sixth (car style))
			      " \\sbasedon16 "
			      (first (car style))
			      ";}") rtf-buf)
	      (puthash (nth 6 (car style))
		       (concat
			"\\cs" (number-to-string (+ 16 (cdr style)))
			"\\f" (number-to-string (second (car style)))
			"\\fs" (number-to-string (* (third (car style)) 2))
			"\\cf" (number-to-string (fourth (car style)))
			"\\cb" (number-to-string (fifth (car style)))
			"\\lang1024"
			(sixth (car style)))
		       style-map)))
	 styles)
	
	;; End the header
	(insert-string (concat "}\n{\\plain\\s15"
			       (when rtf-gen-ruler-p
				 (rtf-ruler tab-twips 30))
			       "{\\cs16"
			       (gethash nil style-map)) rtf-buf)

	;; Go through all the extents writing out the text and the style changes
	(setq old-buf (current-buffer))
	
	(if rtf-untabify-p
	    (progn
	      (setq tmp-buf (generate-new-buffer "*rtf-tmp*"))
	      (let ((old-tab-width tab-width))
		(set-buffer tmp-buf)
		(setq tab-width old-tab-width)
		(insert-buffer old-buf)
		(map-extents '(lambda (extent arg)
				(let ((new-extent (copy-extent extent)))
				  (set-extent-property new-extent 'read-only nil)
				  (insert-extent new-extent
						 (extent-start-position extent)
						 (extent-end-position extent)))
				nil)
			     old-buf)
		(let ((start-mark (make-marker))
		      (end-mark (make-marker)))
		  (set-marker start-mark start)
		  (set-marker end-mark end)
		  (untabify (point-min) (point-max))
		  (setq start (marker-position start-mark))
		  (setq end (marker-position end-mark))
		  ))))

	(let ((pos start)
	      (percent 5))
	  (while (< pos end)
	    (let ((next-change
		   (or (next-single-property-change pos 'face)
		       end))
		  (formatting (gethash (get-text-property pos 'face)
				       style-map)))
	      (if formatting
		  (insert-string (concat
				  "{" formatting
				  " "
				  (rtf-map-chars (buffer-substring-no-properties
						  pos next-change))
				  "}")
				 rtf-buf)
		(insert-string (rtf-map-chars (buffer-substring-no-properties
					       pos next-change))
			       rtf-buf))
	      
	      (setq pos next-change)
	      
	      (let ((real-percent (/ (* 100 (- pos start)) (- end start))))
		(when (> real-percent percent)
		  (progn
		    (message "rtf-spool-region: %d%% ..." percent)
		    (setq percent (- (+ 5 real-percent) (mod real-percent 5))))))
	      )))

	(set-buffer old-buf)
	
	;; Delete temporary buffer
	(if rtf-untabify-p
	    (kill-buffer tmp-buf))
	
	;; End the file
	(insert-string "\\par\n}}}" rtf-buf)

	(message "rtf-spool-region: done")
	))
    rtf-buf
    ))

;;;###autoload
(defun rtf-spool-buffer ()
  "Spool the entire buffer."
  (interactive)
  (rtf-spool-region 1 (buffer-size)))

;;; Functions users are most likely to use

;;;###autoload
(defun rtf-export (filename)
  "Export the current document as RTF, preserving faces."
  (interactive "FExport RTF: ")
  (let ((rtf-buf (rtf-spool-buffer)))
    (save-excursion
      (set-buffer rtf-buf)
      (write-file filename t))
    (kill-buffer rtf-buf)))

;;;###autoload
(defun rtf-export-region (filename start end)
  "Export the selected region as RTF, preserving faces."
  (interactive "FExport RTF: \nr")
  (let ((rtf-buf (rtf-spool-region start end)))
    (save-excursion
      (set-buffer rtf-buf)
      (write-file filename t))
    (kill-buffer rtf-buf)))

;; The selection converter function (we only support *output* for now)
(defun rtf-convert-to-rtf (selection type value)
  "Convert VALUE to RTF, where VALUE is one of:

* A string. The string is converted to RTF. Non-duplicable extents will
  not be converted to RTF style changes.

* A buffer. The buffer's contents are converted to RTF.

* A list of the form (BUFFER START END), where BUFFER is the buffer from
  which to convert, START and END define a region within the buffer to
  be converted."
  (cond ((stringp value)
	 (let ((tmp-buf (generate-new-buffer "*rtf-tmp*")))
	   (insert-string value tmp-buf)
	   (rtf-convert-to-rtf selection type tmp-buf)))
	((bufferp value)
	 (let* ((rtf-buf (rtf-spool-buffer))
		(text (buffer-substring-no-properties
		       (point-min rtf-buf)
		       (point-max rtf-buf)
		       rtf-buf)))
	   (kill-buffer rtf-buf)
	   text))
	((and (listp value)
	      (bufferp (first value))
	      (integer-or-marker-p (second value))
	      (integer-or-marker-p (third value)))
	 (let* ((start (if (integerp (second value))
			   (second value)
			 (marker-position (second value))))
		(end (if (integerp (third value))
			 (third value)
		       (marker-position (third value))))
		(rtf-buf (rtf-spool-region start end t))
		(text (buffer-substring-no-properties
		       (point-min rtf-buf) (point-max rtf-buf) rtf-buf)))
	   (kill-buffer rtf-buf)
	   text))
	(t nil)
	))

;; Install the converter
(unless (assq rtf-data-type selection-converter-out-alist)
  (setq selection-converter-out-alist
	(cons (cons rtf-data-type 'rtf-convert-to-rtf)
	      selection-converter-out-alist)))
  
;;;###autoload
(defun rtf-clip-buffer ()
  "Send the entire buffer to the clipboard as Rich Text Format. The function
also copies the buffer as ordinary text, just for consistency."
  (interactive)
  (let ((text (buffer-substring-no-properties)))
    (own-selection text 'CLIPBOARD)
    (own-selection (current-buffer) 'CLIPBOARD 'replace-existing
		   rtf-data-type)))

;;;###autoload
(defun rtf-clip-region (start end)
  "Send the specified region (the selection if called interactively) to the
clipboard as Rich Text Format. The function also copies the region in ordinary
text, just for consistency."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (own-selection text 'CLIPBOARD)
    (own-selection (list (current-buffer) start end) 'CLIPBOARD
		   'replace-existing rtf-data-type)))

;;; Provides RTF support

(provide 'rtf-support)
