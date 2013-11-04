;; live-icon.el --- make frame icons represent the current frame contents

;; Copyright (C) 1995 Rich Williams <rdw@hplb.hpl.hp.com>
;; Copyright (C) 1995 Jamie Zawinski <jwz@jwz.org>

;; Authors: Rich Williams <rdw@hplb.hpl.hp.com>
;;          Jamie Zawinski <jwz@jwz.org>

;; Minor cleanups and conversion from obsolete functions by
;; Karl M. Hegbloom <karlheg@inetarena.com>

;; Version 1.3


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

;; Generates little pixmaps representing the contents of your frames.

(defun live-icon-alloc-colour (cmv colour)
  "Allocate a colour and a char from the magic vector"
  (let ((bob (assoc colour (aref cmv 0)))
	(jim (aref cmv 2)))
    (if bob
	(cdr bob)
      (aset cmv 0 (cons (cons colour jim) (aref cmv 0)))
      (aset cmv 1 (1+ (aref cmv 1)))
      (aset cmv 2 (1+ jim))
      jim)))

(defun live-icon-from-frame (&optional frame)
  "Calculates the live-icon XPM of FRAME."
  (if (not frame)
      (setq frame (selected-frame)))
  (save-excursion
    (select-frame frame)
    (let* ((w (frame-width))
	   (h (frame-height))
	   (pix (make-vector h nil))
	   (ny 0)
	   (cmv (vector nil 0 ?A))
	   (d (live-icon-alloc-colour
	       cmv (color-name (face-background 'default))))
	   (m (live-icon-alloc-colour
	       cmv (color-name (face-background 'modeline))))
	   (x (live-icon-alloc-colour
	       cmv (color-name (face-foreground 'default))))
	   y)
      (let ((loop 0))
	(while (< loop h)
	  (aset pix loop (make-string w d))
	  (setq loop (1+ loop))))
      (mapcar #'(lambda (win)
		      (save-excursion
			(save-window-excursion
			  (select-window win)
			  (save-restriction
			    (setq y ny
				  ny (+ ny (1- (window-height))))
			    (aset pix (- ny 2) (make-string w m))
			    (widen)
			    (if (> (window-end) (window-start))
				(narrow-to-region (window-start)
						  (1- (window-end))))
			    (goto-char (point-min))
			    (while (and (not (eobp))
					(< y (1- ny)))
			      (while (and (not (eolp))
					  (< (current-column) w))
				(if (> (char-after (point)) 32)
				    (let* ((ex (extent-at (point) (current-buffer) 'face))
					   (f (if ex (let ((f (extent-face ex)))
						       (if (not (consp f))
							   f
							 (car f)))))
					   (z (if f (color-name (face-foreground f))))
					   (c (if z (live-icon-alloc-colour cmv z) x)))
				      (aset (aref pix y) (current-column) c)))
				(forward-char 1))
			      (setq y (1+ y))
			      (forward-line 1))))))
	      (sort (if (fboundp 'window-list)
			(window-list)
		      (let* ((w (frame-root-window))
			     (ws nil))
			(while (not (memq (setq w (next-window w)) ws))
			  (setq ws (cons w ws)))
			ws))
			#'(lambda (won woo)
			    (< (nth 1 (window-pixel-edges won))
			       (nth 1 (window-pixel-edges woo))))))
      (concat "/* XPM */\nstatic char icon[] = {\n" 
	      (format "\"%d %d %d 1\",\n" w (* h 2) (aref cmv 1))
	      (mapconcat #'(lambda (colour-entry)
			   (format "\"%c c %s\"" 
				   (cdr colour-entry) 
				   (car colour-entry)))
			 (aref cmv 0)
			 ",\n")
	      ",\n"
	      (mapconcat #'(lambda (scan-line)
			   (concat "\"" scan-line "\"," "\n"
				   "\"" (make-string w d) "\","
				   ))
			 pix
			 ",\n")
	      "};\n"))))

(defun live-icon-one-frame (&optional frame)
  "Gives FRAME (defaulting to (selected-frame)) a live icon."
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  (unless (frame-property frame 'balloon-help)
    (set-glyph-image frame-icon-glyph (live-icon-from-frame frame) frame)))

;;(defun live-icon-all-frames ()
;;  "Gives all your frames live-icons."
;;  (interactive)
;;  (mapcar #'(lambda (fr)
;;	      (set-glyph-image frame-icon-glyph
;;			       (live-icon-from-frame fr)
;;			       fr))
;;	  (frame-list)))

(add-hook 'unmap-frame-hook 'live-icon-one-frame)
;;(start-itimer "live-icon" 'live-icon-all-frames 120 120)

(provide 'live-icon)
;;; live-icon.el ends here



;;;; Spare parts and leftovers department:

;; #### This thing is somewhat of a mess and could stand some clean-up.

;;(defun live-icon-colour-name-from-face (face &optional bg-p)
;;  "Do backward compatible things to faces and colours"
;;  (if (and (boundp 'emacs-major-version)
;;	   (or (> emacs-major-version 19)
;;	       (and (= emacs-major-version 19)
;;		    (>= emacs-minor-version 12))))
;;      (let* ((face (if (consp face) (car face) face))
;;	     (colour (if bg-p
;;			 (face-background face)
;;		       (face-foreground face))))
;;	(if (consp colour)
;;	    (setq colour (cdr (car colour))))
;;	(if (color-instance-p colour)
;;	    (setq colour (color-instance-name colour)))
;;	(if (specifierp colour)
;;	    (setq colour (color-name colour)))
;;	(if colour
;;	    (let ((hack (format "%s" colour)))
;;	      (if (string-match "(?\\([^)]*\\))?" hack)
;;		  (substring hack (match-beginning 1) (match-end 1))
;;		hack))))
;;    (let ((p (if bg-p (face-background face) (face-foreground face))))
;;      (and (pixelp p)
;;	   ;; ** The following functions are not known to be defined:  pixelp
;;	   (pixel-name p)))))
;;;;  ** pixel-name is an obsolete function; use color-name instead.

;;(defun live-icon-start-ppm-stuff (&optional frame)
;;  "Start a live icon conversion going"
;;  (interactive)
;;  (if (not frame)
;;      (setq frame (selected-frame)))
;;  (let ((buf (get-buffer-create " *live-icon*")))
;;    (message "live-icon...(backgrounding)")
;;    (save-excursion
;;      (set-buffer buf)
;;      (erase-buffer))
;;    (set-process-sentinel
;;     (start-process-shell-command "live-icon"
;;				  buf
;;				  "xwd"
;;				  "-id" (format "%s" (x-window-id frame)) "|"
;;				  "xwdtopnm" "|" 
;;				  "pnmscale" "-xysize" "64" "64" "|"
;;				  "ppmquant" "256" "|"
;;				  "ppmtoxpm")
;;     #'(lambda (p s)
;;	 (message "live-icon...(munching)")
;;	 (save-excursion
;;	   (set-buffer " *live-icon*")
;;	   (goto-char (point-min))
;;	   (search-forward "/* XPM */")
;;	   (set-glyph-image frame-icon-glyph
;;			    (buffer-substring (match-beginning 0) (point-max))
;;			    frame))
;;	 (message "live-icon...... done"))))
;;  nil)

;;(defun live-icon-goto-position (x y)
;;  (let (window edges)
;;    (catch 'done
;;      (walk-windows
;;       #'(lambda (w)
;;	   (setq edges (window-edges w))
;;	   (if (and (>= x (nth 0 edges))
;;		    (<= x (nth 2 edges))
;;		    (>= y (nth 1 edges))
;;		    (<= y (nth 3 edges)))
;;	       (throw 'done (setq window w))))
;;       nil t))
;;    (if (not window)
;;	nil
;;      (select-window window)
;;      (move-to-window-line (- y (nth 1 edges)))
;;      (move-to-column (- x (nth 0 edges)))
;;      )))

;;(defun live-icon-make-image (width height)
;;  (let* ((text-aspect 1.5)
;;	 (xscale (/ (/ (* (frame-width)  1.0) width) text-aspect))
;;	 (yscale (/ (* (frame-height) 1.0) height))
;;	 (x 0)
;;	 (y 0)
;;	 (cmv (vector nil 0 ?A))
;;	 (default-fg (live-icon-alloc-colour
;;		      cmv (color-name (face-foreground 'default))))
;;	 (default-bg (live-icon-alloc-colour
;;		      cmv (color-name (face-background 'default))))
;;	 (modeline-bg (live-icon-alloc-colour
;;		       cmv (color-name (face-background 'modeline))))
;;	 (lines (make-vector height nil)))
;;    ;;
;;    ;; Put in the text.
;;    ;;
;;    (save-excursion
;;      (save-window-excursion
;;	(while (< y height)
;;	  (aset lines y (make-string width default-bg))
;;	  (setq x 0)
;;	  (while (< x width)
;;	    (let ((sx (floor (* x xscale)))
;;		  (sy (floor (* y yscale))))
;;	      (live-icon-goto-position sx sy)
;;	      (let* ((extent (extent-at (point) (current-buffer) 'face))
;;		     (face (if extent (extent-face extent)))
;;		     (name (if face (live-icon-colour-name-from-face
;;				     face (<= (char-after (point)) 32))))
;;		     (color (if name
;;				(live-icon-alloc-colour cmv name)
;;			      (if (<= (or (char-after (point)) 0) 32)
;;				  default-bg default-fg))))
;;		(aset (aref lines y) x color)))
;;	    (setq x (1+ x)))
;;	  (setq y (1+ y)))))
;;    ;;
;;    ;; Now put in the modelines.
;;    ;;
;;    (let (sx sy)
;;      (walk-windows
;;       #'(lambda (w)
;;	   (let ((edges (window-edges w)))
;;	     (setq x (nth 0 edges)
;;		   y (nth 3 edges)
;;		   sx (floor (/ x xscale))
;;		   sy (floor (/ y yscale)))
;;	     (while (and (< x (1- (nth 2 edges)))
;;			 (< sx (length (aref lines 0))))
;;	       (aset (aref lines sy) sx modeline-bg)
;;	       (if (> sy 0)
;;		   (aset (aref lines (1- sy)) sx modeline-bg))
;;	       (setq x (1+ x)
;;		     sx (floor (/ x xscale))))
;;	     (if (>= sx (length (aref lines 0)))
;;		 (setq sx (1- sx)))
;;	     (while (>= y (nth 1 edges))
;;	       (aset (aref lines sy) sx modeline-bg)
;;	       (setq y (1- y)
;;		     sy (floor (/ y yscale))))))
;;       nil nil))
;;    ;;
;;    ;; Now put in the top and left edges
;;    ;;
;;    (setq x 0)
;;    (while (< x width)
;;      (aset (aref lines 0) x modeline-bg)
;;      (setq x (1+ x)))
;;    (setq y 0)
;;    (while (< y height)
;;      (aset (aref lines y) 0 modeline-bg)
;;      (setq y (1+ y)))
;;    ;;
;;    ;; Now make the XPM
;;    ;;
;;    (concat "/* XPM */\nstatic char icon[] = {\n" 
;;	    (format "\"%d %d %d 1\",\n"
;;		    width
;;;;		    (* height 2)
;;		    height
;;		    (aref cmv 1))
;;	    (mapconcat #'(lambda (colour-entry)
;;			   (format "\"%c c %s\""
;;				   (cdr colour-entry) 
;;				   (car colour-entry)))
;;		       (aref cmv 0)
;;		       ",\n")
;;	    ",\n"
;;	    (mapconcat #'(lambda (scan-line)
;;			   (concat "\"" scan-line "\"," "\n"
;;;;				   "\"" scan-line "\""
;;;;				   "\"" (make-string width default-bg)
;;;;				   "\","
;;				   ))
;;		       lines
;;		       ",\n")
;;	    "};\n")))
