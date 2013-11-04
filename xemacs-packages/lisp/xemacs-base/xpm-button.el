;;; xpm-button.el --- create XPM buttons

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995 Kyle E. Jones

;; Author: Kyle Jones
;; Maintainer: XEmacs Development Team
;; Keywords: frames, internal

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Create XPM text buttons under XEmacs (requires 19.12 or beyond)

;; Send bug reports to kyle@wonderworks.com

;; Sample use of this package.
;;(progn
;;  (setq b (xpm-button-create "Test button" 2 "yellow" "#a0a0d0"))
;;  (setq e (make-extent (point-max) (point-max)))
;;  (setq g (make-glyph (nth 0 b)))
;;  (set-extent-begin-glyph e g)
;;  (setq g2 (make-glyph (nth 1 b)))
;;  (setq k (make-sparse-keymap))
;;  (define-key k 'button1 '(lambda (ev)
;;                          (interactive "e")
;;                          (set-extent-begin-glyph e g2)))
;;  (define-key k 'button1up '(lambda (ev)
;;                            (interactive "e")
;;                            (set-extent-begin-glyph e g)))
;;  (set-extent-property e 'keymap k))

;; The sole interface function is xpm-button-create.

;;; Code:

(provide 'xpm-button)

(defvar xpm-button-version "1.02"
  "Version string for xpm-button.")

(defvar xpm-button-vertical-padding 3
  "Number of pixels between the text and the top/bottom of the button.")

(defvar xpm-button-horizontal-padding 3
  "Number of pixels between the text and the left/right edges of the button.")

(defvar xpm-button-font-pixel-lines
  '(
"    xx     xxxxxx     xxxx x xxxxxxx   xxxxxxxx xxxxxxxx   xxxx x  xxxx  xxxx xxxx    xxxx xxxx xxxx xxxx     xxxx   xxxx xxx    xxx   xxxxx   xxxxxxx    xxxxx    xxxxxxx    xxxx x xxxxxxxx xxxx  xxx xxxx   xxx xxxx xxxx xxx xxxx xxxx xxxx xxx xxxxxxxx         xxx                 xxx           xxx         xxx       xx   xx xxx      xxx                                                                 x                                                            xx     x   xxxx   xxxx    xxx   xxxxx   xxx  xxxxxx  xxxx   xxxx   x        xx     xxxx      x x   xxxx    xx   x       x      xxx       x      x x                                  xxxx   xx xxxx xx         xx x x                       x  xxxx  x     x                                                                                         "
"    xx      xx  xx   xx   xx  xx   xx   xx   xx  xx   xx  xx   xx   xx    xx   xx      xx   xx   x    xx       xxx   xxx   xxx    x   xx   xx   xx   xx  xx   xx    xx   xx  xx   xx xx xx xx  xx    x   xx     x   xx   xx   x   xx   x    xx   x  xx   xxx          xx                  xx          xx x          xx       xx   xx  xx       xx                                                                xx                                                           x  x  xxx  xx  xx xx  xx   xxx   xxxx   x  xx xxxxxx xx  xx xx  xx x         xx   xx    x     x x  xx xxx  xx xxxx       x     xx  x    x x x   x   x                                 xx    xx    xx  xx        xx x x                       x xx  xx xx    x                                                                                         "
"   x xx     xx  xx  xx     x  xx    xx  xx    x  xx    x xx     x   xx    xx   xx      xx   xx  x     xx       xxx   xxx   xxxx   x  xx     xx  xx   xx xx     xx   xx   xx  xx    x x  xx  x  xx    x   xx     x   xx   xx   x   xxx x     xx   x  x   xxx           xx                  xx          xx       xx   xx                xx       xx                                                                xx                                                          xx  xx  xx  xx  xx xx  xx  x xx   x     xx  xx x   x  xx   x xx  xx xx        xx  x  xxxx x  xxxxxx x  x   xx  x x       xxx    xx x      xxx   xx   xx                           x    xx    xx    xx  xx         x x x                       x xx  xx  x    x                                                                                         "
"   x xx     xx  xx  xx     x  xx    xx  xx  x    xx  x x xx     x   xx    xx   xx      xx   xx x      xx       x xx x xx   x xxx  x  xx     xx  xx   xx xx     xx   xx   xx  xxxx    x  xx  x  xx    x    xx   x     xx x xx x     xxxx      xx x      xxx    xxxx    xx xx     xxx    xx xx    xxx  xxxxx  xxxx    xx xx   xxx  xxx  xx xxx   xx  xxx xx  xx   xxx xx     xxx   xxx xx     xx xx  xxx xx  xxxx xxxxx xxx xxx  xxxx xxx xxx xxxx xxx xxx xxx xxxx xxx xxxxxx xx  xx  xx      xx     xx  x xx   xxxx  xx        xx   xx x  xx  xx xx        xx  x x  xx x   x x   xx x   xx  x x  xx   x x     xx  xxx x x x  x     x                           x    xx    xx    xx  xx  xx xx x             xx    xx      xx     xx  xx   x                                                                                         "
"  x   xx    xxxxx   xx        xx    xx  xxxxx    xxxxx   xx         xxxxxxxx   xx      xx   xxxxx     xx       x xx x xx   x  xxx x  xx     xx  xx  xx  xx     xx   xxxxx     xxxxx     xx     xx    x    xx   x     xx x xx x      xxx      xx x      xx     x  xx   xxx xxx xxx xx xxx xxx  xxx xx  xx   xx  xx   xxx xx   xx   xx  xx xx    xx   xxx xxx xx   xxx xx  xxx xxx  xxx xxx xxx xxx   xxxxx x   x  xx    xx  xx   xx   x   xx  xx   x   xx  x   xx   x  x  xxx xx  xx  xx     xx    xx   x  xx   x  xx xxxxx     xx   xxxx   xxxxx     xx  x xx x xx xx  x   x x    xxxx  xx x x  xx x xx xx   xxxx  x    x   xx     xx                xxxxxx    x    xx    x     xx   x  xx xx             xxx      xxx    x     xx    x   x                                                                                         "
"  xxxxxx    xx   xx xx        xx    xx  xx  x    xx  x   xx   xxxx  xx    xx   xx      xx   xx xxx    xx       x xx x xx   x   xxxx  xx     xx  xxxxx   xx xx  xx   xx  xx      xxxx    xx     xx    x     xx x      xx x xx x     x xxx      xx      xxx      xxxx   xx   xx xx     xx   xx  xxxxxx  xx   xx  xx   xx  xx   xx   xx  xxxx     xx   xx  xx  xx   xx  xx  xx   xx  xx   xx xx   xx   xx    xxxx   xx    xx  xx    xx x     xx xxx x     xxx     xx x     xxx  xx  xx  xx    xx       xx x  xx      xx xx  xx   xx    x xxx     xx    xxxxxx xx x xx xx x  xxxxxx     xxx  xx  x xx  x x   x  x  xxxx         xx     xx xxxxxx                xxxxxxx xx   x      xx    x                 xxx          xxx xx    x      xx  x                                                                                         "
" x     xx   xx   xx xx     x  xx    xx  xx    x  xx      xx    xx   xx    xx   xx  xx  xx   xx  xx    xx    x  x  xx  xx   x    xxx  xx     xx  xx      xxx  x xx   xx  xx   x    xx    xx     xx    x     xx x       xx   xx      x  xxx     xx     xxx   x xx  xx   xx   xx xx     xx   xx  xx      xx    xxxx    xx  xx   xx   xx  xx xx    xx   xx  xx  xx   xx  xx  xx   xx  xx   xx xx   xx   xx     xxxx  xx    xx  xx    xx x     xx xxx x     xxx     xx x    xxx   xx  xx  xx   x   x xx  xx xxxxxx xx  xx xx  xx   xx   x   xx xx  xx    x  xx     x  xx xx    x x    xx x x     x  xx  x       xx   xxx         xx     xx                xxxxxx    x    xx    x     xx   x                    xxx      xxx   x             x  x                                                                                         "
" x     xx   xx   xx  xx   xx  xx   xx   xx   xx  xx       xx   xx   xx    xx   xx  xx  xx   xx  xxx   xx   xx  x  xx  xx   x     xx   xx   xx   xx       xx  xxx    xx   xxx xx   xx    xx     xxx  xx      xx        xx   xx     x    xx     xx    xxx   xx xx xxx   xx  xxx xxx xx xxx xxx  xxx xx  xx   x        xx  xx   xx   xx  xx  xx   xx   xx  xx  xx   xx  xx  xxx xxx  xxx xxx xxx xxx   xx    x   x  xx    xx xxx     xx       xx  xx     x  xx     xx    xxx  x  x  x   xx  xxxxxx xx  xx    xx  xx  xx xx  xx   xx   xx  xx xx  x            xx  x      x   x x    xx x x     x  xx x        xxx  xxxxx        x     x                           x    xx    xx    xx  xx  xx xx        xx     xx xx xx     x     xx      xx x                                                                                         "
"xxx   xxxx xxxxxxx    xxxxx  xxxxxxx   xxxxxxxx xxxx       xxxx x  xxxx  xxxx xxxx  xxx    xxxx  xxx xxxxxxxx xxx xx xxxx xxx     x    xxxxx   xxxx       xxxxx    xxxx  xx  x xxxx    xxxx     xxxxx       xx        xx   xx    xxx  xxxx   xxxx   xxxxxxxx  xx xxx  x xxx     xxx    xx xxx   xxx  xxxx  xxxxxx  xxxx xxx xxxx  xx xxxx xxx xxxx xxxx xxx xxx xxxx xxx   xxx    xx xx     xx xx  xxxx   xxxx    xx    xx xxx    xx       xx  xx    xxx xxx    xx    xxxxxx   xx   xxxx xxxxxx  xxxx    xxxx  xxxx   xxxx    xx    xxxx   xxx             xx   xxxxxx    x x     xxxx     x    xx          xxxx  xx         xx   xx                           x    xx    xx    xx  xx  xx xx        xx        xx        x     xx       x x                                                                                         "
"                                                                                                                                                              xxxx                                                                                                                                          xxxxxx                xx                                              xx           xx                                                               x                                                                                                                   x                                         x   x                                 xx    xx    xx  xx   x            x                                                                                                                             "
"                                                                                                                                                               xx                                                                                                                                          x    xx                xx                                              xx           xx                                                            xxx                                                                                                                                                               x x           xxxxxxx                xxxx   xx xxxx xx   x            x                                                                                                                              "
"                                                                                                                                                                                                                                                                                                            xxxxx                xx                                              xxxx         xxxx                                                           xx                                                                                                                                                                                                                                                                                                                                                                     "
   )
  "List of strings representing pixel lines for the button font.")

(defvar xpm-button-font-line-indices
  '(("A" 0 10)
    ("B" 11 19)
    ("C" 20 28)
    ("D" 29 38)
    ("E" 39 47)
    ("F" 48 56)
    ("G" 57 66)
    ("H" 67 77)
    ("I" 78 82)
    ("J" 83 90)
    ("K" 91 100)
    ("L" 101 109)
    ("M" 110 121)
    ("N" 122 132)
    ("O" 133 142)
    ("P" 143 151)
    ("Q" 152 162)
    ("R" 163 172)
    ("S" 173 180)
    ("T" 181 189)
    ("U" 190 199)
    ("V" 200 210)
    ("W" 211 224)
    ("X" 225 234)
    ("Y" 235 243)
    ("Z" 244 252)
    ("a" 253 260)
    ("b" 261 269)
    ("c" 270 276)
    ("d" 277 285)
    ("e" 286 292)
    ("f" 293 298)
    ("g" 299 306)
    ("h" 307 315)
    ("i" 316 320)
    ("j" 321 324)
    ("k" 325 333)
    ("l" 334 338)
    ("m" 339 351)
    ("n" 352 360)
    ("o" 361 368)
    ("p" 369 377)
    ("q" 378 386)
    ("r" 387 393)
    ("s" 394 399)
    ("t" 400 405)
    ("u" 406 414)
    ("v" 415 423)
    ("w" 424 436)
    ("x" 437 444)
    ("y" 445 453)
    ("z" 454 460)
    ("0" 461 467)
    ("1" 468 472)
    ("2" 473 479)
    ("3" 480 486)
    ("4" 487 493)
    ("5" 494 500)
    ("6" 501 507)
    ("7" 508 514)
    ("8" 515 521)
    ("9" 522 528)
    ("`" 529 531)
    ("~" 532 538)
    ("!" 539 541)
    ("@" 542 552)
    ("#" 553 560)
    ("$" 561 567)
    ("%" 568 580)
    ("^" 581 586)
    ("&" 587 597)
    ("*" 598 603)
    ("(" 604 608)
    (")" 609 613)
    ("-" 614 620)
    ("_" 621 628)
    ("=" 629 635)
    ("+" 636 643)
    ("[" 644 648)
    ("{" 649 653)
    ("]" 654 658)
    ("}" 659 663)
    (";" 664 666)
    (":" 667 669)
    ("'" 670 672)
    ("\"" 673 676)
    ("," 677 679)
    ("<" 680 686)
    ("." 687 689)
    (">" 690 696)
    ("/" 697 700)
    ("?" 701 707)
    ("\\" 708 713)
    ("|" 714 715)
    (" " 716 719))
  "Indices into the xpm-button-font-pixel-lines strings for each character.
Format is
  (STR START END)
STR contains the character.
START is where the character's pixels start in each string of
   xpm-button-font-pixel-lines (0 is the index of the first pixel).
END is the index of the position after the last pixel of the character.")

(defun xpm-button-color-rgb-components (color)
  "Return the RGB components of COLOR as a list of integers (R G B).
16-bit values are always returned.
#FEFEFE and rgb:fe/fe/fe style color specifications are parsed directly
into their components."
  (color-instance-rgb-components (make-color-instance color)))

(defun xpm-button-compute-contrast-color (color)
  "Compute a contrasting color for COLOR.
The new color is created by xor-ing the RGB values of COLOR with all ones."
  (let* ((rgb (xpm-button-color-rgb-components color))
	 (r (logxor (nth 0 rgb) 65535))
	 (g (logxor (nth 1 rgb) 65535))
	 (b (logxor (nth 2 rgb) 65535)))
    (format "rgb:%04x/%04x/%04x" r g b)))

(defun xpm-button-compute-shadow-colors (color)
  "Compute shadow colors for COLOR.
COLOR should be a string naming a color.
Returns (COLOR1 . COLOR2) where COLOR1 is the brighter shadow color
and COLOR2 is the dimmer color."
  (let* ((rgb (xpm-button-color-rgb-components color))
	 (r (nth 0 rgb))
	 (g (nth 1 rgb))
	 (b (nth 2 rgb))
	 (bright-r (/ (* r 12) 10))
	 (bright-g (/ (* g 12) 10))
	 (bright-b (/ (* b 12) 10))
	 (dim-r (/ (* r 8) 10))
	 (dim-g (/ (* g 8) 10))
	 (dim-b (/ (* b 8) 10)))
    (if (> bright-r 65535)
	(setq bright-r 65535))
    (if (> bright-g 65535)
	(setq bright-g 65535))
    (if (> bright-b 65535)
	(setq bright-b 65535))
    (cons (format "rgb:%04x/%04x/%04x" bright-r bright-g bright-b)
	  (format "rgb:%04x/%04x/%04x" dim-r dim-g dim-b))))

;;;###autoload
(defun xpm-button-create (text shadow-thickness fg-color bg-color)
  "Returns a list of XPM image instantiators for a button displaying TEXT.
The list is of the form
   (UP DOWN DISABLED)
where UP, DOWN, and DISABLED are the up, down and disabled image
instantiators for the button.

SHADOW-THICKNESS specifies how many pixels should be used for the
shadows on the edges of the buttons.  It should be a positive integer,
or 0 to mean no shadows on the edges.
FG-COLOR is the color used to display the text.  It should be a string.
BG-COLOR is the background color the text will be displayed upon.
It should be a string."
  (save-excursion
    (set-buffer (get-buffer-create " xpm-button"))
    (erase-buffer)
    ;; create the correct number of lines for the pixels for the
    ;; characters.
    (insert-char ?\n (length xpm-button-font-pixel-lines))
    (let ((i 0)
	  (str (make-string 1 0))
	  (lim (length text))
	  (bg-char ? )
	  font-pixel-lines q)
      ;; loop through text, adding the character pixels
      (while (< i lim)
	(aset str 0 (aref text i))
	(if (null (setq q (assoc str xpm-button-font-line-indices)))
	    nil ; no pixel data for this character
	  (goto-char (point-min))
	  (setq font-pixel-lines xpm-button-font-pixel-lines)
	  (while font-pixel-lines
	    (end-of-line)
	    (if (not (bolp))
		;; Insert space before some of the characters.
		;; This isn't really correct for this font
		;; but doing it right is too hard.
		;; This isn't TeX after all.
		(if (memq (aref str 0) '(?, ?. ?\" ?! ?| ?\' ?\`))
		    (insert-char bg-char 1))
	      ;; offset the start a bit from the left edge of the button
	      (insert-char bg-char xpm-button-horizontal-padding))
	    ;; insert the character pixels.
	    (insert (substring (car font-pixel-lines) (nth 1 q) (nth 2 q)))
	    (forward-line)
	    (setq font-pixel-lines (cdr font-pixel-lines))))
	(setq i (1+ i)))
      ;; now offset the text from the right edge of the button.
      (goto-char (point-min))
      (while (not (eobp))
	(end-of-line)
	(insert-char bg-char xpm-button-horizontal-padding)
	(forward-line)))
    (let ((bright-char ?b)
	  (dim-char ?d)
	  (fg-char ?x)
	  (bg-char ? )
	  (shadow-colors (xpm-button-compute-shadow-colors bg-color))
	  i len up-string down-string disabled-string)
      ;; find the length of a pixel line.
      (goto-char (point-min))
      (end-of-line)
      (setq len (- (point) (point-min)))
      ;; offset text from the top of the button
      (goto-char (point-min))
      (setq i xpm-button-vertical-padding)
      (while (> i 0)
	(insert-char bg-char len)
	(insert ?\n)
	(setq i (1- i)))
      ;; offset text from the bottom of the button
      (goto-char (point-max))
      (setq i xpm-button-vertical-padding)
      (while (> i 0)
	(insert-char bg-char len)
	(insert ?\n)
	(setq i (1- i)))
      ;; add shadows to the pixel lines
      (goto-char (point-min))
      (while (not (eobp))
	(insert-char bright-char shadow-thickness)
	(end-of-line)
	(insert-char dim-char shadow-thickness)
	(forward-line))
      ;; add top and bottom shadow lines
      (setq i shadow-thickness)
      (goto-char (point-min))
      (while (> i 0)
	(insert-char bright-char (+ len shadow-thickness i))
	(insert-char dim-char (- shadow-thickness i))
	(insert ?\n)
	(setq i (1- i)))
      (setq i shadow-thickness)
      (goto-char (point-max))
      (while (> i 0)
	(insert-char bright-char i)
	(insert-char dim-char (+ len (* 2 shadow-thickness) (- i)))
	(insert ?\n)
	(setq i (1- i)))
      ;; add doublequotes, commas and XPM header goop.
      (goto-char (point-min))
      (while (not (eobp))
	(insert "\"")
	(end-of-line)
	(insert "\",")
	(forward-line))
      (insert "};\n")
      (goto-char (point-min))
      ;; color map for the UP button
      (insert (format
	       "/* XPM */
static char * button_xpm[] = {
\"%d %d 4 1\",
\"%c   c %s\",
\"%c   c %s\",
\"%c   c %s\",
\"%c   c %s\",
"
	       (+ len (* shadow-thickness 2))
	       (+ (* xpm-button-vertical-padding 2)
		  (* shadow-thickness 2)
		  (length xpm-button-font-pixel-lines))
	       fg-char fg-color
	       bg-char bg-color
	       bright-char (car shadow-colors)
	       dim-char (cdr shadow-colors)))
      (setq up-string (buffer-string))
      (delete-region (point-min) (point))
      ;; color map for the DOWN button
      (insert (format
	       "/* XPM */
static char * button_xpm[] = {
\"%d %d 4 1\",
\"%c   c %s\",
\"%c   c %s\",
\"%c   c %s\",
\"%c   c %s\",
"
	       (+ len (* shadow-thickness 2))
	       (+ (* xpm-button-vertical-padding 2)
		  (* shadow-thickness 2)
		  (length xpm-button-font-pixel-lines))
	       fg-char (xpm-button-compute-contrast-color fg-color)
	       bg-char bg-color
	       bright-char (cdr shadow-colors)
	       dim-char (car shadow-colors)))
      (setq down-string (buffer-string))
      (delete-region (point-min) (point))
      ;; color map for the DISABLED button
      (insert (format
	       "/* XPM */
static char * button_xpm[] = {
\"%d %d 4 1\",
\"%c   c %s\",
\"%c   c %s\",
\"%c   c %s\",
\"%c   c %s\",
"
	       (+ len (* shadow-thickness 2))
	       (+ (* xpm-button-vertical-padding 2)
		  (* shadow-thickness 2)
		  (length xpm-button-font-pixel-lines))
	       fg-char fg-color
	       bg-char bg-color
	       bright-char (car shadow-colors)
	       dim-char (cdr shadow-colors)))
      ;; stipple the foreground pixels
      (let ((str (make-string 1 0))
	    (bit 0)
	    lim line-start)
	(aset str 0 fg-char)
	(while (not (eobp))
	  (setq lim (save-excursion (end-of-line) (point))
		line-start (point))
	  (while (search-forward str lim t)
	    (if (= (% (- (point) line-start) 2) bit)
		(subst-char-in-region (1- (point)) (point) fg-char bg-char t)))
	  (if (zerop bit)
	      (setq bit 1)
	    (setq bit 0))
	  (forward-line)))
      (setq disabled-string (buffer-string))

      (list (vector 'xpm ':data up-string)
	    (vector 'xpm ':data down-string)
	    (vector 'xpm ':data disabled-string)) )))

;;; xpm-button.el ends here
