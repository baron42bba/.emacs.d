;;; sokoban.el --- Implementation of Sokoban for Emacs.

;; Copyright (C) 1998, 2013, 2017 Free Software Foundation, Inc.

;; Author: Glynn Clements <glynn.clements@xemacs.org>
;; Maintainer: Dieter Deyke <dieter.deyke@gmail.com>
;; Version: 1.4.6
;; Package-Requires: ((emacs "23.1"))
;; Created: 1997-09-11
;; Keywords: games
;; Package-Type: multi

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not synched.

;;; Commentary:

;; Modified: 1998-01-09, conditionalised use of locate-data-directory
;; Modified: 1998-01-27, added mouse interface code
;;   (provided by Sean MacLennan <bn932@freenet.carleton.ca>
;; Modified: 1998-02-06, fixed bug, where sokoban-done wasn't reset to
;;   zero in sokoban-restart-level
;; Modified: 1998-02-27, patches from Hrvoje Niksic
;;   added bounds check to sokoban-goto-level
;;   added popup menu
;;   display level and score in modeline
;; Modified: 1998-06-04, added `undo' feature
;;   added number of blocks done/total to score and modeline
;; Modified: 2003-06-14, update email address, remove URL

;; The game is based upon XSokoban, by
;; Michael Bischoff <mbi@mo.math.nat.tu-bs.de>

;; The levels and some of the pixmaps were
;; taken directly from XSokoban

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'gamegrid)
(require 'xml)

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sokoban-use-glyphs t
  "Non-nil means use glyphs when available.")

(defvar sokoban-use-color t
  "Non-nil means use color when available.")

(defvar sokoban-font "-*-courier-medium-r-*-*-*-200-100-75-*-*-iso8859-*"
  "Name of the font used in X mode.")

(defvar sokoban-buffer-name "*Sokoban*")

(defvar sokoban-level-file
  (if (fboundp 'locate-data-file)
      (locate-data-file "sokoban.levels")
    (or (locate-library "sokoban.levels")
            (let ((file (expand-file-name
                         "sokoban.levels"
                         (if load-file-name
                             (file-name-directory load-file-name)))))
              (and (file-exists-p file) file))
	(expand-file-name "sokoban.levels" data-directory))))

(defvar sokoban-width)
(defvar sokoban-height)

(defvar sokoban-buffer-width)
(defvar sokoban-buffer-height)

(defvar sokoban-score-x)
(defvar sokoban-score-y)

(defvar sokoban-level-data nil)

(defconst sokoban-state-filename (locate-user-emacs-file "sokoban-state"))

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sokoban-floor-xpm "\
/* XPM */
static char * floor_xpm[] = {
\"32 32 1 1\",
\"  c None\",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
};
")

(defconst sokoban-target-xpm "\
/* XPM */
static char * target_xpm[] = {
\"32 32 3 1\",
\"  c None\",
\". c black\",
\"X c green\",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"          ............          \",
\"          .XXXXXXXXXX.          \",
\"           .XXXXXXXX.           \",
\"            .XXXXXX.            \",
\"      ..     .XXXX.     ..      \",
\"      .X.     .XX.     .X.      \",
\"      .XX.     ..     .XX.      \",
\"      .XXX.          .XXX.      \",
\"      .XXXX.        .XXXX.      \",
\"      .XXXXX.      .XXXXX.      \",
\"      .XXXXX.      .XXXXX.      \",
\"      .XXXX.        .XXXX.      \",
\"      .XXX.          .XXX.      \",
\"      .XX.     ..     .XX.      \",
\"      .X.     .XX.     .X.      \",
\"      ..     .XXXX.     ..      \",
\"            .XXXXXX.            \",
\"           .XXXXXXXX.           \",
\"          .XXXXXXXXXX.          \",
\"          ............          \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
\"                                \",
};
")

(defconst sokoban-wall-xpm "\
/* XPM */
static char * wall_xpm[] = {
\"32 32 2 1\",
\"  c white\",
\". c SteelBlue\",
\" .............................. \",
\". ............................ .\",
\".. .......................... . \",
\"... ........................ . .\",
\"....                        . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\".... ......................  . .\",
\".... ...................... . . \",
\"....                         . .\",
\"... . . . . . . . . . . . .   . \",
\".. . . . . . . . . . . . . .   .\",
\". . . . . . . . . . . . . . .   \",
\" . . . . . . . . . . . . . . .  \",
};
")

(defconst sokoban-block-xpm "\
/* XPM */
static char * block_xpm[] = {
\"32 32 3 1\",
\"  c None\",
\". c black\",
\"X c yellow\",
\".............................   \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.   \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX..  \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX..  \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.X. \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.X. \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".............................XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.X.\",
\" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.X.\",
\"  .XXXXXXXXXXXXXXXXXXXXXXXXXXX..\",
\"  .XXXXXXXXXXXXXXXXXXXXXXXXXXX..\",
\"   .XXXXXXXXXXXXXXXXXXXXXXXXXXX.\",
\"   .............................\",
};
")

(defconst sokoban-block-on-target-xpm "\
/* XPM */
static char * block_on_target_xpm[] = {
\"32 32 3 1\",
\"  c None\",
\". c black\",
\"X c green\",
\".............................   \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.   \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX..  \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX..  \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.X. \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.X. \",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\".............................XX.\",
\".XXXXXXXXXXXXXXXXXXXXXXXXXXX.XX.\",
\" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.X.\",
\" .XXXXXXXXXXXXXXXXXXXXXXXXXXX.X.\",
\"  .XXXXXXXXXXXXXXXXXXXXXXXXXXX..\",
\"  .XXXXXXXXXXXXXXXXXXXXXXXXXXX..\",
\"   .XXXXXXXXXXXXXXXXXXXXXXXXXXX.\",
\"   .............................\",
};
")

(defconst sokoban-player-xpm "\
/* XPM */
static char * player_xpm[] = {
\"32 32 3 1\",
\"  c None\",
\"o c white\",
\". c black\",
\"                                \",
\"                                \",
\"                                \",
\"            oooooooo            \",
\"            o......o            \",
\"           o.oooooo.o           \",
\"           o.oooooo.o           \",
\"          o.oooooooo.o          \",
\"          o.o..oo..o.o          \",
\"          o.oooooooo.o          \",
\"          oo.o....o.oo          \",
\"         oo..oo..oo..oo         \",
\"         o....o..o....o         \",
\"         o.o..o..o..o.o         \",
\"         o.o...oo...o.o         \",
\"        o.oo........oo.o        \",
\"        o.oo........oo.o        \",
\"       o.ooo........ooo.o       \",
\"       o.ooo........ooo.o       \",
\"       o.ooo........ooo.o       \",
\"        o.oo........oo.o        \",
\"        o.oo........oo.o        \",
\"        o.o..........o.o        \",
\"         o............o         \",
\"          o..........o          \",
\"           o........o           \",
\"          o.o.oooo.o.o          \",
\"         o.....oo.....o         \",
\"        o......oo......o        \",
\"       o.......oo.......o       \",
\"      o..o..o..oo.oo..o..o      \",
\"      oooooooooooooooooooo      \",
};
")

(defconst sokoban-player-on-target-xpm "\
/* XPM */
static char * player_on_target_xpm[] = {
\"32 32 4 1\",
\"  c None\",
\"o c white\",
\". c black\",
\"X c green\",
\"                                \",
\"                                \",
\"                                \",
\"            oooooooo            \",
\"            o......o            \",
\"           o.oooooo.o           \",
\"          .o.oooooo.o.          \",
\"          o.oooooooo.o          \",
\"          o.o..oo..o.o          \",
\"          o.oooooooo.o          \",
\"      ..  oo.o....o.oo  ..      \",
\"      .X.oo..oo..oo..oo.X.      \",
\"      .XXo....o..o....oXX.      \",
\"      .XXo.o..o..o..o.oXX.      \",
\"      .XXo.o...oo...o.oXX.      \",
\"      .Xo.oo........oo.oX.      \",
\"      .Xo.oo........oo.oX.      \",
\"      .o.ooo........ooo.o.      \",
\"      .o.ooo........ooo.o.      \",
\"      .o.ooo........ooo.o.      \",
\"      .Xo.oo........oo.oX.      \",
\"      ..o.oo........oo.o..      \",
\"        o.o..........o.o        \",
\"         o............o         \",
\"          o..........o          \",
\"          .o........o.          \",
\"          o.o.oooo.o.o          \",
\"         o.....oo.....o         \",
\"        o......oo......o        \",
\"       o.......oo.......o       \",
\"      o..o..o..oo.oo..o..o      \",
\"      oooooooooooooooooooo      \",
};
")

(defconst sokoban-floor ?\&)
;; note - space character in level file is also allowed to indicate floor
(defconst sokoban-target ?\.)
(defconst sokoban-wall ?\#)
(defconst sokoban-block ?\$)
(defconst sokoban-player ?\@)
(defconst sokoban-block-on-target ?\*)
(defconst sokoban-player-on-target ?\+)

;; ;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sokoban-floor-options
  `(((glyph
      [xpm :data ,sokoban-floor-xpm])
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar sokoban-target-options
  `(((glyph
      [xpm :data ,sokoban-target-xpm])
     ((mono-x mono-tty emacs-tty) ?\.)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [1 1 0.5])
     (color-tty "yellow"))))

(defvar sokoban-wall-options
  `(((glyph
      [xpm :data ,sokoban-wall-xpm])
     (emacs-tty ?\X)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [0 0 1])
     (color-tty "blue"))))

(defvar sokoban-block-options
  `(((glyph
      [xpm :data ,sokoban-block-xpm])
     ((mono-x mono-tty emacs-tty) ?\O)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [1 0 0])
     (color-tty "red"))))

(defvar sokoban-block-on-target-options
  `(((glyph
      [xpm :data ,sokoban-block-on-target-xpm])
     ((mono-x mono-tty emacs-tty) ?\O)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [1 0 0])
     (color-tty "red"))))

(defvar sokoban-player-options
  `(((glyph
      [xpm :data ,sokoban-player-xpm])
     (t ?\*))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 1 0])
     (color-tty "green"))))

(defvar sokoban-player-on-target-options
  `(((glyph
      [xpm :data ,sokoban-player-on-target-xpm])
     (t ?\*))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 1 0])
     (color-tty "green"))))

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sokoban-level 0)
(make-variable-buffer-local 'sokoban-level)
(defvar sokoban-level-map nil)
(make-variable-buffer-local 'sokoban-level-map)
(defvar sokoban-targets 0)
(make-variable-buffer-local 'sokoban-targets)
(defvar sokoban-x 0)
(make-variable-buffer-local 'sokoban-x)
(defvar sokoban-y 0)
(make-variable-buffer-local 'sokoban-y)
(defvar sokoban-moves 0)
(make-variable-buffer-local 'sokoban-moves)
(defvar sokoban-pushes 0)
(make-variable-buffer-local 'sokoban-pushes)
(defvar sokoban-done 0)
(make-variable-buffer-local 'sokoban-done)
(defvar sokoban-mouse-x 0)
(make-variable-buffer-local 'sokoban-mouse-x)
(defvar sokoban-mouse-y 0)
(make-variable-buffer-local 'sokoban-mouse-y)
(defvar sokoban-undo-list nil)
(make-variable-buffer-local 'sokoban-undo-list)

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sokoban-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n"	'sokoban-start-game)
    (define-key map "r"	'sokoban-restart-level)
    (define-key map "g"	'sokoban-goto-level)
    (define-key map "F"	'fit-frame-to-buffer)

    (define-key map [left]	'sokoban-move-left)
    (define-key map [right]	'sokoban-move-right)
    (define-key map [up]	'sokoban-move-up)
    (define-key map [down]	'sokoban-move-down)

    (define-key map [down-mouse-2] 'sokoban-mouse-event-start)
    (define-key map [mouse-2]      'sokoban-mouse-event-end)
    ;; On some systems (OS X) middle mouse is difficult.
    ;; FIXME: Use follow-link?
    (define-key map [down-mouse-1] 'sokoban-mouse-event-start)
    (define-key map [mouse-1]      'sokoban-mouse-event-end)

    (define-key map [(control ?/)]	'sokoban-undo)
    map))

;; ;;;;;;;;;;;;;;;; level file parsing functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sokoban-level-regexp "^;LEVEL [0-9]+$")

(defconst sokoban-comment-regexp "^;")

(defun sokoban-convert-xml-to-plain-text ()
  (let ((n 0) (tree (xml-parse-region)))
    (erase-buffer)
    (dolist (SokobanLevels tree)
      (dolist (LevelCollection (xml-get-children SokobanLevels 'LevelCollection))
        (dolist (Level (xml-get-children LevelCollection 'Level))
          (incf n)
          (insert (format ";LEVEL %d\n" n))
          (dolist (L (xml-get-children Level 'L))
            (insert (car (xml-node-children L)))
            (insert "\n"))))))
  (goto-char (point-min)))

(defun sokoban-init-level-data ()
  (setq sokoban-level-data nil
        sokoban-width 15 ; need at least 15 for score display
        sokoban-height 1)
  (with-temp-buffer
    (insert-file-contents sokoban-level-file)
    (goto-char (point-min))
    (if (looking-at "<\\?xml version=")
        (sokoban-convert-xml-to-plain-text))
    (re-search-forward sokoban-level-regexp nil t)
    (forward-char)
    (let (r)
      (while (not (eobp))
        (while (looking-at sokoban-comment-regexp)
	  (forward-line))
        (setq r 0)
        (while (not (or (eobp)
		        (looking-at sokoban-comment-regexp)))
          (incf r)
          (setq sokoban-height (max sokoban-height r)
                sokoban-width (max sokoban-width (- (line-end-position) (line-beginning-position))))
	  (forward-line))))
    (setq sokoban-buffer-width sokoban-width
          sokoban-buffer-height (+ 4 sokoban-height)
          sokoban-score-x 0
          sokoban-score-y (1+ sokoban-height))

    (goto-char (point-min))
    (re-search-forward sokoban-level-regexp nil t)
    (forward-char)
    (while (not (eobp))
      (while (looking-at sokoban-comment-regexp)
        (forward-line))
      (let ((data (make-vector sokoban-height nil))
	    (fmt (format "%%-%ds" sokoban-width)))
        (dotimes (y sokoban-height)
	  (cond ((or (eobp)
		     (looking-at sokoban-comment-regexp))
	         (aset data y (format fmt "")))
	        (t
	         (let ((start (point))
                       (end (line-end-position)))
                   (aset data
                         y
                         (format fmt (buffer-substring start end)))
                   (goto-char (1+ end))))))
        (push data sokoban-level-data)))
    (kill-buffer (current-buffer))
    (setq sokoban-level-data (nreverse sokoban-level-data))))

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sokoban-display-options ()
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
	    (cond ((= c sokoban-floor)
		   sokoban-floor-options)
                  ((= c sokoban-target)
		   sokoban-target-options)
                  ((= c sokoban-wall)
		   sokoban-wall-options)
                  ((= c sokoban-block)
		   sokoban-block-options)
                  ((= c sokoban-block-on-target)
		   sokoban-block-on-target-options)
                  ((= c sokoban-player)
		   sokoban-player-options)
                  ((= c sokoban-player-on-target)
		   sokoban-player-on-target-options)
                  (t
		   '(nil nil nil)))))
    options))

(defun sokoban-get-level-data ()
  (setq sokoban-level-map (nth (1- sokoban-level) sokoban-level-data)
	sokoban-targets 0)
  (dotimes (y sokoban-height)
    (dotimes (x sokoban-width)
      (let ((c (aref (aref sokoban-level-map y) x)))
	(cond
	 ((or (eq c sokoban-target)
	      (eq c sokoban-player-on-target))
	  (incf sokoban-targets))
	 ((eq c sokoban-block-on-target)
	  (incf sokoban-targets)
	  (incf sokoban-done))
	 ((= c ?\040) ;; treat space characters in level file as floor
	  (aset (aref sokoban-level-map y) x sokoban-floor)))))))

(defun sokoban-init-buffer ()
  (gamegrid-init-buffer sokoban-buffer-width
			sokoban-buffer-height
			?\040)
  (dotimes (y sokoban-height)
    (dotimes (x sokoban-width)
      (let ((c (aref (aref sokoban-level-map y) x)))
	(if (or (eq c sokoban-player)
	        (eq c sokoban-player-on-target))
	    (setq sokoban-x x
		  sokoban-y y))
	(gamegrid-set-cell x y c)))))

(defun sokoban-draw-score ()
  (let ((y sokoban-score-y))
    (dolist (string (list (format "Moves:  %05d" sokoban-moves)
			  (format "Pushes: %05d" sokoban-pushes)
			  (format "Done:   %d/%d"
				  sokoban-done
				  sokoban-targets)))
      (let* ((len (length string)))
        (dotimes (x len)
	  (gamegrid-set-cell (+ sokoban-score-x x)
			     y (aref string x))))
      (incf y)))
  (setq mode-line-format
	(format "Sokoban:   Level: %d/%d   Moves: %05d   Pushes: %05d   Done: %d/%d"
		sokoban-level (length sokoban-level-data) sokoban-moves sokoban-pushes
		sokoban-done sokoban-targets))
  (force-mode-line-update))

(defun sokoban-add-move (dx dy)
  (push (list 'move dx dy) sokoban-undo-list)
  (incf sokoban-moves)
  (sokoban-draw-score))

(defun sokoban-add-push (dx dy)
  (push (list 'push dx dy) sokoban-undo-list)
  (incf sokoban-moves)
  (incf sokoban-pushes)
  (sokoban-draw-score))

(defun sokoban-targetp (x y)
  (let ((c (aref (aref sokoban-level-map y) x)))
    (or (eq c sokoban-target)
	(eq c sokoban-block-on-target)
	(eq c sokoban-player-on-target))))

(defun sokoban-set-floor (x y)
  (gamegrid-set-cell x y
                     (if (sokoban-targetp x y)
                         sokoban-target
                       sokoban-floor)))

(defun sokoban-set-player (x y)
  (gamegrid-set-cell x y
                     (if (sokoban-targetp x y)
                         sokoban-player-on-target
                       sokoban-player)))

(defun sokoban-set-block (x y)
  (gamegrid-set-cell x y
                     (if (sokoban-targetp x y)
                         sokoban-block-on-target
                       sokoban-block)))

(defun sokoban-undo ()
  "Undo previous Sokoban change."
  (interactive)
  ;; FIXME: Use the normal undo (via `apply' undo entries).
  (if (null sokoban-undo-list)
      (message "Nothing to undo")
    (let* ((entry (pop sokoban-undo-list))
	   (type (car entry))
	   (dx (nth 1 entry))
	   (dy (nth 2 entry)))
      (cond ((eq type 'push)
	     (let* ((x (+ sokoban-x dx))
		    (y (+ sokoban-y dy)))
	       (sokoban-set-floor x y)
	       (if (sokoban-targetp x y)
		   (decf sokoban-done))
	       (sokoban-set-block sokoban-x sokoban-y)
	       (if (sokoban-targetp sokoban-x sokoban-y)
		   (incf sokoban-done)))
	     (setq sokoban-x (- sokoban-x dx))
	     (setq sokoban-y (- sokoban-y dy))
	     (sokoban-set-player sokoban-x sokoban-y)
	     (decf sokoban-pushes)
	     (decf sokoban-moves))
	    ((eq type 'move)
	     (sokoban-set-floor sokoban-x sokoban-y)
	     (setq sokoban-x (- sokoban-x dx))
	     (setq sokoban-y (- sokoban-y dy))
	     (sokoban-set-player sokoban-x sokoban-y)
	     (decf sokoban-moves))
	    (t
	     (message "Invalid entry in sokoban-undo-list")))
      (sokoban-draw-score))))

(defun sokoban-move (dx dy)
  (let* ((x (+ sokoban-x dx))
	 (y (+ sokoban-y dy))
	 (c (gamegrid-get-cell x y)))
    (cond ((or (eq c sokoban-floor)
	       (eq c sokoban-target))
           (sokoban-set-floor sokoban-x sokoban-y)
	   (setq sokoban-x x
		 sokoban-y y)
           (sokoban-set-player sokoban-x sokoban-y)
	   (sokoban-add-move dx dy))
	  ((or (eq c sokoban-block)
	       (eq c sokoban-block-on-target))
	   (let* ((xx (+ x dx))
		  (yy (+ y dy))
		  (cc (gamegrid-get-cell xx yy)))
	     (cond ((or (eq cc sokoban-floor)
			(eq cc sokoban-target))
		    (if (sokoban-targetp x y)
			(decf sokoban-done))
                    (sokoban-set-block xx yy)
		    (sokoban-set-player x y)
		    (sokoban-set-floor sokoban-x sokoban-y)
		    (setq sokoban-x x
			  sokoban-y y)
		    (if (sokoban-targetp xx yy)
			(incf sokoban-done))
		    (sokoban-add-push dx dy)
		    (cond ((= sokoban-done sokoban-targets)
                           (let ((level sokoban-level))
                             (with-temp-file sokoban-state-filename
                               (print level (current-buffer))))
			   (sit-for 3)
			   (sokoban-next-level))))))))))

(defun sokoban-event-x (event)
  (let ((x (gamegrid-event-x event)))
    ;; 32.0 is the pixel width of the xpm image
    (floor x (/ 32.0 (frame-char-width)))))

(defun sokoban-event-y (event)
  (let ((y (gamegrid-event-y event)))
    (floor y (/ 32.0 (frame-char-height)))))

(defun sokoban-mouse-event-start (event)
  "Record the beginning of a mouse click."
  (interactive "e")
  (setq sokoban-mouse-x (sokoban-event-x event))
  (setq sokoban-mouse-y (sokoban-event-y event)))

(defun sokoban-mouse-event-end (event)
  "Move according to the clicked position."
  (interactive "e")
  (let* ((x (sokoban-event-x event))
	 (y (sokoban-event-y event))
	 (dx (- x sokoban-x))
	 (dy (- y sokoban-y)))
    (cond
     ;; Ensure that press and release are in the same square
     ;; (which allows you to abort a move)
     ((not (and (eq sokoban-mouse-x x) (eq sokoban-mouse-y y)))
      nil)
     ;; Check that the move isn't diagonal
     ((not (or (eq dx 0) (eq dy 0)))
      nil)
     ((< dx 0)	;; Left
      (while (< dx 0)
	(sokoban-move -1 0)
	(setq dx (1+ dx))))
     ((> dx 0)	;; Right
      (while (> dx 0)
	(sokoban-move 1 0)
	(setq dx (1- dx))))
     ((> dy 0)	;; Up
      (while (> dy 0)
	(sokoban-move 0 1)
	(setq dy (1- dy))))
     ((< dy 0)	;; Down
      (while (< dy 0)
	(sokoban-move 0 -1)
	(setq dy (1+ dy)))))))

(defun sokoban-move-left ()
  "Move one square left."
  (interactive)
  (sokoban-move -1 0))

(defun sokoban-move-right ()
  "Move one square right."
  (interactive)
  (sokoban-move 1 0))

(defun sokoban-move-up ()
  "Move one square up."
  (interactive)
  (sokoban-move 0 -1))

(defun sokoban-move-down ()
  "Move one square down."
  (interactive)
  (sokoban-move 0 1))

(defun sokoban-restart-level ()
  "Restart the current level."
  (interactive)
  (setq sokoban-moves 0
	sokoban-pushes 0
	sokoban-done 0
	sokoban-undo-list nil)
  (sokoban-get-level-data)
  (sokoban-init-buffer)
  (sokoban-draw-score))

(defun sokoban-next-level ()
  (sokoban-goto-level (1+ sokoban-level)))

(defun sokoban-goto-level (level)
  "Jump to a specified LEVEL."
  (interactive "nLevel: ")
  (when (or (< level 1)
            (> level (length sokoban-level-data)))
    (signal 'args-out-of-range
            (list
             (format "No such level number %d, should be 1..%d"
                     level (length sokoban-level-data)))))
  (setq sokoban-level level)
  (sokoban-restart-level))

(defun sokoban-start-game ()
  "Start a new game of Sokoban."
  (interactive)
  (setq sokoban-level 0)
  (sokoban-next-level))

(put 'sokoban-mode 'mode-class 'special)

(easy-menu-define sokoban-popup-menu nil "Popup menu for Sokoban mode."
  '("Sokoban Commands"
    ["Restart this level" sokoban-restart-level]
    ["Start new game" sokoban-start-game]
    ["Go to specific level" sokoban-goto-level]
    ["Fit frame to buffer" fit-frame-to-buffer]))
(define-key sokoban-mode-map [down-mouse-3] sokoban-popup-menu)

(define-derived-mode sokoban-mode special-mode "Sokoban"
  "A mode for playing Sokoban.

sokoban-mode keybindings:
   \\{sokoban-mode-map}"

  (set (make-local-variable 'gamegrid-use-glyphs) sokoban-use-glyphs)
  (set (make-local-variable 'gamegrid-use-color) sokoban-use-color)
  (set (make-local-variable 'gamegrid-font) sokoban-font)

  (gamegrid-init (sokoban-display-options))

  (if (null sokoban-level-data)
      (sokoban-init-level-data)))

;;;###autoload
(defun sokoban ()
  "Sokoban.

Push the blocks onto the target squares.

sokoban-mode keybindings:
   \\<sokoban-mode-map>
\\[sokoban-start-game]	Starts a new game of Sokoban
\\[sokoban-restart-level]	Restarts the current level
\\[sokoban-goto-level]	Jumps to a specified level
\\[fit-frame-to-buffer]	Fit frame to buffer
\\[sokoban-move-left]	Move one square to the left
\\[sokoban-move-right]	Move one square to the right
\\[sokoban-move-up]	Move one square up
\\[sokoban-move-down]	Move one square down"
  (interactive)

  (switch-to-buffer sokoban-buffer-name)
  (gamegrid-kill-timer)
  (sokoban-mode)
  (setq sokoban-level 0)
  (if (file-exists-p sokoban-state-filename)
    (setq sokoban-level
          (with-temp-buffer
            (insert-file-contents sokoban-state-filename)
            (goto-char (point-min))
            (read (current-buffer)))))
  (sokoban-next-level))

;;;###autoload
(define-key-after			; install a menu entry
  (lookup-key global-map [menu-bar tools games])
  [sokoban]
  '(menu-item "Sokoban" sokoban)
  'snake)

(provide 'sokoban)

;;; sokoban.el ends here
