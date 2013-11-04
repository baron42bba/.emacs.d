;;; diff-mode.el --- a mode for viewing/editing context diffs

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: convenience patch diff

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Synched up with: GNU Emacs 21.3, except for the easy-mmode-defmap stuff.
;;;                  Also various trivial bits synced from Emacs CVS 2004-08-13
;;;                  and 2005-12-01.

;;; Commentary:

;; Provides support for font-lock, outline, navigation
;; commands, editing and various conversions as well as jumping
;; to the corresponding source file.

;; Inspired by Pavel Machek's patch-mode.el (<pavel@@atrey.karlin.mff.cuni.cz>)
;; Some efforts were spent to have it somewhat compatible with XEmacs'
;; diff-mode as well as with compilation-minor-mode

;; Bugs:

;; - Reverse doesn't work with normal diffs.

;; Todo:

;; - Improve narrowed-view support.
;; - re-enable (conditionally) the `compile' support after improving it to use
;;   the same code as diff-goto-source.
;; - Support for # comments in context->unified.
;; - Do a fuzzy search in diff-goto-source.
;; - Allow diff.el to use diff-mode.
;;   This mostly means ability to jump from half-hunk to half-hunk
;;   in context (and normal) diffs and to jump to the corresponding
;;   (i.e. new or old) file.
;; - Handle `diff -b' output in context->unified.

;; Low priority:
;; - Spice up the minor-mode with font-lock support.
;; - Recognize pcl-cvs' special string for `cvs-execute-single'.

;;; Code:

;; XEmacs change
(if (featurep 'diff)
    (error 'invalid-state
	   "The diff and diff-mode packages are incompatible."))

(eval-when-compile (require 'cl))


(defgroup diff-mode ()
  "Major mode for viewing/editing diffs."
  :version "21.1"
  :group 'tools
  :group 'diff)

(defcustom diff-default-read-only nil
  "If non-nil, `diff-mode' buffers default to being read-only."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-jump-to-old-file nil
  "*Non-nil means `diff-goto-source' jumps to the old file.
Else, it jumps to the new file."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-update-on-the-fly t
  "*Non-nil means hunk headers are kept up-to-date on-the-fly.
When editing a diff file, the line numbers in the hunk headers
need to be kept consistent with the actual diff.  This can
either be done on the fly (but this sometimes interacts poorly with the
undo mechanism) or whenever the file is written (can be slow
when editing big diffs)."
  :type 'boolean
  :group 'diff-mode)

(defcustom diff-advance-after-apply-hunk t
  "*Non-nil means `diff-apply-hunk' will move to the next hunk after applying."
  :type 'boolean
  :group 'diff-mode)


(defvar diff-mode-hook nil
  "Run after setting up the `diff-mode' major mode.")

(defvar diff-outline-regexp
  "\\([*+][*+][*+] [^0-9]\\|@@ ...\\|\\*\\*\\* [0-9].\\|--- [0-9]..\\)")

;;;;
;;;; keymap, menu, ...
;;;;

;; XEmacs change: the clever keymap tricks here didn't work; fix them.

(defmacro diff-defmap (var bindings doc)
  `(defvar ,var
     (let ((m (make-keymap)))
       (dolist (b ,bindings)
	 (define-key m (car b) (cdr b)))
       m)
     ,doc))

(defmacro diff-metify-map (var bindings doc)
  `(defvar ,var
     (let ((m (make-keymap)))
       (dolist (b ,bindings)
	 (define-key m (char-to-string (+ 128 (string-to-char (car b))))
	   (cdr b)))
       m)
     ,doc))

(let ((bindings
       '(;; from Pavel Machek's patch-mode
	 ("n" . diff-hunk-next)
	 ("N" . diff-file-next)
	 ("p" . diff-hunk-prev)
	 ("P" . diff-file-prev)
	 ("k" . diff-hunk-kill)
	 ("K" . diff-file-kill)
	 ;; From compilation-minor-mode.
	 ("}" . diff-file-next)
	 ("{" . diff-file-prev)
	 ("\C-m" . diff-goto-source)
	 ("W" . widen)
	 ;;("." . diff-goto-source)		;display-buffer
	 ;;("f" . diff-goto-source)		;find-file
	 ("o" . diff-goto-source)	;other-window
	 ;;("w" . diff-goto-source)		;other-frame
	 ;;("N" . diff-narrow)
	 ;;("h" . diff-show-header)
	 ;;("j" . diff-show-difference)	;jump to Nth diff
	 ;;("q" . diff-quit)
	 (" " . scroll-up)
	 ("\177" . scroll-down)
	 ;; Our very own bindings.
	 ("A" . diff-ediff-patch)
	 ("r" . diff-restrict-view)
	 ("R" . diff-reverse-direction)
	 ("U" . diff-context->unified)
	 ("C" . diff-unified->context))))
  (diff-defmap
   diff-mode-shared-map bindings
   "Keymap for read-only `diff-mode'. Only active in read-only mode.")
  (diff-metify-map
   diff-mode-map bindings
   "Keymap for `diff-mode'.  See also `diff-mode-shared-map'."))

;; from compilation-minor-mode
(define-key diff-mode-map "\C-c\C-c" 'diff-goto-source)
;; from GNU Emacs
(define-key diff-mode-map "\C-c\C-s" 'diff-split-hunk)
(define-key diff-mode-map "\C-c\C-a" 'diff-apply-hunk)
(define-key diff-mode-map "\C-c\C-t" 'diff-test-hunk)

(easy-menu-define diff-mode-menu diff-mode-map
  "Menu for `diff-mode'."
  '("Diff"
    ["Jump to Source"		diff-goto-source	t]
    ["Apply hunk"		diff-apply-hunk		t]
    ["Apply diff with Ediff"	diff-ediff-patch	t]
    ["-----" nil nil]
    ["Reverse direction"	diff-reverse-direction	t]
    ["Context -> Unified"	diff-context->unified	t]
    ["Unified -> Context"	diff-unified->context	t]
    ;;["Fixup Headers"		diff-fixup-modifs	(not buffer-read-only)]
    ))

(defcustom diff-minor-mode-prefix "\C-c="
  "Prefix key for `diff-minor-mode' commands."
  :type '(choice (string "\e") (string "C-c=") string)
  :group 'diff-mode)

(easy-mmode-defmap diff-minor-mode-map
  `((,diff-minor-mode-prefix . ,diff-mode-shared-map))
  "Keymap for `diff-minor-mode'.  See also `diff-mode-shared-map'.")


;;;;
;;;; font-lock support
;;;;

(defface diff-header-face
  '((((type tty pc) (class color) (background light))
     (:foreground "blue1" :bold t))
    (((type tty pc) (class color) (background dark))
     (:foreground "green" :bold t))
    (((class color) (background light))
     (:background "grey85"))
    (((class color) (background dark))
     (:background "grey45"))
    (t (:bold t)))
  "`diff-mode' face inherited by hunk and index header faces."
  :group 'diff-mode)
(defvar diff-header-face 'diff-header-face)

(defface diff-file-header-face
  '((((type tty pc) (class color) (background light))
     (:foreground "yellow" :bold t))
    (((type tty pc) (class color) (background dark))
     (:foreground "cyan" :bold t))
    (((class color) (background light))
     (:background "grey70" :bold t))
    (((class color) (background dark))
     (:background "grey60" :bold t))
    (t (:bold t)))			; :height 1.3
  "`diff-mode' face used to highlight file header lines."
  :group 'diff-mode)
(defvar diff-file-header-face 'diff-file-header-face)

(defface diff-index-face
  ;; XEmacs change: no :inherit in 21.4, copy/pasted `diff-file-header-face'.
  '((((type tty pc) (class color) (background light))
     (:foreground "yellow" :bold t))
    (((type tty pc) (class color) (background dark))
     (:foreground "cyan" :bold t))
    (((class color) (background light))
     (:background "grey70" :bold t))
    (((class color) (background dark))
     (:background "grey60" :bold t))
    (t (:bold t)))			; :height 1.3
  "`diff-mode' face used to highlight index header lines."
  :group 'diff-mode)
(defvar diff-index-face 'diff-index-face)

(defface diff-hunk-header-face
  ;; XEmacs change: no :inherit in 21.4; copy/pasted `diff-header-face'.
  '((((type tty pc) (class color) (background light))
     (:foreground "blue1" :bold t))
    (((type tty pc) (class color) (background dark))
     (:foreground "green" :bold t))
    (((class color) (background light))
     (:background "grey85"))
    (((class color) (background dark))
     (:background "grey45"))
    (t (:bold t)))
  "`diff-mode' face used to highlight hunk header lines."
  :group 'diff-mode)
(defvar diff-hunk-header-face 'diff-hunk-header-face)

(defface diff-removed-face
  ;; XEmacs change: no :inherit in 21.4; copy/pasted `diff-changed-face'.
  '((((type tty pc) (class color) (background light))
     (:foreground "magenta" :bold t :italic t))
    (((type tty pc) (class color) (background dark))
     (:foreground "yellow" :bold t :italic t))
    (t ()))
  "`diff-mode' face used to highlight removed lines."
  :group 'diff-mode)
(defvar diff-removed-face 'diff-removed-face)

(defface diff-added-face
  ;; XEmacs change: no :inherit in 21.4; copy/pasted `diff-changed-face'.
  '((((type tty pc) (class color) (background light))
     (:foreground "magenta" :bold t :italic t))
    (((type tty pc) (class color) (background dark))
     (:foreground "yellow" :bold t :italic t))
    (t ()))
  "`diff-mode' face used to highlight added lines."
  :group 'diff-mode)
(defvar diff-added-face 'diff-added-face)

(defface diff-changed-face
  '((((type tty pc) (class color) (background light))
     (:foreground "magenta" :bold t :italic t))
    (((type tty pc) (class color) (background dark))
     (:foreground "yellow" :bold t :italic t))
    (t ()))
  "`diff-mode' face used to highlight changed lines."
  :group 'diff-mode)
(defvar diff-changed-face 'diff-changed-face)

(defface diff-function-face
  ;; XEmacs change: no :inherit in 21.4; copy/pasted `diff-context-face'.
  '((((class color) (background light))
     (:foreground "grey50"))
    (((class color) (background dark))
     (:foreground "grey70"))
    (t ))
  "`diff-mode' face used to highlight function names produced by \"diff -p\"."
  :group 'diff-mode)
(defvar diff-function-face 'diff-function-face)

(defface diff-context-face
  '((((class color) (background light))
     (:foreground "grey50"))
    (((class color) (background dark))
     (:foreground "grey70"))
    (t ))
  "`diff-mode' face used to highlight context and other side-information."
  :group 'diff-mode)
(defvar diff-context-face 'diff-context-face)

(defface diff-nonexistent-face
  ;; XEmacs change: no :inherit in 21.4; copy/pasted `diff-file-header-face'.
  '((((type tty pc) (class color) (background light))
     (:foreground "yellow" :bold t))
    (((type tty pc) (class color) (background dark))
     (:foreground "cyan" :bold t))
    (((class color) (background light))
     (:background "grey70" :bold t))
    (((class color) (background dark))
     (:background "grey60" :bold t))
    (t (:bold t)))			; :height 1.3
  "`diff-mode' face used to highlight nonexistent files in recursive diffs."
  :group 'diff-mode)
(defvar diff-nonexistent-face 'diff-nonexistent-face)

(defvar diff-font-lock-keywords
  '(("^\\(@@ -[0-9,]+ \\+[0-9,]+ @@\\)\\(.*\\)$" ;unified
     (1 diff-hunk-header-face)
     (2 diff-function-face))
    ("^--- .+ ----$" . diff-hunk-header-face) ;context
    ("^\\(\\*\\{15\\}\\)\\(.*\\)$"	;context
     (1 diff-hunk-header-face)
     (2 diff-function-face))
    ("^\\*\\*\\* .+ \\*\\*\\*\\*". diff-hunk-header-face) ;context
    ("^\\(---\\|\\+\\+\\+\\|\\*\\*\\*\\) \\(\\S-+\\)\\(.*[^*-]\\)?\n"
     (0 diff-header-face) (2 diff-file-header-face prepend))
    ("^[0-9,]+[acd][0-9,]+$" . diff-hunk-header-face)
    ("^!.*\n" . diff-changed-face)	;context
    ("^[+>].*\n" . diff-added-face)
    ("^[-<].*\n" . diff-removed-face)
    ("^Index: \\(.+\\).*\n" (0 diff-header-face) (1 diff-index-face prepend))
    ("^Only in .*\n" . diff-nonexistent-face)
    ("^#.*" . font-lock-string-face)
    ("^[^-=+*!<>].*\n" . diff-context-face)))

(defconst diff-font-lock-defaults
  '(diff-font-lock-keywords t nil nil nil (font-lock-multiline . nil)))

(defvar diff-imenu-generic-expression
  ;; Prefer second name as first is most likely to be a backup or
  ;; version-control name.  The [\t\n] at the end of the unidiff pattern
  ;; catches Debian source diff files (which lack the trailing date).
  '((nil "\\+\\+\\+\\ \\([^\t\n]+\\)[\t\n]" 1) ; unidiffs
    (nil "^--- \\([^\t\n]+\\)\t.*\n\\*" 1))) ; context diffs

;; XEmacs change: we don't have combine-after-change-calls.
(eval-when-compile
  (unless (fboundp 'combine-after-change-calls)
    (defmacro combine-after-change-calls (&rest body)
      "Execute `body'."
      `(progn ,@body))))

;; XEmacs change: we don't have delete-and-extract-region
(eval-when-compile
  (unless (fboundp 'delete-and-extract-region)
    (defun delete-and-extract-region (start end)
      "Delete the text between START and END and return it."
      (let ((str (buffer-substring start end)))
	(delete-region start end)
	str))))

;;;;
;;;; Compile support
;;;;

(defvar diff-file-regexp-alist
  '(("Index: \\(.+\\)" 1)))

(defvar diff-error-regexp-alist
  '(("@@ -\\([0-9]+\\),[0-9]+ \\+\\([0-9]+\\),[0-9]+ @@" nil 2)
    ("--- \\([0-9]+\\),[0-9]+ ----" nil 1)
    ("\\([0-9]+\\)\\(,[0-9]+\\)?[adc]\\([0-9]+\\)" nil 3)))

;;;;
;;;; Movement
;;;;

(defconst diff-hunk-header-re "^\\(@@ -[0-9,]+ \\+[0-9,]+ @@.*\\|\\*\\{15\\}.*\n\\*\\*\\* .+ \\*\\*\\*\\*\\|[0-9]+\\(,[0-9]+\\)?[acd][0-9]+\\(,[0-9]+\\)?\\)$")
(defconst diff-file-header-re (concat "^\\(--- .+\n\\+\\+\\+ \\|\\*\\*\\* .+\n--- \\|[^-+!<>0-9@* ]\\).+\n" (substring diff-hunk-header-re 1)))
(defvar diff-narrowed-to nil)

(defun diff-end-of-hunk (&optional style)
  (when (looking-at diff-hunk-header-re)
    (unless style
      ;; Especially important for unified (because headers are ambiguous).
      (setq style (cdr (assq (char-after) '((?@ . unified) (?* . context))))))
    (goto-char (match-end 0)))
  (let ((end (and (re-search-forward (case style
				       ;; A `unified' header is ambiguous.
				       (unified (concat "^[^-+# \\]\\|"
							diff-file-header-re))
				       (context "^[^-+#! \\]")
				       (normal "^[^<>#\\]")
				       (t "^[^-+#!<> \\]"))
				     nil t)
		  (match-beginning 0))))
    ;; The return value is used by easy-mmode-define-navigation.
    (goto-char (or end (point-max)))))

(defun diff-beginning-of-hunk ()
  (beginning-of-line)
  (unless (looking-at diff-hunk-header-re)
    (forward-line 1)
    (condition-case ()
	(re-search-backward diff-hunk-header-re)
      (error (error "Can't find the beginning of the hunk")))))

(defun diff-beginning-of-file ()
  (beginning-of-line)
  (unless (looking-at diff-file-header-re)
    (forward-line 2)
    (condition-case ()
	(re-search-backward diff-file-header-re)
      (error (error "Can't find the beginning of the file")))))

(defun diff-end-of-file ()
  (re-search-forward "^[-+#!<>0-9@* \\]" nil t)
  (re-search-forward (concat "^[^-+#!<>0-9@* \\]\\|" diff-file-header-re)
		     nil 'move)
  (if (match-beginning 1)
      (goto-char (match-beginning 1))
    (beginning-of-line)))

;; Define diff-{hunk,file}-{prev,next}
(easy-mmode-define-navigation
 diff-hunk diff-hunk-header-re "hunk" diff-end-of-hunk)
(easy-mmode-define-navigation
 diff-file diff-file-header-re "file" diff-end-of-hunk)

(defun diff-restrict-view (&optional arg)
  "Restrict the view to the current hunk.
If the prefix ARG is given, restrict the view to the current file instead."
  (interactive "P")
  (save-excursion
    (if arg (diff-beginning-of-file) (diff-beginning-of-hunk))
    (narrow-to-region (point)
		      (progn (if arg (diff-end-of-file) (diff-end-of-hunk))
			     (point)))
    (set (make-local-variable 'diff-narrowed-to) (if arg 'file 'hunk))))


(defun diff-hunk-kill ()
  "Kill current hunk."
  (interactive)
  (diff-beginning-of-hunk)
  (let ((start (point))
	(firsthunk (save-excursion
		     (ignore-errors
		       (diff-beginning-of-file) (diff-hunk-next) (point))))
	(nexthunk  (save-excursion
		     (ignore-errors
		       (diff-hunk-next) (point))))
	(nextfile (save-excursion
		    (ignore-errors
		      (diff-file-next) (point)))))
    (if (and firsthunk (= firsthunk start)
	     (or (null nexthunk)
		 (and nextfile (> nexthunk nextfile))))
	;; It's the only hunk for this file, so kill the file.
	(diff-file-kill)
      (diff-end-of-hunk)
      (kill-region start (point)))))

(defun diff-file-kill ()
  "Kill current file's hunks."
  (interactive)
  (diff-beginning-of-file)
  (let* ((start (point))
	 (prevhunk (save-excursion
		     (ignore-errors
		       (diff-hunk-prev) (point))))
	 (index (save-excursion
		  (re-search-backward "^Index: " prevhunk t))))
    (when index (setq start index))
    (diff-end-of-file)
    (if (looking-at "^\n") (forward-char 1)) ;`tla' generates such diffs.
    (kill-region start (point))))

(defun diff-kill-junk ()
  "Kill spurious empty diffs."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\(Index: .*\n\\)"
					"\\([^-+!* <>].*\n\\)*?"
					"\\(\\(Index:\\) \\|"
					diff-file-header-re "\\)")
				nil t)
	(delete-region (if (match-end 4) (match-beginning 0) (match-end 1))
		       (match-beginning 3))
	(beginning-of-line)))))

(defun diff-count-matches (re start end)
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (re-search-forward re end t) (incf n))
      n)))

(defun diff-split-hunk ()
  "Split the current (unified diff) hunk at point into two hunks."
  (interactive)
  (beginning-of-line)
  (let ((pos (point))
	(start (progn (diff-beginning-of-hunk) (point))))
    (unless (looking-at "@@ -\\([0-9]+\\),[0-9]+ \\+\\([0-9]+\\),[0-9]+ @@")
      (error "diff-split-hunk only works on unified context diffs"))
    (forward-line 1)
    (let* ((start1 (string-to-number (match-string 1)))
	   (start2 (string-to-number (match-string 2)))
	   (newstart1 (+ start1 (diff-count-matches "^[- \t]" (point) pos)))
	   (newstart2 (+ start2 (diff-count-matches "^[+ \t]" (point) pos))))
      (goto-char pos)
      ;; Hopefully the after-change-function will not screw us over.
      (insert "@@ -" (number-to-string newstart1) ",1 +"
	      (number-to-string newstart2) ",1 @@\n")
      ;; Fix the original hunk-header.
      (diff-fixup-modifs start pos))))


;;;;
;;;; jump to other buffers
;;;;

(defvar diff-remembered-files-alist nil)

(defun diff-filename-drop-dir (file)
  (when (string-match "/" file) (substring file (match-end 0))))

(defun diff-merge-strings (ancestor from to)
  "Merge the diff between ANCESTOR and FROM into TO.
Returns the merged string if successful or nil otherwise.
The strings are assumed not to contain any \"\\n\" (i.e. end of line).
If ANCESTOR = FROM, returns TO.
If ANCESTOR = TO, returns FROM.
The heuristic is simplistic and only really works for cases
like \(diff-merge-strings \"b/foo\" \"b/bar\" \"/a/c/foo\")."
  ;; Ideally, we want:
  ;;   AMB ANB CMD -> CND
  ;; but that's ambiguous if `foo' or `bar' is empty:
  ;; a/foo a/foo1 b/foo.c -> b/foo1.c but not 1b/foo.c or b/foo.c1
  (let ((str (concat ancestor "\n" from "\n" to)))
    (when (and (string-match (concat
			      "\\`\\(.*?\\)\\(.*\\)\\(.*\\)\n"
			      "\\1\\(.*\\)\\3\n"
			      "\\(.*\\(\\2\\).*\\)\\'") str)
	       (equal to (match-string 5 str)))
      (concat (substring str (match-beginning 5) (match-beginning 6))
	      (match-string 4 str)
	      (substring str (match-end 6) (match-end 5))))))

(defun diff-find-file-name (&optional old)
  "Return the file corresponding to the current patch.
Non-nil OLD means that we want the old file."
  (save-excursion
    (unless (looking-at diff-file-header-re)
      (or (ignore-errors (diff-beginning-of-file))
	  (re-search-forward diff-file-header-re nil t)))
    (let* ((limit (save-excursion
		   (condition-case ()
		       (progn (diff-hunk-prev) (point))
		     (error (point-min)))))
	   (header-files
	    (if (looking-at "[-*][-*][-*] \\(\\S-+\\)\\(\\s-.*\\)?\n[-+][-+][-+] \\(\\S-+\\)")
		(list (if old (match-string 1) (match-string 3))
		      (if old (match-string 3) (match-string 1)))
	      (forward-line 1) nil))
	   (fs (append
		(when (save-excursion
			(re-search-backward "^Index: \\(.+\\)" limit t))
		  (list (match-string 1)))
		header-files
		(when (re-search-backward "^diff \\(-\\S-+ +\\)*\\(\\S-+\\)\\( +\\(\\S-+\\)\\)?" nil t)
		  (list (if old (match-string 2) (match-string 4))
			(if old (match-string 4) (match-string 2))))))
	   (fs (delq nil fs)))
      (or
       ;; use any previously used preference
       (cdr (assoc fs diff-remembered-files-alist))
       ;; try to be clever and use previous choices as an inspiration
       (dolist (rf diff-remembered-files-alist)
	 (let ((newfile (diff-merge-strings (caar rf) (car fs) (cdr rf))))
	   (if (and newfile (file-exists-p newfile)) (return newfile))))
       ;; look for each file in turn.  If none found, try again but
       ;; ignoring the first level of directory, ...
       (do* ((files fs (delq nil (mapcar 'diff-filename-drop-dir files)))
	     (file nil nil))
	   ((or (null files)
		(setq file (do* ((files files (cdr files))
				 (file (car files) (car files)))
			       ((or (null file) (file-exists-p file))
				file))))
	    file))
       ;; <foo>.rej patches implicitly apply to <foo>
       (and (string-match "\\.rej\\'" (or buffer-file-name ""))
	    (let ((file (substring buffer-file-name 0 (match-beginning 0))))
	      (when (file-exists-p file) file)))
       ;; if all else fails, ask the user
       (let ((file (read-file-name (format "Use file %s: " (or (first fs) ""))
				   nil (first fs) t (first fs))))
	 (set (make-local-variable 'diff-remembered-files-alist)
	      (cons (cons fs file) diff-remembered-files-alist))
	 file)))))


(defun diff-mouse-goto-source (event)
  "Run `diff-goto-source' for the diff at a mouse click."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (diff-goto-source)))


(defun diff-ediff-patch ()
  "Call `ediff-patch-file' on the current buffer."
  (interactive)
  (condition-case err
      (ediff-patch-file nil (current-buffer))
    (wrong-number-of-arguments (ediff-patch-file))))

;;;;
;;;; Conversion functions
;;;;

;;(defvar diff-inhibit-after-change nil
;;  "Non-nil means inhibit `diff-mode's after-change functions.")

(defun diff-unified->context (start end)
  "Convert unified diffs to context diffs.
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole bufer."
  (interactive (if current-prefix-arg
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (unless (markerp end) (setq end (copy-marker end)))
  (let (;;(diff-inhibit-after-change t)
	(inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward "^\\(\\(---\\) .+\n\\(\\+\\+\\+\\) .+\\|@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@.*\\)$" nil t)
		  (< (point) end))
	(combine-after-change-calls
	  (if (match-beginning 2)
	      ;; we matched a file header
	      ;; XEmacs change: don't use `replace-match'
	      ;; which has a different API there.
	      (save-excursion
		;; use reverse order to make sure the indices are kept valid
		(delete-region (match-beginning 3) (match-end 3))
		(goto-char (match-beginning 3))
		(insert-string "---")
		(delete-region (match-beginning 2) (match-end 2))
		(goto-char (match-beginning 2))
		(insert-string "***"))
	    ;; we matched a hunk header
	    (let ((line1 (match-string 4))
		  (lines1 (match-string 5))
		  (line2 (match-string 6))
		  (lines2 (match-string 7)))
	      (replace-match
	       (concat "***************\n*** " line1 ","
		       (number-to-string (+ (string-to-number line1)
					    (string-to-number lines1)
					    -1)) " ****"))
	      (forward-line 1)
	      (save-restriction
		(narrow-to-region (point)
				  (progn (diff-end-of-hunk 'unified) (point)))
		(let ((hunk (buffer-string)))
		  (goto-char (point-min))
		  (if (not (save-excursion (re-search-forward "^-" nil t)))
		      (delete-region (point) (point-max))
		    (goto-char (point-max))
		    (let ((modif nil) last-pt)
		      (while (progn (setq last-pt (point))
				    (= (forward-line -1) 0))
			(case (char-after)
			  (?  (insert " ") (setq modif nil) (backward-char 1))
			  (?+ (delete-region (point) last-pt) (setq modif t))
			  (?- (if (not modif)
				  (progn (forward-char 1)
					 (insert " "))
				(delete-char 1)
				(insert "! "))
			      (backward-char 2))
			  (?\\ (when (save-excursion (forward-line -1)
						     (= (char-after) ?+))
				 (delete-region (point) last-pt) (setq modif t)))
			  (t (setq modif nil))))))
		  (goto-char (point-max))
		  (save-excursion
		    (insert "--- " line2 ","
			    (number-to-string (+ (string-to-number line2)
						 (string-to-number lines2)
						 -1)) " ----\n" hunk))
		  ;;(goto-char (point-min))
		  (forward-line 1)
		  (if (not (save-excursion (re-search-forward "^+" nil t)))
		      (delete-region (point) (point-max))
		    (let ((modif nil) (delete nil))
		      (while (not (eobp))
			(case (char-after)
			  (?  (insert " ") (setq modif nil) (backward-char 1))
			  (?- (setq delete t) (setq modif t))
			  (?+ (if (not modif)
				  (progn (forward-char 1)
					 (insert " "))
				(delete-char 1)
				(insert "! "))
			      (backward-char 2))
			  (?\\ (when (save-excursion (forward-line 1)
						     (not (eobp)))
				 (setq delete t) (setq modif t)))
			  (t (setq modif nil)))
			(let ((last-pt (point)))
			  (forward-line 1)
			  (when delete
			    (delete-region last-pt (point))
			    (setq delete nil)))))))))))))))

(defun diff-context->unified (start end)
  "Convert context diffs to unified diffs.
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole bufer."
  (interactive (if current-prefix-arg
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (unless (markerp end) (setq end (copy-marker end)))
  (let (;;(diff-inhibit-after-change t)
	(inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward "^\\(\\(\\*\\*\\*\\) .+\n\\(---\\) .+\\|\\*\\{15\\}.*\n\\*\\*\\* \\([0-9]+\\),\\(-?[0-9]+\\) \\*\\*\\*\\*\\)$" nil t)
		  (< (point) end))
	(combine-after-change-calls
	  (if (match-beginning 2)
	      ;; we matched a file header
	      ;; XEmacs change: don't use `replace-match'
	      ;; which has a different API there.
	      (save-excursion
	       ;; use reverse order to make sure the indices are kept valid
		(delete-region (match-beginning 3) (match-end 3))
		(goto-char (match-beginning 3))
		(insert-string "+++")
		(delete-region (match-beginning 2) (match-end 2))
		(goto-char (match-beginning 2))
		(insert-string "---"))
	    ;; we matched a hunk header
	    (let ((line1s (match-string 4))
		  (line1e (match-string 5))
		  (pt1 (match-beginning 0)))
	      (replace-match "")
	      (unless (re-search-forward
		       "^--- \\([0-9]+\\),\\(-?[0-9]+\\) ----$" nil t)
		(error "Can't find matching `--- n1,n2 ----' line"))
	      (let ((line2s (match-string 1))
		    (line2e (match-string 2))
		    (pt2 (progn
			   (delete-region (progn (beginning-of-line) (point))
					  (progn (forward-line 1) (point)))
			   (point-marker))))
		(goto-char pt1)
		(forward-line 1)
		(while (< (point) pt2)
		  (case (char-after)
		    ((?! ?-) (delete-char 2) (insert "-") (forward-line 1))
		    (?\ 		;merge with the other half of the chunk
		     (let* ((endline2
			     (save-excursion
			       (goto-char pt2) (forward-line 1) (point)))
			    (c (char-after pt2)))
		       (case c
			 ((?! ?+)
			  (insert "+"
				  (prog1 (buffer-substring (+ pt2 2) endline2)
				    (delete-region pt2 endline2))))
			 (?\ 		;FIXME: check consistency
			  (delete-region pt2 endline2)
			  (delete-char 1)
			  (forward-line 1))
			 (?\\ (forward-line 1))
			 (t (delete-char 1) (forward-line 1)))))
		    (t (forward-line 1))))
		(while (looking-at "[+! ] ")
		  (if (/= (char-after) ?!) (forward-char 1)
		    (delete-char 1) (insert "+"))
		  (delete-char 1) (forward-line 1))
		(save-excursion
		  (goto-char pt1)
		  (insert "@@ -" line1s ","
			  (number-to-string (- (string-to-number line1e)
					       (string-to-number line1s)
					       -1))
			  " +" line2s ","
			  (number-to-string (- (string-to-number line2e)
					       (string-to-number line2s)
					       -1)) " @@"))))))))))

;; XEmacs change: extracted from `diff-reverse-direction'
(defvar diff-patch-start-regexp
  (concat
   ;; start
   "^\\("
   ;; Typical start of diff
   ;; --- file
   ;; +++ file
   "\\([-*][-*][-*] \\)\\(.+\\)\n"
   "\\([-+][-+][-+] \\)\\(.+\\)"
   ;; or
   "\\|"
   ;; I'm not sure
   "\\*\\{15\\}.*\n"
   "\\*\\*\\* \\(.+\\) \\*\\*\\*\\*"
   ;; or
   "\\|"
   ;; hunk
   "@@ -\\([0-9,]+\\) \\+\\([0-9,]+\\) @@.*"
   ;; end
   "\\)$"))


;; XEmacs change: use region (mark) if set, do whole buffer otherwise
(defun diff-reverse-direction (start end)
  "Reverse the direction of the diffs.
START and END are either taken from the region (if it is set) or
else cover the whole buffer."
  (interactive (if (mark)
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (unless (markerp end) (setq end (copy-marker end)))
  (let (;;(diff-inhibit-after-change t)
	(inhibit-read-only t))
    (save-excursion
      (goto-char start)
      ;; XEmacs change: use `diff-patch-start-regexp'
      (while (and (re-search-forward diff-patch-start-regexp nil t)
		  (< (point) end))
	(combine-after-change-calls
	  (cond
	   ;; a file header
	   ((match-beginning 2) (replace-match "\\2\\5\n\\4\\3" nil))
	   ;; a context-diff hunk header
	   ((match-beginning 6)
	    (let ((pt-lines1 (match-beginning 6))
		  (lines1 (match-string 6)))
	      (replace-match "" nil nil nil 6)
	      (forward-line 1)
	      (let ((half1s (point)))
		(while (looking-at "[-! \\][ \t]\\|#")
		  (when (= (char-after) ?-) (delete-char 1) (insert "+"))
		  (forward-line 1))
		(let ((half1 (delete-and-extract-region half1s (point))))
		  (unless (looking-at "^--- \\([0-9]+,-?[0-9]+\\) ----$")
		    (insert half1)
		    (error "Can't find matching `--- n1,n2 ----' line"))
		  (let ((str1 (match-string 1)))
		    (replace-match lines1 nil nil nil 1)
		    (forward-line 1)
		    (let ((half2s (point)))
		      (while (looking-at "[!+ \\][ \t]\\|#")
			(when (= (char-after) ?+) (delete-char 1) (insert "-"))
			(forward-line 1))
		      (let ((half2 (delete-and-extract-region half2s (point))))
			(insert (or half1 ""))
			(goto-char half1s)
			(insert (or half2 ""))))
		    (goto-char pt-lines1)
		    (insert str1))))))
	   ;; a unified-diff hunk header
	   ((match-beginning 7)
	    (replace-match "@@ -\\8 +\\7 @@" nil)
	    (forward-line 1)
	    (let ((c (char-after)) first last)
	      (while (case (setq c (char-after))
		       ;; XEmacs change: check for --- and +++ before reversing
		       (?- (if (and (char= (char-after (+ 1 (point)))
                                           ?-)
                                    (char= (char-after (+ 2 (point)))
                                           ?-))
                               nil
                               (progn
                                 (setq first (or first (point)))
                                 (delete-char 1)
                                 (insert "+")
                                 t)))
		       (?+ (if (and (char= (char-after (+ 1 (point)))
                                           ?+)
                                    (char= (char-after (+ 2 (point)))
                                           ?+))
                               nil
                               (progn
                                 (setq last (or last (point)))
                                 (delete-char 1)
                                 (insert "-")
                                 t)))
		       ((?\\ ?#) t)
		       (t (when (and first last (< first last))
			    (insert (delete-and-extract-region first last)))
			  (setq first nil last nil)
			  (equal ?\  c)))
		(forward-line 1))))))))))

(defun diff-fixup-modifs (start end)
  "Fixup the hunk headers (in case the buffer was modified).
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole buffer."
  (interactive (if current-prefix-arg
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char end) (diff-end-of-hunk)
      (let ((plus 0) (minus 0) (space 0) (bang 0))
	(while (and (= (forward-line -1) 0) (<= start (point)))
	  (if (not (looking-at
		    (concat "@@ -[0-9,]+ \\+[0-9,]+ @@"
			    "\\|[-*][-*][-*] [0-9,]+ [-*][-*][-*][-*]$"
			    "\\|--- .+\n\\+\\+\\+ ")))
	      (case (char-after)
		(?\  (incf space))
		(?+ (incf plus))
		(?- (incf minus))
		(?! (incf bang))
		((?\\ ?#) nil)
		(t  (setq space 0 plus 0 minus 0 bang 0)))
	    (cond
	     ((looking-at "@@ -[0-9]+,\\([0-9]*\\) \\+[0-9]+,\\([0-9]*\\) @@.*$")
	      (let* ((old1 (match-string 1))
		     (old2 (match-string 2))
		     (new1 (number-to-string (+ space minus)))
		     (new2 (number-to-string (+ space plus))))
		(unless (string= new2 old2) (replace-match new2 t t nil 2))
		(unless (string= new1 old1) (replace-match new1 t t nil 1))))
	     ((looking-at "--- \\([0-9]+\\),\\([0-9]*\\) ----$")
	      (when (> (+ space bang plus) 0)
		(let* ((old1 (match-string 1))
		       (old2 (match-string 2))
		       (new (number-to-string
			     (+ space bang plus -1 (string-to-number old1)))))
		  (unless (string= new old2) (replace-match new t t nil 2)))))
	     ((looking-at "\\*\\*\\* \\([0-9]+\\),\\(-?[0-9]*\\) \\*\\*\\*\\*$")
	      (when (> (+ space bang minus) 0)
		(let* ((old (match-string 1))
		       (new (format
			     (concat "%0" (number-to-string (length old)) "d")
			     (+ space bang minus -1 (string-to-number old)))))
		  (unless (string= new old) (replace-match new t t nil 2))))))
	    (setq space 0 plus 0 minus 0 bang 0)))))))

;;;;
;;;; Hooks
;;;;

(defun diff-write-contents-hooks ()
  "Fixup hunk headers if necessary."
  (if (buffer-modified-p) (diff-fixup-modifs (point-min) (point-max)))
  nil)

;; XEmacs change: we don't seem to have this feature.
(defvar undo-in-progress nil)

;; It turns out that making changes in the buffer from within an
;; *-change-function is asking for trouble, whereas making them
;; from a post-command-hook doesn't pose much problems
(defvar diff-unhandled-changes nil)
(defun diff-after-change-function (beg end len)
  "Remember to fixup the hunk header.
See `after-change-functions' for the meaning of BEG, END and LEN."
  ;; Ignoring changes when inhibit-read-only is set is strictly speaking
  ;; incorrect, but it turns out that inhibit-read-only is normally not set
  ;; inside editing commands, while it tends to be set when the buffer gets
  ;; updated by an async process or by a conversion function, both of which
  ;; would rather not be uselessly slowed down by this hook.
  (when (and (not undo-in-progress) (not inhibit-read-only))
    (if diff-unhandled-changes
	(setq diff-unhandled-changes
	      (cons (min beg (car diff-unhandled-changes))
		    (max end (cdr diff-unhandled-changes))))
      (setq diff-unhandled-changes (cons beg end)))))

(defun diff-post-command-hook ()
  "Fixup hunk headers if necessary."
  (when (consp diff-unhandled-changes)
    (ignore-errors
      (save-excursion
	(goto-char (car diff-unhandled-changes))
	(unless (ignore-errors
		  (diff-beginning-of-hunk)
		  (save-excursion
		    (diff-end-of-hunk)
		    (> (point) (car diff-unhandled-changes))))
	  (goto-char (car diff-unhandled-changes))
	  (re-search-forward diff-hunk-header-re (cdr diff-unhandled-changes))
	  (diff-beginning-of-hunk))
	(diff-fixup-modifs (point) (cdr diff-unhandled-changes))))
    (setq diff-unhandled-changes nil)))

;; XEmacs change: no autoload for this for the transition period
(defadvice vc-backend-diff (around diff-mode-vc activate)
  ;; BEWARE!! when this is autoloaded, CL might not be available
  (condition-case ()
      (with-current-buffer "*vc-diff*" (setq buffer-read-only nil))
    (error nil))
  ad-do-it
  (condition-case ()
      (with-current-buffer "*vc-diff*"
        (if (or (eq major-mode default-major-mode)
                (memq major-mode '(fundamental-mode text-mode diff-mode)))
            (diff-mode))
        (if (eq major-mode 'diff-mode) (font-lock-mode)))
    (error nil)))


;;;; 
;;;; The main function
;;;; 

;; XEmacs hack: autoload a dummy autoload instead of a derived mode
;;;###autoload(autoload 'diff-mode "diff-mode" nil t)
(define-derived-mode diff-mode fundamental-mode "Diff"
  "Major mode for viewing/editing context diffs.
Supports unified and context diffs as well as (to a lesser extent)
normal diffs.
When the buffer is read-only, the ESC prefix is not necessary."
  (set (make-local-variable 'font-lock-defaults) diff-font-lock-defaults)
  (set (make-local-variable 'outline-regexp) diff-outline-regexp)
  (set (make-local-variable 'imenu-generic-expression)
       diff-imenu-generic-expression)
  ;; These are not perfect.  They would be better done separately for
  ;; context diffs and unidiffs.
  ;; (set (make-local-variable 'paragraph-start)
  ;;        (concat "@@ "			; unidiff hunk
  ;; 	       "\\|\\*\\*\\* "		; context diff hunk or file start
  ;; 	       "\\|--- [^\t]+\t"))	; context or unidiff file
  ;; 					; start (first or second line)
  ;;   (set (make-local-variable 'paragraph-separate) paragraph-start)
  ;;   (set (make-local-variable 'page-delimiter) "--- [^\t]+\t")
  ;; compile support

  ;;;; compile support is not good enough yet.  Also it can be annoying
  ;; and should thus only be enabled conditionally.
  ;; (set (make-local-variable 'compilation-file-regexp-alist)
  ;;      diff-file-regexp-alist)
  ;; (set (make-local-variable 'compilation-error-regexp-alist)
  ;;      diff-error-regexp-alist)
  ;; (when (string-match "\\.rej\\'" (or buffer-file-name ""))
  ;;   (set (make-local-variable 'compilation-current-file)
  ;; 	 (substring buffer-file-name 0 (match-beginning 0))))
  ;; (compilation-shell-minor-mode 1)

  (when (and (> (point-max) (point-min)) diff-default-read-only)
    (toggle-read-only t))
  ;; setup change hooks
  (if (not diff-update-on-the-fly)
      (add-hook 'write-contents-hooks 'diff-write-contents-hooks)
    (make-local-variable 'diff-unhandled-changes)
    ;; XEmacs change: make local
    (make-local-hook 'after-change-functions)
    (add-hook 'after-change-functions 'diff-after-change-function nil t)
    ;; XEmacs change: make local
    (make-local-hook 'post-command-hook)
    (add-hook 'post-command-hook 'diff-post-command-hook nil t))

  ;; Neat trick from Dave Love to add more bindings in read-only mode:
  ;; XEmacs change: we don't have minor-mode-overriding-map-alist
  (add-to-list (make-local-variable 'minor-mode-map-alist)
	       (cons 'buffer-read-only diff-mode-shared-map))
  ;; add-log support
  (set (make-local-variable 'add-log-current-defun-function)
       'diff-current-defun)
  (set (make-local-variable 'add-log-buffer-file-name-function)
       'diff-find-file-name))

;; XEmacs hack: autoload a dummy autoload instead of a minor mode
;;;###autoload(autoload 'diff-minor-mode "diff-mode" nil t)
(define-minor-mode diff-minor-mode
  "Minor mode for viewing/editing context diffs.
\\{diff-minor-mode-map}"
  nil " Diff" nil
  ;; FIXME: setup font-lock
  ;; setup change hooks
  (if (not diff-update-on-the-fly)
      (add-hook 'write-contents-hooks 'diff-write-contents-hooks)
    (make-local-variable 'diff-unhandled-changes)
    (add-hook 'after-change-functions 'diff-after-change-function nil t)
    (add-hook 'post-command-hook 'diff-post-command-hook nil t)))


;;;
;;; Misc operations that have proved useful at some point.
;;;

(defun diff-next-complex-hunk ()
  "Jump to the next \"complex\" hunk.
\"Complex\" is approximated by \"the hunk changes the number of lines\".
Only works for unified diffs."
  (interactive)
  (while
      (and (re-search-forward "^@@ [-0-9]+,\\([0-9]+\\) [+0-9]+,\\([0-9]+\\) @@"
			      nil t)
	   (equal (match-string 1) (match-string 2)))))

(defun diff-hunk-text (hunk destp &optional char-offset)
  "Return the literal source text from HUNK.
if DESTP is nil return the source, otherwise the destination text.
If CHAR-OFFSET is non-nil, it should be a char-offset in
HUNK, and instead of a string, a cons cell is returned whose car is the
appropriate text, and whose cdr is the corresponding char-offset in that text."
  (with-temp-buffer
    (insert hunk)
    (goto-char (point-min))
    (let ((src-pos nil)
	  (dst-pos nil)
	  (divider-pos nil)
	  (num-pfx-chars 2))
      ;; Set the following variables:
      ;;  SRC-POS     buffer pos of the source part of the hunk or nil if none
      ;;  DST-POS     buffer pos of the destination part of the hunk or nil
      ;;  DIVIDER-POS buffer pos of any divider line separating the src & dst
      ;;  NUM-PFX-CHARS  number of line-prefix characters used by this format"
      (cond ((looking-at "^@@")
	     ;; unified diff
	     (setq num-pfx-chars 1)
	     (forward-line 1)
	     (setq src-pos (point) dst-pos (point)))
	    ((looking-at "^\\*\\*")
	     ;; context diff
	     (forward-line 2)
	     (setq src-pos (point))
	     (re-search-forward "^--- " nil t)
	     (forward-line 0)
	     (setq divider-pos (point))
	     (forward-line 1)
	     (setq dst-pos (point)))
	    ((looking-at "^[0-9]+a[0-9,]+$")
	     ;; normal diff, insert
	     (forward-line 1)
	     (setq dst-pos (point)))
	    ((looking-at "^[0-9,]+d[0-9]+$")
	     ;; normal diff, delete
	     (forward-line 1)
	     (setq src-pos (point)))
	    ((looking-at "^[0-9,]+c[0-9,]+$")
	     ;; normal diff, change
	     (forward-line 1)
	     (setq src-pos (point))
	     (re-search-forward "^---$" nil t)
	     (forward-line 0)
	     (setq divider-pos (point))
	     (forward-line 1)
	     (setq dst-pos (point)))
	    (t
	     (error "Unknown diff hunk type")))

      (if (if destp (null dst-pos) (null src-pos))
	  ;; Implied empty text
	  (if char-offset '("" . 0) "")

	;; For context diffs, either side can be empty, (if there's only
	;; added or only removed text).  We should then use the other side.
	(cond ((equal src-pos divider-pos) (setq src-pos dst-pos))
	      ((equal dst-pos (point-max)) (setq dst-pos src-pos)))

	(when char-offset (goto-char (+ (point-min) char-offset)))

	;; Get rid of anything except the desired text.
	(save-excursion
	  ;; Delete unused text region
	  (let ((keep (if destp dst-pos src-pos)))
	    (when (and divider-pos (> divider-pos keep))
	      (delete-region divider-pos (point-max)))
	    (delete-region (point-min) keep))
	  ;; Remove line-prefix characters, and unneeded lines (unified diffs).
	  (let ((kill-char (if destp ?- ?+)))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (if (eq (char-after) kill-char)
		  (delete-region (point) (progn (forward-line 1) (point)))
		(delete-char num-pfx-chars)
		(forward-line 1)))))

	(let ((text (buffer-substring-no-properties (point-min) (point-max))))
	  (if char-offset (cons text (- (point) (point-min))) text))))))


(defun diff-find-text (text)
  "Return the buffer position of the nearest occurrence of TEXT.
If TEXT isn't found, nil is returned."
  (let* ((orig (point))
	 (forw (and (search-forward text nil t)
			  (match-beginning 0)))
	 (back (and (goto-char (+ orig (length text)))
		    (search-backward text nil t)
			  (match-beginning 0))))
	  ;; Choose the closest match.
    (if (and forw back)
	(if (> (- forw orig) (- orig back)) back forw)
      (or back forw))))

(defsubst diff-xor (a b) (if a (not b) b))

(defun diff-find-source-location (&optional other-file reverse)
  "Find out (BUF LINE-OFFSET POS SRC DST SWITCHED)."
  (save-excursion
    (let* ((other (diff-xor other-file diff-jump-to-old-file))
	   (char-offset (- (point) (progn (diff-beginning-of-hunk) (point))))
	   (hunk (buffer-substring (point)
				   (save-excursion (diff-end-of-hunk) (point))))
	   (old (diff-hunk-text hunk reverse char-offset))
	   (new (diff-hunk-text hunk (not reverse) char-offset))
	   ;; Find the location specification.
	   (line (if (not (looking-at "\\(?:\\*\\{15\\}.*\n\\)?[-@* ]*\\([0-9,]+\\)\\([ acd+]+\\([0-9,]+\\)\\)?"))
		     (error "Can't find the hunk header")
		   (if other (match-string 1)
		     (if (match-end 3) (match-string 3)
		       (unless (re-search-forward "^--- \\([0-9,]+\\)" nil t)
			 (error "Can't find the hunk separator"))
		       (match-string 1)))))
	   (file (or (diff-find-file-name other) (error "Can't find the file")))
	   (buf (find-file-noselect file)))
      ;; Update the user preference if he so wished.
      (when (> (prefix-numeric-value other-file) 8)
	(setq diff-jump-to-old-file other))
      (with-current-buffer buf
	(goto-line (string-to-number line))
	(let* ((orig-pos (point))
	       (pos (diff-find-text (car old)))
	       (switched nil))
	  (when (null pos)
	    (setq pos (diff-find-text (car new)) switched t))
	  (nconc
	   (list buf)
	   (if pos (list (count-lines orig-pos pos) pos) (list nil orig-pos))
	   (if switched (list new old t) (list old new))))))))


(defun diff-hunk-status-msg (line-offset reversed dry-run)
  (let ((msg (if dry-run
		 (if reversed "already applied" "not yet applied")
	       (if reversed "undone" "applied"))))
    (message (cond ((null line-offset) "Hunk text not found")
		   ((= line-offset 0) "Hunk %s")
		   ((= line-offset 1) "Hunk %s at offset %d line")
		   (t "Hunk %s at offset %d lines"))
	     msg line-offset)))


(defun diff-apply-hunk (&optional reverse)
  "Apply the current hunk to the source file and go to the next.
By default, the new source file is patched, but if the variable
`diff-jump-to-old-file' is non-nil, then the old source file is
patched instead (some commands, such as `diff-goto-source' can change
the value of this variable when given an appropriate prefix argument).

With a prefix argument, REVERSE the hunk."
  (interactive "P")
  (destructuring-bind (buf line-offset pos old new &optional switched)
      ;; If REVERSE go to the new file, otherwise go to the old.
      (diff-find-source-location (not reverse) reverse)
    (cond
     ((null line-offset)
      (error "Can't find the text to patch"))
     ((and switched
	   ;; A reversed patch was detected, perhaps apply it in reverse.
	   (not (save-window-excursion
		  (pop-to-buffer buf)
		  (goto-char (+ pos (cdr old)))
		  (y-or-n-p
		   (if reverse
		       "Hunk hasn't been applied yet; apply it now? "
		     "Hunk has already been applied; undo it? ")))))
      (message "(Nothing done)"))
     (t
      ;; Apply the hunk
      (with-current-buffer buf
	(goto-char pos)
	(delete-char (length (car old)))
	(insert (car new)))
      ;; Display BUF in a window
      (set-window-point (display-buffer buf) (+ pos (cdr new)))
      (diff-hunk-status-msg line-offset (diff-xor switched reverse) nil)
      (when diff-advance-after-apply-hunk
	(diff-hunk-next))))))


(defun diff-test-hunk (&optional reverse)
  "See whether it's possible to apply the current hunk.
With a prefix argument, try to REVERSE the hunk."
  (interactive "P")
  (destructuring-bind (buf line-offset pos src dst &optional switched)
      ;; If REVERSE go to the new file, otherwise go to the old.
      (diff-find-source-location (not reverse) reverse)
    (set-window-point (display-buffer buf) (+ pos (cdr src)))
    (diff-hunk-status-msg line-offset (diff-xor reverse switched) t)))


(defun diff-goto-source (&optional other-file)
  "Jump to the corresponding source line.
`diff-jump-to-old-file' (or its opposite if the OTHER-FILE prefix arg
is given) determines whether to jump to the old or the new file.
If the prefix arg is bigger than 8 (for example with \\[universal-argument] \\[universal-argument])
  then `diff-jump-to-old-file' is also set, for the next invocations."
  (interactive "P")
  ;; When pointing at a removal line, we probably want to jump to
  ;; the old location, and else to the new (i.e. as if reverting).
  ;; This is a convenient detail when using smerge-diff.
  (let ((rev (not (save-excursion (beginning-of-line) (looking-at "[-<]")))))
    (destructuring-bind (buf line-offset pos src dst &optional switched)
	(diff-find-source-location other-file rev)
      (pop-to-buffer buf)
      (goto-char (+ pos (cdr src)))
      (diff-hunk-status-msg line-offset (diff-xor rev switched) t))))


(defun diff-current-defun ()
  "Find the name of function at point.
For use in `add-log-current-defun-function'."
  (destructuring-bind (buf line-offset pos src dst &optional switched)
      (diff-find-source-location)
    (save-excursion
      (beginning-of-line)
      (or (when (memq (char-after) '(?< ?-))
	    ;; Cursor is pointing at removed text.  This could be a removed
	    ;; function, in which case, going to the source buffer will
	    ;; not help since the function is now removed.  Instead,
	    ;; try to figure out the function name just from the code-fragment.
	    (let ((old (if switched dst src)))
	      (with-temp-buffer
		(insert (car old))
		(goto-char (cdr old))
		(funcall (with-current-buffer buf major-mode))
		(add-log-current-defun))))
	  (with-current-buffer buf
	    (goto-char (+ pos (cdr src)))
	    (add-log-current-defun))))))

;; XEmacs change: added.
;;;###autoload(add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))

;; provide the package
(provide 'diff-mode)

;;; Old Change Log from when diff-mode wasn't part of Emacs:
;; Revision 1.11  1999/10/09 23:38:29  monnier
;; (diff-mode-load-hook): dropped.
;; (auto-mode-alist): also catch *.diffs.
;; (diff-find-file-name, diff-mode):  add smarts to find the right file
;;     for *.rej files (that lack any file name indication).
;;
;; Revision 1.10  1999/09/30 15:32:11  monnier
;; added support for "\ No newline at end of file".
;;
;; Revision 1.9  1999/09/15 00:01:13  monnier
;; - added basic `compile' support.
;; - have diff-kill-hunk call diff-kill-file if it's the only hunk.
;; - diff-kill-file now tries to kill the leading garbage as well.
;;
;; Revision 1.8  1999/09/13 21:10:09  monnier
;; - don't use CL in the autoloaded code
;; - accept diffs using -T
;;
;; Revision 1.7  1999/09/05 20:53:03  monnier
;; interface to ediff-patch
;;
;; Revision 1.6  1999/09/01 20:55:13  monnier
;; (ediff=patch-file):  add bindings to call ediff-patch.
;; (diff-find-file-name):  taken out of diff-goto-source.
;; (diff-unified->context, diff-context->unified, diff-reverse-direction,
;;  diff-fixup-modifs):  only use the region if a prefix arg is given.
;;
;; Revision 1.5  1999/08/31 19:18:52  monnier
;; (diff-beginning-of-file, diff-prev-file):  fixed wrong parenthesis.
;;
;; Revision 1.4  1999/08/31 13:01:44  monnier
;; use `combine-after-change-calls' to minimize the slowdown of font-lock.
;;

;; arch-tag: 2571d7ff-bc28-4cf9-8585-42e21890be66
;;; diff-mode.el ends here
