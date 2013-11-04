;;; bench.el --- benchmarking utility for emacsen

;; Copyright (C) 1987,88,89,90,93,94,95,96 Free Software Foundation, Inc.
;; $Id: bench.el,v 1.4 2001/09/08 11:01:26 adrian Exp $	
;; $Source: /pack/xemacscvs/XEmacs/packages/xemacs-packages/xemacs-devel/bench.el,v $
;; $Revision: 1.4 $
;; $Author: adrian $
;; $Date: 2001/09/08 11:01:26 $

;; Author: Shane Holder <holder@rsn.hp.com>
;; Adapted-By: Steve Baur <steve@xemacs.org>
;; Further adapted by: Shane Holder <holder@rsn.hp.com>
;; Keywords: internal, maint

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

;;; Commentary:

;; Adapted from Shane Holder's bench.el by steve@xemacs.org.

;; To run
;; Extract the shar file in /tmp, or modify bench-lisp-file to
;; point to the gnus.el file.
;; At the shell prompt emacs -q --no-site-file <= don't load users .emacs or site-file
;; M-x byte-compile-file "/tmp/bench.el"
;; M-x load-file "/tmp/bench.elc"
;; In the scratch buffer (bench 1)


;; All bench marks must be named bench-mark-<something>
;; Results are put in bench-mark-<something-times which is a list of
;;  times for the runs.
;; If the bench mark is not simple then there needs to be a
;;  corresponding bench-handler-<something>

;;; Code:

;; Use elp to profile benchmarks
(require 'cl)				;Emacs doesn't have when and cdar

;-----------------------------------------------------------------------------
(defvar bench-mark-hanoi-times nil)

(defun bench-handler-hanoi (times)
  (let ((start-time))
  (while (> times 0)
;    (setq start-time (bench-get-time))
    (bench-mark-hanoi)
;    (setq bench-mark-hanoi-times (cons (- (bench-get-time) start-time ) bench-mark-hanoi-times ))
    (setq times (- times 1))))
)

(defun bench-mark-hanoi ()
  "How long to complete the tower of hanoi."
  (hanoi 4))

;-----------------------------------------------------------------------------
(defvar bench-mark-font-lock-buffer nil "buffer used for bench-mark-fontlock")

(defun bench-handler-font-lock (times)
  (setq bench-mark-font-lock-buffer (find-file bench-lisp-file))
  (while (> times 0)
    (bench-mark-font-lock)
    (font-lock-mode)			; Turn it off
    (setq times (- times 1)))
  (kill-buffer bench-mark-font-lock-buffer)
)

(defun bench-mark-font-lock ()
  "How long to fonitfy a large file."
  (font-lock-fontify-buffer)
)

;-----------------------------------------------------------------------------
(defvar bench-mark-scrolling-buffer nil "buffer used for bench-mark-scrolling")

(defun bench-handler-scrolling (times)
  (setq bench-mark-scrolling-buffer (find-file bench-lisp-file))
  (set-buffer bench-mark-scrolling-buffer)
;  (setq scroll-step 1)
  (font-lock-mode -1)
  (goto-char (point-min))		;Start at point min
  (let ((temp-times times))
    (while (> temp-times 0)
      (bench-mark-scrolling-down)
      (bench-mark-scrolling-up)
      (setq temp-times (- temp-times 1))))

  (font-lock-fontify-buffer)

  (goto-char (point-min))		;Start at point min
  (let ((temp-times times))
    (while (> temp-times 0)
      (bench-mark-scrolling-down-fontified)
      (bench-mark-scrolling-up-fontified)
      (setq temp-times (- temp-times 1))))
  (kill-buffer bench-mark-scrolling-buffer)
)

(defun bench-mark-scrolling-down ()
  "How long does it take to scroll down through a large file.
Expect point to be at point min"
  (let ((buffer-read-only t))
    (while (< (point) (point-max))
      (next-line 1)
      (sit-for 0))))

(defun bench-mark-scrolling-up ()
  "How long does it take to scroll up through a large fontified ile."
  (let ((buffer-read-only t))
    (while (> (point) (point-min))
      (previous-line 1)
      (sit-for 0))))

(defun bench-mark-scrolling-down-fontified ()
  "How long does it take to scroll down through a large fontified file."
  (let ((buffer-read-only t))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (next-line 1)
      (sit-for 0))))

(defun bench-mark-scrolling-up-fontified ()
  "How long does it take to scroll up through a large fontified ile."
  (let ((buffer-read-only t))
    (while (> (point) (point-min))
      (previous-line 1)
      (sit-for 0))))

;-----------------------------------------------------------------------------

(defun bench-handler-make-frames (times)
  (let ((temp-times times)
	(frame))
    (while (> temp-times 0)
      (setq frame (bench-mark-make-frame)) ;Make frame
      (bench-mark-delete-frame frame)	;Delete frame
      (setq temp-times (- temp-times 1))))

  (let ((temp-times times)
	(frames))
    (while (> temp-times 0)
      (setq frames (cons (bench-mark-make-multiple-frames) frames)) ;Make frames
      (setq temp-times (- temp-times 1)))

    (setq temp-times times)

    (while (> temp-times 0)
      (bench-mark-delete-multiple-frames (car frames))	;Delete frames
      (setq frames (cdr frames))
      (setq temp-times (- temp-times 1))))

)

(defun bench-mark-make-frame ()
  "How quickly can emacs create a new frame."
  (make-frame))

(defun bench-mark-delete-frame (frame)
  "How quickly can emacs create a new frame."
  (delete-frame frame))

(defun bench-mark-make-multiple-frames ()
  "How quickly can emacs create a new frame."
  (make-frame))

(defun bench-mark-delete-multiple-frames (frame)
  "How quickly can emacs create a new frame."
  (delete-frame frame))


;-----------------------------------------------------------------------------
(defconst bench-mark-make-words-buffer nil)
(defconst bench-mark-make-words-buffer-name "*bench-mark-make-words*")
(defconst bench-mark-make-words-number-of-words 10000)

(defun bench-handler-make-words (times)
  (setq bench-mark-make-words-buffer (get-buffer-create bench-mark-make-words-buffer-name))
  (set-buffer bench-mark-make-words-buffer)
  (while (> times 0)
    (bench-mark-make-words)
    (erase-buffer)
    (setq times (- times 1)))
  (kill-buffer bench-mark-make-words-buffer)
)

(defun bench-mark-make-words ()
  "How long does it take to generate lots of random words."
  (let ((tmp-words bench-mark-make-words-number-of-words))
    (while (not (= tmp-words 0))
      (let ((word-len (random 10)))
	(while (not (= word-len 0))
	  (insert (+ ?a (random 25)))
	  (setq word-len (- word-len 1))))
      (insert "\n")
      (setq tmp-words (- tmp-words 1)))))

;-----------------------------------------------------------------------------
(defconst bench-mark-sort-words-buffer-name "*bench-mark-sort-words*")
(defconst bench-mark-sort-words-buffer nil)
(defconst bench-mark-sort-words-number-words 10000)

(defun bench-handler-sort-words (times)
  (setq bench-mark-sort-words-buffer (get-buffer-create bench-mark-sort-words-buffer-name))
  (switch-to-buffer bench-mark-sort-words-buffer)
  (while (> times 0)
    (bench-pre-sort-words)			;Generate the random words
    (bench-mark-sort-words)			;Sort those puppies
    (erase-buffer)
    (setq times (- times 1)))
  (kill-buffer bench-mark-sort-words-buffer)
)

(defun bench-pre-sort-words ()
  "How long does it take to generate lots of random words."
  (let ((tmp-words bench-mark-sort-words-number-words))
    (while (not (= tmp-words 0))
      (let ((word-len (random 10)))
	(while (not (= word-len 0))
	  (insert (+ ?a (random 25)))
	  (setq word-len (- word-len 1))))
      (insert "\n")
      (setq tmp-words (- tmp-words 1)))))

(defun bench-mark-sort-words ()
  (sort-lines nil (point-min) (point-max))
)

;-----------------------------------------------------------------------------
; Byte compile a file
(defun bench-handler-byte-compile (times)
  (while (> times 0)
    (bench-mark-byte-compile)
    (setq times (- times 1)))
)

(defun bench-mark-byte-compile ()
  "How long does it take to byte-compile a large lisp file"
  (byte-compile-file bench-lisp-file)
)

;-----------------------------------------------------------------------------
; Run through a loop

(defconst bench-mark-loop-count 250000)

(defun bench-handler-loop (times)
  (while (> times 0)
    (bench-mark-loop)
    (setq times (- times 1)))
)

(defun bench-mark-loop ()
  "How long does it take to run through a loop."
  (let ((count bench-mark-loop-count))
    (let ((i 0) (gcount 0))
      (while (< i count)
	(increment)
	(setq i (1+ i)))
      (message "gcount = %d" gcount))))

(defun increment ()
  "Increment a variable for bench-mark-loop."
  (setq gcount (1+ gcount)))

;-----------------------------------------------------------------------------
(defconst bench-mark-large-list-list-size 500000
  "Size of list to use in small list creation/garbage collection")
(defconst bench-mark-large-list-num-lists 10)

(defun bench-handler-large-list (times)
  (let ((tmp-foo bench-mark-large-list-num-lists))
    (while (> tmp-foo 0)
      (bench-mark-large-list)
      (setq tmp-foo (- tmp-foo 1))))
)

(defun bench-mark-large-list ()
  (make-list bench-mark-large-list-list-size '1)
)

;-----------------------------------------------------------------------------
(defun bench-mark-large-list-garbage-collect (times)
  (garbage-collect)
)

;-----------------------------------------------------------------------------
(defconst bench-mark-small-list-list-size 10
  "Size of list to use in small list creation/garbage collection")

(defconst bench-mark-small-list-num-lists 100000
  "Number of lists to use in small list creation/garbage collections")

(defun bench-handler-small-list (times)
  (let ((tmp-foo bench-mark-small-list-num-lists))
    (while (> tmp-foo 0)
      (bench-mark-small-list)
      (setq tmp-foo (- tmp-foo 1)))
))

(defun bench-mark-small-list ()
  (make-list bench-mark-small-list-list-size '1)
)

;-----------------------------------------------------------------------------
(defun bench-mark-small-list-garbage-collect (times)
  (garbage-collect)
)

;-----------------------------------------------------------------------------
(defconst bench-mark-insert-into-empty-buffer-num-words 100000)

(defun bench-handler-insert-into-empty-buffer (times)
  (set-buffer (get-buffer-create "*tmp*"))
  (bench-mark-insert-into-empty-buffer)
  (erase-buffer)
  (kill-buffer "*tmp*")
)

(defun bench-mark-insert-into-empty-buffer ()
  (let ((a bench-mark-insert-into-empty-buffer-num-words))
    (while (> a 0)
      (insert "0123456789\n")
      (setq a (1- a))))
)

;=============================================================================
(defconst bench-version (let ((rcsvers "$Revision: 1.4 $"))
			  (substring rcsvers 11 (- (length rcsvers) 2)))
  "*Version number of bench.el")

(defconst bench-large-lisp-file (expand-file-name
                                 "bench-large.el" (temp-directory))
  "Large lisp file to use in benchmarks should be /temp-dir/bench-text.el")

(defconst bench-small-lisp-file (expand-file-name
                                 "bench-small.el" (temp-directory))
  "Large lisp file to use in benchmarks should be /temp-dir/bench-text.el")

(defconst bench-lisp-file bench-large-lisp-file)

(defconst bench-pre-bench-hook nil
  "Hook for individual bench mark initialization.")

(defconst bench-post-bench-hook nil
  "Hook for individual bench mark statistic collection.")

(defconst bench-mark-function-alist 
  '(
    (bench-handler-hanoi . "Tower of Hanoi")
    (bench-handler-font-lock               . "Font Lock")
    (bench-handler-scrolling               . "Large File scrolling")
    (bench-handler-make-frames             . "Frame Creation")
    (bench-handler-make-words              . "Generate Words")
    (bench-handler-sort-words              . "Sort Buffer")
    (bench-handler-byte-compile            . "Large File bytecompilation")
    (bench-handler-loop                    . "Loop Computation")
    (bench-handler-large-list              . "Make a Few Large Size List")
    (bench-mark-large-list-garbage-collect . "Garbage Collection Large Size List")
    (bench-handler-small-list              . "Make Several Small Size List")
    (bench-mark-small-list-garbage-collect  . "Garbage Collection Small Size List")
    (bench-handler-insert-into-empty-buffer . "Text Insertion")
))

(defconst bench-enabled-profiling nil
  "If non-nil and the underlying emacs supports it, do function profiling.")

(defconst bench-mark-profile-buffer "*Profile*"
  "Buffer used for collection of profiling data.")

(setq gc-cons-threshold 40000000)

(defconst bench-small-frame-alist '((height . 24) (width . 80)))
(defconst bench-medium-frame-alist '((height . 48) (width . 80)))
(defconst bench-large-frame-alist '((height . 72) (width . 80)))

(defsubst bench-get-time ()
  ;; Stolen from elp
  ;; get current time in seconds and microseconds. I throw away the
  ;; most significant 16 bits of seconds since I doubt we'll ever want
  ;; to profile lisp on the order of 18 hours. See notes at top of file.
  (let ((now (current-time)))
    (+ (float (nth 1 now)) (/ (float (nth 2 now)) 1000000.0))))

(defun bench-init ()
  "Initialize profiling for bench marking package."
  (if (fboundp 'start-profiling)
      (let ((buf (get-buffer-create bench-mark-profile-buffer)))
	(erase-buffer buf)
	(when (profiling-active-p)
	  (stop-profiling)
	  (clear-profiling-info)))
    (message "Profiling not available in this XEmacs.")
    (sit-for 2)))

(defun bench-test-init ()
  "Initialize profiling for bench marking package."
  (if (fboundp 'start-profiling)
      (let ((buf (get-buffer-create bench-mark-profile-buffer)))
	(erase-buffer buf)
	(when (profiling-active-p)
	  (stop-profiling)
	  (clear-profiling-info)))
    (message "Profiling not available in this XEmacs.")
    (sit-for 2))
  (setq bench-lisp-file bench-small-lisp-file)
  (setq bench-mark-make-words-number-of-words 100)
  (setq bench-mark-sort-words-number-of-words 100)
  (setq bench-mark-loop-count 10000)
  (setq bench-mark-large-list-list-size 500)
  (setq bench-mark-small-list-num-lists 100)
  (setq bench-mark-insert-into-empty-buffer-num-words 100)
  
)

(defun bench-profile-start (test-name)
  "Turn on profiling for test `test-name'."
  (when (and bench-enabled-profiling
	     (fboundp 'start-profiling))
    (when (profiling-active-p)
      (stop-profiling))
    (let ((buf (get-buffer-create bench-mark-profile-buffer)))
      (save-excursion
	(set-buffer buf)
	(insert "Test `" test-name "'\n")
	(start-profiling)))))

(defun bench-profile-stop (test-name)
  "Turn off profiling for test `test-name'."
  (when (and bench-enabled-profiling
	     (fboundp 'stop-profiling))
    (stop-profiling)
    (let ((buf (get-buffer-create bench-mark-profile-buffer)))
      (save-excursion
	(set-buffer buf)
	(insert (with-output-to-string
		 (pretty-print-profiling-info)) "\n")))
    (clear-profiling-info)))

(add-hook 'bench-pre-bench-hook 'bench-profile-start)
(add-hook 'bench-post-bench-hook 'bench-profile-stop)

(defun bench-post ()
"Post processing of elp results"
; I can't figure out a good way to sort the lines numerically.
; If someone comes up with a good way, let me know.
  (goto-char (point-min))
  (next-line 2)
  (sort-lines nil (point) (point-max))
  (mail-results (current-buffer))
)

(defun bench (arg)
  "Run a series of benchmarks."
  (interactive "p")
  (elp-instrument-package "bench-mark") ;Only instrument functions
                                        ;beginning with bench-mark
  (bench-init)
  (if (fboundp 'byte-optimize)		;Turn off byte-compile optimization in XEmacs
      (setq byte-optimize nil))
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode -1))			;Turn off menu-bar
  (let ((benches bench-mark-function-alist))
    (while benches
      (let ((test-name (cdar benches)))
	(run-hook-with-args 'bench-pre-bench-hook test-name)
	(message "Running %s - %s." (symbol-name (caar benches)) test-name)
	(funcall (caar benches) arg)
	(setq benches (cdr benches))
	(run-hook-with-args 'bench-post-bench-hook test-name))
      ))
  (elp-results)
  (bench-post)
)

(defun bench-test (arg)
  "Run all the tests but with smaller values so the tests run quicker.
This way I don't have to sit around to see if the tests complete"
  (interactive "p")
  (elp-instrument-package "bench-mark") ;Only instrument functions
                                        ;beginning with bench-mark
  (bench-test-init)
  (if (fboundp 'byte-optimize)		;Turn off byte-compile optimization in XEmacs
      (setq byte-optimize nil))
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode -1))			;Turn off menu-bar
  (let ((benches bench-mark-function-alist))
    (while benches
      (let ((test-name (cdar benches)))
	(run-hook-with-args 'bench-pre-bench-hook test-name)
	(message "Running %s - %s." (symbol-name (caar benches)) test-name)
	(funcall (caar benches) arg)
	(setq benches (cdr benches))
	(run-hook-with-args 'bench-post-bench-hook test-name))
      ))
  (elp-results)
  (bench-post)
)


(defconst bench-send-results-to "holder@rsn.hp.com")
(defconst bench-subject "Bench Mark Results")
(defconst bench-system-form (format "

Please fill in as much of the following as you can
and then hit C-cC-c to send.

CPU Manufacturer (Intel,HP,DEC,etc.): 
CPU Type (Pentium,Alpha): 
CPU Speed: 
RAM (in meg): 
Emacs Version: %s
Emacs (version): %s
Compile line:
Bench Version: %s
" emacs-version (emacs-version) bench-version))

(defun mail-results (buffer)
  (mail nil bench-send-results-to bench-subject)
  (sit-for 0)
  (goto-char (point-max))
  (insert bench-system-form)
  (insert-buffer buffer)
)
;;; bench.el ends here
