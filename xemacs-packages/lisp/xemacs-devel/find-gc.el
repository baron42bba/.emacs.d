;;; find-gc.el --- detect functions that call the garbage collector

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: maint

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

;;; Synched up with: FSF 19.30.

;;; #### before this is really usable, it should be rewritten to call
;;; Makefile to compile the files.

;;; Commentary:

;;; Produce in unsafe-list the set of all functions that may invoke GC.
;;; This expects the Emacs sources to live in emacs-source-directory.
;;; It creates a temporary working directory /tmp/esrc.

;;; Code:

(defvar unsafe-list nil)
(defvar subrs-used nil)
(defvar subrs-called nil)

;; Set this to point to your XEmacs source directory.
(defvar emacs-source-directory "/usr/src/xemacs/xemacs-20/src")

;;; Functions on this list are safe, even if they appear to be able
;;; to call the target.

(defvar noreturn-list '(signal_error error Fthrow wrong_type_argument))

;;; Try to load generated source-files
(load-library (concat emacs-source-directory "/../lisp/source-files.el"))

(defvar source-files nil
  "Set this to the source files you want to check.")

;;;

(defun find-gc-unsafe ()
  (setq subrs-used nil)
  (trace-call-tree t nil)
  (trace-use-tree)
  (set-buffer (get-buffer-create "*gc-tmp*"))
  (erase-buffer)
  (find-unsafe-funcs 'Fgarbage_collect)
  (setq unsafe-list (sort unsafe-list 'find-gc-sort-p))
  (insert (format "%s\n" unsafe-list))
  (setq unsafe-list nil)
  (find-unsafe-funcs 'garbage_collect_1)
  (setq unsafe-list (sort unsafe-list 'find-gc-sort-p))
  (insert (format "%s\n" unsafe-list))
  (goto-char (point-min))
  (while (search-forward ") (" nil t)
    (replace-match ")
 (" nil t))
  )

(defun find-gc-sort-p (x y)
  (string-lessp (car x) (car y)))

;;; This does a depth-first search to find all functions that can
;;; ultimately call the function "target".  The result is an a-list
;;; in unsafe-list; the cars are the unsafe functions, and the cdrs
;;; are (one of) the unsafe functions that these functions directly
;;; call.

(defun find-unsafe-funcs (target)
  (setq unsafe-list (list (list target)))
  (trace-unsafe target))

(defun trace-unsafe (func)
  (let ((used (assq func subrs-used)))
    (or used
	(error "No subrs-used for %s" (car unsafe-list)))
    (while (setq used (cdr used))
      (or (assq (car used) unsafe-list)
	  (memq (car used) noreturn-list)
	  (progn
	    (setq unsafe-list (cons (cons (car used) func) unsafe-list))
	    (trace-unsafe (car used)))))))


;;; This produces an a-list of functions in subrs-called.  The cdr of
;;; each entry is a list of functions which the function in car calls.

(defun trace-call-tree (&optional make-all delete-after)
  (save-excursion
    (setq subrs-called nil)
    (let ((case-fold-search nil)
	  name entry file)
      ;; Stage one, make rtl files with make
      (if make-all
	  (call-process 
	   "sh" nil nil nil "-c" 
	   (format "cd %s; make dortl" emacs-source-directory file))
	(dolist (file source-files)
	  (princ (format "Compiling %s...\n" file))
	  (call-process 
	   "sh" nil nil nil "-c" 
	   (format "cd %s; make %s.rtl" emacs-source-directory file))))
      (set-buffer (get-buffer-create "*Trace Call Tree*"))
      ;; Stage two, process them
      (dolist (file source-files)
	(erase-buffer)
	(insert-file-contents (concat emacs-source-directory "/" file ".rtl"))
	(while (re-search-forward ";; Function \\|(call_insn " nil t)
          (if (= (char-after (- (point) 3)) ?o)
              (progn
                (looking-at "[a-zA-Z0-9_]+")
                (setq name (intern (buffer-substring (match-beginning 0)
                                                     (match-end 0))))
                (princ (format "%s : %s\n" file name))
                (setq entry (list name)
                      subrs-called (cons entry subrs-called)))
            (if (looking-at ".*\n?.*\"\\([A-Za-z0-9_]+\\)\"")
                (progn
                  (setq name (intern (buffer-substring (match-beginning 1)
                                                       (match-end 1))))
                  (or (memq name (cdr entry))
                      (setcdr entry (cons name (cdr entry)))))))))
      (when delete-after
	(dolist (file source-files)
	  (delete-file (concat emacs-source-directory "/" file ".rtl"))))
	    )))


;;; This produces an inverted a-list in subrs-used.  The cdr of each
;;; entry is a list of functions that call the function in car.

(defun trace-use-tree ()
  (setq subrs-used (mapcar 'list (mapcar 'car subrs-called)))
  (let ((ptr subrs-called)
	p2 found)
    (while ptr
      (setq p2 (car ptr))
      (while (setq p2 (cdr p2))
	(if (setq found (assq (car p2) subrs-used))
	    (setcdr found (cons (car (car ptr)) (cdr found)))))
      (setq ptr (cdr ptr)))))

;;; find-gc.el ends here
