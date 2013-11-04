;;; tree-menu.el
;;; v1.20; 10-May-1994
;;; Copyright (C) 1994 Heiko Muenkel
;;; email: muenkel@tnt.uni-hannover.de
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 1, or (at your option)
;;;  any later version.
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Synched up with: Not in FSF.
;;; 
;;; Description:
;;;
;;;	Provides the functions `tree-make-file-list' and `tree-make-menu'.
;;;	With these functions it is possible to generate file browsing menus,
;;; 	where each menu-item calls the same function, but on different files.
;;;	Example: 
;;; 		(popup-menu (cons "Open File"
;;;		  	          (tree-make-menu (tree-make-file-list "~/")
;;;				  		  'find-file
;;;				  		  t
;;;				  		  t
;;;				  		  '("\\..*"))))
;;;
;;;	Note: This function is very time consuming ! Therefore you should
;;;	      call `tree-make-file-list' once and make several menus
;;;	      from the same list. And you should only rebuild the menu if 
;;;	      it is necessary, if you've a big directory tree.
;;;
;;; Installation: 
;;;   
;;;	Put this file in one of your lisp load directories.
;;;
;;;  Changed 18-May-1995, Kyle Jones
;;;    Removed the need for the utils.el package and references thereto.
;;;    Changed file-truename calls to tree-menu-file-truename so
;;;    the calls could be made compatible with FSF Emacs 19's
;;;    file-truename function.

(defvar tree-ls-flags "-AFLR" 
  "*A String with the flags used in the function tree-ls-in-temp-buffer 
for the ls command. Be careful if you want to change this variable. 
The ls command must append a / on all files which are directories. 
The original flags are -AFLR.")


(defun tree-ls-in-temp-buffer (dir temp-buffer)
"List the directory DIR in the TEMP-BUFFER."
  (switch-to-buffer temp-buffer)
  (erase-buffer)
  (call-process "ls" nil temp-buffer nil tree-ls-flags dir)
  (goto-char (point-min))
  (while (search-forward "//" nil t)
    (replace-match "/"))
  (goto-char (point-min)))


(defvar tree-temp-buffername "*tree*"
  "Name of the temp buffers in tree.")


(defun tree-make-file-list-1 (root list)
  (let ((filename (buffer-substring (point) (progn
					      (end-of-line)
					      (point)))))
    (while (not (string= filename ""))
      (setq 
       list 
       (append
	list
	(list
	 (cond ((char-equal (char-after (- (point) 1)) ?/)
		;; Directory
		(setq filename (substring filename 0 (1- (length filename))))
		(save-excursion
		  (search-forward (concat root filename ":"))
		  (forward-line)
		  (tree-make-file-list-1 (concat root filename "/")
						(list (tree-menu-file-truename 
						       filename
						       root)))))
	       ((char-equal (char-after (- (point) 1)) ?*)
		;; Executable
		(setq filename (substring filename 0 (1- (length filename))))
		(tree-menu-file-truename filename root))
	       (t (tree-menu-file-truename filename root))))))
      (forward-line)
      (setq filename (buffer-substring (point) (progn
						 (end-of-line)
						 (point)))))
    list))


(defun tree-menu-file-truename (file &optional root)
  (file-truename (expand-file-name file root)))

(defun tree-make-file-list (dir)
  "Makes a list with the files and subdirectories of DIR.
The list looks like: ((dirname1 file1 file2) 
                      file3
                      (dirname2 (dirname3 file4 file5) file6))"
  (save-window-excursion
    (setq dir (expand-file-name dir))
    (if (not (string= (substring dir -1) "/"))
	(setq dir (concat dir "/")))
;;    (while (string-match "/$" dir)
;;      (setq dir (substring dir 0 -1)))
    (tree-ls-in-temp-buffer dir
				 (generate-new-buffer-name 
				  tree-temp-buffername))
    (let ((list nil))
      (setq list (tree-make-file-list-1 dir nil))
      (kill-buffer (current-buffer))
      list)))


(defun tree-hide-file-p (filename re-hidden-file-list)
  "t, if one of the regexps in RE-HIDDEN-FILE-LIST matches the FILENAME."
  (cond ((not re-hidden-file-list) nil)
	((string-match (car re-hidden-file-list) 
		       (tree-menu-file-truename filename)))
	(t (tree-hide-file-p filename (cdr re-hidden-file-list)))))


(defun tree-make-menu (dirlist 
		       function 
		       selectable 
		       &optional 
		       no-hidden-dirs
		       re-hidden-file-list
		       include-current-dir)
  "Returns a menu list.
Each item of the menu list has the form 
 [\"subdir\" (FUNCTION \"dir\") SELECTABLE].
Hidden directories (with a leading point) are suppressed, 
if NO-HIDDEN-DIRS are non nil. Also all files which are
matching a regexp in RE-HIDDEN-FILE-LIST are suppressed.
If INCLUDE-CURRENT-DIR non nil, then an additional command
for the current directory (.) is inserted."
  (let ((subdir nil)
	(menulist nil))
    (while (setq subdir (car dirlist))
      (setq dirlist (cdr dirlist))
      (cond ((and (stringp subdir)
		  (not (tree-hide-file-p subdir re-hidden-file-list)))
	     (setq menulist
		   (append menulist
			   (list
			    (vector (file-name-nondirectory subdir)
				    (list function subdir)
				    selectable)))))
	    ((and (listp subdir)
		  (or (not no-hidden-dirs)
		      (not (char-equal 
			    ?.
			    (string-to-char 
			     (file-name-nondirectory (car subdir))))))
		  (setq menulist
			(append 
			 menulist
			 (list
			  (cons (file-name-nondirectory (car subdir))
				(if include-current-dir
				    (cons
				     (vector "."
					     (list function
						   (car subdir))
					     selectable)
				     (tree-make-menu (cdr subdir)
						     function
						     selectable
						     no-hidden-dirs
						     re-hidden-file-list
						     include-current-dir
						     ))
				  (tree-make-menu (cdr subdir)
						  function
						  selectable
						  no-hidden-dirs
						  re-hidden-file-list
						  ))))))))
	    (t nil))
      )
    menulist
    )
  )


(provide 'tree-menu)
