;; c-style.el --- sets c-style control variables.
;; Copyright (C) 1992-1993 Free Software Foundation, Inc.

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
;;
;; LCD Archive Entry:
;; c-style|Daniel LaLiberte|liberte@cs.uiuc.edu
;; |sets c-style control variables
;; |Thu Feb 27 13:42:57 CST 1992|Version: 2.1|~/as-is/c-src-doc.el.Z
;;
;;; Synched up with: Not in FSF.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; There are several ways to call set-c-style described below.
;;; None of these methods reindent your program - they only affect
;;; new indentation.
;;;
;;; - Just call set-c-style in your c-mode-hook.
;;;    Without style argument, default-c-style will be used.
;;;    With style argument, this will set the style for every 
;;;    c-mode buffer the same.
;;;
;;; - Call set-c-style from the Local Variables list.
;;;    e.g. "eval:(set-c-style 'C++)"
;;;
;;; - Call set-c-style interactively.  It prompts for the style name
;;;    with completion using default-c-style.
;;;
;;; For convenience, put one of the following in your .emacs:
;;;    (autoload 'set-c-style "c-style" nil t)
;;; or (load "c-style")
;;; =====================================================

(defvar default-c-style 'GNU
  "*The default value of c-style.  Set this in your .emacs.")

;; The following predefined styles are all I know about.
;; If you learn of another style that has a "big" following, please
;; send me the parameters.

(defvar c-style-alist 
  '((GNU 
     (c-indent-level 2)
     (c-continued-statement-offset 2)
     (c-brace-offset 0)
     (c-argdecl-indent 5)
     (c-label-offset -2))

    (BSD
     (c-indent-level 8)
     (c-continued-statement-offset 8)
     (c-brace-offset -8)
     (c-argdecl-indent 8)
     (c-label-offset -8))

    (K&R 
     (c-indent-level 5)
     (c-continued-statement-offset 5)
     (c-brace-offset -5)
     (c-argdecl-indent 0)
     (c-label-offset -5))

    (BS   ; was C++ 
     (c-indent-level 4)
     (c-continued-statement-offset 4)
     (c-brace-offset -4)
     (c-argdecl-indent 4)
     (c-label-offset -4))

    ;; From Lynn Slater
    (LRS
     (c-indent-level 4)
     (c-continued-statement-offset 4)
     (c-brace-offset 0)
     (c-argdecl-indent 4)
     (c-label-offset -2)
     (c-auto-newline nil))

    (Plauger
     (c-indent-level 0)
     (c-continued-statement-offset 8)
     (c-continued-brace-offset -8)
     (c-brace-offset 8)
     (c-brace-imaginary-offset 0)
     (c-argdecl-indent 0)
     (c-label-offset -8)
     (c-auto-newline t)
     (c-tab-always-indent t))

    ;; From Jozsef A Toth <jtoth+@pitt.edu>
    ;; Is this really the Whitesmith style?
    (Alman				
     (c-argdecl-indent 0)
     (c-brace-imaginary-offset 2)  ;;; ????
     (c-brace-offset 0)
     (c-continued-statement-offset 2)
     (c-indent-level 0)
     (c-label-offset -2)
     (c-auto-newline t)
     (comment-column 40)
     (tab-width 2)
     (fill-column '79))

    (Gould
     (c-indent-level 4)
     (c-continued-statement-offset 4)
     (c-brace-offset -4)
     (c-argdecl-indent 8)
     (c-label-offset -2)
     (c-brace-imaginary-offset 0))
     
    ;; From Joan Eslinger <wombat@kilimanjaro.key.amdahl.com>
    (WRS 
     (c-indent-level 0)
     (c-continued-statement-offset 4)
     (c-brace-offset 0)
     (c-argdecl-indent 4)
     (c-label-offset -2)
     (c-brace-imaginary-offset 4)
     (c-continued-brace-offset -4))
    ))
  
(defvar c-style nil
  "The buffer local c-mode indentation style.")

;; Add style name to mode line.  Assumes minor-mode-alist is not buffer local.
;; Thanks to Joan Eslinger.

(defvar c-style-name nil
  "The style name for a c-mode indentation style.
This is to be set by set-c-style, and used by the mode line.")

(or (assq 'c-style-name minor-mode-alist)
    (setq minor-mode-alist
	  (purecopy
	   (append minor-mode-alist
		   ;; use undocumented feature
		   '((c-style-name c-style-name))))))

(defun set-c-style (&optional style)
  "Set up the c-mode style variables from STYLE if it is given, or
default-c-style otherwise.  It makes the c indentation style variables
buffer local."

  (interactive)

  (let ((c-styles (mapcar 'car c-style-alist))) ; for completion
    (if (interactive-p)
	(setq style
	      (let ((style-string	; Get style name with completion.
		     (completing-read
		      (format "Set c-mode indentation style to (default %s): "
			      default-c-style)
		      (vconcat c-styles)
		      (function (lambda (arg) (memq arg c-styles)))
		      )))
		(if (string-equal "" style-string)
		    default-c-style
		  (intern style-string))
		)))
    
    ;; If style is nil, use default-c-style.
    (setq style (or style default-c-style))
    
    (make-local-variable 'c-style)
    (if (memq style c-styles)
	(setq c-style style)
      (error "Undefined c style: %s" style)
      )
    (message "c-style: %s" c-style)
    
    ;; Set the c-style-name
    (make-local-variable 'c-style-name)
    (setq c-style-name (format " %s" c-style))

    ;; Finally, set the indentation style variables making each one local.
    (mapcar (function (lambda (c-style-pair)
			(make-local-variable (car c-style-pair))
			(set (car c-style-pair)
			     (car (cdr c-style-pair)))))
	    (cdr (assq c-style c-style-alist)))
    c-style
    ))
