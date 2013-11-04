;;; permanent-buffers.el --- Permanent buffers management package.

;; Copyright (C) 1997-99 Didier Verna.

;; Author:        Didier Verna <didier@xemacs.org>
;; Maintainer:    Didier Verna <didier@xemacs.org>
;; Created:       Wed Dec 31 10:16:30 1997 under XEmacs 20.5 (beta 15)
;; Last Revision: Mon Mar 15 11:34:01 1999
;; Keywords:      extensions

;; This file is part of XEmacs

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Synched up with: Not in FSF Emacs.


;;; Commentary:

;; Contents management by FCM version 0.1.

;; A permanent buffer is a buffer that you don't want to kill, mainly used for
;; testing or temporary stuff. The *scratch* buffer is the most famous example
;; of what could be a permanent buffer. This package allows you to define
;; several permanent buffers (the scratch buffer can be one of them) that
;; will never disapear. If you kill them or save their contents, they will be
;; regenerated. You can also specify a set of lisp forms to eval in the buffer
;; when it is (re)generated.

;; The package is implemented in a minor-mode fashion. You can customize the
;; default value of `permanent-buffers-mode' or use
;; `turn-on-permanent-buffers' at startup. Within an XEmacs session, use
;; `permanent-buffers-mode' or `tun-o[n|ff]-oermanent-buffers'. You might also
;; want to customize `permanent-buffers-alist'.


;;; Change Log:

;; Mon Mar 15 1999: - Misc code cleanup,
;;                  - added 'editing parent group,
;;                  - provide 'permanent-buffers,
;;                  - customise'd and autoload'ed the initial mode value.
;; Mon Jan  5 1998: Added menu support.
;; Fri Jan  1 1998: Packaging, doc, cleanup.
;; Wed Dec 31 1997: Initial Version.


;;; Code:

;; Public variables =========================================================

(defgroup permanent-buffers nil
  "Permanent Buffers Management."
  :group 'editing)

(defcustom permanent-buffers-alist
  '(("*scratch*" (lisp-interaction-mode))
    ("*text*" (text-mode)))
  "*An alist of permanent buffers. See `permanent-buffers-mode' for more
details on what a permanent buffer is.
Each element looks like (NAME FORM ...) where NAME is the name of the
buffer, and FORMs are lisp expressions to be executed when the buffer
is regenerated (this buffer will be current when evaluating the
expressions)."
  :group 'permanent-buffers
  :type '(repeat (cons :tag "Permanent buffer"
		       (string :tag "Buffer name")
		       (repeat :tag "Forms to execute" (sexp :tag "")))))

;;;###autoload
(defcustom permanent-buffers-mode nil
  "Indicates whether the `permanent-buffers' behavior is active. You can
customize this variable to change the default value. To change the value
during an XEmacs session, please use the function instead."
  :type 'boolean
  :set (lambda (sym val)
	 (permanent-buffers-mode (if val 1 -1)))
  :initialize 'custom-initialize-default
  :require 'permanent-buffers
  :group 'permanent-buffers)


;; Private variables ========================================================

(defconst permanent-buffers-version "1.2")

(make-variable-buffer-local
 (defvar permanent-buffers-setup-done nil
   ;; This variable indicates whether the setup has been done in this buffer.
   ;; It is always buffer local, and is used mainly for initialisation. For
   ;; instance, if the *scratch* buffer is permanent, it exists at startup but
   ;; has not been set up. Checking this variable allows us to set it up
   ;; anyway.
   ))

(defvar permanent-buffers-menu
  '("Permanent Buffers"
    :filter permanent-buffers-menu-filter
    [ "Enable" (permanent-buffers-mode nil)
      :style toggle :selected permanent-buffers-mode ])
  ;; A 'Buffers' submenu with a a toggle button to [en/dis]able the
  ;; permanent buffers behavior, and a list of permanent buffers to switch
  ;; to (thanks to the filter).
  )


;; Private functions ========================================================

(defun permanent-buffers-menu-filter (menu-items)
  ;; This filter builds a list of available permanent buffers and appends
  ;; push buttons to `switch-to-buffer' them, in the submenu.
  (let ((bufs permanent-buffers-alist)
	(menu menu-items)
	buf)
    (while (setq buf (pop bufs))
      (setq menu (append menu
			 `([,(car buf)
			    (switch-to-buffer ,(car buf))
			    permanent-buffers-mode ]))))
    menu))

(defun permanent-buffers-setup-all ()
  ;; Recheck the whole list, and possibly regenerate missing ones.
  (let ((bufs permanent-buffers-alist)
	buf permbuf)
    ;; Check whether a permanent buffer doesn't exist, or is attached to a
    ;; file.
    (while (setq buf (pop bufs))
      (setq permbuf (get-buffer (car buf)))
      (cond ((not permbuf) ;; Killed buffer.
	     (with-current-buffer (get-buffer-create (car buf))
	       (eval (cons 'progn (cdr buf)))
	       (setq permanent-buffers-setup-done t)))
	    ((buffer-file-name permbuf) ;; Buffer attached to a file.
	     (with-current-buffer permbuf
	       (rename-buffer (file-name-nondirectory buffer-file-name) t))
	     (with-current-buffer (get-buffer-create (car buf))
	       (eval (cons 'progn (cdr buf)))
	       (setq permanent-buffers-setup-done t)))
	    ((not permanent-buffers-setup-done) ;; OK, but setup not done.
	     (with-current-buffer (get-buffer-create (car buf))
	       (eval (cons 'progn (cdr buf)))
	       (setq permanent-buffers-setup-done t)))))
    ))

(defun permanent-buffers-kill-buffer-function ()
  ;; Are we in a permanent buffer ?
  (let ((buf (assoc (buffer-name) permanent-buffers-alist)))
    (when buf
      ;; Rename this buffer just before killing it, in order to free its name.
      ;; Otherwise, we would generate a buffer<n> name.
      (rename-buffer (car buf) t)
      (with-current-buffer (get-buffer-create (car buf))
	(eval (cons 'progn (cdr buf)))
	(setq permanent-buffers-setup-done t))
      )))

;; We don't have any way to remember the old buffer name, since there isn't
;; any `before-set-visited-file-name-hook'. So check the whole list. It's a
;; bit too much, but ...
(defun permanent-buffers-asvfn-function ()
  (permanent-buffers-setup-all))


;; Public functions =========================================================

;;;###autoload
(defun permanent-buffers-mode (arg)
  "Toggle on/off the permanent buffers behavior. With a prefix > 0 turn it on.
A permanent buffer is a special buffer, not attached to any file, that can't
be killed or saved-as. If you kill it or save it as a file, it will be
regenerated fresh and empty. See also the variable `permanent-buffers-alist'."
  (interactive "P")
  (let ((turn-it-on (if (null arg) (not permanent-buffers-mode)
		      (> (prefix-numeric-value arg) 0))))
    (cond ((and turn-it-on (not permanent-buffers-mode))
	   ;; Menubar stuff
	   (when (featurep 'menubar)
	     (add-submenu  '("Buffers")
			   permanent-buffers-menu
			   ;; This way of dealing with the position in
			   ;; menus is ugly ... :-(
			   '"--"))
	   ;; Recheck the whole list first.
	   (permanent-buffers-setup-all)
	   (add-hook 'after-set-visited-file-name-hooks
		     'permanent-buffers-asvfn-function)
	   (add-hook 'kill-buffer-hook
		     'permanent-buffers-kill-buffer-function)
	   (setq permanent-buffers-mode t))
	  ((and permanent-buffers-mode (not turn-it-on))
	   (setq permanent-buffers-mode nil)
	   (remove-hook 'after-set-visited-file-name-hooks
			'permanent-buffers-asvfn-function)
	   (remove-hook 'kill-buffer-hook
			'permanent-buffers-kill-buffer-function))
	  )))

;;;###autoload
(defun turn-on-permanent-buffers ()
  "Unconditionally turn on the `permanent-buffers' behavior."
  (interactive)
  (permanent-buffers-mode 1))

(defun turn-off-permanent-buffers ()
  "Unconditionally turn off the `permanent-buffers' behavior."
  (interactive)
  (permanent-buffers-mode -1))

(defun permanent-buffers-version ()
  (interactive)
  (message "permanent-buffers version %s" permanent-buffers-version))

(provide 'permanent-buffers)

;;; permanent-buffers.el ends here
