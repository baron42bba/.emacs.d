;;; comint-xemacs.el --- Face customizations for comint

;; Copyright (C) 1997 by Free Software Foundation, Inc.

;; Author: Steven L Baur <steve@xemacs.org>
;; Keywords: help, faces

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

;;; Synched up with: Not in FSF

;;; Commentary:

;; Declare customizable faces for comint outside the main code so it can
;; be dumped with XEmacs.

;;; Code:

(defgroup comint nil
  "General command interpreter in a window stuff."
  :group 'processes)

(defface comint-input-face '((((class color)
			      (background dark))
			     (:foreground "red"))
			    (((class color)
			      (background light))
			     (:foreground "blue"))
			    (t 
			     (:bold t)))
  "How to display user input for comint shells."
  :group 'comint)



(provide 'comint-xemacs)

;;; comint-xemacs.el ends here
