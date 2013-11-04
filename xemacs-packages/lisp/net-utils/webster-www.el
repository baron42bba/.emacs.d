;;; webster-www.el --- Look up a word in WWW Merriam-Webster dictionaries

;; Copyright (c) 1997, 1998 Tomasz Cholewo <t.cholewo@ieee.org>

;; Authors: Tomasz Cholewo <t.cholewo@ieee.org>
;;          Soren Dayton <csdayton@cs.uchicago.edu>
;; Modified: 1998/04/05
;; Version: 1.2
;; Keywords: comm, hypermedia

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

;;; Synched up with: not in FSF.

;;; Code:
(eval-when-compile
  (autoload 'w3-form-encode-xwfu "w3-forms"))

(defcustom webster-www-url-format "http://www.m-w.com/cgi-bin/mweb?book=%s&va=%s"
  "URL format to reference for Webster's dictionaries.
It is used in a call to `format' with a book name
(\"Dictionary\" or \"Thesaurus\") as its first argument
and a word to look up as its second argument."
  :type 'string)

(defun webster-www-dictionary (arg)
  "Look up a word in Webster Dictionary at http://www.m-w.com."
  (interactive (list (webster-www-prompt "Dictionary")))
  (webster-www-fetch "Dictionary" arg))

(fset 'webster-www (symbol-function 'webster-www-dictionary))

(defun webster-www-thesaurus (arg)
  "Look up a word in Webster Thesaurus at http://www.m-w.com."
  (interactive (list (webster-www-prompt "Thesaurus")))
  (webster-www-fetch "Thesaurus" arg))

(defun webster-www-prompt (book)
  (let* ((prompt (concat
	          (concat "Look up word in Webster " book " (")
 			  (current-word) "): "))
         (arg (read-string prompt)))
    (if (equal "" arg) (current-word) arg)))

(defun webster-www-fetch (book word)
  (require 'url)
  (require 'w3-forms)
  (require 'browse-url)
  (browse-url
   (format webster-www-url-format
 	   book
 	   (w3-form-encode-xwfu word))))

(provide 'webster-www)

;;; webster-www.el ends here
