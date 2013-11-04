;;; rmail-mini.el --- minimal core of "RMAIL" mail reader for Emacs.

;; Copyright (C) 1985,86,87,88,93,94,95,96,97 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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

;;; Synched up with: Emacs 20.1.

;;; Commentary:

;; Stripped of all cruft to support Gnus

;;; Code:

;; Souped up by shane@mit-ajax based on ideas of rlk@athena.mit.edu
;;   New features include attribute and keyword support, message
;;   selection by dispatch table, summary by attributes and keywords,
;;   expunging by dispatch table, sticky options for file commands.

;; Extended by Bob Weiner of Motorola
;;   New features include: rmail and rmail-summary buffers remain
;;   synchronized and key bindings basically operate the same way in both
;;   buffers, summary by topic or by regular expression, rmail-reply-prefix
;;   variable, and a bury rmail buffer (wipe) command.

(defgroup rmail nil
  "Mail reader for Emacs."
  :group 'mail)

(defgroup rmail-reply nil
  "Rmail reply options."
  :prefix "rmail-"
  :group 'rmail)

(setq rmail-mode-map (make-sparse-keymap))

;;;###autoload
(defcustom rmail-dont-reply-to-names nil "\
*A regexp specifying names to prune of reply to messages.
A value of nil means exclude your own name only."
  :type '(choice regexp (const :tag "Your Name" nil))
  :group 'rmail-reply)

;;;###autoload
(defvar rmail-default-dont-reply-to-names "info-" "\
A regular expression specifying part of the value of the default value of
the variable `rmail-dont-reply-to-names', for when the user does not set
`rmail-dont-reply-to-names' explicitly.  (The other part of the default
value is the user's name.)
It is useful to set this variable in the site customization file.")


(provide 'rmail)
(provide 'rmail-mini)

;;; rmail-mini.el ends here
