;;; awk-mode.el --- AWK code editing commands for Emacs

;; Copyright (C) 1988,94,96,2000  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: unix, languages

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

;;; Synched up with: GNU Emacs 21.2 except not derived from c-mode and we
;;;                  have a bit different regexp-opt syntax.

;;; Commentary:

;; Sets up C-mode with support for awk-style #-comments and a lightly
;; hacked syntax table.

;;; Code:

(defvar awk-mode-syntax-table nil
  "Syntax table in use in Awk-mode buffers.")

(if awk-mode-syntax-table
    ()
  (setq awk-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" awk-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " awk-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " awk-mode-syntax-table)
  (modify-syntax-entry ?\# "<   " awk-mode-syntax-table)
  (modify-syntax-entry ?/ "." awk-mode-syntax-table)
  (modify-syntax-entry ?* "." awk-mode-syntax-table)
  (modify-syntax-entry ?+ "." awk-mode-syntax-table)
  (modify-syntax-entry ?- "." awk-mode-syntax-table)
  (modify-syntax-entry ?= "." awk-mode-syntax-table)
  (modify-syntax-entry ?% "." awk-mode-syntax-table)
  (modify-syntax-entry ?< "." awk-mode-syntax-table)
  (modify-syntax-entry ?> "." awk-mode-syntax-table)
  (modify-syntax-entry ?& "." awk-mode-syntax-table)
  (modify-syntax-entry ?| "." awk-mode-syntax-table)
  (modify-syntax-entry ?_ "_" awk-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" awk-mode-syntax-table))

(defvar awk-mode-abbrev-table nil
  "Abbrev table in use in Awk-mode buffers.")
(define-abbrev-table 'awk-mode-abbrev-table ())

;; Regexps written with help from Peter Galbraith <galbraith@mixing.qc.dfo.ca>.
(defconst awk-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Regular expressions. npak@ispras.ru
     ;; We shoul do it before any other fontification
     ;; Split into two cases for Speed god.
     (cons "^[ \t]*/\\(\\(\\([\\].\\)?[^/\\]*\\)+\\)"
           '(1 'font-lock-string-face))
     (cons "[-!~+*=&<>|(][ \t]*/\\(\\(\\([\\].\\)?[^/\\]*\\)+\\)"
           '(1 font-lock-string-face))
     ;;
     ;; Function names.
     '("^[ \t]*\\(function\\)\\>[ \t]*\\(\\sw+\\)?"
       (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
     ;;
     ;; Variable names.
     ;; "RT", field identifiers $0, $1 etc added by npak@ispras.ru
     (cons (concat
            "\\<\\(?:"
            (regexp-opt
             '("ARGC" "ARGIND" "ARGV" "CONVFMT" "ENVIRON" "ERRNO"
               "FIELDWIDTHS" "FILENAME" "FNR" "FS" "IGNORECASE" "NF" "NR"
               "OFMT" "OFS" "ORS" "RLENGTH" "RS" "RT" "RSTART" "SUBSEP"))
            "\\|\\$\\(?:[0-9]+\\|\\sw+\\)\\)\\>")
           'font-lock-variable-name-face)
     ;;
     ;; Keywords.
     ;; "do", "in", "nextfile" added by npak@ispras.ru
     (concat
      "\\<\\(?:"
      (regexp-opt
       '("BEGIN" "END" "break" "continue" "delete" "do" "else" "exit" "for"
         "getline" "if" "in" "next" "nextfile" "print" "printf" "return"
         "while"))
      "\\)\\>")
     ;;
     ;; Builtins.
     ;; "fflush" added by npak@ispras.ru
     (list (concat
            "\\<\\("
            (regexp-opt
             '("atan2" "close" "cos" "ctime" "exp" "gsub" "index" "int"
               "length" "log" "match" "rand" "sin" "split" "sprintf"
               "sqrt" "srand" "sub" "substr" "system" "time" "fflush"
               "tolower" "toupper"))
            "\\)(")
           1 'font-lock-reference-face)
     ;;
     ;; Function call, variables. npak@ispras.ru
     (list "\\<\\([A-Za-z_]\\sw*\\)("
           1 'font-lock-function-name-face)
     (list "\\<\\([A-Za-z_]\\sw*\\)\\($\\|[^(]\\)"
           1 'font-lock-variable-name-face)
     ;;
     ;; Operators.  Is this too much?
     ;; Yes, it is. npak@ispras.ru
     ;(cons (regexp-opt '("&&" "||" "<=" "<" ">=" ">" "==" "!=" "!~" "~"))
     ;      'font-lock-reference-face)
     ))
 "Default expressions to highlight in AWK mode.")

;; npak@ispras.ru
(put 'awk-mode 'font-lock-defaults
     '(awk-font-lock-keywords nil nil ((?_ . "w"))))

;;;###autoload
(defun awk-mode ()
  "Major mode for editing AWK code.
This is much like C mode except for the syntax of comments.  It uses
the same keymap as C mode and has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on AWK mode calls the value of the variable `awk-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (require 'cc-mode)
  (c-initialize-cc-mode)
  (use-local-map c-mode-map)
  (c-common-init)
  (setq major-mode 'awk-mode)
  (setq mode-name "AWK")
  (setq local-abbrev-table awk-mode-abbrev-table)
  (set-syntax-table awk-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; No need in XEmacs
  ;(make-local-variable 'font-lock-defaults)
  ;(setq font-lock-defaults '(awk-font-lock-keywords nil nil ((?_ . "w"))))
  (run-hooks 'awk-mode-hook))

;; XEmacs additions
;;;###autoload(add-to-list 'auto-mode-alist '("\\.awk\\'" . awk-mode))
;;;###autoload(add-to-list 'interpreter-mode-alist '("awk\\b" . awk-mode))

(provide 'awk-mode)

;;; awk-mode.el ends here
