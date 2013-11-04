;;; uil-mode.el --- UIL file editing mode for Emacs

;;; Written by Brett Johnson <brett@fc.hp.com>
;;; Maintained for XEmacs by Jake Colman <jake.colman@xemacs.org>

;;; Commentary:
;; Sets up cc-mode with support for UIL file #-comments , a lightly
;; hacked syntax table, and some minimal font-lock regexps.

;;; Code:

(require 'cc-mode)

(defvar uil-mode-syntax-table nil
  "Syntax table in use in uil-mode buffers.")

(if uil-mode-syntax-table
    ()
  (setq uil-mode-syntax-table (make-syntax-table))
  (c-populate-syntax-table uil-mode-syntax-table)
  ;; add extra comment syntax
  (modify-syntax-entry ?/  ". 14" uil-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23" uil-mode-syntax-table)
  (modify-syntax-entry ?!  "< b" uil-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" uil-mode-syntax-table)
  (modify-syntax-entry ?\^m "> b" uil-mode-syntax-table)
  )

(defvar uil-mode-abbrev-table nil
  "Abbrev table in use in uil-mode buffers.")
(define-abbrev-table 'uil-mode-abbrev-table ())

(defconst uil-font-lock-keywords
  (list
   ;; Make punctuation bold.
   (cons "[-+*/{}():;=]" 'bold)
   ;; Fontify procedure/identifier names.
   '("\\(procedure\\)[ \t]+\\(\\sw+\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
   '("\\(identifier\\)[ \t]+\\(\\sw+\\)"
     (1 font-lock-keyword-face) (2 font-lock-variable-name-face))
   ;; Fontify UIL keywords.
   (cons (concat "\\<\\(module\\|end\\|widget\\|gadget\\|"
                 "arguments\\|callbacks\\|controls\\|identifiers\\|"
                 "include\\|list\\|object\\|procedure[s]?\\|value\\|"
                 "exported\\|private\\|on\\|off\\|true\\|false\\)\\>")
         'font-lock-keyword-face)
   ;; Pseudo-keyworks (not reserved).
   (cons (concat "\\<\\(background\\|case_\\(in\\)?sensitive\\|file\\|"
                 "foreground\\|imported\\|\\(un\\)?managed\\|names\\|"
                 "objects\\|right_to_left\\|user_defined\\)\\>")
         'font-lock-type-face)
   ;; Built in types..
   (cons (concat
          "\\<\\(a\\(ny\\|rgument\\|sciz_\\(string_\\)?table\\)\\|"
          "boolean\\|c\\(haracter_set\\|olor\\(_table\\)?\\)\\|"
          "compound_string\\(_table\\)?\\|font\\(_table\\|set\\)?\\|"
          "i\\(con\\|nteger\\(_table\\)?\\)\\|keysym\\|reason\\|rgb\\|"
          "single_float\\|string\\(_table\\)?\\|translation_table\\|"
          "wide_character\\|xbitmapfile\\|version\\)\\>") 'font-lock-type-face)
   ;; Make a hack at motif constants & fields..
   (cons "\\<\\(Xm[a-zA-Z_]+\\|iso_[a-z0-1A-Z]+\\)\\>" 'font-lock-variable-name-face)
   ))

(put 'uil-mode 'font-lock-keywords 'uil-font-lock-keywords)

;;;###autoload
(defun uil-mode ()
  "Major mode for editing UIL files.
This is much like C mode except for the syntax of comments.  It uses
the same keymap as C mode and has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on uil mode calls the value of the variable `uil-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (require 'cc-mode)
  (c-initialize-cc-mode)
  (use-local-map c-mode-map)
  (c-common-init)
  (setq major-mode 'uil-mode)
  (setq mode-name "uil")
  (setq local-abbrev-table uil-mode-abbrev-table)
  (set-syntax-table uil-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
;  (setq comment-start "!\\|/\*")
;  (make-local-variable 'comment-end)
;  (setq comment-end "\n\\|\*/")
;  (make-local-variable 'comment-start-skip)
  (setq comment-start "!")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "!+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent)
  (run-hooks 'uil-mode-hook))

;; XEmacs addition
;;;###autoload(add-to-list 'auto-mode-alist '("\\.uil$" . uil-mode))

(provide 'uil-mode)

;;; uil-mode.el ends here
