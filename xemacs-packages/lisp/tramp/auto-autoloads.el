;;; DO NOT MODIFY THIS FILE
(if (featurep 'tramp-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "lisp/_pkg.el")

(package-provide 'tramp :version 1.32 :author-version "2.0.53" :type 'regular)

;;;***

;;;### (autoloads (tramp-unload-tramp tramp-unload-file-name-handler-alist tramp-completion-file-name-handler tramp-file-name-handler) "tramp" "lisp/tramp.el")

(defvar tramp-unified-filenames (not (featurep 'xemacs)) "\
Non-nil means to use unified Ange-FTP/Tramp filename syntax.
Nil means to use a separate filename syntax for Tramp.")

(defconst tramp-file-name-regexp-unified "\\`/[^/:]+:" "\
Value for `tramp-file-name-regexp' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and
Tramp.  See `tramp-file-name-structure-unified' for more explanations.")

(defconst tramp-file-name-regexp-separate "\\`/\\[.*\\]" "\
Value for `tramp-file-name-regexp' for separate remoting.
XEmacs uses a separate filename syntax for Tramp and EFS.
See `tramp-file-name-structure-separate' for more explanations.")

(defcustom tramp-file-name-regexp (if tramp-unified-filenames tramp-file-name-regexp-unified tramp-file-name-regexp-separate) "*Regular expression matching file names handled by tramp.\nThis regexp should match tramp file names but no other file names.\n(When tramp.el is loaded, this regular expression is prepended to\n`file-name-handler-alist', and that is searched sequentially.  Thus,\nif the tramp entry appears rather early in the `file-name-handler-alist'\nand is a bit too general, then some files might be considered tramp\nfiles which are not really tramp files.\n\nPlease note that the entry in `file-name-handler-alist' is made when\nthis file (tramp.el) is loaded.  This means that this variable must be set\nbefore loading tramp.el.  Alternatively, `file-name-handler-alist' can be\nupdated after changing this variable.\n\nAlso see `tramp-file-name-structure'." :group 'tramp :type 'regexp)

(defconst tramp-completion-file-name-regexp-unified "^/$\\|^/[^/:][^/]*$" "\
Value for `tramp-completion-file-name-regexp' for unified remoting.
Emacs (not XEmacs) uses a unified filename syntax for Ange-FTP and
Tramp.  See `tramp-file-name-structure-unified' for more explanations.")

(defconst tramp-completion-file-name-regexp-separate "^/\\([[][^]]*\\)?$" "\
Value for `tramp-completion-file-name-regexp' for separate remoting.
XEmacs uses a separate filename syntax for Tramp and EFS.
See `tramp-file-name-structure-separate' for more explanations.")

(defcustom tramp-completion-file-name-regexp (if tramp-unified-filenames tramp-completion-file-name-regexp-unified tramp-completion-file-name-regexp-separate) "*Regular expression matching file names handled by tramp completion.\nThis regexp should match partial tramp file names only.\n\nPlease note that the entry in `file-name-handler-alist' is made when\nthis file (tramp.el) is loaded.  This means that this variable must be set\nbefore loading tramp.el.  Alternatively, `file-name-handler-alist' can be\nupdated after changing this variable.\n\nAlso see `tramp-file-name-structure'." :group 'tramp :type 'regexp)

(autoload 'tramp-file-name-handler "tramp" "\
Invoke Tramp file name handler.
Falls back to normal file name handler if no tramp file name handler exists." nil nil)

(autoload 'tramp-completion-file-name-handler "tramp" "\
Invoke tramp file name completion handler.
Falls back to normal file name handler if no tramp file name handler exists." nil nil)

(put 'tramp-completion-file-name-handler 'safe-magic t)

(add-to-list 'file-name-handler-alist (cons tramp-file-name-regexp 'tramp-file-name-handler))

(autoload 'tramp-unload-file-name-handler-alist "tramp" nil nil nil)

(autoload 'tramp-unload-tramp "tramp" nil t nil)

;;;***

(provide 'tramp-autoloads)
