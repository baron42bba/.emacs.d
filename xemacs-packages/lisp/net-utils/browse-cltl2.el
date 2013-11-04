; -*- Mode: Emacs-Lisp -*- 
;;; browse-cltl2.el --- browse the hypertext-version of 
;;;                     "Common Lisp the Language, 2nd. Edition"

;; Revision 1.1.4
;; last edited on 19.5.1998

;; Copyright (C) 1997, 1998 Holger Schauer

;; Author: Holger Schauer <Holger.Schauer@gmx.de>
;; Keywords: utils lisp ilisp www

;; This file is part of XEmacs.

;; Developed under XEmacs 19.14. Also tested on Emacs 19.29, 19.32 
;; and XEmacs 19.11, 19.16, 20.3. Should work with newer versions, too.
;; Required: browse-url.el
;; Recommended: url.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; This gives you two top-level-functions useful when programming lisp:
;; cltl2-view-function-definition and cltl2-view-index
;; cltl2-view-function-definition asks you for a name of a lisp
;; function (or variable) and will open up your favourite browser
;; (as specified by `browse-url-browser-function') loading the page
;; which documents it.

;;; Installation: (as usual)
;; Put browse-cltl2.el somewhere where emacs can find it.
;; browse-cltl2.el requires a working browse-url, url and cl.
;; Insert the following lines in your .emacs:
;;
;;      (autoload 'cltl2-view-function-definition "browse-cltl2")
;;      (autoload 'cltl2-view-index "browse-cltl2")
;;      (autoload 'cltl2-lisp-mode-install "browse-cltl2")
;;      (add-hook 'lisp-mode-hook 'cltl2-lisp-mode-install)
;;      (add-hook 'ilisp-mode-hook 'cltl2-lisp-mode-install)
;;
;; This should also add the needed hooks to lisp-mode (and ilisp-mode).

;; Gnu Emacs:
;; For Gnu Emacs there doesn't seem to be a lisp-mode-hook so you're
;; on your own with the key-settings.
;; No url.el:
;; If you don't have url.el set *cltl2-use-url* to nil
;; and set *cltl2-fetch-method* to 'local or 'local-index-only.
;; This implies that you need a local copy of the index page of
;; CLtL2 (which you can get from the normal hypertext-version at CMU),
;; so you need to point *cltl2-local-file-pos* and *cltl2-index-file-name*
;; to the place where you put it.
;; Old versions of Emacs (Emacs 19.29 and XEmacs 19.11 for example):
;; When you want to use a local copy (or a local copy of the index file)
;; check the documentation on find-file-noselect. If it doesn't mention
;; an option called RAWFILE set *cltl2-old-find-file-noselect* to 't.


;;; Customization:
;; By default, browse-cltl2 will use a local copy of CLtL2, looking
;; for it in /usr/doc/html/cltl. This can be modified with the help
;; of the following variables:
;; *cltl2-fetch-method*, *cltl2-url*, *cltl-local-file-pos*
;; See the documentation on this variables for more info.
;;
;;; TODO:
;; In this version we can't separate between functions, variables, 
;; constants and loop clauses. This is not that hard to change,
;; but it is more difficult to distinguish what the user is
;; looking for. Until I receive several requests for it, I won't
;; implement it, because there are not that much constructs like * and + 
;; which have two (or more) semantics.

;;; Changes:
;; 28-01-97: HS: now we're using cl-puthash all over the place because
;;          this is common on XEmacs 19.11 and upwards and Gnu Emacs.
;;          Added information on how to install without url.el
;;
;; 29-01-97 HS: included conditionalized versions of the required
;;         functions match-string and buffer-live-p. 
;;         Suggested by Simon Marshall <Simon.Marshall@esrin.esa.it>.
;;         Included new variable *cltl2-use-url* with which one can
;;         specify if he has url.el or not. Introduced variable
;;         *cltl2-old-find-file-noselect*.
;;
;; 05-02-97 HS: added two variables for the key-bindings,
;;         *cltl2-vfd-key* *cltl2-vi-key*.
;;
;; 18-02-97 HS: use compatible keybindings that work on Gnu Emacs and XEmacs.
;;         Made cltl2-lisp-mode-install an interactive function.
;;
;; 28-02-98 HS: use symbol-near-point to obtain a default value to
;;         search for and added a history for already searched entries.
;;
;; 22-04-98 HS: added an error message if index-file does not exist.
;;
;; 27-04-98 HS: fixed a bug with the minibuffer history. Thanks to
;;          Sam Mikes <smikes@alumni.hmc.edu> for pointing out the
;;          existence of `minibuffer-history-minimum-string-length'
;;
;; 19-05-98 HS: changed #' to (function ... in order to make it work
;;          with older emacsen. `minibuffer-history-minimum-string-length'
;;          is also unknown to older emacsen. `read-string' had only
;;          two arguments in Emacs 19.29.


(defvar *cltl2-use-url* 'nil
 "Enables or disables retrieval of the index-file via WWW (or more
 exactly by the use of the function url-retrieve from url.el).
 Default is 't.")

;; needed things
(require 'cl)
(require 'browse-url)
(autoload 'url-retrieve "url")

(when (not *cltl2-use-url*)
   (require 'url))

;;; ******************************
;;; Some variable and constant definitions
;;; ******************************
(defvar *cltl2-fetch-method* 'local
 "This sets the method by which the index-file will be fetched. Three
  methods are possible: 'local assumes that all files are local. 
  'local-index-only assumes that just the index-file is locally but
  all other files will be fetched via www. 'www means that the index-file
  will be fetched via WWW, too. Don't change the value of this variable
  after loading.")

(defvar *cltl2-url* 
 "http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/"
 "The url where the hypertext-version of Common Lisp the Language
 can be found. Note that this assumes to be the top-level of the
 directory structure which should be the same as in the hypertext
 version as provided by the CMU AI Repository. Defaults to
 http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/
 Note the / at the end.")

(defvar *cltl2-local-file-pos* "/usr/doc/html/cltl/"
 "A directory where the CLtl2 can be found. Note that this assumes
 to be the top-level of the directory structure which should be the
 same as in the hypertext version as provided by the CMU AI Repository.
 Defaults to /usr/doc/html/cltl/ Note the / at the end.")

(defconst *cltl2-index-file-name* "clm/index.html"
 "The name of the index-file, typically with directory on front. 
  Defaults to clm/index.html, as this is the momentary position from
  the top-level directory of the CLtL2-home. Defaults to clm/index.html.
  Note that there is no leading /.")

(defvar *cltl2-index-home* 
  (concatenate 'string
     (case *cltl2-fetch-method*
       ('local *cltl2-local-file-pos*)
       ('local-index-only *cltl2-local-file-pos*)
       ('www *cltl2-url*))
     *cltl2-index-file-name*)
 "The absolute path which will be used to fetch the index.")

(defvar *cltl2-home*
  (concatenate 
   'string
   (case *cltl2-fetch-method*
     ('local *cltl2-local-file-pos*)
     ('local-index-only *cltl2-url*)
     ('www *cltl2-url*))
     "clm/")
  "This specifies the home-position of the CLtL2. The value of this variable
  will be concatenated with the name of the nodes of the CLtL2.")

(defvar *cltl2-index-buffer-name* "*cltl2-index*"
 "The name of the buffer which holds the index for CLtL2.")

(defvar *cltl2-old-find-file-noselect* 'nil
 "Older versions of Emacs (at least XEmacs 19.11) don't support the
 option RAWFILE with the function FIND-FILE-NO-SELECT. Set this variable
 to 't if you have such an old version. It will cause fontification and
 other useless stuff on the buffer in which the index is fetched. If
 you don't use a local copy (of the index) this won't bother you.")

(defvar *cltl2-vfd-key* 
  (if (featurep 'ilisp)
      '[(control z) h]
     '[(control c) b])
 "Shortcut for accessing cltl2-view-function-definition. Use meaningful
 setting with Ilisp.")

(defvar *cltl2-vi-key* 
  (if (featurep 'ilisp)
      '[(control z) H]
     '[(control c) B])
 "Shortcut for accessing cltl2-view-index. Use meaningful
 setting with Ilisp.")

(defvar *browse-cltl2-ht* (make-hash-table 0))
(defconst *cltl2-search-regexpr* 
  "<a href=\"\\(.+\\)\"><code>\\(.+\\)</code></a>"
  "A regular expression how to check for entries in the index-file
  of CLtL2. Note that you have to modify this and the 
  prepare-get-entry*-functions if you want to change the search.")

(defvar *browse-cltl2-history* nil
  "History of CLtL2-entries to lookup.")

;;; ******************************
;;; First of all: Compatibility stuff
;;; ******************************
; no match-string in old versions
(if (not (fboundp (function match-string)))
    (defun match-string (num &optional string)
      "Return string of text matched by last search.
 NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
 Zero means the entire text matched by the whole regexp or whole string.
 STRING should be given if the last search was by `string-match' on STRING."
      (if (match-beginning num)
	  (if string
	      (substring string (match-beginning num) (match-end num))
	      (buffer-substring 
	       (match-beginning num) (match-end num))))))

; no buffer-live-p in old versions
 (if (not (fboundp (function buffer-live-p)))
     (defun buffer-live-p (buf-or-name)
       "Checks if BUF-OR-NAME is a live buffer. Returns non-nil
 if BOF-OR-NAME is an editor buffer which has not been deleted.
 Imitating a built-in function from newer Emacs versions."
       (let ((object (if (bufferp buf-or-name) 
                          buf-or-name
			(get-buffer buf-or-name))))
	 (and (bufferp object) (buffer-name object)))))

; no add-submenu in old versions of XEmacs       
(if (and (string-match "XEmacs\\|Lucid" emacs-version)
	 (not (fboundp 'add-submenu)))
    (defun add-submenu (menu-path submenu &optional before)
  "Add a menu to the menubar or one of its submenus.
If the named menu exists already, it is changed.
MENU-PATH identifies the menu under which the new menu should be inserted.
 It is a list of strings; for example, (\"File\") names the top-level \"File\"
 menu.  (\"File\" \"Foo\") names a hypothetical submenu of \"File\".
 If MENU-PATH is nil, then the menu will be added to the menubar itself.
SUBMENU is the new menu to add.
 See the documentation of `current-menubar' for the syntax.
BEFORE, if provided, is the name of a menu before which this menu should
 be added, if this menu is not on its parent already.  If the menu is already
 present, it will not be moved."
  (add-menu menu-path (car submenu) (cdr submenu) before)))

; stolen from XEmacs 19.15 syntax.el
(if (not (fboundp (function symbol-near-point)))
    (defun symbol-near-point ()
      "Return the first textual item to the nearest point."
      (interactive)
	;alg stolen from etag.el
      (save-excursion
	(if (not (memq (char-syntax (preceding-char)) '(?w ?_)))
	    (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
	      (forward-char 1)))
	(while (looking-at "\\sw\\|\\s_")
	  (forward-char 1))
	(if (re-search-backward "\\sw\\|\\s_" nil t)
	    (regexp-quote
	     (progn (forward-char 1)
		    (buffer-substring (point)
				      (progn (forward-sexp -1)
					     (while (looking-at "\\s'")
					       (forward-char 1))
					     (point)))))
	  nil))))

; needed for 19.11, I think
(if (not (fboundp (function url-lazy-message)))
  (defun url-lazy-message (&rest args)
    "Just like `message', but is a no-op if called more than once a second.
Will not do anything if url-show-status is nil."
    (if (or (null url-show-status)
	    (= url-lazy-message-time
	       (setq url-lazy-message-time (nth 1 (current-time)))))
	nil
      (apply 'message args))))

; old find-file-noselect has no RAWFILE argument
(if *cltl2-old-find-file-noselect*
    (unless (boundp 'cltl2-old-find-file-noselect-func)
      (setf (symbol-value 'cltl2-old-find-file-noselect-func)
	    (symbol-function 'find-file-noselect))
      (setf (symbol-function 'find-file-noselect)
	    (function 
	     (lambda (file &optional nowarn rawfile)
	       (funcall cltl2-old-find-file-noselect-func file nowarn))))))
  
;;; ******************************
;;; Functions for fetching the index file
;;; ******************************
(defun cltl2-fetch-index ()
  "Fetches the index page of the CLtl2 and puts it in its own
 buffer called *cltl2-index*."
  ;; if the index isn't here load it into a buffer
  (when (or (not (get-buffer *cltl2-index-buffer-name*))
	    (not (buffer-live-p *cltl2-index-buffer-name*)))
    (message "Fetching the CLtL2 index file ...")
    (case *cltl2-fetch-method* 
      ('local 
       (cltl2-fetch-index-by-file))
      ('local-index-only
       (cltl2-fetch-index-by-file))
      ('www
       (cltl2-fetch-index-by-www))))
  
  (cltl2-prepare-index)
)

;; fetch methods
(defun cltl2-fetch-index-by-file ()
  "Fetch the index from disk."
  (if (not (file-readable-p *cltl2-index-home*))
      (error "CLtL2 index file not readable: %s." *cltl2-index-home*))

  (setf *cltl2-index-buffer-name*
	(find-file-noselect *cltl2-index-home* 'nil 't)))

(defun cltl2-fetch-index-by-www ()
 "Fetch the index via WWW."
 (save-excursion
   (let ((old-url-working-buffer url-working-buffer))
     (setf url-working-buffer *cltl2-index-buffer-name*)
     (url-retrieve *cltl2-index-home*)
     (setf url-working-buffer old-url-working-buffer))))


;;; ******************************
;;; Main functions for viewing
;;; ******************************
(defun cltl2-view-function-definition (entry)
  "First checks if function can be found in the CLtL2-index-file.
 If it can be found, uses the function browse-url to have a look
 at the corresponding documentation from CLtL2."
  (interactive 
   (let ((ol-hist-val (if (boundp 'minibuffer-history-minimum-string-length)
			  minibuffer-history-minimum-string-length
			nil))
	 (entry-val nil))
     (setq minibuffer-history-minimum-string-length nil)
     (setq entry-val
	   (list (read-from-minibuffer
		  (concatenate 'string
			       "CLtL2-Entry to lookup (default " 
			       (symbol-near-point) "):")
		  nil nil nil
		  '*browse-cltl2-history*)))
     (setq minibuffer-history-minimum-string-length ol-hist-val)
     entry-val))
	   
  (cond ((equal "" entry)
	 (setf entry (symbol-near-point))
	 (setf *browse-cltl2-history*
	       (push entry *browse-cltl2-history*))))

  (when (cltl2-index-unprepared-p)
    (cltl2-fetch-index))
  
  (let ((entry-url (cltl2-find-url-for-function (intern entry))))
    (when entry-url
     (message "Loading found entry for %s into browser.." entry)
     (browse-url 
      (concatenate 'string *cltl2-home* entry-url)))))

(defun cltl2-find-url-for-function (entry)
  "Checks if we can find a page for function ENTRY and
 constructs an URL from it."
  (let ((entry-url (gethash entry *browse-cltl2-ht*)))
    (when (not entry-url)
      (error "No entry in CLtL2 for %s" entry))
    entry-url))

(defun cltl2-view-index ()
  "Browse-urls the index file."
  (interactive)
  (browse-url *cltl2-index-home*))

;;; ******************************
;;; Preparing the index (the hashtable)
;;; ******************************
(defun cltl2-prepare-index ()
 "Jumps to the *cltl2-index* buffer and scans it, creating a hashtable
 for all entries."
 (message "Preparing CLtL2 index.")
 (save-excursion
   (set-buffer *cltl2-index-buffer-name*)
   (goto-char (point-min))

   ; search for entry
   (do ((point (re-search-forward 
                 *cltl2-search-regexpr* 
		 nil t)
	       (re-search-forward 
		*cltl2-search-regexpr* 
		nil t)))
       ; until we can't find anymore
       ((null point)); (format "Index-preparation done."))
     ; put found entry in hash-table
     (cl-puthash 
      (cltl2-prepare-get-entry-name)
      (cltl2-prepare-get-entry-url)
      *browse-cltl2-ht*))))

(defun cltl2-prepare-get-entry-name ()
 "Get the enrty name from the last match of regexp-search for entries."
 (let ((name-string (intern (match-string 2))))
   (format "%s" name-string)
 name-string))

(defun cltl2-prepare-get-entry-url ()
 "Get the enrty url from the last match of regexp-search for entries."
 (let ((url (match-string 1)))
   (format "%s" url)
   url))

(defun cltl2-index-unprepared-p ()
 "Check if the index is already prepared."
 ; If the hashtable has entries the index is prepared.
 (not (and (hash-table-p *browse-cltl2-ht*)
	   (>= (hash-table-count *browse-cltl2-ht*) 1))))
 
;;; ******************************
;;; Hooking into lisp mode and ilisp-mode
;;; ******************************
(defun cltl2-lisp-mode-install ()
 "Adds browse-cltl2 to lisp-mode. If you use ilisp (installed via a hook
 on lisp-mode) add browse-cltl2 to ilisp. Check the variables *cltl2-vfd-key*
 and *cltl2-vi-key* for the keybindings. Under XEmacs we will add ourself to
 the corresponding menus if there exists one."
 (interactive)
 ; set key bindings
 (local-set-key *cltl2-vfd-key* 'cltl2-view-function-definition)
 (local-set-key *cltl2-vi-key* 'cltl2-view-index)
 ; under XEmacs hook ourself into the menu if there is one
 (when (string-match "XEmacs\\|Lucid" emacs-version)
   (cond ((and (featurep 'ilisp-easy-menu)
	       ; this is for the menu as provided by ilisp-easy-menu
	       (not (null (car (find-menu-item current-menubar '("ILisp"))))))
	  (add-submenu
	   '("ILisp" "Documentation")
	   '("Browse CLtL2"
	     [ "View entry" cltl2-view-function-definition t]
	     [ "View index" cltl2-view-index t] )))
	   ; perhaps an other Ilisp-Menu is there ?
	 ((not (null (car (find-menu-item current-menubar '("ILisp")))))
	  (add-submenu
	   '("Lisp")
	   '("Browse CLtL2"
	     [ "View entry" cltl2-view-function-definition t]
	     [ "View index" cltl2-view-index t] )))
           ; or at least a Lisp-Menu ?
	 ((not (null (car (find-menu-item current-menubar '("Lisp")))))
	  (add-submenu
	   '("Lisp")
	   '("Browse CLtL2"
	     [ "View entry" cltl2-view-function-definition t]
	     [ "View index" cltl2-view-index t] )))))
)

(add-hook 'lisp-mode-hook 'cltl2-lisp-mode-install)
(add-hook 'ilisp-mode-hook 'cltl2-lisp-mode-install)

;;; Providing ourself. 
(provide 'ilisp-browse-cltl2)
;;; browse-cltl2.el ends here.
