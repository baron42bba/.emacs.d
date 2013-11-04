;;; DO NOT MODIFY THIS FILE
(if (featurep 'ediff-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "ediff/_pkg.el")

(package-provide 'ediff :version 1.62 :author-version "2.75" :type 'regular)

;;;***

;;;### (autoloads (ediff-customize) "ediff-help" "ediff/ediff-help.el")

(autoload 'ediff-customize "ediff-help" nil t nil)

;;;***

;;;### (autoloads nil "ediff-hook" "ediff/ediff-hook.el")

(defvar ediff-window-setup-function)
(defmacro ediff-cond-compile-for-xemacs-or-emacs (xemacs-form emacs-form) (if (string-match "XEmacs" emacs-version) xemacs-form emacs-form))

(ediff-cond-compile-for-xemacs-or-emacs (defun ediff-xemacs-init-menus nil (if (featurep 'menubar) (progn (add-submenu '("Tools") ediff-menu "OO-Browser...") (add-submenu '("Tools") ediff-merge-menu "OO-Browser...") (add-submenu '("Tools") epatch-menu "OO-Browser...") (add-submenu '("Tools") ediff-misc-menu "OO-Browser...") (add-menu-button '("Tools") "-------" "OO-Browser...")))) nil)

(ediff-cond-compile-for-xemacs-or-emacs (progn (defvar ediff-menu '("Compare" ["Two Files..." ediff-files t] ["Two Buffers..." ediff-buffers t] ["Three Files..." ediff-files3 t] ["Three Buffers..." ediff-buffers3 t] "---" ["Two Directories..." ediff-directories t] ["Three Directories..." ediff-directories3 t] "---" ["File with Revision..." ediff-revision t] ["Directory Revisions..." ediff-directory-revisions t] "---" ["Windows Word-by-word..." ediff-windows-wordwise t] ["Windows Line-by-line..." ediff-windows-linewise t] "---" ["Regions Word-by-word..." ediff-regions-wordwise t] ["Regions Line-by-line..." ediff-regions-linewise t])) (defvar ediff-merge-menu '("Merge" ["Files..." ediff-merge-files t] ["Files with Ancestor..." ediff-merge-files-with-ancestor t] ["Buffers..." ediff-merge-buffers t] ["Buffers with Ancestor..." ediff-merge-buffers-with-ancestor t] "---" ["Directories..." ediff-merge-directories t] ["Directories with Ancestor..." ediff-merge-directories-with-ancestor t] "---" ["Revisions..." ediff-merge-revisions t] ["Revisions with Ancestor..." ediff-merge-revisions-with-ancestor t] ["Directory Revisions..." ediff-merge-directory-revisions t] ["Directory Revisions with Ancestor..." ediff-merge-directory-revisions-with-ancestor t])) (defvar epatch-menu '("Apply Patch" ["To a file..." ediff-patch-file t] ["To a buffer..." ediff-patch-buffer t])) (defvar ediff-misc-menu '("Ediff Miscellanea" ["Ediff Manual" ediff-documentation t] ["Customize Ediff" ediff-customize t] ["List Ediff Sessions" ediff-show-registry t] ["Use separate frame for Ediff control buffer" ediff-toggle-multiframe :style toggle :selected (if (and (featurep 'ediff-util) (boundp 'ediff-window-setup-function)) (eq ediff-window-setup-function 'ediff-setup-windows-multiframe))] ["Use a toolbar with Ediff control buffer" ediff-toggle-use-toolbar :style toggle :selected (if (featurep 'ediff-tbar) (ediff-use-toolbar-p))])) (if (and (featurep 'menubar) (not (featurep 'infodock)) (not (featurep 'ediff-hook))) (ediff-xemacs-init-menus))) (if (featurep 'menu-bar) (progn (defvar menu-bar-ediff-misc-menu (make-sparse-keymap "Ediff Miscellanea")) (fset 'menu-bar-ediff-misc-menu (symbol-value 'menu-bar-ediff-misc-menu)) (defvar menu-bar-epatch-menu (make-sparse-keymap "Apply Patch")) (fset 'menu-bar-epatch-menu (symbol-value 'menu-bar-epatch-menu)) (defvar menu-bar-ediff-merge-menu (make-sparse-keymap "Merge")) (fset 'menu-bar-ediff-merge-menu (symbol-value 'menu-bar-ediff-merge-menu)) (defvar menu-bar-ediff-menu (make-sparse-keymap "Compare")) (fset 'menu-bar-ediff-menu (symbol-value 'menu-bar-ediff-menu)) (define-key menu-bar-ediff-menu [window] '("This Window and Next Window" . compare-windows)) (define-key menu-bar-ediff-menu [ediff-windows-linewise] '("Windows Line-by-line..." . ediff-windows-linewise)) (define-key menu-bar-ediff-menu [ediff-windows-wordwise] '("Windows Word-by-word..." . ediff-windows-wordwise)) (define-key menu-bar-ediff-menu [separator-ediff-windows] '("--")) (define-key menu-bar-ediff-menu [ediff-regions-linewise] '("Regions Line-by-line..." . ediff-regions-linewise)) (define-key menu-bar-ediff-menu [ediff-regions-wordwise] '("Regions Word-by-word..." . ediff-regions-wordwise)) (define-key menu-bar-ediff-menu [separator-ediff-regions] '("--")) (define-key menu-bar-ediff-menu [ediff-dir-revision] '("Directory Revisions..." . ediff-directory-revisions)) (define-key menu-bar-ediff-menu [ediff-revision] '("File with Revision..." . ediff-revision)) (define-key menu-bar-ediff-menu [separator-ediff-directories] '("--")) (define-key menu-bar-ediff-menu [ediff-directories3] '("Three Directories..." . ediff-directories3)) (define-key menu-bar-ediff-menu [ediff-directories] '("Two Directories..." . ediff-directories)) (define-key menu-bar-ediff-menu [separator-ediff-files] '("--")) (define-key menu-bar-ediff-menu [ediff-buffers3] '("Three Buffers..." . ediff-buffers3)) (define-key menu-bar-ediff-menu [ediff-files3] '("Three Files..." . ediff-files3)) (define-key menu-bar-ediff-menu [ediff-buffers] '("Two Buffers..." . ediff-buffers)) (define-key menu-bar-ediff-menu [ediff-files] '("Two Files..." . ediff-files)) (define-key menu-bar-ediff-merge-menu [ediff-merge-dir-revisions-with-ancestor] '("Directory Revisions with Ancestor..." . ediff-merge-directory-revisions-with-ancestor)) (define-key menu-bar-ediff-merge-menu [ediff-merge-dir-revisions] '("Directory Revisions..." . ediff-merge-directory-revisions)) (define-key menu-bar-ediff-merge-menu [ediff-merge-revisions-with-ancestor] '("Revisions with Ancestor..." . ediff-merge-revisions-with-ancestor)) (define-key menu-bar-ediff-merge-menu [ediff-merge-revisions] '("Revisions..." . ediff-merge-revisions)) (define-key menu-bar-ediff-merge-menu [separator-ediff-merge] '("--")) (define-key menu-bar-ediff-merge-menu [ediff-merge-directories-with-ancestor] '("Directories with Ancestor..." . ediff-merge-directories-with-ancestor)) (define-key menu-bar-ediff-merge-menu [ediff-merge-directories] '("Directories..." . ediff-merge-directories)) (define-key menu-bar-ediff-merge-menu [separator-ediff-merge-dirs] '("--")) (define-key menu-bar-ediff-merge-menu [ediff-merge-buffers-with-ancestor] '("Buffers with Ancestor..." . ediff-merge-buffers-with-ancestor)) (define-key menu-bar-ediff-merge-menu [ediff-merge-buffers] '("Buffers..." . ediff-merge-buffers)) (define-key menu-bar-ediff-merge-menu [ediff-merge-files-with-ancestor] '("Files with Ancestor..." . ediff-merge-files-with-ancestor)) (define-key menu-bar-ediff-merge-menu [ediff-merge-files] '("Files..." . ediff-merge-files)) (define-key menu-bar-epatch-menu [ediff-patch-buffer] '("To a Buffer..." . ediff-patch-buffer)) (define-key menu-bar-epatch-menu [ediff-patch-file] '("To a File..." . ediff-patch-file)) (define-key menu-bar-ediff-misc-menu [emultiframe] '("Toggle use of separate control buffer frame" . ediff-toggle-multiframe)) (define-key menu-bar-ediff-misc-menu [eregistry] '("List Ediff Sessions" . ediff-show-registry)) (define-key menu-bar-ediff-misc-menu [ediff-cust] '("Customize Ediff" . ediff-customize)) (define-key menu-bar-ediff-misc-menu [ediff-doc] '("Ediff Manual" . ediff-documentation)))))

;;;***

;;;### (autoloads (ediff-show-registry) "ediff-mult" "ediff/ediff-mult.el")

(autoload 'ediff-show-registry "ediff-mult" "\
Display Ediff's registry." t nil)

(defalias 'eregistry 'ediff-show-registry)

;;;***

;;;### (autoloads (ediff-toggle-use-toolbar ediff-toggle-multiframe) "ediff-util" "ediff/ediff-util.el")

(autoload 'ediff-toggle-multiframe "ediff-util" "\
Switch from multiframe display to single-frame display and back.
To change the default, set the variable `ediff-window-setup-function',
which see." t nil)

(autoload 'ediff-toggle-use-toolbar "ediff-util" "\
Enable or disable Ediff toolbar.
Works only in versions of Emacs that support toolbars.
To change the default, set the variable `ediff-use-toolbar-p', which see." t nil)

;;;***

;;;### (autoloads (ediff-documentation ediff-version ediff-revision ediff-patch-buffer ediff-patch-file run-ediff-from-cvs-buffer ediff-merge-revisions-with-ancestor ediff-merge-revisions ediff-merge-buffers-with-ancestor ediff-merge-buffers ediff-merge-files-with-ancestor ediff-merge-files ediff-regions-linewise ediff-regions-wordwise ediff-windows-linewise ediff-windows-wordwise ediff-merge-directory-revisions-with-ancestor ediff-merge-directory-revisions ediff-merge-directories-with-ancestor ediff-merge-directories ediff-directories3 ediff-directory-revisions ediff-directories ediff-buffers3 ediff-buffers ediff-backup ediff-files3 ediff-files) "ediff" "ediff/ediff.el")

(autoload 'ediff-files "ediff" "\
Run Ediff on a pair of files, FILE-A and FILE-B." t nil)

(autoload 'ediff-files3 "ediff" "\
Run Ediff on three files, FILE-A, FILE-B, and FILE-C." t nil)

(defalias 'ediff3 'ediff-files3)

(defalias 'ediff 'ediff-files)

(autoload 'ediff-backup "ediff" "\
Run Ediff on FILE and its backup file.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, `ediff' it with its original." t nil)

(autoload 'ediff-buffers "ediff" "\
Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B." t nil)

(defalias 'ebuffers 'ediff-buffers)

(autoload 'ediff-buffers3 "ediff" "\
Run Ediff on three buffers, BUFFER-A, BUFFER-B, and BUFFER-C." t nil)

(defalias 'ebuffers3 'ediff-buffers3)

(autoload 'ediff-directories "ediff" "\
Run Ediff on a pair of directories, DIR1 and DIR2, comparing files that have
the same name in both.  The third argument, REGEXP, is nil or a regular
expression; only file names that match the regexp are considered." t nil)

(defalias 'edirs 'ediff-directories)

(autoload 'ediff-directory-revisions "ediff" "\
Run Ediff on a directory, DIR1, comparing its files with their revisions.
The second argument, REGEXP, is a regular expression that filters the file
names.  Only the files that are under revision control are taken into account." t nil)

(defalias 'edir-revisions 'ediff-directory-revisions)

(autoload 'ediff-directories3 "ediff" "\
Run Ediff on three directories, DIR1, DIR2, and DIR3, comparing files that
have the same name in all three.  The last argument, REGEXP, is nil or a
regular expression; only file names that match the regexp are considered." t nil)

(defalias 'edirs3 'ediff-directories3)

(autoload 'ediff-merge-directories "ediff" "\
Run Ediff on a pair of directories, DIR1 and DIR2, merging files that have
the same name in both.  The third argument, REGEXP, is nil or a regular
expression; only file names that match the regexp are considered." t nil)

(defalias 'edirs-merge 'ediff-merge-directories)

(autoload 'ediff-merge-directories-with-ancestor "ediff" "\
Merge files in directories DIR1 and DIR2 using files in ANCESTOR-DIR as ancestors.
Ediff merges files that have identical names in DIR1, DIR2.  If a pair of files
in DIR1 and DIR2 doesn't have an ancestor in ANCESTOR-DIR, Ediff will merge
without ancestor.  The fourth argument, REGEXP, is nil or a regular expression;
only file names that match the regexp are considered." t nil)

(autoload 'ediff-merge-directory-revisions "ediff" "\
Run Ediff on a directory, DIR1, merging its files with their revisions.
The second argument, REGEXP, is a regular expression that filters the file
names.  Only the files that are under revision control are taken into account." t nil)

(defalias 'edir-merge-revisions 'ediff-merge-directory-revisions)

(autoload 'ediff-merge-directory-revisions-with-ancestor "ediff" "\
Run Ediff on a directory, DIR1, merging its files with their revisions and ancestors.
The second argument, REGEXP, is a regular expression that filters the file
names.  Only the files that are under revision control are taken into account." t nil)

(defalias 'edir-merge-revisions-with-ancestor 'ediff-merge-directory-revisions-with-ancestor)

(defalias 'edirs-merge-with-ancestor 'ediff-merge-directories-with-ancestor)

(autoload 'ediff-windows-wordwise "ediff" "\
Compare WIND-A and WIND-B, which are selected by clicking, wordwise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A." t nil)

(autoload 'ediff-windows-linewise "ediff" "\
Compare WIND-A and WIND-B, which are selected by clicking, linewise.
With prefix argument, DUMB-MODE, or on a non-windowing display, works as
follows:
If WIND-A is nil, use selected window.
If WIND-B is nil, use window next to WIND-A." t nil)

(autoload 'ediff-regions-wordwise "ediff" "\
Run Ediff on a pair of regions in specified buffers.
Regions (i.e., point and mark) are assumed to be set in advance except
for the second region in the case both regions are from the same buffer.
In such a case the user is asked to interactively establish the second
region.
This function is effective only for relatively small regions, up to 200
lines.  For large regions, use `ediff-regions-linewise'." t nil)

(autoload 'ediff-regions-linewise "ediff" "\
Run Ediff on a pair of regions in specified buffers.
Regions (i.e., point and mark) are assumed to be set in advance except
for the second region in the case both regions are from the same buffer.
In such a case the user is asked to interactively establish the second
region.
Each region is enlarged to contain full lines.
This function is effective for large regions, over 100-200
lines.  For small regions, use `ediff-regions-wordwise'." t nil)

(defalias 'ediff-merge 'ediff-merge-files)

(autoload 'ediff-merge-files "ediff" "\
Merge two files without ancestor." t nil)

(autoload 'ediff-merge-files-with-ancestor "ediff" "\
Merge two files with ancestor." t nil)

(defalias 'ediff-merge-with-ancestor 'ediff-merge-files-with-ancestor)

(autoload 'ediff-merge-buffers "ediff" "\
Merge buffers without ancestor." t nil)

(autoload 'ediff-merge-buffers-with-ancestor "ediff" "\
Merge buffers with ancestor." t nil)

(autoload 'ediff-merge-revisions "ediff" "\
Run Ediff by merging two revisions of a file.
The file is the optional FILE argument or the file visited by the current
buffer." t nil)

(autoload 'ediff-merge-revisions-with-ancestor "ediff" "\
Run Ediff by merging two revisions of a file with a common ancestor.
The file is the optional FILE argument or the file visited by the current
buffer." t nil)

(autoload 'run-ediff-from-cvs-buffer "ediff" "\
Run Ediff-merge on appropriate revisions of the selected file.
First run after `M-x cvs-update'.  Then place the cursor on a line describing a
file and then run `run-ediff-from-cvs-buffer'." t nil)

(autoload 'ediff-patch-file "ediff" "\
Run Ediff by patching SOURCE-FILENAME.
If optional PATCH-BUF is given, use the patch in that buffer
and don't ask the user.
If prefix argument, then: if even argument, assume that the patch is in a
buffer. If odd -- assume it is in a file." t nil)

(autoload 'ediff-patch-buffer "ediff" "\
Run Ediff by patching the buffer specified at prompt.
Without the optional prefix ARG, asks if the patch is in some buffer and
prompts for the buffer or a file, depending on the answer.
With ARG=1, assumes the patch is in a file and prompts for the file.
With ARG=2, assumes the patch is in a buffer and prompts for the buffer.
PATCH-BUF is an optional argument, which specifies the buffer that contains the
patch. If not given, the user is prompted according to the prefix argument." t nil)

(defalias 'epatch 'ediff-patch-file)

(defalias 'epatch-buffer 'ediff-patch-buffer)

(autoload 'ediff-revision "ediff" "\
Run Ediff by comparing versions of a file.
The file is an optional FILE argument or the file entered at the prompt.
Default: the file visited by the current buffer.
Uses `vc.el' or `rcs.el' depending on `ediff-version-control-package'." t nil)

(defalias 'erevision 'ediff-revision)

(autoload 'ediff-version "ediff" "\
Return string describing the version of Ediff.
When called interactively, displays the version." t nil)

(autoload 'ediff-documentation "ediff" "\
Display Ediff's manual.
With optional NODE, goes to that node." t nil)

;;;***

(provide 'ediff-autoloads)
