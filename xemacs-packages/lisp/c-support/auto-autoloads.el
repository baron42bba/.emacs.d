;;; DO NOT MODIFY THIS FILE
(if (featurep 'c-support-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "c-support/_pkg.el")

(package-provide 'c-support :version 1.22 :author-version "No-Upstream-Ver" :type 'single-file)

;;;***

;;;### (autoloads (c-comment-edit) "c-comment-edit" "c-support/c-comment-edit.el")

(autoload 'c-comment-edit "c-comment-edit" "\
Edit multi-line C comments.
This command allows the easy editing of a multi-line C comment like this:
   /*
    * ...
    * ...
    */
The comment may be indented or flush with the left margin.

If point is within a comment, that comment is used.  Otherwise the
comment to be edited is found by searching forward from point.

With one \\[universal-argument] searching starts after moving back one
  paragraph.
With two \\[universal-argument]'s searching starts at the beginning of the
  current or proceeding C function.
With three \\[universal-argument]'s searching starts at the beginning of the
  current page.
With four \\[universal-argument]'s searching starts at the beginning of the
  current buffer (clipping restrictions apply).

Once located, the comment is copied into a temporary buffer, the comment
leaders and delimiters are stripped away and the resulting buffer is
selected for editing.  The major mode of this buffer is controlled by
the variable `c-comment-edit-mode'.\\<c-comment-edit-map>

Use \\[c-comment-edit-end] when you have finished editing the comment.  The
comment will be inserted into the original buffer with the appropriate
delimiters and indention, replacing the old version of the comment.  If
you don't want your edited version of the comment to replace the
original, use \\[c-comment-edit-abort]." t nil)

;;;***

;;;### (autoloads (c-macro-expand) "cmacexp" "c-support/cmacexp.el")

(autoload 'c-macro-expand "cmacexp" "\
Expand C macros in the region, using the C preprocessor.
Normally display output in temp buffer, but
prefix arg means replace the region with it.

`c-macro-preprocessor' specifies the preprocessor to use.
Prompt for arguments to the preprocessor (e.g. `-DDEBUG -I ./include')
if the user option `c-macro-prompt-flag' is non-nil.

Noninteractive args are START, END, SUBST.
For use inside Lisp programs, see also `c-macro-expansion'." t nil)

;;;***

;;;### (autoloads (ctypes-read-file ctypes-auto-parse-mode ctypes-file ctypes-dir ctypes-tags ctypes-all-buffers ctypes-buffer ctypes-define-type-in-mode ctypes-define-type) "ctypes" "c-support/ctypes.el")

(autoload 'ctypes-define-type "ctypes" "\
Add a new TYPE to current major mode and inform font-lock.

When preceded by C-u the display is not updated.

Return non-nil if the type was not known before." t nil)

(autoload 'ctypes-define-type-in-mode "ctypes" "\
Add TYPE to major mode MODE and inform font-lock.

When preceded by C-u the display is not updated.

\(This function is designed for interactive use, please call
`ctypes-define-type' from Lisp programs.)" t nil)

(autoload 'ctypes-buffer "ctypes" "\
Search for types in buffer, inform font-lock if any is found.

When preceded by C-u the action is not performed.

Return non-nil if new types are found." t nil)

(autoload 'ctypes-all-buffers "ctypes" "\
Search for types in all buffers, inform font-lock about all discoveries.

When preceded by C-u the display is not updated.

Return non-nil if new types are found." t nil)

(autoload 'ctypes-tags "ctypes" "\
Search for types in files in the visited TAGS table.
Should no tags table be visited, the user will be prompted for a new.

When preceded by C-u the display is not updated.

Return non-nil if new types are found." t nil)

(autoload 'ctypes-dir "ctypes" "\
Search for types in files in a directory hierarchy.

See variable `ctypes-dir-read-file' for a description of which files
are opened during scanning, and how you can change the behavior.

When preceded by C-u the display is not updated.

Return non-nil if new types are found." t nil)

(autoload 'ctypes-file "ctypes" "\
Search for types in file FILE.
Should FILE not be loaded it is read into a temporary buffer.

Return mode of file, if new types was found." t nil)

(autoload 'ctypes-auto-parse-mode "ctypes" "\
Toggle CTypes auto parse mode; search all new buffers for types.
With arg, turn types Auto Mode on if and only if arg is positive.

This a global minor mode, it does not have a private keymap, nor does
it add itself to the mode line.

Place the following in your startup file to enable this feature in
future sessions:

    (require 'ctypes)
    (ctypes-auto-parse-mode 1)

When activated, the functions in the hook `ctypes-auto-parse-mode-hook'
is called with no args." t nil)

(autoload 'ctypes-read-file "ctypes" "\
Load types previously saved with `ctypes-write-file'.
The name of the file is given by the optional argument FILE.
Should no file name be given the value of the variable `ctypes-file-name'
is used.

Please note that the types read will be added to the current types.

When preceded by C-u the display is not updated.

The third argument, NO-ERROR, determines whether or not we should
raise an error if there should be any problem loading the file.

Should the fourth argument, QUIETLY, be non-nil no messages are
generated when the file is loaded.

Return non-nil if new types are found." t nil)

;;;***

;;;### (autoloads (hide-ifdef-mode) "hideif" "c-support/hideif.el")

(defvar hide-ifdef-mode-map nil "\
Keymap used with Hide-Ifdef mode.")

(add-minor-mode 'hide-ifdef-mode " Ifdef" hide-ifdef-mode-map)

(autoload 'hide-ifdef-mode "hideif" "\
Toggle Hide-Ifdef mode.  This is a minor mode, albeit a large one.
With ARG, turn Hide-Ifdef mode on if arg is positive, off otherwise.
In Hide-Ifdef mode, code within #ifdef constructs that the C preprocessor
would eliminate may be hidden from view.  Several variables affect
how the hiding is done:

hide-ifdef-env
	An association list of defined and undefined symbols for the
	current buffer.  Initially, the global value of `hide-ifdef-env'
	is used.

hide-ifdef-define-alist
	An association list of defined symbol lists.  
        Use `hide-ifdef-set-define-alist' to save the current `hide-ifdef-env'
        and `hide-ifdef-use-define-alist' to set the current `hide-ifdef-env'
        from one of the lists in `hide-ifdef-define-alist'.

hide-ifdef-lines
	Set to non-nil to not show #if, #ifdef, #ifndef, #else, and
	#endif lines when hiding.

hide-ifdef-initially
	Indicates whether `hide-ifdefs' should be called when Hide-Ifdef mode
	is activated.

hide-ifdef-read-only
	Set to non-nil if you want to make buffers read only while hiding.
	After `show-ifdefs', read-only status is restored to previous value.

\\{hide-ifdef-mode-map}" t nil)

(defcustom hide-ifdef-initially nil "*Non-nil means call `hide-ifdefs' when Hide-Ifdef mode is first activated." :type 'boolean :group 'hide-ifdef)

(defcustom hide-ifdef-read-only nil "*Set to non-nil if you want buffer to be read-only while hiding text." :type 'boolean :group 'hide-ifdef)

(defcustom hide-ifdef-lines nil "*Non-nil means hide the #ifX, #else, and #endif lines." :type 'boolean :group 'hide-ifdef)

;;;***

;;;### (autoloads (hs-minor-mode) "hideshow" "c-support/hideshow.el")

(defcustom hs-hide-comments-when-hiding-all t "*Hide the comments too when you do an `hs-hide-all'." :type 'boolean :group 'hideshow)

(defvar hs-special-modes-alist '((c-mode "{" "}" "/[*/]" nil hs-c-like-adjust-block-beginning) (c++-mode "{" "}" "/[*/]" nil hs-c-like-adjust-block-beginning) (bibtex-mode ("^@\\S(*\\(\\s(\\)" 1)) (java-mode "{" "}" "/[*/]" nil hs-c-like-adjust-block-beginning)) "\
*Alist for initializing the hideshow variables for different modes.
Each element has the form
  (MODE START END COMMENT-START FORWARD-SEXP-FUNC ADJUST-BEG-FUNC).

If non-nil, hideshow will use these values as regexps to define blocks
and comments, respectively for major mode MODE.

START, END and COMMENT-START are regular expressions.  A block is
defined as text surrounded by START and END.

As a special case, START may be a list of the form (COMPLEX-START
MDATA-SELECTOR), where COMPLEX-START is a regexp w/ multiple parts and
MDATA-SELECTOR an integer that specifies which sub-match is the proper
place to adjust point, before calling `hs-forward-sexp-func'.  For
example, see the `hs-special-modes-alist' entry for `bibtex-mode'.

For some major modes, `forward-sexp' does not work properly.  In those
cases, FORWARD-SEXP-FUNC specifies another function to use instead.

See the documentation for `hs-adjust-block-beginning' to see what is the
use of ADJUST-BEG-FUNC.

If any of the elements is left nil or omitted, hideshow tries to guess
appropriate values.  The regexps should not contain leading or trailing
whitespace.  Case does not matter.")

(autoload 'hs-minor-mode "hideshow" "\
Toggle hideshow minor mode.
With ARG, turn hideshow minor mode on if ARG is positive, off otherwise.
When hideshow minor mode is on, the menu bar is augmented with hideshow
commands and the hideshow commands are enabled.
The value '(hs . t) is added to `buffer-invisibility-spec'.

The main commands are: `hs-hide-all', `hs-show-all', `hs-hide-block',
`hs-show-block', `hs-hide-level' and `hs-toggle-hiding'.  There is also
`hs-hide-initial-comment-block' and `hs-mouse-toggle-hiding'.

Turning hideshow minor mode off reverts the menu bar and the
variables to default values and disables the hideshow commands.

Lastly, the normal hook `hs-minor-mode-hook' is run using `run-hooks'.

Key bindings:
\\{hs-minor-mode-map}" t nil)

;;;***

(provide 'c-support-autoloads)
