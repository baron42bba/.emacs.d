;;; DO NOT MODIFY THIS FILE
(if (featurep 'edit-utils-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "edit-utils/_pkg.el")

(package-provide 'edit-utils :version 2.34 :author-version "No-Upstream-Ver" :type 'single-file)

;;;***

;;;### (autoloads (After-save--after-save-hook After-save--find-file-hook) "after-save-commands" "edit-utils/after-save-commands.el")

(autoload 'After-save--find-file-hook "after-save-commands" "\
Look up the name of this file in `After-save-alist', and if it has
an entry, turn on the modeline button and indicator." nil nil)

(autoload 'After-save--after-save-hook "after-save-commands" "\
An `after-save-hook' to run a shell-command.
This gets hung on the `after-save-hook'.
See: `After-save-alist'." nil nil)

;;;***

;;;### (autoloads (align-newline-and-indent align-unhighlight-rule align-highlight-rule align-current align-entire align-regexp align) "align" "edit-utils/align.el")

(autoload 'align "align" "\
Attempt to align a region based on a set of alignment rules.
BEG and END mark the region.  If BEG and END are specifically set to
nil (this can only be done programmatically), the beginning and end of
the current alignment section will be calculated based on the location
of point, and the value of `align-region-separate' (or possibly each
rule's `separate' attribute).

If SEPARATE is non-nil, it overrides the value of
`align-region-separate' for all rules, except those that have their
`separate' attribute set.

RULES and EXCLUDE-RULES, if either is non-nil, will replace the
default rule lists defined in `align-rules-list' and
`align-exclude-rules-list'.  See `align-rules-list' for more details
on the format of these lists." t nil)

(autoload 'align-regexp "align" "\
Align the current region using an ad-hoc rule read from the minibuffer.
BEG and END mark the limits of the region.  This function will prompt
for the REGEXP to align with.  If no prefix arg was specified, you
only need to supply the characters to be lined up and any preceding
whitespace is replaced.  If a prefix arg was specified, the full
regexp with parenthesized whitespace should be supplied; it will also
prompt for which parenthesis GROUP within REGEXP to modify, the amount
of SPACING to use, and whether or not to REPEAT the rule throughout
the line.  See `align-rules-list' for more information about these
options.

For example, let's say you had a list of phone numbers, and wanted to
align them so that the opening parentheses would line up:

    Fred (123) 456-7890
    Alice (123) 456-7890
    Mary-Anne (123) 456-7890
    Joe (123) 456-7890

There is no predefined rule to handle this, but you could easily do it
using a REGEXP like \"(\". All you would have to do is to mark the
region, call `align-regexp' and type in that regular expression." t nil)

(autoload 'align-entire "align" "\
Align the selected region as if it were one alignment section.
BEG and END mark the extent of the region.  If RULES or EXCLUDE-RULES
is set to a list of rules (see `align-rules-list'), it can be used to
override the default alignment rules that would have been used to
align that section." t nil)

(autoload 'align-current "align" "\
Call `align' on the current alignment section.
This function assumes you want to align only the current section, and
so saves you from having to specify the region.  If RULES or
EXCLUDE-RULES is set to a list of rules (see `align-rules-list'), it
can be used to override the default alignment rules that would have
been used to align that section." t nil)

(autoload 'align-highlight-rule "align" "\
Highlight the whitespace which a given rule would have modified.
BEG and END mark the extent of the region.  TITLE identifies the rule
that should be highlighted.  If RULES or EXCLUDE-RULES is set to a
list of rules (see `align-rules-list'), it can be used to override the
default alignment rules that would have been used to identify the text
to be colored." t nil)

(autoload 'align-unhighlight-rule "align" "\
Remove any highlighting that was added by `align-highlight-rule'." t nil)

(autoload 'align-newline-and-indent "align" "\
A replacement function for `newline-and-indent', aligning as it goes." t nil)

;;;***

;;;### (autoloads (global-auto-revert-mode turn-on-auto-revert-mode auto-revert-mode) "autorevert" "edit-utils/autorevert.el")

(defvar auto-revert-mode nil "\
*Non-nil when Auto-Revert Mode is active.

Never set this variable directly, use the command `auto-revert-mode'
instead.")

(defcustom global-auto-revert-mode nil "When on, buffers are automatically reverted when files on disk change.\n\nSet this variable using \\[customize] only.  Otherwise, use the\ncommand `global-auto-revert-mode'." :group 'auto-revert :initialize 'custom-initialize-default :set '(lambda (symbol value) (global-auto-revert-mode (or value 0))) :type 'boolean :require 'autorevert)

(autoload 'auto-revert-mode "autorevert" "\
Toggle reverting buffer when file on disk changes.

With arg, turn Auto Revert mode on if and only if arg is positive.
This is a minor mode that affects only the current buffer.
Use `global-auto-revert-mode' to automatically revert all buffers." t nil)

(autoload 'turn-on-auto-revert-mode "autorevert" "\
Turn on Auto-Revert Mode.

This function is designed to be added to hooks, for example:
  (add-hook 'c-mode-hook 'turn-on-auto-revert-mode)" nil nil)

(autoload 'global-auto-revert-mode "autorevert" "\
Revert any buffer when file on disk change.

With arg, turn Auto Revert mode on globally if and only if arg is positive.
This is a minor mode that affects all buffers.
Use `auto-revert-mode' to revert a particular buffer." t nil)

;;;***

;;;### (autoloads (mouse-avoidance-mode) "avoid" "edit-utils/avoid.el")

(defcustom mouse-avoidance-mode nil "Value is t or a symbol if the mouse pointer should avoid the cursor.\nSee function `mouse-avoidance-mode' for possible values. Normally, you\nshouldn't modify this variable by hand, but use the function instead. However,\nthe default value can be customized from the options menu." :type '(radio (const :tag "No mouse avoidance" nil) (const :tag "Move the mouse on keypress" banish) (const :tag "Move the mouse if the cursor gets too close" exile) (const :tag "Displace the mouse if the cursor gets too close" jump) (const :tag "Animate the mouse" animate) (const :tag "Animate + change shape" proteus)) :set (lambda (symbol value) (mouse-avoidance-mode (or value 'none))) :initialize 'custom-initialize-default :require 'avoid :group 'avoid)

(defcustom mouse-avoidance-mode-line-string " Avoid" "*String to display in the modeline when `mouse-avoidance-mode' is active.\nSet this to nil if you don't want a modeline indicator." :type '(choice string (const :tag "none" nil)) :group 'avoid)

(autoload 'mouse-avoidance-mode "avoid" "\
Set cursor avoidance mode to MODE.
MODE should be one of the symbols `banish', `exile', `jump', `animate',
`cat-and-mouse', `proteus', or `none'.

If MODE is nil, toggle mouse avoidance between `none` and `banish'
modes.  Positive numbers and symbols other than the above are treated
as equivalent to `banish'; negative numbers and `-' are equivalent to `none'.

Effects of the different modes: 
 * banish: Move the mouse to the upper-right corner on any keypress.
 * exile: Move the mouse to the corner only if the cursor gets too close,
     and allow it to return once the cursor is out of the way.
 * jump: If the cursor gets too close to the mouse, displace the mouse
     a random distance & direction.
 * animate: As `jump', but shows steps along the way for illusion of motion.
 * cat-and-mouse: Same as `animate'.
 * proteus: As `animate', but changes the shape of the mouse pointer too.

Whenever the mouse is moved, the frame is also raised.

\(see `mouse-avoidance-threshold' for definition of \"too close\",
and `mouse-avoidance-nudge-dist' and `mouse-avoidance-nudge-var' for
definition of \"random distance\".)" t nil)

(add-minor-mode 'mouse-avoidance-mode 'mouse-avoidance-mode-line-string)

;;;***

;;;### (autoloads (balloon-help-minor-mode balloon-help-mode) "balloon-help" "edit-utils/balloon-help.el")

(defcustom balloon-help-mode nil "*Non-nil means Balloon help mode is enabled." :type 'boolean :set (lambda (symbol value) (balloon-help-mode (or value 0))) :initialize 'custom-initialize-default :require 'balloon-help :group 'balloon-help)

(autoload 'balloon-help-mode "balloon-help" "\
Toggle Balloon Help mode.
With arg, turn Balloon Help mode on iff arg is positive.

With Balloon Help enabled, a small frame is displayed whenever
the mouse rests on an object that has a help property of some
kind.  The text of that help property is displayed in the frame.

If you want Balloon Help mode enabled in some buffers only, use
`balloon-help-minor-mode' instead.

For extents, the 'balloon-help' property is checked.

For toolbar buttons, the help-string slot of the toolbar button
is checked.

If the value is a string, it is used as the help message.

If the property's value is a symbol, it is assumed to be the name
of a function and it will be called with one argument, the object
under the mouse, and the return value of that function will be
used as the help message." t nil)

(autoload 'balloon-help-minor-mode "balloon-help" "\
Toggle Balloon Help minor mode.
With arg, turn Balloon Help minor mode on iff arg is positive.

This minor mode is useful if you want `balloon-help-mode' globally disabled,
except in some buffers.

Please refer to the function `balloon-help-mode' for more details" t nil)

;;;***

;;;### (autoloads (blink-cursor-mode) "blink-cursor" "edit-utils/blink-cursor.el")

(defcustom blink-cursor-mode nil "Non nil means `blink-cursor-mode' is on. Normally, you shouldn't modify\nthis variable by hand, but use the function `blink-cursor-mode'\ninstead. However, the default value can be customized from the options menu." :type 'boolean :set (lambda (var val) (blink-cursor-mode (or val 0))) :initialize 'custom-initialize-default :require 'blink-cursor :group 'blink-cursor)

(autoload 'blink-cursor-mode "blink-cursor" "\
Enable or disable a blinking cursor.
If TIMEOUT is nil, toggle on or off.
If TIMEOUT is t, enable with the previous timeout value.
If TIMEOUT is 0, disable.
If TIMEOUT is greater than 0, then the cursor will blink once
each TIMEOUT secs (can be a float)." t nil)

;;;***

;;;### (autoloads (bookmark-menu-delete bookmark-menu-rename bookmark-menu-locate bookmark-menu-jump bookmark-menu-insert bookmark-bmenu-list bookmark-load bookmark-save bookmark-write bookmark-delete bookmark-insert bookmark-rename bookmark-insert-location bookmark-relocate bookmark-jump bookmark-set bookmark-all-names) "bookmark" "edit-utils/bookmark.el")

(if (symbolp (key-binding "r")) nil (progn (define-key ctl-x-map "rb" 'bookmark-jump) (define-key ctl-x-map "rm" 'bookmark-set) (define-key ctl-x-map "rl" 'bookmark-bmenu-list)))

(defvar bookmark-map nil "\
Keymap containing bindings to bookmark functions.
It is not bound to any key by default: to bind it
so that you have a bookmark prefix, just use `global-set-key' and bind a
key of your choice to `bookmark-map'.  All interactive bookmark
functions have a binding in this keymap.")

(define-prefix-command 'bookmark-map)

(define-key bookmark-map "x" 'bookmark-set)

(define-key bookmark-map "m" 'bookmark-set)

(define-key bookmark-map "j" 'bookmark-jump)

(define-key bookmark-map "g" 'bookmark-jump)

(define-key bookmark-map "i" 'bookmark-insert)

(define-key bookmark-map "e" 'edit-bookmarks)

(define-key bookmark-map "f" 'bookmark-insert-location)

(define-key bookmark-map "r" 'bookmark-rename)

(define-key bookmark-map "d" 'bookmark-delete)

(define-key bookmark-map "l" 'bookmark-load)

(define-key bookmark-map "w" 'bookmark-write)

(define-key bookmark-map "s" 'bookmark-save)

(add-hook 'kill-emacs-hook (function (lambda nil (and (featurep 'bookmark) bookmark-alist (bookmark-time-to-save-p t) (bookmark-save)))))

(autoload 'bookmark-all-names "bookmark" "\
Return a list of all current bookmark names." nil nil)

(autoload 'bookmark-set "bookmark" "\
Set a bookmark named NAME inside a file.
If name is nil, then the user will be prompted.
With prefix arg, will not overwrite a bookmark that has the same name
as NAME if such a bookmark already exists, but instead will \"push\"
the new bookmark onto the bookmark alist.  Thus the most recently set
bookmark with name NAME would be the one in effect at any given time,
but the others are still there, should you decide to delete the most
recent one.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-u inserts the name of the last bookmark used in the buffer
\(as an aid in using a single bookmark name to track your progress
through a large file).  If no bookmark was used, then C-u inserts the
name of the file being visited.

Use \\[bookmark-delete] to remove bookmarks (you give it a name,
and it removes only the first instance of a bookmark with that name from
the list of bookmarks.)" t nil)

(autoload 'bookmark-jump "bookmark" "\
Jump to bookmark BOOKMARK (a point in some file).  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

If the file pointed to by BOOKMARK no longer exists, you will be asked
if you wish to give the bookmark a new location, and bookmark-jump
will then jump to the new location, as well as recording it in place
of the old one in the permanent bookmark record." t nil)

(autoload 'bookmark-relocate "bookmark" "\
Relocate BOOKMARK to another file (reading file name with minibuffer).
This makes an already existing bookmark point to that file, instead of
the one it used to point at.  Useful when a file has been renamed
after a bookmark was set in it." t nil)

(autoload 'bookmark-insert-location "bookmark" "\
Insert the name of the file associated with BOOKMARK.
Optional second arg NO-HISTORY means don't record this in the
minibuffer history list `bookmark-history'." t nil)

(defalias 'bookmark-locate 'bookmark-insert-location)

(autoload 'bookmark-rename "bookmark" "\
Change the name of OLD bookmark to NEW name.
If called from keyboard, prompt for OLD and NEW.  If called from
menubar, select OLD from a menu and prompt for NEW.

If called from Lisp, prompt for NEW if only OLD was passed as an
argument.  If called with two strings, then no prompting is done.  You
must pass at least OLD when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consecutive words from the text of the buffer into the new bookmark
name." t nil)

(autoload 'bookmark-insert "bookmark" "\
Insert the text of the file pointed to by bookmark BOOKMARK.  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this." t nil)

(autoload 'bookmark-delete "bookmark" "\
Delete BOOKMARK from the bookmark list.  
Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark (that is, the
one most recently used in this file, if any).
Optional second arg BATCH means don't update the bookmark list buffer,
probably because we were called from there." t nil)

(autoload 'bookmark-write "bookmark" "\
Write bookmarks to a file (reading the file name with the minibuffer).
Don't use this in Lisp programs; use `bookmark-save' instead." t nil)

(autoload 'bookmark-save "bookmark" "\
Save currently defined bookmarks.
Saves by default in the file defined by the variable
`bookmark-default-file'.  With a prefix arg, save it in file FILE
\(second argument).

If you are calling this from Lisp, the two arguments are PREFIX-ARG
and FILE, and if you just want it to write to the default file, then
pass no arguments.  Or pass in nil and FILE, and it will save in FILE
instead.  If you pass in one argument, and it is non-nil, then the
user will be interactively queried for a file to save in.

When you want to load in the bookmarks from a file, use
`bookmark-load', \\[bookmark-load].  That function will prompt you
for a file, defaulting to the file defined by variable
`bookmark-default-file'." t nil)

(autoload 'bookmark-load "bookmark" "\
Load bookmarks from FILE (which must be in bookmark format).
Appends loaded bookmarks to the front of the list of bookmarks.  If
optional second argument REVERT is non-nil, existing bookmarks are
destroyed.  Optional third arg NO-MSG means don't display any messages
while loading.

If you load a file that doesn't contain a proper bookmark alist, you
will corrupt Emacs's bookmark list.  Generally, you should only load
in files that were created with the bookmark functions in the first
place.  Your own personal bookmark file, `~/.emacs.bmk', is
maintained automatically by Emacs; you shouldn't need to load it
explicitly." t nil)

(autoload 'bookmark-bmenu-list "bookmark" "\
Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying." t nil)

(defalias 'list-bookmarks 'bookmark-bmenu-list)

(defalias 'edit-bookmarks 'bookmark-bmenu-list)

(autoload 'bookmark-menu-insert "bookmark" "\
Insert the text of the file pointed to by bookmark BOOKMARK.  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload 'bookmark-menu-jump "bookmark" "\
Jump to bookmark BOOKMARK (a point in some file).  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload 'bookmark-menu-locate "bookmark" "\
Insert the name of the file associated with BOOKMARK. 
\(This is not the same as the contents of that file).

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload 'bookmark-menu-rename "bookmark" "\
Change the name of OLD-BOOKMARK to NEWNAME.  
If called from keyboard, prompts for OLD-BOOKMARK and NEWNAME.
If called from menubar, OLD-BOOKMARK is selected from a menu, and
prompts for NEWNAME. 
If called from Lisp, prompts for NEWNAME if only OLD-BOOKMARK was
passed as an argument.  If called with two strings, then no prompting
is done.  You must pass at least OLD-BOOKMARK when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consecutive words from the text of the buffer into the new bookmark
name.

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(autoload 'bookmark-menu-delete "bookmark" "\
Delete the bookmark named NAME from the bookmark list.  
Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark (that is, the
one most recently used in this file, if any).

Warning: this function only takes an EVENT as argument.  Use the
corresponding bookmark function from Lisp (the one without the
\"-menu-\" in its name)." t nil)

(defvar menu-bar-bookmark-map (make-sparse-keymap "Bookmark functions"))

(defalias 'menu-bar-bookmark-map (symbol-value 'menu-bar-bookmark-map))

(define-key menu-bar-bookmark-map [load] '("Load a Bookmark File..." . bookmark-load))

(define-key menu-bar-bookmark-map [write] '("Save Bookmarks As..." . bookmark-write))

(define-key menu-bar-bookmark-map [save] '("Save Bookmarks" . bookmark-save))

(define-key menu-bar-bookmark-map [edit] '("Edit Bookmark List" . bookmark-bmenu-list))

(define-key menu-bar-bookmark-map [delete] '("Delete Bookmark" . bookmark-menu-delete))

(define-key menu-bar-bookmark-map [rename] '("Rename Bookmark" . bookmark-menu-rename))

(define-key menu-bar-bookmark-map [locate] '("Insert Location" . bookmark-menu-locate))

(define-key menu-bar-bookmark-map [insert] '("Insert Contents" . bookmark-menu-insert))

(define-key menu-bar-bookmark-map [set] '("Set Bookmark" . bookmark-set))

(define-key menu-bar-bookmark-map [jump] '("Jump to Bookmark" . bookmark-menu-jump))

;;;***

;;;### (autoloads (compare-windows) "compare-w" "edit-utils/compare-w.el")

(autoload 'compare-windows "compare-w" "\
Compare text in current window with text in next window.
Compares the text starting at point in each window,
moving over text in each one as far as they match.

This command pushes the mark in each window
at the prior location of point in that window.
If both windows display the same buffer,
the mark is pushed twice in that buffer:
first in the other window, then in the selected window.

A prefix arg means ignore changes in whitespace.
The variable `compare-windows-whitespace' controls how whitespace is skipped.
If `compare-ignore-case' is non-nil, changes in case are also ignored." t nil)

;;;***

;;;### (autoloads (complete) "completion" "edit-utils/completion.el")

(autoload 'complete "completion" "\
Fill out a completion of the word before point.  
Point is left at end.  Consecutive calls rotate through all possibilities.
Prefix args ::
  control-u :: leave the point at the beginning of the completion rather 
               than at the end.
  a number  :: rotate through the possible completions by that amount
  `-'       :: same as -1 (insert previous completion)
 {See the comments at the top of `completion.el' for more info.}" t nil)

;;;***

;;;### (autoloads (completing-read-multiple) "crm" "edit-utils/crm.el")

(autoload 'completing-read-multiple "crm" "\
Read multiple strings in the minibuffer, with completion.
By using this functionality, a user may specify multiple strings at a
single prompt, optionally using completion.

Multiple strings are specified by separating each of the strings with
a prespecified separator character.  For example, if the separator
character is a comma, the strings 'alice', 'bob', and 'eve' would be
specified as 'alice,bob,eve'.

The default value for the separator character is the value of
`crm-default-separator' (comma).  The separator character may be
changed by modifying the value of `crm-separator'.

Contiguous strings of non-separator-characters are referred to as
'elements'.  In the aforementioned example, the elements are: 'alice',
'bob', and 'eve'.

Completion is available on a per-element basis.  For example, if the
contents of the minibuffer are 'alice,bob,eve' and point is between
'l' and 'i', pressing TAB operates on the element 'alice'.

The return value of this function is a list of the read strings.

See the documentation for `completing-read' for details on the arguments:
PROMPT, TABLE, PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF, and
INHERIT-INPUT-METHOD." nil nil)

;;;***

;;;### (autoloads (dabbrev-expand dabbrev-completion) "dabbrev" "edit-utils/dabbrev.el")

(define-key global-map [(meta /)] 'dabbrev-expand)

(define-key global-map [(meta control /)] 'dabbrev-completion)

(autoload 'dabbrev-completion "dabbrev" "\
Completion on current word.
Like \\[dabbrev-expand] but finds all expansions in the current buffer
and presents suggestions for completion.

With a prefix argument, it searches all buffers accepted by the
function pointed out by `dabbrev-friend-buffer-function' to find the
completions.

If the prefix argument is 16 (which comes from C-u C-u),
then it searches *all* buffers.

With no prefix argument, it reuses an old completion list
if there is a suitable one already." t nil)

(autoload 'dabbrev-expand "dabbrev" "\
Expand previous word \"dynamically\".

Expands to the most recent, preceding word for which this is a prefix.
If no suitable preceding word is found, words following point are
considered.  If still no suitable word is found, then look in the
buffers accepted by the function pointed out by variable
`dabbrev-friend-buffer-function'.

A positive prefix argument, N, says to take the Nth backward *distinct*
possibility.  A negative argument says search forward.

If the cursor has not moved from the end of the previous expansion and
no argument is given, replace the previously-made expansion
with the next possible expansion not yet tried.

The variable `dabbrev-backward-only' may be used to limit the
direction of search to backward if set non-nil.

See also `dabbrev-abbrev-char-regexp' and \\[dabbrev-completion]." t nil)

;;;***

;;;### (autoloads (desktop-load-default desktop-read) "desktop" "edit-utils/desktop.el")

(autoload 'desktop-read "desktop" "\
Read the Desktop file and the files it specifies.
This is a no-op when Emacs is running in batch mode." t nil)

(autoload 'desktop-load-default "desktop" "\
Load the `default' start-up library manually.
Also inhibit further loading of it.  Call this from your `.emacs' file
to provide correct modes for autoloaded files." nil nil)

;;;***

;;;### (autoloads (edit-faces) "edit-faces" "edit-utils/edit-faces.el")

(autoload 'edit-faces "edit-faces" "\
Alter face characteristics by editing a list of defined faces.
Pops up a buffer containing a list of defined faces.

WARNING: the changes you may perform with this function are no longer
saved. The prefered way to modify faces is now to use `customize-face'. If you 
want to specify particular X font names for faces, please do so in your
.XDefaults file.

Editing commands:

\\{edit-faces-mode-map}" t nil)

;;;***

;;;### (autoloads (edit-toolbar) "edit-toolbar" "edit-utils/edit-toolbar.el")

(autoload 'edit-toolbar "edit-toolbar" "\
Alter toolbar characteristics by editing a buffer representing the specified toolbar.
Pops up a buffer containing a list of the toolbar matching TOOLBAR_NAME." t nil)

;;;***

;;;### (autoloads (turn-on-fast-lock fast-lock-mode) "fast-lock" "edit-utils/fast-lock.el")

(defcustom fast-lock-mode nil "Non nil means `fast-lock-mode' is on" :group 'fast-lock :require 'fast-lock :type 'boolean :initialize 'custom-initialize-default :set '(lambda (var val) (if val (progn (fast-lock-mode 1) (add-hook 'font-lock-mode-hook 'turn-on-fast-lock)) (fast-lock-mode -1) (remove-hook 'font-lock-mode-hook 'turn-on-fast-lock)) (setq-default fast-lock-mode val)))

(autoload 'fast-lock-mode "fast-lock" "\
Toggle Fast Lock mode.
With arg, turn Fast Lock mode on if and only if arg is positive and the buffer
is associated with a file.  Enable it automatically in your `~/.emacs' by:

 (setq font-lock-support-mode 'fast-lock-mode)

If Fast Lock mode is enabled, and the current buffer does not contain any text
properties, any associated Font Lock cache is used if its timestamp matches the
buffer's file, and its `font-lock-keywords' match those that you are using.

Font Lock caches may be saved:
- When you save the file's buffer.
- When you kill an unmodified file's buffer.
- When you exit Emacs, for all unmodified or saved buffers.
Depending on the value of `fast-lock-save-events'.
See also the commands `fast-lock-read-cache' and `fast-lock-save-cache'.

Use \\[font-lock-fontify-buffer] to fontify the buffer if the cache is bad.

Various methods of control are provided for the Font Lock cache.  In general,
see variable `fast-lock-cache-directories' and function `fast-lock-cache-name'.
For saving, see variables `fast-lock-minimum-size', `fast-lock-save-events',
`fast-lock-save-others' and `fast-lock-save-faces'." t nil)

(autoload 'turn-on-fast-lock "fast-lock" "\
Unconditionally turn on Fast Lock mode." nil nil)

(when (fboundp 'add-minor-mode) (defvar fast-lock-mode nil) (add-minor-mode 'fast-lock-mode nil))

;;;***

;;;### (autoloads (make-file-part) "file-part" "edit-utils/file-part.el")

(autoload 'make-file-part "file-part" "\
Make a file part on buffer BUFFER out of the region.  Call it NAME.
This command creates a new buffer containing the contents of the
region and marks the buffer as referring to the specified buffer,
called the `master buffer'.  When the file-part buffer is saved,
its changes are integrated back into the master buffer.  When the
master buffer is deleted, all file parts are deleted with it.

When called from a function, expects four arguments, START, END,
NAME, and BUFFER, all of which are optional and default to the
beginning of BUFFER, the end of BUFFER, a name generated from
BUFFER's name, and the current buffer, respectively." t nil)

;;;***

;;;### (autoloads (floating-toolbar-from-extent-or-popup-mode-menu floating-toolbar-or-popup-mode-menu floating-toolbar) "floating-toolbar" "edit-utils/floating-toolbar.el")

(autoload 'floating-toolbar "floating-toolbar" "\
Popup a toolbar near the current mouse position.
The toolbar instantiator used is taken from the 'floating-toolbar
property of any extent under the mouse.  If no such non-nil
property exists for any extent under the mouse, then the value of the
variable `floating-toolbar' is checked.  If its value is nil, then
no toolbar will be displayed.

This command should be bound to a button press event.

When called from a program, first arg EVENT should be the button
press event.  Optional second arg EXTENT-LOCAL-ONLY specifies
that only extent local toolbars should be used; this means the
`floating-toolbar' variable will not be consulted." t nil)

(autoload 'floating-toolbar-or-popup-mode-menu "floating-toolbar" "\
Like floating-toolbar, but if no toolbar is displayed
run popup-mode-menu." t nil)

(autoload 'floating-toolbar-from-extent-or-popup-mode-menu "floating-toolbar" "\
Like floating-toolbar-or-popup-mode-menu, but search only for an
extent local toolbar." t nil)

;;;***

;;;### (autoloads (enable-flow-control-on enable-flow-control) "flow-ctrl" "edit-utils/flow-ctrl.el")

(autoload 'enable-flow-control "flow-ctrl" "\
Toggle flow control handling.
When handling is enabled, user can type C-s as C-\\, and C-q as C-^.
With arg, enable flow control mode if arg is positive, otherwise disable." t nil)

(autoload 'enable-flow-control-on "flow-ctrl" "\
Enable flow control if using one of a specified set of terminal types.
Use `(enable-flow-control-on \"vt100\" \"h19\")' to enable flow control
on VT-100 and H19 terminals.  When flow control is enabled,
you must type C-\\ to get the effect of a C-s, and type C-^
to get the effect of a C-q.

This function has no effect unless the current device is a tty.

The tty terminal type is determined from the TERM environment variable.
Trailing hyphens and everything following is stripped, so a TERM
value of \"vt100-nam\" is treated the same as \"vt100\"." nil nil)

;;;***

;;;### (autoloads (fume-setup-buffer turn-on-fume-mode fume-mode function-menu) "func-menu" "edit-utils/func-menu.el")

(autoload 'function-menu "func-menu" "\
Pop up a menu of buffer local functions and related commands.
Each function menu entry jumps to its named function.  Prior to a jump, a
mark is set so that {C-u \\[set-mark-command]} will move point back to the
old location.

With optional prefix argument USE-MENUBAR, add the menu to the current
menubar.  With optional second argument RETURN-ONLY, return the menu
of functions but do not display it.

With optional third argument MENU-ITEM-FUNCTION, use this as the function
called by each menu item (default = 'fume-goto-function).  This function
must take two arguments, function menu item name (a string) and the position
\(an integer) within the buffer to leave point when displaying this menu item." t nil)

(autoload 'fume-mode "func-menu" "\
Toggle the func menu minor mode on/off.
With a prefix numeric value or ARG > 0, turn it on.
The variable `fume-mode-line-string' defines what you will see in the
modeline when the mode is on.

Note: if you had been using

\\{fume-mode-map}" t nil)

(autoload 'turn-on-fume-mode "func-menu" "\
Unconditionaly turn the func menu minor mode on." t nil)

(autoload 'fume-setup-buffer "func-menu" "\
Setup the current buffer for func-menu.
This actually means prepare for turning `fume-mode' on.

If you've been using func-menu the old way (before `fume-mode' was
written), please note that `mouse-function-menu' is bound to M-Button3
\(not Shift-Button3) in this mode.  Nothing prevents you from keeping your
old binding though.

This function is meant to be added to `find-file-hooks'.
If FORCE is non-nil, actually do the operation now; otherwise,
delay until the next time the event loop is processed (this is
so that Lisp routines that temporarily load a file, process it,
and then kill it will not be slowed down by `function-menu' processing)." t nil)

;;;***

;;;### (autoloads (highline-view-off highline-view-on highline-view-mode highline-off highline-on highline-local-mode highline-mode-off highline-mode-on highline-mode highline-customize) "highline" "edit-utils/highline.el")

(autoload 'highline-customize "highline" "\
Customize highline group." t nil)

(autoload 'highline-mode "highline" "\
Toggle global minor mode to highlight line about point (HL on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system." t nil)

(autoload 'highline-mode-on "highline" "\
Turn on global minor mode to highlight line about point (HL on modeline)." t nil)

(autoload 'highline-mode-off "highline" "\
Turn off global minor mode to highlight line about point (HL on modeline)." t nil)

(autoload 'highline-local-mode "highline" "\
Toggle local minor mode to highlight the line about point (hl on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system." t nil)

(autoload 'highline-on "highline" "\
Turn on local highlighting of the current line in buffer (hl on modeline)." t nil)

(autoload 'highline-off "highline" "\
Turn off local highlighting of the current line in buffer (hl on modeline)." t nil)

(autoload 'highline-view-mode "highline" "\
Toggle indirect mode to highlight current line in buffer (Ihl on modeline).

With ARG, turn highline mode on if ARG is positive, off otherwise.
Only useful with a windowing system.

Indirect highline (`highline-view-on', `highline-view-off' and
`highline-view-mode') is useful when you wish to have various \"visions\" of
the same buffer.

Indirect highline uses an indirect buffer to get the \"vision\" of the buffer.
So, if you kill an indirect buffer, the base buffer is not affected; if you
kill the base buffer, all indirect buffer related with the base buffer is
automagicaly killed.  Also, any text insertion/deletion in any indirect or base
buffer is updated in all related buffers.

See also `highline-selected-window'." t nil)

(autoload 'highline-view-on "highline" "\
Turn on indirect highlightining current line in buffer (Ihl on modeline).

Indirect highline (`highline-view-on', `highline-view-off' and
`highline-view-mode') is useful when you wish to have various \"visions\" of
the same buffer.

Indirect highline uses an indirect buffer to get the \"vision\" of the buffer.
So, if you kill an indirect buffer, the base buffer is not affected; if you
kill the base buffer, all indirect buffer related with the base buffer is
automagicaly killed.  Also, any text insertion/deletion in any indirect or base
buffer is updated in all related buffers.

See also `highline-selected-window'." t nil)

(autoload 'highline-view-off "highline" "\
Turn off indirect highlightining current line in buffer (Ihl on modeline).

Indirect highline (`highline-view-on', `highline-view-off' and
`highline-view-mode') is useful when you wish to have various \"visions\" of
the same buffer.

Indirect highline uses an indirect buffer to get the \"vision\" of the buffer.
So, if you kill an indirect buffer, the base buffer is not affected; if you
kill the base buffer, all indirect buffer related with the base buffer is
automagicaly killed.  Also, any text insertion/deletion in any indirect or base
buffer is updated in all related buffers.

See also `highline-selected-window'." t nil)

;;;***

;;;### (autoloads (make-hippie-expand-function hippie-expand) "hippie-exp" "edit-utils/hippie-exp.el")

(defvar hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol) "\
The list of expansion functions tried in order by `hippie-expand'.
To change the behavior of `hippie-expand', remove, change the order of,
or insert functions in this list.")

(defvar hippie-expand-verbose t "\
*Non-nil makes `hippie-expand' output which function it is trying.")

(defvar hippie-expand-max-buffers nil "\
*The maximum number of buffers (apart from the current) searched.
If nil, all buffers are searched.")

(defvar hippie-expand-ignore-buffers '("^ \\*.*\\*$" dired-mode) "\
*A list specifying which buffers not to search (if not current).
Can contain both regexps matching buffer names (as strings) and major modes
\(as atoms)")

(autoload 'hippie-expand "hippie-exp" "\
Try to expand text before point, using multiple methods.
The expansion functions in `hippie-expand-try-functions-list' are
tried in order, until a possible expansion is found.  Repeated
application of `hippie-expand' inserts successively possible
expansions.  
With a positive numeric argument, jumps directly to the ARG next
function in this list.  With a negative argument or just \\[universal-argument], 
undoes the expansion." t nil)

(autoload 'make-hippie-expand-function "hippie-exp" "\
Construct a function similar to `hippie-expand'.
Make it use the expansion functions in TRY-LIST.  An optional second
argument VERBOSE non-nil makes the function verbose." nil 'macro)

;;;***

;;;### (autoloads (icomplete-minibuffer-setup icomplete-mode) "icomplete" "edit-utils/icomplete.el")

(autoload 'icomplete-mode "icomplete" "\
Activate incremental minibuffer completion for this emacs session.
Deactivates with negative universal argument." t nil)

(autoload 'icomplete-minibuffer-setup "icomplete" "\
Run in minibuffer on activation to establish incremental completion.
Usually run by inclusion in `minibuffer-setup-hook'." nil nil)

;;;***

;;;### (autoloads (id-select-double-click-hook id-select-and-kill-thing id-select-and-copy-thing id-select-goto-matching-tag id-select-thing-with-mouse id-select-thing id-select-install) "id-select" "edit-utils/id-select.el")

(autoload 'id-select-install "id-select" "\
Install the id-select mode as the default mode of operation." t nil)

(autoload 'id-select-thing "id-select" "\
Mark the region selected by the syntax of the thing at point.
If invoked repeatedly, selects bigger and bigger things.
If `id-select-display-type' is non-nil, the type of selection is displayed in
the minibuffer." t nil)

(autoload 'id-select-thing-with-mouse "id-select" "\
Select a region based on the syntax of the character from a mouse click.
If the click occurs at the same point as the last click, select
the next larger syntactic structure.  If `id-select-display-type' is non-nil,
the type of selection is displayed in the minibuffer." t nil)

(autoload 'id-select-goto-matching-tag "id-select" "\
If in a major mode listed in `id-select-markup-modes,' moves point to the start of the tag paired with the closest tag that point is within or precedes.
Returns t if point is moved, else nil.
Signals an error if no tag is found following point or if the closing tag
does not have a `>' terminator character." t nil)

(autoload 'id-select-and-copy-thing "id-select" "\
Copy the region surrounding the syntactical unit at point." t nil)

(autoload 'id-select-and-kill-thing "id-select" "\
Kill the region surrounding the syntactical unit at point." t nil)

(autoload 'id-select-double-click-hook "id-select" "\
Select a region based on the syntax of the character wherever the mouse is double-clicked.
If the double-click occurs at the same point as the last double-click, select
the next larger syntactic structure.  If `id-select-display-type' is non-nil,
the type of selection is displayed in the minibuffer." nil nil)

;;;***

;;;### (autoloads (info-complete-file info-complete-symbol info-lookup-file info-lookup-symbol info-lookup-reset) "info-look" "edit-utils/info-look.el")

(autoload 'info-lookup-reset "info-look" "\
Throw away all cached data.
This command is useful if the user wants to start at the beginning without
quitting Emacs, for example, after some Info documents were updated on the
system." t nil)

(autoload 'info-lookup-symbol "info-look" "\
Display the definition of SYMBOL, as found in the relevant manual.
When this command is called interactively, it reads SYMBOL from the minibuffer.
In the minibuffer, use M-n to yank the default argument value
into the minibuffer so you can edit it.
The default symbol is the one found at point.

With prefix arg a query for the symbol help mode is offered." t nil)

(autoload 'info-lookup-file "info-look" "\
Display the documentation of a file.
When this command is called interactively, it reads FILE from the minibuffer.
In the minibuffer, use M-n to yank the default file name
into the minibuffer so you can edit it.
The default file name is the one found at point.

With prefix arg a query for the file help mode is offered." t nil)

(autoload 'info-complete-symbol "info-look" "\
Perform completion on symbol preceding point." t nil)

(autoload 'info-complete-file "info-look" "\
Perform completion on file preceding point." t nil)

;;;***

;;;### (autoloads (iswitchb-buffer-other-frame iswitchb-display-buffer iswitchb-buffer-other-window iswitchb-buffer iswitchb-default-keybindings) "iswitchb" "edit-utils/iswitchb.el")

(autoload 'iswitchb-default-keybindings "iswitchb" "\
Set up default keybindings for `iswitchb-buffer'.
Call this function to override the normal bindings." t nil)

(autoload 'iswitchb-buffer "iswitchb" "\
Switch to another buffer.

The buffer name is selected interactively by typing a substring.  The
buffer is displayed according to `iswitchb-default-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.
For details of keybindings, do `\\[describe-function] iswitchb'." t nil)

(autoload 'iswitchb-buffer-other-window "iswitchb" "\
Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'." t nil)

(autoload 'iswitchb-display-buffer "iswitchb" "\
Display a buffer in another window but don't select it.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'." t nil)

(autoload 'iswitchb-buffer-other-frame "iswitchb" "\
Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `\\[describe-function] iswitchb'." t nil)

;;;***

;;;### (autoloads (turn-on-lazy-lock lazy-lock-mode) "lazy-lock" "edit-utils/lazy-lock.el")

(defcustom lazy-lock-mode nil "Non nil means `lazy-lock-mode' is on." :group 'lazy-lock :require 'lazy-lock :type 'boolean :initialize 'custom-initialize-default :set '(lambda (var val) (if val (progn (lazy-lock-mode 1) (add-hook 'font-lock-mode-hook 'turn-on-lazy-lock)) (lazy-lock-mode -1) (remove-hook 'font-lock-mode-hook 'turn-on-lazy-lock))))

(autoload 'lazy-lock-mode "lazy-lock" "\
Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if arg is positive.  Enable it
automatically in your `~/.emacs' by:

 (setq font-lock-support-mode 'lazy-lock-mode)

When Lazy Lock mode is enabled, fontification can be lazy in a number of ways:

- Demand-driven buffer fontification if `lazy-lock-minimum-size' is non-nil.
  This means initial fontification does not occur if the buffer is greater than
  `lazy-lock-minimum-size' characters in length.  Instead, fontification occurs
  when necessary, such as when scrolling through the buffer would otherwise
  reveal unfontified areas.  This is useful if buffer fontification is too slow
  for large buffers.

- Deferred on-the-fly fontification if `lazy-lock-defer-on-the-fly' is non-nil.
  This means on-the-fly fontification does not occur as you type.  Instead,
  fontification is deferred until after `lazy-lock-defer-time' seconds of Emacs
  idle time, while Emacs remains idle.  This is useful if fontification is too
  slow to keep up with your typing.

- Deferred context fontification if `lazy-lock-defer-contextually' is non-nil.
  This means fontification updates the buffer corresponding to true syntactic
  context, after `lazy-lock-defer-time' seconds of Emacs idle time, while Emacs
  remains idle.  Otherwise, fontification occurs on modified lines only, and
  subsequent lines can remain fontified corresponding to previous syntactic
  contexts.  This is useful where strings or comments span lines.

- Stealthy buffer fontification if `lazy-lock-stealth-time' is non-nil.
  This means remaining unfontified areas of buffers are fontified if Emacs has
  been idle for `lazy-lock-stealth-time' seconds, while Emacs remains idle.
  This is useful if any buffer has any deferred fontification.

Basic Font Lock mode on-the-fly fontification behaviour fontifies modified
lines only.  Thus, if `lazy-lock-defer-contextually' is non-nil, Lazy Lock mode
on-the-fly fontification may fontify differently, albeit correctly.  In any
event, to refontify some lines you can use \\[font-lock-fontify-block].

Stealth fontification only occurs while the system remains unloaded.
If the system load rises above `lazy-lock-stealth-load' percent, stealth
fontification is suspended.  Stealth fontification intensity is controlled via
the variable `lazy-lock-stealth-nice' and `lazy-lock-stealth-lines', and
verbosity is controlled via the variable `lazy-lock-stealth-verbose'." t nil)

(autoload 'turn-on-lazy-lock "lazy-lock" "\
Unconditionally turn on Lazy Lock mode." nil nil)

(add-minor-mode 'lazy-lock-mode 'lazy-lock-mode-line-string)

;;;***

;;;### (autoloads (turn-on-lazy-shot lazy-shot-mode) "lazy-shot" "edit-utils/lazy-shot.el")

(defcustom lazy-shot-mode nil "Non nil means `lazy-shot-mode' is on." :group 'lazy-shot :require 'lazy-shot :type 'boolean :initialize 'custom-initialize-default :set '(lambda (var val) (if val (progn (lazy-shot-mode 1) (add-hook 'font-lock-mode-hook 'turn-on-lazy-shot)) (lazy-shot-mode -1) (remove-hook 'font-lock-mode-hook 'turn-on-lazy-shot)) (setq-default lazy-shot-mode val)))

(autoload 'lazy-shot-mode "lazy-shot" "\
Toggle Lazy Lock mode.
With arg, turn Lazy Lock mode on if and only if ARG is positive." t nil)

(autoload 'turn-on-lazy-shot "lazy-shot" "\
Unconditionally turn on Lazy Lock mode." nil nil)

;;;***

;;;### (autoloads (make-command-summary) "makesum" "edit-utils/makesum.el")

(autoload 'make-command-summary "makesum" "\
Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first." t nil)

;;;***

;;;### (autoloads (Manual-nuke-nroff-bs manual-entry) "man" "edit-utils/man.el")

(autoload 'manual-entry "man" "\
Display the Unix manual entry (or entries) for TOPIC.
If TOPIC starts with -k, then a system apropos search is performed
using man -k for TOPIC." t nil)

(define-key help-map "" 'manual-entry)

(autoload 'Manual-nuke-nroff-bs "man" nil t nil)

(defalias 'nuke-nroff-bs 'Manual-nuke-nroff-bs)

;;;***

;;;### (autoloads (paren-backward-sexp paren-forward-sexp paren-toggle-open-paren-context paren-toggle-matching-quoted-paren paren-toggle-matching-paired-delimiter paren-deactivate paren-activate) "mic-paren" "edit-utils/mic-paren.el")

(autoload 'paren-activate "mic-paren" "\
Activates mic-paren parenthesis highlighting.
paren-activate deactivates the paren.el and stig-paren.el packages if they are
active !
The following options are available via the customize-feature:
  `paren-priority'
  `paren-overlay-priority'
  `paren-sexp-mode'
  `paren-highlight-at-point'
  `paren-highlight-offscreen'
  `paren-display-message'
  `paren-message-linefeed-display'
  `paren-message-no-match'
  `paren-message-show-linenumber'
  `paren-message-truncate-lines'
  `paren-ding-unmatched'
  `paren-delay'
  `paren-dont-touch-blink'
  `paren-match-face'
  `paren-mismatch-face'
  `paren-no-match-face'
  `paren-bind-modified-sexp-functions'
The following options are settable via toggling functions (look at the
documentation of these options for the names of these functions):
  `paren-match-quoted-paren'
  `paren-match-paired-delimiter'
  `paren-open-paren-context-backward'" t nil)

(autoload 'paren-deactivate "mic-paren" "\
Deactivates mic-paren parenthesis highlighting" t nil)

(autoload 'paren-toggle-matching-paired-delimiter "mic-paren" "\
Toggle matching paired delimiter, force on with positive arg. Use this in
mode-hooks to activate or deactivate paired delimiter matching. If optional
second argument NO-MESSAGE is not nil then no message is displayed about the
current activation state of the paired-delimiter-matching feature." t nil)

(autoload 'paren-toggle-matching-quoted-paren "mic-paren" "\
Toggle matching quoted parens, force on with positive arg. Use this in
mode-hooks to activate or deactivate quoted paren matching. If optional second
argument NO-MESSAGE is not nil then no message is displayed about the current
activation state of the quoted-paren-matching feature." t nil)

(autoload 'paren-toggle-open-paren-context "mic-paren" "\
Toggle the determining of the context to display of the matching
open-paren, force backward context with positive arg. Use this in mode-hooks.
For a description of the meaning look at `paren-open-paren-context-backward'." t nil)

(autoload 'paren-forward-sexp "mic-paren" "\
Acts like forward-sexp but can also handle quoted parens. Look at
`paren-match-quoted-paren' for a detailed comment." t nil)

(autoload 'paren-backward-sexp "mic-paren" "\
Acts like backward-sexp but can also matching quoted parens. Look at
`paren-match-quoted-paren' for a detailed comment" t nil)

;;;***

;;;### (autoloads (narrow-stack-mode) "narrow-stack" "edit-utils/narrow-stack.el")

(autoload 'narrow-stack-mode "narrow-stack" "\
narrow-stack-mode implements a stack of narrows. Thus with
narrow-stack-mode you may narrow within narrowing, and widen to the
previous narrowing

For more information on narrowing, see narrow-to-region" t nil)

;;;***

;;;### (autoloads (outl-mouse-minor-mode outl-mouse-mode) "outl-mouse" "edit-utils/outl-mouse.el")

(autoload 'outl-mouse-mode "outl-mouse" "\
Calls outline-mode, with outl-mouse extensions" t nil)

(autoload 'outl-mouse-minor-mode "outl-mouse" "\
Toggles outline-minor-mode, with outl-mouse extensions" t nil)

;;;***

;;;### (autoloads (blink-paren paren-set-mode) "paren" "edit-utils/paren.el")

(defcustom paren-mode nil "*Sets the style of parenthesis highlighting.\nValid values are nil, `blink-paren', `paren', and `sexp'.\n  nil		no parenthesis highlighting.\n  blink-paren	causes the matching paren to blink.\n  paren		causes the matching paren to be highlighted but not to blink.\n  sexp		whole expression enclosed by the local paren at its mate.\n  sexp-surround whole surrounding expression, even if point is not at paren.\n  nested	(not yet implemented) use variable shading to see the\n		nesting of an expression.  Also groks regular expressions\n		and shell quoting.\n\nThis variable is global by default, but you can make it buffer-local and\nhighlight parentheses differently in different major modes." :type '(radio (const :tag "None (default)" nil) (const :tag "Blinking Paren" blink-paren) (const :tag "Highlighted Paren" paren) (const :tag "Highlighted Expression" sexp) (const :tag "Highlighted Surrounding Expression" sexp-surround)) :set (lambda (symbol value) (paren-set-mode value)) :initialize 'custom-initialize-default :require 'paren :group 'paren-matching)

(autoload 'paren-set-mode "paren" "\
Cycles through possible values for `paren-mode', force off with negative arg.
When called from lisp, a symbolic value for `paren-mode' can be passed directly.
See also `paren-mode' and `paren-highlight'." t nil)

(make-obsolete 'blink-paren 'paren-set-mode)

(autoload 'blink-paren "paren" "\
Obsolete.  Use `paren-set-mode' instead." t nil)

;;;***

;;;### (autoloads (turn-on-permanent-buffers permanent-buffers-mode) "permanent-buffers" "edit-utils/permanent-buffers.el")

(defcustom permanent-buffers-mode nil "Indicates whether the `permanent-buffers' behavior is active. You can\ncustomize this variable to change the default value. To change the value\nduring an XEmacs session, please use the function instead." :type 'boolean :set (lambda (sym val) (permanent-buffers-mode (if val 1 -1))) :initialize 'custom-initialize-default :require 'permanent-buffers :group 'permanent-buffers)

(autoload 'permanent-buffers-mode "permanent-buffers" "\
Toggle on/off the permanent buffers behavior. With a prefix > 0 turn it on.
A permanent buffer is a special buffer, not attached to any file, that can't
be killed or saved-as. If you kill it or save it as a file, it will be
regenerated fresh and empty. See also the variable `permanent-buffers-alist'." t nil)

(autoload 'turn-on-permanent-buffers "permanent-buffers" "\
Unconditionally turn on the `permanent-buffers' behavior." t nil)

;;;***

;;;### (autoloads (popper-install) "popper" "edit-utils/popper.el")

(autoload 'popper-install "popper" "\
Install popper into Emacs." t nil)

;;;***

;;;### (autoloads (recent-files-visit-file recent-files-initialize) "recent-files" "edit-utils/recent-files.el")

(autoload 'recent-files-initialize "recent-files" "\
Initialize the recent-files menu." t nil)

(autoload 'recent-files-visit-file "recent-files" "\
Visit a recent file.
Visit a file FILENAME that was visited recently.  Optional second argument
specifies the coding system to use when decoding the file.  Interactively,
with a prefix argument, you will be prompted for the coding system." t nil)

;;;***

;;;### (autoloads (resume-suspend-hook) "resume" "edit-utils/resume.el")

(autoload 'resume-suspend-hook "resume" "\
Clear out the file used for transmitting args when Emacs resumes." nil nil)

;;;***

;;;### (autoloads (resize-minibuffer-mode) "rsz-minibuf" "edit-utils/rsz-minibuf.el")

(autoload 'resize-minibuffer-mode "rsz-minibuf" "\
Enable or disable resize-minibuffer mode.
A negative prefix argument disables this mode.  A positive argument or
argument of 0 enables it.

When this minor mode is enabled, the minibuffer is dynamically resized to
contain the entire region of text put in it as you type.

The variable `resize-minibuffer-mode' is set to t or nil depending on
whether this mode is active or not.

The maximum height to which the minibuffer can grow is controlled by the
variable `resize-minibuffer-window-max-height'.

The variable `resize-minibuffer-window-exactly' determines whether the
minibuffer window should ever be shrunk to make it no larger than needed to
display its contents.

When using a window system, it is possible for a minibuffer to be the sole
window in a frame.  Since that window is already its maximum size, the only
way to make more text visible at once is to increase the size of the frame.
The variable `resize-minibuffer-frame' controls whether this should be
done.  The variables `resize-minibuffer-frame-max-height' and
`resize-minibuffer-frame-exactly' are analogous to their window
counterparts." t nil)

;;;***

;;;### (autoloads (savehist-mode) "savehist" "edit-utils/savehist.el")

(defcustom savehist-mode nil "Mode for automatic saving of minibuffer history.\nSet this by calling the `savehist-mode' function or using the customize\ninterface." :type 'boolean :set (lambda (symbol value) (savehist-mode (or value 0))) :initialize 'custom-initialize-default :require 'savehist :group 'savehist)

(autoload 'savehist-mode "savehist" "\
Toggle savehist-mode.
Positive ARG turns on `savehist-mode'.  When on, savehist-mode causes
minibuffer history to be saved periodically and when exiting Emacs.
When turned on for the first time in an Emacs session, it causes the
previous minibuffer history to be loaded from `savehist-file'.

This mode should normally be turned on from your Emacs init file.
Calling it at any other time replaces your current minibuffer histories,
which is probably undesirable." t nil)

;;;***

;;;### (autoloads (list-matches-in-buffers) "search-buffers" "edit-utils/search-buffers.el")

(autoload 'list-matches-in-buffers "search-buffers" "\
List lines matching REGEXP in any matching buffer.
All buffers chosen via `buffer-regexp-list' are searched.  Results are
displayed in a buffer named *Matches for \"REGEXP\" in buffers*
including hyperlinks to visit any match in any buffer." t nil)

;;;***

;;;### (autoloads (setnu-mode) "setnu" "edit-utils/setnu.el")

(autoload 'setnu-mode "setnu" "\
Toggle setnu-mode.
With prefix argument, turn setnu-mode on if argument is positive.
When setnu-mode is enabled, a line number will appear at the left
margin of each line." t nil)

;;;***

;;;### (autoloads (install-shell-fonts) "shell-font" "edit-utils/shell-font.el")

(autoload 'install-shell-fonts "shell-font" "\
Decorate the current interaction buffer with fonts.
This uses the faces called `shell-prompt', `shell-input' and `shell-output';
you can alter the graphical attributes of those with the normal
face-manipulation functions." nil nil)

;;;***

;;;### (autoloads (toolbar-find-item toolbar-find-button toolbar-kill-item-pos make-toolbar-instantiator toolbar-new-spacer toolbar-new-button toolbar-add-item toolbar-update-toolbar toolbar-add-execute-macro-button toolbar-add-kbd-macro toolbar-add-button-on-the-fly restore-initial-toolbar) "toolbar-utils" "edit-utils/toolbar-utils.el")

(autoload 'restore-initial-toolbar "toolbar-utils" "\
Restores the default toolbar defined by initial-toolbar-spec.

There is also a cache of killed buttons in `button-palette'." t nil)

(autoload 'toolbar-add-button-on-the-fly "toolbar-utils" "\
Add an button at POSITION to the default toolbar of the selected window.
Returns t.

#### The return value may change.  Tell stephen@xemacs.org what value
you think would be (most) useful.

DESCRIPTION is a string describing the action, and displayed as help.
COMMAND is an interactive command (ie, a symbol with an interactive function
definition) implementing the action.
LABEL is a string used to label the button.
POSITION is an optional position (a non-negative integer, or one of the
symbols 'left, 'right, or 'extreme-right, see `toolbar-add-item').
LOCALE is an optional locale, defaulting to the current buffer.  If current-
buffer-only is not what you want, and you don't understand specifier locales,
use 'global.  It's safe and probably does what you want.

#### No error checking, use at your own risk." t nil)

(autoload 'toolbar-add-kbd-macro "toolbar-utils" "\
Add a button invoking a keyboard macro to the toolbar of the current buffer.
The button is added at the end of the left group.

MAC is a keyboard macro name, or the empty string or nil to use a copy of
the last keyboard macro defined.
ICON is a string specifying the icon to be used.  If IS-FILE is non-nil,
it is interpreted as the name of an image file, and searched for using
`locate-data-file'.  Otherwise it is used verbatim as a label.

Interactively, prompts for the macro name MAC and an ICON.  IS-FILE is
non-nil if a prefix argument was used.

Warning: the interpretation of the prefix argument is likely to change." t nil)

(autoload 'toolbar-add-execute-macro-button "toolbar-utils" "\
Add a button to the global toolbar to execute the last keyboard macro.

Unlike `toolbar-add-kbd-macro', this does not copy the macro.  The macro
executed will change with redefinitions.

Due to a simple implementation, this button will not appear in buffers with
local toolbars if invoked after the toolbar is installed.  If you like this
button, it's probably best to invoke this function in your init file." t nil)

(autoload 'toolbar-update-toolbar "toolbar-utils" "\
Use ITEM to update TOOLBAR at POSITION in LOCALE.

ITEM is a toolbar button or spacer specification (eg, from
`toolbar-new-button' or `toolbar-new-spacer').
Optional TOOLBAR is a toolbar specifier object.  It defaults to the value
of `default-toolbar'.
Optional POSITION is a non-negative integer or a symbol (for valid values,
see `toolbar-add-item').  The default is 'right.
Optional LOCALE is a specifier locale.  The default is 'global.  (This
default is not yet set in stone; it's best not to depend on it.)

This is a convenience function for helper applications or minor modes that
would like to add a small number of buttons to an existing toolbar.  For
constructing toolbars from scratch, use list and vector primitives, or
`toolbar-add-item'." nil nil)

(autoload 'toolbar-add-item "toolbar-utils" "\
Add ITEM to TOOLBAR-SPEC at POSITION, and return TOOLBAR-SPEC.
Uses functions that alter list structure.

TOOLBAR-SPEC is a toolbar descriptor (eg, from `toolbar-new-toolbar').
ITEM is a toolbar button or spacer specification (eg, from
`toolbar-new-button' or `toolbar-new-spacer').
Optional POSITION is a non-negative integer, with 0 being the extreme left and
\(length TOOLBAR-SPEC) the extreme right.  The symbols 'left, 'right, and
'extreme-right are also accepted.  'left is synonymous with 0.  'right places
ITEM at the right end of the left group of buttons.  'extreme-right places
ITEM at the extreme right of the toolbar, creating a right group if one
does not exist.

#### This function does not yet support inserting the group delimiter nil
as an explicit item.

POSITION may be greater than (length TOOLBAR-SPEC), in which case it is
truncated to (length TOOLBAR-SPEC).  Note that (length TOOLBAR-SPEC) is not
synonymous with either 'right or 'extreme-right." nil nil)

(autoload 'toolbar-new-button "toolbar-utils" "\
Return a checked toolbar button specification.

ICON-SPEC should be a list of glyphs (from `make-glyph'), a glyph, or a
string to use as the button's icon.  If a string or single glyph, it will
be used for the button-up glyph.  If a list, it may contain 1 to 6 glyphs,
which XEmacs will use for button up, button down, button disabled, button
up with caption, button down with caption, and button disabled with caption,
in that order.  Missing or nil glyphs will be defaulted.  (#### Strings as
list elements are not supported but could be.)
COMMAND is the (interactive) command to invoke when the button is pressed.
HELP-STRING is a string briefly describing the command, displayed in the
echo area or as balloon help when the pointer enters the button.
Optional argument INITIALLY-DISABLED, if non-nil, specifies that the button
should initially be disabled.

See `default-toolbar' or the Lispref (node toolbars) for more information." nil nil)

(autoload 'toolbar-new-spacer "toolbar-utils" "\
Returns a checked toolbar spacer \"button\".

STYLE is one of the symbols '2d or '3d, indicating whether the area is
displayed without shadows (giving it a flat appearance), or with shadows
\(giving it a raised, 3-d appearance).  There is no default.
#### We may set a default style.  Tell stephen@xemacs.org which you use.
SIZE specifies the length, in pixels, of the blank area.  If omitted,
it defaults to a device-specific value (8 pixels for X devices)." nil nil)

(autoload 'make-toolbar-instantiator "toolbar-utils" "\
Return a checked toolbar instantiator, a list of vectors.

TOOLBAR-SPEC may be a list of buttons (ie, a toolbar descriptor, see
`default-toolbar'), a toolbar specifier object, or a symbol whose value is
a toolbar specifier object.  If a toolbar specifier object or variable
containing one, the specification for DOMAIN is used.  If non-nil, DOMAIN
must be a window, a frame, or a device, otherwise it defaults to the selected
window (see `specifier-instance').  The list thus generated is checked and
returned.

If TOOLBAR-SPEC is a list, it is copied; it is safe to pass other packages'
toolbar initializers to this function.  However, you probably do not want
to change any of the objects in the returned specification.  They are
returned as is, not copied.

See `default-toolbar' or the Lispref (node toolbars) for more information." nil nil)

(autoload 'toolbar-kill-item-pos "toolbar-utils" "\
Kill the item at position POS of TOOLBAR in LOCALE.
Killed buttons are prepended to `button-palette'.

LOCALE defaults to 'global.  If there are multiple specs for LOCALE, take
the first one.

This function currently does not accept symbolic positions a la
`toolbar-add-item'.  Use `toolbar-find-item' to locate whole buttons and
spacers, and `toolbar-find-button' to locate buttons by characteristics.
See also `toolbar-find-button-by-icon', `toolbar-find-button-by-command',
and `toolbar-find-button-by-help-string'." nil nil)

(autoload 'toolbar-find-button "toolbar-utils" "\
Return the position of a button containing ITEM in its specification.

ITEM may specify a button, spacer, icon, command, help string, or nil.
If ITEM is nil, find the separator between the group of buttons to be left
justified, and the group to be right justified.  (Distinctions among the
various \"search key types\" are somewhat heuristic but are probably
reliable enough to use in library code.)

If TOOLBAR is non-nil, search it; otherwise search the default toolbar.
If LOCALE is non-nil, get TOOLBAR's descriptor in that locale, otherwise
use the global locale." nil nil)

(autoload 'toolbar-find-item "toolbar-utils" "\
Return the position of ITEM, a button, spacer, or nil.
TOOLBAR and LOCALE determine the descriptor to be searched.

If ITEM is nil, find the separator between the group of buttons to be left
justified, and the group to be right justified.
If there are several matching items, the first is returned.  If none is
found, return nil." nil nil)

;;;***

;;;### (autoloads (vertical-mode) "vertical-mode" "edit-utils/vertical-mode.el")

(autoload 'vertical-mode "vertical-mode" "\
This function toggles vertical mode on and off." t nil)

;;;***

;;;### (autoloads (install-where-was-i toggle-where-was-i) "where-was-i-db" "edit-utils/where-was-i-db.el")

(autoload 'toggle-where-was-i "where-was-i-db" "\
Toggle whether to save your place in this file between sessions.

Toggling place saving on in a file's buffer will cause
`where-was-i-db' to install its hooks, via `install-where-was-i'.  You
do not need to explicitly run an install function -- just toggle it on
for a file for which you want the cursor position to be saved.  In
order for this feature to be automatically installed at startup, you
must customize `wwi-auto-install-on-startup-flag'.

Place saving is enabled ONLY for files in which you've explicitly
toggled it on for.  This prevents application programs, such as Gnus,
W3, VM, or your own programs from making extraneous `where-was-i-db'
database entries for their machine-generated files.

If this mode is enabled, point is recorded for you when you kill the
buffer or exit XEmacs.  Visiting a file again that has had a database
entry made in this fashion will cause `point' to be restored to the
saved position, even in a later XEmacs editing session.

If called with a prefix arg, the mode is enabled if and only if the
argument is positive.  This is for use by program code.  In that case,
you may also like to bind the value of `where-was-i-db'.  You should
not globally `setq' it; but bind it with a `let' around the code for
which you wish point saved in a separate-from-the-main database.

There is more information in the comment header of this program.  It
may be worth reading.  If you haven't installed the `.el' codes for
reference, on at least one machine in your system, you deserve to
lose.
" t nil)

(autoload 'install-where-was-i "where-was-i-db" "\
Unconditionally activate `where-was-i-db' by installing some hook
functions.  An optional prefix arg <= 0 will uninstall the feature.

See the function documentation to `toggle-where-was-i' for more
information about the `where-was-i-db' feature, and consult the code
itself for more information about unloading the feature." t nil)

(defconst toggle-where-was-i nil "\
This is set when `where-was-i-db' place saving is enabled for this
buffer's file.  This must be set by `M-x toggle-where-was-i', since it
performs additional actions beyond just toggling this permanent
buffer-local flag variable.")

;;;***

;;;### (autoloads (winring-rename-configuration winring-delete-configuration winring-jump-to-configuration winring-prev-configuration winring-next-configuration winring-duplicate-configuration winring-new-configuration) "winring" "edit-utils/winring.el")

(autoload 'winring-new-configuration "winring" "\
Save the current window configuration and create an empty new one.
The buffer shown in the new empty configuration is defined by
`winring-new-config-buffer-name'.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name." t nil)

(autoload 'winring-duplicate-configuration "winring" "\
Push the current window configuration on the ring, and duplicate it.

With \\[universal-argument] prompt for the new configuration's name.
Otherwise, the function in `winring-name-generator' will be called to
get the new configuration's name." t nil)

(autoload 'winring-next-configuration "winring" "\
Switch to the next window configuration for this frame." t nil)

(autoload 'winring-prev-configuration "winring" "\
Switch to the previous window configuration for this frame." t nil)

(autoload 'winring-jump-to-configuration "winring" "\
Go to the named window configuration." t nil)

(autoload 'winring-delete-configuration "winring" "\
Delete the current configuration and switch to the next one.
With \\[universal-argument] prompt for named configuration to delete." t nil)

(autoload 'winring-rename-configuration "winring" "\
Rename the current configuration to NAME." t nil)

;;;***

(provide 'edit-utils-autoloads)
