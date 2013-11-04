;;; popper.el --- shrink-wrapped temporary windows for Emacs

;;;     Author: Chris McConnell <ccm@cs.cmu.edu>
;;; Maintainer: Greg Klanderman <greg.klanderman@alum.mit.edu>
;;;    Version: 2.40 XEmacs 1.5
;;;       Date: September 24, 1998
;;;    Created: 1990 (?)
;;;   Keywords: temporary window, popup window


;;; Copyright (C) 1990, 1991, 1992 Chris McConnell, ccm@cs.cmu.edu.
;;; Thanks to Ken Laprade for suggestions and patches.

;;; Copyright (C) 1997, 1998 Greg Klanderman


;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.


;;; Commentary:

;;; 1. VERSION:
;;;
;;; This version of popper is based on version 2.40, which as far as I can
;;; tell is the last version distributed by Chris McConnell, the original
;;; author of popper.  I have been unsuccessful at contacting him and so
;;; have decided to take over maintenance of the package for XEmacs.  New
;;; versions distributed by me will be designated "2.40 XEmacs X.Y".  This
;;; designation is not meant to imply that this is an XEmacs-specific
;;; version, only that it is the version distributed with XEmacs.  This
;;; version incorporates significant bug fixes and changes with respect to
;;; the original.  Overall it should work much better.  Please let me know
;;; if you have any problems or suggestions.

;;; 2. DESCRIPTION:
;;;
;;; This module provides a single shrink-wrapped window for displaying
;;; temporary text called the popper window.  At any time there is at
;;; most one popper window on a given emacs frame.  If there is an entry
;;; for the buffer being displayed on popper-min-heights, the size of
;;; the window will be from that entry.  If the buffer is empty, the
;;; size of the window will be popper-empty-min.  Otherwise its size
;;; will be the minimum of the number of lines of text being displayed
;;; and half the number of lines in the currently selected window when
;;; the popper window was created.  It will work with any function that
;;; uses temporary windows or that has been wrapped with popper-wrap.
;;; The window can be scrolled or buried from any other window.
;;;
;;; When a buffer is displayed using the function
;;; with-output-to-temp-buffer, the text will be displayed in the
;;; popper window if the name of the buffer is in popper-pop-buffers
;;; or popper-pop-buffers is set to `t' and the name is not in
;;; popper-no-pop-buffers.  If you have a buffer with a process, you
;;; can cause it to automatically scroll by setting
;;; popper-scroll-buffers to `t' or to a list of buffer names to
;;; scroll.  Many kinds of completion and help information are
;;; displayed this way.  In general any buffer with *'s around its
;;; name will be a temporary buffer.  Some commands like shell-command
;;; do not use with-output-to-temp-buffer even though you might like
;;; to have their output be temporary.  For commands like this, you
;;; can define a wrapper using the function popper-wrap.
;;;
;;; If popper-use-message-buffer is non-nil then popper output that is
;;; one line or less will be displayed in the minibuffer.
;;;
;;; The default binding for C-x o is changed so that when a buffer is
;;; displayed in a popper window, it will be skipped if it is in
;;; popper-buffers-to-skip or popper-buffers-to-skip is `t' and it is
;;; not in popper-buffers-no-skip.

;;; 3. USAGE:
;;;
;;; Load this file, preferably after byte-compiling it.  By default
;;; popper does not install itself unless the variable
;;; `popper-install-when-load' has been set non-nil before it is loaded.
;;; To manually install popper, type the command M-x popper-install.  If
;;; you later decide popper is not your cup-o-tea, you may un-install it
;;; using the command M-x popper-unstall.
;;;
;;; Once installed, all temporary windows will be handled by popper by
;;; default.  If you do not define custom key bindings using the
;;; popper-load-hook, the bindings for manipulating the popper temporary
;;; window will be:
;;; 
;;;   * C-z C-z, C-z z, C-z 1 : popper-bury-output
;;;     Alternately hide or show the temporary pop-up window.
;;;   
;;;   * C-z C-v, C-z v : popper-scroll-output
;;;     Scroll the popper output window.
;;;   
;;;   * C-z M-v, C-z V : popper-scroll-output-back
;;;     Scroll the popper output window backwards.
;;;   
;;;   * C-z < : popper-beginning-of-buffer
;;;     Scroll the popper output window to the beginning of the buffer.
;;;   
;;;   * C-z > : popper-end-of-buffer
;;;     Scroll the popper output window to the end of the buffer.
;;;   
;;;   * C-z C-j, C-z j, C-z C-o, C-z o : popper-jump-to-popper
;;;     Select the popper window, or jump back to the previous window
;;;     if the popper window is already selected.  With a C-u argument,
;;;     show the popper window first if necessary.
;;;   
;;;   * C-z C-s : popper-isearch-forward
;;;     Do incremental search forward in the popper buffer.
;;;   
;;;   * C-z C-r : popper-isearch-backward
;;;     Do incremental search backward in the popper buffer.
;;;   
;;;   * C-z C-M-s : popper-isearch-forward-regexp
;;;     Do regexp incremental search forward in the popper buffer.
;;;   
;;;   * C-z C-M-r : popper-isearch-backward-regexp
;;;     Do regexp incremental search backward in the popper buffer.
;;;   
;;;   * C-z g : popper-grow-output
;;;     Grow the popper output window.
;;;   
;;;   * C-z G, C-z M-g : popper-shrink-output
;;;     Shrink the popper output window.
;;;   
;;;   * C-z C-b, C-z b : popper-switch-buffer
;;;     Prompt for a different popper temporary buffer to show.
;;;   
;;;   * C-z C-k, C-z k : popper-kill
;;;     Kill or bury the current popper buffer and drop it off of the
;;;     stack of popper buffers for this frame.  If the stack of popper
;;;     buffers (for the selected frame) is not empty, show the new
;;;     top-most buffer.  The buffer is killed if it has been saved with
;;;     `popper-save'; otherwise it is just buried.
;;;   
;;;   * C-z C-x, C-z x : popper-cycle
;;;     Cycle through all popper buffers (for the selected frame) by
;;;     dropping the current popper buffer to bottom of the stack.
;;;   
;;;   * C-z s : popper-save
;;;     Save the current popper buffer by renaming it so that it won't be
;;;     overwritten. 
;;;
;;;   * C-x o : popper-other-window
;;;     Like other-window, but (by default) skips the popper window.
;;;     Use a C-u prefix to select the popper window.  See the variables
;;;     `popper-buffers-to-skip' and `popper-buffers-no-skip'.
;;;
;;;   * C-x 0 : popper-delete-window
;;;     Like delete-window, but hides and re-shows the popper window
;;;     around the call to delete-window.  Also, the variable
;;;     `popper-delete-window-can-delete-frame' can be set to nil to
;;;     prevent deletion of the frame if the user attempts to delete the
;;;     last window.
;;;
;;; Note that "C-z" is the popper-prefix, and that many bindings in the
;;; popper map are chosen by analogy to a similar command in the "C-x"
;;; map.  If you have suggestions for more intuitive or easily typed
;;; bindings, or additional user commands that would be nice, please let
;;; me know.

;;; 4. USER CUSTOMIZATION:
;;; 
;;; See the section `Popper user variables' below for configurable
;;; variables.  Here is a sample load hook for your .emacs:
;;;
;;; (defun gk:popper-load-hook ()
;;;   (setq popper-install-when-load t)          ; Hooray!
;;;   (setq popper-mode-line-text " Popper")     ; I know the key bindings
;;;   (setq popper-no-pop-buffers '("^Man: "))   ; don't pop manual buffers
;;;   (setq popper-max-heights '((".*" . (40)))) ; use at most 40%
;;;   (setq popper-delete-window-can-delete-frame nil) ; extremely losing...
;;;   )
;;; (add-hook 'popper-load-hook 'gk:popper-load-hook)
;;; (require 'popper)
;;;
;;; Once popper is installed, you may wrap a command that does not use
;;; `with-output-to-temp-buffer' so that it will work with popper as
;;; follows:
;;;
;;; (popper-wrap command "*Command Output*")
;;;
;;; This may not yet work in all cases.  It is known not to work for
;;; M-x compile at this time (See the TODO list below).
;;;
;;; When loaded, popper checks the values of certain user variable for
;;; possible problems, unless `popper-inhibit-warnings' is non-nil.  In
;;; addition, if `popper-fix-incompatible-settings' has been set non-nil,
;;; popper will set these variables to values known to work best.
;;; See also the variable `popper-suggested-settings'.

;;; 5. WARNINGS:
;;;
;;; This package redefines the value of `temp-buffer-show-function'.
;;; It also advises the functions `split-window' and `pop-to-buffer'
;;; so that the popper window is buried before calling the old
;;; definition.  The macro `popper-wrap' uses defadvice.

;;; 6. CHANGE-LOG / HISTORY:
;;;
;;; September 24, 1998
;;;   * Increment version to XEmacs 1.5.
;;;   * In `popper-shrink-window' obey window-min-height in the pixels case.
;;;
;;; September 14, 1998
;;;   * Avoid popping up the window in `popper-show' when message buffer got
;;;     used via `popper-use-message-buffer'.
;;;   * Setting `popper-inhibit-warnings' should not also inhibit fixing 
;;;     incompatible settings when `popper-fix-incompatible-settings' is true.
;;;
;;; May 28, 1998
;;;   * Increment version to XEmacs 1.4.
;;;   * In popper-shrink-window, in the pixels case need to compute size
;;;     with respect to the popper buffer's point-min.  Also if point-max
;;;     is visible, call popper-end-of-buffer.
;;;   * Bugfix - popper-beginning-of-buffer needs buffer arg for point-min.
;;;   * Bugfixes in popper-delete-window for display jump avoidance.
;;;   * Bugfix popper-end-of-buffer, popper-grow-output, and
;;;     popper-do-delete-window when popper-use-pixel-heights=t.
;;;
;;; May 20, 1998
;;;   * Increment the version number to XEmacs 1.3.
;;;   * In popper-delete-window, if we delete the frame, we shouldn't re-show
;;;     popper window.  Also, restore popper parent if it didn't get deleted.
;;;   * More pixel-level display jump avoidance.
;;;
;;; May 19, 1998
;;;   * Remove popper-text-lines-pixel-height and popper-vertical-pixel-motion,
;;;     instead use the new primitives window-displayed-text-pixel-height and
;;;     vertical-motion-pixels which do the right things fast.
;;;   * Function window-text-pixel-height changed name to
;;;     window-text-area-pixel-height.
;;;
;;; May 12, 1998
;;;   * Optimize popper-text-lines-pixel-height and
;;;     popper-vertical-pixel-motion to be fast in the case that all lines are
;;;     default height.  Fall back to slow code when this assumption fails.
;;;
;;; May 7, 1998
;;;   * Add internal variable `popper-select-window-has-norecord' to control 
;;;     whether `select-window' has the argument.  Use it in
;;;     `popper-select-window', a new function used throughout.
;;;     Also define macro `popper-with-selected-window' and use it.
;;;
;;; May 6, 1998
;;;   * Work on popper-show-output scrolling of parent to keep point visible.
;;;     For the pixels case use the new function popper-vertical-pixel-motion.
;;;   * Clean up customization.
;;;
;;; May 5, 1998
;;;   * Roll-back-the-parent code in popper-bury-output should still avoid
;;;     redisplay jump if the popper-restore-scroll property does not match.
;;;   * Re-work popper-shrink-window for pixels case.
;;;     New function popper-text-lines-pixel-height. 
;;;   * Avoid select-window where possible.
;;;   * Various cleanups.
;;;
;;; Mar 30, 1998
;;;   * Try to undo the most egregious effects of select-window calling
;;;     record-buffer - in popper-show and popper-do-search.
;;;
;;; Mar 27, 1998
;;;   * Restore the popper-parent's window-start when the popper window gets
;;;     buried, if we had to move it down to keep the parent's point visible.
;;;     New variable `popper-context-lines'.
;;;   * Various cleanup.
;;;
;;; Mar 15, 1998
;;;   * First pass at customization.
;;;   * When splitting windows, restore the popper window from its original
;;;     parent window.
;;;
;;; Mar 14, 1998
;;;   * Use advice to redefine `pop-to-buffer' and `split-window'.
;;;   * Turn `popper-output-buffers' into a frame property.  We should now
;;;     handle multiple frames correctly.  Do things the old way if we don't
;;;     have `set-frame-property'.
;;;   * Don't use `window-displayed-height' - it is unreliable.  Define and
;;;     use `popper-window-displayed-height' which works so long as all the
;;;     text in question is in the default face's font.
;;;
;;; Mar 12, 1998
;;;   * Use a frame property to store the window popper-jump-to-popper came
;;;     from so we can go back if later called from the popper window.
;;;
;;; Mar 8, 1998
;;;   * New internal variable `popper-use-pixel-heights' determines whether
;;;     popper does pixel-level adjustments of the window size.  Enabled
;;;     when the needed functions are available.  New functions
;;;     `popper-window-height', `popper-enlarge-window', and
;;;     `popper-height-to-chars' abstract this decision from the rest of
;;;     the code.
;;;   * Define window-displayed-height as best we can if it doesn't exist.
;;;   * New function popper-build-keymap called from popper-install.
;;;   * Have popper-grow-output obey window-min-height.
;;;   * Put popper-eat-newlines code in its own function:
;;;     popper-trim-whitespace.
;;;   * Fix: popper-min-height should return at least window-min-height.
;;;   * Fix logic error in popper-skip-buffer-p
;;;   * Make popper-last-output-window local variable in popper-show-output
;;;   * Nuke popper-temp-buffer-show-function - Emacs 18.xx compatibility
;;;     is is likely long gone.
;;;
;;; Mar 4, 1998
;;;   * Use symbol-value rather than eval to get value of the symbol
;;;     stored in popper-temp-buffer-show-function.
;;;
;;; Feb 25, 1998: Version XEmacs 1.2
;;;   * XEmacs now uses temp-buffer-show-function for showing
;;;     byte-compilation and display-warning warnings, so we 
;;;     no longer need to have handling these on the TODO list.
;;;   * Don't ever let blank space be shown below (point-max).
;;;     If this happens, please report it as a bug.
;;;   * Several other bugfixes + cleanup
;;;   * New function popper-shrink-output
;;;   * Keep zmacs region when appropriate
;;;   * Don't install popper on loading, use (popper-install),
;;;     or set popper-install-when-load non-nil before loading
;;;     or in your popper-load-hook.
;;;   * New user variable popper-install-when-load 
;;;     and private var popper-installed-p.
;;;     New function popper-install, autoloaded.
;;;     New function popper-unstall to uninstall.
;;;   * Save original show function in popper-original-show-function
;;;   * Add popper-jump-to-popper.
;;;   * Default popper-fix-incompatible-settings to nil.
;;;   * Reformat commentary.
;;;
;;; Feb 23, 1998: Version XEmacs 1.1
;;;   * Add popper-beginning-of-buffer, popper-end-of-buffer
;;;   * Add popper-isearch-* functions.
;;;
;;; Feb 22, 1998: Version XEmacs 1.0
;;;   * To be packaged with XEmacs 20.5b28.
;;;
;;; Updated March 1997 -- February 1998 by Greg Klanderman
;;;   * Fix up unreadable indentation 
;;;   * Fix off-by-one error in popper-eat-newlines code
;;;   * Fix lossage in popper-other-window
;;;   * Modularize predicates for skip, handle, scroll
;;;   * Took out fsf-19 alternate args for where-is-internal;
;;;     the docs didn't seem to indicate this was correct,
;;;     and it seg faulted FSF 19.27 when I tried it.
;;;   * Much code cleanup and reorganization
;;;   * Keep split-window from creating "half lines", eg, compute the
;;;     size if the size arg is nil (meaning "split in half").
;;;     This caused popper windows to sometimes have a partial blank  
;;;     line or text line at the bottom which was rather annoying.
;;;     This seems to also be the cause of the "won't change only window 
;;;     bug".  Eg, the window is height 9, gets split into two windows 
;;;     of height 4 (actually 4.5, but window-height returns 4) then
;;;     things get hosed...  Also wrapped a (when more-than-one ..)
;;;     around the enlarge-window since the error may still happen when 
;;;     somebody mixes different height fonts or has
;;;     modeline-shadow-thickness non-zero.
;;;   * Handle more emacs/XEmacs versions (hopefully! (let me know..))
;;;   * Fix second off-by-one error in popper-shrink-window which allowed
;;;     it to inadvertently enlarge the window by at most one line
;;;   * Added popper-max-heights variable to set max size of pop up windows
;;;   * Add popper-split-preserve-popper-size, defaults to t.  The old 
;;;     behavior meant to deal with incongruity in behavior of split-window
;;;     with and without arg from the popper-parent window.
;;;      - when set t: without arg, splits window in half as if popper
;;;        weren't there; with arg N, splits at N lines, relative to bottom
;;;        of pop up window.
;;;      - when set nil: we hide just popper, split, and reshow popper
;;;        without trying to preserve pop up size; its just like the popper
;;;        weren't there.
;;;   * Found the variable split-window-keep-point, set nil = a good thing
;;;   * Fix bug with split-window redefinition when called from 
;;;     split-window-vertically and split-window-keep-point=nil.
;;;     Is this solution too much of a hack??
;;;   * Fix delete-window too (a la split-window).  Instead of redefinition,
;;;     define popper-delete-window and replace keymap entries
;;;   * Delete-window deletes frame if its the only window - bad bad bad.
;;;     Make popper-delete-window obey the new variable
;;;     popper-delete-window-can-delete-frame
;;;   * Have popper-delete-window avoid display jump when deleting the
;;;     topmost window
;;;   * Define popper-kill (on C-z k) and popper-cycle (on C-z x) to kill
;;;     and cycle the popper buffer, respectively.
;;;   * Define popper-save (on C-z s) to rename popper buffer so it's saved
;;;   * Introduce popper-prefix variable and popper-map keymap
;;;   * Made the popper-pop-buffers list and kin be regexp lists

;;; 7. TODO / KNOWN BUGS:
;;;
;;; finish code cleanup
;;;
;;; process stuff is totally broken
;;; process stuff should allow growing as output is produced
;;;
;;; list ranking preference of window to split
;;;     recognized values: 'current, 'topmost, 'biggest
;;;     current == current, or previous if minibuffer
;;;     others are self explanatory
;;;     or just have a hook or two (from minibuf or not)
;;;
;;; popper-gravity - 'top or 'bottom


;;; ======================================================================


;;; Code:

;;;
;;; Do the right thing if we're being re-loaded
;;;

(eval-when-compile
  (defvar popper-installed-p)
  (defvar popper-install-when-load))
  
(when (and (boundp 'popper-installed-p)
           popper-installed-p)
  (setq popper-install-when-load t)
  (popper-unstall))


;;;
;;; Popper user variables
;;;

(defgroup popper nil
  "Shrink wrapped display of temporary pop-up buffers."
  :group 'environment)

(defcustom popper-install-when-load nil
  "*If non-nil, install popper when it is loaded.
If this variable is set and saved with M-x customize-variable, popper
will be loaded and initialized automatically when emacs is started."
  :type    'boolean
  :require 'popper
  :group   'popper)

(defcustom popper-load-hook nil
  "*List of functions to run when the popper module is loaded, but prior
to popper installation.  This is a good place to change any options."
  :type  'hook
  :group 'popper)

(defcustom popper-prefix "\C-z"
  "*If non-nil, where to install the popper prefix keymap"
  :type  '(choice
           (const :tag "No binding" nil)
           (sexp :tag "Binding"))
  :group 'popper)

(defcustom popper-use-message-buffer nil
  "*If non-nil makes output to popper-buffers that is one line or less
go to the minibuffer."
  :type  'boolean
  :group 'popper)

(defcustom popper-context-lines 1
  "*Number of lines of context to keep above point when popper is shown.
Usually when the popper window is displayed, the parent window's
start position is adjusted to minimize the display jump.  This
variable specifies how many lines of context to keep visible when
the window must be scrolled to keep point visible."
  :type  'integer
  :group 'popper)

(defcustom popper-pop-buffers t
  "*List of buffers to put in the shrink-wrapped pop-up window.  
If it is T, all temporary buffers will be put in the pop-up window."
  :type  '(choice
           (const :tag "All" t)
           (const :tag "None" nil)
           (repeat :tag "Buffer list" (regexp :tag "Buffer regexp")))
  :group 'popper)

(defcustom popper-no-pop-buffers nil
  "*If popper-pop-buffers is T, these buffers will not be put into the
pop-up window."
  :type  '(choice
           (const :tag "None" nil)
           (repeat :tag "Buffer list" (regexp :tag "Buffer regexp")))
  :group 'popper)

(defcustom popper-buffers-to-skip popper-pop-buffers
  "*\\[popper-other-window] will skip over these buffers when they are
used in a temporary window.  If it is T, all popper windows will be
skipped except those in popper-buffers-no-skip."
  :type  '(choice
           (const :tag "All" t)
           (const :tag "None" nil)
           (repeat :tag "Buffer list" (regexp :tag "Buffer regexp")))
  :group 'popper)

(defcustom popper-buffers-no-skip nil
  "*\\[popper-other-window] will not skip these buffers when they are
used in a popper window if popper-buffers-to-skip is T."
  :type  '(choice
           (const :tag "None" nil)
           (repeat :tag "Buffer list" (regexp :tag "Buffer regexp")))
  :group 'popper)

(defcustom popper-eat-newlines t
  "*Boolean for eating trailing newlines in popper buffers."
  :type  'boolean
  :group 'popper)

(defcustom popper-scroll-buffers t
  "*If set to T or a list of buffer names, cause buffers with
associated processes to scroll automatically."
  :type  '(choice
           (const :tag "All" t)
           (repeat :tag "Buffer list" (regexp :tag "Buffer regexp")))
  :group 'popper)

(defcustom popper-empty-min (max 1 (1- window-min-height))
  "*Minimum number of lines to display for an empty popper buffer.
If it is a list, it is a percentage 0-100 of the available space."
  :type  '(choice
           (integer :tag "Lines")
           (list :tag "Percent" (integer)))
  :group 'popper)

(defcustom popper-min-heights nil
  "*List of cons where each car is a regular expression pattern to
match a buffer name and the cdr is the minimum number of lines to
allow when popping buffers that match the regular expression.  If the
number is a list, it is interpreted as the percentage of available
space 0-100 to use for the window."
  :type  '(choice
           (const :tag "None" nil)
           (repeat
            (cons (regexp :tag "Buffer regexp")
                  (choice (integer :tag "Lines")
                          (list :tag "Percent" (integer))))))
  :group 'popper)

(defcustom popper-max-heights nil
  "*List of cons where each car is a regular expression pattern to
match a buffer name and the cdr is the maximum number of lines to
allow when popping up buffers that match the regular expression.  If the
number is a list, it is interpreted as the percentage of available
space 0-100 to use for the window."
  :type  '(choice
           (const :tag "None" nil)
           (repeat
            (cons (regexp :tag "Buffer regexp")
                  (choice (integer :tag "Lines")
                          (list :tag "Percent" (integer))))))
  :group 'popper)

(defcustom popper-mode-line-text nil
  "*Minor mode text for mode line of popper buffers.  If nil, it will
be set to a short help message on first use of popper."
  :type  '(choice (const :tag "Default" nil)
                  (string :tag "Text"))
  :group 'popper)

(defcustom popper-split-preserve-popper-size nil
  "*When non-nil, split-window from the popper parent window should
preserve the pop up window's height."
  :type  'boolean
  :group 'popper)

(defcustom popper-delete-window-can-delete-frame t
  "*When non-nil, `popper-delete-window' will behave like `delete-window'
and when deleting the last window on a frame also delete that frame.
When nil, an attempt to delete the last window on a frame is ignored."
  :type  'boolean
  :group 'popper)

(defcustom popper-use-pixel-heights
  (and (fboundp 'window-pixel-height)
       (fboundp 'window-text-area-pixel-height)
       (fboundp 'enlarge-window-pixels)
       (fboundp 'window-displayed-text-pixel-height)
       (fboundp 'vertical-motion-pixels)
       (let ((func (symbol-function 'vertical-motion)))
         (and (subrp func)  ;; have 3rd inpixels arg?
              (> (subr-max-args func) 2))))
  "*Boolean indicating whether popper should perform pixel level
window height adjustments."
  :type  'boolean
  :group 'popper)

(defcustom popper-inhibit-warnings nil
  "*When non-nil, popper checks the settings of certain Emacs user
variables that might adversely affect popper."
  :type  'boolean
  :group 'popper)

(defcustom popper-fix-incompatible-settings nil
  "*When non-nil, if popper finds settings of certain Emacs user
variables that might adversely affect popper it sets them appropriately."
  :type  'boolean
  :group 'popper)

(defvar popper-map nil
  "The popper keymap.  For complete control over this,
set its bindings from your popper-load-hook.")


;;;
;;; Popper internal variables
;;;

(defvar popper-installed-p nil
  "Is popper currently installed (active)?")

(defvar popper-output-buffers nil
  "LIFO list of buffers displayed in the popper window.
Only used if your emacs does not support frame properties.")

(defvar popper-buffer nil
  "Indicates buffer is a popper for minor-mode-alist.")
(make-variable-buffer-local 'popper-buffer)

(defvar popper-original-filter nil
  "Original process filter, used by `popper-scroll-filter'.")
(make-variable-buffer-local 'popper-original-filter)

(defvar popper-prefix-original-binding nil
  "Save the key binding that is replaced by the popper keymap.")

(defvar popper-original-show-function nil
  "Saves the original value of `temp-buffer-show-function'.")

;;; #### how losing is this?
(defvar popper-suggested-settings
  '((help-selects-help-window  . nil)
    (window-min-height         . 2)
    (scroll-step               . 1)
    (scroll-on-clipped-lines   . nil)
    (pop-up-frames             . nil)
    (pop-up-windows            . t)
    (temp-buffer-shrink-to-fit . nil)
    (split-window-keep-point   . nil))
  "Suggested settings for Emacs variables when popper is in use.")

(defvar popper-wrap-advised-functions nil
  "List of functions advised by popper-wrap.
Used by popper-unstall-popper-wrap.")

(defvar popper-select-window-has-norecord 
  (let ((func (symbol-function 'select-window)))
    (and (subrp func)
         (> (subr-max-args func) 1)))
  "Does `select-window' take a second optional argument `norecord'?")


;;;
;;; General purpose utilities - sure would be nice if these were in emacs
;;;

(defun popper-mem (item list &optional elt=)
  "Test to see if ITEM is equal to an item in LIST.
Optional comparison function ELT= defaults to equal."
  (let ((elt= (or elt= (function equal)))
	(done nil))
    (while (and list (not done))
      (if (funcall elt= item (car list))
	  (setq done list)
        (setq list (cdr list))))
    done))

(defun popper-assoc (item list &optional elt=)
  "Test to see if ITEM is equal to the car of an element of LIST.
Optional comparison function ELT= defaults to equal.
The value is actually the element of LIST whose car equals KEY."
  (let ((elt= (or elt= (function equal)))
	(done nil))
    (while (and list (not done))
      (if (funcall elt= item (car (car list)))
	  (setq done (car list))
        (setq list (cdr list))))
    done))

(defun popper-match-buffer-name (buff regx)
  "Match a buffer name to a regular expression"
  (string-match regx buff))

(defun popper-filter (list &optional pred)
  "Filter LIST with predicate PRED.
Return a new list containing all elements in LIST for which PRED returns
a non-nil value."
  (let ((pred (or pred (function (lambda (x) x))))
        (result nil))
    (while list
      (if (funcall pred (car list))
          (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (nreverse result)))


;;;
;;; Popper internal utilities 
;;;

(defun popper-warn (&rest args)
  "Display a popper warning."
  (display-warning 'popper (apply 'format args)))

(defun popper-handle-buffer-p (&optional buff)
  "Determine if popper is handling BUFF.  BUFF can be a buffer name
or a buffer.  If not specified, it defaults to the current buffer."
  (let ((buff-name  (if (stringp buff) buff (buffer-name buff))))
    (or (and (eq popper-pop-buffers t)
             (not (popper-mem buff-name popper-no-pop-buffers
                              'popper-match-buffer-name)))
        (and (listp popper-pop-buffers)
             (popper-mem buff-name popper-pop-buffers
                         'popper-match-buffer-name)))))

(defun popper-scroll-buffer-p (&optional buff)
  "Determine if popper should scroll process output in BUFF"
  (let ((buff-name  (if (stringp buff) buff (buffer-name buff))))
    (or (eq popper-scroll-buffers t)
        (popper-mem buff-name popper-scroll-buffers
                    'popper-match-buffer-name))))

(defun popper-skip-buffer-p (&optional buffer)
  "Determine if popper-other-window should skip BUFFER, which
defaults to the current buffer."
  (let* ((buff (or buffer (current-buffer)))
         (name (buffer-name buff)))
    (and (eq (popper-output-buffer) buff)
         (or (and (eq popper-buffers-to-skip t)
                  (not (popper-mem name popper-buffers-no-skip
                                   'popper-match-buffer-name)))
             (and (listp popper-buffers-to-skip)
                  (popper-mem buff popper-buffers-to-skip
                              'popper-match-buffer-name))))))

(defun popper-select-window (window)
  "Select WINDOW, using `select-window'.  Pass the `norecord' flag
if this emacs supports it."
  (if popper-select-window-has-norecord
      (select-window window 'norecord)
    (select-window window)))

(defmacro popper-with-selected-window (window &rest body)
  "Execute forms in BODY with WINDOW as the selected window.
The value returned is the value of the last form in BODY."
  (let ((old-win (gensym "popper-wsw")))
    `(let ((,old-win (selected-window)))
       (unwind-protect
           (progn (popper-select-window ,window)
                  ,@body)
         (popper-select-window ,old-win)))))
(put 'popper-with-selected-window 'lisp-indent-function 1)

(defun popper-select (&optional window)
  "Select WINDOW and its buffer.  WINDOW defaults to selected-window."
  (setq window (or window (selected-window)))
  (popper-select-window window)
  (set-buffer (window-buffer window)))

(defun popper-get-output-buffers ()
  "Get the value of `popper-output-buffers' for the current frame."
  (if (fboundp 'set-frame-property)
      (frame-property (selected-frame)
                      'popper-output-buffers)
    popper-output-buffers))

(defun popper-set-output-buffers (value)
  "Set the value of `popper-output-buffers' for the current frame."
  (if (fboundp 'set-frame-property)
      (set-frame-property (selected-frame)
                          'popper-output-buffers
                          value)
    (setq popper-output-buffers value)))  

(defun popper-first-buffer ()
  "Remove killed buffers and return the first buffer on
popper-output-buffers for this frame."
  (let ((output-buffers
         (popper-filter (popper-get-output-buffers)
                        (lambda (buf) (buffer-name buf)))))
    (popper-set-output-buffers output-buffers)
    (car output-buffers)))

(defun popper-output-buffer ()
  "Return the buffer being displayed in the popper window."
  (popper-first-buffer)
  (if (not (eq (selected-window) (next-window nil 'no)))
      (let ((buffers (popper-get-output-buffers))
	    (gotit nil))
	(while buffers
	  (let* ((buffer (car buffers))
		 (window (and (buffer-name buffer)
                              (get-buffer-window buffer))))
	    (if (and window (not gotit))
		(setq gotit buffer)
              (save-excursion
                (set-buffer (car buffers))
                (setq popper-buffer nil)))
            (setq buffers (cdr buffers))))
	(if gotit
            ;; move it to front
            (popper-set-output-buffers
             (cons gotit (delq gotit (popper-get-output-buffers)))))
	gotit)))

;;; #### next-window wraps around ??
(defun popper-parent ()
  "Return the parent of the popper window."
  (let ((output (popper-output-buffer)))
    (if output (next-window (get-buffer-window output) 'no))))

(defun popper-window-height (&optional window)
  "Return the height of WINDOW as measured by popper.
This may be in pixels or characters, depending on the value
of the variable `popper-use-pixel-heights'."
  (setq window (or window (selected-window)))
  (if popper-use-pixel-heights
      (window-pixel-height window)
    (window-height window)))

;;; #### this should probably go away eventually
(defun popper-height-to-chars (height)
  "Convert height to a height in characters, according to
the variable `popper-use-pixel-heights'."
  (if popper-use-pixel-heights
      (/ height (face-height 'default))
    height))

(defun popper-enlarge-window (n)
  "Enlarge current window by N, which may be measured in
pixels or characters depending on the value of the
variable `popper-use-pixel-heights'."
  (if popper-use-pixel-heights
      (enlarge-window-pixels n)
    (enlarge-window n)))

;;; #### this should probably go away eventually
(defun popper-window-displayed-height (&optional window)
  "Return the displayed height of WINDOW as a number of lines
in the font of the default face."
  (if popper-use-pixel-heights
      (/ (window-text-area-pixel-height window)
         (face-height 'default))
    (1- (window-height window))))

(defun popper-window-heights (window)
  "Return a list of the heights of all of the windows following WINDOW."
  (let ((heights nil)
        (startwin window))
    (while (progn 
	     (setq window (next-window window 'no))
	     (not (eq window startwin)))
      (setq heights (cons (popper-window-height window)
                          heights)))
    (nreverse heights)))

(defun popper-min-height ()
  "Return the minimum height to use for the popper buffer in the
current window.  This is either an entry from popper-min-heights,
popper-empty-min if the buffer is empty or window-min-height."
  (let ((min (or (cdr (popper-assoc (buffer-name) popper-min-heights
                                    'popper-match-buffer-name))
                 (if (= (point-min) (point-max))
                     popper-empty-min
                   window-min-height))))
    (max (if (consp min)
             ;; Floating percentage
             (round (/ (* (+ (window-height) (window-height (next-window)))
                          (car min))
                       100.0))
           min)
         window-min-height)))

(defun popper-max-height (buff)
  "Return the maximum height to use for a popper buffer to be
created by splitting the current window.  This is either an entry
from popper-max-heights, or half the height of the current window."
  (let ((val (or (cdr (popper-assoc (buffer-name buff) popper-max-heights
                                    'popper-match-buffer-name))
                 '(50))))
    (when (consp val)                        ; Floating percentage
	(setq val (round (/ (* (window-height) (car val)) 100.0))))
    (max (min val (- (window-height) window-min-height))
         window-min-height)))

;;; #### look at this
(defun popper-scroll-filter (process output)
  "Scroll and keep last point in window."
  (let* ((old (selected-window))
	 (buffer (process-buffer process))
	 (window (get-buffer-window buffer)))
    (save-excursion
      (set-buffer buffer)
      (if popper-original-filter
	  (funcall popper-original-filter process output)
        ;; #### is this case an error ??
        (goto-char (process-mark process))
        (insert output)
        (set-marker (process-mark process) (point))))
    (if (and window)
	(progn
          ;; #### grow the popup if needed & not maxed out
	  (select-window window)
	  (goto-char (point-max))
	  (move-to-window-line nil)
	  (select-window old)))))

;;; #### probably other cleanup
(defun popper-show-output (&optional buffer size)
  "Bring the output window up showing optional BUFFER in window of
SIZE.  If SIZE is not specified, then shrink the window.  Finally
select the original window.  SIZE is measured in either pixels or
characters, according to `popper-use-pixel-heights'."
  ;; if we're in the popper buffer, bury first since by the
  ;; time we detect it below it's difficult to handle
  (if (eq (window-buffer (selected-window))
          (popper-output-buffer))
      (popper-bury-output t))
  (let* ((window (selected-window))
	 (old-buffer (window-buffer window))
	 (buffer (get-buffer-create
		  (or buffer (popper-first-buffer) 
		      (error "No popper buffers"))))
         (char-size (and size (popper-height-to-chars size)))
	 start parent last-output-window
	 (min-height (+ window-min-height (or char-size window-min-height)))
	 (one-line-p (= 1 (save-excursion
                            (set-buffer buffer)
                            (count-lines (point-min) (point-max))))))
    (if (and one-line-p popper-use-message-buffer)
	(message (buffer-substring (point-min buffer) (point-max buffer) buffer))
      (setq last-output-window window)
      (if (eq buffer old-buffer)
          (popper-shrink-window)   ; already shown => just shrink
	(if (eq window (minibuffer-window)) 
	    (let* ((parent (popper-parent)))
	      (popper-bury-output t)
	      (popper-select-window (setq window (or parent
                                                     (previous-window)))))
          (if (not (eq old-buffer (popper-output-buffer)))
              (popper-bury-output t)))
	(if (< (window-height window) min-height)
	    (enlarge-window (- min-height ;; #### ok??
                               (window-height window))))
	(setq start (window-start window))
        (if size
            (progn
              (split-window nil char-size)
              (if popper-use-pixel-heights
                  (enlarge-window-pixels (- size
                                            (window-pixel-height)))))
          (split-window nil (popper-max-height buffer)))
	(set-window-buffer window buffer)
	(set-buffer buffer)
	(setq popper-buffer t)
	(popper-set-output-buffers
         (cons buffer (delq buffer (popper-get-output-buffers))))
	(if (not size)
            (popper-shrink-window))
	(setq parent (next-window window 'no))
	(popper-select parent)
	;; Move the window so that top lines get covered unless it would
	;; cover point in which case point is at top of window
	(save-excursion
	  (set-window-start parent start)
          (if popper-use-pixel-heights
              (progn (goto-char start)
                     (vertical-motion-pixels
                      (window-pixel-height window)))
            (move-to-window-line (window-height window)))
	  (set-window-start parent (point)))
	(let ((point (save-excursion
                       (vertical-motion (- popper-context-lines))
                       (point))))
	  (if (not (pos-visible-in-window-p (point)))
	      (set-window-start (selected-window) point)))
        ;; save how you scrolled in a frame property so we can
        ;; scroll back when we later bury the popper window
        (set-frame-property
         (selected-frame)
         'popper-restore-scroll
         (list window                        ; popper window
               (popper-window-height window) ; height of popper window
               parent                        ; parent window
               (window-buffer parent)        ; parent buffer
               start                         ; old parent window start
               (window-start)))              ; new parent window start
        ;; restore buffer and selected window
	(if (eq last-output-window (minibuffer-window))
	    (popper-select-window (minibuffer-window)))
	(set-buffer old-buffer)))))

(defun popper-trim-whitespace ()
  "Trim leading and trailing whitespace in buffer"
  (let ((buffer-read-only nil)
        (buffer-modified-p (buffer-modified-p)))
    (save-excursion
      ;; Delete trailing blank space
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (if (< (point) (point-max))
          (delete-region (point) (point-max)))
      ;; Delete leading blank space
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (beginning-of-line)
      (if (> (point) (point-min))
          (delete-region (point-min) (point)))
      ;; restore modified-p
      (set-buffer-modified-p buffer-modified-p))))

;;; #### process stuff needs testing
;;; #### be careful not to allow blank space shown after (point-max)
(defun popper-shrink-window ()
  "Shrink the current window if larger than its buffer unless it has
an entry in popper-min-heights or it is empty in which case
popper-empty-min is used."
  (let* ((window (selected-window))
	 (window-lines (popper-window-displayed-height window)))
    (set-buffer (window-buffer window))
    (let ((process (get-buffer-process (current-buffer))))
      ;; fix up leading & trailing space if desired and buffer has no process
      (when (and popper-eat-newlines (not process))
        (popper-trim-whitespace))
      ;; keep scrolling process pop-ups  #### broken
      (when (and process (popper-scroll-buffer-p (current-buffer)))
        (while (not (marker-position (process-mark process)))
          (accept-process-output))
        (goto-char (point-max))
        ;; (set-marker (process-mark process) (point))
        (insert ? ) ;; #### what?
        (set-mark (point))
        (when (not popper-original-filter)
          (setq popper-original-filter (process-filter process))
          (set-process-filter process 'popper-scroll-filter)))
      ;; finally, shrink as needed
      (if popper-use-pixel-heights
          (enlarge-window-pixels
           (let ((old-start (window-start)))
             (unwind-protect
                 (progn
                   (set-window-start window (point-min))
                   (max
                    (- (* (face-height 'default) (popper-min-height))
                       (window-pixel-height))
                    (- (window-displayed-text-pixel-height nil 'noclipped)
                       (window-text-area-pixel-height))))
               (set-window-start window old-start)
               (if (pos-visible-in-window-p (point-max) window)
                   (popper-end-of-buffer)))))
        (enlarge-window (- (max (1+ (save-excursion 
                                      (goto-char (point-min))
                                      (vertical-motion (1- window-lines))))
                                (1- (popper-min-height)))
                           window-lines))))))

(defun popper-show (buffer)
  "Function to display BUFFER in a popper window if it is in
popper-pop-buffers or popper-pop-buffers is T and it is not in
popper-no-pop-buffers.  This is used by popper as the value of
`temp-buffer-show-function'."
  (cond ((popper-handle-buffer-p buffer)
         (popper-show-output buffer)
         (popper-beginning-of-buffer 'noshow)
         ;; select-window calls record-buffer.. ugh
         (if (and (not popper-select-window-has-norecord)
                  (popper-output-buffer)) 
             (bury-buffer (popper-output-buffer))))
        ((and popper-original-show-function  ; paranoia
              (not (eq popper-original-show-function
                       'popper-show)))
         (let ((orig-show popper-original-show-function)
               (popper-original-show-function nil)) ; more paranoia
           (funcall orig-show buffer)))
        (t
         (display-buffer buffer)))
  (setq minibuffer-scroll-window (get-buffer-window buffer)))

(defun popper-read-popper-buffer (prompt &optional default)
  "Read the name of a popper buffer and return as a string.
Prompts with PROMPT.  Optional second arg DEFAULT is value
to return if user enters an empty line."
  (let* ((prompt (if default 
                     (format "%s(default %s) "
                             (gettext prompt)
                             (if (bufferp default)
                                 (buffer-name default)
                               default))
                   prompt))
         (alist (mapcar (lambda (b) (cons (buffer-name b) b))
                        (popper-get-output-buffers)))
         (result (completing-read prompt alist nil t
                                  nil 'buffer-history)))
    (if (equal result "")
        (setq result default))
    (if (bufferp result)
        (buffer-name result)
      result)))

(defun popper-do-search (&optional search-fn)
  "Perform a search in the popper window's buffer."
  (setq search-fn (or search-fn 'isearch-forward))
  (let ((buffer (popper-output-buffer)))
    (if buffer
        (popper-with-selected-window (get-buffer-window buffer)
          (funcall search-fn))
      (error "No popper window displayed"))))


;;;
;;; Popper user commands
;;;

(defun popper-bury-output (&optional no-show)
  "Bury the popper output.  If no popper output window is active, show 
the popper output window, unless optional argument NO-SHOW is non-nil."
  (interactive "_")
  (let ((buffer (popper-output-buffer)))
    (if buffer
	(let* ((old (current-buffer))
	       (old-window (selected-window))
	       (output (get-buffer-window buffer))
	       (start (popper-window-height output))
	       (parent (next-window output 'no))
	       (height (popper-window-height output))
	       (heights (popper-window-heights output))
               (more-than-one (cdr heights)))
	  (bury-buffer buffer)
	  (delete-window output)
	  (popper-select parent)
          (while heights
            ;; only resize if it makes sense, eg there's more than one window
            (when more-than-one
              (popper-enlarge-window (- (+ height (car heights))
                                        (popper-window-height))))
            ;; reset the parent's window-start
            (if start
                (let ((vals (frame-property (selected-frame)
                                            'popper-restore-scroll)))
                  (if (and
                       vals 
                       (eq (nth 0 vals) output) ; popper window
                       (eq (nth 1 vals) height) ; height of popper window
                       (eq (nth 2 vals) parent) ; parent window
                       (eq (nth 3 vals) (window-buffer parent)) ; parent buff
                       (eq (nth 5 vals) (window-start parent))) ; parent start
                      ;; have frame property and is applicable
                      (set-window-start parent (nth 4 vals))
                    ;; otherwise just avoid a redisplay jump...
                    (condition-case ()
                        (if popper-use-pixel-heights
                            (save-excursion
                              (goto-char (window-start))
                              (vertical-motion-pixels (- start))
                              (set-window-start parent (point)))
                          (scroll-down (popper-height-to-chars start)))
                      (error nil)))))
            (popper-select-window (next-window (selected-window) 'no))
            (setq height 0
                  start  nil)
            (setq heights (cdr heights)))
	  (set-buffer old)
	  (if (not (eq old-window output))
	      (popper-select-window old-window)))
      (if (not no-show)
          (popper-show-output)))))

(defun popper-scroll-output (&optional n reverse)
  "Scroll text of the popper window upward ARG lines; or near full
screen if no ARG.  When calling from a program, supply a number as
argument or nil.  If the output window is not being displayed, it will
be brought up."
  (interactive "_P")
  (let ((buffer (popper-output-buffer)))
    (if buffer
        (popper-with-selected-window (get-buffer-window buffer)
          (if (pos-visible-in-window-p (point-max))
              (popper-beginning-of-buffer)
            (condition-case ()
                (if reverse
                    (scroll-down n)
                  (scroll-up n))
              (error ;; can scroll-up ever signal an error ??
               (if (or (null n) (and (numberp n) (> n 0)))
                   (popper-beginning-of-buffer)
                 (popper-end-of-buffer)))))
          (if (pos-visible-in-window-p (point-max))
              (popper-end-of-buffer))
          (if (pos-visible-in-window-p (point-min))
              (popper-beginning-of-buffer))
          (set-window-point (selected-window) (window-start)))
      ;; wasn't shown, so show it
      (popper-show-output))))

(defun popper-scroll-output-back (&optional n)
  "Scroll text of the popper window downward ARG lines; or near full
screen if no ARG.  When calling from a program, supply a number as
argument or nil.  If the output window is not being displayed, it will
be brought up."
  (interactive "_P")
  (popper-scroll-output n t))

(defun popper-beginning-of-buffer (&optional noshow)
  "Scroll the text of the popper window to the beginning of the buffer."
  (interactive "_")
  (let ((buffer (popper-output-buffer)))
    (if buffer
	(let ((window (get-buffer-window buffer)))
          (set-window-start window (point-min buffer))
          (set-window-point window (point-min buffer)))
      (or noshow
          (popper-show-output)))))

(defun popper-end-of-buffer (&optional noshow)
  "Scroll the text of the popper window to the end of the buffer."
  (interactive "_")
  (let ((buffer (popper-output-buffer)))
    (if buffer
        (popper-with-selected-window (get-buffer-window buffer)
          (goto-char (point-max))
          (if popper-use-pixel-heights
              (vertical-motion-pixels (- (vertical-motion 0 nil t)
                                         (window-text-area-pixel-height))
                                      nil
                                      -1) ;; at-most
            (vertical-motion (- 1 (popper-window-displayed-height))))
          (set-window-start (selected-window) (point)))
      (or noshow
          (popper-show-output)))))

(defun popper-isearch-forward ()
  "Do incremental search forward in the popper window."
  (interactive)
  (popper-do-search 'isearch-forward))

(defun popper-isearch-backward ()
  "Do incremental search backward in the popper window."
  (interactive)
  (popper-do-search 'isearch-backward))

(defun popper-isearch-forward-regexp ()
  "Do incremental search forward in the popper window."
  (interactive)
  (popper-do-search 'isearch-forward-regexp))

(defun popper-isearch-backward-regexp ()
  "Do incremental search backward in the popper window."
  (interactive)
  (popper-do-search 'isearch-backward-regexp))

(defun popper-grow-output (&optional n)
  "Grow the popper window by ARG (default 1) lines.  If the popper
window is not being shown, it will be brought up."
  (interactive "_p")
  (let* ((buffer (popper-output-buffer))
         (popwin (and buffer (get-buffer-window buffer)))
         (oldh   (popper-window-height popwin)))
    (if buffer
        (save-excursion
          (popper-with-selected-window popwin
            (setq n (max (- window-min-height (window-height))
                         (min n (- (window-height
                                    (next-window popwin 'no))
                                   window-min-height))))
            (enlarge-window n)
            (popper-select (next-window (selected-window) 'no))
            ;; #### need to adjust the popper-restore-scroll frame property
            ;; #### should save pixel remainder & use it if more growth later
            (save-excursion
              (goto-char (window-start))
              (if popper-use-pixel-heights
                  (vertical-motion-pixels (- (window-pixel-height popwin)
                                             oldh))
                (vertical-motion n))
              (set-window-start (selected-window) (point)))))
      (popper-show-output))))

(defun popper-shrink-output (&optional n)
  "Shrink the popper window by ARG (default 1) lines.  If the popper
window is not being shown, it will be brought up."
  (interactive "_p")
  (popper-grow-output (- n)))

(defun popper-jump-to-popper (arg)
  (interactive "P")
  "Select the popper window, if one exists.
With ARG, pop up the popper window first."
  (let* ((buffer    (popper-output-buffer))
         (window    (and buffer (get-buffer-window buffer)))
         (back      (frame-property (selected-frame)
                                    'popper-window-before-jump))
         (jump-back (and (eq window (selected-window))
                         (windowp back)
                         (window-live-p back)
                         (eq (window-frame back) (selected-frame)))))
    (cond (jump-back (select-window back))
          (window    (set-frame-property (selected-frame)
                                         'popper-window-before-jump
                                         (selected-window))
                     (popper-select-window window)
                     t)
          (arg       (popper-show-output)
                     (popper-jump-to-popper nil))
          (t         (error "No popper window displayed")
                     nil))))

(defun popper-other-window (arg)
  "Select the arg'th other window.  If arg is a C-u prefix, the popper
window will be selected.  Otherwise, windows that contain buffers in
popper-buffers-to-skip will be skipped or if popper-buffers-to-skip is
T those that are not in popper-buffers-no-skip.  This is the popper
 replacement for the function `other-window'"
  (interactive "P")
  (if (consp arg)
      ;; select popper window
      (popper-jump-to-popper nil)
    ;; otherwise, normal other-window behavior, but skip popper windows
    (let ((win (selected-window))
          last-win
          dirn)
      (setq arg  (if (eq arg '-) -1 (if (integerp arg) arg 1)))
      (setq dirn (if (> arg 0) 'next-window 'previous-window))
      (setq arg  (abs arg))
      (while (> arg 0)
        (setq last-win win)
        (setq win (funcall dirn win))
        ;; if we landed on a buffer we want to skip, keep going
        ;; until we find one which is ok or we exhaust all windows
        (while (and (popper-skip-buffer-p (window-buffer win))
                    (not (eq win last-win)))
          (setq win (funcall dirn win)))
        ;; decr arg
        (setq arg (1- arg)))
      (select-window win))))

(defun popper-switch-buffer (buffer)
  "Switch the popper window to BUFFER.  Interactively,
prompt for another popper output buffer to switch to."
  (interactive
   (list (let ((dflt (car (if (popper-output-buffer)
                              (cdr (popper-get-output-buffers))
                            (popper-get-output-buffers)))))
           (when dflt 
             (popper-read-popper-buffer "Popper buffer " dflt)))))
  (if buffer
      (popper-show-output buffer)
    (error "No popper buffer to switch to")))

(defun popper-kill ()
  "Remove the current popper buffer from the LIFO stack of popper 
buffers and show the new topmost buffer, if there is one."
  (interactive "_")
  (let ((buf (popper-output-buffer)))
    (when buf
      (popper-bury-output t)
      (popper-set-output-buffers (delq buf (popper-get-output-buffers)))
      (if (string-match "^popper-save:" (buffer-name buf))
          (kill-buffer buf)
        (bury-buffer buf))
      (if (car (popper-get-output-buffers))
          (popper-show-output)))))

(defun popper-cycle ()
  "Move the current popper buffer to the end of the LIFO stack of 
popper buffers and show the new topmost buffer, if there is one."
  (interactive "_")
  (let ((buf (popper-output-buffer)))
    (when buf
      (popper-bury-output t)
      (popper-set-output-buffers
       (append (delq buf (popper-get-output-buffers))
                    (list buf)))
      (if (car (popper-get-output-buffers))
          (popper-show-output)))))

(defun popper-save ()
  "Rename the current popper buffer to a unique name,
so that it will be preserved."
  (interactive "_")
  (let ((buf (popper-output-buffer)))
    (cond ((null buf)
           (error "No popper buffer to save"))
          ((string-match "^popper-save:" (buffer-name buf))
           (message "Popper buffer already saved"))
          (t
           (save-excursion
             (set-buffer buf)
             (rename-buffer
              (generate-new-buffer-name
               (concat "popper-save:" (buffer-name buf)))))))))

(defun popper-do-delete-window (window force)
  "Helper function for `popper-delete-window'."
  (or window (setq window (selected-window)))
  (if (or popper-delete-window-can-delete-frame
          (cdr (window-list nil 'never)))
      (let* ((sib (next-window window 'never))
             (fixup (and (eq window (frame-highest-window))
                         (not (eq window sib))))
             (dwin-ht (window-height window))
             (dwin-pix (popper-window-height window)))
        ;; delete it
        (delete-window window force)
        ;; when deleting the topmost window, counteract the display jump
        (when fixup
          (save-excursion
            (set-buffer (window-buffer sib))
            (goto-char (window-start sib))
            (if popper-use-pixel-heights
                (vertical-motion-pixels (- dwin-pix))
              (vertical-motion (- dwin-ht)))
            (set-window-start sib (point)))))
    (error "Won't delete last window on a frame")))

(defun popper-delete-window (&optional window force)
  "Remove WINDOW from the display.  Default is selected window.
If window is the only one on the frame, the frame is destroyed.
Normally, you cannot delete the last non-minibuffer-only frame (you must
use `save-buffers-kill-emacs' or `kill-emacs').  However, if optional
second argument FORCE is non-nil, you can delete the last frame. (This
will automatically call `save-buffers-kill-emacs'.)

This function is similar to `delete-window', but buries the popper window
when the window is actually being deleted.  Also, if the variable 
`popper-delete-window-can-delete-frame' is nil, the frame won't be deleted
if the window is the only window on the frame.  Finally, when the topmost
window is deleted, the display jump is avoided if possible."
  (interactive)
  (or window (setq window (selected-window)))
  (let ((parent (popper-parent)))
    (if parent
	(let ((pop-size (and popper-split-preserve-popper-size
                             (popper-window-height
                              (get-buffer-window (popper-output-buffer))))))
          (if (eq parent window) ; deleting parent - choose a new one
              (setq parent (next-window parent 'never)))
	  (popper-bury-output t)
	  (prog1
	      (popper-do-delete-window window force)
            (when (window-live-p parent)
              (popper-with-selected-window parent
                (popper-show-output nil pop-size))
              (if (and (popper-output-buffer)
                       (eq (selected-window)
                           (get-buffer-window (popper-output-buffer))))
                  (popper-other-window nil)))))
      (popper-do-delete-window window force))))


;;;
;;; Function redefinitions
;;;

;; HACK: This is used by the redefined `split-window' as a post-command-hook
;; to reshow the popper window after split-window-vertically finishes.  Is
;; this too ugly, or is there a better way to accomplish this?
(defvar popper-split-window-pop-size nil)
(defvar popper-split-window-parent   nil)
(defun popper-split-window-reshow-output ()
  (remove-hook 'post-command-hook 'popper-split-window-reshow-output)
  (let ((parent (if (and (window-live-p popper-split-window-parent)
                         (eq (window-frame popper-split-window-parent)
                             (selected-frame)))
                    popper-split-window-parent
                  (selected-window))))
    (if (eq parent (selected-window))
        (popper-show-output nil popper-split-window-pop-size)
      (popper-with-selected-window parent
        (popper-show-output nil popper-split-window-pop-size))))
  (setq popper-split-window-pop-size nil)
  (setq popper-split-window-parent   nil))

(defun popper-install-redefinitions ()
  ;;
  ;; Don't do this stuff until popper-install() is called
  ;;
  (require 'advice)
  (ad-start-advice)

  ;; Outer `split-window' advice: bury and restore the popper window
  ;; around the call to split-window.
  (defadvice split-window (around popper:split-window-outer last activate)
    ;; (split-window &optional WINDOW SIZE HOR-FLAG)
    (let ((the-size (ad-get-arg 1))
          (horizontally (ad-get-arg 2))
          (parent (popper-parent)))
      (if (eq parent (selected-window))
          (let ((pop-size (and popper-split-preserve-popper-size
                               (popper-window-height
                                (get-buffer-window (popper-output-buffer))))))
            (if (and the-size pop-size (not horizontally))
                (ad-set-arg 1 (+ the-size (popper-height-to-chars pop-size))))
            (popper-bury-output t)
            (prog1
                ad-do-it
              (if (and (eq this-command 'split-window-vertically)
                       (not split-window-keep-point))
                  (progn ; total hack to reshow *after* s-w-vertically
                    (setq popper-split-window-pop-size pop-size)
                    (setq popper-split-window-parent   parent)
                    (add-hook 'post-command-hook
                              'popper-split-window-reshow-output))
                (popper-show-output nil pop-size))))
        ad-do-it)))

  ;; Inner `split-window' advice: compute the size if not specified -- 
  ;; this prevents a line from getting split in half, which is ugly and
  ;; causes other nightmares because then the sum of the new window-heights
  ;; is not equal to the original window-height.
  (defadvice split-window (around popper:split-window-inner last activate)
    ;; (split-window &optional WINDOW SIZE HOR-FLAG)
    (let ((horizontally (ad-get-arg 2)))
      (if (null (ad-get-arg 1))
          (ad-set-arg 1
                      (max (/ (if horizontally
                                  (window-width window)
                                (window-height window))
                              2)
                           (if horizontally
                               window-min-width
                             window-min-height))))
      ad-do-it))

  ;; Advice for `pop-to-buffer': bury popper output window around call
  (defadvice pop-to-buffer (around popper:pop-to-buffer activate)
    ;; (pop-to-buffer BUFFER &optional OTHER-WINDOW ON-FRAME)
    (let ((the-buffer (ad-get-arg 0))
          (parent (popper-parent)))
      (if (and parent 
               (not (eq (get-buffer the-buffer)
                        (popper-output-buffer))))
          (progn
            (popper-bury-output t)
            ad-do-it
            (popper-select-window parent)
            (popper-show-output)
            (sit-for 0) ; allow display update
            ;; now do it again but use existing window
            (ad-set-arg 1 nil)
            ad-do-it)
        ad-do-it)))

  ) ;; end of popper-install-redefinitions()

(defun popper-unstall-redefinitions ()
  ;;
  ;; uninstall popper redefinitions
  ;;
  (ad-remove-advice 'split-window 'around 'popper:split-window-inner)
  (ad-remove-advice 'split-window 'around 'popper:split-window-outer)
  (ad-update 'split-window)

  (ad-remove-advice 'pop-to-buffer 'around 'popper:pop-to-buffer)
  (ad-update 'pop-to-buffer)
  )


;;;
;;; Wrap functions that don't use with-output-to-temp-buffer
;;;

(defun popper-wrap-register (function)
  "Register FUNCTION as popper-wrap'ed."
  (setq popper-wrap-advised-functions
        (cons function
              (delq function
                    popper-wrap-advised-functions))))

(defmacro popper-wrap (function buffer)
  "Define a wrapper on FUNCTION so that BUFFER will be a pop up window."
  (let ((name (intern (concat "popper-wrapper:" (symbol-name function)))))
    `(progn
       (popper-wrap-register ',function)
       (defadvice ,function (around ,name activate)
         (let ((shown nil))
           (save-window-excursion 
             ad-do-it
             (setq shown (get-buffer-window ,buffer)))
           (if shown
               (funcall temp-buffer-show-function
                        ,buffer)))))))

(defun popper-install-popper-wrap ()
  ;;
  ;; Don't do this stuff until popper-install() is called
  ;;
  (require 'advice)
  (ad-start-advice)

  (popper-wrap shell-command           "*Shell Command Output*")
  (popper-wrap shell-command-on-region "*Shell Command Output*")
  )

(defun popper-unstall-popper-wrap ()
  ;;
  ;; unadvise any popper-wrapped functions
  ;;
  (let ((funcs popper-wrap-advised-functions))
    (while funcs
      (let ((name (intern (concat "popper-wrapper:"
                                  (symbol-name (car funcs))))))
        (ad-remove-advice (car funcs) 'around name)
        (ad-update (car funcs))
        (setq funcs (cdr funcs)))))
  (setq popper-wrap-advised-functions nil))


;;;
;;; Define installation / de-installation functions
;;;

(defun popper-build-keymap ()
  "Create the popper keymap if necessary and define default bindings."
  (unless popper-map
    (setq popper-map (make-sparse-keymap "popper-map")))

  ;; define standard bindings
  (define-key popper-map "1"       'popper-bury-output)
  (define-key popper-map "z"       'popper-bury-output)
  (define-key popper-map "\C-z"    'popper-bury-output)

  (define-key popper-map "v"       'popper-scroll-output)
  (define-key popper-map "\C-v"    'popper-scroll-output)
  (define-key popper-map 'down     'popper-scroll-output)

  (define-key popper-map "V"       'popper-scroll-output-back)
  (define-key popper-map "\M-v"    'popper-scroll-output-back)
  (define-key popper-map 'up       'popper-scroll-output-back)

  (define-key popper-map "<"       'popper-beginning-of-buffer)
  (define-key popper-map ">"       'popper-end-of-buffer)

  (define-key popper-map "j"       'popper-jump-to-popper)
  (define-key popper-map "\C-j"    'popper-jump-to-popper)
  (define-key popper-map "o"       'popper-jump-to-popper)
  (define-key popper-map "\C-o"    'popper-jump-to-popper)

  (define-key popper-map "\C-s"    'popper-isearch-forward)
  (define-key popper-map "\C-r"    'popper-isearch-backward)
  (define-key popper-map "\M-\C-s" 'popper-isearch-forward-regexp)
  (define-key popper-map "\M-\C-r" 'popper-isearch-backward-regexp)

  (define-key popper-map "g"       'popper-grow-output)
  (define-key popper-map "G"       'popper-shrink-output)
  (define-key popper-map "\M-g"    'popper-shrink-output)

  (define-key popper-map "b"       'popper-switch-buffer)
  (define-key popper-map "\C-b"    'popper-switch-buffer)

  (define-key popper-map "k"       'popper-kill)
  (define-key popper-map "\C-k"    'popper-kill)

  (define-key popper-map "x"       'popper-cycle)
  (define-key popper-map "\C-x"    'popper-cycle)

  (define-key popper-map "s"       'popper-save))

;; define installer function and autoload it
;;;###autoload
(defun popper-install ()
  "Install popper into Emacs."
  (interactive) 

  (if popper-installed-p
      (error "Popper is already installed.")

    ;; save original temp-buffer-show-function
    ;; but avoid saving popper's show function...
    (when (and (null popper-original-show-function)
               (not (eq temp-buffer-show-function
                        'popper-show)))
      (setq popper-original-show-function
            temp-buffer-show-function))

    ;; install temp-buffer-show-function
    (setq temp-buffer-show-function 'popper-show)

    ;; set default key bindings unless already specified by user
    (unless popper-map
      ;; make the keymap.  user can override completely by making
      ;; their own map from the load hook
      (popper-build-keymap))

    ;; install the popper-map, saving original binding
    (when popper-prefix
      (setq popper-prefix-original-binding
            (lookup-key global-map popper-prefix))
      (define-key global-map popper-prefix popper-map))

    ;; replace bindings for `other-window' with `popper-other-window'
    (substitute-key-definition 'other-window
                               'popper-other-window
                               global-map)

    ;; replace bindings for `delete-window' with `popper-delete-window'
    (substitute-key-definition 'delete-window
                               'popper-delete-window
                               global-map)

    ;; install modeline indication for popper
    (unless (assq 'popper-buffer minor-mode-alist)
      (setq minor-mode-alist
            (cons '(popper-buffer popper-mode-line-text) minor-mode-alist)))

    ;; set popper modeline text unless already specified by user
    ;; do this *after* keys get set...
    (unless popper-mode-line-text
      (setq popper-mode-line-text
            (list (format 
                   " %s bury, %s scroll" 
                   (key-description
                    (where-is-internal 'popper-bury-output nil t))
                   (key-description
                    (where-is-internal 'popper-scroll-output nil t))))))

    ;; redefine `split-window' and `pop-to-buffer'
    (popper-install-redefinitions)

    ;; install popper-wrap
    (popper-install-popper-wrap)

    ;; warn about possible trouble...
    (mapc (lambda (pair)
            (when (boundp (car pair))
              (unless (eq (eval (car pair)) (cdr pair))
                (unless popper-inhibit-warnings
                  (popper-warn "Popper suggests setting variable `%s' to %s.%s"
                               (car pair) (cdr pair)
                               (if (and (symbolp (car pair))
                                        popper-fix-incompatible-settings)
                                   " Did it."
                                 "")))
                (if (and (symbolp (car pair))
                         popper-fix-incompatible-settings)
                    (set (car pair) (cdr pair))))))
          popper-suggested-settings)
    (setq popper-inhibit-warnings t)

    ;; remember that we're installed
    (setq popper-installed-p t)
    (message "Installed popper."))

  ) ; end of popper-install()

(defun popper-unstall ()
  "Un-install popper from Emacs."
  (interactive)

  (if (not popper-installed-p)
      (error "Popper isn't installed.")

    ;; uninstall temp-buffer-show-function
    (setq temp-buffer-show-function nil)

    ;; try to restore original temp-buffer-show-function,
    ;; but avoid restoring popper's show function...
    (when (and popper-original-show-function
               (not (eq popper-original-show-function
                        'popper-show)))
      (setq temp-buffer-show-function
            popper-original-show-function))
    (setq popper-original-show-function nil)

    ;; uninstall the popper-map
    (when popper-prefix
      (define-key global-map popper-prefix popper-prefix-original-binding))

    ;; restore bindings for `other-window'
    (substitute-key-definition 'popper-other-window
                               'other-window
                               global-map)

    ;; restore bindings for `delete-window'
    (substitute-key-definition 'popper-delete-window
                               'delete-window
                               global-map)

    ;; redefinitions
    (popper-unstall-redefinitions)

    ;; popper-wrap
    (popper-unstall-popper-wrap)

    ;; not installed
    (setq popper-installed-p nil)
    (message "Un-installed popper."))

  ) ; end of popper-unstall()


;;;
;;; Installation actions
;;;

;; run our load-hook
(run-hooks 'popper-load-hook)

;; if user requested it, install popper now
(when popper-install-when-load
  (popper-install))


;;;
;;; Finally, provide the feature
;;;

(provide 'popper)


;;; popper.el ends here
