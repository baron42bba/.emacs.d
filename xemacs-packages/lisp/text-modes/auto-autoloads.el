;;; DO NOT MODIFY THIS FILE
(if (featurep 'text-modes-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "text-modes/_pkg.el")

(package-provide 'text-modes :version 1.9 :author-version "No-Upstream-Ver" :type 'single-file)

;;;***

;;;### (autoloads (ansi-color-process-output ansi-color-for-comint-mode-on) "ansi-color" "text-modes/ansi-color.el")

(autoload 'ansi-color-for-comint-mode-on "ansi-color" "\
Set `ansi-color-for-comint-mode' to t." t nil)

(autoload 'ansi-color-process-output "ansi-color" "\
Maybe translate SGR control sequences of comint output into text-properties.

Depending on variable `ansi-color-for-comint-mode' the comint output is
either not processed, SGR control sequences are filtered using
`ansi-color-filter-region', or SGR control sequences are translated into
text-properties using `ansi-color-apply-on-region'.

The comint output is assumed to lie between the marker
`comint-last-output-start' and the process-mark.

This is a good function to put in `comint-output-filter-functions'." nil nil)

;;;***

;;;### (autoloads (apache-mode) "apache-mode" "text-modes/apache-mode.el")

(defgroup apache nil "Major mode for editing Apache configuration files." :prefix "apache-" :group 'languages)

(defcustom apache-file-patterns (list "\\.htaccess\\(\\.default\\)?$" "httpd\\.conf\\(\\.default\\)?$" "srm\\.conf\\(\\.default\\)?$" "access\\.conf\\(\\.default\\)?$") "*List of file patterns for which to automatically invoke `apache-mode'." :type '(repeat (regexp :tag "Pattern")) :group 'apache)

(let ((apache-file-patterns-temp apache-file-patterns)) (while apache-file-patterns-temp (add-to-list 'auto-mode-alist (cons (car apache-file-patterns-temp) 'apache-mode)) (setq apache-file-patterns-temp (cdr apache-file-patterns-temp))))

(autoload 'apache-mode "apache-mode" "\
Major mode for editing Apache configuration files.

\\{apache-mode-map}

\\[apache-mode] runs the hook `apache-mode-hook'." t nil)

;;;***

;;;### (autoloads (define-auto-insert auto-insert) "autoinsert" "text-modes/autoinsert.el")

(autoload 'auto-insert "autoinsert" "\
Insert default contents into a new file if `auto-insert' is non-nil.
Matches the visited file name against the elements of `auto-insert-alist'." t nil)

(autoload 'define-auto-insert "autoinsert" "\
Associate CONDITION with (additional) ACTION in `auto-insert-alist'.
Optional AFTER means to insert action after all existing actions for CONDITION,
or if CONDITION had no actions, after all other CONDITIONs." nil nil)

;;;***

;;;### (autoloads (css-mode) "css-mode" "text-modes/css-mode.el")

(autoload 'css-mode "css-mode" "\
Major mode for editing CSS style sheets.
\\{cssm-mode-map}" t nil)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))

;;;***

;;;### (autoloads (desktop-entry-mode) "desktop-entry-mode" "text-modes/desktop-entry-mode.el")

(autoload 'desktop-entry-mode "desktop-entry-mode" "\
Major mode for editing freedesktop.org desktop entry files.
See <http://www.freedesktop.org/Standards/desktop-entry-spec> for more
information.  See `desktop-entry-mode-version' for information about which
version of the specification this mode is up to date with.

Turning on desktop entry mode calls the value of the variable
`desktop-entry-mode-hook' with no args, if that value is non-nil." t nil)
(add-to-list 'auto-mode-alist '("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode))

;;;***

;;;### (autoloads (flyspell-buffer flyspell-region flyspell-mode-off flyspell-version flyspell-mode flyspell-prog-mode) "flyspell" "text-modes/flyspell.el")

(defcustom flyspell-mode-line-string " Fly" "*String displayed on the modeline when flyspell is active.\nSet this to nil if you don't want a modeline indicator." :group 'flyspell :type '(choice string (const :tag "None" nil)))

(autoload 'flyspell-prog-mode "flyspell" "\
Turn on `flyspell-mode' for comments and strings." t nil)

(defvar flyspell-mode nil)

(defvar flyspell-mode-map (make-sparse-keymap))

(autoload 'flyspell-mode "flyspell" "\
Minor mode performing on-the-fly spelling checking.
Ispell is automatically spawned on background for each entered words.
The default flyspell behavior is to highlight incorrect words.
With no argument, this command toggles Flyspell mode.
With a prefix argument ARG, turn Flyspell minor mode on iff ARG is positive.
  
Bindings:
\\[ispell-word]: correct words (using Ispell).
\\[flyspell-auto-correct-word]: automatically correct word.
\\[flyspell-auto-correct-previous-word]: automatically correct the last misspelled word.
\\[flyspell-correct-word] (or down-mouse-2): popup correct words.

Hooks:
This runs `flyspell-mode-hook' after flyspell is entered.

Remark:
`flyspell-mode' uses `ispell-mode'.  Thus all Ispell options are
valid.  For instance, a personal dictionary can be used by
invoking `ispell-change-dictionary'.

Consider using the `ispell-parser' to check your text.  For instance
consider adding:
\(add-hook 'tex-mode-hook (function (lambda () (setq ispell-parser 'tex))))
in your .emacs file.

\\[flyspell-region] checks all words inside a region.
\\[flyspell-buffer] checks the whole buffer." t nil)

(if (fboundp 'add-minor-mode) (add-minor-mode 'flyspell-mode 'flyspell-mode-line-string flyspell-mode-map nil 'flyspell-mode) (or (assoc 'flyspell-mode minor-mode-alist) (setq minor-mode-alist (cons '(flyspell-mode flyspell-mode-line-string) minor-mode-alist))) (or (assoc 'flyspell-mode minor-mode-map-alist) (setq minor-mode-map-alist (cons (cons 'flyspell-mode flyspell-mode-map) minor-mode-map-alist))))

(autoload 'flyspell-version "flyspell" "\
The flyspell version" t nil)

(autoload 'flyspell-mode-off "flyspell" "\
Turn Flyspell mode off." nil nil)

(autoload 'flyspell-region "flyspell" "\
Flyspell text between BEG and END." t nil)

(autoload 'flyspell-buffer "flyspell" "\
Flyspell whole buffer." t nil)

;;;***

;;;### (autoloads (folding-mode turn-on-folding-mode turn-off-folding-mode folding-mode-add-find-file-hook folding-keep-hooked folding-install-hooks folding-uninstall-hooks) "folding" "text-modes/folding.el")

(defvar folding-mode nil "\
When Non nil, Folding mode is active in the current buffer.")

(defcustom folding-default-keys-function 'folding-bind-default-keys "*Function or list of functions used to define keys for Folding mode.\nPossible values are:\n  folding-bind-default-key\n	The standard keymap.\n\n  `folding-bind-backward-compatible-keys'\n	Keys used by older versions of Folding mode. This function\n	does not conform to Emacs 19.29 style conversions concerning\n	key bindings. The prefix key is C - c\n\n  `folding-bind-outline-compatible-keys'\n	Define keys compatible with Outline mode.\n\n  `folding-bind-foldout-compatible-keys'\n	Define some extra keys compatible with Foldout.\n\nAll except `folding-bind-backward-compatible-keys' used the value of\nthe variable `folding-mode-prefix-key' as prefix the key.\nThe default is C - c @" :type 'function :group 'folding)

(defcustom folding-default-mouse-keys-function 'folding-bind-default-mouse "*Function to bind default mouse keys to `folding-mode-map'." :type 'function :group 'folding)

(defcustom folding-inside-mode-name "Fld" "*Mode line addition to show inside levels of 'fold' ." :type 'string :group 'folding)

(defcustom folding-mode-string "Fld" "*The minor mode string displayed when mode is on." :type 'string :group 'folding)

(defcustom folding-mode-hook-no-regexp "RMAIL" "*Regexp which disable automatic folding mode turn on for certain files." :type 'string :group 'folding)

(defvar folding-mode-marks-alist nil "\
List of (major-mode . fold mark) default combinations to use.
When Folding mode is started, the major mode is checked, and if there
are fold marks for that major mode stored in `folding-mode-marks-alist',
those marks are used by default. If none are found, the default values
of \"{{{ \" and \"}}}\" are used.

Use function  `folding-add-to-marks-list' to add more fold marks. The function
also explains the alist use in details.

Use function `folding-set-local-variables' if you change the current mode's
folding marks during the session.")

(autoload 'folding-uninstall-hooks "folding" "\
Remove hooks set by folding." nil nil)

(autoload 'folding-install-hooks "folding" "\
Install folding hooks." nil nil)

(autoload 'folding-keep-hooked "folding" "\
Make sure hooks are in their places." nil nil)

(autoload 'folding-mode-add-find-file-hook "folding" "\
Append `folding-mode-find-file-hook' to the list `find-file-hooks'.

This has the effect that afterwards, when a folded file is visited, if
appropriate Emacs local variable entries are recognized at the end of
the file, Folding mode is started automatically.

If `inhibit-local-variables' is non-nil, this will not happen regardless
of the setting of `find-file-hooks'.

To declare a file to be folded, put `folded-file: t' in the file's
local variables. eg., at the end of a C source file, put:

/*
Local variables:
folded-file: t
*/

The local variables can be inside a fold." t nil)

(autoload 'turn-off-folding-mode "folding" "\
Turn off folding." nil nil)

(autoload 'turn-on-folding-mode "folding" "\
Turn on folding." nil nil)

(autoload 'folding-mode "folding" "\
A folding-editor-like minor mode. ARG INTER.

These are the basic commands that Folding mode provides:

\\{folding-mode-map}

Keys starting with `folding-mode-prefix-key'

\\{folding-mode-prefix-map}

     folding-convert-buffer-for-printing:
     `\\[folding-convert-buffer-for-printing]'
     Makes a ready-to-print, formatted, unfolded copy in another buffer.

     Read the documentation for the above functions for more information.

Overview

    Folds are a way of hierarchically organizing the text in a file, so
    that the text can be viewed and edited at different levels. It is
    similar to Outline mode in that parts of the text can be hidden from
    view. A fold is a region of text, surrounded by special \"fold marks\",
    which act like brackets, grouping the text. Fold mark pairs can be
    nested, and they can have titles. When a fold is folded, the text is
    hidden from view, except for the first line, which acts like a title
    for the fold.

    Folding mode is a minor mode, designed to cooperate with many other
    major modes, so that many types of text can be folded while they are
    being edited (eg., plain text, program source code, Texinfo, etc.).

Folding-mode function

    If Folding mode is not called interactively (`(interactive-p)' is nil),
    and it is called with two or less arguments, all of which are nil, then
    the point will not be altered if `folding-folding-on-startup' is set
    and `folding-whole-buffer' is called. This is generally not a good
    thing, as it can leave the point inside a hidden region of a fold, but
    it is required if the local variables set \"mode: folding\" when the
    file is first read (see `hack-local-variables').

    Not that you should ever want to, but to call Folding mode from a
    program with the default behavior (toggling the mode), call it with
    something like `(folding-mode nil t)'.

Fold marks

    For most types of folded file, lines representing folds have \"{{{\"
    near the beginning. To enter a fold, move the point to the folded line
    and type `\\[folding-shift-in]'. You should no longer be able to see
    the rest of the file, just the contents of the fold, which you couldn't
    see before. You can use `\\[folding-shift-out]' to leave a fold, and
    you can enter and exit folds to move around the structure of the file.

    All of the text is present in a folded file all of the time. It is just
    hidden. Folded text shows up as a line (the top fold mark) with \"...\"
    at the end. If you are in a fold, the mode line displays \"inside n
    folds Narrow\", and because the buffer is narrowed you can't see outside
    of the current fold's text.

    By arranging sections of a large file in folds, and maybe subsections
    in sub-folds, you can move around a file quickly and easily, and only
    have to scroll through a couple of pages at a time. If you pick the
    titles for the folds carefully, they can be a useful form of
    documentation, and make moving though the file a lot easier. In
    general, searching through a folded file for a particular item is much
    easier than without folds.

Managing folds

    To make a new fold, set the mark at one end of the text you want in the
    new fold, and move the point to the other end. Then type
    `\\[folding-fold-region]'. The text you selected will be made into a
    fold, and the fold will be entered. If you just want a new, empty fold,
    set the mark where you want the fold, and then create a new fold there
    without moving the point. Don't worry if the point is in the middle of
    a line of text, `folding-fold-region' will not break text in the middle
    of a line. After making a fold, the fold is entered and the point is
    positioned ready to enter a title for the fold. Do not delete the fold
    marks, which are usually something like \"{{{\" and \"}}}\". There may
    also be a bit of fold mark which goes after the fold title.

    If the fold markers get messed up, or you just want to see the whole
    unfolded file, use `\\[folding-open-buffer]' to unfolded the whole
    file, so you can see all the text and all the marks. This is useful for
    checking/correcting unbalanced fold markers, and for searching for
    things. Use `\\[folding-whole-file]' to fold the buffer again.

    `folding-shift-out' will attempt to tidy the current fold just before
    exiting it. It will remove any extra blank lines at the top and bottom,
    (outside the fold marks). It will then ensure that fold marks exists,
    and if they are not, will add them (after asking). Finally, the number
    of blank lines between the fold marks and the contents of the fold is
    set to 1 (by default).

Folding package customizations

    If the fold marks are not set on entry to Folding mode, they are set to
    a default for current major mode, as defined by
    `folding-mode-marks-alist' or to \"{{{ \" and \"}}}\" if none are
    specified.

    To bind different commands to keys in Folding mode, set the bindings in
    the keymap `folding-mode-map'.

    The hooks `folding-mode-hook' and `<major-mode-name>-folding-hook' are
    called before folding the buffer and applying the key bindings in
    `folding-mode-map'. This is a good hook to set extra or different key
    bindings in `folding-mode-map'. Note that key bindings in
    `folding-mode-map' are only examined just after calling these hooks;
    new bindings in those maps only take effect when Folding mode is being
    started. The hook `folding-load-hook' is called when Folding mode is
    loaded into Emacs.

Mouse behavior

    If you want folding to detect point of actual mouse click, please see
    variable `folding-mouse-yank-at-p'.

    To customise the mouse actions, look at `folding-behave-table'." t nil)

;;;***

;;;### (autoloads (hexlify-buffer hexl-find-file hexl-mode) "hexl" "text-modes/hexl.el")

(autoload 'hexl-mode "hexl" "\
\\<hexl-mode-map>A mode for editing binary files in hex dump format.
This is not an ordinary major mode; it alters some aspects
if the current mode's behavior, but not all; also, you can exit
Hexl mode and return to the previous mode using `hexl-mode-exit'.

This function automatically converts a buffer into the hexl format
using the function `hexlify-buffer'.

Each line in the buffer has an \"address\" (displayed in hexadecimal)
representing the offset into the file that the characters on this line
are at and 16 characters from the file (displayed as hexadecimal
values grouped every 16 bits) and as their ASCII values.

If any of the characters (displayed as ASCII characters) are
unprintable (control or meta characters) they will be replaced as
periods.

If `hexl-mode' is invoked with an argument the buffer is assumed to be
in hexl format.

A sample format:

  HEX ADDR: 0001 0203 0405 0607 0809 0a0b 0c0d 0e0f     ASCII-TEXT
  --------  ---- ---- ---- ---- ---- ---- ---- ----  ----------------
  00000000: 5468 6973 2069 7320 6865 786c 2d6d 6f64  This is hexl-mod
  00000010: 652e 2020 4561 6368 206c 696e 6520 7265  e.  Each line re
  00000020: 7072 6573 656e 7473 2031 3620 6279 7465  presents 16 byte
  00000030: 7320 6173 2068 6578 6164 6563 696d 616c  s as hexadecimal
  00000040: 2041 5343 4949 0a61 6e64 2070 7269 6e74   ASCII.and print
  00000050: 6162 6c65 2041 5343 4949 2063 6861 7261  able ASCII chara
  00000060: 6374 6572 732e 2020 416e 7920 636f 6e74  cters.  Any cont
  00000070: 726f 6c20 6f72 206e 6f6e 2d41 5343 4949  rol or non-ASCII
  00000080: 2063 6861 7261 6374 6572 730a 6172 6520   characters.are 
  00000090: 6469 7370 6c61 7965 6420 6173 2070 6572  displayed as per
  000000a0: 696f 6473 2069 6e20 7468 6520 7072 696e  iods in the prin
  000000b0: 7461 626c 6520 6368 6172 6163 7465 7220  table character 
  000000c0: 7265 6769 6f6e 2e0a                      region..

Movement is as simple as movement in a normal emacs text buffer.  Most
cursor movement bindings are the same (ie. Use \\[hexl-backward-char], \\[hexl-forward-char], \\[hexl-next-line], and \\[hexl-previous-line]
to move the cursor left, right, down, and up).

Advanced cursor movement commands (ala \\[hexl-beginning-of-line], \\[hexl-end-of-line], \\[hexl-beginning-of-buffer], and \\[hexl-end-of-buffer]) are
also supported.

There are several ways to change text in hexl mode:

ASCII characters (character between space (0x20) and tilde (0x7E)) are
bound to self-insert so you can simply type the character and it will
insert itself (actually overstrike) into the buffer.

\\[hexl-quoted-insert] followed by another keystroke allows you to insert the key even if
it isn't bound to self-insert.  An octal number can be supplied in place
of another key to insert the octal number's ASCII representation.

\\[hexl-insert-hex-char] will insert a given hexadecimal value (if it is between 0 and 0xFF)
into the buffer at the current point.

\\[hexl-insert-octal-char] will insert a given octal value (if it is between 0 and 0377)
into the buffer at the current point.

\\[hexl-insert-decimal-char] will insert a given decimal value (if it is between 0 and 255)
into the buffer at the current point.

\\[hexl-mode-exit] will exit hexl-mode.

Note: saving the file with any of the usual Emacs commands
will actually convert it back to binary format while saving.

You can use \\[hexl-find-file] to visit a file in Hexl mode.

\\[describe-bindings] for advanced commands." t nil)

(autoload 'hexl-find-file "hexl" "\
Edit file FILENAME in hexl-mode.
Switch to a buffer visiting file FILENAME, creating one if none exists." t nil)

(autoload 'hexlify-buffer "hexl" "\
Convert a binary buffer to hexl format.
This discards the buffer's undo information." t nil)

;;;***

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file htmlize-region htmlize-buffer) "htmlize" "text-modes/htmlize.el")

(autoload 'htmlize-buffer "htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses." t nil)

(autoload 'htmlize-region "htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details." t nil)

(autoload 'htmlize-file "htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name." t nil)

(autoload 'htmlize-many-files "htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file." t nil)

(autoload 'htmlize-many-files-dired "htmlize" "\
HTMLize dired-marked files." t nil)

;;;***

;;;### (autoloads (image-mode) "image-mode" "text-modes/image-mode.el")

(defvar image-formats-alist '(("png" . png) ("gif" . gif) ("jpe?g" . jpeg) ("tiff?" . tiff) ("xbm" . xbm) ("xpm" . xpm) ("bmp" . bmp)))

(autoload 'image-mode "image-mode" "\
\\{image-mode-map}" t nil)

(progn (setq format-alist (remove-if (lambda (x) (eq (nth 6 x) 'image-mode)) format-alist)) (dolist (format image-formats-alist) (let* ((re (car format)) (type (cdr format)) (regexp (concat "\\.\\(" re "\\|" (upcase re) "\\)\\'")) (item (cons regexp 'image-mode))) (and (featurep type) (add-to-list 'auto-mode-alist item)))))

;;;***

;;;### (autoloads (iso-accents-mode) "iso-acc" "text-modes/iso-acc.el")

(autoload 'iso-accents-mode "iso-acc" "\
Toggle ISO Accents mode, in which accents modify the following letter.
This permits easy insertion of accented characters according to ISO-8859-1.
When Iso-accents mode is enabled, accent character keys
\(`, ', \", ^, / and ~) do not self-insert; instead, they modify the following
letter key so that it inserts an ISO accented letter.

You can customize ISO Accents mode to a particular language
with the command `iso-accents-customize'.

Special combinations: ~c gives a c with cedilla,
~d gives an Icelandic eth (d with dash).
~t gives an Icelandic thorn.
\"s gives German sharp s.
/a gives a with ring.
/e gives an a-e ligature.
~< and ~> give guillemots.
~! gives an inverted exclamation mark.
~? gives an inverted question mark.

With an argument, a positive argument enables ISO Accents mode, 
and a negative argument disables it." t nil)

;;;***

;;;### (autoloads (electric-nroff-mode nroff-mode) "nroff-mode" "text-modes/nroff-mode.el")

(autoload 'nroff-mode "nroff-mode" "\
Major mode for editing text intended for nroff to format.
\\{nroff-mode-map}
Turning on Nroff mode runs `text-mode-hook', then `nroff-mode-hook'.
Also, try `nroff-electric-mode', for automatically inserting
closing requests for requests that are used in matched pairs." t nil)

(autoload 'electric-nroff-mode "nroff-mode" "\
Toggle `nroff-electric-newline' minor mode.
`nroff-electric-newline' forces Emacs to check for an nroff request at the
beginning of the line, and insert the matching closing request if necessary.
This command toggles that mode (off->on, on->off), with an argument,
turns it on iff arg is positive, otherwise off." t nil)

(defvar nroff-electric-mode nil "\
Non-nil if in electric-nroff minor mode.")

(add-minor-mode 'nroff-electric-mode " Electric" nil nil 'electric-nroff-mode)
(add-to-list 'auto-mode-alist '("\\.m\\(?:[mes]\\|an\\)\\'" . nroff-mode))
(setq auto-mode-alist (append auto-mode-alist '(("\\.[123456789]\\'" . nroff-mode))))

;;;***

;;;### (autoloads (rtf-clip-region rtf-clip-buffer rtf-export-region rtf-export rtf-spool-buffer rtf-spool-region) "rtf-support" "text-modes/rtf-support.el")

(defgroup rtf nil "Support RTF selections and spooling of RTF to a buffer." :group 'wp :tag "RTF")

(autoload 'rtf-spool-region "rtf-support" "\
Spool a buffer as Microsoft Rich Text Format text.
Like `ps-spool-region', although the rtf-support code doesn't keep
track of spooled regions to despool (because RTF isn't useful for
printing). Returns the buffer containing the RTF." t nil)

(autoload 'rtf-spool-buffer "rtf-support" "\
Spool the entire buffer." t nil)

(autoload 'rtf-export "rtf-support" "\
Export the current document as RTF, preserving faces." t nil)

(autoload 'rtf-export-region "rtf-support" "\
Export the selected region as RTF, preserving faces." t nil)

(autoload 'rtf-clip-buffer "rtf-support" "\
Send the entire buffer to the clipboard as Rich Text Format. The function
also copies the buffer as ordinary text, just for consistency." t nil)

(autoload 'rtf-clip-region "rtf-support" "\
Send the specified region (the selection if called interactively) to the
clipboard as Rich Text Format. The function also copies the region in ordinary
text, just for consistency." t nil)

;;;***

;;;### (autoloads (scribe-mode) "scribe" "text-modes/scribe.el")

(autoload 'scribe-mode "scribe" "\
Major mode for editing files of Scribe (a text formatter) source.
Scribe-mode is similar text-mode, with a few extra commands added.
\\{scribe-mode-map}

Interesting variables:

scribe-fancy-paragraphs
  Non-nil makes Scribe mode use a different style of paragraph separation.

scribe-electric-quote
  Non-nil makes insert of double quote use `` or '' depending on context.

scribe-electric-parenthesis
  Non-nil makes an open-parenthesis char (one of `([<{')
  automatically insert its close if typed after an @Command form." t nil)
(add-to-list 'auto-mode-alist '("\\.mss\\'" . scribe-mode))

;;;***

;;;### (autoloads (tabify untabify) "tabify" "text-modes/tabify.el")

(autoload 'untabify "tabify" "\
Convert all tabs in region to multiple spaces, preserving columns.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops." t nil)

(autoload 'tabify "tabify" "\
Convert multiple spaces in region to tabs when possible.
A group of spaces is partially replaced by tabs
when this can be done without changing the column they end at.
Called non-interactively, the region is specified by arguments
START and END, rather than by the position of point and mark.
The variable `tab-width' controls the spacing of tab stops." t nil)

;;;***

;;;### (autoloads (tpum-global-mode tpum-minor-mode tpum-frame-popup-menu tpum-popup-menu) "tpum" "text-modes/tpum.el")

(autoload 'tpum-popup-menu "tpum" "\
Popup MENU in text mode.
EVENT is not used." nil nil)

(autoload 'tpum-frame-popup-menu "tpum" "\
Popup MENU in separate frame.
EVENT is some mouse event." nil nil)

(autoload 'tpum-minor-mode "tpum" "\
Toggle tpum minor mode.
With prefix ARG, turn on if positive, otherwise off." t nil)

(autoload 'tpum-global-mode "tpum" "\
Toggle tpum global mode.
With prefix ARG, turn on if positive, otherwise off." t nil)

;;;***

;;;### (autoloads (ununderline-and-unoverstrike-region overstrike-region unoverstrike-region ununderline-region underline-region) "underline" "text-modes/underline.el")

(autoload 'underline-region "underline" "\
Underline all nonblank characters in the region.
Works by overstriking underscores.
Called from program, takes two arguments START and END
which specify the range to operate on." t nil)

(autoload 'ununderline-region "underline" "\
Remove all underlining (overstruck underscores) in the region.
Called from program, takes two arguments START and END
which specify the range to operate on." t nil)

(autoload 'unoverstrike-region "underline" "\
Remove all overstriking (character-backspace-character) in the region.
Called from program, takes two arguments START and END which specify the
range to operate on." t nil)

(autoload 'overstrike-region "underline" "\
Overstrike (character-backspace-character) all nonblank characters in
the region. Called from program, takes two arguments START and END which
specify the range to operate on." t nil)

(autoload 'ununderline-and-unoverstrike-region "underline" "\
Remove underlining and overstriking in the region.  Called from a program,
takes two arguments START and END which specify the range to operate on." t nil)

;;;***

;;;### (autoloads (old-whitespace-incremental-mode old-whitespace-mode) "whitespace-mode" "text-modes/whitespace-mode.el")

(defcustom old-whitespace-mode-line-string " WSP" "*String displayed on the modeline when old-whitespace-mode is active.\nSet this to nil if you don't want a modeline indicator." :group 'old-whitespace :type 'string)

(defcustom old-whitespace-incremental-mode-line-string " WSPI" "*String displayed on the modeline when old-whitespace-incremental-mode\nis active. Set this to nil if you don't want a modeline indicator." :group 'old-whitespace :type 'string)

(autoload 'old-whitespace-mode "whitespace-mode" "\
Toggle whitespace mode.
With arg, turn whitespace mode on iff arg is positive.
In whitespace mode the different whitespaces (tab and blank)
are highlighted with different faces. The faces are:
`old-whitespace-blank-face' and `old-whitespace-tab-face'.
Use the command `old-whitespace-show-faces' to show their values." t nil)

(autoload 'old-whitespace-incremental-mode "whitespace-mode" "\
Toggle whitespace incremental mode.
With arg, turn whitespace incremental mode on iff arg is positive.
In whitespace incremental mode the different whitespaces (tab and blank)
are highlighted with different faces. The faces are:
`old-whitespace-blank-face' and `old-whitespace-tab-face'.
Use the command `old-whitespace-show-faces' to show their values.
In this mode only these tabs and blanks are highlighted, which are in 
the region from (point) - (window-height) to (point) + (window-height)." t nil)

(if (fboundp 'add-minor-mode) (progn (add-minor-mode 'old-whitespace-mode old-whitespace-mode-line-string) (add-minor-mode 'old-whitespace-incremental-mode old-whitespace-incremental-mode-line-string)) (or (assq 'old-whitespace-mode minor-mode-alist) (setq minor-mode-alist (cons '(old-whitespace-mode old-whitespace-mode-line-string) minor-mode-alist))) (or (assq 'old-whitespace-incremental-mode minor-mode-alist) (setq minor-mode-alist (cons '(old-whitespace-incremental-mode old-whitespace-incremental-mode-line-string) minor-mode-alist))))

;;;***

;;;### (autoloads (whitespace-visual-incremental-mode whitespace-visual-mode) "whitespace-visual-mode" "text-modes/whitespace-visual-mode.el")

(defcustom whitespace-visual-mode-line-string " WSP" "*String displayed on the modeline when whitespace-visual-mode is active.\nSet this to nil if you don't want a modeline indicator." :group 'whitespace-visual :type 'string)

(defcustom whitespace-visual-incremental-mode-line-string " WSPI" "*String displayed on the modeline when whitespace-visual-incremental-mode\nis active. Set this to nil if you don't want a modeline indicator." :group 'whitespace-visual :type 'string)

(autoload 'whitespace-visual-mode "whitespace-visual-mode" "\
Toggle whitespace visual mode.
With arg, turn whitespace visual mode on iff arg is positive.
In whitespace visual mode the different whitespaces (tab and blank)
are highlighted with different faces. The faces are:
`whitespace-visual-blank-face' and `whitespace-visual-tab-face'.
Use the command `whitespace-visual-show-faces' to show their values." t nil)

(autoload 'whitespace-visual-incremental-mode "whitespace-visual-mode" "\
Toggle whitespace visual incremental mode.
With arg, turn whitespace-visual incremental mode on iff arg is positive.
In whitespace visual incremental mode the different whitespaces (tab and
blank) are highlighted with different faces. The faces are:
`whitespace-visual-blank-face' and `whitespace-visual-tab-face'.
Use the command `whitespace-visual-show-faces' to show their values.
In this mode only these tabs and blanks are highlighted, which are in 
the region from (point) - (window-height) to (point) + (window-height)." t nil)

(if (fboundp 'add-minor-mode) (progn (add-minor-mode 'whitespace-visual-mode whitespace-visual-mode-line-string) (add-minor-mode 'whitespace-visual-incremental-mode whitespace-visual-incremental-mode-line-string)) (or (assq 'whitespace-visual-mode minor-mode-alist) (setq minor-mode-alist (cons '(whitespace-visual-mode whitespace-visual-mode-line-string) minor-mode-alist))) (or (assq 'whitespace-visual-incremental-mode minor-mode-alist) (setq minor-mode-alist (cons '(whitespace-visual-incremental-mode whitespace-visual-incremental-mode-line-string) minor-mode-alist))))

;;;***

;;;### (autoloads (whitespace-write-file-hook whitespace-cleanup-region whitespace-cleanup whitespace-region whitespace-buffer whitespace-toggle-ateol-check whitespace-toggle-spacetab-check whitespace-toggle-indent-check whitespace-toggle-trailing-check whitespace-toggle-leading-check) "whitespace" "text-modes/whitespace.el")

(autoload 'whitespace-toggle-leading-check "whitespace" "\
Toggle the check for leading space in the local buffer." t nil)

(autoload 'whitespace-toggle-trailing-check "whitespace" "\
Toggle the check for trailing space in the local buffer." t nil)

(autoload 'whitespace-toggle-indent-check "whitespace" "\
Toggle the check for indentation space in the local buffer." t nil)

(autoload 'whitespace-toggle-spacetab-check "whitespace" "\
Toggle the check for space-followed-by-TABs in the local buffer." t nil)

(autoload 'whitespace-toggle-ateol-check "whitespace" "\
Toggle the check for end-of-line space in the local buffer." t nil)

(autoload 'whitespace-buffer "whitespace" "\
Find five different types of white spaces in buffer.
These are:
1. Leading space (empty lines at the top of a file).
2. Trailing space (empty lines at the end of a file).
3. Indentation space (8 or more spaces, that should be replaced with TABS).
4. Spaces followed by a TAB. (Almost always, we never want that).
5. Spaces or TABS at the end of a line.

Check for whitespace only if this buffer really contains a non-empty file
and:
1. the major mode is one of the whitespace-modes, or
2. `whitespace-buffer' was explicitly called with a prefix argument." t nil)

(autoload 'whitespace-region "whitespace" "\
Check the region for whitespace errors." t nil)

(autoload 'whitespace-cleanup "whitespace" "\
Cleanup the five different kinds of whitespace problems.

Use \\[describe-function] whitespace-describe to read a summary of the
whitespace problems." t nil)

(autoload 'whitespace-cleanup-region "whitespace" "\
Whitespace cleanup on the region." t nil)
(autoload 'whitespace-global-mode "whitespace" nil t)

(autoload 'whitespace-write-file-hook "whitespace" "\
Hook function to be called on the buffer when whitespace check is enabled.
This is meant to be added buffer-locally to `write-file-functions'." t nil)

;;;***

;;;### (autoloads (winmgr-mode) "winmgr-mode" "text-modes/winmgr-mode.el")

(autoload 'winmgr-mode "winmgr-mode" "\
Major mode for editing winmgr config files." t nil)
(add-to-list 'auto-mode-alist '("\\.[^/]*wm2?\\(?:rc\\)?\\'" . winmgr-mode))

;;;***

;;;### (autoloads (wordstar-mode) "ws-mode" "text-modes/ws-mode.el")

(autoload 'wordstar-mode "ws-mode" "\
Major mode with WordStar-like key bindings.

BUGS:
 - Help menus with WordStar commands (C-j just calls help-for-help)
   are not implemented
 - Options for search and replace
 - Show markers (C-k h) is somewhat strange
 - Search and replace (C-q a) is only available in forward direction

No key bindings beginning with ESC are installed, they will work
Emacs-like.

The key bindings are:

  C-a		backward-word
  C-b		fill-paragraph
  C-c		scroll-up-line
  C-d		forward-char
  C-e		previous-line
  C-f		forward-word
  C-g		delete-char
  C-h		backward-char
  C-i		indent-for-tab-command
  C-j		help-for-help
  C-k		wordstar-C-k-map
  C-l		ws-repeat-search
  C-n		open-line
  C-p		quoted-insert
  C-r		scroll-down-line
  C-s		backward-char
  C-t		kill-word
  C-u		keyboard-quit
  C-v		overwrite-mode
  C-w		scroll-down
  C-x		next-line
  C-y		kill-complete-line
  C-z		scroll-up

  C-k 0		ws-set-marker-0
  C-k 1		ws-set-marker-1
  C-k 2		ws-set-marker-2
  C-k 3		ws-set-marker-3
  C-k 4		ws-set-marker-4
  C-k 5		ws-set-marker-5
  C-k 6		ws-set-marker-6
  C-k 7		ws-set-marker-7
  C-k 8		ws-set-marker-8
  C-k 9		ws-set-marker-9
  C-k b		ws-begin-block
  C-k c		ws-copy-block
  C-k d		save-buffers-kill-emacs
  C-k f		find-file
  C-k h		ws-show-markers
  C-k i		ws-indent-block
  C-k k		ws-end-block
  C-k p		ws-print-block
  C-k q		kill-emacs
  C-k r		insert-file
  C-k s		save-some-buffers
  C-k t		ws-mark-word
  C-k u		ws-exdent-block
  C-k C-u	keyboard-quit
  C-k v		ws-move-block
  C-k w		ws-write-block
  C-k x		kill-emacs
  C-k y		ws-delete-block

  C-o c		wordstar-center-line
  C-o b		switch-to-buffer
  C-o j		justify-current-line
  C-o k		kill-buffer
  C-o l		list-buffers
  C-o m		auto-fill-mode
  C-o r		set-fill-column
  C-o C-u	keyboard-quit
  C-o wd	delete-other-windows
  C-o wh	split-window-horizontally
  C-o wo	other-window
  C-o wv	split-window-vertically

  C-q 0		ws-find-marker-0
  C-q 1		ws-find-marker-1
  C-q 2		ws-find-marker-2
  C-q 3		ws-find-marker-3
  C-q 4		ws-find-marker-4
  C-q 5		ws-find-marker-5
  C-q 6		ws-find-marker-6
  C-q 7		ws-find-marker-7
  C-q 8		ws-find-marker-8
  C-q 9		ws-find-marker-9
  C-q a		ws-query-replace
  C-q b		ws-to-block-begin
  C-q c		end-of-buffer
  C-q d		end-of-line
  C-q f		ws-search
  C-q k		ws-to-block-end
  C-q l		ws-undo
  C-q p		ws-last-cursorp
  C-q r		beginning-of-buffer
  C-q C-u	keyboard-quit
  C-q w		ws-last-error
  C-q y		ws-kill-eol
  C-q DEL	ws-kill-bol
" t nil)

;;;***

;;;### (autoloads (xpm-mode) "xpm-mode" "text-modes/xpm-mode.el")
(add-to-list 'auto-mode-alist '("\\.xpm$" . xpm-mode))

(autoload 'xpm-mode "xpm-mode" "\
Treat the current buffer as an xpm file and colorize it.

  Shift-button-1 lets you paint by dragging the mouse.  Shift-button-1 on a
color definition line will change the current painting color to that line's
value.

  Characters inserted from the keyboard will NOT be colored properly yet.
Use the mouse, or do xpm-init (\\[xpm-init]) after making changes.

\\[xpm-add-color] Add a new color, prompting for character and value
\\[xpm-show-image] show the current image at the top of the buffer
\\[xpm-parse-color] parse the current line's color definition and add
   it to the color table.  Provided as a means of changing colors.
XPM minor mode bindings:
\\{xpm-mode-map}" t nil)

;;;***

;;;### (autoloads (xrdb-mode) "xrdb-mode" "text-modes/xrdb-mode.el")

(autoload 'xrdb-mode "xrdb-mode" "\
Major mode for editing xrdb config files" t nil)
(add-to-list 'auto-mode-alist '("[./\\]X\\(defaults\\|environment\\|resources\\|modmap\\)\\'" . xrdb-mode))
(add-to-list 'auto-mode-alist '("\\.ad\\'" . xrdb-mode))
(add-to-list 'auto-mode-alist '("/app-defaults/" . xrdb-mode))

;;;***

(provide 'text-modes-autoloads)
