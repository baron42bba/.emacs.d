;;; DO NOT MODIFY THIS FILE
(if (featurep 'mail-lib-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "mail-lib/_pkg.el")

(package-provide 'mail-lib :version 1.76 :author-version "No-Upstream-Ver" :type 'regular)

;;;***

;;;### (autoloads nil "browse-url-xemacs" "mail-lib/browse-url-xemacs.el")

(progn (defun browse-url-xemacs-init-menu nil (if (featurep 'menubar) (add-submenu '("Options" "Internet") browse-url-menu))))

;;;***

;;;### (autoloads (browse-url-opera browse-url-kde browse-url-generic browse-url-mail browse-url-mmm browse-url-lynx-emacs browse-url-lynx-xterm browse-url-w3m-gnudoit browse-url-w3m browse-url-w3-gnudoit browse-url-w3 browse-url-iximosaic browse-url-cci browse-url-grail browse-url-mosaic browse-url-gnome-moz browse-url-galeon browse-url-firefox browse-url-mozilla browse-url-netscape browse-url-default-browser browse-url-at-mouse browse-url-at-point browse-url browse-url-of-region browse-url-of-dired-file browse-url-of-buffer browse-url-of-file) "browse-url" "mail-lib/browse-url.el")

(defcustom browse-url-browser-function (cond ((memq system-type '(windows-nt ms-dos cygwin32)) 'browse-url-default-windows-browser) ((memq system-type '(darwin)) 'browse-url-default-macosx-browser) (t 'browse-url-default-browser)) "*Function to display the current buffer in a WWW browser.\nThis is used by the `browse-url-at-point', `browse-url-at-mouse', and\n`browse-url-of-file' commands.\n\nIf the value is not a function it should be a list of pairs\n(REGEXP . FUNCTION).  In this case the function called will be the one\nassociated with the first REGEXP which matches the current URL.  The\nfunction is passed the URL and any other args of `browse-url'.  The last\nregexp should probably be \".\" to specify a default browser." :type '(choice (function-item :tag "Emacs W3" :value browse-url-w3) (function-item :tag "W3 in another Emacs via `gnudoit'" :value browse-url-w3-gnudoit) (function-item :tag "Emacs W3M" :value browse-url-w3) (function-item :tag "W3M in another Emacs via `gnudoit'" :value browse-url-w3-gnudoit) (function-item :tag "Mozilla" :value browse-url-mozilla) (function-item :tag "Firefox" :value browse-url-firefox) (function-item :tag "Galeon" :value browse-url-galeon) (function-item :tag "Epiphany" :value browse-url-epiphany) (function-item :tag "Netscape" :value browse-url-netscape) (function-item :tag "Mosaic" :value browse-url-mosaic) (function-item :tag "Mosaic using CCI" :value browse-url-cci) (function-item :tag "IXI Mosaic" :value browse-url-iximosaic) (function-item :tag "Lynx in an xterm window" :value browse-url-lynx-xterm) (function-item :tag "Lynx in an Emacs window" :value browse-url-lynx-emacs) (function-item :tag "Grail" :value browse-url-grail) (function-item :tag "MMM" :value browse-url-mmm) (function-item :tag "KDE" :value browse-url-kde) (function-item :tag "Opera" :value browse-url-opera) (function-item :tag "Specified by `Browse Url Generic Program'" :value browse-url-generic) (function-item :tag "Default Windows browser" :value browse-url-default-windows-browser) (function-item :tag "Default Mac OS X browser" :value browse-url-default-macosx-browser) (function-item :tag "GNOME invoking Mozilla" :value browse-url-gnome-moz) (function-item :tag "Default browser" :value browse-url-default-browser) (function :tag "Your own function")) :group 'browse-url)

(defcustom browse-url-browser-display nil "*The X display for running the browser, if not same as Emacs'." :type '(choice string (const :tag "Default" nil)) :group 'browse-url)

(defcustom browse-url-mozilla-program "mozilla" "*The name by which to invoke Mozilla." :type 'string :group 'browse-url)

(defcustom browse-url-firefox-program "firefox" "*The name by which to invoke Firefox." :type 'string :group 'browse-url)

(defcustom browse-url-galeon-program "galeon" "*The name by which to invoke Galeon." :type 'string :group 'browse-url)

(defcustom browse-url-new-window-flag nil "*If non-nil, always open a new browser window with appropriate browsers.\nPassing an interactive argument to \\[browse-url], or specific browser\ncommands reverses the effect of this variable.  Requires Netscape version\n1.1N or later or XMosaic version 2.5 or later if using those browsers." :type 'boolean :group 'browse-url)

(defvaralias 'browse-url-new-window-p 'browse-url-new-window-flag)

(defcustom browse-url-save-file nil "*If non-nil, save the buffer before displaying its file.\nUsed by the `browse-url-of-file' command." :type 'boolean :group 'browse-url)

(defcustom browse-url-generic-program nil "*The name of the browser program used by `browse-url-generic'." :type '(choice string (const :tag "None" nil)) :group 'browse-url)

(autoload 'browse-url-of-file "browse-url" "\
Ask a WWW browser to display FILE.
Display the current buffer's file if FILE is nil or if called
interactively.  Turn the filename into a URL with function
`browse-url-file-url'.  Pass the URL to a browser using the
`browse-url' function then run `browse-url-of-file-hook'." t nil)

(autoload 'browse-url-of-buffer "browse-url" "\
Ask a WWW browser to display BUFFER.
Display the current buffer if BUFFER is nil.  Display only the
currently visible part of BUFFER (from a temporary file) if buffer is
narrowed." t nil)

(autoload 'browse-url-of-dired-file "browse-url" "\
In Dired, ask a WWW browser to display the file named on this line." t nil)

(autoload 'browse-url-of-region "browse-url" "\
Ask a WWW browser to display the current region." t nil)

(autoload 'browse-url "browse-url" "\
Ask a WWW browser to load URL.
Prompts for a URL, defaulting to the URL at or before point.  Variable
`browse-url-browser-function' says which browser to use." t nil)

(autoload 'browse-url-at-point "browse-url" "\
Ask a WWW browser to load the URL at or before point.
Doesn't let you edit the URL like `browse-url'.  Variable
`browse-url-browser-function' says which browser to use." t nil)

(autoload 'browse-url-at-mouse "browse-url" "\
Ask a WWW browser to load a URL clicked with the mouse.
The URL is the one around or before the position of the mouse click
but point is not changed.  Doesn't let you edit the URL like
`browse-url'.  Variable `browse-url-browser-function' says which browser
to use." t nil)

(autoload 'browse-url-default-browser "browse-url" "\
Find a suitable browser and ask it to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window, if possible, otherwise use
a random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'.

The order attempted is emacs-w3m, gnome-moz-remote, Mozilla, Firefox,
Galeon, Konqueror, Netscape, Opera, Mosaic, IXI Mosaic, Lynx in an
xterm, MMM, and then W3." nil nil)

(autoload 'browse-url-netscape "browse-url" "\
Ask the Netscape WWW browser to load URL.
Default to the URL around or before point.  The strings in variable
`browse-url-netscape-arguments' are also passed to Netscape.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Netscape window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-netscape-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'." t nil)

(autoload 'browse-url-mozilla "browse-url" "\
Ask the Mozilla WWW browser to load URL.
Default to the URL around or before point.  The strings in variable
`browse-url-mozilla-arguments' are also passed to Mozilla.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Mozilla window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-mozilla-new-window-is-tab' is non-nil, then whenever a
document would otherwise be loaded in a new window, it is loaded in a
new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'.

On MS-Windows systems the optional `new-window' parameter is ignored.
Mozilla for Windows does not support the \"-remote\" command line
parameter. Therefore the `browse-url-new-window-flag', 
`browse-url-new-window-flag' and `browse-url-mozilla-new-window-is-tab'
are ignored as well. Mozilla on Windows will always open the
requested URL in a new window." t nil)

(autoload 'browse-url-firefox "browse-url" "\
Ask the Firefox WWW browser to load URL.
Default to the URL around or before point.  The strings in variable
`browse-url-firefox-arguments' are also passed to Firefox.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Firefox window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-firefox-new-window-is-tab' is non-nil, then whenever a
document would otherwise be loaded in a new window, it is loaded in a
new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'.

On MS-Windows systems the optional `new-window' parameter is ignored.
Firefox for Windows does not support the \"-remote\" command line
parameter. Therefore the `browse-url-new-window-flag', 
`browse-url-new-window-flag' and `browse-url-firefox-new-window-is-tab'
are ignored as well. Firefox on Windows will always open the
requested URL in a new window." t nil)

(autoload 'browse-url-galeon "browse-url" "\
Ask the Galeon WWW browser to load URL.
Default to the URL around or before point.  The strings in variable
`browse-url-galeon-arguments' are also passed to Galeon.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Galeon window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-galeon-new-window-is-tab' is non-nil, then whenever a
document would otherwise be loaded in a new window, it is loaded in a
new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'." t nil)

(autoload 'browse-url-gnome-moz "browse-url" "\
Ask Mozilla/Netscape to load URL via the GNOME program `gnome-moz-remote'.
Default to the URL around or before point.  The strings in variable
`browse-url-gnome-moz-arguments' are also passed.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new browser window, otherwise use an
existing one.  A non-nil interactive prefix argument reverses the
effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'." t nil)

(autoload 'browse-url-mosaic "browse-url" "\
Ask the XMosaic WWW browser to load URL.

Default to the URL around or before point.  The strings in variable
`browse-url-mosaic-arguments' are also passed to Mosaic and the
program is invoked according to the variable
`browse-url-mosaic-program'.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Mosaic window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'." t nil)

(defvar browse-url-grail (concat (or (getenv "GRAILDIR") "~/.grail") "/user/rcgrail.py") "\
Location of Grail remote control client script `rcgrail.py'.
Typically found in $GRAILDIR/rcgrail.py, or ~/.grail/user/rcgrail.py.")

(autoload 'browse-url-grail "browse-url" "\
Ask the Grail WWW browser to load URL.
Default to the URL around or before point.  Runs the program in the
variable `browse-url-grail'." t nil)

(autoload 'browse-url-cci "browse-url" "\
Ask the XMosaic WWW browser to load URL.
Default to the URL around or before point.

This function only works for XMosaic version 2.5 or later.  You must
select `CCI' from XMosaic's File menu, set the CCI Port Address to the
value of variable `browse-url-CCI-port', and enable `Accept requests'.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new browser window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'." t nil)

(autoload 'browse-url-iximosaic "browse-url" "\
Ask the IXIMosaic WWW browser to load URL.
Default to the URL around or before point." t nil)

(autoload 'browse-url-w3 "browse-url" "\
Ask the w3 WWW browser to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window.  A non-nil interactive
prefix argument reverses the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'." t nil)

(autoload 'browse-url-w3-gnudoit "browse-url" "\
Ask another Emacs running gnuserv to load the URL using the W3 browser.
The `browse-url-gnudoit-program' program is used with options given by
`browse-url-gnudoit-args'.  Default to the URL around or before point." t nil)

(autoload 'browse-url-w3m "browse-url" "\
Ask the emacs-w3m WWW browser to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window.  A non-nil interactive
prefix argument reverses the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'." t nil)

(autoload 'browse-url-w3m-gnudoit "browse-url" "\
Ask the Emacs running gnuserv to load the URL using the emacs-w3m browser.
The `browse-url-gnudoit-program' program is used with options given by
`browse-url-gnudoit-args'.  Default to the URL around or before point." t nil)

(autoload 'browse-url-lynx-xterm "browse-url" "\
Ask the Lynx WWW browser to load URL.
Default to the URL around or before point.  A new Lynx process is run
in an Xterm window using the Xterm program named by `browse-url-xterm-program'
with possible additional arguments `browse-url-xterm-args'." t nil)

(autoload 'browse-url-lynx-emacs "browse-url" "\
Ask the Lynx WWW browser to load URL.
Default to the URL around or before point.  With a prefix argument, run
a new Lynx process in a new buffer.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new lynx in a new term window,
otherwise use any existing one.  A non-nil interactive prefix argument
reverses the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'." t nil)

(autoload 'browse-url-mmm "browse-url" "\
Ask the MMM WWW browser to load URL.
Default to the URL around or before point." t nil)

(autoload 'browse-url-mail "browse-url" "\
Open a new mail message buffer within Emacs.
Default to using the mailto: URL around or before point as the
recipient's address.  Supplying a non-nil interactive prefix argument
will cause the mail to be composed in another window rather than the
current one.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil use `compose-mail-other-window', otherwise `compose-mail'.  A
non-nil interactive prefix argument reverses the effect of
`browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'." t nil)

(autoload 'browse-url-generic "browse-url" "\
Ask the WWW browser defined by `browse-url-generic-program' to load URL.
Default to the URL around or before point.  A fresh copy of the
browser is started up in a new process with possible additional arguments
`browse-url-generic-args'.  This is appropriate for browsers which
don't offer a form of remote control." t nil)

(autoload 'browse-url-kde "browse-url" "\
Ask the KDE WWW browser to load URL.
Default to the URL around or before point." t nil)

(autoload 'browse-url-opera "browse-url" "\
Ask the Opera WWW browser to load URL.
Default to the URL around or before point.  The strings in variable
`browse-url-opera-arguments' are also passed to Opera.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Opera window, otherwise use an
existing one.  A non-nil interactive prefix argument reverses the
effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'." t nil)

;;;***

;;;### (autoloads (highlight-headers-follow-url highlight-headers) "highlight-headers" "mail-lib/highlight-headers.el")

(autoload 'highlight-headers "highlight-headers" "\
Highlight message headers between start and end.
Faces used:
  message-headers			the part before the colon
  message-header-contents		the part after the colon
  message-highlighted-header-contents	contents of \"special\" headers
  message-cited-text			quoted text from other messages
  message-url				URLs (WWW uniform resource locators)

Variables used:

  highlight-headers-regexp			what makes a \"special\" header
  highlight-headers-citation-regexp		matches lines of quoted text
  highlight-headers-citation-header-regexp	matches headers for quoted text

If HACK-SIG is true,then we search backward from END for something that
looks like the beginning of a signature block, and don't consider that a
part of the message (this is because signatures are often incorrectly
interpreted as cited text.)" nil nil)

(define-obsolete-function-alias 'highlight-headers-follow-url-netscape 'browse-url-netscape)

(define-obsolete-function-alias 'highlight-headers-follow-url-kfm 'browse-url-kde)

(define-obsolete-function-alias 'highlight-headers-follow-url-mosaic 'browse-url-mosaic)

(autoload 'highlight-headers-follow-url "highlight-headers" nil t nil)

;;;***

;;;### (autoloads (define-mail-alias build-mail-aliases mail-aliases-setup mail-abbrev-mailrc-file) "mail-abbrevs" "mail-lib/mail-abbrevs.el")

(defcustom mail-abbrev-mailrc-file nil "Name of file with mail aliases.   If nil, ~/.mailrc is used." :type '(choice (const :tag "Default" nil) file) :group 'mail-abbrevs)

(autoload 'mail-abbrev-mailrc-file "mail-abbrevs" nil nil nil)

(defvar mail-aliases nil "\
Word-abbrev table of mail address aliases.
If this is nil, it means the aliases have not yet been initialized and
should be read from the .mailrc file.  (This is distinct from there being
no aliases, which is represented by this being a table with no entries.)")

(autoload 'mail-aliases-setup "mail-abbrevs" nil nil nil)

(autoload 'build-mail-aliases "mail-abbrevs" "\
Read mail aliases from .mailrc and set mail-aliases." nil nil)

(autoload 'define-mail-alias "mail-abbrevs" "\
Define NAME as a mail-alias that translates to DEFINITION.
If DEFINITION contains multiple addresses, separate them with commas." t nil)

;;;***

;;;### (autoloads (what-domain mail-extract-address-components) "mail-extr" "mail-lib/mail-extr.el")

(autoload 'mail-extract-address-components "mail-extr" "\
Given an RFC-822 address ADDRESS, extract full name and canonical address.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
If no name can be extracted, FULL-NAME will be nil.  Also see
`mail-extr-ignore-single-names'.

If the optional argument ALL is non-nil, then ADDRESS can contain zero
or more recipients, separated by commas, and we return a list of
the form ((FULL-NAME CANONICAL-ADDRESS) ...) with one element for
each recipient.  If ALL is nil, then if ADDRESS contains more than
one recipients, all but the first is ignored.

ADDRESS may be a string or a buffer.  If it is a buffer, the visible
\(narrowed) portion of the buffer will be interpreted as the address.
\(This feature exists so that the clever caller might be able to avoid
consing a string.)" nil nil)

(autoload 'what-domain "mail-extr" "\
Convert mail domain DOMAIN to the country it corresponds to." t nil)

;;;***

;;;### (autoloads (mail-fetch-field mail-file-babyl-p) "mail-utils" "mail-lib/mail-utils.el")

(defcustom mail-use-rfc822 nil "*If non-nil, use a full, hairy RFC822 parser on mail addresses.\nOtherwise, (the default) use a smaller, somewhat faster, and\noften correct parser." :type 'boolean :group 'mail)

(autoload 'mail-file-babyl-p "mail-utils" nil nil nil)

(autoload 'mail-fetch-field "mail-utils" "\
Return the value of the header field whose type is FIELD-NAME.
The buffer is expected to be narrowed to just the header of the message.
If second arg LAST is non-nil, use the last field of type FIELD-NAME.
If third arg ALL is non-nil, concatenate all such fields with commas between.
If 4th arg LIST is non-nil, return a list of all such fields." nil nil)

;;;***

;;;### (autoloads (pop3-movemail pop3-nnmail-movemail) "pop3" "mail-lib/pop3.el")

(autoload 'pop3-nnmail-movemail "pop3" "\
Function to move mail from INBOX on a pop3 server to file CRASHBOX." nil nil)

(autoload 'pop3-movemail "pop3" "\
Transfer contents of a maildrop to the specified CRASHBOX." nil nil)

;;;***

;;;### (autoloads (reporter-submit-bug-report) "reporter" "mail-lib/reporter.el")

(autoload 'reporter-submit-bug-report "reporter" "\
Begin submitting a bug report via email.

ADDRESS is the email address for the package's maintainer.  PKGNAME is
the name of the package (if you want to include version numbers,
you must put them into PKGNAME before calling this function).
Optional PRE-HOOKS and POST-HOOKS are passed to `reporter-dump-state'.
Optional SALUTATION is inserted at the top of the mail buffer,
and point is left after the salutation.

VARLIST is the list of variables to dump (see `reporter-dump-state'
for details).  The optional argument PRE-HOOKS and POST-HOOKS are
passed to `reporter-dump-state'.  Optional argument SALUTATION is text
to be inserted at the top of the mail buffer; in that case, point is
left after that text.

This function prompts for a summary if `reporter-prompt-for-summary-p'
is non-nil.

This function does not send a message; it uses the given information
to initialize a message, which the user can then edit and finally send
\(or decline to send).  The variable `mail-user-agent' controls which
mail-sending package is used for editing and sending the message." nil nil)

;;;***

;;;### (autoloads nil "rmail-mini" "mail-lib/rmail-mini.el")

(defcustom rmail-dont-reply-to-names nil "*A regexp specifying names to prune of reply to messages.\nA value of nil means exclude your own name only." :type '(choice regexp (const :tag "Your Name" nil)) :group 'rmail-reply)

(defvar rmail-default-dont-reply-to-names "info-" "\
A regular expression specifying part of the value of the default value of
the variable `rmail-dont-reply-to-names', for when the user does not set
`rmail-dont-reply-to-names' explicitly.  (The other part of the default
value is the user's name.)
It is useful to set this variable in the site customization file.")

;;;***

;;;### (autoloads (rmail-file-p) "rmailout" "mail-lib/rmailout.el")

(autoload 'rmail-file-p "rmailout" nil nil nil)

;;;***

;;;### (autoloads (mail-other-frame mail-other-window mail mail-mode user-mail-address) "sendmail" "mail-lib/sendmail.el")

(defcustom mail-from-style 'angles "*Specifies how \"From:\" fields look.\n\nIf `nil', they contain just the return address like:\n	king@grassland.com\nIf `parens', they look like:\n	king@grassland.com (Elvis Parsley)\nIf `angles', they look like:\n	Elvis Parsley <king@grassland.com>\nIf `system-default', allows the mailer to insert its default From field\nderived from the envelope-from address.\n\nThe email address passed to the mailer to specify the envelope-from\naddress is controlled by `mail-specify-envelope-from'." :type '(choice (const nil) (const parens) (const angles) (const system-default)) :group 'sendmail)

(defcustom mail-specify-envelope-from nil "*If non-nil, specify the envelope-from address when sending mail.\nThe value used to specify it is whatever is found in\n`mail-envelope-from', with `user-mail-address' as fallback.\n\nOn most systems, specifying the envelope-from address\nis a privileged operation." :type 'boolean :group 'sendmail)

(defcustom mail-self-blind nil "*Non-nil means insert BCC to self in messages to be sent.\nThis is done when the message is initialized,\nso you can remove or alter the BCC field to override the default." :type 'boolean :group 'sendmail)

(defcustom mail-interactive nil "*Non-nil means when sending a message wait for and display errors.\nnil means let mailer mail back a message to report errors." :type 'boolean :group 'sendmail)

(defvar rmail-ignored-headers (purecopy (concat "^\\(" (mapconcat 'identity '("Sender:" "References:" "Return-Path:" "Received:" "[^: 	\n]*Message-ID:" "Errors-To:" "Path:" "Expires:" "Xref:" "Lines:" "Approved:" "Distribution:" "Content-Length:" "Mime-Version:" "Content-Type:" "Content-Transfer-Encoding:" "X400-Received:" "X400-Originator:" "X400-Mts-Identifier:" "X400-Content-Type:" "Content-Identifier:" "Status:" "Summary-Line:" "X-Attribution:" "Via:" "Sent-Via:" "Mail-From:" "Origin:" "Comments:" "Originator:" "NF-ID:" "NF-From:" "Posting-Version:" "Posted:" "Posted-Date:" "Date-Received:" "Relay-Version:" "Article-I\\.D\\.:" "NNTP-Version:" "NNTP-Posting-Host:" "X-Mailer:" "X-Newsreader:" "News-Software:" "X-Received:" "X-References:" "X-Envelope-To:" "X-VMS-" "Remailed-" "X-Plantation:" "X-Windows:" "X-Pgp-") "\\|") "\\)")) "\
*Gubbish header fields one would rather not see.")

(defcustom mail-yank-ignored-headers (purecopy (concat rmail-ignored-headers "\\|" "^\\(" (mapconcat 'identity '("Resent-To:" "Resent-By:" "Resent-CC:" "To:" "Subject:" "In-Reply-To:") "\\|") "\\)")) "*Delete these headers from old message when it's inserted in a reply." :type 'regexp :group 'sendmail)

(defcustom send-mail-function (if (or (eq system-type 'windows-nt) (eq system-type 'cygwin32)) 'smtpmail-send-it 'sendmail-send-it) "Function to call to send the current buffer as mail.\nThe headers should be delimited by a line which is\nnot a valid RFC822 header or continuation line,\nthat match the variable `mail-header-separator'.\nThis is used by the default mail-sending commands.  See also\n`message-send-mail-function' for use with the Message package." :type '(radio (function-item sendmail-send-it :tag "Use Sendmail package") (function-item smtpmail-send-it :tag "Use SMTPmail package") (function-item feedmail-send-it :tag "Use Feedmail package") function) :group 'sendmail)

(defcustom mail-header-separator "--text follows this line--" "*Line used to separate headers from text in messages being composed." :type 'string :group 'sendmail)

(defcustom mail-archive-file-name nil "*Name of file to write all outgoing messages in, or nil for none.\nThis can be an inbox file or an Rmail file." :type '(choice file (const nil)) :group 'sendmail)

(defcustom mail-default-reply-to nil "*Address to insert as default Reply-to field of outgoing messages.\nIf nil, it will be initialized from the REPLYTO environment variable\nwhen you first send mail." :type '(choice (const nil) string) :group 'sendmail)

(defcustom mail-alias-file nil "*If non-nil, the name of a file to use instead of `/usr/lib/aliases'.\nThis file defines aliases to be expanded by the mailer; this is a different\nfeature from that of defining aliases in `.mailrc' to be expanded in Emacs.\nThis variable has no effect unless your system uses sendmail as its mailer." :type '(choice (const nil) file) :group 'sendmail)

(defcustom mail-yank-prefix "> " "*Prefix insert on lines of yanked message being replied to.\nnil means use indentation." :type '(choice (const nil) string) :group 'sendmail)

(defcustom mail-signature nil "*Text inserted at end of mail buffer when a message is initialized.\nIf t, it means to insert the contents of the file `mail-signature-file'.\nIf a string, that string is inserted.\n (To make a proper signature, the string should begin with \\n\\n-- \\n,\n  which is the standard way to delimit a signature in a message.)\nOtherwise, it should be an expression; it is evaluated\nand should insert whatever you want to insert." :type '(choice (const :tag "None" nil) (const :tag "Use `.signature' file" t) (string :tag "String to insert") (sexp :tag "Expression to evaluate")) :group 'sendmail)

(defcustom mail-default-directory "~/" "*Directory for mail buffers.\nValue of `default-directory' for mail buffers.\nThis directory is used for auto-save files of mail buffers." :type '(directory :tag "Directory") :group 'sendmail :version "21.4")

(autoload 'user-mail-address "sendmail" "\
Query the user for his mail address, unless it is already known." t nil)

(autoload 'mail-mode "sendmail" "Major mode for editing mail to be sent.\nLike Text Mode but with these additional commands:\n\\[mail-send]  mail-send (send the message)    \\[mail-send-and-exit]  mail-send-and-exit\nHere are commands that move to a header field (and create it if there isn't):\n	 \\[mail-to]  move to To:	\\[mail-subject]  move to Subject:\n	 \\[mail-cc]  move to CC:	\\[mail-bcc]  move to BCC:\n	 \\[mail-fcc]  move to FCC:      \\[mail-reply-to] move to Reply-To:\n\\[mail-text]  mail-text (move to beginning of message text).\n\\[mail-signature]  mail-signature (insert `mail-signature-file' file).\n\\[mail-yank-original]  mail-yank-original (insert current message, in Rmail).\n\\[mail-fill-yanked-message]  mail-fill-yanked-message (fill what was yanked).\n\\[mail-sent-via]  mail-sent-via (add a Sent-via field for each To or CC).\nTurning on Mail mode runs the normal hooks `text-mode-hook' and\n`mail-mode-hook' (in that order)." t nil)
(add-hook 'same-window-buffer-names "*mail*")

(autoload 'mail "sendmail" "\
Edit a message to be sent.  Prefix arg means resume editing (don't erase).
When this function returns, the buffer `*mail*' is selected.
The value is t if the message was newly initialized; otherwise, nil.

Optionally, the signature file `mail-signature-file' can be inserted at the
end; see the variable `mail-signature'.

\\<mail-mode-map>
While editing message, type \\[mail-send-and-exit] to send the message and exit.

Various special commands starting with C-c are available in sendmail mode
to move to message header fields:
\\{mail-mode-map}

If `mail-self-blind' is non-nil, a BCC to yourself is inserted
when the message is initialized.

If `mail-default-reply-to' is non-nil, it should be an address (a string);
a Reply-to: field with that address is inserted.

If `mail-archive-file-name' is non-nil, an FCC field with that file name
is inserted.

The normal hook `mail-setup-hook' is run after the message is
initialized.  It can add more default fields to the message.

When calling from a program, the first argument if non-nil says
not to erase the existing contents of the `*mail*' buffer.

The second through fifth arguments,
 TO, SUBJECT, IN-REPLY-TO and CC, specify if non-nil
 the initial contents of those header fields.
 These arguments should not have final newlines.
The sixth argument REPLYBUFFER is a buffer which contains an
 original message being replied to, or else an action
 of the form (FUNCTION . ARGS) which says how to insert the original.
 Or it can be nil, if not replying to anything.
The seventh argument ACTIONS is a list of actions to take
 if/when the message is sent.  Each action looks like (FUNCTION . ARGS);
 when the message is sent, we apply FUNCTION to ARGS.
 This is how Rmail arranges to mark messages `answered'." t nil)

(autoload 'mail-other-window "sendmail" "\
Like `mail' command, but display mail buffer in another window." t nil)

(autoload 'mail-other-frame "sendmail" "\
Like `mail' command, but display mail buffer in another frame." t nil)

;;;***

;;;### (autoloads (smtpmail-send-queued-mail smtpmail-send-it) "smtpmail" "mail-lib/smtpmail.el")

(autoload 'smtpmail-send-it "smtpmail" nil nil nil)

(autoload 'smtpmail-send-queued-mail "smtpmail" "\
Send mail that was queued as a result of setting `smtpmail-queue-mail'." t nil)

;;;***

(provide 'mail-lib-autoloads)
