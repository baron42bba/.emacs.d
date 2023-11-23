;;; org-mac-link-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-mac-link" "org-mac-link.el" (0 0 0 0))
;;; Generated autoloads from org-mac-link.el

(autoload 'org-mac-link-get-link "org-mac-link" "\
Prompt for an application to grab a link from.
When done, go grab the link, and insert it at point. If a region
is active, that will be the link's description.

\(fn &optional BEG END)" t nil)

(autoload 'org-mac-link-firefox-get-frontmost-url "org-mac-link" "\
Get the link to the frontmost window of the Firefox.app." t nil)

(autoload 'org-mac-link-firefox-insert-frontmost-url "org-mac-link" "\
Insert the link to the frontmost window of the Firefox.app." t nil)

(autoload 'org-mac-link-vimperator-get-frontmost-url "org-mac-link" "\
Get the link to the frontmost window of the Firefox.app with Vimperator." t nil)

(autoload 'org-mac-link-vimperator-insert-frontmost-url "org-mac-link" "\
Insert the link to the frontmost window of the Firefox.app with Vimperator." t nil)

(autoload 'org-mac-link-chrome-get-frontmost-url "org-mac-link" "\
Get the link to the frontmost window of the Chrome.app." t nil)

(autoload 'org-mac-link-chrome-insert-frontmost-url "org-mac-link" "\
Insert the link to the frontmost window of the Chrome.app." t nil)

(autoload 'org-mac-link-brave-get-frontmost-url "org-mac-link" "\
Get the link to the frontmost window of the Brave.app." t nil)

(autoload 'org-mac-link-brave-insert-frontmost-url "org-mac-link" "\
Insert the link to the frontmost window of the Brave.app." t nil)

(autoload 'org-mac-link-safari-get-frontmost-url "org-mac-link" "\
Get the link to the frontmost window of the Safari.app." t nil)

(autoload 'org-mac-link-safari-insert-frontmost-url "org-mac-link" "\
Insert the link to the frontmost window of the Safari.app." t nil)

(autoload 'org-mac-link-together-get-selected "org-mac-link" "\
Get the links to selected items in the Together.app." t nil)

(autoload 'org-mac-link-together-insert-selected "org-mac-link" "\
Insert the links to selected items in the Together.app." t nil)

(autoload 'org-mac-link-finder-item-get-selected "org-mac-link" "\
Get the links to selected items in the Finder.app." t nil)

(autoload 'org-mac-link-finder-insert-selected "org-mac-link" "\
Insert the links to selected items in the Finder.app." t nil)

(autoload 'org-mac-link-addressbook-item-get-selected "org-mac-link" "\
Get the links to selected items in the addressbook." t nil)

(autoload 'org-mac-link-addressbook-item-insert-selected "org-mac-link" "\
Insert the links to selected items in the addressbook." t nil)

(autoload 'org-mac-link-skim-get-page "org-mac-link" "\
Get the link to the page in the Skim.app." t nil)

(autoload 'org-mac-link-skim-insert-page "org-mac-link" "\
Insert the link to the page in the Skim.app." t nil)

(autoload 'org-mac-link-acrobat-get-page "org-mac-link" "\
Get the link to the page in the Adobe Acrobat Pro.app." t nil)

(autoload 'org-mac-link-acrobat-insert-page "org-mac-link" "\
Insert the link to the page in the Adobe Acrobat Pro.app." t nil)

(autoload 'org-mac-link-outlook-message-get-links "org-mac-link" "\
Create links to selected or flagged messages in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject of the
messages in Microsoft Outlook.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-link-outlook-message-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject
of the active mail in Microsoft Outlook.app and make a link out of it." t nil)

(autoload 'org-mac-link-outlook-message-insert-flagged "org-mac-link" "\
Asks for an ORG-BUFFER and a heading within it, and replace message links.
If ORG-HEADING exists, delete all mac-outlook:// links in heading's first level.
If heading doesn't exist, create it at point-max.
Insert list of mac-outlook:// links to flagged mail after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

(autoload 'org-mac-link-evernote-note-insert-selected "org-mac-link" "\
Insert a link to the notes currently selected in Evernote.app.
This will use AppleScript to get the note id and the title of the
note(s) in Evernote.app and make a link out of it/them." t nil)

(autoload 'org-mac-link-devonthink-item-insert-selected "org-mac-link" "\
Insert a link to the item(s) currently selected in DEVONthink Pro Office.
This will use AppleScript to get the `uuid'(s) and the name(s) of the
selected items in DEVONthink Pro Office and make link(s) out of it/them." t nil)

(autoload 'org-mac-link-mail-get-links "org-mac-link" "\
Create links to the messages currently selected or flagged in Mail.app.
This will use AppleScript to get the message-id and the subject of the
messages in Mail.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-link-mail-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Mail.app.
This will use AppleScript to get the message-id and the subject of the
active mail in Mail.app and make a link out of it." t nil)

(autoload 'org-mac-link-mail-insert-flagged "org-mac-link" "\
Asks for an ORG-BUFFER and a heading within it, and replace message links.
If ORG-HEADING exists, delete all message:// links within heading's first level.
If heading doesn't exist, create it at point-max.
Insert list of message:// links to flagged mail after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

(autoload 'org-mac-link-qutebrowser-get-frontmost-url "org-mac-link" "\
Get the link to the frontmost window of the qutebrowser.app." t nil)

(autoload 'org-mac-link-qutebrowser-insert-frontmost-url "org-mac-link" "\
Insert the link to the frontmost window of the qutebrowser.app." t nil)

(register-definition-prefixes "org-mac-link" '("org-mac-link-"))

;;;***

;;;### (autoloads nil nil ("org-mac-link-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-mac-link-autoloads.el ends here
