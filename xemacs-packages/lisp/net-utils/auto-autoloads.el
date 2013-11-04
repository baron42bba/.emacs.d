;;; DO NOT MODIFY THIS FILE
(if (featurep 'net-utils-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "net-utils/_pkg.el")

(package-provide 'net-utils :version 1.48 :author-version "N/A" :type 'single-file)

;;;***

;;;### (autoloads (dns-mode-soa-increment-serial dns-mode) "dns-mode" "net-utils/dns-mode.el")

(autoload 'dns-mode "dns-mode" "Major mode for viewing and editing DNS master files.\nThis mode is inherited from text mode.  It add syntax\nhighlighting, and some commands for handling DNS master files.\nIts keymap inherits from `text-mode' and it has the same\nvariables for customizing indentation.  It has its own abbrev\ntable and its own syntax table.\n\nTurning on DNS mode runs `dns-mode-hook'." t nil)

(autoload 'dns-mode-soa-increment-serial "dns-mode" "\
Locate SOA record and increment the serial field." t nil)
(add-to-list 'auto-mode-alist '("\\.soa\\'" . dns-mode))

;;;***

;;;### (autoloads (feedmail-queue-reminder feedmail-run-the-queue feedmail-run-the-queue-global-prompt feedmail-run-the-queue-no-prompts) "feedmail" "net-utils/feedmail.el")

(autoload 'feedmail-run-the-queue-no-prompts "feedmail" "\
Like feedmail-run-the-queue, but suppress confirmation prompts." t nil)

(autoload 'feedmail-run-the-queue-global-prompt "feedmail" "\
Like feedmail-run-the-queue, but with a global confirmation prompt.
This is generally most useful if run non-interactively, since you can 
bail out with an appropriate answer to the global confirmation prompt." t nil)

(autoload 'feedmail-run-the-queue "feedmail" "\
Visit each message in the feedmail queue directory and send it out.
Return value is a list of three things: number of messages sent, number of
messages skipped, and number of non-message things in the queue (commonly
backup file names and the like)." t nil)

(autoload 'feedmail-queue-reminder "feedmail" "\
Perform some kind of reminder activity about queued and draft messages.
Called with an optional symbol argument which says what kind of event
is triggering the reminder activity.  The default is 'on-demand, which
is what you typically would use if you were putting this in your emacs start-up
or mail hook code.  Other recognized values for WHAT-EVENT (these are passed
internally by feedmail):

   after-immediate      (a message has just been sent in immediate mode)
   after-queue          (a message has just been queued)
   after-draft          (a message has just been placed in the draft directory)
   after-run            (the queue has just been run, possibly sending messages)

WHAT-EVENT is used as a key into the table feedmail-queue-reminder-alist.  If
the associated value is a function, it is called without arguments and is expected
to perform the reminder activity.  You can supply your own reminder functions 
by redefining feedmail-queue-reminder-alist.  If you don't want any reminders, 
you can set feedmail-queue-reminder-alist to nil." t nil)

;;;***

;;;### (autoloads (google-query-region google-query) "google-query" "net-utils/google-query.el")

(autoload 'google-query "google-query" "\
Query google for STRING." t nil)

(autoload 'google-query-region "google-query" "\
Query google for the string BEG END." t nil)

;;;***

;;;### (autoloads (metamail-region metamail-buffer metamail-interpret-body metamail-interpret-header) "metamail" "net-utils/metamail.el")

(autoload 'metamail-interpret-header "metamail" "\
Interpret a header part of a MIME message in current buffer.
Its body part is not interpreted at all." t nil)

(autoload 'metamail-interpret-body "metamail" "\
Interpret a body part of a MIME message in current buffer.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted.
Its header part is not interpreted at all." t nil)

(autoload 'metamail-buffer "metamail" "\
Process current buffer through `metamail'.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument BUFFER specifies a buffer to be filled (nil
means current).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted." t nil)

(autoload 'metamail-region "metamail" "\
Process current region through 'metamail'.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument BUFFER specifies a buffer to be filled (nil
means current).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted." t nil)

;;;***

;;;### (autoloads (network-connection network-connection-to-service whois-reverse-lookup whois finger ftp nslookup nslookup-host route arp netstat ipconfig ping traceroute) "net-utils" "net-utils/net-utils.el")

(autoload 'traceroute "net-utils" "\
Run traceroute program for TARGET." t nil)

(autoload 'ping "net-utils" "\
Ping HOST.
If your system's ping continues until interrupted, you can try setting 
`ping-program-options'." t nil)

(autoload 'ipconfig "net-utils" "\
Run ipconfig program." t nil)

(defalias 'ifconfig 'ipconfig)

(autoload 'netstat "net-utils" "\
Run netstat program." t nil)

(autoload 'arp "net-utils" "\
Run the arp program." t nil)

(autoload 'route "net-utils" "\
Run the route program." t nil)

(autoload 'nslookup-host "net-utils" "\
Lookup the DNS information for HOST." t nil)

(autoload 'nslookup "net-utils" "\
Run nslookup program." t nil)

(autoload 'ftp "net-utils" "\
Run ftp program." t nil)

(autoload 'finger "net-utils" "\
Finger USER on HOST." t nil)

(autoload 'whois "net-utils" "\
Send SEARCH-STRING to server defined by the `whois-server-name' variable.
With argument, prompt for whois server." t nil)

(autoload 'whois-reverse-lookup "net-utils" nil t nil)

(autoload 'network-connection-to-service "net-utils" "\
Open a network connection to SERVICE on HOST." t nil)

(autoload 'network-connection "net-utils" "\
Open a network connection to HOST on PORT." t nil)

;;;***

;;;### (autoloads (remote-compile) "rcompile" "net-utils/rcompile.el")

(autoload 'remote-compile "rcompile" "\
Compile the current buffer's directory on HOST.  Log in as USER.
See \\[compile]." t nil)

;;;***

;;;### (autoloads (webjump) "webjump" "net-utils/webjump.el")

(autoload 'webjump "webjump" "\
Jumps to a Web site from a programmable hotlist.

See the documentation for the `webjump-sites' variable for how to customize the
hotlist.

Please submit bug reports and other feedback to the author, Neil W. Van Dyke
<nwv@acm.org>.

The latest version can be gotten from `http://www.cs.brown.edu/people/nwv/'.
That Web site also contains `webjump-plus.el', a larger and more frequently
updated sample WebJump hotlist." t nil)

;;;***

;;;### (autoloads (report-xemacs-bug) "xemacsbug" "net-utils/xemacsbug.el")

(autoload 'report-xemacs-bug "xemacsbug" "\
Report a bug in XEmacs.
Prompts for bug subject.  Leaves you in a mail buffer." t nil)

(defalias 'report-emacs-bug 'report-xemacs-bug)

;;;***

(provide 'net-utils-autoloads)
