;;; mew-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "mew" "mew.el" (0 0 0 0))
;;; Generated autoloads from mew.el

(autoload 'mew "mew" "\
Execute Mew first unless Mew is running. And retrieve arrived
messages or just visit to the default folder.

'proto' is determined by 'mew-proto' and 'mew-case'.

If 'proto' is '+' (ie a local folder), a mailbox is determined
according to 'mew-mailbox-type'. Otherwise (ie a remote folder), an
appropriate protocol to retrieve messages is chosen according to
'proto'.

If 'mew-auto-get' is 't', arrived messages are asynchronously fetched
and listed up in Summary mode.

'mew-auto-get' is 'nil', just visit to the folder determined by
'proto'.

When executed with '\\[universal-argument]', 'mew-auto-get' is
considered reversed.

\(fn &optional ARG)" t nil)

(autoload 'mew-send "mew" "\
Execute Mew then prepare a draft. This may be used as library
function.

\(fn &optional TO CC SUBJECT)" t nil)

(autoload 'mew-user-agent-compose "mew" "\
Set up message composition draft with Mew.
This is 'mail-user-agent' entry point to Mew.

The optional arguments TO and SUBJECT specify recipients and the
initial Subject field, respectively.

OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

A Draft buffer is prepared according to SWITCH-FUNCTION.

CONTINUE, YANK-ACTION and SEND-ACTIONS are ignored.

\(fn &optional TO SUBJECT OTHER-HEADERS CONTINUE SWITCH-FUNCTION YANK-ACTION SEND-ACTIONS &rest DUMMY)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew" '("mew-")))

;;;***

;;;### (autoloads nil "mew-addrbook" "mew-addrbook.el" (0 0 0 0))
;;; Generated autoloads from mew-addrbook.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-addrbook" '("mew-")))

;;;***

;;;### (autoloads nil "mew-attach" "mew-attach.el" (0 0 0 0))
;;; Generated autoloads from mew-attach.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-attach" '("mew-")))

;;;***

;;;### (autoloads nil "mew-auth" "mew-auth.el" (0 0 0 0))
;;; Generated autoloads from mew-auth.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-auth" '("mew-")))

;;;***

;;;### (autoloads nil "mew-blvs" "mew-blvs.el" (0 0 0 0))
;;; Generated autoloads from mew-blvs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-blvs" '("mew-")))

;;;***

;;;### (autoloads nil "mew-bq" "mew-bq.el" (0 0 0 0))
;;; Generated autoloads from mew-bq.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-bq" '("mew-")))

;;;***

;;;### (autoloads nil "mew-cache" "mew-cache.el" (0 0 0 0))
;;; Generated autoloads from mew-cache.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-cache" '("mew-")))

;;;***

;;;### (autoloads nil "mew-complete" "mew-complete.el" (0 0 0 0))
;;; Generated autoloads from mew-complete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-complete" '("mew-")))

;;;***

;;;### (autoloads nil "mew-config" "mew-config.el" (0 0 0 0))
;;; Generated autoloads from mew-config.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-config" '("mew-")))

;;;***

;;;### (autoloads nil "mew-const" "mew-const.el" (0 0 0 0))
;;; Generated autoloads from mew-const.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-const" '("mew-")))

;;;***

;;;### (autoloads nil "mew-darwin" "mew-darwin.el" (0 0 0 0))
;;; Generated autoloads from mew-darwin.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-darwin" '("mew-")))

;;;***

;;;### (autoloads nil "mew-decode" "mew-decode.el" (0 0 0 0))
;;; Generated autoloads from mew-decode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-decode" '("mew-")))

;;;***

;;;### (autoloads nil "mew-demo" "mew-demo.el" (0 0 0 0))
;;; Generated autoloads from mew-demo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-demo" '("mew-")))

;;;***

;;;### (autoloads nil "mew-draft" "mew-draft.el" (0 0 0 0))
;;; Generated autoloads from mew-draft.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-draft" '("mew-")))

;;;***

;;;### (autoloads nil "mew-edit" "mew-edit.el" (0 0 0 0))
;;; Generated autoloads from mew-edit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-edit" '("mew-")))

;;;***

;;;### (autoloads nil "mew-encode" "mew-encode.el" (0 0 0 0))
;;; Generated autoloads from mew-encode.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-encode" '("mew-")))

;;;***

;;;### (autoloads nil "mew-env" "mew-env.el" (0 0 0 0))
;;; Generated autoloads from mew-env.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-env" '("mew-")))

;;;***

;;;### (autoloads nil "mew-exec" "mew-exec.el" (0 0 0 0))
;;; Generated autoloads from mew-exec.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-exec" '("mew-")))

;;;***

;;;### (autoloads nil "mew-ext" "mew-ext.el" (0 0 0 0))
;;; Generated autoloads from mew-ext.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-ext" '("mew-")))

;;;***

;;;### (autoloads nil "mew-fib" "mew-fib.el" (0 0 0 0))
;;; Generated autoloads from mew-fib.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-fib" '("mew-fib-")))

;;;***

;;;### (autoloads nil "mew-func" "mew-func.el" (0 0 0 0))
;;; Generated autoloads from mew-func.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-func" '("mew-")))

;;;***

;;;### (autoloads nil "mew-gemacs" "mew-gemacs.el" (0 0 0 0))
;;; Generated autoloads from mew-gemacs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-gemacs" '("mew-")))

;;;***

;;;### (autoloads nil "mew-header" "mew-header.el" (0 0 0 0))
;;; Generated autoloads from mew-header.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-header" '("mew-")))

;;;***

;;;### (autoloads nil "mew-highlight" "mew-highlight.el" (0 0 0 0))
;;; Generated autoloads from mew-highlight.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-highlight" '("mew-")))

;;;***

;;;### (autoloads nil "mew-imap" "mew-imap.el" (0 0 0 0))
;;; Generated autoloads from mew-imap.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-imap" '("mew-imap-")))

;;;***

;;;### (autoloads nil "mew-imap2" "mew-imap2.el" (0 0 0 0))
;;; Generated autoloads from mew-imap2.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-imap2" '("mew-")))

;;;***

;;;### (autoloads nil "mew-key" "mew-key.el" (0 0 0 0))
;;; Generated autoloads from mew-key.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-key" '("mew-")))

;;;***

;;;### (autoloads nil "mew-lang-jp" "mew-lang-jp.el" (0 0 0 0))
;;; Generated autoloads from mew-lang-jp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-lang-jp" '("mew-")))

;;;***

;;;### (autoloads nil "mew-lang-kr" "mew-lang-kr.el" (0 0 0 0))
;;; Generated autoloads from mew-lang-kr.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-lang-kr" '("mew-thread-indent-strings")))

;;;***

;;;### (autoloads nil "mew-lang-latin" "mew-lang-latin.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from mew-lang-latin.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-lang-latin" '("mew-")))

;;;***

;;;### (autoloads nil "mew-local" "mew-local.el" (0 0 0 0))
;;; Generated autoloads from mew-local.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-local" '("mew-")))

;;;***

;;;### (autoloads nil "mew-mark" "mew-mark.el" (0 0 0 0))
;;; Generated autoloads from mew-mark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-mark" '("mew-")))

;;;***

;;;### (autoloads nil "mew-message" "mew-message.el" (0 0 0 0))
;;; Generated autoloads from mew-message.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-message" '("mew-")))

;;;***

;;;### (autoloads nil "mew-mime" "mew-mime.el" (0 0 0 0))
;;; Generated autoloads from mew-mime.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-mime" '("mew-")))

;;;***

;;;### (autoloads nil "mew-minibuf" "mew-minibuf.el" (0 0 0 0))
;;; Generated autoloads from mew-minibuf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-minibuf" '("mew-")))

;;;***

;;;### (autoloads nil "mew-mule" "mew-mule.el" (0 0 0 0))
;;; Generated autoloads from mew-mule.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-mule" '("mew-")))

;;;***

;;;### (autoloads nil "mew-mule3" "mew-mule3.el" (0 0 0 0))
;;; Generated autoloads from mew-mule3.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-mule3" '("mew-")))

;;;***

;;;### (autoloads nil "mew-net" "mew-net.el" (0 0 0 0))
;;; Generated autoloads from mew-net.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-net" '("mew-")))

;;;***

;;;### (autoloads nil "mew-nntp" "mew-nntp.el" (0 0 0 0))
;;; Generated autoloads from mew-nntp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-nntp" '("mew-")))

;;;***

;;;### (autoloads nil "mew-nntp2" "mew-nntp2.el" (0 0 0 0))
;;; Generated autoloads from mew-nntp2.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-nntp2" '("mew-nntp2-")))

;;;***

;;;### (autoloads nil "mew-passwd" "mew-passwd.el" (0 0 0 0))
;;; Generated autoloads from mew-passwd.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-passwd" '("mew-")))

;;;***

;;;### (autoloads nil "mew-pgp" "mew-pgp.el" (0 0 0 0))
;;; Generated autoloads from mew-pgp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-pgp" '("mew-")))

;;;***

;;;### (autoloads nil "mew-pick" "mew-pick.el" (0 0 0 0))
;;; Generated autoloads from mew-pick.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-pick" '("mew-")))

;;;***

;;;### (autoloads nil "mew-pop" "mew-pop.el" (0 0 0 0))
;;; Generated autoloads from mew-pop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-pop" '("mew-pop-")))

;;;***

;;;### (autoloads nil "mew-refile" "mew-refile.el" (0 0 0 0))
;;; Generated autoloads from mew-refile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-refile" '("mew-")))

;;;***

;;;### (autoloads nil "mew-scan" "mew-scan.el" (0 0 0 0))
;;; Generated autoloads from mew-scan.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-scan" '("mew-")))

;;;***

;;;### (autoloads nil "mew-search" "mew-search.el" (0 0 0 0))
;;; Generated autoloads from mew-search.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-search" '("mew-")))

;;;***

;;;### (autoloads nil "mew-smime" "mew-smime.el" (0 0 0 0))
;;; Generated autoloads from mew-smime.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-smime" '("mew-")))

;;;***

;;;### (autoloads nil "mew-smtp" "mew-smtp.el" (0 0 0 0))
;;; Generated autoloads from mew-smtp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-smtp" '("mew-")))

;;;***

;;;### (autoloads nil "mew-sort" "mew-sort.el" (0 0 0 0))
;;; Generated autoloads from mew-sort.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-sort" '("mew-s")))

;;;***

;;;### (autoloads nil "mew-ssh" "mew-ssh.el" (0 0 0 0))
;;; Generated autoloads from mew-ssh.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-ssh" '("mew-")))

;;;***

;;;### (autoloads nil "mew-ssl" "mew-ssl.el" (0 0 0 0))
;;; Generated autoloads from mew-ssl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-ssl" '("mew-")))

;;;***

;;;### (autoloads nil "mew-summary" "mew-summary.el" (0 0 0 0))
;;; Generated autoloads from mew-summary.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-summary" '("mew-")))

;;;***

;;;### (autoloads nil "mew-summary2" "mew-summary2.el" (0 0 0 0))
;;; Generated autoloads from mew-summary2.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-summary2" '("mew-")))

;;;***

;;;### (autoloads nil "mew-summary3" "mew-summary3.el" (0 0 0 0))
;;; Generated autoloads from mew-summary3.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-summary3" '("mew-")))

;;;***

;;;### (autoloads nil "mew-summary4" "mew-summary4.el" (0 0 0 0))
;;; Generated autoloads from mew-summary4.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-summary4" '("mew-")))

;;;***

;;;### (autoloads nil "mew-syntax" "mew-syntax.el" (0 0 0 0))
;;; Generated autoloads from mew-syntax.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-syntax" '("mew-")))

;;;***

;;;### (autoloads nil "mew-thread" "mew-thread.el" (0 0 0 0))
;;; Generated autoloads from mew-thread.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-thread" '("mew-")))

;;;***

;;;### (autoloads nil "mew-unix" "mew-unix.el" (0 0 0 0))
;;; Generated autoloads from mew-unix.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-unix" '("mew-")))

;;;***

;;;### (autoloads nil "mew-vars" "mew-vars.el" (0 0 0 0))
;;; Generated autoloads from mew-vars.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-vars" '("mew-")))

;;;***

;;;### (autoloads nil "mew-vars2" "mew-vars2.el" (0 0 0 0))
;;; Generated autoloads from mew-vars2.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-vars2" '("mew-")))

;;;***

;;;### (autoloads nil "mew-vars3" "mew-vars3.el" (0 0 0 0))
;;; Generated autoloads from mew-vars3.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-vars3" '("mew-")))

;;;***

;;;### (autoloads nil "mew-varsx" "mew-varsx.el" (0 0 0 0))
;;; Generated autoloads from mew-varsx.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-varsx" '("mew-defvar")))

;;;***

;;;### (autoloads nil "mew-virtual" "mew-virtual.el" (0 0 0 0))
;;; Generated autoloads from mew-virtual.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-virtual" '("mew-")))

;;;***

;;;### (autoloads nil "mew-win32" "mew-win32.el" (0 0 0 0))
;;; Generated autoloads from mew-win32.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mew-win32" '("mew-")))

;;;***

;;;### (autoloads nil nil ("mew-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mew-autoloads.el ends here
