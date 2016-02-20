;;; json-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "json-mode" "json-mode.el" (22218 58078 0 0))
;;; Generated autoloads from json-mode.el

(autoload 'json-mode-beautify "json-mode" "\
Beautify / pretty-print from BEG to END, and optionally PRESERVE-KEY-ORDER.

\(fn &optional PRESERVE-KEY-ORDER)" t nil)

(autoload 'json-mode-beautify-ordered "json-mode" "\
Beautify / pretty-print from BEG to END preserving key order.

\(fn)" t nil)

(autoload 'json-mode "json-mode" "\
Major mode for editing JSON files

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; json-mode-autoloads.el ends here
