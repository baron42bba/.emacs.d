;;; clomacs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "clomacs" "clomacs.el" (22692 30292 0 0))
;;; Generated autoloads from clomacs.el

(autoload 'clomacs-def "clomacs" "\
Wrap CL-ENTITY-NAME, evaluated on clojure side by EL-ENTITY-NAME.
DOC - optional elisp function docstring (when nil it constructed from
underlying clojure entity docstring if possible).
TYPE possible values are listed in the CLOMACS-POSSIBLE-RETURN-TYPES,
or it may be a custom function (:string by default).

\(fn EL-ENTITY-NAME CL-ENTITY-NAME &key (DOC nil) (TYPE :string) LIB-NAME NAMESPACE)" nil t)

(autoload 'clomacs-defun "clomacs" "\
Wrap CL-FUNC-NAME, evaluated on clojure side by EL-FUNC-NAME.
CALL-TYPE - call Clojure side :sync or :async.
CALLBACK - callback function for :async CALL-TYPE case.
DOC - optional elisp function docstring (when nil it constructed from
underlying clojure entity docstring if possible).
RETURN-TYPE possible values are listed in the CLOMACS-POSSIBLE-RETURN-TYPES,
or it may be a custom function (:string by default).
RETURN-VALUE may be :value or :stdout (:value by default).

\(fn EL-FUNC-NAME CL-FUNC-NAME &key (CALL-TYPE :sync) (CALLBACK nil) (DOC nil) (RETURN-TYPE :string) (RETURN-VALUE :value) LIB-NAME NAMESPACE)" nil t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; clomacs-autoloads.el ends here
