;;; ejc-sql.el --- Emacs SQL client uses Clojure JDBC. -*- lexical-binding: t -*-

;;; Copyright © 2012-2020 - Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/kostafey/ejc-sql
;; Keywords: sql, jdbc
;; Version: 0.4.1
;; Package-Requires: ((emacs "26.3")(clomacs "0.0.5")(dash "2.16.0")(spinner "1.7.3"))

;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software Foundation,
;;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

;;; Commentary:

;; ejc-sql turns Emacs into simple SQL client, it uses JDBC connection to
;; databases via clojure/java.jdbc lib.

;; See README.md for detailed description.

;;; Code:

(require 'sql)
(require 'dash)
(require 'cl-lib)
(require 'org-table)
(require 'ejc-lib)
(require 'ejc-eldoc)
(require 'ejc-format)
(require 'ejc-interaction)
(require 'ejc-result-mode)
(require 'ejc-result-buffer)
(require 'ejc-completion-common)

(defvar-local ejc-db nil
  "JDBC connection info for current SQL buffer.")

(defvar ejc-connections nil
  "List of existing configured jdbc connections")

(defvar ejc-current-buffer-query nil
  "Current SQL edit buffer lanched query.")

(defcustom ejc-temp-editor-buffer-name "ejc-sql-editor"
  "Template name for buffers used to ad-hoc edit SQL scripts."
  :group 'ejc-sql
  :type 'string)

(defcustom ejc-temp-editor-file-path (expand-file-name "~/tmp/ejc-sql/")
  "Directory to save temp ad-hoc SQL edit buffers contents."
  :group 'ejc-sql
  :type 'string)

(defcustom ejc-keymap-prefix (kbd "C-c e")
  "ejc-sql keymap prefix."
  :group 'ejc-sql
  :type 'string)

(defcustom ejc-date-output-format "%d.%m.%Y %H:%M:%S"
  "ejc-sql date output format."
  :group 'ejc-sql
  :type 'string)

(defcustom ejc-connection-validate-timeout 5
  "The time in seconds to wait for the database connection validation."
  :group 'ejc-sql
  :type 'integer)

(defcustom ejc-result-table-impl 'orgtbl-mode
  "Set mode for result-set table. Possible values are one of:
'orgtbl-mode
'ejc-result-mode."
  :group 'ejc-sql
  :type 'symbol)

(defcustom ejc-org-mode-babel-wrapper t
  "Add wrapper around org-mode default `org-babel-execute:sql'."
  :type 'boolean
  :safe #'booleanp
  :group 'ejc-sql)

(defcustom ejc-org-mode-show-results t
  "When t show SQL query results of `org-mode' code snippet in the same buffer.
An expected behaviour for `org-mode' users. Disable popup window with SQL
results. When nil, otherwise, provide `ejc-sql' users expected behaviour."
  :group 'ejc-sql
  :type 'boolean)

(defcustom ejc-jdbc-drivers
  '("sqlite"     [org.xerial/sqlite-jdbc "3.23.1"]
    "h2"         [com.h2database/h2 "1.4.199"]
    "mysql"      [mysql/mysql-connector-java "5.1.44"]
    "postgresql" [postgresql/postgresql "9.3-1102.jdbc41"]
    "sqlserver"  [com.microsoft.sqlserver/mssql-jdbc "6.2.2.jre8"]
    "oracle"     [com.oracle.jdbc/ojdbc8 "12.2.0.1"])
  "Artifacts used as JDBC drivers for each database type in Leiningen format."
  :group 'ejc-sql
  :type '(plist :key-type string :value-type (vector symbol string)))

(defcustom ejc-completion-system 'standard
  "The completion system used by `ejc-connect'."
  :group 'ejc-sql
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Standard" standard)
          (function :tag "Custom function")))

(defvar-local ejc-replace-double-quotes nil
  "When t replace double quotes with single ones in SQL before evaluation.")

(defvar ejc-sql-mode-keymap (make-keymap) "ejc-sql-mode keymap.")
(define-key ejc-sql-mode-keymap (kbd "C-c C-c") 'ejc-eval-user-sql-at-point)
(define-key ejc-sql-mode-keymap (kbd "C-c C-r") 'ejc-eval-user-sql-region)
(define-key ejc-sql-mode-keymap (kbd "C-h t") 'ejc-describe-table)
(define-key ejc-sql-mode-keymap (kbd "C-h d") 'ejc-describe-entity)
(define-key ejc-sql-mode-keymap (kbd "C-M-S-b") (lambda () (interactive) (ejc-previous-sql t)))
(define-key ejc-sql-mode-keymap (kbd "C-M-S-f") (lambda () (interactive) (ejc-next-sql t)))
(define-key ejc-sql-mode-keymap (kbd "C-M-b") 'ejc-previous-sql)
(define-key ejc-sql-mode-keymap (kbd "C-M-f") 'ejc-next-sql)
(define-key ejc-sql-mode-keymap (kbd "C-g") 'ejc-cancel-query)
(define-key ejc-sql-mode-keymap (kbd "M-.") 'ejc-describe-entity)
(define-key ejc-sql-mode-keymap (kbd "M-,") 'ejc-show-prev-result)
(define-key ejc-sql-mode-keymap (kbd ".") 'ejc-dot-pressed)

(defvar ejc-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'ejc-connect)
    (define-key map (kbd "i") #'ejc-connect-interactive)
    (define-key map (kbd "<up>") #'ejc-show-last-result)
    (define-key map (kbd "t") #'ejc-show-tables-list)
    (define-key map (kbd "v") #'ejc-show-views-list)
    (define-key map (kbd "p") #'ejc-show-procedures-list)
    (define-key map (kbd "T") #'ejc-show-user-types-list)
    (define-key map (kbd "s") #'ejc-strinp-sql-at-point)
    (define-key map (kbd "S") #'ejc-dress-sql-at-point)
    (define-key map (kbd "f") #'ejc-format-sql-at-point)
    map)
  "Keymap for ejc-sql commands after `ejc-keymap-prefix'.")
(fset 'ejc-command-map ejc-command-map)

(define-key ejc-sql-mode-keymap ejc-keymap-prefix 'ejc-command-map)

(defvar ejc-sql-minor-mode-exit-hook nil
  "*Functions to be called when `ejc-sql-mode' is exited.")

(defvar ejc-sql-minor-mode-hook nil
  "*Functions to be called when `ejc-sql-mode' is entered.")

(defvar ejc-sql-connected-hook nil
  "Hook run when nREPL started and some buffer connected to DataBase.")

(defvar ejc-sql-complete-query-hook nil
  "Hook run when SQL query executed and the result is outputted.")

(defvar ejc-sql-mode nil)

(defvar ejc-conn-statistics (list)
  "Keep connection usage statistics and offer most frequently used first
 when `ejc-connect' is called.")

(defcustom ejc-conn-statistics-file (expand-file-name
                                     "~/.ejc-sql/connection-statistics.el")
  "Connection usage statistics data file location."
  :group 'ejc-sql
  :type 'string)

(defface ejc-separator-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight SQL statement separators."
  :group 'ejc-sql)

(defun ejc-refresh-font-lock ()
  (funcall (if ejc-sql-mode
               'font-lock-add-keywords
             'font-lock-remove-keywords)
           nil `((,(ejc-sql-separator-re) . 'ejc-separator-face)))
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

;;;###autoload
(define-minor-mode ejc-sql-mode
  "Toggle ejc-sql mode."
  :lighter " ejc"
  :keymap ejc-sql-mode-keymap
  :group 'ejc
  :global nil
  (if ejc-sql-mode
      (progn
        (ejc-create-menu)
        (ejc-refresh-font-lock)
        (run-hooks 'ejc-sql-minor-mode-hook))
    (progn
      (ejc-refresh-font-lock)
      (run-hooks 'ejc-sql-minor-mode-exit-hook))))

;;;###autoload
(defun ejc-create-menu ()
  (define-key-after
    ejc-sql-mode-keymap
    [menu-bar ejc-menu]
    (cons "ejc-sql" (make-sparse-keymap "ejc-sql mode"))
    'tools )
  (define-key
    ejc-sql-mode-keymap
    [menu-bar ejc-menu ev]
    '("Eval SQL" . ejc-eval-user-sql-at-point))
  (define-key
    ejc-sql-mode-keymap
    [menu-bar ejc-menu fs]
    '("Format SQL" . ejc-format-sql-at-point))
  (define-key
    ejc-sql-mode-keymap
    [menu-bar ejc-menu ms]
    '("Mark SQL" . ejc-mark-this-sql))
  (define-key
    ejc-sql-mode-keymap
    [menu-bar ejc-menu tl]
    '("Show tables list" . ejc-show-tables-list))
  (define-key
    ejc-sql-mode-keymap
    [menu-bar ejc-menu cl]
    '("Show constraints list" . ejc-show-constraints-list))
  (define-key
    ejc-sql-mode-keymap
    [menu-bar ejc-menu pl]
    '("Show procedures list" . ejc-show-procedures-list))
  (define-key
    ejc-sql-mode-keymap
    [menu-bar ejc-menu ss]
    '("Strip SQL" . ejc-strinp-sql-at-point))
  (define-key
    ejc-sql-mode-keymap
    [menu-bar ejc-menu ds]
    '("Dress SQL" . ejc-dress-sql-at-point))
  (define-key
    ejc-sql-mode-keymap
    [menu-bar ejc-menu ol]
    '("Open log" . ejc-open-log))
  (define-key
    ejc-sql-mode-keymap
    [menu-bar ejc-menu sl]
    '("Show last result" . ejc-show-last-result))
  (define-key
    ejc-sql-mode-keymap
    [menu-bar ejc-menu qc]
    '("Quit connection" . ejc-quit-connection)))

(cl-defun ejc-create-connection (connection-name
                                 &key
                                 ;; ----------
                                 ;; DriverManager (preferred):
                                 dbtype
                                 dbname
                                 host
                                 port
                                 ;; others (may include :user and :password)
                                 ;; ----------
                                 ;; Raw:
                                 connection-uri
                                 ;; others (may include :user and :password)
                                 ;; ----------
                                 ;; DriverManager (alternative / legacy style):
                                 subprotocol
                                 subname
                                 ;; ----------
                                 ;; Others:
                                 user
                                 password
                                 dependencies
                                 classpath
                                 separator
                                 sslmode
                                 ;; ----------
                                 ;; Optional:
                                 classname)
  "Add new connection configuration named CONNECTION-NAME.
It adds new connection to `ejc-connections' list or replace existing with the
same CONNECTION-NAME.
For more details about parameters see `get-connection' function in jdbc.clj:
`https://github.com/clojure/java.jdbc/blob/master/src/main/clojure/clojure/java/jdbc.clj'"
  (setq ejc-connections
        (-remove (lambda (x) (equal (car x) connection-name))
                 ejc-connections))
  (setq ejc-connections
        (cons (cons
               connection-name
               (let ((new-connection nil))
                 (-map (lambda (arg)
                         (if (cdr arg)
                             (setq new-connection
                                   (cons arg new-connection))))
                       (list
                        (cons :dbtype dbtype)
                        (cons :dbname dbname)
                        (cons :host host)
                        (cons :port port)
                        (cons :connection-uri connection-uri)
                        (cons :subprotocol subprotocol)
                        (cons :subname subname)
                        (cons :user user)
                        (cons :password password)
                        (cons :dependencies dependencies)
                        (cons :classpath
                              (when classpath
                                (if (vectorp classpath)
                                    (apply 'vector
                                           (-map 'file-truename classpath))
                                  (vector (file-truename classpath)))))
                        (cons :separator separator)
                        (cons :sslmode sslmode)
                        (cons :classname classname)))
                 new-connection))
              ejc-connections)))

(cl-defun ejc-completing-read (prompt choices &key initial-input)
  "Present a PROMPT with CHOICES and optional INITIAL-INPUT."
  (cond
   ((eq ejc-completion-system 'ido)
    (ido-completing-read prompt choices nil nil initial-input))
   ((eq ejc-completion-system 'standard)
    (completing-read prompt choices nil nil initial-input))
   (t (funcall ejc-completion-system prompt choices))))

(defun ejc-read-file-name (prompt)
  "Read file name, prompting with PROMPT."
  (cond
   ((eq ejc-completion-system 'ido)
    (ido-read-file-name prompt))
   (t
    (read-file-name prompt))))

(defun ejc-find-connection (connection-name)
  "Return pair with name CONNECTION-NAME and db connection structure from
`ejc-connections'."
  (-find (lambda (x) (equal (car x) connection-name))
         ejc-connections))

(defun ejc-load-conn-statistics ()
  "Load connection usage statistics to `ejc-conn-statistics' var."
  (setq ejc-conn-statistics
        (ejc-load-from-file ejc-conn-statistics-file
                            :default (list)
                            :check 'ejc-plist-p)))

(defun ejc-update-conn-statistics (connection-name)
  "Update connection usage statistics, persist it in `ejc-conn-statistics-file'"
  (setq ejc-conn-statistics
        (lax-plist-put
         ejc-conn-statistics
         connection-name
         (1+ (or (lax-plist-get ejc-conn-statistics connection-name) 0))))
  (ejc-save-to-file ejc-conn-statistics-file ejc-conn-statistics))

(defun ejc-set-mode-name (connection-name)
  "Show CONNECTION-NAME as part of `mode-name' in `mode-line'."
  (setq mode-name (format "%s->[%s]"
                          (car (split-string
                                (if (listp mode-name)
                                    (car mode-name)
                                  mode-name)
                                "->\\[.+\\]"))
                          connection-name)))

(cl-defun ejc-add-connection (&optional connection-name db)
  "Add ejc connection information to current buffer.
If the current mode is `sql-mode' prepare buffer to operate as `ejc-sql-mode'."
  (when (derived-mode-p 'sql-mode)
    (sql-set-product (if db
                         (ejc-get-product-name db)
                       "ansi"))
    (ejc-sql-mode t))
  (setq-local ejc-connection-name connection-name)
  (setq-local ejc-db db)
  (ejc-set-mode-name connection-name))

(defun ejc-eval-org-snippet (&optional orig-fun body params)
  "Used to eval SQL code in `org-mode' code snippets."
  (if (or (not ejc-org-mode-babel-wrapper)
          (and (cdr (assq :engine params))
               (not
                (yes-or-no-p
                 (concat "ejc-sql is enabled, ignore source block connection"
                         " header arguments and use ejc-sql to execute it? ")))))
      (funcall orig-fun body params)
    (cl-multiple-value-bind (beg end) (save-mark-and-excursion
                                        (org-babel-mark-block)
                                        (list (point) (mark)))
      (ejc-eval-user-sql-at-point
       :beg beg
       :end end
       :sync ejc-org-mode-show-results
       :display-result (not ejc-org-mode-show-results))
      (if ejc-org-mode-show-results
          (with-temp-buffer
            (insert-file-contents (ejc-get-result-file-path))
            (buffer-string))))))

(defun ejc-org-edit-special (orig-fun &rest args)
  (if (and (equal "sql" (car (org-babel-get-src-block-info)))
           (boundp 'ejc-db) ejc-db
           (boundp 'ejc-connection-name) ejc-connection-name)
      (let* ((db ejc-db)
             (connection-name ejc-connection-name))
        (apply orig-fun args)
        (ejc-add-connection connection-name db))
    (apply orig-fun args)))

(defun ejc-read-connection-name ()
  "Read connection-name in minibuffer."
  (ejc-completing-read
   "DataBase connection name: "
   (let ((conn-list (mapcar 'car ejc-connections))
         (conn-statistics (ejc-load-conn-statistics)))
     (-sort (lambda (c1 c2)
              (> (or (lax-plist-get conn-statistics c1) 0)
                 (or (lax-plist-get conn-statistics c2) 0)))
            conn-list))))

;;;###autoload
(defun ejc-insert-connection-data (connection-name)
  "Insert configured connection data to keep it between Emacs restarts.
Assume to be evaluated somewhere in .emacs or any file, loaded as Emacs
configuration."
  (interactive
   (list (or (and (boundp 'connection-name) connection-name)
             (ejc-read-connection-name))))
  (let ((connection-params (cdr (ejc-find-connection connection-name))))
    (insert
     (format "(ejc-create-connection\n \"%s\"%s)"
             connection-name
             (-reduce-from
              (lambda (result x)
                (concat result
                        (format "\n %s \"%s\""
                                (symbol-name (car x))
                                (cdr x))))
              ""
              connection-params)))))

(defalias 'ejc-save-connection-data 'ejc-insert-connection-data)

(defun ejc-resolve-jdbc-driver (dbtype)
  "Resolve and download artifacts (JDBC drivers) for DBTYPE.
Apropriate artifacts list located in `ejc-jdbc-drivers'."
  (let* ((artifact (lax-plist-get ejc-jdbc-drivers dbtype))
         (jar-path (ejc-lein-artifact-to-path artifact)))
    (if (not (and (file-exists-p jar-path)
                  (if (> (file-attribute-size (file-attributes jar-path))
                         0)
                      t
                    ;; If JDBC driver jar file is accidentally empty - it
                    ;; should be deleted and reinstalled.
                    (progn (delete-file jar-path)
                           nil))))
        (ejc-resolve-dependencies
         :coordinates (vector artifact)
         :repositories '(("central" . "https://repo1.maven.org/maven2/")
                         ("clojars" . "https://clojars.org/repo"))))
    jar-path))

;;;###autoload
(defun ejc-connect-interactive (connection-name)
  "Create new connection interactively and connect to it."
  (interactive
   (list
    (or (and (boundp 'connection-name) connection-name)
        (read-string "DataBase new connection name: "))))
  (let* ((db-types '(("MySQL" . "mysql")
                     ("Oracle" . "oracle")
                     ("MS SQL Server" . "sqlserver")
                     ("H2" . "h2")
                     ("SQLite" . "sqlite")
                     ("PostgreSQL" . "postgresql")))
         (dbtype (cdr (assoc-string
                       (ejc-completing-read
                        "Database type: "
                        (cl-sort (-map 'car db-types)
                                 'string-lessp :key 'downcase))
                       db-types)))
         (properties
          (-concat
           (pcase dbtype
             ("sqlite"
              `((:subprotocol . "sqlite")
                (:subname . "")))
             ("h2"
              `((:subprotocol . "h2")
                (:subname . "")))
             ("mysql"
              `((:dbtype . "mysql")
                (:classname . "com.mysql.jdbc.Driver")
                (:dbname . "")
                (:host . "localhost")
                (:port . "3306")))
             ("postgresql"
              `((:dbtype . "postgresql")
                (:dbname . "")
                (:host . "localhost")
                (:port . "5432")))
             ("sqlserver"
              `((:dbtype . "sqlserver")
                (:dbname . "")
                (:host . "localhost")
                (:port . "1433")))
             ("oracle"
              `((:dbtype . "oracle")
                (:dbname . "")
                (:host . "localhost")
                (:port . "1521"))))
           '((:classpath . "")
             (:user . "")
             (:password . ""))))
         (args
          (-reduce-from
           (lambda (memo p)
             (-concat
              memo
              (list
               (car p)
               (cl-case (car p)
                 (:dbtype (cdr p))
                 (:classpath (ejc-resolve-jdbc-driver dbtype))
                 (:classname (cdr p))
                 (:subprotocol (cdr p))
                 (:subname (if (member dbtype '("h2" "sqlite"))
                               (let ((fpath
                                      (file-truename
                                       (ejc-read-file-name "DB file: "))))
                                 (if (equal "h2" dbtype)
                                     (concat "file://"
                                             (file-name-sans-extension
                                              (file-name-sans-extension
                                               fpath))
                                             ";AUTO_SERVER=TRUE")
                                   fpath))
                             (read-string "DataBase subname: " (cdr p))))
                 (:dbname (read-string "DataBase name: " (cdr p)))
                 (:host (read-string "Host: " (cdr p)))
                 (:port (read-string "Port: " (cdr p)))
                 (:user (read-string "User: "))
                 (:password (read-passwd "Password: "))))))
           (list connection-name)
           properties)))
    (apply 'ejc-create-connection args)
    (ejc-connect connection-name)))

;;;###autoload
(defun ejc-connect (connection-name)
  "Connect to selected db."
  (interactive (list
                (or (and (boundp 'connection-name) connection-name)
                    (ejc-read-connection-name))))
  (if-let ((db (cdr (ejc-find-connection connection-name))))
      (progn
        (ejc-update-conn-statistics connection-name)
        (ejc-add-connection connection-name db)
        (when (derived-mode-p 'org-mode)
          (require 'ob-sql)
          (advice-add 'org-babel-execute:sql :around 'ejc-eval-org-snippet)
          (advice-add 'org-edit-special :around #'ejc-org-edit-special))
        (message "Connection started...")
        (clomacs-with-nrepl "ejc-sql"
          (lambda (db connection-name)
            (ejc-connect-to-db db)
            (let ((validation-result
                   (ejc-validate-connection
                    :db db
                    :timeout ejc-connection-validate-timeout)))
              (when (alist-get :status validation-result)
                (ejc-set-mode-name connection-name)
                (run-hooks 'ejc-sql-connected-hook)
                (message (let ((msg (alist-get :message validation-result)))
                           (if (equal "Connected." msg)
                               (format
                                "Connected -> %s."
                                (propertize connection-name
                                            'face 'font-lock-keyword-face))
                             msg))))))
          :params (list db connection-name)))
    (ejc-connect-interactive connection-name)))

;;;###autoload
(defun ejc-connect-existing-repl ()
  "Connect to existing ejc-sql nREPL running process.
You can `cd` to your ejc-sql project folder (typically
'~/.emacs.d/elpa/ejc-sql-<version>') and launch nREPL via `lein repl`.
Then run in Emacs `ejc-connect-existing-repl', type HOST and PORT
from your `lein run` console output. Finally, use `ejc-connect' from
any SQL buffer to connect to exact database, as always. "
  (interactive)
  (let* ((params (cider-select-endpoint))
         (host (car params))
         (port (cdr params))
         (current-repl-b-name (cider-connect (list :host host :port port)))
         (ejc-repl-b-name (nrepl-repl-buffer-name
                           (list
                            :session-name "ejc-sql"
                            :repl-type "clj"
                            :host host
                            :port port))))
    (with-current-buffer current-repl-b-name
      (rename-buffer ejc-repl-b-name))))

(defun ejc-check-connection ()
  (unless (ejc-buffer-connected-p)
    (error "Run M-x ejc-connect first!")))

(defun ejc-get-sql-from-string (sql)
  (let* ((sql (replace-regexp-in-string ejc-clear-sql-regexp "" sql))
         (sql (if ejc-replace-double-quotes
                  (replace-regexp-in-string "\"" "'" sql)
                sql)))
    sql))

(defun ejc-add-outside-borders-p ()
  (cl-case ejc-result-table-impl
    (orgtbl-mode     t)
    (ejc-result-mode nil)))

(defun ejc-message-query-done (start-time status)
  (message
   "%s SQL query at %s. Exec time %.03f"
   (cl-case status
     (:done (propertize "Done" 'face 'font-lock-keyword-face))
     (:error (propertize "Error" 'face 'error))
     (:terminated (propertize "Terminated" 'face 'font-lock-keyword-face)))
   (format-time-string ejc-date-output-format
                       (current-time))
   (float-time (time-since start-time))))

(defun ejc-spinner-stop ()
  "Stop spinner indicating current running query."
  (if ejc-current-buffer-query
   (with-current-buffer ejc-current-buffer-query
     (spinner-stop))))

(cl-defun ejc-complete-query (result-file-path
                              &key
                              start-time
                              status
                              display-result
                              mode
                              connection-name
                              db
                              goto-symbol)
  "Called by Clojure side, when SQL evaluation thread completes."
  (ejc-spinner-stop)
  (if result-file-path
      (setq ejc-result-file-path result-file-path))
  (if display-result
      (ejc-show-last-result :mode mode
                            :connection-name connection-name
                            :db db
                            :goto-symbol goto-symbol))
  (if (and start-time status)
      (ejc-message-query-done start-time status))
  (run-hooks 'ejc-sql-complete-query-hook)
  nil)

(cl-defun ejc-cancel-query (&key start-time)
  "Terminate current (long) running query. Aimed to cancel SELECT queries.
Unsafe for INSERT/UPDATE/CREATE/ALTER queries."
  (interactive)
  (ejc-spinner-stop)
  (if (and (clomacs-get-connection "ejc-sql")
           (ejc--is-query-running-p))
      (let ((start-time (or start-time (ejc--cancel-query))))
        (ejc-message-query-done start-time :terminated))
    (keyboard-quit)))

(defun ejc-get-prompt-symbol-under-point (msg)
  "Read user typed string from minibuffer.
MSG is a first part of the prompt message. The second optional part in
brackets is a symbol under point (cursor). Return a list of two items:
(schema|owner db-entity)."
  (let* ((prefix (if (not mark-active)
                     (ejc-get-prefix-word)))
         (sql-symbol (if mark-active
                         (buffer-substring (mark) (point))
                       (ejc-get-word-at-point (point))))
         (enable-recursive-minibuffers t)
         (typed-string (read-string
                        (if sql-symbol
                            (format "%s (default %s): "
                                    msg
                                    (if prefix
                                        (format "%s.%s" prefix sql-symbol)
                                      sql-symbol))
                          (format "%s: " msg)))))
    (if (equal typed-string "")
        (list prefix sql-symbol)
      (let ((split-typed-string (split-string typed-string "\\.")))
        (if (cadr split-typed-string)
            split-typed-string
          (list nil typed-string))))))

(defun ejc-describe-table (prefix table-name)
  "Describe SQL table TABLE-NAME (default table name - word around the point)."
  (interactive (ejc-get-prompt-symbol-under-point "Describe table"))
  (ejc-check-connection)
  (ejc--describe-table :db ejc-db
                       :connection-name ejc-connection-name
                       :table table-name
                       :owner prefix
                       :result-file (ejc-next-result-file-path)
                       :add-outside-borders (ejc-add-outside-borders-p)))

(defun ejc-describe-entity (prefix entity-name)
  "Describe SQL entity ENTITY-NAME - function, procedure, type or view
   (default entity name - word around the point)."
  (interactive (ejc-get-prompt-symbol-under-point "Describe entity"))
  (ejc-check-connection)
  (ejc-get-entity-description :db ejc-db
                              :connection-name ejc-connection-name
                              :prefix prefix
                              :entity-name entity-name
                              :result-file (ejc-next-result-file-path)))

(cl-defun ejc-eval-user-sql (sql &key
                                 db
                                 rows-limit
                                 fetch-size
                                 column-width-limit
                                 append
                                 sync
                                 display-result
                                 result-file)
  "User starts SQL evaluation process."
  (message "Processing SQL query...")
  (when sql
    (spinner-start 'rotating-line)
    (setq ejc-current-buffer-query (current-buffer))
    (let* ((prepared-sql (ejc-get-sql-from-string sql)))
      (ejc--eval-sql-and-log-print
       (or db ejc-db)
       prepared-sql
       :start-time (current-time)
       :rows-limit rows-limit
       :fetch-size fetch-size
       :column-width-limit column-width-limit
       :append append
       :sync sync
       :display-result display-result
       :result-file (or result-file
                        (ejc-next-result-file-path))
       :add-outside-borders (ejc-add-outside-borders-p)))))

;;;###autoload
(defun ejc-eval-user-sql-region (beg end)
  "Evaluate SQL bounded by the selection area."
  (interactive "r")
  (ejc-check-connection)
  (let ((sql (buffer-substring beg end)))
    (ejc-eval-user-sql sql
                       :display-result t))
  (if (region-active-p)
      (deactivate-mark)))

;;;###autoload
(cl-defun ejc-eval-user-sql-at-point (&key
                                      sync
                                      beg
                                      end
                                      (display-result t))
  "Evaluate SQL bounded by the `ejc-sql-separator' or/and buffer
boundaries."
  (interactive)
  (ejc-check-connection)
  (ejc-flash-this-sql :beg beg
                      :end end)
  (ejc-eval-user-sql (ejc-get-sql-at-point :beg beg :end end)
                     :sync sync
                     :display-result display-result))

;;;###autoload
(defun ejc-show-tables-list ()
  "Output tables list."
  (interactive)
  (ejc-check-connection)
  (ejc-eval-user-sql
   (ejc-select-db-meta-script ejc-db :all-tables)
   :rows-limit 0
   :fetch-size 0
   :column-width-limit 0
   :display-result t))

;;;###autoload
(defun ejc-show-views-list ()
  "Output views list."
  (interactive)
  (ejc-check-connection)
  (ejc-eval-user-sql
   (ejc-select-db-meta-script ejc-db :views)
   :rows-limit 0
   :column-width-limit 0
   :display-result t))

;;;###autoload
(defun ejc-show-user-types-list (&optional owner)
  "Output user types list."
  (interactive)
  (ejc-check-connection)
  (ejc-eval-user-sql (ejc-select-db-meta-script ejc-db :types
                                                :owner owner)))

;;;###autoload
(defun ejc-show-constraints-list (&optional owner table)
  "Output constraints list."
  (interactive)
  (ejc-check-connection)
  (ejc-eval-user-sql (ejc-select-db-meta-script ejc-db :constraints
                                                :owner owner
                                                :table table)))

;;;###autoload
(defun ejc-show-procedures-list ()
  "Output procedures list."
  (interactive)
  (ejc-check-connection)
  (ejc-eval-user-sql
   (ejc-select-db-meta-script ejc-db :procedures
                              :owner (ejc-get-this-owner ejc-db))
   :rows-limit 0
   :column-width-limit 0
   :display-result t))

;;;###autoload
(defun ejc-get-temp-editor-buffer (&optional num)
  (interactive "P")
  "Switch to buffer dedicated to ad-hoc edit and SQL scripts.
If the buffer is not exists - create it.
Buffer can be saved to file with `ejc-temp-editor-file' path."
  (let* ((tmp-file-name (if num
                            (format "%s-%s"
                                    ejc-temp-editor-buffer-name
                                    (if (numberp num)
                                        (int-to-string num)
                                      num))
                          ejc-temp-editor-buffer-name))
         (tmp-file-path (progn
                          (unless (file-exists-p ejc-temp-editor-file-path)
                            (make-directory ejc-temp-editor-file-path t))
                          (expand-file-name (concat tmp-file-name ".sql")
                                            ejc-temp-editor-file-path)))
         (tmp-buffer-name (format "*%s*" tmp-file-name)))
    (if (get-buffer tmp-buffer-name)
        (switch-to-buffer tmp-buffer-name)
      (progn
        (find-file tmp-file-path)
        (rename-buffer tmp-buffer-name)
        (sql-mode)
        (ejc-add-connection)
        (get-buffer tmp-buffer-name)))))

(defun ejc-open-log ()
  (interactive)
  (find-file-read-only (ejc-get-log-file-path))
  (goto-char (point-max)))

;;;###autoload
(defun ejc-version ()
  "Get the ejc-sql version as string."
  (interactive)
  (if (require 'pkg-info nil t)
      (message "ejc-sql %s" (pkg-info-version-info 'ejc-sql))
    (error "Cannot determine version without package pkg-info")))

(provide 'ejc-sql)

;;; ejc-sql.el ends here
