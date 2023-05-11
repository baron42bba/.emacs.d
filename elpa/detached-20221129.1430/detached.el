;;; detached.el --- A package to launch, and manage, detached processes -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022  Free Software Foundation, Inc.

;; Author: Niklas Eklund <niklas.eklund@posteo.net>
;; Maintainer: detached.el Development <~niklaseklund/detached.el@lists.sr.ht>
;; URL: https://sr.ht/~niklaseklund/detached.el/
;; Version: 0.10.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience processes

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The detached package allows users to run shell commands detached from
;; Emacs.  These commands are launched in sessions, using the program
;; dtach[1].  These sessions can be easily created through the command
;; `detached-shell-command', or any of the commands provided by the
;; `detached-shell', `detached-eshell' and `detached-compile' extensions.

;; When a session is created, detached makes sure that Emacs is attached
;; to it the same time, which makes it a seamless experience for the
;; users.  The `detached' package internally creates a `detached-session'
;; for all commands.

;; [1] https://github.com/crigler/dtach

;;; Code:

;;;; Requirements

(require 'ansi-color)
(require 'autorevert)
(require 'comint)
(require 'notifications)
(require 'filenotify)
(eval-when-compile (require 'rx))
(require 'simple)
(require 'subr-x)
(require 'tramp)

;;;; Variables

;;;;; Customizable

(defcustom detached-session-directory
  (locate-user-emacs-file "detached/sessions")
  "The directory to store sessions."
  :type 'string
  :group 'detached)

(defcustom detached-db-directory
  (locate-user-emacs-file "detached")
  "The directory to store the `detached' database."
  :type 'string
  :group 'detached)

(defcustom detached-dtach-program "dtach"
  "The name of the dtach program."
  :type 'string
  :group 'detached)

(defcustom detached-tail-program "tail"
  "The name of the tail program."
  :type 'string
  :group 'detached)

(defcustom detached-grep-program "grep"
  "The name of the grep program."
  :type 'string
  :group 'detached)

(defcustom detached-tee-program "tee"
  "The name of the tee program."
  :type 'string
  :group 'detached)

(defcustom detached-script-program "script"
  "The name of the script program."
  :type 'string
  :group 'detached)

(defcustom detached-shell-program shell-file-name
  "Path to the shell to run the dtach command in."
  :type 'string
  :group 'detached)

(defcustom detached-session-context-lines 50
  "Number of context lines to display for a session."
  :type 'string
  :group 'detached)

(defcustom detached-show-session-context t
  "If session context should be shown when attaching."
  :type 'boolean
  :group 'detached)

(defcustom detached-terminal-data-command nil
  "The command for the tool script, which is used to record terminal data.
The variable needs to be set in order for terminal data to be
recorded, instead of plain-text data.

Acceptable values are
- `gnu/linux'
- `darwin'
- a custom string
"
  :type 'string
  :group 'detached)

(defcustom detached-plain-text-commands nil
  "A list of regexps for commands to run in plain-text mode."
  :type 'list
  :group 'detached)

(defcustom detached-annotation-format
  '((:width 3 :padding 2 :function detached--status-str :face detached-failure-face)
    (:width 3 :padding 4 :function detached--state-str :face detached-state-face)
    (:width 10 :padding 4 :function detached--host-str :face detached-host-face)
    (:width 40 :padding 4 :function detached--working-dir-str :face detached-working-dir-face)
    (:width 40 :padding 4 :function detached--metadata-str :face detached-metadata-face)
    (:width 10 :padding 4 :function detached--duration-str :face detached-duration-face)
    (:width 8 :padding 4 :function detached--size-str :face detached-size-face)
    (:width 12 :padding 4 :function detached--creation-str :face detached-creation-face))
  "The format of the annotations."
  :type '(repeat symbol)
  :group 'detached)

(defcustom detached-command-format
  '(:width 90 :padding 4 :function detached-command-str)
  "The format for displaying the command."
  :type 'integer
  :group 'detached)

(defcustom detached-shell-command-session-action
  '(:attach detached-shell-command-attach-session
            :view detached-view-dwim
            :run detached-start-shell-command-session)
  "Actions for a session created with `detached-shell-command'."
  :type 'plist
  :group 'detached)

(defcustom detached-session-command
  nil
  "Command to run in session."
  :group 'detached
  :type 'string)

(defcustom detached-session-environment
  nil
  "A property list with variables for session."
  :group 'detached
  :type 'plist)

(defcustom detached-shell-command-initial-input t
  "Variable to control initial command input for `detached-shell-command'.
If set to a non nil value the latest entry to
`detached-shell-command-history' will be used as the initial input in
`detached-shell-command' when it is used as a command."
  :type 'bool
  :group 'detached)

(defcustom detached-degraded-commands nil
  "A list of commands which `detached' should consider degraded."
  :type '(repeat (regexp :format "%v"))
  :group 'detached)

(defcustom detached-notification-function #'detached-state-transition-notifications-message
  "Variable to set which function to use to issue a notification."
  :type 'function
  :group 'detached)

(defcustom detached-detach-key "C-c C-d"
  "Variable to set the keybinding for detaching."
  :type 'string
  :group 'detached)

(defcustom detached-filter-ansi-sequences t
  "Variable to instruct `detached' to use `ansi-filter'."
  :type 'bool
  :group 'detached)

(defcustom detached-log-mode-hook '()
  "Hook for customizing `detached-log' mode."
  :type 'hook
  :group 'detached)

(defcustom detached-shell-mode-filter-functions
  '(detached--env-message-filter
    detached--dtach-eof-message-filter)
  "A list of filter functions that are run in `detached-shell-mode'."
  :type 'list
  :group 'detached)

(defcustom detached-dtach-socket-creation-delay 1.0
  "A maximum delay, in seconds, that is reasonable for dtach socket creation."
  :type 'float
  :group 'detached)

(defcustom detached-open-session-display-buffer-action
  '(display-buffer-pop-up-window)
  "The action used to display a detached session."
  :group 'detached
  :type 'sexp)

(defcustom detached-session-info-buffer-action
  '(display-buffer-in-side-window
    (side . bottom)
    (dedicated . t))
  "The action used to display a information about a detached session."
  :group 'detached
  :type 'sexp)

(defcustom detached-debug-enabled
  nil
  "If t enable debug messages in `detached'."
  :group 'detached
  :type 'boolean)

;;;;; Public

(defvar detached-session-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'detached-edit-session-annotation)
    (define-key map "d" #'detached-detach-session)
    (define-key map "D" #'detached-describe-duration)
    (define-key map "e" #'detached-edit-and-run-session)
    (define-key map "k" #'detached-kill-session)
    (define-key map "r" #'detached-rerun-session)
    (define-key map "S" #'detached-describe-session)
    (define-key map "w" #'detached-copy-session-command)
    (define-key map "W" #'detached-copy-session-output)
    map))

(defvar detached-enabled nil)

(defvar detached-session-mode 'attached
  "Mode of operation for session.
Valid values are: create, new and attach")

(defvar detached-session-origin nil
  "Variable to specify the origin of the session.")

(defvar detached-session-action nil
  "A property list of actions for a session.")

(defvar detached-shell-command-history nil
  "History of commands run with `detached-shell-command'.")

(defvar detached-local-session nil
  "If set to t enforces a local session.")

(defvar detached-compile-session-hooks nil
  "Hooks to run when compiling a session.")

(defvar detached-update-db-hooks nil
  "Hooks to run when the database is updated.")

(defvar detached-metadata-annotators-alist nil
  "An alist of annotators for metadata.")

(defvar detached-session-annotation nil
  "An annotation string.")

(defconst detached-session-version "0.10.1.0"
  "The version of `detached-session'.
This version is encoded as [package-version].[revision].")

(defconst detached-minimum-session-version "0.9.2.2"
  "The version of `detached-session' that the package is compatible with.")

;;;;; Faces

(defgroup detached-faces nil
  "Faces used by `detached'."
  :group 'detached
  :group 'faces)

(defface detached-metadata-face
  '((t :inherit font-lock-builtin-face))
  "Face used to highlight metadata in `detached'.")

(defface detached-failure-face
  '((t :inherit error))
  "Face used to highlight failure in `detached'.")

(defface detached-state-face
  '((t :inherit success))
  "Face used to highlight state in `detached'.")

(defface detached-duration-face
  '((t :inherit font-lock-builtin-face))
  "Face used to highlight duration in `detached'.")

(defface detached-size-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight size in `detached'.")

(defface detached-creation-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight date in `detached'.")

(defface detached-working-dir-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight working directory in `detached'.")

(defface detached-host-face
  '((t :inherit font-lock-constant-face))
  "Face used to highlight host in `detached'.")

(defface detached-identifier-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight identifier in `detached'.")

(defface detached-mark-face
  '((t :inherit detached-state-face))
  "Face used to highlight marked session in `detached-list-mode'.")

(defface detached-annotation-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight the annotation of a session in `eldoc-mode'.")

(defface detached-uninitialized-face
  '((t :inherit font-lock-comment-face))
  "Face used to highlight that a session is uninitialized.")


;;;;; Private

(defvar detached--sessions-initialized nil
  "Sessions are initialized.")

(defvar detached--sessions nil
  "A list of sessions.")

(defvar detached--watched-session-directories nil
  "An alist where values are a (directory . descriptor).")

(defvar detached--hashed-sessions nil
  "Hashed sessions.")

(defvar detached--unvalidated-session-ids nil
  "A list of unvalidated session ids.")

(defvar detached--current-emacsen nil
  "List of current detached Emacsen.")

(defvar detached--db-watch nil
  "A descriptor to the `detached-db-directory'.")

(defvar-local detached-buffer-session nil
  "The `detached-session' session in current buffer.")

(defvar detached-current-session nil
  "The current session.")

(defvar detached--session-candidates nil
  "An alist of session candidates.")

(defvar detached--annotation-widths nil
  "An alist of widths to use for annotation.")

(defvar detached--update-database t
  "Update the database when value is t.")

(defconst detached--shell-command-buffer "*Detached Shell Command*"
  "Name of the `detached-shell-command' buffer.")

(defconst detached--dtach-eof-message "\\[EOF - dtach terminating\\]"
  "Message printed when `dtach' terminates.")

(defconst detached--dtach-detached-message "\\[detached\\]\^M"
  "Message printed when detaching from `dtach'.")

(defconst detached--dtach-detach-character "\C-\\"
  "Character used to detach from a session.")

;;;; Data structures

(cl-defstruct (detached-session (:constructor detached--session-create)
                                (:conc-name detached--session-))
  (id nil :read-only t)
  (command nil :read-only t)
  (origin nil :read-only t)
  (working-directory nil :read-only t)
  (initial-mode nil)
  (directory nil :read-only t)
  (metadata nil :read-only t)
  (host nil :read-only t)
  (degraded nil :read-only t)
  (text-mode nil :read-only t)
  (env nil :read-only t)
  (action nil :read-only t)
  (local nil :read-only t)
  (annotation)
  (time nil)
  (status nil)
  (size nil)
  (state nil)
  (initialized-emacsen nil))

;;;; Macros

(defmacro detached-connection-local-variables (&rest body)
  "A macro that conditionally use `connection-local-variables' when executing BODY."
  `(if detached-local-session
       (progn
         ,@body)
     (with-connection-local-variables
      (progn
        ,@body))))

(defmacro detached-with-session (session &rest body)
  "A macro that set up SESSION's environment before evaluating BODY."
  (declare (indent 1))
  `(let ((default-directory (detached-session-working-directory ,session))
         (detached-session-origin (detached--session-origin ,session))
         (detached-local-session (detached--session-local-p ,session))
         (detached-session-mode (detached--session-initial-mode ,session))
         (detached-session-action (detached--session-action ,session))
         (detached-session-command (detached-session-command ,session))
         (detached-session-environment (detached--session-env ,session))
         (detached-current-session ,session))
     ,@body))

;;;; Commands

;;;###autoload
(defun detached-shell-command (command &optional suppress-output)
  "Execute COMMAND with `detached'.

Optionally SUPPRESS-OUTPUT if prefix-argument is provided."
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Detached shell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          "Detached shell command: ")
                        (when detached-shell-command-initial-input
                          (car detached-shell-command-history))
                        'detached-shell-command-history)
    current-prefix-arg))
  (let* ((detached-session-origin (or detached-session-origin 'shell-command))
         (detached-session-action (or detached-session-action
                                      detached-shell-command-session-action))
         (detached-session-mode (or detached-session-mode
                                    (if suppress-output 'detached 'attached)))
         (session (detached-create-session command)))
    (detached-start-session session)))

;;;###autoload
(defun detached-open-session (session)
  "Open a `detached' SESSION."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (let ((initialized-session (detached--get-initialized-session session)))
      (if (detached-session-active-p initialized-session)
          (detached-attach-session initialized-session)
        (funcall
         (detached-session-view-function initialized-session)
         initialized-session)))))

;;;###autoload
(defun detached-compile-session (session)
  "Compile SESSION.

The session is compiled by opening its output and enabling
`compilation-minor-mode'."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (let ((buffer-name "*detached-session-output*")
          (file
           (detached--session-file session 'log))
          (tramp-verbose 1))
      (when (file-exists-p file)
        (with-current-buffer (get-buffer-create buffer-name)
          (let ((inhibit-read-only t))
            (setq-local buffer-read-only nil)
            (erase-buffer)
            (insert (detached-session-output session))
            (setq-local default-directory
                        (detached-session-working-directory session))
            (run-hooks 'detached-compile-session-hooks)
            (detached-log-mode)
            (compilation-minor-mode)
            (setq detached-buffer-session session)
            (setq-local font-lock-defaults '(compilation-mode-font-lock-keywords t)))
          (font-lock-mode)
          (read-only-mode)
          (goto-char (point-max)))
        (select-window
         (display-buffer buffer-name detached-open-session-display-buffer-action))))))

;;;###autoload
(defun detached-edit-session-annotation (session)
  "Edit SESSION's annotation."
  (interactive
   (list (detached-session-in-context)))
  (when-let* ((initial-value (or
                              (detached--session-annotation session)
                              ""))
              (annotation (read-string "Annotation: " initial-value)))
    (setf (detached--session-annotation session) annotation)
    (detached--db-update-entry session)))

;;;###autoload
(defun detached-edit-and-run-session (session &optional toggle-session-mode)
  "Edit and re-run SESSION at point.

Optionally TOGGLE-SESSION-MODE."
  (interactive
   (list (detached-session-in-context)
         current-prefix-arg))
  (when session
    (detached-with-session session
      (when-let* ((detached-session-command
                   (read-string "Edit command: "
                                (detached-session-command session)))
                  (detached-session-mode
                   (if toggle-session-mode
                       (if (eq 'detached (detached--session-initial-mode session))
                           'attached
                         'detached)
                     (detached--session-initial-mode session)))
                  (detached-current-session
                   (detached-create-session detached-session-command)))
        (detached-start-session detached-current-session)))))

;;;###autoload
(defun detached-rerun-session (session &optional toggle-session-mode)
  "Re-run SESSION at point.

Optionally TOGGLE-SESSION-MODE."
  (interactive
   (list (detached-session-in-context)
         current-prefix-arg))
  (when session
    (detached-with-session session
      (let* ((detached-session-mode
              (if toggle-session-mode
                  (if (eq 'detached (detached--session-initial-mode session))
                      'attached
                    'detached)
                (detached--session-initial-mode session)))
             (detached-current-session (detached-create-session detached-session-command)))
        (detached-start-session detached-current-session)))))

;;;###autoload
(defun detached-describe-session ()
  "Describe current session."
  (interactive)
  (when-let* ((session (detached-session-in-context))
              (buffer (get-buffer-create "*detached-session-info*"))
              (window (display-buffer buffer detached-session-info-buffer-action)))
    (select-window window)
    (with-current-buffer buffer
      (erase-buffer)
      (insert
       (string-trim
        (detached--session-header session)))
      (goto-char (point-min)))))

;;;###autoload
(defun detached-describe-duration (session)
  "Describe the SESSION's duration statistics."
  (interactive
   (list (detached-session-in-context)))
  (let* ((statistics (detached-session-duration-statistics session)))
    (message "%s: %s %s: %s"
             (propertize "μ" 'face 'detached-mark-face)
             (if-let ((mean (plist-get statistics :mean)))
                  (detached--duration-str2 (plist-get statistics :mean)) "-")
             (propertize "σ" 'face 'detached-mark-face)
             (if-let ((std (plist-get statistics :std)))
                 (detached--duration-str2 std) "-"))))

;;;###autoload
(defun detached-attach-session (session)
  "Attach to SESSION."
  (interactive
   (list (detached-session-in-context)))
  (when (and session
             (detached--valid-dtach-executable-p session))
    (let ((initialized-session (detached--get-initialized-session session)))
      (if (detached-session-inactive-p initialized-session)
          (detached-open-session initialized-session)
        (funcall (detached-session-attach-function initialized-session)
                 initialized-session)))))

;;;###autoload
(defun detached-copy-session-output (session)
  "Copy SESSION's output."
  (interactive
   (list (detached-session-in-context)))
  (when session
    (with-temp-buffer
      (insert (detached-session-output session))
      (when (detached-session-terminal-data-p session)
        ;; Enable `detached-log-mode' to parse ansi-escape sequences
        (detached-log-mode))
      (kill-new (buffer-string)))))

;;;###autoload
(defun detached-copy-session-command (session)
  "Copy SESSION's command."
  (interactive
   (list (detached-session-in-context)))
  (kill-new (detached-session-command session)))

;;;###autoload
(defun detached-insert-session-command (session)
  "Insert SESSION's command."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (insert (detached-session-command session))))

;;;###autoload
(defun detached-delete-session (session)
  "Delete SESSION."
  (interactive
   (list (detached-session-in-context)))
  (when session
    (if (detached-session-active-p session)
        (message "Kill session first before removing it.")
      (detached--db-remove-entry session))))

;;;###autoload
(defun detached-kill-session (session &optional delete)
  "Send a TERM signal to SESSION.

Optionally DELETE the session if prefix-argument is provided."
  (interactive
   (list (detached-session-in-context)
         current-prefix-arg))
  (when (detached-valid-session session)
    (when-let* ((default-directory (detached-session-directory session)))
      (if (derived-mode-p 'comint-mode)
          (call-interactively #'comint-interrupt-subjob)
        (detached-session-kill session)))
    (when delete
      (detached--db-remove-entry session))))

;;;###autoload
(defun detached-view-session (session)
  "View the SESSION."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (when (detached-valid-session session)
    (let* ((buffer-name "*detached-session-output*")
           (file-path
            (detached--session-file session 'log))
           (tramp-verbose 1))
      (if (file-exists-p file-path)
          (progn
            (with-current-buffer (get-buffer-create buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (detached-session-output session))
                (setq-local default-directory (detached-session-working-directory session))
                (detached-log-mode))
              (setq detached-buffer-session session)
              (goto-char (point-max)))
            (select-window
             (display-buffer buffer-name detached-open-session-display-buffer-action)))
        (message "Detached can't find file: %s" file-path)))))

;;;###autoload
(defun detached-diff-session (session1 session2)
  "Diff SESSION1 with SESSION2."
  (interactive
   (let ((sessions (detached-get-sessions)))
     `(,(detached-completing-read sessions)
       ,(detached-completing-read sessions))))
  (when (and (detached-valid-session session1)
             (detached-valid-session session2))
    (let ((buffer1 "*detached-session-output-1*")
          (buffer2 "*detached-session-output-2*"))
      (with-current-buffer (get-buffer-create buffer1)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (detached--session-header session1))
          (insert (detached-session-output session1))
          (when (detached-session-terminal-data-p session1)
            ;; Enable `detached-log-mode' to parse ansi-escape sequences
            (detached-log-mode))))
      (with-current-buffer (get-buffer-create buffer2)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (detached--session-header session2))
          (insert (detached-session-output session2))
          (when (detached-session-terminal-data-p session2)
            ;; Enable `detached-log-mode' to parse ansi-escape sequences
            (detached-log-mode))))
      (ediff-buffers buffer1 buffer2))))

;;;###autoload
(defun detached-open-session-directory (session)
  "Open SESSION's log directory."
  (interactive
   (list (detached-completing-read (detached-get-sessions))))
  (let* ((file-path
          (detached--session-file session 'log))
         (tramp-verbose 1))
    (when (file-exists-p file-path)
      (dired-jump-other-window file-path))))

;;;###autoload
(defun detached-detach-session ()
  "Detach from session in current buffer.

This command is only activated if `detached-buffer-session' is an
active session.  For sessions created with `detached-compile' or
`detached-shell-command', the command will also kill the window."
  (interactive)
  (if-let ((has-session (detached-session-p detached-buffer-session)))
      (detached--detach-session major-mode)
    (message "No detached-session found in buffer.")))

;;;###autoload
(defun detached-delete-sessions (&optional all-hosts)
  "Delete `detached' sessions which belong to the current host, unless ALL-HOSTS."
  (interactive "P")
  (let* ((host-name (car (detached--host)))
         (sessions (if all-hosts
                       (detached-get-sessions)
                     (seq-filter (lambda (it)
                                   (string= (detached-session-host-name it) host-name))
                                 (detached-get-sessions)))))
    (seq-do #'detached--db-remove-entry sessions)))

;;;; Functions

;;;;; Session

(defun detached-create-session (command)
  "Create a `detached' session from COMMAND."
  (detached-connection-local-variables
   (detached--create-session-directory)
   (let ((session
          (detached--session-create :id (intern (detached--create-id command))
                                    :command command
                                    :origin detached-session-origin
                                    :action detached-session-action
                                    :working-directory (detached--get-working-directory)
                                    :degraded (detached-degraded-command-p command)
                                    :initial-mode detached-session-mode
                                    :time `(:start 0.0 :end 0.0 :duration 0.0 :offset 0.0)
                                    :status '(unknown . 0)
                                    :annotation detached-session-annotation
                                    :local detached-local-session
                                    :size 0
                                    :directory (detached--get-session-directory)
                                    :text-mode (detached--text-mode command)
                                    :env detached-session-environment
                                    :host (detached--host)
                                    :metadata (detached-metadata)
                                    :state 'unknown
                                    :initialized-emacsen `(,(emacs-pid)))))
     (detached--db-insert-entry session)
     session)))

(defun detached--start-session-process (session start-command)
  "Start SESSION with START-COMMAND."
  (detached-watch-session session)
  (if (detached--session-local-p session)
      (apply #'start-process-shell-command `("detached" nil ,start-command))
    (apply #'start-file-process-shell-command `("detached" nil ,start-command))))

(defun detached-session-candidates (sessions)
  "Return an alist of SESSIONS candidates."
  (when sessions
    (setq detached--annotation-widths
          (detached--annotation-widths sessions detached-annotation-format))
    (let ((command-length
           (thread-last sessions
                        (seq-map #'detached-session-command)
                        (seq-map #'length)
                        (seq-max)
                        (min (plist-get detached-command-format :width)))))
      (let ((command-fun (plist-get detached-command-format :function)))
        (setq detached--session-candidates
              (thread-last sessions
                           (seq-map (lambda (it)
                                      `(,(apply command-fun `(,it ,command-length))
                                        . ,it)))
                           (detached--session-deduplicate)
                           (seq-map (lambda (it)
                                      `(,(concat (car it)
                                                 (make-string (plist-get detached-command-format :padding) ?\s))
                                        . ,(cdr it))))))))))

(defun detached-session-annotation (item)
  "Associate ITEM to a session and return ts annotation."
  (let ((session (cdr (assoc item detached--session-candidates))))
    (string-join
     (cl-loop for annotation in detached-annotation-format
              collect (let ((str (funcall (plist-get annotation :function) session))
                            (width (alist-get (plist-get annotation :function) detached--annotation-widths)))
                        (when (> width 0)
                          (concat
                           (truncate-string-to-width
                            (propertize str 'face (plist-get annotation :face))
                            width
                            0 ?\s)
                           (make-string (plist-get annotation :padding) ?\s)))))
     "")))

;;;###autoload
(defun detached-initialize-sessions ()
  "Initialize `detached' sessions from the database."

  ;; Initialize sessions
  (unless detached--sessions-initialized
    (unless (file-exists-p detached-db-directory)
      (make-directory detached-db-directory t))
    (detached--db-initialize)
    (detached--register-detached-emacs)
    (setq detached--db-watch
          (file-notify-add-watch detached-db-directory
                                 '(change attribute-change)
                                 #'detached--db-directory-event))

    (setq detached--sessions-initialized t)

    ;; Hash sessions and set the status for all sessions to uninitialized
    (setq detached--hashed-sessions
          (let* ((sessions (detached--db-get-sessions))
                 (ht (make-hash-table :test #'equal :size (length sessions))))
            (seq-do (lambda (session)
                      (puthash (detached-session-id session) 'uninitialized ht))
                    sessions)
            ht))

    ;; Initialize accessible sessions
    (let ((detached--current-emacsen (detached--active-detached-emacsen)))
      (detached--update-detached-emacsen)
      (let ((detached--update-database nil))
        (thread-last (detached--db-get-sessions)
                     (seq-filter #'detached--session-accessible-p)
                     (seq-do #'detached--initialize-session)))
      (detached--db-update-sessions))))

(defun detached-valid-session (session)
  "Ensure that SESSION is valid.

If session is not valid trigger an automatic cleanup on SESSION's host."
  (when (detached-session-p session)
    (if (not (detached--session-missing-p session))
        t
      (let ((hostname (detached-session-host-name session)))
        (message "Session does not exist. Initiate session cleanup on host %s" hostname)
        (detached--cleanup-host-sessions hostname)
        nil))))

(defun detached-session-exit-code-status (session)
  "Return status based on exit-code in SESSION."
  (let ((detached-env-message
         (with-temp-buffer
           (insert-file-contents (detached--session-file session 'log))
           (goto-char (point-max))
           (thing-at-point 'line t)))
        (failure-message (rx "detached-exit-code: " (group (one-or-more digit)))))
    (cond ((null detached-env-message) `(success . 0))
          ((string-match failure-message detached-env-message)
           `(failure . ,(string-to-number (match-string 1 detached-env-message))))
          (t `(success . 0)))))

(defun detached-state-transitionion-echo-message (session)
  "Issue a notification when SESSION transitions from active to inactive.
This function uses the echo area."
  (let ((status (pcase (detached-session-status session)
                  ('success "Detached finished")
                  ('failure "Detached failed"))))
    (message "%s [%s]: %s" status (detached-session-host-name session) (detached-session-command session))))

(defun detached-state-transition-notifications-message (session)
  "Issue a notification when SESSION transitions from active to inactive.
This function uses the `notifications' library."
  (let ((status (detached-session-status session))
        (host (detached-session-host-name session)))
    (notifications-notify
     :title (pcase status
              ('success (format "Detached finished [%s]" host))
              ('failure (format "Detached failed [%s]" host)))
     :body (detached-session-command session)
     :urgency (pcase status
                ('success 'normal)
                ('failure 'critical)))))

(defun detached-view-dwim (session)
  "View SESSION in a do what I mean fashion."
  (let ((status (detached-session-status session)))
    (cond ((eq 'success status)
           (detached-view-session session))
          ((eq 'failure status)
           (detached-compile-session session))
          ((eq 'unknown status)
           (detached-view-session session))
          (t (message "Detached session is in an unexpected state.")))))

(defun detached-get-sessions ()
  "Return as initialized sessions as possible."
  ;; Try to initialize unknown sessions
  (when-let* ((detached--update-database nil)
              (initialized-sessions
               (thread-last (detached--uninitialized-sessions)
                            (seq-filter #'detached--session-accessible-p)
                            (seq-do #'detached--initialize-session))))
    (detached--db-update-sessions))
  (detached--db-get-sessions))

(defun detached-shell-attach-session (session)
  "Attach to SESSION.

`comint-add-to-input-history' is temporarily disabled to avoid
cluttering the `comint-history' with dtach commands."
  (interactive
   (list (detached-select-host-session)))
  (when (detached-valid-session session)
    (if (detached-session-active-p session)
        (cl-letf ((detached-current-session session)
                  (comint-input-sender #'detached-shell--attach-input-sender)
                  ((symbol-function 'comint-add-to-input-history) (lambda (_) t)))
          (setq detached-buffer-session session)
          (let ((kill-ring nil))
            (comint-kill-input))
          (insert "[attached]")
          (comint-send-input))
      (detached-open-session session))))

(defun detached-shell-command-attach-session (session)
  "Attach to SESSION with `async-shell-command'."
  (let* ((inhibit-message t))
    (detached-with-session session
      (cl-letf* (((symbol-function #'set-process-sentinel) #'ignore)
                 (detached-session-mode 'attached)
                 (buffer (get-buffer-create detached--shell-command-buffer))
                 (default-directory (detached-session-directory session))
                 (command (detached-session-attach-command session :type 'string)))
        (when (get-buffer-process buffer)
          (setq buffer (generate-new-buffer (buffer-name buffer))))
        (funcall #'async-shell-command command buffer)
        (with-current-buffer buffer
          (setq-local default-directory (detached-session-working-directory session))
          (setq detached-buffer-session detached-current-session))))))

(defun detached-start-shell-command-session (session)
  "Start SESSION as a `shell-command'."
  (cl-letf* ((inhibit-message t)
             (default-directory (if (detached--session-local-p session)
                                    (detached-session-directory session)
                                  (detached-session-working-directory session)))
             ((symbol-function #'set-process-sentinel) #'ignore)
             (buffer (detached--generate-buffer detached--shell-command-buffer
                                                (lambda (buffer)
                                                  (not (get-buffer-process buffer)))))
             (command (detached-session-start-command session
                                                      :type 'string)))
    (detached-watch-session session)
    (funcall #'async-shell-command command buffer)
    (with-current-buffer buffer
      (setq detached-buffer-session session))))

(defun detached-start-session (session)
  "Start SESSION."
  (when (detached--valid-dtach-executable-p session)
    (if (eq 'detached (detached--session-initial-mode session))
        (detached--start-session-process session
                                         (detached-session-start-command
                                          session
                                          :type 'string))
      (detached-with-session session
        (funcall (detached-session-run-function session) session)))))

(defun detached-watch-session (session)
  "Start to watch SESSION.

If there is no file-notify watch on SESSION's directory it should be
added.  This watch will ensure that when a socket is created the
session is set to active and when the socket is deleted it is set to
inactive.  However there could be situations where the watch creation
is delayed and takes place after the socket appears.  This is most
likely to happen on remote hosts.  If so we fallback to a timer for
session validation."
  (detached--create-session-validator session)
  (detached--watch-session-directory (detached-session-directory session)))

;;;;; Public session functions

(cl-defun detached-session-start-command (session &key type)
  "Return command to start SESSION with specified TYPE."
  (when (detached--valid-dtach-executable-p session)
    (detached-watch-session session)
    (detached-connection-local-variables
     (let* ((socket (detached--session-file session 'socket t))
            (detached-session-mode (detached--session-initial-mode session))
            (log (detached--session-file session 'log t))
            (dtach-arg (if (eq 'detached (detached--session-initial-mode session))
                           "-n"
                         "-c"))
            (dtach-command-fun (lambda (session)
                                 (let ((detached-session-mode (detached--session-initial-mode session)))
                                   `(,detached-dtach-program
                                     ,dtach-arg
                                     ,socket
                                     "-z"
                                     ,detached-shell-program
                                     "-c"
                                     ,(if (eq type 'string)
                                          (shell-quote-argument (detached--detached-command session))
                                        (detached--detached-command session))))))
            (command
             (if (eq 'detached (detached--session-initial-mode session))
                 (funcall dtach-command-fun session)
               (if (not (detached-session-degraded-p session))
                   (funcall dtach-command-fun session)
                 (detached--start-session-process session
                                                  (string-join
                                                   (funcall dtach-command-fun session) " "))
                 `(,detached-tail-program
                   "-F"
                   "-n"
                   ,(number-to-string detached-session-context-lines)
                   ,log)))))
       (pcase type
         ('string (string-join command " "))
         ('list command)
         (_ nil))))))

(cl-defun detached-session-attach-command (session &key type)
  "Return command to attach SESSION with specified TYPE."
  (when (detached--valid-dtach-executable-p session)
    (detached-connection-local-variables
     (let* ((socket (detached--session-file session 'socket t))
            (log (detached--session-file session 'log t))
            (command
             (if (detached-session-degraded-p session)
                 `(,detached-tail-program "-F"
                                          "-n"
                                          ,(number-to-string detached-session-context-lines)
                                          ,log)
               (append
                (when detached-show-session-context
                  `(,detached-tail-program "-n"
                                           ,(number-to-string detached-session-context-lines)
                                           ,(concat log ";")))
                `(,detached-dtach-program "-a"
                                          ,socket
                                          "-r" "none")))))
       (pcase type
         ('string (string-join command " "))
         ('list command)
         (_ nil))))))


(defun detached-session-kill (session)
  "Kill SESSION."
  (interactive
   (list (detached-session-in-context)))
  (when session
    (if (detached-session-active-p session)
        (cl-letf* (((getenv "HISTFILE") "")
                   (default-directory (detached-session-directory session))
                   (buffer (get-buffer-create (format "*dtach-%s*" (detached-session-id session))))
                   (termination-delay 0.5)
                   (comint-exec-hook
                    `(,(lambda ()
                         (when-let ((process (get-buffer-process (current-buffer))))
                           (run-with-timer termination-delay nil
                                           (lambda ()
                                             ;; Attach to session
                                             (with-current-buffer buffer
                                               (let ((detached-show-session-context nil))
                                                 (when detached-debug-enabled
                                                   (message "Kill function attaching to session %s" (detached-session-id session)))
                                                 (detached-shell-attach-session session))
                                               (run-with-timer termination-delay nil
                                                               (lambda ()
                                                                 ;; Send termination signal to session
                                                                 (when detached-debug-enabled
                                                                   (message "Kill function sending termination signal to session %s" (detached-session-id session)))
                                                                 (with-current-buffer buffer
                                                                   (call-interactively #'comint-interrupt-subjob)
                                                                   (let ((kill-buffer-query-functions nil))
                                                                     (kill-buffer)))))))))))))
          (apply #'make-comint-in-buffer `("dtach" ,buffer ,detached-shell-program nil)))
      (message "Session %s is already inactive." (detached-session-id session)))))

(defun detached-session-output (session)
  "Return content of SESSION's output."
  (let* ((filename (detached--session-file session 'log))
         (detached-message
          (rx (regexp "\\[detached-exit-code: .*\\]"))))
    (with-temp-buffer
      (insert-file-contents filename)
      (detached--maybe-watch-session session)
      (goto-char (point-max))
      (forward-line -1)
      (let ((beginning (point-min))
            (end (if (string-match detached-message (thing-at-point 'line t))
                     (and (forward-line -1) (point))
                   (point-max))))
        (buffer-substring beginning end)))))

(defun detached-session-state (session)
  "Return SESSION's state."
  (detached--session-state session))

(defun detached-session-status (session)
  "Return status for SESSION."
  (pcase-let ((`(,status . ,_exit-code)
               (detached--session-status session)))
    status))

(defun detached-session-host-name (session)
  "Return SESSION's host name."
  (pcase-let ((`(,name . ,_type)
               (detached--session-host session)))
    name))

(defun detached-session-start-time (session)
  "Return SESSION's start time."
  (plist-get (detached--session-time session) :start))

(defun detached-session-end-time (session)
  "Return SESSION's end time."
  (plist-get (detached--session-time session) :end))

(defun detached-session-duration (session)
  "Return SESSION's duration."
  (if (detached-session-inactive-p session)
      (plist-get
       (detached--session-time session) :duration)
    (- (time-to-seconds) (detached-session-start-time session))))

(defun detached-session-host-type (session)
  "Return the type of SESSION's host."
  (pcase-let ((`(,_name . ,type)
               (detached--session-host session)))
    type))

(defun detached-session-exit-code (session)
  "Return exit code for SESSION."
  (pcase-let ((`(,_status . ,exit-code)
               (detached--session-status session)))
    exit-code))

(defun detached-session-id (session)
  "Return SESSION's id."
  (detached--session-id session))

(defun detached-session-identifier (session)
  "Return SESSION's identifier string."
  (string-join
   `(,(detached-session-command session)
     ,(detached--host-str session)
     ,(detached-session-working-directory session))
   ", "))

(defun detached-session-view-function (session)
  "Return SESSION's view function."
  (or
   (plist-get (detached--session-action session) :view)
   #'detached-view-dwim))

(defun detached-session-attach-function (session)
  "Return SESSION's attach function."
  (or
   (plist-get (detached--session-action session) :attach)
   #'detached-shell-command-attach-session))

(defun detached-session-run-function (session)
  "Return SESSION's run function."
  (or
   (plist-get (detached--session-action session) :run)
   #'detached-start-shell-command-session))

(defun detached-session-callback-function (session)
  "Return SESSION's callback function."
  (or
   (plist-get (detached--session-action session) :callback)
   #'ignore))

(defun detached-session-environment-property (session property)
  "Return PROPERTY from SESSION environment."
  (plist-get (detached--session-env session) property))

(defun detached-session-status-function (session)
  "Return SESSION's status function."
  (or
   (plist-get (detached--session-action session) :status)
   #'detached-session-exit-code-status))

(defun detached-session-command (session)
  "Return command run in SESSION."
  (detached--session-command session))

(defun detached-session-directory (session)
  "Return directory where SESSION files are located."
  (detached--session-directory session))

(defun detached-session-working-directory (session)
  "Return SESSION's working directory."
  (detached--session-working-directory session))

(defun detached-session-in-context ()
  "Return session from current context."
  (let ((session
         (or (detached--session-in-context major-mode)
             (detached-completing-read (detached-get-sessions)))))
    (when (detached-session-p session)
      session)))

(defun detached-session-duration-statistics (session)
  "Return duration statistics for SESSION."
  (when-let* ((identifier (detached-session-identifier session))
              (sessions (thread-last (detached-get-sessions)
                                     (seq-filter (lambda (it)
                                                   (string= identifier (detached-session-identifier it))))
                                     (seq-remove #'detached-session-failed-p)
                                     (seq-filter #'detached-session-inactive-p))))
    (detached--get-duration-statistics sessions)))

(defun detached-session-validated-p (session)
  "Return t if SESSION has been validated."
  (not
   (member (detached-session-id session)
           detached--unvalidated-session-ids)))

(defun detached-session-failed-p (session)
  "Return t if SESSION failed."
  (eq 'failure (detached-session-status session)))

(defun detached-session-remotehost-p (session)
  "Return t if SESSION is running on a remote host."
  (eq 'remotehost (detached-session-host-type session)))

(defun detached-session-localhost-p (session)
  "Return t if SESSION is running on the local host."
  (eq 'localhost (detached-session-host-type session)))

(defun detached-session-started-p (session)
  "Return t if SESSION has been started."
  (not (eq 'unknown (detached-session-state session))))

(defun detached-session-active-p (session)
  "Return t if SESSION is active."
  (eq 'active (detached-session-state session)))

(defun detached-session-inactive-p (session)
  "Return t if SESSION is inactive."
  (eq 'inactive (detached-session-state session)))

(defun detached-session-degraded-p (session)
  "Return t if SESSION is degraded."
  (detached--session-degraded session))

(defun detached-session-uninitialized-p (session)
  "Return t if SESSION is uninitialized."
  (eq 'uninitialized
      (gethash (detached-session-id session) detached--hashed-sessions)))

(defun detached-session-initialized-p (session)
  "Return t if SESSION is initialized."
  (eq 'initialized
      (gethash (detached-session-id session) detached--hashed-sessions)))

(defun detached-session-terminal-data-p (session)
  "Return t if SESSION is run with text mode set to terminal data."
  (eq 'terminal-data
      (detached--session-text-mode session)))

(defun detached-session-watched-p (session)
  "Return t if SESSION is being watched."
  (detached--watched-session-directory-p
   (detached-session-directory session)))

;;;;; Other

(defun detached-degraded-command-p (command)
  "Return t if COMMAND is degraded."
  (>
   (thread-last detached-degraded-commands
                (seq-filter (lambda (regexp)
                              (string-match-p regexp command)))
                (length))
   0))

(defun detached-metadata ()
  "Return a property list with metadata."
  (let ((metadata '()))
    (seq-doseq (annotator detached-metadata-annotators-alist)
      (push `(,(car annotator) . ,(funcall (cdr annotator))) metadata))
    metadata))

(defun detached-select-host-session ()
  "Return selected session."
  (let* ((host-name (car (detached--host)))
         (sessions
          (thread-last (detached-get-sessions)
                       (seq-filter (lambda (it)
                                     (string= (detached-session-host-name it) host-name)))
                       (seq-filter #'detached-session-active-p))))
    (detached-completing-read sessions)))

(defun detached-completing-read (sessions)
  "Select a session from SESSIONS through `completing-read'."
  (let* ((candidates (detached-session-candidates sessions))
         (metadata `(metadata
                     (category . detached)
                     (cycle-sort-function . identity)
                     (display-sort-function . identity)
                     (annotation-function . detached-session-annotation)
                     (affixation-function .
                                          ,(lambda (cands)
                                             (seq-map (lambda (s)
                                                        `(,s nil ,(detached-session-annotation s)))
                                                      cands)))))
         (collection (lambda (string predicate action)
                       (if (eq action 'metadata)
                           metadata
                         (complete-with-action action candidates string predicate))))
         (cand (completing-read "Select session: " collection nil t)))
    (detached--decode-session cand)))

(defun detached-command-str (session max-length)
  "Return SESSION's command as a string restrict it to MAX-LENGTH."
  (let ((command (detached-session-command session)))
    (if (<= (length command) max-length)
        (truncate-string-to-width
         command
         max-length
         0 ?\s)
      (concat (substring (detached-session-command session) 0 (- max-length 3)) "..."))))

;;;; Support functions

;;;;; Session

(defun detached--determine-session-state (session)
  "Return t if SESSION is active."
  (if (file-exists-p
       (detached--session-file session 'socket))
      'active
    'inactive))


(defun detached--valid-dtach-executable-p (session)
  "Verify that dtach executable is accessible for SESSION."
  (let ((default-directory (detached-session-directory session)))
    (detached-connection-local-variables
     (if (executable-find detached-dtach-program t)
         t
       (error "The `detached-dtach-program' doesn't contain a path to a dtach executable")))))

(defun detached--state-transition-p (session)
  "Return t if SESSION has transitioned from active to inactive."
  (and
   (eq 'active (detached--session-state session))
   (eq 'inactive (detached--determine-session-state session))))

(defun detached--session-accessible-p (session)
  "Return t if SESSION is accessible."
  (or (detached-session-localhost-p session)
      (file-remote-p (detached-session-directory session) nil t)))

(defun detached--watched-session-directory-p (directory)
  "Return t if DIRECTORY is being watched."
  (alist-get directory
             detached--watched-session-directories
             nil nil #'string=))

(defun detached--session-local-p (session)
  "Return t if SESSION is forced to run locally."
  (detached--session-local session))

(defun detached--session-missing-p (session)
  "Return t if SESSION is missing."
  (not
   (file-exists-p
    (detached--session-file session 'log))))

(defun detached--session-header (session)
  "Return header for SESSION."
  (string-join
   `(,(format "Command: %s" (detached-session-command session))
     ,(format "Working directory: %s" (detached--working-dir-str session))
     ,(format "Host: %s" (detached-session-host-name session))
     ,(format "Id: %s" (symbol-name (detached-session-id session)))
     ,(format "Status: %s" (detached-session-status session))
     ,(format "Annotation: %s" (if-let ((annotation (detached--session-annotation session))) annotation ""))
     ,(format "Exit-code: %s" (detached-session-exit-code session))
     ,(format "Metadata: %s" (detached--metadata-str session))
     ,(format "Created at: %s" (detached--creation-str session))
     ,(format "Duration: %s\n" (detached--duration-str session))
     "")
   "\n"))

(defun detached--session-deduplicate (sessions)
  "Make car of SESSIONS unique by adding an identifier to it."
  (let* ((ht (make-hash-table :test #'equal :size (length sessions)))
         (occurences
          (thread-last sessions
                       (seq-group-by #'car)
                       (seq-map (lambda (it) (seq-length (cdr it))))
                       (seq-max)))
         (identifier-width (if (> occurences 1)
                               (+ (length (number-to-string occurences)) 3)
                             0))
         (reverse-sessions (seq-reverse sessions)))
    (dolist (session reverse-sessions)
      (if-let (count (gethash (car session) ht))
          (setcar session (format "%s%s" (car session)
                                  (truncate-string-to-width
                                   (propertize (format " (%s)" (puthash (car session) (1+ count) ht)) 'face 'detached-identifier-face)
                                   identifier-width 0 ?\s)))
        (puthash (car session) 0 ht)
        (setcar session (format "%s%s" (car session) (make-string identifier-width ?\s)))))
    (seq-reverse reverse-sessions)))

(defun detached--decode-session (item)
  "Return the session associated with ITEM."
  (cdr (assoc item detached--session-candidates)))

(defun detached--create-session-validator (session)
  "Create a function to validate SESSION.

It can take some time for a dtach socket to be created.  Therefore all
sessions are created with state unknown.  This function creates a
function to verify that a session was created correctly.  If the
session is missing its deleted from the database."
  (let ((session-id (detached-session-id session))
        (start-time
         `(:start ,(time-to-seconds (current-time)) :end 0.0 :duration 0.0 :offset 0.0)))
    (push session-id detached--unvalidated-session-ids)
    (run-with-timer detached-dtach-socket-creation-delay
                    nil
                    (lambda ()
                      (when (member session-id detached--unvalidated-session-ids)
                        (when detached-debug-enabled
                          (message "Session %s is set to active by validator" session-id))
                        (let ((session (detached--db-get-session session-id)))
                          (setq detached--unvalidated-session-ids
                                (delete session-id detached--unvalidated-session-ids))
                          (setf (detached--session-state session) 'active)
                          (setf (detached--session-time session) start-time)
                          (detached--db-update-entry session)))))))

(defun detached--session-file (session file &optional local)
  "Return the full path to SESSION's FILE.

Optionally make the path LOCAL to host."
  (let* ((file-name
          (concat
           (symbol-name
            (detached-session-id session))
           (pcase file
             ('socket ".socket")
             ('log ".log"))))
         (remote-local-path (file-remote-p (expand-file-name file-name (detached-session-directory session)) 'localname))
         (full-path (expand-file-name file-name (detached-session-directory session))))
    (if (and local remote-local-path)
        remote-local-path
      full-path)))

(defun detached--cleanup-host-sessions (hostname)
  "Run cleanup on HOSTNAME sessions."
  (thread-last (detached--db-get-sessions)
               (seq-filter (lambda (it) (string= hostname (detached-session-host-name it))))
               (seq-filter #'detached--session-missing-p)
               (seq-do #'detached--db-remove-entry)))

(defun detached--maybe-watch-session (session)
  "Maybe watch SESSION."
  (and (detached-session-active-p session)
       (not (detached-session-watched-p session))
       (detached--watch-session-directory
        (detached-session-directory session))))

(defun detached--create-session-directory ()
  "Create session directory if it doesn't exist."
  (let ((directory (detached--get-session-directory)))
    (unless (file-exists-p directory)
      (make-directory directory t))))

(defun detached--get-working-directory ()
  "Return an abbreviated working directory path."
  (if-let (remote (file-remote-p default-directory))
      (replace-regexp-in-string  (expand-file-name remote)
                                 (concat remote "~/")
                                 (expand-file-name default-directory))
    (abbreviate-file-name default-directory)))

(defun detached--get-session-directory ()
  "Return the session directory."
  (if detached-local-session
      detached-session-directory
    (concat (file-remote-p default-directory) detached-session-directory)))

(cl-defgeneric detached--session-in-context (_mode)
  "Default implementation."
  detached-buffer-session)

(cl-defgeneric detached--detach-session (_mode)
  "Default implementation."
  (message "`detached' doesn't know how to detach from a session in this mode"))

(cl-defmethod detached--detach-session ((_mode (derived-mode detached-log-mode)))
  "Detach from session when MODE is `detached-log-mode'."
  (detached--quit-session-buffer))

(cl-defmethod detached--detach-session ((_mode (derived-mode shell-mode)))
  "Detach from session when MODE is `shell-mode'."
  (if (detached-session-degraded-p detached-buffer-session)
      (comint-interrupt-subjob)
    (detached--detach-from-comint-process))
  (when (string-match detached--shell-command-buffer (buffer-name))
    (detached--quit-session-buffer)))

(cl-defmethod detached--detach-session ((_mode (derived-mode comint-mode)))
  "Detach from session when MODE is `comint-mode'."
  (detached--detach-from-comint-process)
  (detached--quit-session-buffer))

(defun detached--detach-from-comint-process ()
  "Detach from the underlying `comint' process."
  (when-let ((active-session (detached-session-active-p
                              (alist-get (detached-session-id detached-buffer-session)
                                         detached--sessions)))
             (dtach-process (get-buffer-process (current-buffer))))
    (comint-simple-send dtach-process detached--dtach-detach-character)
    (setq detached-buffer-session nil)))

(defun detached--quit-session-buffer ()
  "Quit session buffer."
  (message "[detached]")
  (let ((kill-buffer-query-functions nil))
    (if (= (length (window-list)) 1)
        (kill-buffer)
      (kill-buffer-and-window))))

(defun detached--get-duration-statistics (sessions)
  "Return a plist of duration statistics for SESSIONS."
  (let* ((durations (seq-map #'detached-session-duration sessions))
         (mean (/ (seq-reduce #'+ durations 0) (seq-length durations)))
         (std (sqrt (/ (seq-reduce (lambda (sum duration)
                                     (+ sum (* (- duration mean)
                                               (- duration mean))))
                                   durations
                                   0.0)
                       (seq-length durations)))))
    `(:durations ,durations :mean ,mean :std ,std)))

(defun detached-shell--attach-input-sender (proc _string)
  "Attach to `detached--session' and send the attach command to PROC."
  (let* ((input
          (detached-session-attach-command detached-current-session
                                           :type 'string)))
    (comint-simple-send proc input)))

;;;;; Database

(defun detached--db-initialize ()
  "Return all sessions stored in database."
  (let ((db (expand-file-name "detached-sessions.db" detached-db-directory)))
    (when (file-exists-p db)
      (with-temp-buffer
        (insert-file-contents db)
        (cl-assert (bobp))
        (if (detached--verify-db-compatibility)
            (setq detached--sessions
                  (read (current-buffer)))
          (warn "Detached database has version %s while minimum version is %s"
                (detached--db-session-version) detached-minimum-session-version))))))

(defun detached--db-session-version ()
  "Return `detached-session-version' from database."
  (let ((header (thing-at-point 'line))
        (regexp (rx "Detached Session Version: " (group (one-or-more (or digit punct))))))
    (string-match regexp header)
    (match-string 1 header)))

(defun detached--db-insert-entry (session)
  "Insert SESSION into `detached--sessions' and update database."
  (push `(,(detached-session-id session) . ,session) detached--sessions)
  (when detached--update-database
    (detached--db-update-sessions)))

(defun detached--db-remove-entry (session)
  "Remove SESSION from `detached--sessions', delete log and update database."
  (let ((log (detached--session-file session 'log)))
    (when (file-exists-p log)
      (delete-file log)))
  (setq detached--sessions
        (assq-delete-all (detached-session-id session) detached--sessions))
  (when detached--update-database
    (detached--db-update-sessions)))

(defun detached--db-update-entry (session)
  "Update SESSION in `detached--sessions' and the database."
  (setf (alist-get (detached--session-id session) detached--sessions) session)
  (when detached--update-database
    (detached--db-update-sessions)))

(defun detached--db-get-session (id)
  "Return session with ID."
  (alist-get id detached--sessions))

(defun detached--db-get-sessions ()
  "Return all sessions stored in the database."
  (seq-map #'cdr detached--sessions))

(defun detached--db-update-sessions ()
  "Write `detached--sessions' to database."
  (run-hooks 'detached-update-db-hooks)
  (let ((db (expand-file-name "detached-sessions.db" detached-db-directory)))
    (with-temp-file db
      (insert (format ";; Detached Session Version: %s\n\n" detached-session-version))
      (prin1 detached--sessions (current-buffer)))))

(defun detached--read-detached-emacsen ()
  "Read the PIDs of detached Emacsen."
  (let ((file (expand-file-name "detached-emacsen" detached-db-directory)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (cl-assert (bobp))
        (read (current-buffer))))))

(defun detached--register-detached-emacs ()
  "Register the Emacs process."
  (let* ((file (expand-file-name "detached-emacsen" detached-db-directory))
         (emacs-process (process-attributes (emacs-pid)))
         (emacsen (detached--read-detached-emacsen)))
    (setf (alist-get (emacs-pid) emacsen) emacs-process)
    (with-temp-file file
      (insert (format ";; Detached Emacsen\n\n"))
      (prin1 emacsen (current-buffer)))))

(defun detached--primary-detached-emacs-p (session)
  "Return t if current Emacs is primary Emacs for SESSION."
  (let ((emacsen (detached--read-detached-emacsen))
        (initialized-emacsen (detached--session-initialized-emacsen
                              session)))
    (thread-last initialized-emacsen
                 (seq-find (lambda (pid)
                             (when-let ((emacs-process (alist-get pid emacsen))
                                        (process-attrs (process-attributes pid)))
                               (and
                                ;; Make sure the args are the same
                                (string= (alist-get 'args emacs-process)
                                         (alist-get 'args process-attrs))
                                ;; Make sure process id of the parent is the same
                                (= (alist-get 'ppid emacs-process)
                                   (alist-get 'ppid process-attrs))
                                ;; Make sure current Emacs is the right Emacs
                                (= pid (emacs-pid)))))))))

(defun detached--update-detached-emacsen ()
  "Update list of detached Emacsen."
  (with-temp-file (expand-file-name "detached-emacsen" detached-db-directory)
    (insert (format ";; Detached Emacsen\n\n"))
    (prin1 detached--current-emacsen (current-buffer))))

(defun detached--active-detached-emacsen ()
  "Return a list of active detached Emacsen."
  (thread-last (detached--read-detached-emacsen)
               (seq-filter (lambda (emacs-process)
                             (when-let ((process-attrs (process-attributes (car emacs-process))))
                               (and
                                ;; Make sure the args are the same
                                (string= (alist-get 'args (cdr emacs-process))
                                         (alist-get 'args process-attrs))
                                ;; Make sure process id of the parent is the same
                                (= (alist-get 'ppid (cdr emacs-process))
                                   (alist-get 'ppid process-attrs))))))))

(defun detached--get-initialized-session (session)
  "Return an initialized copy of SESSION."
  (if (detached-session-uninitialized-p session)
      (progn
        (detached--initialize-session session)
        (detached--db-update-sessions)
        (detached--db-get-session (detached-session-id session)))
    session))

(defun detached--verify-db-compatibility ()
  "Verify that the database version is compatible with the package."
  (let ((minimum-version
         (detached--decode-version-string detached-minimum-session-version))
        (db-version
         (detached--decode-version-string (detached--db-session-version))))
    (if (> (plist-get db-version :major) (plist-get minimum-version :major))
        t
      (when (= (plist-get db-version :major) (plist-get minimum-version :major))
        (if (> (plist-get db-version :minor) (plist-get minimum-version :minor))
            t
          (when (= (plist-get db-version :minor) (plist-get minimum-version :minor))
            (if (> (plist-get db-version :patch) (plist-get minimum-version :patch))
                t
              (when (= (plist-get db-version :patch) (plist-get minimum-version :patch))
                (>= (plist-get db-version :revision) (plist-get minimum-version :revision))))))))))

;;;;; Other

(defun detached--decode-version-string (version)
  "Return a decode property list of VERSION."
  (let ((version-regexp
         (rx (group (one-or-more digit)) "."
             (group (one-or-more digit)) "."
             (group (one-or-more digit)) "."
             (group (one-or-more digit)))))
    (when (string-match version-regexp version)
      `(:major ,(string-to-number (match-string 1 version))
               :minor ,(string-to-number (match-string 2 version))
               :patch ,(string-to-number (match-string 3 version))
               :revision ,(string-to-number (match-string 4 version))))))

(defun detached--dtach-arg ()
  "Return dtach argument based on `detached-session-mode'."
  (pcase detached-session-mode
    ('detached "-n")
    ('attached "-c")
    ('attach "-a")
    (_ (error "`detached-session-mode' has an unknown value"))))

(defun detached--session-state-transition-update (session &optional approximate)
  "Update SESSION due to state transition.

Optionally specify if the end-time should be APPROXIMATE or not."
  (let ((session-size (file-attribute-size
                       (file-attributes
                        (detached--session-file session 'log))))
        (session-time (detached--update-session-time session approximate))
        (status-fun (detached-session-status-function session)))
    (setf (detached--session-size session) session-size)
    (setf (detached--session-time session) session-time)
    (setf (detached--session-state session) 'inactive)
    (setf (detached--session-status session) (funcall status-fun session)))

  ;; Send notification
  (funcall detached-notification-function session)

  ;; Update session in database
  (detached--db-update-entry session)

  ;; Execute callback
  (funcall (detached-session-callback-function session)
           session))

(defun detached--detached-command (session)
  "Return the detached command for SESSION.

If SESSION is degraded fallback to a command that doesn't rely on tee."
  (let* ((log (detached--session-file session 'log t))
         (begin-shell-group (if (string= "fish" (file-name-nondirectory detached-shell-program))
                                "begin;"
                              "{"))
         (end-shell-group (if (or (string= "fish" (file-name-nondirectory detached-shell-program)))
                              "end"
                            "}"))
         (redirect
          (if (detached--session-degraded session)
              (format "&> %s" log)
            (format "2>&1 | %s %s" detached-tee-program log)))
         (shell (format "%s -c" detached-shell-program))
         (command
          (shell-quote-argument
           (format "if %s; then true; else echo \"[detached-exit-code: $?]\"; fi"
                   (if (and (detached-session-terminal-data-p session)
                            detached-terminal-data-command)
                       (format "TERM=eterm-color %s"
                               (format
                                (detached--get-terminal-data-command)
                                (detached-session-command session)))
                     (detached-session-command session))))))
    (format "%s %s %s; %s %s" begin-shell-group shell command end-shell-group redirect)))

(defun detached--get-terminal-data-command ()
  "Return terminal data command."
  (pcase detached-terminal-data-command
    ('gnu/linux (concat detached-script-program " --quiet --flush --return --command \"%s\" /dev/null"))
    ('darwin (concat detached-script-program " -F -q /dev/null %s"))
    ((and (pred stringp) command) command)
    (_ (error "Unable to determine script command, set `detached-terminal-data-command' properly"))))

(defun detached--text-mode (command)
  "Return the text mode to run in COMMAND in."
  (if (seq-find (lambda (regexp)
                  (string-match-p regexp command))
                detached-plain-text-commands)
      'plain-text
    'terminal-data))

(defun detached--host ()
  "Return a cons with (host . type)."
  (let ((remote
         (and (file-remote-p default-directory)
              (not detached-local-session))))
    `(,(if remote (file-remote-p default-directory 'host) (system-name)) . ,(if remote 'remotehost 'localhost))))

(defun detached--update-session-time (session &optional approximate)
  "Update SESSION's time property.

If APPROXIMATE, use latest modification time of SESSION's
log to deduce the end time."
  (let* ((start-time (plist-get (detached--session-time session) :start))
         (end-time))
    (if approximate
        (setq end-time
              (time-to-seconds
               (file-attribute-modification-time
                (file-attributes
                 (detached--session-file session 'log)))))
      (setq end-time (time-to-seconds)))
    `(:start ,start-time :end ,end-time :duration ,(- end-time start-time))))

(defun detached--create-id (command)
  "Return a hash identifier for COMMAND."
  (let ((current-time (number-to-string (time-to-seconds))))
    (secure-hash 'md5 (concat command current-time))))

(defun detached--env-message-filter (str)
  "Remove `detached-env' message in STR."
  (replace-regexp-in-string "\n?.*detached-exit-code:.*\n?" "" str))

(defun detached--dtach-eof-message-filter (str)
  "Remove `detached--dtach-eof-message' in STR."
  (replace-regexp-in-string (format "\n?%s\^M\n" detached--dtach-eof-message) "" str))

(defun detached--dtach-detached-message-filter (str)
  "Remove `detached--dtach-detached-message' in STR."
  (replace-regexp-in-string (format "\n?%s\n" detached--dtach-detached-message) "" str))

(defun detached--watch-session-directory (session-directory)
  "Watch for events in SESSION-DIRECTORY."
  (unless (detached--watched-session-directory-p session-directory)
    (push
     `(,session-directory . ,(file-notify-add-watch
                              session-directory
                              '(change)
                              #'detached-session-directory-event))
     detached--watched-session-directories)))

(defun detached-session-directory-event (event)
  "Act on an EVENT in a directory in `detached--watched-session-directories'.

If event is caused by the deletion of a socket, locate the related
session and trigger a state transition."
  (pcase-let* ((`(,_ ,action ,file) event))
    ;; Session becomes inactive
    (when (and (eq action 'deleted)
               (string= "socket" (file-name-extension file)))

      (when-let* ((id (intern (file-name-base file)))
                  (session (detached--db-get-session id))
                  (session-directory (detached-session-directory session))
                  (is-primary
                   (detached--primary-detached-emacs-p session)))
        (when detached-debug-enabled
          (message "Session %s is set to inactive by notify-watch event" (detached-session-id session)))

        ;; Remove from un-validated sessions
        (setq detached--unvalidated-session-ids
              (delete id detached--unvalidated-session-ids))

        ;; Update session
        (detached--session-state-transition-update session)

        ;; Remove session directory from `detached--watch-session-directory'
        ;; if there is no active session associated with the directory
        (unless
            (thread-last (detached--db-get-sessions)
                         (seq-filter #'detached-session-active-p)
                         (seq-map #'detached-session-directory)
                         (seq-uniq)
                         (seq-filter (lambda (it) (string= it session-directory))))
          (file-notify-rm-watch
           (alist-get session-directory detached--watched-session-directories nil nil #'string=))
          (setq detached--watched-session-directories
                (assoc-delete-all session-directory detached--watched-session-directories)))))

    ;; Session becomes active
    (when (and (eq action 'created)
               (string= "log" (file-name-extension file)))
      (when-let* ((id (intern (file-name-base file)))
                  (session (detached--db-get-session id))
                  (session-directory (detached-session-directory session))
                  (is-primary (detached--primary-detached-emacs-p session)))
        (when detached-debug-enabled
          (message "Session %s is set to active by notify-watch event" (detached-session-id session)))
        (setq detached--unvalidated-session-ids
              (delete (detached-session-id session) detached--unvalidated-session-ids))
        (setf (detached--session-state session) 'active)
        (setf (detached--session-time session) `(:start ,(time-to-seconds (current-time)) :end 0.0 :duration 0.0 :offset 0.0))
        (detached--db-update-entry session)))))

(defun detached--initialize-session (session)
  "Initialize SESSION."
  (puthash (detached-session-id session) 'initialized detached--hashed-sessions)

  (let* ((emacsen
          (thread-last `(,(emacs-pid) ,@(detached--session-initialized-emacsen session))
                       (seq-filter (lambda (it)
                                     (alist-get it detached--current-emacsen)))
                       (seq-uniq))))
    (setf (detached--session-initialized-emacsen session) emacsen))

  (if (detached-session-active-p session)
      (if (detached--state-transition-p session)
          (detached--session-state-transition-update session 'approximate)
        (detached--db-update-entry session)
        (detached--watch-session-directory (detached-session-directory session)))))

(defun detached--uninitialized-sessions ()
  "Return a list of uninitialized sessions."
  (seq-filter #'detached-session-uninitialized-p
              (detached--db-get-sessions)))

(defun detached--db-directory-event (event)
  "Act on EVENT in `detached-db-directory'.

If event is cased by an update to the `detached' database, re-initialize
`detached--sessions'."
  (pcase-let* ((`(,_descriptor ,action ,file) event)
               (database-updated (and (string-match "detached-sessions.db$" file)
                                      (or (eq 'attribute-changed action)
                                          (eq 'changed action))))
               (detached--current-emacsen (detached--active-detached-emacsen)))
    (when database-updated
      ;; Re-initialize the sessions
      (detached--db-initialize)
      ;; Initialize unknown sessions
      (seq-do (lambda (session)
                (unless (gethash (detached-session-id session) detached--hashed-sessions)
                  (if (not (detached--session-accessible-p session))
                      (puthash (detached-session-id session) 'uninitialized detached--hashed-sessions)
                    (detached--initialize-session session))))
              (detached--db-get-sessions)))))

(defun detached--annotation-widths (sessions annotation-format)
  "Return widths for ANNOTATION-FORMAT based on SESSIONS."
  (seq-map (lambda (it) (detached--annotation-width sessions it)) annotation-format))

(defun detached--annotation-width (sessions annotation)
  "Determine width for ANNOTATION based on SESSIONS."
  (let ((annotation-fun (plist-get annotation :function))
        (width (plist-get annotation :width)))
    `(,annotation-fun .
                      ,(thread-last sessions
                                    (seq-map annotation-fun)
                                    (seq-map #'length)
                                    (seq-max)
                                    (min width)))))

(defun detached--generate-buffer (name reuse-p &optional number)
  "Reuse or generate new buffer like built-in function `generate-new-buffer'.
NAME is used the same way as `generate-new-buffer' but if a buffer which
REUSE-P for buffer returns nil, return buffer instead.

NUMBER is used internally for recursive calls, but can be used to
start searching at NUMBER offset."
  (let* ((buffer-name (if number
                          (format "%s<%d>" name number)
                        name))
         (buffer (get-buffer buffer-name))
         (number (or number 1)))
    (if (and buffer (not (funcall reuse-p buffer)))
        (detached--generate-buffer name reuse-p (1+ number))
      (get-buffer-create buffer-name))))

(defun detached--metadata-git-branch ()
  "Return current git branch."
  (let ((args '("symbolic-ref" "HEAD" "--short")))
    (with-temp-buffer
      (when (= 0 (apply #'process-file `("git" nil t nil ,@args)))
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position)))))))

;;;;; UI

(defun detached--metadata-str (session)
  "Return SESSION's metadata as a string."
  (string-join
   (thread-last (detached--session-metadata session)
                (seq-filter (lambda (it) (cdr it)))
                (seq-map
                 (lambda (it)
                   (concat (symbol-name (car it)) ": " (cdr it)))))
   ""))

(defun detached--duration-str (session)
  "Return SESSION's duration time."
  (if (detached-session-started-p session)
      (detached--duration-str2 (detached-session-duration session))
    ""))

(defun detached--duration-str2 (duration)
  "Return propertized DURATION."
  (let* ((duration (round duration))
         (hours (/ duration 3600))
         (minutes (/ (mod duration 3600) 60))
         (seconds (mod duration 60)))
    (cond ((> duration (* 60 60)) (format "%sh %sm %ss" hours minutes seconds))
          ((> duration 60) (format "%sm %ss" minutes seconds))
          (t (format "%ss" seconds)))))

(defun detached--creation-str (session)
  "Return SESSION's creation time."
  (if (detached-session-started-p session)
      (format-time-string
       "%b %d %H:%M"
       (detached-session-start-time session))
    ""))

(defun detached--size-str (session)
  "Return the size of SESSION's output."
  (if (detached-session-active-p session)
      ""
    (file-size-human-readable
     (detached--session-size session))))

(defun detached--status-str (session)
  "Return string if SESSION has failed."
  (pcase (detached-session-status session)
    ('failure "!")
    ('success "")
    ('unknown "")))

(defun detached--state-str (session)
  "Return string based on SESSION state."
  (pcase (detached-session-state session)
    ('active (if (detached--session-accessible-p session)
                 "*"
               "?"))
    ('inactive "")
    ('unknown "?")))

(defun detached--working-dir-str (session)
  "Return working directory of SESSION."
  (let ((working-directory
         (detached-session-working-directory session)))
    (if-let ((remote (file-remote-p working-directory)))
        (string-remove-prefix remote working-directory)
      working-directory)))

(defun detached--host-str (session)
  "Return host name of SESSION."
  (detached-session-host-name session))

;;;; Minor modes

(defvar detached-shell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd detached-detach-key) #'detached-detach-session)
    (define-key map (kbd "C-c C-.") #'detached-describe-session)
    map)
  "Keymap for `detached-shell-mode'.")

;;;###autoload
(define-minor-mode detached-shell-mode
  "Integrate `detached' in `shell-mode'."
  :lighter " detached-shell"
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (if detached-shell-mode
      (dolist (filter detached-shell-mode-filter-functions)
        (add-hook 'comint-preoutput-filter-functions filter 0 t))
    (dolist (filter detached-shell-mode-filter-functions)
      (remove-hook 'comint-preoutput-filter-functions filter t))))

;;;; Major modes

(defvar detached-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd detached-detach-key) #'detached-detach-session)
    (define-key map (kbd "C-c C-.") #'detached-describe-session)
    map)
  "Keymap for `detached-log-mode'.")

;;;###autoload
(define-derived-mode detached-log-mode nil "Detached Log"
  "Major mode for `detached' logs."
  (when detached-filter-ansi-sequences
    (comint-carriage-motion (point-min) (point-max))
    (set-buffer-modified-p nil)
    (ansi-color-apply-on-region (point-min) (point-max)))
  (read-only-mode t))

;; TODO Deprecate the following functions:

(cl-defgeneric detached--shell-command (entity &optional concat)
  "Return shell command for ENTITY optionally CONCAT.")

(cl-defmethod detached--shell-command ((command string) &optional concat)
  "Return shell command for COMMAND.

Optionally CONCAT the command return command into a string."
  (detached--shell-command (detached-create-session command) concat))

(cl-defmethod detached--shell-command ((session detached-session) &optional concat)
  "Return shell command for SESSION.

Optionally CONCAT the command return command into a string."
  (if (detached--session-degraded session)
      (detached--tail-command session concat)
    (detached--dtach-command session concat)))

(cl-defgeneric detached--tail-command (entity &optional concat)
  "Return tail command for ENTITY optionally CONCAT.")

(cl-defmethod detached--tail-command ((command string) &optional concat)
  "Return tail command for COMMAND.

Optionally CONCAT the command return command into a string."
  (detached--tail-command (detached-create-session command) concat))

(cl-defmethod detached--tail-command ((session detached-session) &optional concat)
  "Return tail command for SESSION.

Optionally CONCAT the command return command into a string."
  (detached-connection-local-variables
   (let* ((log (detached--session-file session 'log t))
          (tail-command `(,detached-tail-program "-F"
                                                 "-n"
                                                 ,(number-to-string detached-session-context-lines)
                                                 ,log)))
     (cond ((eq 'detached detached-session-mode)
            (detached--dtach-command session))
           ((eq 'attached detached-session-mode)
            (let ((detached-session-mode 'detached)
                  (detached-current-session session))
              (detached-start-session (detached-session-command session))
              (if concat
                  (string-join tail-command " ")
                tail-command)))
           ((eq 'attach detached-session-mode)
            (if concat
                (string-join tail-command " ")
              tail-command))))))

(cl-defgeneric detached--dtach-command (entity &optional concat)
  "Return dtach command for ENTITY optionally CONCAT.")

(cl-defmethod detached--dtach-command ((command string) &optional concat)
  "Return dtach command for COMMAND.

Optionally CONCAT the command return command into a string."
  (detached--dtach-command (detached-create-session command) concat))

(cl-defmethod detached--dtach-command ((session detached-session) &optional concat)
  "Return dtach command for SESSION.

Optionally CONCAT the command return command into a string."
  (detached-connection-local-variables
   (let* ((socket (detached--session-file session 'socket t))
          (log (detached--session-file session 'log t))
          (dtach-arg (detached--dtach-arg)))
     (if (eq detached-session-mode 'attach)
         (if concat
             (string-join
              `(,(when detached-show-session-context
                   (format  "%s -n %s %s;" detached-tail-program detached-session-context-lines log))
                ,detached-dtach-program
                ,dtach-arg
                ,socket
                "-r none")
              " ")
           (append
            (when detached-show-session-context
              `(,detached-tail-program "-n"
                                       ,(number-to-string detached-session-context-lines)
                                       ,(concat log ";")))
            `(,detached-dtach-program ,dtach-arg ,socket "-r" "none")))
       (if concat
           (string-join
            `(,detached-dtach-program
              ,dtach-arg
              ,socket "-z"
              ,detached-shell-program "-c"
              ,(shell-quote-argument (detached--detached-command session)))
            " ")
         `(,detached-dtach-program
           ,dtach-arg ,socket "-z"
           ,detached-shell-program "-c"
           ,(detached--detached-command session)))))))

(provide 'detached)

;;; detached.el ends here
