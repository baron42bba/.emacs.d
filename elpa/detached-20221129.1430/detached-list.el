;;; detached-list.el --- Manage detached sessions -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

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

;; This is an interface to manage `detached' sessions.

;;; Code:

;;;; Requirements

(require 'detached)
(require 'hl-line)
(require 'imenu)
(require 'tabulated-list)

;;;; Variables

(defcustom detached-list-config
  `((:name "Command" :function detached-list--command-str :length 90)
    (:name "Status" :function detached-list--status-str :length 10)
    (:name "Host" :function detached--host-str :length 15 :face detached-host-face)
    (:name "Directory" :function detached--working-dir-str :length 40 :face detached-working-dir-face)
    (:name "Metadata" :function detached--metadata-str :length 30 :face detached-metadata-face)
    (:name "Duration" :function detached--duration-str :length 10 :face detached-duration-face)
    (:name "Created" :function detached--creation-str :length 20 :face detached-creation-face))
  "Configuration for `detached' list mode."
  :type '(repeat (plist :options ((:name symbol)
                                  (:function symbol)
                                  (:length symbol)
                                  (:face symbol))))
  :group 'detached)

(defcustom detached-list-display-buffer-action
  '(display-buffer-same-window
    (inhibit-same-window . nil))
  "The action used to display the detached list buffer."
  :group 'detached
  :type 'sexp)

(defcustom detached-list-open-session-display-buffer-action
  '(display-buffer-same-window
    (inhibit-same-window . nil))
  "The action used to display a detached session."
  :group 'detached
  :type 'sexp)

(defcustom detached-list-filters nil
  "An alist of custom filters that can be applied.

The filters are built using the different narrow functions that
detached list implements."
  :group 'detached
  :type '(alist :key-type string))

(defcustom detached-list-session-identifier-function
  #'detached-session-identifier
  "The function to use for identifying a session."
  :group 'detached
  :type 'sexp)

(defcustom detached-list-state-symbols
  '((unknown . "-")
    (active . "*")
    (failure . "!")
    (success . " ")
    (initially-attached . "o")
    (initially-detached . " "))
  "An alist of symbols to use to communicate different states."
  :group 'detached
  :type '(alist :key-type symbol :value-type string))

;;;; Private

(defvar detached-list--marked-sessions nil
  "A list of marked session ids.")
(defvar detached-list--narrow-criteria nil
  "A list of criteria to apply when displaying the sessions.")

;;;; Functions

(defun detached-list-imenu-index ()
  "Create an `imenu' index for `detached-list'."
  (let ((index))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((session (tabulated-list-get-id)))
        (push `(,(detached-session-command session) . ,(point))
              index))
      (forward-line 1))
    (seq-reverse index)))

(defun detached-list-eldoc (_callback)
  "A member of `eldoc-documentation-functions', for signatures."
  (let ((session (tabulated-list-get-id)))
    (when (detached-session-p session)
      (let ((strs `(,(when-let  ((annotation (detached--session-annotation session)))
                       (propertize annotation 'face 'detached-annotation-face))
                    ,(detached-session-command session))))
        (string-join (seq-remove #'null strs) "\n")))))

;;;; Commands

(defun detached-list-initialize-session-directory (&optional all)
  "Initialize a session-directory.

Optionally initialize ALL session-directories."
  (interactive "P")
  (if-let* ((uninitialized-directories
             (thread-last (detached-get-sessions)
                          (seq-filter #'detached-session-uninitialized-p)
                          (seq-map #'detached-session-directory)
                          (seq-uniq))))
      (if all
          (seq-do #'detached-list--initialize-directory uninitialized-directories)
        (when-let ((directory (completing-read "Initialize directory: " uninitialized-directories)))
          (detached-list--initialize-directory directory)))
    (message "All session directories have been initialized")))

(defun detached-list-quit ()
  "Quit command."
  (interactive)
  (if (= (length (window-list)) 1)
      (bury-buffer)
    (delete-window)))

(defun detached-list-remove-narrow-criterion ()
  "Remove narrow criterion."
  (interactive)
  (if detached-list--narrow-criteria
      (detached-list-narrow-sessions
       (butlast detached-list--narrow-criteria))
    (message "No criterion to remove")))

(defun detached-list-widen ()
  "Remove all narrowing restrictions."
  (interactive)
  (when detached-list--narrow-criteria
    (detached-list-narrow-sessions nil)))

(defun detached-list-detach-from-session (session)
  "Detach from SESSION at point."
  (interactive
   (list (tabulated-list-get-id)))
  (when-let* ((buffer (detached-list--attached-p session)))
    (unless (get-buffer-window buffer)
      (pop-to-buffer buffer))
    (with-selected-window (get-buffer-window buffer)
      (detached-detach-session))))

(defun detached-list-jump-to-directory (session)
  "Jump to SESSION at point's directory."
  (interactive
   (list (tabulated-list-get-id)))
  (detached-open-session-directory session))

(defun detached-list-kill-session ()
  "Send a TERM signal to sessions at point, or all marked sessions.

Optionally DELETE the session if prefix-argument is provided."
  (interactive)
  (when (y-or-n-p (if detached-list--marked-sessions
                      "Kill all marked sessions? "
                    "Kill session at point? "))
    (seq-do
     (lambda (session)
       (detached-list--unmark-session session)
       (detached-kill-session session current-prefix-arg))
     (detached-list--get-marked-or-current-sessions))
    (detached-list-revert)))

(defun detached-list-view-session (session)
  "View SESSION."
  (interactive
   (list (detached-session-in-context)))
  (let ((detached-open-session-display-buffer-action
         detached-list-open-session-display-buffer-action))
    (detached-view-dwim session)))

(defun detached-list-edit-and-run-session (session &optional toggle-suppress-output)
  "Edit and run SESSION at point.

Optionally TOGGLE-SUPPRESS-OUTPUT."
  (interactive
   (list (tabulated-list-get-id)
         current-prefix-arg))
  (let ((detached-session-mode
         (if toggle-suppress-output
             (if (eq 'detached (detached--session-initial-mode session))
                 'attached
               'detached)
           (detached--session-initial-mode session))))
    (unless (eq detached-session-mode 'detached)
      (when-let ((single-window (> (length (window-list)) 1))
                 (buffer (current-buffer)))
        (delete-window (get-buffer-window))
        (bury-buffer buffer)))
    (detached-edit-and-run-session session)))

(defun detached-list-rerun-session (session &optional toggle-session-mode)
  "Rerun SESSION at point.

Optionally TOGGLE-SESSION-MODE."
  (interactive
   (list (tabulated-list-get-id)
         current-prefix-arg))
  (let ((detached-session-mode
         (if toggle-session-mode
             (if (eq 'attached (detached--session-initial-mode session))
                 'attached
               'detached)
           (detached--session-initial-mode session))))
    (unless (eq detached-session-mode 'detached)
      (when-let ((single-window (> (length (window-list)) 1))
                 (buffer (current-buffer)))
        (delete-window (get-buffer-window))
        (bury-buffer buffer)))
    (detached-rerun-session session)))

(defun detached-list-diff-marked-sessions ()
  "Diff two sessions."
  (interactive)
  (if (= (length detached-list--marked-sessions) 2)
      (progn
        (when-let ((single-window (> (length (window-list)) 1))
                   (buffer (current-buffer)))
          (delete-window (get-buffer-window))
          (bury-buffer buffer))
        (apply #'detached-diff-session detached-list--marked-sessions))
    (message "Mark two sessions")))

(defun detached-list-open-session ()
  "View session."
  (interactive)
  (let ((session (tabulated-list-get-id))
        (detached-open-session-display-buffer-action
         detached-list-open-session-display-buffer-action))
    (when-let ((single-window (> (length (window-list)) 1))
               (buffer (current-buffer)))
      (delete-window (get-buffer-window))
      (bury-buffer buffer))
    (detached-open-session session)))

(defun detached-list-narrow-unique ()
  "Narrow to unique sessions."
  (interactive)
  (when detached-list-session-identifier-function
    (detached-list-narrow-sessions
     `(,@detached-list--narrow-criteria
       ("Unique" .
        ,(lambda (sessions)
           (thread-last sessions
                        (seq-group-by detached-list-session-identifier-function)
                        (seq-map (lambda (it)
                                   (pcase-let ((`(,_identifier . ,duplicate-sessions) it))
                                     (car duplicate-sessions))))
                        (seq-sort-by #'detached-session-start-time #'>))))))))

(defun detached-list-narrow-after-time (time-threshold)
  "Narrow to session's created after TIME-THRESHOLD."
  (interactive
   (list
    (read-string "Enter time: ")))
  (when time-threshold
    (if-let ((parsed-threshold (detached--list-parse-time time-threshold)))
        (detached-list-narrow-sessions
         `(,@detached-list--narrow-criteria
           (,(format "+%s" time-threshold) .
            ,(lambda (sessions)
               (let ((current-time (time-to-seconds (current-time))))
                 (seq-filter (lambda (it)
                               (< (- current-time
                                     (detached-session-start-time it))
                                  parsed-threshold))
                             sessions))))))
      (message "Cannot parse time"))))

(defun detached-list-narrow-before-time (time-threshold)
  "Narrow to session's created before TIME-THRESHOLD."
  (interactive
   (list
    (read-string "Enter time: ")))
  (when time-threshold
    (if-let ((parsed-threshold (detached--list-parse-time time-threshold)))
        (detached-list-narrow-sessions
         `(,@detached-list--narrow-criteria
           (,(format "-%s" time-threshold) .
            ,(lambda (sessions)
               (let ((current-time (time-to-seconds (current-time))))
                 (seq-filter (lambda (it)
                               (> (- current-time
                                     (detached-session-start-time it))
                                  parsed-threshold))
                             sessions))))))
      (message "Cannot parse time"))))

(defun detached-list-narrow-host (hostname)
  "Narrow to sessions from a selected HOSTNAME."
  (interactive
   (list
    (when-let* ((hostnames
                 (thread-last (detached-list--get-narrowed-sessions)
                              (seq-map #'detached-session-host-name)
                              (seq-uniq))))
      (completing-read
       "Select host: "
       hostnames))))
  (when hostname
    (detached-list-narrow-sessions
     `(,@detached-list--narrow-criteria
       (,(concat "Host: " hostname) .
        ,(lambda (sessions)
           (seq-filter (lambda (it)
                         (string-match hostname
                                       (detached-session-host-name it)))
                       sessions)))))))

(defun detached-list-narrow-output (regexp)
  "Narrow to sessions which output contain REGEXP."
  (interactive
   (list (read-regexp
          "Filter session outputs containing (regexp): ")))
  (when regexp
    (detached-list-narrow-sessions
     `(,@detached-list--narrow-criteria
       (,(concat "Output: " regexp) .
        ,(lambda (sessions)
           (detached--grep-sesssions-output sessions regexp)))))))

(defun detached-list-narrow-command (regexp)
  "Narrow to sessions which command match REGEXP."
  (interactive
   (list
    (if current-prefix-arg
        (regexp-quote
         (detached-session-command
          (detached-session-in-context)))
      (read-regexp
       "Filter session commands containing (regexp): "))))
  (when regexp
    (detached-list-narrow-sessions
     `(,@detached-list--narrow-criteria
       (,(concat "Command: " regexp) .
        ,(lambda (sessions)
           (seq-filter (lambda (it)
                         (string-match regexp
                                       (detached-session-command it)))
                       sessions)))))))

(defun detached-list-narrow-working-directory (regexp)
  "Narrow to sessions with working directory matching REGEXP."
  (interactive
   (list
    (if current-prefix-arg
        (regexp-quote
         (detached-session-working-directory
          (detached-session-in-context)))
      (read-regexp
       "Filter session working directories containing (regexp): "))))
  (when regexp
    (detached-list-narrow-sessions
     `(,@detached-list--narrow-criteria
       (,(concat "Working-Directory: " regexp) .
        ,(lambda (sessions)
           (seq-filter (lambda (it)
                         (string-match regexp
                                       (detached-session-working-directory it)))
                       sessions)))))))

(defun detached-list-narrow-session-directory (session-directory)
  "Narrow to sessions with SESSION-DIRECTORY."
  (interactive
   (list
    (when-let* ((directories
                 (thread-last (detached-list--get-narrowed-sessions)
                              (seq-map #'detached-session-directory)
                              (seq-uniq))))
      (completing-read
       "Select session directory: "
       directories))))
  (when session-directory
    (detached-list-narrow-sessions
     `(,@detached-list--narrow-criteria
       (,(concat "Session-Directory: " session-directory) .
        ,(lambda (sessions)
           (seq-filter (lambda (it)
                         (string-match session-directory
                                       (detached-session-directory it)))
                       sessions)))))))

(defun detached-list-narrow-annotation (regexp)
  "Narrow to sessions which annotation match REGEXP."
  (interactive
   (list (read-regexp
          "Filter session annotations containing (regexp): ")))
  (when regexp
    (detached-list-narrow-sessions
     `(,@detached-list--narrow-criteria
       (,(concat "Annotation: " regexp) .
        ,(lambda (sessions)
           (seq-filter (lambda (it)
                         (when-let ((annotation (detached--session-annotation it)))
                           (string-match regexp annotation)))
                       sessions)))))))

(defun detached-list-narrow-localhost ()
  "Narrow to local-host sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(,@detached-list--narrow-criteria
     ("Localhost" .
      ,(lambda (sessions)
         (seq-filter #'detached-session-localhost-p sessions))))))

(defun detached-list-narrow-remotehost ()
  "Narrow to remote-host sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(,@detached-list--narrow-criteria
     ("Remotehost" .
      ,(lambda (sessions)
         (seq-filter #'detached-session-remotehost-p sessions))))))

(defun detached-list-narrow-currenthost ()
  "Narrow to current-host sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(,@detached-list--narrow-criteria
     ("Currenthost" .
      ,(lambda (sessions)
         (let ((current-host (car (detached--host))))
           (seq-filter (lambda (it)
                         (string= (detached-session-host-name it) current-host))
                       sessions)))))))

(defun detached-list-select-filter ()
  "Select filter from `detached-list-filter' to apply."
  (interactive)
  (when-let* ((metadata `(metadata
                          (category . detached)
                          (display-sort-function . identity)))
              (collection (lambda (string predicate action)
                            (if (eq action 'metadata)
                                metadata
                              (complete-with-action action detached-list-filters string predicate))))
              (filter-name
               (completing-read "Select filter: " collection nil t)))
    (detached-list--apply-filter
     (alist-get filter-name detached-list-filters nil nil #'string=))))

(defun detached-list-narrow-origin (origin)
  "Narrow to sessions with a specific ORIGIN."
  (interactive
   (list
    (when-let ((origins
                (thread-last (detached-list--get-narrowed-sessions)
                             (seq-map #'detached--session-origin)
                             (seq-uniq)
                             (seq-remove #'null)
                             (seq-map #'symbol-name))))
      (completing-read
       "Select origin: "
       origins))))
  (when origin
    (detached-list-narrow-sessions
     `(,@detached-list--narrow-criteria
       (,(concat "Origin: " origin) .
        ,(lambda (sessions)
           (seq-filter
            (lambda (it)
              (string-match origin
                            (symbol-name (detached--session-origin it))))
            sessions)))))))

(defun detached-list-narrow-active ()
  "Narrow to active sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(,@detached-list--narrow-criteria
     ("Active" .
      ,(lambda (sessions)
         (seq-filter #'detached-session-active-p sessions))))))

(defun detached-list-narrow-inactive ()
  "Narrow to inactive sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(,@detached-list--narrow-criteria
     ("Inactive" .
      ,(lambda (sessions)
         (seq-remove #'detached-session-active-p sessions))))))

(defun detached-list-narrow-success ()
  "Narrow to successful sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(,@detached-list--narrow-criteria
     ("Success" .
      ,(lambda (sessions)
         (seq-remove #'detached-session-failed-p sessions))))))

(defun detached-list-narrow-failure ()
  "Narrow to failed sessions."
  (interactive)
  (detached-list-narrow-sessions
   `(,@detached-list--narrow-criteria
     ("Failure" .
      ,(lambda (sessions)
         (seq-filter #'detached-session-failed-p sessions))))))

(defun detached-list-mark-regexp (regexp)
  "Mark sessions which command match REGEXP.

If prefix-argument is provided unmark instead of mark."
  (interactive
   (list (read-regexp
          (concat (if current-prefix-arg "Unmark" "Mark")
                  " session commands containing (regexp): "))))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((session (tabulated-list-get-id)))
        (when (string-match regexp (detached-session-command session))
          (if current-prefix-arg
              (detached-list--unmark-session session)
            (detached-list--mark-session session))))
      (forward-line))))

(defun detached-list-delete-session ()
  "Delete session at point, or all marked sessions."
  (interactive)
  (when (y-or-n-p (if detached-list--marked-sessions
                      "Delete all marked sessions? "
                    "Delete session at point? "))
    (let ((detached--update-database nil))
      (seq-do
       (lambda (session)
         (detached-list--unmark-session session)
         (detached-delete-session session))
       (detached-list--get-marked-or-current-sessions)))
    (detached--db-update-sessions)
    (detached-list-revert)))

(defun detached-list-mark-session ()
  "Mark session at point and advance to next session."
  (interactive)
  (let* ((session (tabulated-list-get-id)))
    (detached-list--mark-session session)
    (forward-line)))

(defun detached-list-unmark-session ()
  "Unmark session at point and advance to next session."
  (interactive)
  (let* ((session (tabulated-list-get-id)))
    (detached-list--unmark-session session)
    (forward-line)))

(defun detached-list-unmark-sessions ()
  "Unmark all sessions."
  (interactive)
  (setq detached-list--marked-sessions nil)
  (detached-list-revert))

(defun detached-list-toggle-mark-session ()
  "Toggle mark on session at point."
  (interactive)
  (let* ((session (tabulated-list-get-id)))
    (if (detached-list--marked-session-p session)
        (detached-list--unmark-session session)
      (detached-list--mark-session session))))

(defun detached-list-toggle-sessions ()
  "Toggle mark on all sessions."
  (interactive)
  (let* ((sessions (seq-map #'car tabulated-list-entries))
         (unmarked-sessions
          (seq-remove
           (lambda (session)
             (seq-find
              (lambda (marked-session)
                (eq (detached-session-id marked-session)
                    (detached-session-id session)))
              detached-list--marked-sessions))
           sessions)))
    (setq detached-list--marked-sessions unmarked-sessions)
    (detached-list-revert)))

(defun detached-list-revert ()
  "Update content in buffer."
  (interactive)
  (tabulated-list-revert)
  (detached-list--restore-marks)
  (when hl-line-mode (hl-line-highlight)))

;;;###autoload
(defun detached-list-sessions ()
  "Open list of `detached'."
  (interactive)
  (let* ((current-directory default-directory)
         (buffer (get-buffer-create "*detached-list*"))
         (window
          (or
           (get-buffer-window buffer)
           (display-buffer buffer detached-list-display-buffer-action))))
    (select-window window)
    (setq-local default-directory current-directory)
    (unless (eq major-mode 'detached-list-mode)
      (detached-list-mode)
      (setq tabulated-list-entries
            (seq-map #'detached-list--get-entry
                     (detached-list--get-narrowed-sessions)))
      (tabulated-list-print t))))

(defun detached-list-narrow-sessions (criteria)
  "Narrow session(s) based on CRITERIA."
  (setq detached-list--narrow-criteria criteria)
  (when (eq major-mode 'detached-list-mode)
    (setq tabulated-list-entries
          (seq-map #'detached-list--get-entry
                   (detached-list--get-narrowed-sessions)))
    (tabulated-list-print t)
    (detached-list-revert)))

;;;; Support functions

(defun detached-list--apply-filter (filter)
  "Apply FILTER."
  (setq detached-list--narrow-criteria nil)
  (seq-do (lambda (criteria) (apply criteria)) filter))

(defun detached--revert-selection-change (&rest _)
  "Revert function to add to `window-selection-change-functions'."
  (detached-list-revert))

(defun detached-list--initialize-directory (directory)
  "Initialize sessions in DIRECTORY."
  (thread-last (detached-get-sessions)
               (seq-filter (lambda (it) (string= directory (detached-session-directory it))))
               (seq-do #'detached--initialize-session)))

(defun detached--list-parse-time (time)
  "Return a value in seconds based on TIME."
  (when time
    (cond ((string-match (rx (group (one-or-more digit)) "h") time)
           (* 60 60 (string-to-number (match-string 1 time))))
          ((string-match (rx (group (one-or-more digit)) space "day" (zero-or-more "s")) time)
           (* 60 60 24 (string-to-number (match-string 1 time))))
          ((string-match (rx (group (one-or-more digit)) space "week" (zero-or-more "s")) time)
           (* 60 60 24 7 (string-to-number (match-string 1 time))))
          ((string-match (rx (group (one-or-more digit)) space "month" (zero-or-more "s")) time)
           (* 60 60 24 7 30 (string-to-number (match-string 1 time))))
          ('t nil))))

(defun detached-list--db-update ()
  "Function to run when the database is updated."
  (when-let ((detached-list-buffer (detached-list--get-list-mode-buffer)))
    (with-current-buffer detached-list-buffer
      (detached-list-revert))))

(defun detached-list--get-list-mode-buffer ()
  "Return buffer with `detached-list-mode'."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (eq major-mode 'detached-list-mode)))
            (buffer-list)))

(defun detached-list--revert-sessions ()
  "Recompute `tabulated-list-entries'."
  (setq tabulated-list-entries
        (seq-map #'detached-list--get-entry
                 (detached-list--get-narrowed-sessions))))

(defun detached-list--get-entry (session)
  "Return list entry based on SESSION."
  `(,session
    ,(cl-loop for config in detached-list-config
              vconcat `(,
                        (let ((str (funcall (plist-get config ':function) session)))
                          (if-let ((face (plist-get config :face)))
                              (propertize str 'face face)
                            str))))))

(defun detached-list--get-format ()
  "Return the format for `detached-list'."
  (cl-loop for config in detached-list-config
           vconcat `((,(plist-get config ':name)
                      ,(plist-get config ':length)
                      ,(plist-get config ':sort)))))

(defun detached-list--marked-session-p (session)
  "Return t if SESSION is marked."
  (seq-find (lambda (it)
              (eq (detached-session-id it)
                  (detached-session-id session)))
            detached-list--marked-sessions))

(defun detached-list--attached-p (session)
  "Return t if Emacs is attached to SESSION."
  (let ((id (detached-session-id session)))
    (seq-find
     (lambda (buffer)
       (with-current-buffer buffer
         (when-let ((buffer-session detached-buffer-session)
                    (buffer-session-id (detached-session-id buffer-session)))
           (eq buffer-session-id id))))
     (buffer-list))))

(defun detached-list--unmark-session (session)
  "Unmark SESSION."
  (when (detached-list--marked-session-p session)
    (tabulated-list-put-tag " ")
    (setq detached-list--marked-sessions
          (seq-remove (lambda (it)
                        (eq (detached-session-id it)
                            (detached-session-id session)))
                      detached-list--marked-sessions))))

(defun detached-list--mark-session (session)
  "Mark SESSION."
  (unless (detached-list--marked-session-p session)
    (tabulated-list-put-tag (detached-list--mark-identifier))
    (setq detached-list--marked-sessions
          (push session detached-list--marked-sessions))))

(defun detached-list--restore-marks ()
  "Restore mark(s) in `detached-list-mode' buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((session (tabulated-list-get-id)))
        (when (detached-list--marked-session-p session)
          (tabulated-list-put-tag (detached-list--mark-identifier))))
      (forward-line))))

(defun detached-list--mark-identifier ()
  "Return identifier for marked sessions."
  (let ((str "*"))
    (propertize str 'face 'detached-mark-face)))

(defun detached-list--status-str (session)
  "Return a string representation of SESSION's status."
  (let* ((status (detached-session-status session))
         (status-str
          (cond ((detached-session-active-p session)
                 (alist-get 'active detached-list-state-symbols "?"))
                ((and (detached-session-inactive-p session) (eq status 'failure))
                 (alist-get 'failure detached-list-state-symbols "?"))
                ((and (detached-session-inactive-p session) (eq status 'success))
                 (alist-get 'success detached-list-state-symbols "?"))
                ((not (detached-session-started-p session))
                 (alist-get 'unknown detached-list-state-symbols "?"))
                (t "?")))
         (status-face
          (cond ((and (detached-session-uninitialized-p session)
                      (detached-session-active-p session))
                 'detached-identifier-face)
                ((detached-session-active-p session) 'font-lock-type-face)
                ((eq status 'failure) 'detached-failure-face)
                ((eq status 'success) 'detached-state-face)
                (t 'detached-identifier-face)))
         (attach-str
          (cond ((eq 'attached (detached--session-initial-mode session))
                 (alist-get 'initially-attached detached-list-state-symbols "?"))
                ((eq 'detached (detached--session-initial-mode session))
                 (alist-get 'initially-detached detached-list-state-symbols "?"))
                (t "?")))
         (initial-mode-face
          (cond ((eq 'attached (detached--session-initial-mode session)) 'detached-identifier-face)
                ((eq 'detached (detached--session-initial-mode session)) 'detached-identifier-face)
                (t "?"))))
    (string-join
     `(,(propertize status-str 'face status-face)
       ,(propertize attach-str 'face initial-mode-face))
     " ")))

(defun detached-list--command-str (session)
  "Return command string for SESSION."
  (let ((command-str (detached-session-command session)))
    (if (detached-session-uninitialized-p session)
        (propertize command-str 'face 'detached-uninitialized-face)
      command-str)))

(defun detached-list--get-marked-or-current-sessions ()
  "Return a list of relevant sessions."
  (or detached-list--marked-sessions
      `(,(tabulated-list-get-id))))

(defun detached-list--get-narrowed-sessions ()
  "Return a list of narrowed sessions."
  (let ((sessions (detached-get-sessions)))
    (seq-do (lambda (it)
              (pcase-let ((`(,_identifier . ,criteria) it))
                (setq sessions (funcall criteria sessions))))
            detached-list--narrow-criteria)
    sessions))

(cl-defmethod detached--session-in-context ((_mode (derived-mode detached-list-mode)))
  "Return session when in `detached-list-mode'."
  (tabulated-list-get-id))

(defun detached--grep-sesssions-output (sessions regexp)
  "Return a narrowed list with SESSIONS containing REGEXP."
  (let* ((sessions-and-directories
          (thread-last sessions
                       (seq-group-by #'detached-session-directory)
                       (seq-filter (lambda (it)
                                     ;; Filter out only accessible directories
                                     (or (not (file-remote-p (car it)))
                                         (file-remote-p (car it) nil t))))))
         (session-ids
          (thread-last sessions-and-directories
                       (seq-map
                        (lambda (it)
                          (pcase-let* ((`(,session-directory . ,sessions) it)
                                       (default-directory session-directory)
                                       (includes
                                        (seq-map (lambda (session)
                                                   (format "--include=%s"
                                                           (file-name-nondirectory
                                                            (detached--session-file
                                                             session
                                                             'log))))
                                                 sessions))
                                       (grep-command
                                        (string-join `(,detached-grep-program
                                                       "--files-with-matches"
                                                       ,@includes
                                                       "--no-messages"
                                                       "--ignore-case"
                                                       "--recursive"
                                                       ,(format "\"%s\"" regexp))
                                                     " ")))
                            (split-string
                             (with-connection-local-variables
                              (with-temp-buffer
                                (process-file-shell-command grep-command nil t)
                                (buffer-string)))
                             "\n" t))))
                       (flatten-tree)
                       (seq-remove #'null)
                       (seq-map #'file-name-sans-extension))))
    (seq-filter (lambda (it)
                  (member (symbol-name (detached-session-id it)) session-ids))
                sessions)))

;;;; Major mode

(defvar detached-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'detached-edit-session-annotation)
    (define-key map (kbd "d") #'detached-list-delete-session)
    (define-key map (kbd "e") #'detached-edit-and-run-session)
    (define-key map (kbd "f") #'detached-list-select-filter)
    (define-key map (kbd "g") #'detached-list-revert)
    (define-key map (kbd "i") #'detached-list-initialize-session-directory)
    (define-key map (kbd "j") #'imenu)
    (define-key map (kbd "k") #'detached-list-kill-session)
    (define-key map (kbd "m") #'detached-list-mark-session)
    ;; Narrow
    (define-key map (kbd "n a") #'detached-list-narrow-annotation)
    (define-key map (kbd "n c") #'detached-list-narrow-command)
    (define-key map (kbd "n d") #'detached-list-narrow-session-directory)
    ;; Host
    (define-key map (kbd "n h h") #'detached-list-narrow-host)
    (define-key map (kbd "n h c") #'detached-list-narrow-currenthost)
    (define-key map (kbd "n h l") #'detached-list-narrow-localhost)
    (define-key map (kbd "n h r") #'detached-list-narrow-remotehost)
    (define-key map (kbd "n o") #'detached-list-narrow-output)
    (define-key map (kbd "n O") #'detached-list-narrow-origin)
    ;; State
    (define-key map (kbd "n s a") #'detached-list-narrow-active)
    (define-key map (kbd "n s f") #'detached-list-narrow-failure)
    (define-key map (kbd "n s i") #'detached-list-narrow-inactive)
    (define-key map (kbd "n s s") #'detached-list-narrow-success)
    (define-key map (kbd "n u") #'detached-list-narrow-unique)
    (define-key map (kbd "n w") #'detached-list-narrow-working-directory)
    (define-key map (kbd "n +") #'detached-list-narrow-after-time)
    (define-key map (kbd "n -") #'detached-list-narrow-before-time)
    (define-key map (kbd "q") #'detached-list-quit)
    (define-key map (kbd "r") #'detached-rerun-session)
    (define-key map (kbd "t") #'detached-list-toggle-mark-session)
    (define-key map (kbd "T") #'detached-list-toggle-sessions)
    (define-key map (kbd "u") #'detached-list-unmark-session)
    (define-key map (kbd "U") #'detached-list-unmark-sessions)
    (define-key map (kbd "v") #'detached-list-view-session)
    (define-key map (kbd "w") #'detached-copy-session-command)
    (define-key map (kbd "W") #'detached-copy-session-output)
    (define-key map (kbd "x") #'detached-list-detach-from-session)
    (define-key map (kbd "%") #'detached-list-mark-regexp)
    (define-key map (kbd "=") #'detached-list-diff-marked-sessions)
    (define-key map (kbd "-") #'detached-list-widen)
    (define-key map (kbd "!") #'detached-shell-command)
    ;; Describe
    (define-key map (kbd ". s") #'detached-describe-session)
    (define-key map (kbd ". d") #'detached-describe-duration)
    (define-key map (kbd "<backspace>") #'detached-list-remove-narrow-criterion)
    (define-key map (kbd "<return>") #'detached-list-open-session)
    map)
  "Keymap used in `detached-list-mode'.")

(defun detached-list--mode-line-indicator ()
  "Return the mode line indicator based on narrow criteria."
  (if detached-list--narrow-criteria
      (string-join
       (thread-last detached-list--narrow-criteria
                    (seq-map #'car))
       " > ")
    ""))

(define-derived-mode detached-list-mode tabulated-list-mode "Detached List"
  "Mode for `detached' list."
  (setq tabulated-list-format (detached-list--get-format))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq imenu-create-index-function #'detached-list-imenu-index)
  (setq-local eldoc-echo-area-use-multiline-p t)
  (setq-local eldoc-idle-delay 0)
  (hl-line-mode)
  (add-hook 'eldoc-documentation-functions #'detached-list-eldoc nil t)
  (add-hook 'tabulated-list-revert-hook #'detached-list--revert-sessions nil t)
  (add-hook 'window-selection-change-functions #'detached--revert-selection-change nil t)
  (setq-local mode-line-position '((:eval (detached-list--mode-line-indicator))))
  (tabulated-list-init-header))

(provide 'detached-list)

;;; detached-list.el ends here
