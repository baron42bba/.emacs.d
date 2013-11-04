;;; background.el --- fun with background jobs

;; Copyright (C) 1988 Joe Keane <jk3k+@andrew.cmu.edu>
;; Keywords: processes

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:

;; - Adapted to use comint and cleaned up somewhat. Olin Shivers 5/90
;; - Background failed to set the process buffer's working directory
;;   in some cases. Fixed. Olin 6/14/90
;; - Background failed to strip leading cd's off the command string
;;   after performing them. This screwed up relative pathnames.
;;   Furthermore, the proc buffer's default dir wasn't initialised 
;;   to the user's buffer's default dir before doing the leading cd.
;;   This also screwed up relative pathnames if the proc buffer already
;;   existed and was set to a different default dir. Hopefully we've
;;   finally got it right. The pwd is now reported in the buffer
;;   just to let the user know. Bug reported by Piet Van Oostrum.
;;   Olin 10/19/90
;; - Fixed up the sentinel to protect match-data around invocations.
;;   Also slightly rearranged the cd match code for similar reasons.
;;   Olin 7/16/91
;; - Dec 29 1995: changed for new stuff (shell-command-switch, second
;;   arg to shell-command --> BUFFER-NAME arg to background) from
;;   FSF 19.30.  Ben Wing

;;; Code:

(provide 'background)
(require 'comint)

(defgroup background nil
  "Fun with background jobs"
  :group 'processes)


;; user variables
(defcustom background-show t
  "*If non-nil, background jobs' buffers are shown when they're started."
  :type 'boolean
  :group 'background)
(defcustom background-select nil
  "*If non-nil, background jobs' buffers are selected when they're started."
  :type 'boolean
  :group 'background)

(defcustom background-get-job-name 'background-get-job-name-simple
  "Function to use to generate the job name (and therefore the buffer name
of processes run in the background."
  :type '(choice
           (function-item :tag "Simple Numbered"
                          :value background-get-job-name-simple)
           (function-item :tag "Command based"
                          :value background-get-job-name-command)
           (function-item :tag "Command and Directory based"
                          :value background-get-job-name-command-n-dir)
           )
  :group 'background)


(defun background-search-job-space (form)
  (let ((job-number 1))
    (while (get-process (format form job-number))
      (setq job-number (1+ job-number)))
    job-number))
  
(defun background-get-job-name-simple (command dir)
  (let* ((form "background-%d")
         (job-num (background-search-job-space form)))
    (format form job-num)))

(defun format-quote-string (str)
  (replace-in-string str "%" "%%"))

(defun get-bottom-dir (str)
  (let* ((dasplit (split-string str "\/"))
         (dalen (length dasplit)))
    (if (= dalen 2)
        (elt dasplit (- dalen 1))
      (if (= (length (elt dasplit (- dalen 1))) 0)
          (elt dasplit (- dalen 2))
        (elt dasplit (- dalen 1))))))

(defun background-get-job-name-command (command dir)
  (let* ((form (concat (format "BG(%s" (format-quote-string command)) ")%d"))
         (job-num (background-search-job-space form)))
    (format form job-num)))

(defun background-get-job-name-command-n-dir (command dir)
  (let* ((form (concat (format "BG(%s)(%s"
                               (format-quote-string (get-bottom-dir dir))
                               (format-quote-string command)) ")%d"))
         (job-num (background-search-job-space form)))
    (format form job-num)))

;;;###autoload
(defun background (command &optional buffer-name)
  "Run COMMAND in the background like csh.  
A message is displayed when the job starts and finishes.  The buffer is in
comint mode, so you can send input and signals to the job.  The process object
is returned if anyone cares.  See also comint-mode and the variables
background-show and background-select.

Optional second argument BUFFER-NAME is a buffer to insert the output into.
If omitted, a buffer name is constructed from the command run."
  (interactive "s%% ")
  (let* ((dir default-directory)
         (job-name (if (functionp background-get-job-name)
                       (apply background-get-job-name (list command dir))
                     (background-get-job-name-simple command dir))))
    (or buffer-name
	(setq buffer-name (format "*%s*" job-name)))
    (if background-select (pop-to-buffer buffer-name)
      (if background-show (with-output-to-temp-buffer buffer-name)) ; cute
      (set-buffer (get-buffer-create buffer-name)))
    (erase-buffer)

    (setq default-directory dir) ; Do this first, in case cd is relative path.
    (if (string-match "^cd[\t ]+\\([^\t ;]+\\)[\t ]*;[\t ]*" command)
	(let ((dir (substring command (match-beginning 1) (match-end 1))))
	   (setq command (substring command (match-end 0)))
	   (setq default-directory
		 (file-name-as-directory (expand-file-name dir)))))

    (insert-before-markers "--- working directory: " default-directory
	    "\n% " command ?\n)

    (let ((proc (get-buffer-process
		 (comint-exec buffer-name job-name shell-file-name
			      nil (list shell-command-switch command)))))
      (comint-mode)
      ;; COND because the proc may have died before the G-B-P is called.
      (cond (proc (set-process-sentinel proc 'background-sentinel)
		  (message "%d" (process-id proc))))
      (setq mode-name "Background")
      proc)))

(defun background-sentinel (process msg)
  "Called when a background job changes state."
  (let ((ms (match-data))) ; barf
    (unwind-protect
	 (let ((msg (cond ((string= msg "finished\n") "Done")
			  ((string-match "^exited" msg)
			   (concat "Exit " (substring msg 28 -1)))
			  ((zerop (length msg)) "Continuing")
			  (t (concat (upcase (substring msg 0 1))
				     (substring msg 1 -1))))))
	   (message "[%s] %s %s" (process-name process)
		    msg
		    (nth 2 (process-command process)))
	   (if (null (buffer-name (process-buffer process)))
	       (set-process-buffer process nil) ; WHY? Olin.
	       (if (memq (process-status process) '(signal exit))
		   (save-excursion
		     (set-buffer (process-buffer process))
		     (let ((at-end (eobp)))
		       (save-excursion
			 (goto-char (point-max))
			 (insert ?\n msg ? 
				 (substring (current-time-string) 11 19) ?\n))
		       (if at-end (goto-char (point-max))))
		     (set-buffer-modified-p nil)))))
      (store-match-data ms))))

;;; background.el ends here
