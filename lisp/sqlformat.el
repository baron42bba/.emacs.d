;;; sqlformat.el --- Use sqlformat to make SQL readable in Emacs

;; Copyright 2015, steckerhalter
;; Copyright 2015, Friedrich Paetzke <paetzke@fastmail.fm>

;; Author: steckerhalter
;; URL: https://github.com/steckerhalter/sqlformat.el
;; Keywords: sql format region buffer

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides the `sqlformat' commmand, which use the external
;; "sqlformat" tool to format SQL in the current region or buffer.

;; it is based on `format-sql.el' by Friedrich Paetzke, see:
;; https://github.com/paetzke/format-sql.el
;; but uses the sqlformat command instead of format-sql

;;; Code:

(defgroup sqlformat nil
  "Use sqlformat to sort the imports in a Python buffer."
  :group 'convenience
  :prefix "sqlformat-")

(defcustom sqlformat-options '("--reindent"
                               "--keywords" "upper"
                               "--identifiers" "lower")
  "Options used for sqlformat."
  :group 'sqlformat
  :type '(repeat (string :tag "option")))

(defun sqlformat-apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in sqlformat-apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (goto-char (point-min))
                (forward-line (- from line-offset 1))
                (setq line-offset (+ line-offset len))
                (kill-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in sqlformat-apply-rcs-patch")))))))))

(defun sqlformat-replace-region (filename)
  (delete-region (region-beginning) (region-end))
  (insert-file-contents filename))

;;;###autoload
(defun sqlformat (&optional start end)
  "Uses the `sqlformat' tool to reformat the current buffer or region.
If a region is active it operates on that, otherwise on the buffer."
  (interactive "r")
  (when (not (executable-find "sqlformat"))
    (error "\"sqlformat\" command not found. Install sqlformat with \"pip install sqlparse\""))

  (let* ((tmpfile (make-temp-file "sqlformat" nil ".sql"))
         (patchbuf (get-buffer-create "*sqlformat patch*"))
         (resultbuf (get-buffer-create "*sqlformat result*"))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))

    (with-current-buffer resultbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (if (use-region-p)
        (write-region start end tmpfile)
      (write-region nil nil tmpfile))

    (if (zerop (eval `(call-process "sqlformat" nil resultbuf nil ,@sqlformat-options tmpfile)))
        (progn
          (with-current-buffer resultbuf
            (write-region nil nil tmpfile))
          (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
              (progn
                (kill-buffer resultbuf)
                (message "Buffer is already sqlformated"))
            (if (use-region-p)
                (progn
                  (delete-region start end)
                  (insert-file-contents tmpfile))
              (sqlformat-apply-rcs-patch patchbuf))
            (kill-buffer resultbuf)
            (message "Applied sqlformat.")))
      (error "Could not apply sqlformat. Check *sqlformat result* for details"))
    (kill-buffer patchbuf)
    (delete-file tmpfile)))

(provide 'sqlformat)

;;; sqlformat.el ends here
