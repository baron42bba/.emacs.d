;;; outline-yaml.el --- Code folding and Outline for Yaml files  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.2
;; URL: https://github.com/jamescherti/outline-yaml.el
;; Keywords: outlines
;; Package-Requires: ((emacs "24.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode for code folding and outlining YAML files
;; in Emacs. It leverages the built-in `outline` package to create a structured
;; view of YAML files, allowing users to collapse and expand sections based on
;; indentation levels.

;; Outline Yaml files.

;;; Code:

(defgroup outline-yaml nil
  "Non-nil if outline-yaml mode mode is enabled."
  :group 'outline-yaml
  :prefix "outline-yaml-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/outline-yaml.el"))

(defvar outline-yaml-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `outline-yaml-minor-mode'.")

(defun outline-yaml--level ()
  "Determine the outline level for YAML mode based on indentation."
  (let* ((space-count (current-indentation))
         (spaces-per-level (cond ((boundp 'yaml-indent-offset)
                                  yaml-indent-offset)
                                 (t 1))))
    (+ 1 (/ space-count spaces-per-level))))

;;;###autoload
(define-minor-mode outline-yaml-minor-mode
  "Toggle `outline-yaml-minor-mode'."
  :lighter " OutlYaml"
  :keymap outline-yaml-minor-mode-map
  :group 'outline-yaml
  (if outline-yaml-minor-mode
      (progn
        (setq-local outline-level 'outline-yaml--level)
        (setq-local outline-heading-end-regexp "\n")
        (setq-local outline-regexp
                    (rx
                     bol
                     (or
                      ;; 1. Document separator "---"
                      (seq "---"
                           (seq (zero-or-more " \t")
                                (or (optional "#")
                                    eol)))

                      (seq
                       (zero-or-more (any " \t"))  ; Indentation
                       (or
                        ;; 2. List
                        (seq "-" (any " \t"))

                        ;; 3. Handling list items or keys in mappings
                        (seq (or
                              ;; Double quoted strings
                              (seq "\""
                                   ;; Matches an escaped quote or any
                                   ;; other character except a quote
                                   ;; or newline
                                   (zero-or-more
                                    (or (seq "\\" "\"")
                                        (not (in "\""
                                                 "\n"))))
                                   "\"")

                              ;; Single quoted strings
                              (seq "'"
                                   ;; Matches an escaped single quote
                                   ;; Matches any other character
                                   ;; except a single quote or newline
                                   (zero-or-more
                                    (or "''"
                                        (not (in "'"
                                                 "\n"))))
                                   "'")

                              ;; Unquoted keys
                              (one-or-more (not (in "#"
                                                    ":"
                                                    "\n"))))

                             ;; Colon indicating a key in YAML
                             ":"))))
                     (zero-or-more nonl)
                     eol))
        (outline-minor-mode 1))
    (outline-minor-mode -1)))

(provide 'outline-yaml)
;;; outline-yaml.el ends here
