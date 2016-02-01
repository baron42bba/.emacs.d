;;; helm-safari.el --- Helm interface for Safari Bookmarks and History  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; Package-Requires: ((helm "1.9.1") (emacs "24"))
;; Version: 0.1
;; Package-Version: 20160115.2334
;; Keywords: tools

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

;;

;;; Code:

(require 'helm)

(defgroup helm-safari nil
  "Helm interface for Safari Bookmarks and History."
  :group 'helm)

(defun helm-safari-list-to-alist (list)
  ;; '(1 2 3 4) ⇒ ((2 . 1) (4 . 3))
  (let* ((idx 0)
         res)
    (while (< idx (length list))
      (push (cons (nth (1+ idx) list)
                  (nth idx list))
            res)
      (setq idx (+ idx 2)))
    (nreverse res)))

(defvar helm-safari-bookmarks-alist nil)
(defvar helm-source-safari-bookmarks
  (helm-build-sync-source "Safari Bookmarks"
    :init
    (lambda ()
      (setq helm-safari-bookmarks-alist
            (let ((bookmarks-list
                   (split-string
                    (shell-command-to-string
                     "plutil -p ~/Library/Safari/Bookmarks.plist | grep 'URLString\\|title' | sed 's/.* => \"\\(.*\\)\"/\\1/'")
                    "\n" t)))
              (helm-safari-list-to-alist bookmarks-list))))
    :candidates 'helm-safari-bookmarks-alist
    :action '(("Browse Url" . browse-url))))

;;;###autoload
(defun helm-safari-bookmarks ()
  "Search Safari Bookmark using `helm'."
  (interactive)
  (helm :sources 'helm-source-safari-bookmarks
        :prompt "Find Bookmark: "
        :buffer "*Helm Safari Bookmarks*"))

(defvar helm-safari-history-alist nil)
(defvar helm-source-safari-history
  (helm-build-sync-source "Safari History"
    :init
    (lambda ()
      (setq helm-safari-history-alist
            (let ((history-list
                   (split-string
                    (shell-command-to-string
                     "plutil -p ~/Library/Safari/History.plist | grep '\"title\"\\|\"\" ' | sed 's/.* => \"\\(.*\\)\"$/\\1/'")
                    "\n" t)))
              (helm-safari-list-to-alist history-list))))
    :candidates 'helm-safari-history-alist
    :action '(("Browse Url" . browse-url))))

;;;###autoload
(defun helm-safari-history ()
  "Search Safari Bookmark using `helm'."
  (interactive)
  (helm :sources 'helm-source-safari-history
        :prompt "Find Bookmark: "
        :buffer "*Helm Safari History*"))

(provide 'helm-safari)
;;; helm-safari.el ends here
