;;; detached-consult.el --- Detached interface using Consult multi sources -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022  Free Software Foundation, Inc.

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

;; This package integrates `detached' with `consult'[1].  The package
;; provides a command `detached-consult-session' which provides multiple session sources.
;;
;; [1] https://github.com/minad/consult

;;; Code:

;;;; Requirements

(require 'detached)

(declare-function consult--multi "consult")

;;;; Variables

(defcustom detached-consult-hidden-predicates nil
  "Predicates for sessions that should be hidden."
  :type '(repeat function)
  :group 'detached)

(defcustom detached-consult-sources
  '(detached-consult--source-session
	detached-consult--source-active-session
	detached-consult--source-inactive-session
	detached-consult--source-hidden-session
	detached-consult--source-success-session
	detached-consult--source-failure-session
	detached-consult--source-local-session
	detached-consult--source-remote-session
	detached-consult--source-current-session)
  "Sources used by `detached-consult-session'.

See `consult-multi' for a description of the source values."
  :type '(repeat symbol)
  :group 'detached)

(defvar detached-consult--source-session
  `(:category detached
			  :annotate detached-session-annotation
			  :action (lambda (x) (detached-open-session (detached--decode-session x)))
			  :items
			  ,(lambda ()
				 (mapcar #'car
						 (seq-remove
						  (lambda (x)
							(seq-find (lambda (predicate)
										(apply predicate `(,(cdr x))))
									  detached-consult-hidden-predicates))
						  (detached-session-candidates (detached-get-sessions))))))
  "All `detached' sessions as a source for `consult'.")

(defvar detached-consult--source-hidden-session
  `(:narrow (?\s . "Hidden")
			:hidden t
			:category detached
			:annotate detached-session-annotation
			:action (lambda (x) (detached-open-session (detached--decode-session x)))
			:items
			,(lambda ()
			   (mapcar #'car
					   (seq-filter
						(lambda (x)
						  (seq-find (lambda (predicate)
									  (apply predicate `(,(cdr x))))
									detached-consult-hidden-predicates))
						(detached-session-candidates (detached-get-sessions))))))
  "Active `detached' sessions as a source for `consult'.")

(defvar detached-consult--source-active-session
  `(:narrow (?a . "Active")
            :hidden t
            :category detached
            :annotate detached-session-annotation
            :action (lambda (x) (detached-open-session (detached--decode-session x)))
            :items
            ,(lambda ()
               (mapcar #'car
                       (thread-last (detached-session-candidates (detached-get-sessions))
                                    (seq-map #'cdr)
                                    (seq-filter #'detached-session-active-p)))))
  "Active `detached' sessions as a source for `consult'.")

(defvar detached-consult--source-inactive-session
  `(:narrow (?i . "Inactive")
            :hidden t
            :category detached
            :annotate detached-session-annotation
            :action (lambda (x) (detached-open-session (detached--decode-session x)))
            :items
            ,(lambda ()
               (mapcar #'car
                       (thread-last (detached-session-candidates (detached-get-sessions))
                                    (seq-map #'cdr)
                                    (seq-filter #'detached-session-inactive-p)))))
  "Inactive `detached' sessions as a source for `consult'.")

(defvar detached-consult--source-failure-session
  `(:narrow (?f . "Failure")
            :hidden t
            :category detached
            :annotate detached-session-annotation
            :action (lambda (x) (detached-open-session (detached--decode-session x)))
            :items
            ,(lambda ()
               (mapcar #'car
                       (thread-last (detached-session-candidates (detached-get-sessions))
                                    (seq-map #'cdr)
                                    (seq-filter #'detached-session-failed-p)))))
  "Failed `detached' sessions as a source for `consult'.")

(defvar detached-consult--source-success-session
  `(:narrow (?s . "Success")
            :hidden t
            :category detached
            :annotate detached-session-annotation
            :action (lambda (x) (detached-open-session (detached--decode-session x)))
            :items
            ,(lambda ()
               (mapcar #'car
                       (thread-last (detached-session-candidates (detached-get-sessions))
                                    (seq-map #'cdr)
                                    (seq-remove #'detached-session-failed-p)))))
  "Successful `detached' sessions as a source for `consult'.")

(defvar detached-consult--source-local-session
  `(:narrow (?l . "Local Host")
            :hidden t
            :category detached
            :annotate detached-session-annotation
            :action (lambda (x) (detached-open-session (detached--decode-session x)))
            :items
            ,(lambda ()
               (mapcar #'car
                       (thread-last (detached-session-candidates (detached-get-sessions))
                                    (seq-map #'cdr)
                                    (seq-filter #'detached-session-localhost-p))))
            "Local host `detached' sessions as a source for `consult'."))

(defvar detached-consult--source-remote-session
  `(:narrow (?r . "Remote Host")
            :hidden t
            :category detached
            :annotate detached-session-annotation
            :action (lambda (x) (detached-open-session (detached--decode-session x)))
            :items
            ,(lambda ()
               (mapcar #'car
                       (thread-last (detached-session-candidates (detached-get-sessions))
                                    (seq-map #'cdr)
                                    (seq-filter #'detached-session-remotehost-p)))))
  "Remote host `detached' sessions as a source for `consult'.")

(defvar detached-consult--source-current-session
  `(:narrow (?c . "Current Host")
            :hidden t
            :category detached
            :annotate detached-session-annotation
            :action (lambda (x) (detached-open-session (detached--decode-session x)))
            :items
            ,(lambda ()
               (let ((host-name (car (detached--host))))
                 (mapcar #'car
                         (thread-last (detached-session-candidates (detached-get-sessions))
                                      (seq-map #'cdr)
                                      (seq-filter
                                       (lambda (x)
                                         (string= (detached-session-host-name x) host-name))))))))
  "Current host `detached' sessions as a source for `consult'.")

;;;; Commands

;;;###autoload
(defun detached-consult-session ()
  "Enhanced `detached-open-session' command."
  (interactive)
  (unless (require 'consult nil 'noerror)
	(error "Install Consult to use detached-consult"))
  (consult--multi detached-consult-sources
				  :prompt "Select session: "
				  :require-match t
				  :sort nil))

(provide 'detached-consult)

;;; detached-consult.el ends here
