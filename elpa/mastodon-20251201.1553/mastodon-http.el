;;; mastodon-http.el --- HTTP request/response functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2020-2024 Marty Hiatt
;; Author: Johnson Denen <johnson.denen@gmail.com>
;;         Marty Hiatt <mousebot@disroot.org>
;; Maintainer: Marty Hiatt <mousebot@disroot.org>
;; Homepage: https://codeberg.org/martianh/mastodon.el

;; This file is not part of GNU Emacs.

;; This file is part of mastodon.el.

;; mastodon.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mastodon.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mastodon.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; mastodon-http.el provides HTTP request/response functions.

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)
(require 'shr)

(defvar mastodon-instance-url)
(defvar mastodon-toot--media-attachment-ids)
(defvar mastodon-toot--media-attachment-filenames)

(autoload 'mastodon-auth--access-token "mastodon-auth")
(autoload 'mastodon-toot--update-status-fields "mastodon-toot")
(autoload 'url-insert "url-handlers")

(defvar mastodon-http--api-version "v1")

(defconst mastodon-http--timeout 15
  "HTTP request timeout, in seconds.  Has no effect on Emacs < 26.1.")

(defun mastodon-http--api (endpoint &optional version)
  "Return Mastodon API URL for ENDPOINT.
Optionally specify VERSION in format vX."
  (concat mastodon-instance-url "/api/"
          (or version mastodon-http--api-version) "/" endpoint))

(defun mastodon-http--api-v2 (endpoint)
  "Return Mastodon API v2 URL for ENDPOINT."
  (mastodon-http--api endpoint "v2"))

(defun mastodon-http--url-retrieve-synchronously (url &optional silent)
  "Retrieve URL asynchronously.
This is a thin abstraction over the system
`url-retrieve-synchronously'.  Depending on which version of this
is available we will call it with or without a timeout.
SILENT means don't message."
  (if (< (cdr (func-arity 'url-retrieve-synchronously)) 4)
      (url-retrieve-synchronously url)
    (url-retrieve-synchronously url (or silent nil) nil mastodon-http--timeout)))

(defun mastodon-http--response-status (resp)
  "Return the status code from RESP, a response buffer."
  ;; `url-http-parse-response' breaks tests, as
  ;; `url-http-end-of-headers' not set, so we roll our own:
  (with-current-buffer resp
    (goto-char (point-min))
    (skip-chars-forward " \t\n")    ; Skip any blank crap
    (skip-chars-forward "/HPT0-9.") ; Skip HTTP Version "HTTP/X.Y"
    (read (current-buffer))))

(defun mastodon-http--triage (response success)
  "Determine if RESPONSE was successful.
Call SUCCESS on RESPONSE if successful. Message status and JSON error
from RESPONSE if unsuccessful."
  (let ((status (mastodon-http--response-status response)))
    (if (and (>= 200 status)
             (<= status 299))
        ;; (string-prefix-p "2" (number-to-string status))
        (funcall success response)
      (if (= 404 status)
          (message "Error %s: page not found" status)
        (let ((json-response (with-current-buffer response
                               (mastodon-http--process-json))))
          (message "Error %s: %s" status (alist-get 'error json-response)))))))

(defun mastodon-http--read-file-as-string (filename &optional url)
  "Read a file FILENAME as a string.
Used to generate image preview.
URL means FILENAME is a URL."
  (with-temp-buffer
    (if url
        (url-insert-file-contents filename)
      (insert-file-contents filename))
    (string-to-unibyte (buffer-string))))

(defmacro mastodon-http--authorized-request (method body &optional unauthenticated-p)
  "Make a METHOD type request using BODY, with Mastodon authorization.
Unless UNAUTHENTICATED-P is non-nil."
  (declare (debug 'body)
           (indent 1))
  `(let ((url-request-method ,method)
         (url-request-extra-headers
          (unless ,unauthenticated-p
            (list (cons "Authorization"
                        (concat "Bearer " (mastodon-auth--access-token)))))))
     ,body))

(defun mastodon-http--build-params-string (params)
  "Build a request parameters string from parameters alist PARAMS."
  ;; (url-build-query-string args nil))
  ;; url-build-query-string adds 'nil' for empty params so lets stick with our
  ;; own:
  (mapconcat (lambda (p)
               (when (cdr p) ; only when value
                 (concat (url-hexify-string (car p))
                         "=" (url-hexify-string (cdr p)))))
             params "&"))

(defun mastodon-http--build-array-params-alist (param-str array)
  "Return parameters alist using PARAM-STR and ARRAY param values.
Used for API form data parameters that take an array."
  (cl-loop for x in array
           collect (cons param-str x)))

(defun mastodon-http--concat-params-to-url (url params)
  "Build a query string with PARAMS and concat to URL."
  (if params
      (concat url "?"
              (mastodon-http--build-params-string params))
    url))

(defun mastodon-http--post (url
                            &optional params headers unauthenticated-p json)
  "POST synchronously to URL, optionally with PARAMS and HEADERS.
Authorization header is included by default unless
UNAUTHENTICATED-P is non-nil.
If JSON is :json, encode PARAMS as JSON for
the request data. If it is :raw, just use the plain params."
  ;; NB: raw is used by `mastodon-tl-unfilter-user-languages'; not sure if
  ;; there's a way around it?
  (mastodon-http--authorized-request "POST"
    (let* ((url-request-data
            (when params
              (cond ((eq json :json)
                     (json-encode params))
                    ((eq json :raw)
                     params)
                    (t
                     (mastodon-http--build-params-string params)))))
           (url-request-extra-headers
            (append url-request-extra-headers ; auth set in macro
                    (if json
                        '(("Content-Type" . "application/json")
                          ("Accept" . "application/json"))
                      (unless (assoc "Content-Type" headers) ; pleroma compat:
                        '(("Content-Type" . "application/x-www-form-urlencoded"))))
                    headers)))
      (with-temp-buffer
        (mastodon-http--url-retrieve-synchronously url)))
    unauthenticated-p))

(defun mastodon-http--get (url &optional params silent)
  "Make synchronous GET request to URL.
PARAMS is an alist of any extra parameters to send with the request.
SILENT means don't message."
  (mastodon-http--authorized-request "GET"
    ;; url-request-data doesn't seem to work with GET requests?:
    (let ((url (mastodon-http--concat-params-to-url url params)))
      (mastodon-http--url-retrieve-synchronously url silent))))

(defun mastodon-http--get-response (url &optional params no-headers silent vector)
  "Make synchronous GET request to URL. Return JSON and response headers.
PARAMS is an alist of any extra parameters to send with the request.
SILENT means don't message.
NO-HEADERS means don't collect http response headers.
VECTOR means return json arrays as vectors."
  (let ((buf (mastodon-http--get url params silent)))
    ;; --get can return nil if instance unresponsive:
    (if (not buf)
        (user-error "Looks like the server response borked. \
Is your instance up?")
      (with-current-buffer buf
        (mastodon-http--process-response no-headers vector)))))

(defun mastodon-http--get-json (url &optional params silent vector)
  "Return only JSON data from URL request.
PARAMS is an alist of any extra parameters to send with the request.
SILENT means don't message.
VECTOR means return json arrays as vectors."
  (car (mastodon-http--get-response url params :no-headers silent vector)))

(defun mastodon-http--process-json ()
  "Return only JSON data from async URL request.
Callback to `mastodon-http--get-json-async', usually
`mastodon-tl--init*', is run on the result."
  (car (mastodon-http--process-response :no-headers)))

(defun mastodon-http--render-html-err (string)
  "Render STRING as HTML in a temp buffer.
STRING should be a HTML for a 404 errror."
  (with-temp-buffer
    (insert string)
    (shr-render-buffer (current-buffer))
    (view-mode))) ; for 'q' to kill buffer and window
    ;; (error ""))) ; stop subsequent processing

(defun mastodon-http--process-response (&optional no-headers vector)
  "Process http response.
Return a cons of JSON list and http response headers.
If NO-HEADERS is non-nil, just return the JSON.
VECTOR means return json arrays as vectors.
Callback to `mastodon-http--get-response-async', usually
`mastodon-tl--init*', is run on the result."
  ;; view raw response:
  ;; (switch-to-buffer (current-buffer))
  (let ((headers (unless no-headers
                   (mastodon-http--process-headers))))
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (let ((json-array-type (if vector 'vector 'list))
          (json-string (string-trim-right
                        (decode-coding-string
                         (buffer-substring-no-properties (point) (point-max))
                         'utf-8))))
      (kill-buffer)
      (cond ((or (string-empty-p json-string) (null json-string))
             nil)
            ;; if we get html, just render it and error:
            ;; ideally we should handle the status code in here rather than
            ;; this crappy hack?
            ((string-prefix-p "\n<" json-string) ; html hack
             ;; NB: in this case, process-response returns t!:
             (mastodon-http--render-html-err json-string)
             nil) ;; return nil instead of t
            ;; if no json or html, maybe we have a plain string error message
            ;; (misskey does this, but there are probably better ways to do
            ;; this):
            ((not (or (string-prefix-p "\n{" json-string)
                      (string-prefix-p "\n[" json-string)))
             (error "%s" json-string))
            (t
             ;; instance may return error in JSON e.g. ((error . "Record not
             ;; found")) for a null endpoint. but we don't error here because
             ;; sometimes we just want to check for such an error in an
             ;; if/cond.
             `(,(json-read-from-string json-string) . ,headers))))))

(defun mastodon-http--process-headers ()
  "Return an alist of http response headers."
  (goto-char (point-min))
  (let* ((head-str (buffer-substring-no-properties
                    (point-min)
                    (re-search-forward "^$" nil 'move)))
         (head-list (split-string head-str "\n")))
    (mapcar (lambda (x)
              (let ((list (split-string x ": ")))
                (cons (car list) (cadr list))))
            head-list)))

(defun mastodon-http--delete (url &optional params)
  "Make DELETE request to URL.
PARAMS is an alist of any extra parameters to send with the request."
  ;; url-request-data only works with POST requests?
  (let ((url (mastodon-http--concat-params-to-url url params)))
    (mastodon-http--authorized-request "DELETE"
      (with-temp-buffer
        (mastodon-http--url-retrieve-synchronously url)))))

(defun mastodon-http--put (url &optional params headers)
  "Make PUT request to URL.
PARAMS is an alist of any extra parameters to send with the request.
HEADERS is an alist of any extra headers to send with the request."
  (mastodon-http--authorized-request "PUT"
    (let ((url-request-data
           (when params (mastodon-http--build-params-string params)))
          (url-request-extra-headers
           (append url-request-extra-headers ; auth set in macro
                   (unless (assoc "Content-Type" headers) ; pleroma compat:
                     '(("Content-Type" . "application/x-www-form-urlencoded")))
                   headers)))
      (with-temp-buffer (mastodon-http--url-retrieve-synchronously url)))))

;; profile update functions

(defun mastodon-http--patch-json (url &optional params)
  "Make synchronous PATCH request to URL. Return JSON response.
Optionally specify the PARAMS to send."
  (with-current-buffer (mastodon-http--patch url params)
    (mastodon-http--process-json)))

(defun mastodon-http--patch (url &optional params json)
  "Make synchronous PATCH request to URL.
Optionally specify the PARAMS to send.
JSON means send params as JSON data."
  (mastodon-http--authorized-request "PATCH"
    ;; NB: unlike POST, PATCHing only works if we use query params!
    ;; so here, unless JSON arg, we use query params and do not set
    ;; `url-request-data'. this is probably an error, i don't understand it.
    (let* ((url-request-data
            (when (and params json)
              (encode-coding-string
               (json-encode params) 'utf-8)))
           ;; (mastodon-http--build-params-string params))))
           (url (if (not json)
                    (mastodon-http--concat-params-to-url url params)
                  url))
           (headers (when json
                      '(("Content-Type" . "application/json")
                        ("Accept" . "application/json"))))
           (url-request-extra-headers
            (append url-request-extra-headers headers)))
      (mastodon-http--url-retrieve-synchronously url))))

 ;; Asynchronous functions

(defun mastodon-http--get-async (url &optional params callback &rest cbargs)
  "Make GET request to URL.
Pass response buffer to CALLBACK function with args CBARGS.
PARAMS is an alist of any extra parameters to send with the request."
  (let ((url (mastodon-http--concat-params-to-url url params)))
    (mastodon-http--authorized-request "GET"
      (url-retrieve url callback cbargs))))

(defun mastodon-http--get-response-async (url &optional params callback &rest cbargs)
  "Make GET request to URL. Call CALLBACK with http response and CBARGS.
PARAMS is an alist of any extra parameters to send with the request."
  (mastodon-http--get-async
   url
   params
   (lambda (status)
     (when status ; for flakey servers
       (apply callback (mastodon-http--process-response) cbargs)))))

(defun mastodon-http--get-json-async (url &optional params callback &rest cbargs)
  "Make GET request to URL. Call CALLBACK with json-list and CBARGS.
PARAMS is an alist of any extra parameters to send with the request."
  (mastodon-http--get-async
   url
   params
   (lambda (status)
     (when status ;; only when we actually get sth?
       (apply callback (mastodon-http--process-json) cbargs)))))

(defun mastodon-http--post-async (url params _headers &optional callback &rest cbargs)
  "POST asynchronously to URL with PARAMS and HEADERS.
Then run function CALLBACK with arguements CBARGS.
Authorization header is included by default unless UNAUTHENTICED-P is non-nil."
  (mastodon-http--authorized-request "POST"
    (let ((url-request-data (when params
                              (mastodon-http--build-params-string params))))
      (with-temp-buffer
        (url-retrieve url callback cbargs)))))

(defun mastodon-http--get-cb-data (status)
  "Return data using `json-read' after a successful async request.
If STATUS includes an error, emit a message describing it and return nil."
  (let* ((buf (current-buffer))
         (data (with-temp-buffer
                 (url-insert buf)
                 (goto-char (point-min))
                 (json-read))))
    (if-let* ((error-thrown (plist-get status :error)))
        ;; not necessarily a user error, but we want its functionality:
        (user-error "%S %s" error-thrown (alist-get 'error data))
      data)))

(defun mastodon-http--post-media-callback (status file caption buffer)
  "Callback function called after posting FILE as an attachment with CAPTION.
The toot is being composed in BUFFER. See `url-retrieve' for STATUS."
  (unwind-protect
      (when-let* ((data (mastodon-http--get-cb-data status)))
        (with-current-buffer buffer
          (let ((id (alist-get 'id data)))
            ;; update ids:
            (if (not mastodon-toot--media-attachment-ids)
                ;; add first id:
                (push id mastodon-toot--media-attachment-ids)
              ;; add new id to end of list to preserve order:
              (push id (cdr
                        (last mastodon-toot--media-attachment-ids))))
            ;; pleroma, PUT the description:
            ;; this is how the mangane akkoma web client does it
            ;; and it seems easier than the other options!
            (when (and caption
                       (not (string= caption (alist-get 'description data))))
              (let ((url (mastodon-http--api (format "media/%s" id))))
                ;; (message "PUTting image description")
                (mastodon-http--put url `(("description" . ,caption)))))
            (message "Uploading %s... (done)" file)
            (mastodon-toot--update-status-fields))))
    (kill-buffer (current-buffer))))

(defun mastodon-http--post-media-prep-file (filename)
  "Return the request data to upload FILENAME."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename)
    (let ((boundary (buffer-hash)))
      (goto-char (point-min))
      (insert "--" boundary "\r\n"
              (format "Content-Disposition: form-data; name=\"file\"; filename=\"%s\"\r\n\r\n"
                      (file-name-nondirectory filename)))
      (goto-char (point-max))
      (insert "\r\n" "--" boundary "--" "\r\n")
      `(,boundary . ,(buffer-substring-no-properties (point-min) (point-max))))))

(defun mastodon-http--post-media-attachment (url filename caption)
  "Make POST request to upload FILENAME with CAPTION to the server's media URL.
The upload is asynchronous. On succeeding,
`mastodon-toot--media-attachment-ids' is set to the id(s) of the
item uploaded, and `mastodon-toot--update-status-fields' is run."
  (mastodon-http--authorized-request "POST"
    (let* ((data (mastodon-http--post-media-prep-file filename))
           (url-request-extra-headers
            (append url-request-extra-headers ; auth set in macro
                    `(("Content-Type" . ,(format "multipart/form-data; boundary=%s"
                                                 (car data))))))
           (url-request-data (cdr data))
           (params `(("description" . ,caption)))
           (url (mastodon-http--concat-params-to-url url params)))
      (url-retrieve url #'mastodon-http--post-media-callback
                    `(,filename ,caption ,(current-buffer))))))

(provide 'mastodon-http)
;;; mastodon-http.el ends here
