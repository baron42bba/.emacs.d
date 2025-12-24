;;; mastodon-auth.el --- Auth functions for mastodon.el  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019 Johnson Denen
;; Copyright (C) 2021 Abhiseck Paira <abhiseckpaira@disroot.org>
;; Copyright (C) 2025 Marty Hiatt <mousebot@disroot.org>
;; Author: Johnson Denen <johnson.denen@gmail.com>
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

;; mastodon-auth.el supports authorizing and authenticating with Mastodon.

;;; Code:

(require 'plstore)
(require 'auth-source)
(require 'json)
(require 'url)

(eval-when-compile (require 'subr-x)) ; for if-let*

(autoload 'mastodon-client "mastodon-client")
(autoload 'mastodon-client--active-user "mastodon-client")
(autoload 'mastodon-client--form-user-from-vars "mastodon-client")
(autoload 'mastodon-client--make-user-active "mastodon-client")
(autoload 'mastodon-client--store-access-token "mastodon-client")
(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--concat-params-to-url "mastodon-http")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-return-credential-account "mastodon")
(autoload 'mastodon-client--general-read "mastodon-client")
(autoload 'mastodon-client--token-file "mastodon-client")

(defvar mastodon-instance-url)
(defvar mastodon-client-scopes)
(defvar mastodon-client-redirect-uri)
(defvar mastodon-active-user)

(defgroup mastodon-auth nil
  "Authenticate with Mastodon."
  :prefix "mastodon-auth-"
  :group 'mastodon)

(defcustom mastodon-auth-use-auth-source t
  "Whether to use auth sources for user credentials.
If t, save and read user access token in the user's auth source
file (see `auth-sources'). If nil, use `mastodon-client--token-file'
instead.
If you change the value of this variable, call
`mastodon-forget-all-logins' and log in again.
If for some reason you generate a new token, you'll have to update your
auth souce file manually, or at least remove the entry and authenticate
again, as auth-source.el only provides unreliable tools for updating
entries."
  :type 'boolean)

(defvar mastodon-auth-source-file nil
  "This variable is obsolete.
This variable currently serves no purpose and will be removed in
the future.")

(defvar mastodon-auth--token-alist nil
  "Alist of User access tokens keyed by instance url.")

(defvar mastodon-auth--acct-alist nil
  "Alist of account accts (name@domain) keyed by instance url.")

(defvar mastodon-auth--user-unaware
  "          ** MASTODON.EL - NOTICE **

User variables not set: mastodon.el requires that you set both
`mastodon-active-user' and `mastodon-instance-url' in your init file.

Please see its documentation to understand what value it accepts
by running M-x describe-variable on it or visiting our web page:
https://codeberg.org/martianh/mastodon.el.
")

(defun mastodon-auth--get-browser-login-url ()
  "Return properly formed browser login url."
  (let ((client-id (plist-get (mastodon-client) :client_id)))
    (if (not client-id)
        (error "Failed to set up client id")
      (mastodon-http--concat-params-to-url
       (concat mastodon-instance-url "/oauth/authorize/")
       `(("response_type" . "code")
         ("redirect_uri" . ,mastodon-client-redirect-uri)
         ("scope" . ,mastodon-client-scopes)
         ("client_id" . ,client-id))))))

(defvar mastodon-auth--explanation
  (format
   "
1. A URL has been copied to your clipboard.  Open this URL in a
javascript capable browser and your browser will take you to your
Mastodon instance's login page.

2. Login to your account (%s) and authorize \"mastodon.el\".

3. After authorization you will be presented an authorization
code. Copy this code and paste it in the minibuffer prompt."
   (mastodon-client--form-user-from-vars)))

(defun mastodon-auth--show-notice (notice buffer-name &optional ask)
  "Display NOTICE to user.
By default NOTICE is displayed in vertical split occupying 50% of total
width.  The buffer name of the buffer being displayed in the
window is BUFFER-NAME.
When optional argument ASK is given which should be a string, use
ASK as the minibuffer prompt.  Return whatever user types in
response to the prompt.
When ASK is absent return nil."
  (let ((buffer (get-buffer-create buffer-name))
        (inhibit-read-only t)
        ask-value window)
    (set-buffer buffer)
    (erase-buffer)
    (insert notice)
    (fill-region (point-min) (point-max))
    (read-only-mode)

    (setq window (select-window
                  (split-window (frame-root-window) nil 'below)
                  t))
    (switch-to-buffer buffer t)
    (when ask
      (setq ask-value (read-string ask))
      (kill-buffer buffer)
      (delete-window window))
    ask-value))

(defun mastodon-auth--request-authorization-code ()
  "Ask authorization code and return it."
  (let ((url (mastodon-auth--get-browser-login-url))
        (select-enable-clipboard t)
        authorization-code)
    (kill-new url)
    (message "%s" url)
    (setq authorization-code
          (mastodon-auth--show-notice mastodon-auth--explanation
                                      "*mastodon-notice*"
                                      "Authorization Code: "))
    authorization-code))

(defun mastodon-auth--generate-token ()
  "Generate access_token for the user.  Return response buffer."
  (let ((authorization-code (mastodon-auth--request-authorization-code)))
    (mastodon-http--post
     (concat mastodon-instance-url "/oauth/token")
     `(("grant_type" . "authorization_code")
       ("client_secret" . ,(plist-get (mastodon-client) :client_secret))
       ("client_id" . ,(plist-get (mastodon-client) :client_id))
       ("code" . ,authorization-code)
       ("redirect_uri" . ,mastodon-client-redirect-uri))
     nil
     :unauthenticated)))

(defun mastodon-auth--get-token ()
  "Make a request to generate an auth token and return JSON response."
  (with-current-buffer (mastodon-auth--generate-token)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (let ((json-object-type 'plist)
          (json-key-type 'keyword)
          (json-array-type 'vector)
          (json-string (buffer-substring-no-properties (point) (point-max))))
      (json-read-from-string json-string))))

(defun mastodon-auth--plstore-token-check (&optional auth-source)
  "Signal an error if plstore contains unencrypted access-token.
If AUTH-SOURCE, and if `mastodon-auth-use-auth-source' is non-nil,
return non-nil if it contains any access token.
Used to help users switch to the new encrypted auth token flow."
  ;; FIXME: is it poss to move this plstore read to have one less read?
  ;; e.g. inside of `mastodon-client--active-user'? the issue is that
  ;; ideally we want to test "user-" entry, even if fetching "active-user"
  ;; entry, so we would have to re-do the plstore read functions.
  (when
      (mastodon-auth--plstore-access-token-member auth-source)
    (if auth-source
        (user-error "Auth source storage of tokens is enabled,\
 but there is also an access token in your plstore.\
 If you're seeing this message after updating,\
 call `mastodon-forget-all-logins', and try again.
 If you don't want to use auth sources,\
 also set `mastodon-auth-use-auth-source' to nil.\
 If this message is in error, contact us on the mastodon.el repo")
      (user-error "Unencrypted access token in your plstore.\
 If you're seeing this message after updating,\
 call `mastodon-forget-all-logins', and log in again.
 If this message is in error, contact us on the mastodon.el repo"))))

(defun mastodon-auth--plstore-access-token-member (&optional auth-source)
  "Return non-nil if the user entry of the plstore contains :access_token.
If AUTH-SOURCE, also check if it contains :secret-access_token."
  (let* ((plstore (plstore-open (mastodon-client--token-file)))
         (name (concat "user-" (mastodon-client--form-user-from-vars)))
         ;; get alist like plstore.el does, so that keys will display with
         ;; ":secret-" prefix if encrypted:
         (alist (assoc name (plstore--get-merged-alist plstore))))
    (if (and auth-source mastodon-auth-use-auth-source)
        (or (member :access_token alist)
            (member :secret-access_token alist))
      (member :access_token alist))))

(defun mastodon-auth--access-token ()
  "Return the access token to use with `mastodon-instance-url'.
Generate/save token if none known yet."
  (cond
   (mastodon-auth--token-alist
    ;; user variables are known and initialised.
    (alist-get mastodon-instance-url
               mastodon-auth--token-alist nil nil #'string=))
   ;; if auth source enabled, but we have an access token in plstore,
   ;; error out and tell user to remove plstore and start over or disable
   ;; auth source:
   ((mastodon-auth--plstore-token-check))
   ((plist-get (mastodon-client--active-user) :access_token)
    ;; user variables need to be read from plstore active-user entry.
    (push (cons mastodon-instance-url
                (plist-get (mastodon-client--active-user) :access_token))
          mastodon-auth--token-alist)
    (alist-get mastodon-instance-url
               mastodon-auth--token-alist nil nil #'string=))
   ((null mastodon-active-user)
    ;; user not aware of 2FA-related changes and has not set
    ;; `mastodon-active-user'. Make user aware and error out.
    (mastodon-auth--show-notice mastodon-auth--user-unaware
                                "*mastodon-notice*")
    (user-error "Variables not set properly"))
   (t
    ;; user access-token needs to fetched from the server and
    ;; stored and variables initialised.
    (mastodon-auth--handle-token-response (mastodon-auth--get-token)))))

(defun mastodon-auth--handle-token-response (response)
  "Add token RESPONSE to `mastodon-auth--token-alist'.
The token is returned by `mastodon-auth--get-token'.
Handle any errors from the server."
  (pcase response
    ((and (let token (plist-get response :access_token))
          (guard token))
     (mastodon-client--make-user-active
      (mastodon-client--store-access-token token))
     (cdar (push (cons mastodon-instance-url token)
                 mastodon-auth--token-alist)))
    (`(:error ,class :error_description ,error)
     (error "Mastodon-auth--access-token: %s: %s" class error))
    (_ (error "Unknown response from mastodon-auth--get-token!"))))

(defun mastodon-auth-source-get (user host &optional token create)
  "Fetch an auth source token, searching by USER and HOST.
If CREATE, use TOKEN or prompt for it, and save it if there is no such entry.
Return a list of user, password/secret, and the item's save-function."
  (let* ((auth-source-creation-prompts
          '((secret . "%u access token: ")))
         (source
          (car
           (auth-source-search :host host :user user
                               :require '(:user :secret)
                               :secret (if token token nil)
                               ;; "create" alone doesn't work here!:
                               :create (if create t nil)))))
    (when source
      (let ((creds
             `(,(plist-get source :user)
               ,(auth-info-password source)
               ,(plist-get source :save-function))))
        (when create ;; call save function:
          (when (functionp (nth 2 creds))
            (funcall (nth 2 creds))))
        creds))))

(defun mastodon-auth-source-token (url handle &optional token create)
  "Parse URL, search auth sources with it, user HANDLE and TOKEN.
Calls `mastodon-auth-source-get', returns only the token.
If CREATE, create an entry is none is found."
  (let ((host (url-host
               (url-generic-parse-url url)))
        (username (car (split-string handle "@"))))
    (nth 1
         (mastodon-auth-source-get username host token create))))

(defun mastodon-auth--get-account-name ()
  "Request user credentials and return an account name."
  (alist-get 'acct
             (mastodon-return-credential-account)))

(defun mastodon-auth--get-account-id ()
  "Request user credentials and return an account name."
  (alist-get 'id
             (mastodon-return-credential-account)))

(defun mastodon-auth--user-acct ()
  "Return a mastodon user acct name."
  (or (cdr (assoc mastodon-instance-url mastodon-auth--acct-alist))
      (let ((acct (mastodon-auth--get-account-name)))
        (push (cons mastodon-instance-url acct) mastodon-auth--acct-alist)
        acct)))

(provide 'mastodon-auth)
;;; mastodon-auth.el ends here
