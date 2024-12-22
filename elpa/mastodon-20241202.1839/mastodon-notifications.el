;;; mastodon-notifications.el --- Notification functions for mastodon.el -*- lexical-binding: t -*-

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

;; mastodon-notification.el provides notification functions for Mastodon.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'mastodon-widget)

(autoload 'mastodon-http--api "mastodon-http")
(autoload 'mastodon-http--get-params-async-json "mastodon-http")
(autoload 'mastodon-http--post "mastodon-http")
(autoload 'mastodon-http--triage "mastodon-http")
(autoload 'mastodon-media--inline-images "mastodon-media")
(autoload 'mastodon-tl--byline "mastodon-tl")
(autoload 'mastodon-tl--byline-author "mastodon-tl")
(autoload 'mastodon-tl--clean-tabs-and-nl "mastodon-tl")
(autoload 'mastodon-tl--content "mastodon-tl")
(autoload 'mastodon-tl--field "mastodon-tl")
(autoload 'mastodon-tl--find-property-range "mastodon-tl")
(autoload 'mastodon-tl--has-spoiler "mastodon-tl")
(autoload 'mastodon-tl--init "mastodon-tl")
(autoload 'mastodon-tl--property "mastodon-tl")
(autoload 'mastodon-tl--reload-timeline-or-profile "mastodon-tl")
(autoload 'mastodon-tl--spoiler "mastodon-tl")
(autoload 'mastodon-tl--item-id "mastodon-tl")
(autoload 'mastodon-tl--update "mastodon-tl")
(autoload 'mastodon-views--view-follow-requests "mastodon-views")
(autoload 'mastodon-tl--current-filters "mastodon-views")
(autoload 'mastodon-tl--render-text "mastodon-tl")
(autoload 'mastodon-notifications-get "mastodon")
(autoload 'mastodon-tl--byline-uname-+-handle "mastodon-tl")
(autoload 'mastodon-tl--byline-handle "mastodon-tl")
(autoload 'mastodon-http--get-json "mastodon-http")
(autoload 'mastodon-media--get-avatar-rendering "mastodon-media")
(autoload 'mastodon-tl--image-trans-check "mastodon-tl")
(autoload 'mastodon-tl--symbol "mastodon-tl")
(autoload 'mastodon-tl--display-or-uname "mastodon-tl")
(autoload 'mastodon-tl--goto-next-item "mastodon-tl")
(autoload 'mastodon-tl--buffer-type-eq "mastodon-tl")
(autoload 'mastodon-tl--buffer-property "mastodon-tl")

;; notifications defcustoms moved into mastodon.el
;; as some need to be available without loading this file

(defvar mastodon-tl--buffer-spec)
(defvar mastodon-tl--display-media-p)
(defvar mastodon-mode-map)
(defvar mastodon-tl--fold-toots-at-length)
(defvar mastodon-tl--show-avatars)
(defvar mastodon-profile-note-in-foll-reqs)
(defvar mastodon-profile-note-in-foll-reqs-max-length)
(defvar mastodon-group-notifications)
(defvar mastodon-notifications-grouped-names-count)

(defvar mastodon-notifications--types
  '("all" "favourite" "reblog" "mention" "poll"
    "follow_request" "follow" "status" "update")
  ;; "severed_relationships" "moderation_warning")
  "A list of notification types according to their name on the server, plus \"all\".")

(defvar mastodon-notifications--filter-types-alist
  '(("all"                    . mastodon-notifications-get)
    ("favourite"              . mastodon-notifications--get-favourites)
    ("reblog"                 . mastodon-notifications--get-boosts)
    ("mention"                . mastodon-notifications--get-mentions)
    ("poll"                   . mastodon-notifications--get-polls)
    ("follow_request"         . mastodon-notifications--get-follow-requests)
    ("follow"                 . mastodon-notifications--get-follows)
    ("status"                 . mastodon-notifications--get-statuses)
    ("update"                 . mastodon-notifications--get-edits))
  "An alist of notification types and their corresponding load functions.
Notification types are named according to their name on the server.")

(defvar mastodon-notifications--response-alist
  '(("Followed"             . "you")
    ("Favourited"           . "your post")
    ("Boosted"              . "your post")
    ("Mentioned"            . "you")
    ("Posted a poll"        . "that has now ended")
    ("Requested to follow"  . "you")
    ("Posted"               . "a post")
    ("Edited"               . "their post"))
  "Alist of subjects for notification types.")

(defvar mastodon-notifications--map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mastodon-mode-map)
    (define-key map (kbd "a") #'mastodon-notifications--follow-request-accept)
    (define-key map (kbd "j") #'mastodon-notifications--follow-request-reject)
    (define-key map (kbd "C-k") #'mastodon-notifications--clear-current)
    (define-key map (kbd "C-c C-c") #'mastodon-notifications--cycle-type)
    map)
  "Keymap for viewing notifications.")

(defun mastodon-notifications--byline-action-str (message)
  "Return an action (top) byline string for TOOT with MESSAGE."
  (concat " "
          (propertize message 'face 'mastodon-boosted-face)
          " " (cdr (assoc message mastodon-notifications--response-alist))
          "\n"))

(defun mastodon-notifications--follow-request-process (&optional reject)
  "Process the follow request at point.
With no argument, the request is accepted. Argument REJECT means
reject the request. Can be called in notifications view or in
follow-requests view."
  (if (not (mastodon-tl--find-property-range 'item-json (point)))
      (user-error "No follow request at point?")
    (let* ((item-json (mastodon-tl--property 'item-json))
           (f-reqs-view-p (string= "follow_requests"
                                   (plist-get mastodon-tl--buffer-spec 'endpoint)))
           (f-req-p (or (string= "follow_request"
                                 (mastodon-tl--property 'notification-type
                                                        :no-move))
                        f-reqs-view-p)))
      (if (not f-req-p)
          (user-error "No follow request at point?")
        (let-alist (or (alist-get 'account item-json) ;notifs
                       item-json) ;f-reqs
          (if (not .id)
              (user-error "No account result at point?")
            (let ((response
                   (mastodon-http--post
                    (mastodon-http--api
                     (format "follow_requests/%s/%s"
                             .id (if reject "reject" "authorize"))))))
              (mastodon-http--triage
               response
               (lambda (_)
                 (if f-reqs-view-p
                     (mastodon-views--view-follow-requests)
                   (mastodon-tl--reload-timeline-or-profile))
                 (message "Follow request of %s (@%s) %s!"
                          .username .acct (if reject "rejected" "accepted")))))))))))

(defun mastodon-notifications--follow-request-accept ()
  "Accept a follow request.
Can be called in notifications view or in follow-requests view."
  (interactive)
  (mastodon-notifications--follow-request-process))

(defun mastodon-notifications--follow-request-reject ()
  "Reject a follow request.
Can be called in notifications view or in follow-requests view."
  (interactive)
  (mastodon-notifications--follow-request-process :reject))

(defun mastodon-notifications--comment-note-text (str)
  "Add comment face to all text in STR with `shr-text' face only."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let (prop)
      (while (setq prop (text-property-search-forward 'face 'shr-text t))
        (add-text-properties (prop-match-beginning prop)
                             (prop-match-end prop)
                             '(face (font-lock-comment-face shr-text)))))
    (buffer-string)))

(defvar mastodon-notifications-grouped-types
  '(follow reblog favourite)
  "List of notification types for which grouping is implemented.")

(defvar mastodon-notifications--action-alist
  '((reblog          . "Boosted")
    (favourite       . "Favourited")
    (follow_request  . "Requested to follow")
    (follow          . "Followed")
    (mention         . "Mentioned")
    (status          . "Posted")
    (poll            . "Posted a poll")
    (update          . "Edited"))
  "Action strings keyed by notification type.
Types are those of the Mastodon API.")

(defun mastodon-notifications--alist-by-value (str field json)
  "From JSON, return the alist whose FIELD value matches STR.
JSON is a list of alists."
  (cl-some (lambda (y)
             (when (string= str (alist-get field y))
               y))
           json))

(defun mastodon-notifications--group-accounts (ids json)
  "For IDS, return account data in JSON."
  (cl-loop
   for x in ids
   collect (mastodon-notifications--alist-by-value x 'id json)))

(defun mastodon-notifications--severance-body (group)
  "Return a body for a severance notification GROUP."
  ;; FIXME: actually implement this when we encounter one in the wild!
  (let-alist (alist-get 'event group)
    (concat .type ": "
            .target_name
            "\nRelationships affected: "
            .relationships_count)))

(defun mastodon-notifications--mod-warning-body (group)
  "Return a body for a moderation warning notification GROUP."
  (let-alist (alist-get 'moderation_warning group)
    (concat .action ": "
            .text
            "\nStatuses: "
            .status_ids
            "\nfor account: "
            .target_account)))

(defun mastodon-notifications--format-note (note)
  "Format for a NOTE, a non-grouped notification."
  (let* ((type (intern (alist-get 'type note)))
         (profile-note
          (when (eq 'follow_request type)
            (let ((str (mastodon-tl--field
                        'note
                        (mastodon-tl--field 'account note))))
              (if mastodon-profile-note-in-foll-reqs-max-length
                  (string-limit str mastodon-profile-note-in-foll-reqs-max-length)
                str))))
         (status (mastodon-tl--field 'status note))
         (follower (alist-get 'account note))
         (follower-name (or (alist-get 'display_name follower)
                            (alist-get 'username follower)))
         (filtered (mastodon-tl--field 'filtered status))
         (filters (when filtered
                    (mastodon-tl--current-filters filtered))))
    (if (and filtered (assoc "hide" filters))
        nil
      (mastodon-notifications--insert-note
       ;; toot
       ;; should always be note, otherwise notif data not avail
       ;; later on:
       note
       ;; body
       (mastodon-notifications--body-arg
        type filters status profile-note follower-name)
       ;; action-byline (top)
       (mastodon-notifications--action-byline
        type nil nil note follower-name)
       ;; base toot (always provide)
       status
       nil nil nil type))))

(defun mastodon-notifications--format-group-note (group status accounts)
  "Format for a GROUP notification.
STATUS is the status's JSON.
ACCOUNTS is data of the accounts that have reacted to the notification."
  (let ((folded nil))
    ;; FIXME: apply/refactor filtering as per/with `mastodon-tl--toot'
    (let-alist group
      (let* ((type (intern .type))
             (profile-note
              (when (member type '(follow_request))
                (let ((str (mastodon-tl--field 'note (car accounts))))
                  (if mastodon-profile-note-in-foll-reqs-max-length
                      (string-limit str mastodon-profile-note-in-foll-reqs-max-length)
                    str))))
             (follower (when (member type '(follow follow_request))
                         (car accounts)))
             (follower-name (or (alist-get 'display_name follower)
                                (alist-get 'username follower)))
             (filtered (mastodon-tl--field 'filtered status))
             (filters (when filtered
                        (mastodon-tl--current-filters filtered))))
        (unless (and filtered (assoc "hide" filters))
          (mastodon-notifications--insert-note
           ;; toot
           (if (member type '(follow follow_request))
               follower
             status)
           ;; body
           (mastodon-notifications--body-arg
            type filters status profile-note follower-name group)
           ;; action-byline
           (mastodon-notifications--action-byline
            type accounts group)
           ;; base toot (no need for update/poll/?)
           (when (member type '(favourite reblog))
             status)
           folded group accounts))))))

(defun mastodon-notifications--action-byline
    (type &optional accounts group note follower-name)
  "Return an action (top) byline for notification of TYPE.
ACCOUNTS and GROUP group are used by grouped notifications.
NOTE and FOLLOWER-NAME are used for non-grouped notifs."
  (let ((action-str
         (unless (member type '(follow follow_request mention))
           (downcase
            (mastodon-notifications--byline-action-str
             (alist-get type mastodon-notifications--action-alist)))))
        (action-symbol (if (eq type 'mention)
                           ""
                         (mastodon-tl--symbol type)))
        (action-authors
         (if (member type '(follow follow_request mention))
             "" ;; mentions are normal statuses
           (if group
               (mastodon-notifications--byline-accounts accounts group)
             (mastodon-tl--byline-handle note nil
                                         follower-name
                                         'mastodon-display-name-face)))))
    (propertize
     (concat action-symbol " " action-authors action-str)
     'byline-top t)))

(defun mastodon-notifications--body-arg
    (type &optional filters status profile-note follower-name group)
  "TYPE is a symbol, a member of `mastodon-notifiations--types'.
FILTERS STATUS PROFILE-NOTE FOLLOWER-NAME GROUP."
  (let ((body
         (if-let ((match (assoc "warn" filters)))
             (mastodon-tl--spoiler status (cadr match))
           (mastodon-tl--clean-tabs-and-nl
            (cond ((mastodon-tl--has-spoiler status)
                   (mastodon-tl--spoiler status))
                  ((eq type 'follow_request)
                   (mastodon-tl--render-text profile-note))
                  (t (mastodon-tl--content status)))))))
    (cond
     ((eq type 'follow)
      (propertize "Congratulations, you have a new follower!"
                  'face 'default))
     ((eq type 'follow_request)
      (concat
       (propertize (format "You have a follow request from %s"
                           follower-name)
                   'face 'default)
       (when mastodon-profile-note-in-foll-reqs
         (concat
          ":\n"
          (mastodon-notifications--comment-note-text body)))))
     ((eq type 'severed_relationships)
      (mastodon-notifications--severance-body group))
     ((eq type 'moderation_warning)
      (mastodon-notifications--mod-warning-body group))
     ((member type '(favourite reblog))
      (propertize
       (mastodon-notifications--comment-note-text body)))
     (t body))))

(defun mastodon-notifications--insert-note
    (toot body action-byline
          &optional base-toot unfolded group accounts type)
  "Display the content and byline of timeline element TOOT.
BODY will form the section of the toot above the byline.
AUTHOR-BYLINE is an optional function for adding the author
portion of the byline that takes one variable. By default it is
`mastodon-tl--byline-author'.
ACTION-BYLINE is a string, obtained by calling
`mastodon-notifications--action-byline'.
BASE-TOOT is the JSON of the toot responded to.
UNFOLDED is a boolean meaning whether to unfold or fold item if
foldable.
GROUP is the notification group data.
ACCOUNTS is the notification accounts data.
TYPE is notification type, used for non-group notifs."
  (let* ((type (if type
                   (symbol-name type) ;; non-group
                 (alist-get 'type group)))
         (toot-foldable
          (and mastodon-tl--fold-toots-at-length
               (length> body mastodon-tl--fold-toots-at-length)))
         (ts ;; types listed here use base item timestamp, else we use
          ;; group's latest timestamp:
          (when (and group
                     (not
                      (member type '("favourite" "reblog" "edit" "poll"))))
            (mastodon-tl--field 'latest_page_notification_at group))))
    (insert
     (propertize ;; top byline, body + byline:
      (concat
       (if (equal type "mention") ;; top (action) byline
           ""
         action-byline)
       (propertize body ;; body only
                   'toot-body t) ;; includes newlines etc. for folding
       "\n"
       ;; actual byline:
       (mastodon-tl--byline toot nil nil base-toot group ts))
      'item-type     'toot ;; for nav, actions, etc.
      'item-id       (or (alist-get 'page_max_id group) ;; newest notif
                         (alist-get 'id toot)) ; toot id
      'base-item-id  (mastodon-tl--item-id
                      ;; if status is a notif, get id from base-toot
                      ;; (-tl--item-id toot) will not work here:
                      (or base-toot
                          toot)) ; else normal toot with reblog check
      'item-json     toot
      'base-toot     base-toot
      'cursor-face   'mastodon-cursor-highlight-face
      'toot-foldable toot-foldable
      'toot-folded   (and toot-foldable (not unfolded))
      ;; grouped notifs data:
      'notification-type type
      'notification-id (alist-get 'group_key group)
      'notification-group group
      'notification-accounts accounts
      ;; for pagination:
      'notifications-min-id (alist-get 'page_min_id group)
      'notifications-max-id (alist-get 'page_max_id group))
     "\n")))

(defun mastodon-notifications--byline-accounts
    (accounts group &optional avatar)
  "Propertize author byline ACCOUNTS.
GROUP is the group notification data.
When AVATAR, include the account's avatar image.
When DOMAIN, force inclusion of user's domain in their handle."
  (let ((total (alist-get 'notifications_count group))
        (accts mastodon-notifications-grouped-names-count))
    (concat
     (string-trim ;; remove trailing newline
      (cl-loop
       for account in accounts
       repeat accts
       concat
       (let-alist account
         (concat
          ;; avatar insertion moved up to `mastodon-tl--byline' by
          ;; default to be outside 'byline propt.
          (when (and avatar ; used by `mastodon-profile--format-user'
                     mastodon-tl--show-avatars
                     mastodon-tl--display-media-p
                     (mastodon-tl--image-trans-check))
            (mastodon-media--get-avatar-rendering .avatar))
          (let ((uname (mastodon-tl--display-or-uname account)))
            (mastodon-tl--byline-handle account nil
                                        uname 'mastodon-display-name-face))
          ", ")))
      nil ", ")
     (if (< accts total)
         (let ((diff (- total accts)))
           (propertize ;; help-echo remaining notifs authors:
            (format " and %s other%s" diff (if (= 1 diff) "" "s"))
            'help-echo (mapconcat (lambda (a)
                                    (propertize (alist-get 'username a)
                                                'face 'mastodon-display-name-face))
                                  (cddr accounts) ;; not first two
                                  ", ")))))))

(defun mastodon-notifications--render (json no-group)
  "Display grouped notifications in JSON.
NO-GROUP means don't render grouped notifications."
  ;; (setq masto-grouped-notifs json)
  (if no-group
      (cl-loop for x in json
               do (mastodon-notifications--format-note x))
    (let ((groups (alist-get 'notification_groups json)))
      (cl-loop
       for g in groups
       for start-pos = (point)
       for accounts = (mastodon-notifications--group-accounts
                       (alist-get 'sample_account_ids g)
                       (alist-get 'accounts json))
       for status = (mastodon-notifications--alist-by-value
                     (or (alist-get 'status_id g)
                         ;; if no status_id, just try the first item?
                         (alist-get 'id
                                    (car
                                     (alist-get 'statuses json))))
                     'id
                     (alist-get 'statuses json))
       do (mastodon-notifications--format-group-note g status accounts)
       (when mastodon-tl--display-media-p
         ;; images-in-notifs custom is handeld in
         ;; `mastodon-tl--media-attachment', not here
         (mastodon-media--inline-images start-pos (point)))))))

(defun mastodon-notifications--timeline (json &optional type)
  "Format JSON in Emacs buffer.
Optionally specify TYPE."
  (if (seq-empty-p json)
      (user-error "Looks like you have no (more) notifications for now")
    (mastodon-widget--create
     "Filter" mastodon-notifications--types
     (or type "all")
     (lambda (widget &rest _ignore)
       (let ((value (widget-value widget)))
         (mastodon-notifications--get-type value)))
     :newline)
    (insert "\n")
    (mastodon-notifications--render json (not mastodon-group-notifications))
    (goto-char (point-min))
    (mastodon-tl--goto-next-item)))

(defun mastodon-notifications--get-type (&optional type)
  "Read a notification type and load its timeline.
Optionally specify TYPE."
  (let ((choice (or type
                    (completing-read
                     "View notifications: "
                     mastodon-notifications--filter-types-alist))))
    (funcall (alist-get
              choice mastodon-notifications--filter-types-alist
              nil nil #'equal))))

(defun mastodon-notifications--cycle-type (&optional prefix)
  "Cycle the current notifications view.
With arg PREFIX, `completing-read' a type and load it."
  (interactive "P")
  ;; FIXME: do we need a sept buffer-type result for all notifs views?
  (if (not (or (mastodon-tl--buffer-type-eq 'notifications)
               (mastodon-tl--buffer-type-eq 'mentions)))
      (user-error "Not in a notifications view")
    (let* ((choice
            (if prefix
                (completing-read "Notifs by type: "
                                 mastodon-notifications--types)
              (mastodon-notifications--get-next-type)))
           (fun (alist-get choice mastodon-notifications--filter-types-alist
                           nil nil #'equal)))
      (funcall fun))))

(defun mastodon-notifications--get-next-type ()
  "Return the next notif type based on current buffer spec."
  (let* ((update-params (mastodon-tl--buffer-property
                         'update-params nil :no-error))
         (type (alist-get "types[]" update-params nil nil #'equal)))
    (if (not update-params)
        (cadr mastodon-notifications--types)
      (or (cadr (member type mastodon-notifications--types))
          (car mastodon-notifications--types)))))

(defun mastodon-notifications--get-mentions ()
  "Display mention notifications in buffer."
  (interactive)
  (mastodon-notifications-get "mention" "mentions"))

(defun mastodon-notifications--get-favourites ()
  "Display favourite notifications in buffer."
  (interactive)
  (mastodon-notifications-get "favourite" "favourites"))

(defun mastodon-notifications--get-boosts ()
  "Display boost notifications in buffer."
  (interactive)
  (mastodon-notifications-get "reblog" "boosts"))

(defun mastodon-notifications--get-polls ()
  "Display poll notifications in buffer."
  (interactive)
  (mastodon-notifications-get "poll" "polls"))

(defun mastodon-notifications--get-statuses ()
  "Display status notifications in buffer.
Status notifications are created when you call
`mastodon-tl--enable-notify-user-posts'."
  (interactive)
  (mastodon-notifications-get "status" "statuses"))

(defun mastodon-notifications--get-follows ()
  "Display follow notifications in buffer."
  (interactive)
  (mastodon-notifications-get "follow" "follows"))

(defun mastodon-notifications--get-follow-requests ()
  "Display follow request notifications in buffer."
  (interactive)
  (mastodon-notifications-get "follow_request" "follow-requests"))

(defun mastodon-notifications--get-edits ()
  "Display edited post notifications in buffer."
  (interactive)
  (mastodon-notifications-get "update" "edits"))

(defun mastodon-notifications--filter-types-list (type)
  "Return a list of notification types with TYPE removed."
  (remove type mastodon-notifications--types))

(defun mastodon-notifications--clear-all ()
  "Clear all notifications."
  (interactive)
  (when (y-or-n-p "Clear all notifications?")
    (let ((response
           (mastodon-http--post (mastodon-http--api "notifications/clear"))))
      (mastodon-http--triage
       response (lambda (_)
                  (when mastodon-tl--buffer-spec
                    (mastodon-tl--reload-timeline-or-profile))
                  (message "All notifications cleared!"))))))

(defun mastodon-notifications--clear-current ()
  "Dismiss the notification at point."
  (interactive)
  (let* ((id (or (or (mastodon-tl--property 'notification-id) ;; grouped
                     (mastodon-tl--property 'item-id)
                     (mastodon-tl--field
                      'id
                      (mastodon-tl--property 'item-json)))))
         (endpoint (mastodon-http--api
                    (format "notifications/%s/dismiss" id)
                    "v2"))
         (response (mastodon-http--post endpoint)))
    (mastodon-http--triage
     response (lambda (_)
                (when mastodon-tl--buffer-spec
                  (mastodon-tl--reload-timeline-or-profile))
                (message "Notification dismissed!")))))

(defun mastodon-notifications--get-single-notif ()
  "Return a single notification JSON for v2 notifs."
  (interactive)
  (let* ((id (mastodon-tl--property
              'notification-id)) ;; grouped, doesn't work for ungrouped!
         ;; (key (format "ungrouped-%s"
         ;;              (mastodon-tl--property 'item-id)))
         (endpoint (mastodon-http--api
                    (format "notifications/%s" id)
                    "v2"))
         (response (mastodon-http--get-json endpoint)))
    (mastodon-http--triage
     response (lambda (_)
                (message "%s" (prin1-to-string response))))))

(defun mastodon-notifications--get-unread-count ()
  "Return the number of unread notifications for the current account."
  ;; params: limit - max 1000, default 100, types[], exclude_types[], account_id
  (let* ((endpoint "notifications/unread_count")
         (url (mastodon-http--api endpoint))
         (resp (mastodon-http--get-json url)))
    (alist-get 'count resp)))

(provide 'mastodon-notifications)
;;; mastodon-notifications.el ends here
