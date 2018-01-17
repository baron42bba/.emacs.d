;;; aws-snippets.el --- Yasnippets for AWS

;; Copyright (C) 2018 Andreas Gerler
;; keywords: snippets
;; Version: 0.1.0
;; Package-Requires: ((yasnippet "0.8.0"))

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(defvar aws-snippets-dir (file-name-directory (or (buffer-file-name)
                                                 load-file-name)))
(defconst aws-snippets--default-regions
  '("us-east-1" "eu-west-1" "ap-southeast-1"))

(defcustom aws-snippets-regions aws-snippets--default-regions
  "List of AWS regions for selections."
  :type '(choice (string :tag "Region")
                 (repeat :tag "List of Regions"
                         (string :tag "Region")))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             ))
  :group 'aws-snippets
  )


(defconst aws-snippets--default-profiles
  '("test" "prod"))

(defcustom aws-snippets-profiles aws-snippets--default-profiles
  "List of AWS profiles for selections."
  :type '(choice (string :tag "Profile")
                 (repeat :tag "List of Profiles"
                         (string :tag "Profiles")))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             ))
  :group 'aws-snippets
  )

(defconst aws-snippets--default-filters
  '("tag:Name" "tag:Project" "tag:Type" "tag:Cluster" "instance.group-id" "ip-address" "private-ip-address" "network-interface.subnet-id" "description" ))

(defcustom aws-snippets-filters aws-snippets--default-filters
  "List of AWS filters for selections."
  :type '(choice (string :tag "Filter")
                 (repeat :tag "List of Filters"
                         (string :tag "Filter")))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             ))
  :group 'aws-snippets
  )



(defconst aws-snippets--ec2-list-instances-query
  '"Reservations[].Instances[].[join(\`,\`,Tags[?Key==\`Name\`].Value),join(\`,\`,Tags[?Key==\`Schedule\`].Value),InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime]")

(defcustom aws-snippets-ec2-list-instances-query aws-snippets--ec2-list-instances-query
  "Query string used for ec2 describe-instances."
  :group 'aws-snippets
  :type 'string
  )
;;;###autoload
(defun aws-snippets-initialize ()
  "Initialize package."
  (let ((snip-dir (expand-file-name "snippets" aws-snippets-dir)))
    (when (boundp 'yas-snippet-dirs)(add-to-list 'yas-snippet-dirs snip-dir t))
    (yas-load-directory snip-dir)))

;;;###autoload
(eval-after-load 'yasnippet
   '(aws-snippets-initialize))

(require 'yasnippet)

(provide 'aws-snippets)
;;; aws-snippets.el ends here
