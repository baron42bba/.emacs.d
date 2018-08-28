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

;;; Commentary:

;; A collection of yasnippet snippets to be used with AWS CLI

;;; Code:

(defvar aws-snippets-dir (file-name-directory (or load-file-name
						  (buffer-file-name))))

(defcustom aws-snippets-regions '("us-east-1" "eu-west-1" "ap-southeast-1")
  "List of AWS regions for selections."
  :tag "regions"
  :type '(choice (string :tag "Region")
                 (repeat :tag "List of Regions"
                         (string :tag "Region")))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             ))
  :group 'aws-snippets)

(defcustom aws-snippets-profiles '("test" "prod")
  "List of AWS profiles for selections."
  :tag "profiles"
  :type '(choice (string :tag "Profile")
                 (repeat :tag "List of Profiles"
                         (string :tag "Profiles")))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             ))
  :group 'aws-snippets)

(defcustom aws-snippets-filters '("tag:Name" "tag:Project" "tag:Type" "tag:Cluster" "instance.group-id" "ip-address" "private-ip-address" "network-interface.subnet-id" "description" "name" "iam-instance-profile.arn")
  "List of AWS filters for selections."
  :tag "default-filters"
  :type '(choice (string :tag "Filter")
                 (repeat :tag "List of Filters"
                         (string :tag "Filter")))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             ))
  :group 'aws-snippets)

(defcustom aws-snippets-ami-ls-query '"reverse(sort_by(Images, &CreationDate)[].{CreationDate:CreationDate,Name:Name,ImageId:ImageId,Public:Public,Description:Description})"
  "Query string used for ami-ls."
  :tag "ami-ls-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-autoscaling-describe-auto-scaling-groups-query '"AutoScalingGroups[].{Name: AutoScalingGroupName, Desired: DesiredCapacity,Min: MinSize, Max: MaxSize}"
  "Query string used for autoscaling-describe-auto-scaling-groups."
  :tag "autoscaling-describe-auto-scaling-groups"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-autoscaling-describe-launch-configurations-query '"reverse(sort_by(LaunchConfigurations &CreatedTime)[*].[LaunchConfigurationName, CreatedTime])"
  "Query string used for autoscaling-describe-launch-configurations."
  :tag "autoscaling-describe-launch-configurations-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-dns-names-query '"Reservations[].Instances[?State.Name=='running'].[InstanceId, PublicDnsName]"
  "Query string used for aws-ec2-get-dns-names."
  :tag "ec2-get-dns-names-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-instance-volumes-query '"Reservations[].Instances[].[join(\`,\`,Tags[?Key==\`Name\`].Value),join(' ', BlockDeviceMappings[].Ebs.VolumeId)]"
  "Query string used for ec2-get-instance-volumes."
  :tag "ec2-get-instance-volumes-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-securitygroups-query '"sort_by(SecurityGroups, &GroupName)[*].[GroupName, GroupId, Description]"
  "Query string used for ec2-get-securitygroups."
  :tag "ec2-get-securitygroups-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-snapshots-query '"Snapshots[].[join(\`,\`,Tags[?Key==\`Name\`].Value),StartTime,Description,SnapshotId,VolumeId,VolumeSize,Progress]"
  "Query string used for ec2-get-snapshots-query."
  :tag "ec2-get-snapshots-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-volumes-query '"Volumes[].[VolumeId,State,SnapshotId,join(\`,\`,Tags[?Key==\`Name\`].Value)]"
  "Query string used for ec2-get-volumes-query."
  :tag "ec2-get-volumes-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-get-volumes-unused-query '"Volumes[].[VolumeId,Size,VolumeType,CreateTime,State,SnapshotId]"
  "Query string used for ec2-get-volumes-unused-query."
  :tag "ec2-get-volumes-unused-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-instance-running-query '"Reservations[].Instances[].[InstanceId, State.Name]"
  "Query string used for ec2-instance-running."
  :tag "ec2-instance-running-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-list-instances-query '("Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime]"
						   "Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, IamInstanceProfile.Arn]"
						   "Reservations[].Instances[].[Tags[?Key==`Name`].Value[] | [0],Tags[?Key==`Schedule`].Value[] | [0],InstanceId, State.Name, PublicDnsName, InstanceType,Placement.AvailabilityZone,LaunchTime, KeyName]")
  "Query string used for ec2 describe-instances."
  :tag "ec2-list-instances-query"
  :type '(choice (string :tag "Query")
                 (repeat :tag "List of Queries"
                         (string :tag "Query")))
  :set #'(lambda (symbol new)
           (let ((old (and (boundp symbol)
                           (symbol-value symbol))))
             (set-default symbol new)
             ))
  :group 'aws-snippets)

(defcustom aws-snippets-ec2-list-subnets-query '"sort_by(sort_by(Subnets[].{VpcId:VpcId, SubnetId:SubnetId, State:State, AvailabilityZone:AvailabilityZone, CidrBlock:CidrBlock, AvailableIpAddressCount:AvailableIpAddressCount Tags:to_string(Tags[].[Key, Value])}, &SubnetId), &VpcId)"
  "Query string used for ec2-list-subnets."
  :tag "ec2-list-subnets-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-ec2-list-vpcs-query '"sort_by(Vpcs[].{VpcId:VpcId, State:State, AvailabilityZone:AvailabilityZone, CidrBlock:CidrBlock, AvailableIpAddressCount:AvailableIpAddressCount Tags:to_string(Tags[].[Key, Value])}, &VpcId)"
  "Query string used for ec2-list-vpcs."
  :tag "ec2-list-vpcs-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-elb-get-list-query '"reverse(sort_by(LoadBalancerDescriptions, &CreatedTime)[].[LoadBalancerName, CreatedTime])"
  "Query string used for elb-get-list."
  :tag "elb-get-list-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-emr-list-clusters-query '"Clusters[*].{ID: Id, NAME: Name}"
  "Query string used for emr-list-clusters."
  :tag "emr-list-clusters-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-rds-describe-instances-query '"DBInstances[*].[DBInstanceIdentifier,DbiResourceId, Endpoint.Address]"
  "Query string used for rds describe-db-instances."
  :tag "rds-describe-db-instances-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-route53-list-hosted-zones-query '"HostedZones[].[Id, Name]"
  "Query string used for route53 list-hosted-zones."
  :tag "route53-list-hosted-zones-query"
  :group 'aws-snippets
  :type 'string)

(defcustom aws-snippets-aws-support-ls-query '"reverse(sort_by(cases, &timeCreated)[].[caseId, subject, timeCreated, displayId])"
  "Query string used for aws support-ls-query."
  :tag "aws-support-ls-query"
  :group 'aws-snippets
  :type 'string)

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
