;;; asterisk-dialplan.el --- Asterisk PBX dialplan support for Emacs

;;; This file is not a part of GNU Emacs, but is made available under
;; the same conditions.

;; Author: Joris Engbers <info@jorisengbers.nl>
;; URL: https://github.com/JorisE/asterisk-dialplan.el
;; Version: 0.1
;; Keywords: Asterisk PBX, Dialplan

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.; Copyright (C) 2003-2014 Free Software Foundation, Inc.

;;; Commentary:
;; asterisk-dialplan-mode adds some syntax highlighting, but is currently not
;; extensive in that respect.
;; The main benefit is to be had from navigation using imenu. Every context is
;; added to imenu and using helm-imenu will easily let you navigate to the
;; context at point.

;;; Code:
;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\extensions.conf\\'")  'asterisk-dialplan-mode))

(defvar asterisk-dialplan-mode-hook nil)


;; Bindings

(defvar dialplan-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c\C-j" 'imenu)
    map)
  "Keymap for dialplan major mode")


;; Font-lock
;; I use the exact symbols instead of an elaborate regex, that way we can
;; discern between functions and dialplan applications.

(defvar asterisk-dialplan-applications
    (rx symbol-start
          (or
           "AbsoluteTimeout" "AddQueueMember" "ADSIProg" "Agent"
           "AgentCallBackLogin" "AgentLogin" "AgentMonitorOutgoing" "AGI"
           "Answer" "AppendCDRUserField" "Authenticate" "Background" "Busy"
           "CallingPres" "ChangeMonitor" "ChanIsAvail" "Congestion" "Cut"
           "DateTime" "DBdel" "DBdeltree" "DBget" "DBput" "DeadAGI" "Dial"
           "DigitTimeout" "Directory" "DISA" "EAGI" "Echo" "EnumLookup" "Eval"
           "ExecIf" "Festival" "Flash" "GetCPEID" "Goto" "GotoIf" "GotoIfTime"
           "Gosub" "GosubIf" "Hangup" "HasNewVoicemail" "ICES" "LookupBlacklist"
           "LookupCIDName" "Macro" "MailboxExists" "MeetMe" "MeetMeCount"
           "Milliwatt" "Monitor" "MP3Player" "MusicOnHold" "NBScat" "NoCDR"
           "NoOp" "ParkAndAnnounce" "ParkedCall" "Playback" "Playtones" "Prefix"
           "PrivacyManager" "Queue" "Random" "Read" "ReceiveFAX" "Record"
           "RemoveQueueMember" "ResetCDR" "Return" "ResponseTimeout" "Ringing"
           "SayDigits" "SayNumber" "SayUnixTime" "SendDTMF" "SendImage"
           "SendURL" "SetAccount" "SetCallerID" "SetCDRUserField" "SetCIDName"
           "SetCIDNum" "SetGlobalVar" "SetLanguage" "SetMusicOnHold" "Set"
           "SetVar" "SIPDtmfMode" "SoftHangup" "StopMonitor" "StopPlaytones"
           "StripLSD" "StripMSD" "SubString" "Suffix" "Swift" "System"
           "Transfer" "Verbose" "VoiceMail" "Voicemail" "VoiceMail2"
           "VoiceMailMain" "VoicemailMain" "VoiceMailMain2" "Wait" "WaitExten"
           "WaitForRing" "WaitMusicOnHold" "Zapateller""ZapBarge" "ZapRAS"
           "ZapScan")
          symbol-end))

(defvar asterisk-dialplan-functions
    (rx symbol-start
          (or
           "AES_DECRYPT" "AES_ENCRYPT" "AGC" "AGENT" "AMI_CLIENT"
           "ARRAY" "AST_CONFIG" "AUDIOHOOK_INHERIT" "BASE64_DECODE"
           "BASE64_ENCODE" "BLACKLIST" "CALENDAR_BUSY"
           "CALENDAR_EVENT" "CALENDAR_QUERY" "CALENDAR_QUERY_RESULT"
           "CALENDAR_WRITE" "CALLCOMPLETION" "CALLERID" "CALLERPRES"
           "CDR" "CHANNEL" "CHANNELS" "CHECKSIPDOMAIN" "CONFBRIDGE"
           "CONFBRIDGE_INFO" "CONNECTEDLINE" "CSV_QUOTE" "CURL"
           "CURLOPT" "CUT" "DB" "DB_DELETE" "DB_EXISTS" "DB_KEYS"
           "DEC" "DENOISE" "DEVICE_STATE" "DIALGROUP"
           "DIALPLAN_EXISTS" "DUNDILOOKUP" "DUNDIQUERY" "DUNDIRESULT"
           "ENUMLOOKUP" "ENUMQUERY" "ENUMRESULT" "ENV" "EVAL"
           "EXCEPTION" "EXISTS" "EXTENSION_STATE" "FAXOPT"
           "FAXOPT_res_fax" "FEATURE" "FEATUREMAP" "FIELDNUM"
           "FIELDQTY" "FILE" "FILE_COUNT_LINE" "FILE_FORMAT" "FILTER"
           "FRAME_TRACE" "GLOBAL" "GROUP" "GROUP_COUNT" "GROUP_LIST"
           "GROUP_MATCH_COUNT" "HANGUPCAUSE" "HANGUPCAUSE_KEYS" "HASH"
           "HASHKEYS" "HINT" "IAXPEER" "IAXVAR" "ICONV" "IF"
           "IFMODULE" "IFTIME" "IMPORT" "INC" "ISNULL"
           "JABBER_RECEIVE" "JABBER_RECEIVE_res_jabber"
           "JABBER_RECEIVE_res_xmpp" "JABBER_STATUS"
           "JABBER_STATUS_res_jabber" "JABBER_STATUS_res_xmpp"
           "JITTERBUFFER" "KEYPADHASH" "LEN" "LISTFILTER" "LOCAL"
           "LOCAL_PEEK" "LOCK" "MAILBOX_EXISTS" "MASTER_CHANNEL"
           "MATH" "MD5" "MEETME_INFO" "MESSAGE" "MESSAGE_DATA"
           "MINIVMACCOUNT" "MINIVMCOUNTER" "MUTEAUDIO" "ODBC"
           "ODBC_FETCH" "PASSTHRU" "PITCH_SHIFT" "POP"
           "PP_EACH_EXTENSION" "PP_EACH_USER" "PRESENCE_STATE" "PUSH"
           "QUEUE_EXISTS" "QUEUE_MEMBER" "QUEUE_MEMBER_COUNT"
           "QUEUE_MEMBER_LIST" "QUEUE_MEMBER_PENALTY"
           "QUEUE_VARIABLES" "QUEUE_WAITING_COUNT" "QUOTE" "RAND"
           "REALTIME" "REALTIME_DESTROY" "REALTIME_FIELD"
           "REALTIME_HASH" "REALTIME_STORE" "REDIRECTING" "REGEX"
           "REPLACE" "SET" "SHA1" "SHARED" "SHELL" "SHIFT"
           "SIP_HEADER" "SIPCHANINFO" "SIPPEER" "SMDI_MSG"
           "SMDI_MSG_RETRIEVE" "SORT" "SPEECH" "SPEECH_ENGINE"
           "SPEECH_GRAMMAR" "SPEECH_RESULTS_TYPE" "SPEECH_SCORE"
           "SPEECH_TEXT" "SPRINTF" "SQL_ESC" "SRVQUERY" "SRVRESULT"
           "STACK_PEEK" "STAT" "STRFTIME" "STRPTIME" "STRREPLACE"
           "SYSINFO" "TESTTIME" "TIMEOUT" "TOLOWER" "TOUPPER"
           "TRYLOCK" "TXTCIDNAME" "UNLOCK" "UNSHIFT" "URIDECODE"
           "URIENCODE" "VALID_EXTEN" "VERSION" "VM_INFO" "VMCOUNT"
           "VOLUME")
          symbol-end))

;; Matches exten => or same =>.
(defvar exten-same (rx (group line-start (zero-or-more whitespace) (or "exten" "same") (zero-or-more whitespace) "=>" (zero-or-more whitespace))))

;; matches anything that leads up to the next comma ",".
(defvar extension-or-priority (rx (group (1+ (not (any ",")))) ","))

(defvar dialplan-font-lock-keywords
  `(
    ;; comments
    (";.*" . font-lock-comment-face)
    ;; section
    (,(rx line-start "[" (1+ any) "]") . font-lock-function-name-face)
    ;; globals
    (,(rx line-start (group (1+ (or word ?_))) "=")  (1 font-lock-constant-face))
    ;; exten => or same =>
    (,exten-same . font-lock-constant-face)
    ;; extension
    (,(concat exten-same extension-or-priority) (2 font-lock-variable-name-face))
    ;; priority
    (,(concat exten-same extension-or-priority extension-or-priority) (3 font-lock-type-face))
    ;; Asterisk dialplan applications
    (,asterisk-dialplan-applications . font-lock-keyword-face)
    ;; Asteisk dialplan functions
    (,asterisk-dialplan-functions . font-lock-function-name-face)
))


;; imenu

(defvar dialplan--imenu-generic-expression
  `((nil ,(rx line-start "[" (group (1+ any)) "]" (* any)) 1)))


;; TODO: add menu entry

;; setup

;;;###autoload
(define-derived-mode asterisk-dialplan-mode conf-mode "Dialplan"
  "Major mode for editing Asterisk dialplan files.

\\{dialplan-mode-map}"
  (set (make-local-variable 'font-lock-defaults) '(dialplan-font-lock-keywords))
  (set (make-local-variable 'imenu-generic-expression) dialplan--imenu-generic-expression)
  (set (make-local-variable 'comment-start) "; ")
)

(provide 'asterisk-dialplan)
;;; asterisk-dialplan.el ends here
