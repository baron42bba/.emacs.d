;;; apache-mode.el --- major mode for editing Apache configuration files

;; Keywords:	languages, faces
;; Author:	Jonathan Marten  <jonathan.marten@uk.sun.com> or
;; Last edit:	12-May-2002      <rendhalver@xemacs.org>

;; This file is an add-on for XEmacs or GNU Emacs (not tested with the latter).
;;
;; It is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; It is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your copy of Emacs; see the file COPYING.  If not, write
;; to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; There isn't really much to say.  The list of keywords was derived from
;; the documentation for Apache 1.3; there may be some errors or omissions.
;;
;; There are currently no local keybindings defined, but the hooks are
;; there in the event that anyone gets around to adding any.
;;

;; PB: hopefully this will no longer be needed :)
;;
;; no longer need to hack auto-mode-alist :)

;; To enable automatic selection of this mode when appropriate files are
;; visited, add the following to your favourite site or personal Emacs
;; configuration file:
;;
;;   (autoload 'apache-mode "apache-mode" "autoloaded" t)
;;   (add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
;;   (add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))
;;   (add-to-list 'auto-mode-alist '("srm\\.conf$"    . apache-mode))
;;   (add-to-list 'auto-mode-alist '("access\\.conf$" . apache-mode))
;;

;;; Change Log:
;;
;; Version 1.0, October 1999	First public release

;; Version 1.1, April 2002      changed var's to use customise
;;                              updated the keywords to apache 1.3.24
;;                              added apache-file-patterns to save having to hack
;;                              auto-mode-alist, added autoloaded function to add
;;                              apache-file-patterns to auto-mode-alist on autoload
;;
;; Version 1.2, April 2002      Added mod_ssl 2.8.8, Apache-SSL 1.47 and
;;                              mod_perl 1.26 keywords.

;; Version 1.2.1, May 2002      separated apache directives into sections
;;                              for easier updating

;; Version 1.3, May 2002        updated keywords to include new directives in 
;;                              apache 2

;;; Code:

;; Requires
(require 'font-lock)
(require 'regexp-opt)
(require 'custom)

;; Variables

;;;###autoload
(defgroup apache nil
  "Major mode for editing Apache configuration files."
  :prefix "apache-"
  :group 'languages)

(defcustom apache-manual-url "http://httpd.apache.org/"
  "*URL at which to find the Apache manual."
  :type 'string
  :group 'apache)

;;;###autoload
(defcustom apache-file-patterns
  (list "\\.htaccess\\(\\.default\\)?$" "httpd\\.conf\\(\\.default\\)?$"
	"srm\\.conf\\(\\.default\\)?$" "access\\.conf\\(\\.default\\)?$")
  "*List of file patterns for which to automatically invoke `apache-mode'."
  :type '(repeat (regexp :tag "Pattern"))
  :group 'apache)

(defcustom apache-mode-hook nil
  "*List of hook functions run by `apache-mode' (see `run-hooks')."
  :type 'hook
  :group 'apache)

(defvar apache-mode-map nil
  "Keymap used in `apache-mode' buffers.")

(defvar apache-mode-syntax-table nil
  "Syntax table for `apache-mode'.")

;;; blatently pinched and hacked from php-mode
;;; hey if it works use it
;;
;; Make apache-mode the default mode for apache config buffers.
;;;###autoload
(let ((apache-file-patterns-temp apache-file-patterns))
  (while apache-file-patterns-temp
    (add-to-list 'auto-mode-alist
		  (cons (car apache-file-patterns-temp) 'apache-mode))
    (setq apache-file-patterns-temp (cdr apache-file-patterns-temp))))


;; Font lock
(defconst apache-font-lock-keywords
  (purecopy
   (list
    (list "^\\s-*#.*$" 0 'font-lock-comment-face t)

    (list (concat                                       ; sections
	   "^\\s-*</?\\("
           (regexp-opt
            '("Directory" "DirectoryMatch" "Files" "FilesMatch"
              "IfDefine" "IfModule" "Limit" "LimitExcept" "Location"
              "LocationMatch" "VirtualHost"
              ;; mod_perl
              "Perl"
              ))
           "\\)\\(>\\|\\s-\\)")
	  1 'font-lock-function-name-face)

    (list (concat                                       ; keywords
	   "^\\s-*\\("
           (regexp-opt
            (delete-duplicates
             '(
	       ;; core directives
	       "AcceptFilter" "AcceptMutex" "AccessConfig" "AccessFileName" "AddDefaultCharset"
	       "AddModule" "AllowOverride" "AuthName" "AuthType" "BindAddress"
	       "BS2000Account" "ClearModuleList" "ContentDigest" "CoreDumpDirectory"
	       "DefaultType" "DocumentRoot" "EBCDICConvert" "EBCDICConvertByType"
	       "EBCDICKludge" "ErrorDocument" "ErrorLog" "FileETag" "Group"
	       "HostnameLookups" "IdentityCheck" "Include" "KeepAlive" "KeepAliveTimeout"
	       "LimitRequestBody" "LimitRequestFields" "LimitRequestFieldsize"
	       "LimitRequestLine" "Listen" "ListenBacklog" "LockFile" "LogLevel"
	       "MaxClients" "MaxKeepAliveRequests" "MaxRequestsPerChild" "MaxSpareServers"
	       "MinSpareServers" "NameVirtualHost" "Options" "PidFile" "Port" "Require"
	       "ResourceConfig" "RLimitCPU" "RLimitMEM" "RLimitNPROC" "Satisfy"
	       "ScoreBoardFile" "ScriptInterpreterSource" "SendBufferSize" "ServerAdmin"
	       "ServerAlias" "ServerName" "ServerPath" "ServerRoot" "ServerSignature"
	       "ServerTokens" "ServerType" "StartServers" "ThreadsPerChild"
	       "ThreadStackSize" "TimeOut" "UseCanonicalName" "User"
	       ;; apache2 core directives
	       "AcceptPathInfo" "ForceType" "LimitXMLRequestBody" "Require"
	       "SetHandler" "SetInputFilter" "SetOutputFilter"
	       ;; apache2 mpm_common
	       "CoreDumpDirectory" "Group" "Listen" "ListenBackLog" "LockFile"
	       "MaxClients" "MaxRequestsPerChild" "MaxSpareThreads"
	       "MaxThreadsPerChild" "MinSpareThreads" "NumServers" "PidFile"
	       "ScoreBoardFile" "SendBufferSize" "ServerLimit" "StartServers"
	       "StartThreads" "ThreadLimit" "ThreadsPerChild" "User"
	       ;; mpm_netware
	       "Listen" "ListenBacklog" "MaxRequestsPerChild" "MaxSpareThreads"
	       "MaxThreads" "MinSpareThreads" "SendBufferSize" "StartThreads"
	       "ThreadStackSize"
	       ;; mpm_winnt
	       "CoreDumpDirectory" "Listen" "ListenBacklog"
	       "MaxRequestsPerChild" "PidFile" "SendBufferSize" "ThreadsPerChild"
	       ;; mpm_perchild
	       "AssignUserId" "ChildPerUserId" "CoreDumpDirectory" "Group"
	       "Listen" "ListenBacklog" "LockFile" "MaxRequestsPerChild"
	       "MaxSpareThreads" "MaxThreadsPerChild" "MinSpareThreads"
	       "NumServers" "PidFile" "ScoreBoardFile" "SendBufferSize"
	       "StartThreads" "User"
	       ;; mpm_prefork
	       "AcceptMutex" "CoreDumpDirectory" "Listen" "ListenBacklog"
	       "LockFile" "MaxRequestsPerChild" "MaxSpareServers"
	       "MaxSpareServers" "MinSpareServers" "MinSpareServers"
	       "PidFile" "ScoreBoardFile" "SendBufferSize"
	       "ServerLimit" "StartServers" "User"
	       ;; mpm_worker
	       "CoreDumpDirectory" "Group" "Listen" "ListenBacklog"
	       "LockFile" "MaxClients" "MaxRequestsPerChild"
	       "MaxSpareThreads" "MinSpareThreads" "PidFile"
	       "ScoreBoardFile" "SendBufferSize" "ServerLimit"
	       "StartServers" "ThreadLimit" "ThreadsPerChild" "User"
	       ;; Environment Creation directives
	       ;; mod_env 
	       "PassEnv" "SetEnv" "UnsetEnv"
	       ;; mod_setenvif
	       "BrowserMatch" "BrowserMatchNoCase" "SetEnvIf" "SetEnvIfNoCase"
	       ;; Content Type Decisions
	       ;; mod_mime
	       "AddCharset" "AddEncoding" "AddHandler" "AddLanguage" "AddType"
	       "DefaultLanguage" "ForceType" "RemoveEncoding" "RemoveHandler"
	       "RemoveType" "SetHandler" "TypesConfig"
	       ;; apache2 mod_mime
	       "AddInputFilter" "AddOutputFilter" "MultiviewsMatch"
	       "RemoveCharset" "RemoveInputFilter" "RemoveLanguage"
	       "RemoveOutputFilter"
	       ;; mod_mime_magic
	       "MimeMagicFile"
	       ;; mod_negotiation
	       "CacheNegotiatedDocs" "LanguagePriority"
	       ;; apache2 mod_negotiation
	       "ForceLangaugePriority"
	       ;; URL Mapping
	       ;; mod_alias
	       "Alias" "AliasMatch" "Redirect" "RedirectMatch" "RedirectTemp"
	       "RedirectPermanent" "ScriptAlias" "ScriptAliasMatch"
	       ;; mod_rewrite
	       "RewriteEngine" "RewriteOptions" "RewriteLog" "RewriteLogLevel"
	       "RewriteLock" "RewriteMap" "RewriteBase" "RewriteCond" "RewriteRule"
	       ;; mod_userdir
	       "UserDir"
	       ;; mod_speling
	       "CheckSpelling"
	       ;; mod_vhost_alias
	       "VirtualDocumentRoot" "VirtualDocumentRootIP" "VirtualScriptAlias"
	       "VirtualScriptAliasIP"
	       ;; Directory Handling
	       ;; mod_dir
	       "DirectoryIndex"
	       ;; mod_autoindex
	       "AddAlt" "AddAltByEncoding" "AddAltByType" "AddDescription" "AddIcon"
	       "AddIconByEncoding" "AddIconByType" "DefaultIcon" "FancyIndexing"
	       "HeaderName" "IndexIgnore" "IndexOptions" "IndexOrderDefault" "ReadmeName"
	       ;; Access Control
	       ;; mod_access
	       "Allow" "Deny" "Order"
	       ;; mod_auth
	       "AuthGroupFile" "AuthUserFile" "AuthAuthoritative"
	       ;; mod_auth_dbm
	       "AuthDBMGroupFile" "AuthDBMUserFile" "AuthDBMAuthoritative"
	       ;; apache2 mod_auth_dbm
	       "AuthDBMType"
	       ;; mod_auth_db
	       "AuthDBGroupFile" "AuthDBUserFile" "AuthDBAuthoritative"
	       ;; mod_auth_anon
	       "Anonymous" "Anonymous_Authoritative" "Anonymous_LogEmail"
	       "Anonymous_MustGiveEmail" "Anonymous_NoUserID" "Anonymous_VerifyEmail"
	       ;; mod_auth_digest
	       "AuthDigestFile" "AuthDigestGroupFile" "AuthDigestQop"
	       "AuthDigestNonceLifetime" "AuthDigestNonceFormat" "AuthDigestNcCheck"
	       "AuthDigestAlgorithm" "AuthDigestDomain"
	       ;; apache2 mod_auth_digest
	       "AuthDigestAlgorithm" "AuthDigestNcCheck" "AuthDigestNonceFormat"
	       "AuthDigestNonceLifetime"
	       ;; mod_digest
	       "AuthDigestFile"
	       ;; HTTP Response
	       ;; mod_headers
	       "Header"
	       ;; apache2 mod_headers
	       "RequestHeader"
	       ;; mod_cern_meta
	       "MetaFiles" "MetaDir" "MetaSuffix"
	       ;; mod_expires
	       "ExpiresActive" "ExpiresByType" "ExpiresDefault"
	       ;; Dynamic Content
	       ;; mod_include
	       "XBitHack"
	       ;; apache2 mod_include
	       "SSIEndTag" "SSIErrorMsg" "SSIStartTag" "SSITimeFormat"
	       "SSIUndefinedEcho"
	       ;; mod_cgi
	       "ScriptLog" "ScriptLogLength" "ScriptLogBuffer"
	       ;; mod_actions
	       "Action" "Script" 
	       ;; mod_isapi WIN32 only
	       "ISAPIReadAheadBuffer" "ISAPILogNotSupported" "ISAPIAppendLogToErrors"
	       "ISAPIAppendLogToQuery"
	       ;; apache2 mod_isapi
	       "ISAPIFileChache"
	       ;; Internal Content Handlers
	       ;; mod_status
	       "ExtendedStatus" 
	       ;; mod_info
	       "AddModuleInfo"
	       ;; Logging
	       ;; mod_log_config
	       "CookieLog" "CustomLog" "LogFormat" "TransferLog"
	       ;; mod_log_agent
	       "AgentLog"
	       ;; mod_log_referer
	       "RefererIgnore" "RefererLog"
	       ;; mod_usertrack
	       "CookieDomain" "CookieExpires" "CookieName" "CookieStyle" "CookieTracking"
	       ;; Miscellaneous
	       ;; mod_imap
	       "ImapMenu" "ImapDefault" "ImapBase"
	       ;; mod_proxy
	       "ProxyRequests" "ProxyRemote" "ProxyPass" "ProxyPassReverse" "ProxyBlock"
	       "AllowCONNECT" "ProxyReceiveBufferSize" "ProxyIOBufferSize" "NoProxy"
	       "ProxyDomain" "ProxyVia" "CacheRoot" "CacheSize" "CacheMaxExpire"
	       "CacheDefaultExpire" "CacheLastModifiedFactor" "CacheGcInterval"
	       "CacheDirLevels" "CacheDirLength" "CacheForceCompletion" "NoCache"
	       ;; apache2 mod_proxy
	       "ProxyErrorOverride" "ProxyMaxForwards" "ProxyPreserveHost"
	       "ProxyRemote" "ProxyTimeout"
	       ;; mod_so
	       "LoadFile" "LoadModule" 
	       ;; mod_mmap_static
	       "MMapFile"
	       ;; Development
	       ;; mod_example
               "Example"
	       ;; Obsolete directives
	       ;; mod_browser
	       "BrowserMatch" "BrowserMatchNoCase"
	       ;; mod_cookies
	       "CookieLog"
	       ;; mod_dld
	       "LoadFile" "LoadModule"
	       ;; mod_log_common
	       "TransferLog"
	       ;; other stuff that i dont know which mod they belong in
	       "DefaultMode" "HTTPLogFile" "HTMLDir" "PrivateDir"
               "TopSites" "TopURLs" "LastURLs" "HeadPrefix" "HeadSuffix"
               "DocTitle" "DocTrailer" "HideURL" "HideSys"
	       ;;; apache2 extra builtin modules
	       ;; mod_charset_lite
	       "CharsetDefault" "CharsetOptions" "CharsetSourceEnc"
	       ;; mod_auth_ldap
	       "AuthLDAPAuthoritative" "AuthLDAPBindDN" "AuthLDAPBindPassword"
	       "AuthLDAPCompareDNOnServer" "AuthLDAPDereferenceAliases"
	       "AuthLDAPEnabled" "AuthLDAPFrontPageHack" "AuthLDAPGroupAttribute"
	       "AuthLDAPGroupAttributeIsDN" "AuthLDAPRemoteUserIsDN"
	       "AuthLDAPStartTLS" "AuthLDAPUrl"
	       ;; mod_cgid
	       "ScriptLog" "ScriptLogBuffer" "ScriptLogLength" "ScriptSock"
	       ;; mod_ext_filter
	       "ExtFilterDefine" "ExtFilterOptions"
	       ;; mod_suexec
	       "SuexecUserGroup"
	       ;; mod_file_cache
	       "CacheFile" "MMapFile"
	       ;; mod_cache
	       "CacheDefaultExpire" "CacheDisable" "CacheEnable"
	       "CacheIgnoreCacheControl" "CacheIgnoreNoLastMod"
	       "CacheLastModifiedFactor" "CacheMaxExpire" "CacheOn"
	       ;; mod_dav
	       "Dav" "DavDepthInfinity" "DavLockDB" "DavMinTimeout"
	       ;; mod_deflate
	       "DeflateFilterNote" "DeflateMemLevel" "DeflateWindowSize"
	       ;; mod_ssl
	       "SSLCACertificateFile" "SSLCACertificatePath" "SSLCARevocationFile"
	       "SSLCARevocationPath" "SSLCertificateChainFile"
	       "SSLCertificateFile" "SSLCertificateKeyFile" "SSLCipherSuite"
	       "SSLEngine" "SSLLog" "SSLLogLevel" "SSLMutex" "SSLOptions"
	       "SSLPassPhraseDialog" "SSLProtocol" "SSLRandomSeed"
	       "SSLRequire" "SSLRequireSSL" "SSLSessionCache" 
	       "SSLSessionCacheTimeout" "SSLVerifyClient" "SSLVerifyDepth"
	       ;; mod_ldap
	       "LDAPCacheEntries" "LDAPCacheTTL" "LDAPCertDBPath"
	       "LDAPOpCacheEntries" "LDAPOpCacheTTL" "LDAPSharedCacheSize"

               ;;; non builtin apache modules 
               ;; mod_ssl
               "SSLPassPhraseDialog" "SSLMutex" "SSLRandomSeed"
               "SSLSessionCache" "SSLSessionCacheTimeout" "SSLEngine"
               "SSLProtocol" "SSLCipherSuite" "SSLCertificateFile"
               "SSLCertificateKeyFile" "SSLCertificateChainFile"
               "SSLCACertificatePath" "SSLCACertificateFile"
               "SSLCARevocationPath" "SSLCARevocationFile"
               "SSLVerifyClient" "SSLVerifyDepth" "SSLLog"
               "SSLLogLevel" "SSLOptions" "SSLRequireSSL"
               "SSLRequire"

               ;; Apache-SSL
               "SSLBanCipher" "SSLCACertificateFile"
               "SSLCACertificatePath" "SSLCacheServerPath"
               "SSLCacheServerPort" "SSLCacheServerRunDir"
               "SSLCertificateFile" "SSLCertificateKeyFile"
               "SSLCheckClientDN" "SSLDenySSL" "SSLDisable"
               "SSLEnable" "SSLEngineID" "SSLExportClientCertificates"
               "SSLFakeBasicAuth" "SSLKeyNoteTrustedAssertion"
               "SSLKeyNoteTrustedIssuerTemplate" "SSLNoCAList"
               "SSLRandomFile" "SSLRandomFilePerConnection"
               "SSLRequireCipher" "SSLRequireSSL" "SSLRequiredCiphers"
               "SSLSessionCacheTimeout" "SSLVerifyClient"
               "SSLVerifyDepth"

               ;; mod_perl 1 and 2
               "PerlAccessHandler" "PerlAddVar" "PerlAuthenHandler"
               "PerlAuthzHandler" "PerlChildExitHandler"
               "PerlChildInitHandler" "PerlCleanupHandler"
               "PerlFixupHandler" "PerlHeaderParserHandler"
               "PerlInitHandler" "PerlLogHandler" "PerlModule"
               "PerlPassEnv" "PerlPostReadRequestHandler"
               "PerlRequire" "PerlSetEnv" "PerlSetVar"
               "PerlTypeHandler"

               ;; mod_perl 1
               "PerlDispatchHandler" "PerlFreshRestart"
               "PerlHandler" "PerlOpmask" "PerlRestartHandler"
               "PerlScript" "PerlSendHeader" "PerlSetupEnv"
               "PerlTaintCheck" "PerlTransHandler" "PerlWarn"

               ;; mod_perl 2
               "PerlLoadModule" "PerlOptions" "PerlSwitches"
               "PerlOpenLogsHandler" "PerlPostConfigHandler"
               "PerlPreConnectionHandler" "PerlProcessConnectionHandler"
               "PerlInputFilterHandler" "PerlOutputFilterHandler"
               "PerlSetInputFilter" "PerlSetOutputFilter"
               "PerlResponseHandler" "PerlInterpStart"
               "PerlInterpMax" "PerlInterpMinSpare"
               "PerlInterpMaxSpare" "PerlInterpMaxRequests"
               "PerlInterpScope" "PerlTrace"

               ;; mod_python
               "PythonAccessHandler" "PythonAuthenHandler"
               "PythonAuthzHandler" "PythonAutoReload"
               "PythonCleanupHandler" "PythonConnectionHandler"
               "PythonDebug" "PythonEnablePdb" "PythonFixupHandler"
               "PythonHandler" "PythonHandlerModule"
               "PythonHeaderParserHandler" "PythonImport"
               "PythonInitHandler" "PythonInputFilter"
               "PythonInterpPerDirective" "PythonInterpPerDirectory"
               "PythonInterpreter" "PythonLogHandler" "PythonOptimize"
               "PythonOption" "PythonOutputFilter" "PythonPath"
               "PythonPostReadRequestHandler" "PythonTransHandler"
               "PythonTypeHandler"

               ) :test 'string=))
           "\\)\\s-")
	  1 'font-lock-keyword-face)

    (list (concat                                       ; values
	   "\\(?:^\\|\\W\\)\\("
           (regexp-opt
            (delete-duplicates
             '("allow" "deny" "on" "valid-user" "inetd" "standalone"
               "off" "user" "group" "any" "env" "mutual-failure" "full"
               "email" "force-response-1.0" "downgrade-1.0" "nokeepalive"
               "permanent" "temporary" "seeother" "gone" "All" "Options"
               "FileInfo" "AuthConfig" "Limit" "from" "None" "Basic"
               "Digest" "FancyIndexing" "IconsAreLinks" "ScanHTMLTitles"
               "SuppressLastModified" "SuppressSize" "SuppressDescription"
               "Minimal" "OS" "Full" "set" "append" "add" "unset" "none"
               "formatted" "semi-formatted" "unformatted" "error" "nocontent"
               "map" "referer" "URL" "inherit" "double" "GET" "PUT" "POST"
               "DELETE" "CONNECT" "OPTIONS" "Options" "Indexes" "Includes"
               "ExecCGI" "FollowSymLinks" "MultiViews" "IncludesNOEXEC"
               "SymLinksIfOwnerMatch"
               "uslock" "pthread" "sysvsem" "fcntl" "flock" "os2sem"
               "tpfcore" "default" "INode" "MTime"

               ;; mod_ssl
               "builtin" "exec" "none" "file" "sem" "egd" "dbm" "on" "off"
               "shm" "shmht" "shmcb"
               "SSLv2" "SSLv3" "TLSv1" "All" "startup" "connect"
               "optional" "require" "optional_no_ca" "ssl-unclean-shutdown"
               "error" "warn" "info" "trace" "debug"
               "StdEnvVars" "CompatEnvVars" "ExportCertData" "FakeBasicAuth"
               "StrictRequire" "OptRenegotiate"
               "ssl-accurate-shutdown" "ssl-unclean-shutdown"
               ;; cipher stuff
               "kRSA" "kDHr" "kDHd" "kEDH" "aNULL" "aRSA" "aDSS" "aDH" "eNULL"
               "DES" "3DES" "RC4" "RC2" "IDEA" "MD5" "SHA1" "SHA" "EXP"
               "EXPORT40" "EXPORT56" "LOW" "MEDIUM" "HIGH" "RSA" "DH" "EDH"
               "ADH" "DSS" "NULL"
               "DES-CBC3-SHA" "DES-CBC3-MD5" "IDEA-CBC-SHA" "RC4-SHA" "RC4-MD5"
               "IDEA-CBC-MD5" "RC2-CBC-MD5" "RC4-MD5" "DES-CBC-SHA"
               "RC4-64-MD5" "DES-CBC-MD5" "EXP-DES-CBC-SHA" "EXP-RC2-CBC-MD5"
               "EXP-RC4-MD5" "EXP-RC2-CBC-MD5" "EXP-RC4-MD5" "NULL-SHA"
               "NULL-MD5" "ADH-DES-CBC3-SHA" "ADH-DES-CBC-SHA" "ADH-RC4-MD5"
               "EDH-RSA-DES-CBC3-SHA" "EDH-DSS-DES-CBC3-SHA"
               "EDH-RSA-DES-CBC-SHA" "EDH-DSS-DES-CBC-SHA"
               "EXP-EDH-RSA-DES-CBC-SHA" "EXP-EDH-DSS-DES-CBC-SHA"
               "EXP-ADH-DES-CBC-SHA" "EXP-ADH-RC4-MD5"

               ;; Apache-SSL
               "file" "egd" "0" "1" "2" "3"
               ;; cipher stuff
               "IDEA-CBC-SHA" "NULL-MD5" "NULL-SHA" "EXP-RC4-MD5" "RC4-MD5"
               "RC4-SHA" "EXP-RC2-CBC-MD5" "IDEA-CBC-MD5" "EXP-DES-CBC-SHA"
               "DES-CBC-SHA" "DES-CBC3-SHA" "EXP-DH-DSS-DES-CBC-SHA"
               "DH-DSS-DES-CBC-SHA" "DH-DSS-DES-CBC3-SHA"
               "EXP-DH-RSA-DES-CBC-SHA" "DH-RSA-DES-CBC-SHA"
               "DH-RSA-DES-CBC3-SHA" "EXP-EDH-DSS-DES-CBC-SHA"
               "EDH-DSS-DES-CBC-SHA" "EDH-DSS-DES-CBC3-SHA"
               "EXP-EDH-RSA-DES-CBC" "EDH-RSA-DES-CBC-SHA"
               "EDH-RSA-DES-CBC3-SHA" "EXP-ADH-RC4-MD5" "ADH-RC4-MD"
               "EXP-ADH-DES-CBC-SHA" "ADH-DES-CBC-SHA" "ADH-DES-CBC3-SHA"
               "FZA-NULL-SHA" "FZA-FZA-CBC-SHA" "FZA-RC4-SHA" "DES-CFB-M1"
               "RC2-CBC-MD5" "DES-CBC-MD5" "DES-CBC3-MD5" "RC4-64-MD5" "NULL"

               ;; mod_asis
               "send-as-is"
               ;; mod_cgi
               "cgi-script"
               ;; mod_imap
               "imap-file"
               ;; mod_info
               "server-info"
               ;; mod_isapi
               "isapi-isa"
               ;; mod_ldap
               "ldap-status"
               ;; mod_status
               "server-status"

               ;; mod_perl
               "On" "Off" "perl-script"

               ;; mod_python
               "On" "Off" "python-program"

               ) :test 'string=))
           "\\)\\W")
	  1 'font-lock-type-face)))
  "Expressions to highlight in `apache-mode' buffers.")

(put 'apache-mode 'font-lock-defaults
     '(apache-font-lock-keywords nil t
                                 ((?_ . "w")
                                  (?- . "w"))))
;; Syntax table
(if apache-mode-syntax-table
    nil
  (setq apache-mode-syntax-table (copy-syntax-table nil))
  (modify-syntax-entry ?_   "_"     apache-mode-syntax-table)
  (modify-syntax-entry ?-   "_"     apache-mode-syntax-table)
  (modify-syntax-entry ?\(  "(\)"   apache-mode-syntax-table)
  (modify-syntax-entry ?\)  ")\("   apache-mode-syntax-table)
  (modify-syntax-entry ?\<  "(\>"   apache-mode-syntax-table)
  (modify-syntax-entry ?\>  ")\<"   apache-mode-syntax-table)
  (modify-syntax-entry ?\"   "\""   apache-mode-syntax-table))


;;;###autoload
(defun apache-mode ()
  "Major mode for editing Apache configuration files.

\\{apache-mode-map}

\\[apache-mode] runs the hook `apache-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map apache-mode-map)
  (set-syntax-table apache-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#\\W*")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
  (setq mode-name "Apache")
  (setq major-mode 'apache-mode)
  (run-hooks 'apache-mode-hook))


;; Provides
(provide 'apache-mode)

;;; apache-mode.el ends here
