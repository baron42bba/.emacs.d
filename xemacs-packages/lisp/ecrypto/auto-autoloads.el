;;; DO NOT MODIFY THIS FILE
(if (featurep 'ecrypto-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "ecrypto/_pkg.el")

(package-provide 'ecrypto :version 0.2 :author-version "2.0" :type 'regular)

;;;***

;;;### (autoloads (encrypt-insert-file-contents encrypt-find-model) "encrypt" "ecrypto/encrypt.el")

(autoload 'encrypt-find-model "encrypt" "\
Given a filename, find a encrypt-file-alist entry" nil nil)

(autoload 'encrypt-insert-file-contents "encrypt" "\
Decrypt FILE into the current buffer." t nil)

;;;***

;;;### (autoloads (md4) "md4" "ecrypto/md4.el")

(autoload 'md4 "md4" "\
Returns the MD4 hash string of 16 bytes long for a string IN of N
bytes long.  N is required to handle strings containing character 0." nil nil)

;;;***

;;;### (autoloads (md5) "md5-el" "ecrypto/md5-el.el")

(autoload 'md5 "md5-el" "\
Return the MD5 (a secure message digest algorithm) of an object.
OBJECT is either a string or a buffer.
Optional arguments START and END denote buffer positions for computing the
hash of a portion of OBJECT." nil nil)

;;;***

;;;### (autoloads (sha1) "sha1" "ecrypto/sha1.el")

(autoload 'sha1 "sha1" "\
Return the SHA1 (Secure Hash Algorithm) of an object.
OBJECT is either a string or a buffer.
Optional arguments BEG and END denote buffer positions for computing the
hash of a portion of OBJECT.
If BINARY is non-nil, return a string in binary form." nil nil)

;;;***

(provide 'ecrypto-autoloads)
