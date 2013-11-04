;;; DO NOT MODIFY THIS FILE
(if (featurep 'pgg-autoloads) (error "Already loaded"))

;;;### (autoloads nil "_pkg" "pgg/_pkg.el")

(package-provide 'pgg :version 1.06 :author-version "0.1" :type 'regular)

;;;***

;;;### (autoloads (pgg-snarf-keys pgg-snarf-keys-region pgg-insert-key pgg-verify pgg-verify-region pgg-sign pgg-sign-region pgg-decrypt pgg-decrypt-region pgg-encrypt pgg-encrypt-region) "pgg" "pgg/pgg.el")

(autoload 'pgg-encrypt-region "pgg" "\
Encrypt the current region between START and END for RCPTS.
If optional argument SIGN is non-nil, do a combined sign and encrypt." t nil)

(autoload 'pgg-encrypt "pgg" "\
Encrypt the current buffer for RCPTS.
If optional argument SIGN is non-nil, do a combined sign and encrypt.
If optional arguments START and END are specified, only encrypt within
the region." t nil)

(autoload 'pgg-decrypt-region "pgg" "\
Decrypt the current region between START and END." t nil)

(autoload 'pgg-decrypt "pgg" "\
Decrypt the current buffer.
If optional arguments START and END are specified, only decrypt within
the region." t nil)

(autoload 'pgg-sign-region "pgg" "\
Make the signature from text between START and END.
If the optional 3rd argument CLEARTEXT is non-nil, it does not create
a detached signature.
If this function is called interactively, CLEARTEXT is enabled
and the the output is displayed." t nil)

(autoload 'pgg-sign "pgg" "\
Sign the current buffer.
If the optional argument CLEARTEXT is non-nil, it does not create a
detached signature.
If optional arguments START and END are specified, only sign data
within the region.
If this function is called interactively, CLEARTEXT is enabled
and the the output is displayed." t nil)

(autoload 'pgg-verify-region "pgg" "\
Verify the current region between START and END.
If the optional 3rd argument SIGNATURE is non-nil, it is treated as
the detached signature of the current region.

If the optional 4th argument FETCH is non-nil, we attempt to fetch the
signer's public key from `pgg-default-keyserver-address'." t nil)

(autoload 'pgg-verify "pgg" "\
Verify the current buffer.
If the optional argument SIGNATURE is non-nil, it is treated as
the detached signature of the current region.
If the optional argument FETCH is non-nil, we attempt to fetch the
signer's public key from `pgg-default-keyserver-address'.
If optional arguments START and END are specified, only verify data
within the region." t nil)

(autoload 'pgg-insert-key "pgg" "\
Insert the ASCII armored public key." t nil)

(autoload 'pgg-snarf-keys-region "pgg" "\
Import public keys in the current region between START and END." t nil)

(autoload 'pgg-snarf-keys "pgg" "\
Import public keys in the current buffer." t nil)

;;;***

(provide 'pgg-autoloads)
