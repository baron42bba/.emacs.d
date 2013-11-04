;;;###autoload
(package-provide 'xemacs-base
		 :version 2.03
          :author-version "No-Upstream-Ver"
		 :type 'regular)

;;;###autoload
(when (fboundp 'package-suppress)
      (package-suppress 'xemacs-base "regexp-opt" '(fboundp 'package-suppress)) (package-suppress 'xemacs-base "easy-mmode" '(fboundp 'package-suppress))
)
