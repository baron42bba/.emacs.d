#!/bin/bash
:<<=cut
=pod

=head1 NAME

make_emacsconfig.sh - initialize .emacs.d and keep it up to date.

=head1 DESCRIPTION

create .emacs and .emacs.d based on http://git.bundesbrandschatzamt.de/emacsconfig.git
if .emacs and .emacs.d exists fetch git.bundesbrandschatzamt.de emacs config.


=cut

if [ ! -e ${HOME}/.emacs.d ]; then
    git clone http://git.bundesbrandschatzamt.de/emacsconfig.git ${HOME}/.emacs.d
    cat >${HOME}/.emacs <<EOF
;; This sets up the load path so that we can override it
;; (package-initialize)
;; Override the packages with the git version of Org and other packages

  (setq load-path
  (append
  (list (expand-file-name "~/.emacs.d/"))
  ;;           (expand-file-name "/usr/share/emacs/lisp/")
  load-path))


(add-to-list 'load-path "~/.emacs.d/xemacs-packages/lisp/org-8.2.3c")

;; (add-to-list 'load-path "~/elisp/org-mode/contrib/lisp")
;; Load the rest of the packages
;; (package-initialize t)
;; (setq package-enable-at-startup nil)
(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/bba.org"))

EOF

else
    cd ${HOME}/.emacs.d
    git pull origin
fi



:<<=cut
=head1 AUTHOR


Andreas Gerler <baron@bundesbrandschatzamt.de>

=head1 LICENSE

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
