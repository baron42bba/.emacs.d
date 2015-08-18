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
;; Override the packages with the git version of Org and other packages

(setq load-path
(append
(list (expand-file-name "~/.emacs.d/lisp") (expand-file-name "~/.emacs.d/elpa/org-20150629"))
load-path))

(setq org-use-extra-keys t)
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
