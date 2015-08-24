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
