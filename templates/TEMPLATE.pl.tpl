#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;

my %params;

=head1 NAME

(>>>FILE<<<) -

=head1 DESCRIPTION

(>>>POINT<<<)

=head1 SYNOPSIS

=head1 EXAMPLES

=head1 SUBS

=head2 main

=cut

sub main {

    my $result = GetOptions (
			     "help|h"		=> \$params{help},
			     "verbose|v"        => \$params{verbose},
			     "man|m"		=> \$params{man}
			    );

    if ( $params{man} ) { pod2usage( -verbose => 2 ); }

    if ( ! ( $server ) || $params{help} )
    {
	pod2usage(1);
    }


}

main();

=head1 AUTHOR


(>>>USER_NAME<<<) <(>>>AUTHOR<<<)>

=head1 LICENSE

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
