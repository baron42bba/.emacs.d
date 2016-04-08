#!/bin/${1:sh}
:<<=cut
=pod

=head1 NAME

`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))` - $2

=head1 DESCRIPTION


=head1 SYNOPSIS

  `(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`

 --help, -h   for help
 --man,  -m   to get this man page.
 --verbose, -v be more verbose.

=cut


# parse parameters

for param in "$@"
do
    case $param in
	-k|--keep)
	    KEEP=1
	    shift # past argument=value
	    ;;
	-v|--verbose)
	    VERBOSE=1
	    shift # past argument=value
	    ;;
	-h|--help)
	    pod2usage \$0
	    exit
	    ;;
	-m|--man)
	    perldoc \$0
	    exit
	    ;;
	*)
	    echo "unknown option $param"
	    echo
	    pod2usage \$0
	    exit
	    ;;
    esac
done

EXITCODE=0

$0

:<<=cut
=head1 AUTHOR


`(user-full-name)` <`user-mail-address`>

=head1 LICENSE

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
