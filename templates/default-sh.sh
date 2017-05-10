#!/bin/${1:sh}
:<<=cut
=pod

=head1 NAME

`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))` - $2

=head1 DESCRIPTION


=head1 SYNOPSIS

  `(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`

 --help, -h    for help
 --man,  -m    to get this man page.
 --verbose, -v be more verbose.

=head1 EXAMPLES

  `(downcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))` -p password


=cut


# parse parameters
TEMP=\`getopt -o mhvp: --long man,help,verbose,password: -n '`(downcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`' -- "$@"\`

if [ $? != 0 ]; then echo "Wrong parameters..." >&2; exit 1; fi

eval set -- "$TEMP"

while true; do
    case "\$1" in
	-p | --password ) PASSWORD="\$2"; shift 2 ;;
	-h | --help ) pod2usage \$0; exit ;;
	-m | --man ) perldoc \$0; exit ;;
	-v | --verbose ) VERBOSE=1; shift ;;
	* ) break ;;
    esac
done

EXITCODE=0

if [ -z "${PASSWORD}" ]; then
    read -s -p "Password: " PASSWORD
    echo
fi

if [ -z "${PASSWORD}" ]; then pod2usage $0; exit; fi

$0

:<<=cut
=head1 AUTHOR


`(user-full-name)` <`user-mail-address`>

=head1 LICENSE

This program is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
