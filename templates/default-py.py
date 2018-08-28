#!/usr/bin/env python3
"""Script to

Authors:
   `( bba-insert-name )`

"""

import argparse
import logging
import sys

def main():
    """Main function.
    """
    parser = argparse.ArgumentParser(description='Druid Rollups and Drops')
    parser.add_argument('--config-file', dest='config_file',
                        help='JSON config file', required=True)
    parser.add_argument('--template-dir', dest='template_dir',
                        help='Directory for druid templates', required=True)
    parser.add_argument('--log-level', dest='log_level',
                        help='Log level, can be one of: INFO, WARNING, ERROR, '
                        'DEBUG', default='INFO')
    args = parser.parse_args()
    logging.basicConfig(format='%(asctime)s %(levelname)s: %(message)s',
                        level=args.log_level)
    logging.getLogger("requests").setLevel(logging.WARNING)
    if len(args) != 1:
        _error('No parameters')
        parser.print_usage()
        sys.exit(1)
 

if __name__ == "__main__":
    main()

