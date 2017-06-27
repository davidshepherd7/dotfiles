#! /usr/bin/env python3

import sys
import argparse

from subprocess import check_call

def parse_arguments(argv):
    parser = argparse.ArgumentParser(description=main.__doc__,

                                     # Don't mess up my formating in the help message
                                     formatter_class=argparse.RawDescriptionHelpFormatter)

    parser.add_argument('--rev', '-r', default='.', help = '')
    parser.add_argument('--bookmark', '-B', required=True, help = '')
    parser.add_argument('repo', default=None, help='')

    args = parser.parse_args(argv)
    return args


def main(argv):
    """
    Push specified revision as a specified bookmark to repo.
    """

    args = parse_arguments(argv)

    check_call(['hg', 'pull', '-B', args.bookmark, args.repo])
    check_call(['hg', 'bookmark', '-f', '-r', args.rev, args.bookmark])
    check_call(['hg', 'push', '-B', args.bookmark, args.repo])

    return 0


# If this script is run from a shell then run main() and return the result.
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
