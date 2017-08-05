#! /usr/bin/env python3

import sys
import argparse
import re

from subprocess import check_call, check_output

def parse_arguments(argv):
    parser = argparse.ArgumentParser(description=main.__doc__,

                                     # Don't mess up my formating in the help message
                                     formatter_class=argparse.RawDescriptionHelpFormatter)

    parser.add_argument('--rev', '-r', default='.', help = '')
    parser.add_argument('--bookmark', '-B', required=True, help = '')
    parser.add_argument('repo', default=None, help='')
    parser.add_argument('--mercurial-binary', default='hg', help='Specify the mercurial to use, you may want to use this in aliases as `--mercurial-binary "$HG"` to avoid weirdness from mixing versions')

    args = parser.parse_args(argv)
    return args


def main(argv):
    """
    Push specified revision as a specified bookmark to repo.
    """

    args = parse_arguments(argv)

    pulled = check_output([args.mercurial_binary, 'pull', '-B', args.bookmark, args.repo]).decode('ascii')

    print(pulled)
    if re.match("adding changesets", pulled):
        print("Unseen changes found on bookmark", args.bookmark, "you should probably rebase first", file=sys.stderr)

    check_call([args.mercurial_binary, 'bookmark', '-f', '-r', args.rev, args.bookmark])
    check_call([args.mercurial_binary, 'push', '-B', args.bookmark, args.repo])

    return 0


# If this script is run from a shell then run main() and return the result.
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
