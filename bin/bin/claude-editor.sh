#!/bin/bash -eu

set -o pipefail

if [ $# -lt 1 ] || [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    cat << EOF >&2
Open a file in an editor so that the user can interact with it.

Usage: claude-editor.sh FILE...

Every FILE can be either just a FILENAME or [+LINE[:COLUMN]] FILENAME.

EOF
    exit 0
fi

emacsclient -n -- "$@"
