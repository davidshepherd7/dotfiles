#!/bin/bash -eu

set -o pipefail

if [ $# -lt 1 ] || [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    cat << EOF >&2
Launch a terminal for the user in a directory.

Usage: claude-term.sh [DIR]

EOF
    exit 0
fi

cd "$1" && urxvt &
