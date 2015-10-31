#!/bin/bash

set -o errexit
set -o nounset

usage="Print number of files contained in sub-directories (counts
recursively).

    usage:    file_count.h [dir]"


if [ "$#" -gt 0 ]; then
    if [ "$1" == "-h" ]; then
        echo "$usage"
        exit 1
    fi
    dir="$1"
else
    dir="."
fi

cd "$dir"

paste <(ls -1A | tr '\n' '\0' | xargs -0 -n1 __count_one.sh) <(ls -1A)
