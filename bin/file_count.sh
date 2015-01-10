#!/bin/bash

set -o errexit
set -o nounset

if [ "$#" -gt 0 ]; then
    dir="$1"
else
    dir="."
fi

cd "$dir"

paste <(ls -1A | tr '\n' '\0' | xargs -0 -n1 count_one.sh) <(ls -1A)
