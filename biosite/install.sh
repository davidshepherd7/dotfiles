#!/bin/bash

set -o errexit
set -o nounset

SCRIPTDIR="$( cd "$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")" && pwd )"

if [ $# -lt 1 ]; then
    echo "Required argument: boron directory" >&2
    exit 1
fi

cd "$1"

rm .hg/hgrc .dir-locals.el common/.dir-locals.el
ln -s "$SCRIPTDIR/boron-hgrc" .hg/hgrc
ln -s "$SCRIPTDIR/boron-dir-locals.el" .dir-locals.el
ln -s "$SCRIPTDIR/common-dir-locals.el" common/.dir-locals.el
