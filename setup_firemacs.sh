#!/bin/bash

set -o errexit
set -o nounset


if pgrep firefox > /dev/null; then
    echo "Close firefox first!"
    exit 5
fi

files=$(find "$HOME/.mozilla/firefox/" -name 'prefs.js')
firemacs_conf="$(pwd)/firemacs"

# Handle multiple "firefox users" without any manual fiddling
for preffile in $files; do
    cp $preffile ${preffile}.backup
    cat $preffile | sed '/firemacs/d' | cat - "$firemacs_conf" > $preffile
done
