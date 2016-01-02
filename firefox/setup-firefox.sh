#!/bin/bash

set -o errexit
set -o nounset

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"


echo "Closing firefox first"
killall -e firefox || true
sleep 1

pref_js_files="$(find "$HOME/.mozilla/firefox/" -name 'prefs.js')"

# Handle multiple "firefox users" without any manual fiddling
for pref_js in $pref_js_files; do
    dir="$(dirname "$pref_js")"
    echo "linking user.js in dir $dir"
    mv "$dir/user.js" "$dir/user.js.bak"
    ln -s -t "$dir" "./user.js"
done
