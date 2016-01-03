#!/bin/bash

set -o errexit
set -o nounset

keystroke="CTRL+F5"

# The browser to refresh
BROWSER="${1:-firefox}"

# find all visible browser windows
browser_windows="$(xdotool search --sync --all --onlyvisible --name "$BROWSER")"

# Send keystroke
for bw in $browser_windows; do
    xdotool key --window "$bw" "$keystroke"
done
