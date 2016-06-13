#!/bin/bash

set -o errexit
set -o nounset

keystroke="CTRL+F5"

browsers=(firefox chromium chrome)

current_window="$(xdotool getwindowfocus)"

for browser in "${browsers[@]}"; do
  # find all visible browser windows
  browser_windows="$(xdotool search --all --onlyvisible --name "$browser" || echo '')"

  # Send keystroke
  for bw in $browser_windows; do
    xdotool windowfocus "$bw"
    xdotool key --window "$bw" "$keystroke"
  done
done

xdotool windowfocus "$current_window"
