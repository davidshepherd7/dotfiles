#!/bin/bash

set -o errexit
set -o nounset

tempfile="$(mktemp XXXXXX.emacs-anywhere-8c7ed5092fff --tmpdir)"

emacsclient -c "$tempfile"

# Copy to all clipboards because some applications only use one of them
# (firefox!)
xclip "$tempfile" -selection primary
xclip "$tempfile" -selection clipboard
xclip "$tempfile" -selection secondary

# X11 paste key, should work everywhere
xdotool key shift+Insert
