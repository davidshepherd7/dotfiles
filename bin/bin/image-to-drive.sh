#!/bin/bash

set -o errexit
set -o nounset

image_file="$1"

drive push "$image_file"
drive pub "$image_file" | sed -e 's|^.*published on \(.*\)$|\1|' | tee /dev/fd/2 | xclip -sel clip
