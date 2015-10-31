#!/bin/bash

# Helper function for file_count.sh

# Much easier to put it in it's own script for use with xargs than messing
# around trying to export functions from zsh to bash!

set -o errexit
set -o nounset

find "$1" -type f | wc -l
