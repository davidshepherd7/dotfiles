#!/bin/bash

set -o errexit
set -o nounset

eslint --fix -c "$HOME/code/boron-unstable/boron/web_applications/future-eslintrc.json" "$@"
