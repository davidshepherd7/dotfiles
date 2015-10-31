#!/bin/bash

set -o errexit
set -o nounset

emacsclient -c "$@"
