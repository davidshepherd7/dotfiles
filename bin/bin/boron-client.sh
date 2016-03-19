#!/bin/bash

set -o errexit
set -o nounset

exec /home/david/code/boron-unstable/build/bin/boron-client "$@"
