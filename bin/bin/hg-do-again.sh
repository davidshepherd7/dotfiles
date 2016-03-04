#!/bin/bash

set -o errexit
set -o nounset

# Hopefully using parents/children revsets will cause this to fail if used
# around branching (where it probabaly won't do the right thing), but I
# need to test this.
hg up -r 'parents(.)'
hg import --no-commit <(hg diff -r '.:children(.)')

echo "Remember to use strip to remove the old head when you are done"
