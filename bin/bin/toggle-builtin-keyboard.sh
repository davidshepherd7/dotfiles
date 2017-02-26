#!/bin/bash

set -o errexit
set -o nounset

function builtin-keyboard-id() {
    xinput --list | grep "AT Translated" | sed 's/.*id=\([0-9]*\).*/\1/'
}

function is-disabled() {
    xinput --list --long | grep -A 1 "id=$1" | grep -q disabled
}

id="$(builtin-keyboard-id)"

if is-disabled "$id"; then
    echo "Enabling $id"
    xinput enable "$id"
else
    echo "Disabling $id"
    xinput disable "$id"
fi
