#!/bin/bash

set -o errexit
set -o nounset

tempfile="$(mktemp "hg-clean-whitespace-$USER.XXXXXXX" --tmpdir)"
backupfile="$(mktemp "hg-clean-whitespace-backup-$USER.XXXXXXX" --tmpdir)"
# Disable user customisations
export HGPLAIN="on"

# Backup a diff
hg diff > "$backupfile"
echo "Stored a backup diff in $backupfile"

hg diff -w > "$tempfile"
hg revert --all
hg import --no-commit "$tempfile"
