#!/bin/bash

set -o errexit
set -o nounset

tempfile="$(mktemp "git-clean-whitespace-$USER.XXXXXXX" --tmpdir)"
backupfile="$(mktemp "git-clean-whitespace-backup-$USER.XXXXXXX" --tmpdir)"

# Backup a diff
git diff > "$backupfile"
echo "Stored a backup diff in $backupfile"

git diff -w > "$tempfile"
git reset --hard HEAD

# Use patch not import so that this works within a histedit
(
    cd "$(git rev-parse --show-toplevel)"
    patch -p1 < "$tempfile"
)
