#!/bin/bash

set -o errexit
set -o nounset

tempfile="$(mktemp "hg-clean-whitespace-$USER.XXXXXXX" --tmpdir)"
backupfile="$(mktemp "hg-clean-whitespace-backup-$USER.XXXXXXX" --tmpdir)"

# Disable user customisations
export HGPLAIN="on"

substatus="$(cd "$(hg root)" && awk '{print $2}' .hgsubstate | xargs -n1 -I{} sh -c 'cd {} && hg status --')"

if [ "$substatus" != "" ]; then
    cat <<EOF >&2
Sub repos not clean:
    $substatus
Aborting!
EOF
    exit 5
fi

# Backup a diff
hg diff > "$backupfile"
echo "Stored a backup diff in $backupfile"

hg diff -w > "$tempfile"
hg revert --all
hg import --no-commit "$tempfile"
