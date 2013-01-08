#!/bin/bash

set -o errexit
set -o nounset

folders="$HOME/Dropbox/ \n
$HOME/Downloads/ \n
$HOME/ \n
$HOME/Dropbox/phd/reports/ongoing-writeup"

echo -e $folders | ~/.cabal/bin/yeganesh -f -- -i | xargs -n1 thunar



