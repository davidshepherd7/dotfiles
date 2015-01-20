#!/bin/bash

set -o errexit
set -o nounset

cd ~/Downloads

7z x "hub*.gz.tar"
7z x "hub*.gz"

cp Downloads/hub_*_linux_amd64/hub ~/code/bin

chmod +x ~/code/bin/hub
