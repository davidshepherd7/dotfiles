#!/bin/bash

set -o errexit
set -o nounset

cd ~/Downloads

atool --extract hub-linux*tar.gz

mkdir -p ~/code/bin
cp ~/Downloads/hub-linux*/hub ~/code/bin/

chmod +x ~/code/bin/hub
