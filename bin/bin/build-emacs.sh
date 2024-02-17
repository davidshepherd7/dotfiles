#!/bin/bash -eu

set -o pipefail

cd ~/code/emacs

# Ensure these packages are installed first:
#
# apt-transport-https 
# ca-certificates 
# gnupg-agent
# software-properties-common
# libgccjit-10-dev
# gcc-10
# libgccjit0
# libjansson4
# libjansson-dev

# And make sure that tree sitter is built and installed

git checkout emacs-29.1
git clean -Xf
./autogen.sh
CC="gcc-10" CFLAGS="-O3 -march=native" ./configure --with-x-toolkit="lucid" --with-json --with-native-compilation --with-tree-sitter
make

echo "Try out ~/code/emacs/src/emacs, if it works do sudo make install"
