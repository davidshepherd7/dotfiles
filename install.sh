#!/bin/bash

set -o errexit
set -o nounset

stow zsh -t $HOME

mkdir -p ~/.emacs.d
stow emacs -t $HOME

stow X -t $HOME

mkdir -p ~/.xmonad
stow xmonad -t $HOME

stow conky -t $HOME

stow git -t $HOME

mkdir -p ~/.ipython/profile_default/
mkdir -p ~/.ipython/profile_default/startup
stow ipython -t $HOME

mkdir -p ~/.config/sxhkd
stow sxhkd -t $HOME

stow readline -t $HOME

stow gdb -t $HOME

stow bash -t $HOME

# Note: requires sudo
sudo mkdir -p /etc/udev/hwdb.d
sudo stow hwdb -t /etc/udev/hwdb.d
