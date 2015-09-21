#!/bin/bash

set -o errexit
set -o nounset

stow zsh -t $HOME --ignore=".dropbox.attr"

mkdir -p ~/.emacs.d
stow emacs -t $HOME --ignore=".dropbox.attr"

stow X -t $HOME --ignore=".dropbox.attr"

mkdir -p ~/.xmonad
stow xmonad -t $HOME --ignore=".dropbox.attr"

stow conky -t $HOME --ignore=".dropbox.attr"

stow git -t $HOME --ignore=".dropbox.attr"

mkdir -p ~/.ipython/profile_default/
mkdir -p ~/.ipython/profile_default/startup
stow ipython -t $HOME --ignore=".dropbox.attr"

mkdir -p ~/.config/sxhkd
stow sxhkd -t $HOME --ignore=".dropbox.attr"

stow readline -t $HOME --ignore=".dropbox.attr"

stow gdb -t $HOME --ignore=".dropbox.attr"

stow bash -t $HOME --ignore=".dropbox.attr"

# Note: requires sudo
sudo mkdir -p /etc/udev/hwdb.d
sudo stow hwdb -t /etc/udev/hwdb.d --ignore=".dropbox.attr"

# Just do a plain copy for this, links don't seem to work
sudo cp desktop-session/xsession.desktop /usr/share/xsessions/
