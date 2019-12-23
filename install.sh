#!/bin/bash

set -o errexit
set -o nounset

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$DIR"

stow_file()
{
    stow "$1" -t "$HOME" --ignore=".dropbox.attr"
}

stow_file zsh
stow_file conky
stow_file git
stow_file hg
stow_file readline
stow_file gdb
stow_file bash
stow_file bin
stow_file calibre
stow_file psql
stow_file ag
stow_file parallel
stow_file R

# Cases which need dirs creating
mkdir -p ~/.ccache
stow_file ccache

mkdir -p ~/.emacs.d
stow_file emacs

stow_file X

mkdir -p ~/.xmonad
stow_file xmonad

mkdir -p ~/.ipython/profile_default/
mkdir -p ~/.ipython/profile_default/startup
stow_file ipython

mkdir -p ~/.config/sxhkd
stow_file sxhkd

mkdir -p ~/.keysnail/
# stow_file keysnail

mkdir -p ~/.config/
stow_file flake8

# Note: requires sudo
sudo mkdir -p /etc/udev/hwdb.d
sudo stow hwdb -t /etc/udev/hwdb.d --ignore=".dropbox.attr"

# Just do a plain copy for this, links don't seem to work
sudo cp desktop-session/xsession.desktop /usr/share/xsessions/

# Firefox needs extra tricks to find the user dir run:
# ./firefox/setup-firefox.sh, not done here because it's error prone and
# not idempotent.


# Don't store ssh config here because this is public, but we need to make a temp
# dir in there for connection multiplexing.
mkdir -p ~/.ssh/tmp

mkdir -p ~/.docsets/
stow_file docsets

sudo cp /usr/share/X11/xkb/symbols/pl /usr/share/X11/xkb/symbols/pl.bak
sudo cat /usr/share/X11/xkb/symbols/pl ./polish-gb-colemak | sudo tee /usr/share/X11/xkb/symbols/pl.temp >/dev/null
sudo mv /usr/share/X11/xkb/symbols/pl.temp /usr/share/X11/xkb/symbols/pl

sudo cp /usr/share/X11/xkb/symbols/fr /usr/share/X11/xkb/symbols/fr.bak
sudo cat /usr/share/X11/xkb/symbols/fr ./french-gb-colemak | sudo tee /usr/share/X11/xkb/symbols/fr.temp >/dev/null
sudo mv /usr/share/X11/xkb/symbols/fr.temp /usr/share/X11/xkb/symbols/fr
