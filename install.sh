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
