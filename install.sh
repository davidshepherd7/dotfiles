#!/bin/bash

set -o errexit
set -o nounset

stow zsh -t $HOME
stow emacs -t $HOME
stow X -t $HOME

mkdir -p ~/.xmonad
stow xmonad -t $HOME

stow conky -t $HOME

stow ipython -t $HOME
