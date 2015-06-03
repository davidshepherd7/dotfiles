#!/bin/bash

set -o errexit
set -o nounset

stow zsh -t $HOME
stow emacs -t $HOME
