eval $(keychain --eval --agents ssh -Q --quiet)

# This seems to make zsh startup ~4x faster on my laptop!
skip_global_compinit=1

# Set up path etc here so that it's ready sooner

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

export PATH="$PATH:/opt/bin"
export PATH="$PATH:$HOME/bin"
export PATH="$HOME/.npm-global/bin:$PATH"

export MANPATH="$HOME/.npm-global/share/man:$MANPATH"

# For some reason this runs an emacs daemon if emacs is not already running and
# you try to run an emacsclient
export EDITOR="emacsclient -c"
export ALTERNATE_EDITOR=''
