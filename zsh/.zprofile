eval $(keychain --eval --agents ssh -Q --quiet)

# In order for sxhkd to start a new `emacs --daemon` when no server is
# available we need to set this here.
export ALTERNATE_EDITOR=''

# # Startx automatically on tty 1
# if [ -z "$DISPLAY" ] && [[ $(tty) == /dev/tty1 ]]; then
#     startx
# fi

unset MANPATH
export MANPATH="$(manpath)"

export PATH="$HOME/.cargo/bin:$PATH"

export PATH="$HOME/.npm-global/bin:$PATH"
export MANPATH="$HOME/.npm-global/share/man:$MANPATH"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
