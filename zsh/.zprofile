eval $(keychain --eval --agents ssh -Q --quiet)

# In order for sxhkd to start a new `emacs --daemon` when no server is
# available we need to set this here.
export ALTERNATE_EDITOR=''

# Startx automatically on tty 1
if [ -z "$DISPLAY" ] && [[ $(tty) == /dev/tty1 ]]; then
    startx
fi

export PATH="$HOME/.cargo/bin:$PATH"
