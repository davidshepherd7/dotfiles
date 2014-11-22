eval $(keychain --eval --agents ssh -Q --quiet)

# Startx automatically on tty 1
if [ -z "$DISPLAY" ] && [[ $(tty) == /dev/tty1 ]]; then
    startx
fi

