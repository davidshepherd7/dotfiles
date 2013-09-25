#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi


# ============================================================
# End of /etc/skel/.xinitrc



# Standard keyboard layout
setxkbmap gb

# right alt is just alt:
xmodmap -e "clear mod5"
xmodmap -e "keycode 108 = Alt_L"


# Set desktop background
feh --bg-scale $HOME/Dropbox/other/wallpapers

# Start dropbox
dropbox start

# Transparancy for emacs
xcompmgr &

# # Emacs daemon (via bash to make sure we get all the $PATH etc. variables
# # right).
# bash -l -c "emacs --daemon" &

# # Maybe call something if we recognise the pc name
# if
# ./uni-pc-init.sh # needs sudo..

# and finally run xmonad itself
xmonad
