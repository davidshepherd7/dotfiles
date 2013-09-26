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


# Key binds
# ============================================================

# Standard keyboard layout (just in case...)
setxkbmap gb
killall xcape


# Force myself to press the right shift buttons
xmodmap ~/Dropbox/linux_setup/rcfiles/keyboard_force_correct_hands.xmm

# Make alt gr just normal alt:
xmodmap -e "clear mod5"
xmodmap -e "keycode 108 = Alt_L"


# *Space to ctrl*
# Map a new (currently non-existant) keysym to the spacebar's
# keycode and make it a control modifier.
xmodmap -e 'keycode 65 = 0x1234'
xmodmap -e 'add control = 0x1234'

# Map space to a new keycode which has no corresponding key (to
# keep it around for xcape to use).
xmodmap -e 'keycode any = space'

# Finally use xcape to cause our new keysym to generate a space
# when tapped.
xcape_str=""
xcape_str=$xcape_str'#65=space;'

# Use keycodes for xcape keys so that rebinding those buttons doesn't break
# things, for example in keyboard_force_correct_hands.sh.

# Tapping shift buttons generates ( or ).
xcape_str=$xcape_str'#50=parenleft;'
xcape_str=$xcape_str'#62=parenright;'

# Tapping ctrl buttons generates " and _. ??ds use mode_switch key here if
# we use keyboard_force_correct_hands.sh, otherwise need to manually swap
# it to Shift_R!
xcape_str=$xcape_str'#37=Mode_switch|quotedbl;'
xcape_str=$xcape_str'#105=Shift_L|underscore;'

# TODO ??ds
# Make caps lock button generate caps lock when tapped or alt gr when held
# xcape_str=$xcape_str''


# # Symbols bound to something else using xcape, so don't press them!
# xmodmap -e "keysym 9 = 9 $UbKS $UBKS $UBKS"
# xmodmap -e "keysym 0 = 0 $UBKS $UBKS $UBKS"
# xmodmap -e "keysym minus = minus $UBKS $UBKS $UBKS"
# xmodmap -e "keysym 2 = 2 $UBKS $UBKS $UBKS"


# Execute all the xcape stuff we just set up:
$HOME/code/xcape/xcape -e $xcape_str



# System specific code
# ============================================================

# TODO: run footpedal bindings if they exist



# Run things
# ============================================================

# Set desktop background (randomly, including sub folders)
feh --bg-scale $HOME/Dropbox/other/wallpapers -zr

# Start dropbox
dropbox start

# Transparancy for emacs
xcompmgr &

# Get all the pretty mint settings for MATE
/usr/bin/mate-settings-daemon &

# Get some nice things from MATE:
mate-panel &
mate-volume-control-applet &
# mate-keyring-daemon --start --components=gpg &
nm-applet &
mate-power-manager &
system-config-printer-applet &
/usr/bin/mate-keyring-daemon --start --components=ssh &
mate-screensaver &

start-pulseaudio-x11 &

# Disable access control on the X-server (should be safe, mint uses this
# setting)
xhost +

# and finally run xmonad itself
xmonad
