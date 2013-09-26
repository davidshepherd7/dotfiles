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
./xcape -e '0x1234=space'


# Tapping shift buttons generates ( or ).
xcape_str=$xcape_str'Shift_L=parenleft;'
xcape_str=$xcape_str'Shift_R=parenright;'

# Tapping ctrl buttons generates " and _
xcape_str=$xcape_str'Control_L=Shift_R|quotedbl;'
xcape_str=$xcape_str'Control_R=Shift_R|underscore;'

# TODO ??ds
# Make caps lock alt gr

# Make caps lock button generate caps lock when tapped
# xcape_str=$xcape_str''


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
