#! /bin/bash

set -o errexit
set -o nounset


# TODO ??ds
# Make caps lock button generate caps lock when tapped or alt gr when held
# xcape_str=$xcape_str''
# or dollar maybe? &? smart depending on emacs mode?
# bind super keys as well? Figure out how to get alt gr to bind keys?

lang="${1:-gb}"

# Reset to standard keyboard layout
setxkbmap "${lang}" -variant "${2:-colemak}"
killall xcape || true


# Make alt gr just normal alt:
if [[ "$lang" = "gb" ]]; then
    xmodmap -e "keycode 108 = Alt_L"
fi

# Make caps lock another function key (which we can bind to whatever we like)
xmodmap -e "keycode 66 = F35"

# String ready to store out xcape commands (so that we only run it once)
xcape_str=""


# # Space to ctrl
# # ============================================================
# # Map an unused modifier's keysym to the spacebar's keycode and make it a
# # control modifier. It needs to be an existing key so that emacs won't
# # spazz out when you press it.
spare_modifier="Hyper_L"
# xmodmap -e "keycode 65 = $spare_modifier"
xmodmap -e "remove mod4 = $spare_modifier" # hyper_l is mod4 by default
xmodmap -e "add Control = $spare_modifier"

# # Map space to a new keycode which has no corresponding key (to
# # keep it around for xcape to use).
# xmodmap -e "keycode any = space"

# # Finally use xcape to cause the space bar to generate a space when tapped.
# xcape_str=$xcape_str"$spare_modifier=space;"


# pedal keys
# ============================================================
xmodmap -e "keycode any = XF86Tools" -e "keysym XF86Tools = $spare_modifier"
xmodmap -e "keycode any = XF86Launch5" -e "keysym XF86Launch5 = Alt_L"
xmodmap -e "keycode any = XF86Launch6" -e "keysym XF86Launch6 = Super_L"


# Generating letters by pressing modifiers (with xcape)
# ============================================================

# Use keycodes for xcape keys so that rebinding those buttons doesn't break
# things, for example in keyboard_force_correct_hands.sh.

# Tapping shift buttons generates ( or ).
xcape_str=$xcape_str'#50=parenleft;'
xcape_str=$xcape_str'#62=parenright;'

# Tapping ctrl buttons generates " and _. ??ds use mode_switch key here if
# we use keyboard_force_correct_hands.sh, otherwise need to manually swap
# it to Shift_R!
xcape_str=$xcape_str'#37=Shift_L|quotedbl;'
xcape_str=$xcape_str'#105=Shift_L|underscore;'

# Tapping alt buttons generates something
xcape_str=$xcape_str'#64=Shift_L|dollar;'
# xcape_str=$xcape_str'#92=Shift_L|asciicircum;' # doesn't work :(

# # Symbols bound to something else using xcape, so don't press them!
# xmodmap -e "keysym 9 = 9 $UbKS $UBKS $UBKS"
# xmodmap -e "keysym 0 = 0 $UBKS $UBKS $UBKS"
# xmodmap -e "keysym minus = minus $UBKS $UBKS $UBKS"
# xmodmap -e "keysym 2 = 2 $UBKS $UBKS $UBKS"


# Execute all the xcape stuff we just set up:
xcape -e $xcape_str


# Mouse speed stuff
# ============================================================

# disable the pointing stick, if the stick doesn't exist this does nothing
xinput -set-prop "DualPoint Stick" "Device Enabled" 0 &

# Slow dooowwwn the mouse
xset m 1 0
(xinput --list --short |\
     grep 'pointer' | grep 'Microsoft' | awk '{print $8}' | cut -d'=' -f 2 |\
     xargs -n1 -I{} xinput --set-prop {} "Device Accel Constant Deceleration" 2) || true
# Bloody microsoft put a trademark symbol in the device name, so we have grep
# + awk fun times.

# Ubuntu 18.04 is a bit different
mouse_id="$(xinput --list --short | grep 'Microsoft.*Mouse' | sed -E 's/.*id=([0-9]+).*/\1/')"
xinput --set-prop "$mouse_id" "libinput Accel Speed" -1
