#! /bin/bash


# TODO ??ds
# Make caps lock button generate caps lock when tapped or alt gr when held
# xcape_str=$xcape_str''
# or dollar maybe? &? smart depending on emacs mode?
# bind super keys as well? Figure out how to get alt gr to bind keys?


# Reset to standard keyboard layout (just in case...)
setxkbmap gb -variant colemak
killall xcape


# Make alt gr just normal alt:
xmodmap -e "keycode 108 = Alt_L"

# Make insert be caps lock
xmodmap -e "keycode 118 = Caps_Lock"


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
$HOME/code/xcape/xcape -e $xcape_str
