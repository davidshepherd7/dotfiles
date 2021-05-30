#!/bin/bash -eu

set -o pipefail


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
