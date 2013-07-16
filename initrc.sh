#!/bin/bash

# Transparency
xcompmgr & 

# right alt_gr same as left alt
xmodmap -e "clear mod5"
xmodmap -e "keycode 108 = Alt_L"

# space2ctrl
cd code/space2ctrl/
./start_Space2Ctrl.sh