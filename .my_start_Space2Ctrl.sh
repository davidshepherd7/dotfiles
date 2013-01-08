#!/bin/bash

set -o errexit
set -o nounset

s2cdir=$1

origmap=$(xmodmap -pke | grep -E "^keycode[[:blank:]]*?65")
#newmap=$(echo ${origmap} | perl -pe "s/65[[:blank:]]*?=[[:blank:]]*?space/65  = Control_L/")
newmap=$(echo ${origmap} | perl -pe "s/ space/ Control_L/g")

xmodmap -e "$newmap"
xmodmap -e "keycode 255 = space VoidSymbol VoidSymbol VoidSymbol VoidSymbol"
nohup ${s2cdir}/Space2Ctrl >> ~/.Space2Ctrl.log 2>&1 &

# Don't ask me why but we need to wait more than one second else the ctrl
# modifier is reinitialized to XK_Control_L XK_Control_R as soon as keycode
# 65 is pressed!
sleep 2

xmodmap -e "add control = Control_L Control_R"

exit 0
