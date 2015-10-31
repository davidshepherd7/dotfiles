#!/bin/bash

layout="$(setxkbmap -query | grep variant | tr -d ' ' | cut -f 2 -d ':')"
layout_2="${layout:-qwerty}"

echo "$layout_2"
