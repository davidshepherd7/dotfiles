#!/bin/bash

lang="$(setxkbmap -query | grep 'layout' | tr -d ' ' | cut -f 2 -d ':')"
layout="$(setxkbmap -query | grep 'variant' | tr -d ' ' | cut -f 2 -d ':')"

echo "$lang ${layout:-qwerty}"
