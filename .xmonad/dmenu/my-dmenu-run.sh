#!/bin/sh
exe=`dmenu_path | ~/.cabal/bin/yeganesh -f -- ${1+"$@"}` && exec $exe
