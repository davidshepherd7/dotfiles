#!/bin/sh

# A script to rebind all shifted keys to force you to use the correct shift
# button for minimum risk of rsi.

# reset keyboard
setxkbmap gb

# Set right shift to be a temporary caps lock (ony on while held)
xmodmap -e "keysym Shift_R = Mode_switch"


# Buttons not really on either side, leave these alone so either works.
# 6 asciicircum
# y Y
# b B


# Bind lhs of keyboard to only have its case affected by by this "temporary
# caps lock modifier".

# letters
xmodmap -e "keysym q = q q Q Q"
xmodmap -e "keysym w = w w W W"
xmodmap -e "keysym e = e e E E"
xmodmap -e "keysym r = r r R R"
xmodmap -e "keysym t = t t T T"
xmodmap -e "keysym a = a a A A"
xmodmap -e "keysym s = s s S S"
xmodmap -e "keysym d = d d D D"
xmodmap -e "keysym f = f f F F"
xmodmap -e "keysym g = g g G G"
xmodmap -e "keysym z = z z Z Z"
xmodmap -e "keysym x = x x X X"
xmodmap -e "keysym c = c c C C"
xmodmap -e "keysym v = v v V V"

# symbols
xmodmap -e "keysym backslash = backslash backslash bar bar"
xmodmap -e "keysym grave = grave grave notsign notsign"
xmodmap -e "keysym 1 = 1 1 exclam"
xmodmap -e "keysym 2 = 2 2 quotedbl"
xmodmap -e "keysym 3 = 3 3 sterling"
xmodmap -e "keysym 4 = 4 4 dollar"
xmodmap -e "keysym 5 = 5 5 percent"



# Bind rhs of keyboard to only have its case affected by normal shift (not
# caps lock).

xmodmap -e "keysym u = u U u U"
xmodmap -e "keysym i = i I i I"
xmodmap -e "keysym o = o O o O"
xmodmap -e "keysym p = p P p P"
xmodmap -e "keysym h = h H h H"
xmodmap -e "keysym j = j J j J"
xmodmap -e "keysym k = k K k K"
xmodmap -e "keysym l = l L l L"
xmodmap -e "keysym n = n N n N"
xmodmap -e "keysym m = m M m M"

# symbols
xmodmap -e "keysym 7 = 7 ampersand 7 ampersand"
xmodmap -e "keysym 8 = 8 asterisk 8 asterisk"
xmodmap -e "keysym 9 = 9 parenleft 9 parenleft"
xmodmap -e "keysym 0 = 0 parenright 0 parenright"
xmodmap -e "keysym minus = minus underscore minus underscore"
xmodmap -e "keysym equal = equal plus equal plus"
xmodmap -e "keysym bracketleft = bracketleft braceleft bracketleft braceleft"
xmodmap -e "keysym bracketright = bracketright braceright bracketright braceright"
xmodmap -e "keysym semicolon = semicolon colon semicolon colon"
xmodmap -e "keysym apostraphe = apostraphe at apostraphe at"
xmodmap -e "keysym numbersign = numbersign asciitilde numbersign asciitilde"
xmodmap -e "keysym comma = comma less comma less"
xmodmap -e "keysym period = period greater period greater"
xmodmap -e "keysym slash = slash question slash question"
