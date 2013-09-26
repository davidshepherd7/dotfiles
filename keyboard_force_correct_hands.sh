#!/bin/sh

# A script to rebind all shifted keys to force you to use the correct shift
# button for minimum risk of rsi.

# reset keyboard
setxkbmap gb

# Set right shift to be a temporary caps lock (ony on while held)
xmodmap -e "keysym Shift_R = Mode_switch"

# Buttons not really on either side, leave these alone so either shift works:
# 6 asciicircum
# y Y
# b B


# Set this to a keysym which is not used elsewhere in your key map so that
# the wrong key generates nothing. This keysym should be fine for most
# people. Note: this may interfere with emacs in especially annoying ways
# because emacs like to tell you when you press unbound keys.
UBKS="0x3571"


# Bind lhs of keyboard to only have its case affected by by this "temporary
# caps lock modifier".

# letters
xmodmap -e "keysym q = q $UBKS Q Q"
xmodmap -e "keysym w = w $UBKS W W"
xmodmap -e "keysym e = e $UBKS E E"
xmodmap -e "keysym r = r $UBKS R R"
xmodmap -e "keysym t = t $UBKS T T"
xmodmap -e "keysym a = a $UBKS A A"
xmodmap -e "keysym s = s $UBKS S S"
xmodmap -e "keysym d = d $UBKS D D"
xmodmap -e "keysym f = f $UBKS F F"
xmodmap -e "keysym g = g $UBKS G G"
xmodmap -e "keysym z = z $UBKS Z Z"
xmodmap -e "keysym x = x $UBKS X X"
xmodmap -e "keysym c = c $UBKS C C"
xmodmap -e "keysym v = v $UBKS V V"

# symbols
xmodmap -e "keysym backslash = backslash $UBKS bar bar"
xmodmap -e "keysym grave = grave $UBKS notsign notsign"
xmodmap -e "keysym 1 = 1 $UBKS exclam"
xmodmap -e "keysym 2 = 2 $UBKS quotedbl"
xmodmap -e "keysym 3 = 3 $UBKS sterling"
xmodmap -e "keysym 4 = 4 $UBKS dollar"
xmodmap -e "keysym 5 = 5 $UBKS percent"



# Bind rhs of keyboard to only have its case affected by normal shift (not
# caps lock).

# letters
xmodmap -e "keysym u = u U $UBKS U"
xmodmap -e "keysym i = i I $UBKS I"
xmodmap -e "keysym o = o O $UBKS O"
xmodmap -e "keysym p = p P $UBKS P"
xmodmap -e "keysym h = h H $UBKS H"
xmodmap -e "keysym j = j J $UBKS J"
xmodmap -e "keysym k = k K $UBKS K"
xmodmap -e "keysym l = l L $UBKS L"
xmodmap -e "keysym n = n N $UBKS N"
xmodmap -e "keysym m = m M $UBKS M"

# symbols
xmodmap -e "keysym 7 = 7 ampersand $UBKS ampersand"
xmodmap -e "keysym 8 = 8 asterisk $UBKS asterisk"
xmodmap -e "keysym 9 = 9 parenleft $UBKS parenleft"
xmodmap -e "keysym 0 = 0 parenright $UBKS parenright"
xmodmap -e "keysym minus = minus underscore $UBKS underscore"
xmodmap -e "keysym equal = equal plus $UBKS plus"
xmodmap -e "keysym bracketleft = bracketleft braceleft $UBKS braceleft"
xmodmap -e "keysym bracketright = bracketright braceright $UBKS braceright"
xmodmap -e "keysym semicolon = semicolon colon $UBKS colon"
xmodmap -e "keysym apostrophe = apostrophe at $UBKS at"
xmodmap -e "keysym numbersign = numbersign asciitilde $UBKS asciitilde"
xmodmap -e "keysym comma = comma less $UBKS less"
xmodmap -e "keysym period = period greater $UBKS greater"
xmodmap -e "keysym slash = slash question $UBKS question"
