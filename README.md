
Requirements:
=======

* emacs
  * yasnippet
  * probably many other packages...

* xmonad (window manager)
  * dmenu
  * yagnesh

* space2ctrl (see https://github.com/r0adrunner/Space2Ctrl)

* urxvt (terminal editor)

Cool things
============

* Xmonad working reasonably well as a replacement for emacs' built in
  window manager:
  * Most things launch new emacs "frames" rather than emacs
    "windows". Except gdb which does it's own thing...
  * Completion is done cyclically and/or shown in the mini-buffer depending
    on number of options. Avoids spawning needless frames.
  
* Pretty colour schemes and transparency in both emacs and urxvt. Turns out
  it's quite fiddly to get all the various bits of emacs to be reasonable
  colours for transparency.
    
* Emacs daemon integrated into xmonad. Actually really easy: a bash
  one-liner can launch either a daemon + a client or just a client
  depending on what is needed. I bound this to a key in xmonad.

* Xmonad working with all the distributions I've tried (only Ubunutu,
  XUbunutu and Mint) in a single config. Uses the EwmhDesktops and a bunch
  of bash || statements to pick which terminal to run.

* Lots of other stuff borrowed from other peoples config files!
