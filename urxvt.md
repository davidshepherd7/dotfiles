urxvt (terminal emulator)
=====================


Why use it?
------
This is probably the least interesting of the little tools that I've set up for programming so this will be quite short. Basically there are two reasons why I'm using `urxvt` (`rxvt-unicode` is the package name in debian etc.):

* It is configured by a simple text file

    This means when I set up a new system (or, more likely, install a new
    version of linux) all I need to do to get things set up is copy a file into
    `~/`. While terminal settings are not really a big deal (pretty colours
    mostly...) it's much easier than all the clicking through menus needed to
    set up something like `mate-terminal`.

* It works on any linux distribution

    Pretty self explanatory really. If I decide at some point to drop
    gnome-based linux distributions (which I nearly did a few months ago) I
    don't want to have to find new versions of various programs.

* It supports unicode.



I'm sure there are plenty of terminal emulators which fufil these two criteria, however I'd heard (well, read) good things about urxvt so I gave it a try.


The setup
--------

As mentioned above there's not really all that much to configure here. However there are some nasty little gotchas in the file syntax.
