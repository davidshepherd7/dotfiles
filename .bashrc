
# Start off with the Ubuntu defaults:

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# # If not running interactively, don't do anything
# [ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
HISTFILESIZE=200000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        # We have color support; assume it's compliant with Ecma-48
        # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
        # a case would tend to support setf rather than setaf.)
        color_prompt=yes
    else
        color_prompt=
    fi
fi

# Set the prompt (notice the space between use name and location, for
# easy cut+paste).
if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]: \[\033[01;34m\]\w\[\033[00m\]'
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w'
fi
unset color_prompt force_color_prompt

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

######################################################################
# EVERYTHING AFTER THIS ADDED BY ME
######################################################################

# Colour codes:
# Black       0;30     Dark Gray     1;30
# Blue        0;34     Light Blue    1;34
# Green       0;32     Light Green   1;32
# Cyan        0;36     Light Cyan    1;36
# Red         0;31     Light Red     1;31
# Purple      0;35     Light Purple  1;35
# Brown       0;33     Yellow        1;33
# Light Gray  0;37     White         1;37

# Tell programs it's an xterm (even if it's not...) so they don't complain.
export TERM='xterm'

# Make sure it's using my readline config
export INPUTRC='$HOME/.inputrc'


# Some general aliases
# ============================================================
# apt-get aliases
alias inst='sudo apt-get install'

# Open location in gnome
alias go='nautilus .'

# Git aliases
alias gs='git status'

# Matlab in a terminal
alias matlab='matlab -nodisplay -nodesktop -nojvm'

# Use less instead of more always
alias more='less'

# Move thing to trash
alias trsh='trash-put'

# Fancy grep: exclude source control and binaries
alias dg='grep -n -H --color=auto --exclude-dir=.git --exclude-dir=.svn -I --exclude-dir=*.deps --exclude=*.lo --exclude=*.la --exclude=*.lai  --exclude=Makefile*'

alias pylab='ipython --pylab'

# Get a sorted list of disk usage (take from http://www.commandlinefu.com/commands/view/4786/nice-disk-usage-sorted-by-size-see-description-for-full-command )
sdu()
{
du -sk ./* | sort -nr | awk 'BEGIN{ pref[1]="K"; pref[2]="M"; pref[3]="G";} { total = total + $1; x = $1; y = 1; while( x > 1024 ) { x = (x + 1023)/1024; y++; } printf("%g%s\t%s\n",int(x*10)/10,pref[y],$2); } END { y = 1; while( total > 1024 ) { total = (total + 1023)/1024; y++; } printf("Total: %g%s\n",int(total*10)/10,pref[y]); }'
}

# Emacs
# ============================================================
# For some reason this runs an emacs daemon if emacs is not already running and
# you try to run an emacsclient
export ALTERNATE_EDITOR=""

# Aliases for using emacs with a daemon, ec just starts a client, emacs starts a new window.
alias ec='emacsclient -n'
alias emacs='emacsclient -c -n'


# cd aliases/changes
# ============================================================
# Quickly cd to useful directorys
export om="$HOME/oomph-lib/user_drivers/micromagnetics"
alias om='cd ~/oomph-lib/user_drivers/micromagnetics'

alias hs='cd ~/Dropbox/programming/helperscripts'
alias wr='cd ~/Dropbox/phd/reports/ongoing-writeup'
alias rs='cd ~/Dropbox/phd/results'
alias sicp='cd ~/programming/sicp/exercises4'

alias sp='cd ~/programming/simplellg/simplellg'
alias spe='cd ~/programming/simplellg/experiments'

alias now='cd ~/oomph-lib/user_drivers/micromagnetics/experiments/spatially_constant_m_length_variations'

# Aliases for cds upwards
alias ....='cd ../../..'
alias ...='cd ../..'
alias ..='cd ../'

# Set cd to correct small spelling mistakes
shopt -s cdspell

# A function to cd then ls
function cs ()
{
    cd $1;
    ls --color=auto;
}


# General PATH additions
# ============================================================
# Add my scripts to PATH
export PATH="$PATH:$HOME/Dropbox/programming/helperscripts/gnuplot:$HOME/Dropbox/programming/helperscripts/oomph-lib:$HOME/Dropbox/programming/helperscripts"
PATH="$PATH:$HOME/Dropbox/programming/oomph-scripts"
PATH="$PATH:$HOME/bin"

# Add oomph-lib bin to path
export PATH="$PATH:$HOME/oomph-lib/bin"

# Paraview
export PATH="$PATH:$HOME/code/paraview/bin"

# Matlab
export PATH="$PATH:$HOME/code/matlab/bin"


# nsim/nmag stuff
# ============================================================
# Add nsim to PATH
export PATH="$PATH:$HOME/code/nmag-0.2.1/bin"

# # Some other variables
# PATH="/home/david/code/nmag/bin:/home/david/code/nmag/lib/mpich2/bin:$PATH"
# PETSC_DIR="/home/david/code/nmag/lib/petsc"
# LD_LIBRARY_PATH="/home/david/code/nmag/lib:/home/david/code/nmag/lib/petsc/linux-gnu-c-opt/lib:/home/david/code/nmag/lib/mpich2/lib:$LD_LIBRARY_PATH"

# Stupid netgen!!
export NETGENDIR="/usr/share/netgen/"


# Colour in man pages
# ============================================================
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'


# Prompt modifications
# ============================================================
# In git line in prompt: show symbols for non-clean state
GIT_PS1_SHOWDIRTYSTATE=1


gitbranch()
{
    if type __git_ps1 &> /dev/null
    then
        __git_ps1
    else
        echo
    fi
}


# Append git branch followed by newline and $ to prompt. Note that we HAVE to
# use single quotes for the __git_ps1 part. Stuff in \[ \] is colour commands.
PS1="$PS1"'\[\033[1;36m\]$(gitbranch " (%s)")\[\033[0m\] \$\n '


# Changes to defaults for "make"
# ============================================================
# How many cores do we have? Find out from /proc/cpuinfo (using regexp matching
# the start of info for a new processor).
NCORES=`grep '^processor[[:space:]]*:' /proc/cpuinfo | wc -l`

# Apparently make runs fastest with one more job than there are cores.
NJOBS=$(($NCORES + 1))
export MAKEFLAGS="-j$NJOBS"


# Python
# ============================================================
# Add my scripts to python path
export PYTHONPATH="$HOME/programming/helperscripts/python:$HOME/programming/matplotlib:$HOME/programming/simplellg/"

alias opython="ipython scipy"

# Oomph-lib
# ============================================================

alias mytests='cd $HOME/oomph-lib/user_drivers/micromagnetics/test_collection && ./run_tests.sh'

# magnum.fe
# ============================================================
# Add FEniCS environment variables
source /home/david/code/dorsal_code/FEniCS/share/fenics/fenics.conf
export PYTHONPATH=$PYTHONPATH:$HOME/code/magnum.fe/site-packages
