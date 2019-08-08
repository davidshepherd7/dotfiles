
# Start off with the Ubuntu defaults:

# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# # DON'T use this it ruins things for e.g. emacsserver (e.g. PATH variables
# # are wrong)
# # # If not running interactively, don't do anything
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

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

######################################################################
# EVERYTHING AFTER THIS ADDED BY ME
######################################################################

# How many cores do we have? Find out from /proc/cpuinfo (using regexp matching
# the start of info for a new processor).
NCORES=$(grep --count '^processor[[:space:]]*:' /proc/cpuinfo)

# Colour codes:
# Black       0;30     Dark Gray     1;30
# Blue        0;34     Light Blue    1;34
# Green       0;32     Light Green   1;32
# Cyan        0;36     Light Cyan    1;36
# Red         0;31     Light Red     1;31
# Purple      0;35     Light Purple  1;35
# Brown       0;33     Yellow        1;33
# Light Gray  0;37     White         1;37

# # Tell programs it's an xterm (even if it's not...) so they don't complain.
# export TERM='xterm'
# Aparently this is a (very) bad idea, (arch linux wiki)

# Make sure it's using my readline config
export INPUTRC="$HOME/.inputrc"

# Disable stupid behaviour of ctrl-s in term
stty -ixon


# In `make` set default number of jobs to run. Apparently make runs fastest
# with one more job than there are cores.
NJOBS=$(($NCORES + 1))
export MAKEFLAGS="-j$NJOBS"



# Fancy prompt with git stuff
# ============================================================

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# Set the prompt (notice the space between use name and location, for
# easy cut+paste). Mostly from Ubuntu defaults..
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]: \[\033[01;34m\]\w\[\033[00m\]'

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
PS1="[bash] $PS1"'\[\033[1;36m\]$(gitbranch " (%s)")\[\033[0m\] \$\n'



# Colour in man pages
# ============================================================
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'



# Aliases
# ============================================================

# Tail -F isn't really tail anymore...c all it rcat(refresh cat)
alias rcat='tail -F -n 100000'

# gdb with autorun and --args
alias gdbr='gdb -ex "run" --args'

# ls aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Grep
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# package manager
alias ai='sudo apt install'
__apt_install_completion()
{
    # copied out of /usr/share/bash-completion/completions/apt:_apt_get()

    # Get information about completion
    local cur prev words cword
    _init_completion || return

    # Generate list of matching packages
    COMPREPLY=( $( apt-cache --no-generate pkgnames "$cur" \
        2> /dev/null ) )
    return 0
} &&
complete -F __apt_install_completion ai

alias ar="sudo apt remove"

__apt_remove_completion()
{
    # copied out of /usr/share/bash-completion/completions/apt:_apt_get()

    # Get information about completion
    local cur prev words cword
    _init_completion || return

    # Generate list of installed packages (assuming debian based)
    COMPREPLY=( $(_xfunc dpkg _comp_dpkg_installed_packages $cur ) )
    # Note: _xfunc is from /usr/share/bash-completion/bash_completion, it
    # dynamically loads completion functions. This sources the bash
    # completion file "dpkg" then runs the function
    # `_comp_dpkg_installed_packages` from that file.

    return 0
} &&
complete -F __apt_remove_completion ar

alias update='sudo apt update && sudo apt upgrade --assume-yes --quiet'
alias pm='sudo pacmatic -S'

# Open location in gnome
alias go='nautilus .'

# Git aliases
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl1='git log -n1 -p'
alias gc='git cherry-pick'
alias gca='git commit --amend'
alias gc='git commit'
alias gsri='git stash --keep-index && git stash && git rebase --interactive HEAD~20 && git stash pop && git add -p && git commit --amend && git rebase --continue && git stash pop'
alias gap='git add -p'
alias gcp='git checkout -p'
alias grh='git reset HEAD'


alias gss='git stash --keep-index && git stash'
alias gsp='git stash pop'
alias gri='git rebase --interactive HEAD~20'
alias grc='git rebase --continue'
alias glp='git log -p'


# svn aliases
alias sst='svn status -q'
alias sd='svn diff'

# A make alias with completion. make completions are dynamically loaded:
# load them so that we can apply them to our m command then copy the make
# completion options.
m()
{
    make --keep-going --silent LIBTOOLFLAGS=--silent $@
}
_completion_loader make
$(complete -p make) m


# Matlab in a terminal
alias matlab='matlab -nodesktop -nosplash'

# Maple in a terminal
alias maple="~/code/maple17/bin/maple"

# Move thing to trash
alias trsh='trash-put'

# Simplified find comamnds
function fname() { find . -iname "*$@*"; }
alias findc="find \( -name '*.cc' -o -name '*.h' -o -name '*.cpp' -o -name '*.hpp' \)"

# python
alias pylab='ipython --pylab'
alias nosetests="nosetests --processes="$NCORES

# Find all "code" files recursively
fcode()
{
    # Could probably do this with a fancy regex but this is easier
    cat <(find "$@" -name "*.cc" ) <(find "$@" -name "*.h") \
        <(find "$@" -name "*.cpp") <(find "$@" -name "*.c") \
        <(find "$@" -name "*.py") <(find "$@" -name "*.sh") \
        <(find "$@" -name "*.el")
}


# Fancy grep: with line num, with filename, exclude source control, binaries and make junk
mygrep ()
{
    grep  -n -H -I --exclude-dir=.git --exclude-dir=.svn \
        --exclude-dir=*.deps --exclude=*.lo --exclude=*.la --exclude=*.lai \
        --exclude=Makefile --exclude=Makefile.in --exclude=TAGS --color=auto $@
}
export -f mygrep # export so that parallel can find it
$(complete -p grep) mygrep # add completion

# grep source code files only
gcode ()
{
    # -u prevents grouping of output, so that --color=auto works correctly
    fcode | parallel -u mygrep $@
}
$(complete -p grep) gcode # add completions from grep


# Count lines of actual code recursively
lccode()
{
    # Find code files, grep out comments/blank lines then wc lines
    fcode | xargs grep -v "//\|^[ \t]*$\|#" | wc -l
}


# Edit command scripts. Autocomplete using command names available
we()
{
    $EDITOR $(type -p "$1")
}
complete -c we # add completion of commands

# Set up pedals
alias pedals='sudo /lib/udev/keymap -i input/event2 /lib/udev/keymaps/microdia'

# Cd to currently used dirs
function now ()
{
    cd ~/oomph-lib/user_drivers/micromagnetics/
}

# Aliases for cds upwards
alias ....='cd ../../..'
alias ...='cd ../..'
alias ..='cd ../'


# Set cd to correct small spelling mistakes
shopt -s cdspell

# Set to cd if we just put a directory, no command
shopt -s autocd

# A function to cd then ls
function cs ()
{
    cd $1;
    ls --color=auto;
}

pipe-evince()
{
    # Safely generate a temp file
    TMPFILE=$(mktemp evince-$USER.XXXXXXX --tmpdir)

    # Read from stdin into tempfile
    cat <&0 > $TMPFILE

    # Open in evince
    evince "$TMPFILE"

    # Kill temp file (or comment to let the OS kill it on reboot)
    # bash -c "sleep 2; rm '$TMPFILE'" &
}


# git diff of pdfs
git-pdf-diff()
{
    cat "$1" | pipe-evince &
    git show "HEAD:$1"  | pipe-evince
}





# Emacs
# ============================================================
# For some reason this runs an emacs daemon if emacs is not already running and
# you try to run an emacsclient
export EDITOR="emacsclient -c -n"
export ALTERNATE_EDITOR=""

# Aliases for using emacs with a daemon, ec just starts a client, emacs starts a new window.
alias ec='emacsclient -n'
alias emacs='emacsclient -c -n'
alias e='emacsclient -c -n'



# PATH additions
# ============================================================
# Add my scripts to PATH
PATH="$PATH:$HOME/bin"
