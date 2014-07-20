# Fix C-i in urxvt?
# shell-word movement/deletes


# The following lines were added by compinstall

zstyle ':completion:*' completer _expand _complete _ignored _match _correct _approximate _prefix
zstyle ':completion:*' match-original both
zstyle ':completion:*' max-errors 2
zstyle :compinstall filename '/home/david/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Enable stored history, and make it large, don't store repeats
HISTSIZE=100000
SAVEHIST=$HISTSIZE
HISTFILE=~/.zsh_history
setopt histignoredups

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

# In `make` set default number of jobs to run. Apparently make runs fastest
# with one more job than there are cores.
NJOBS=$(($NCORES + 1))
export MAKEFLAGS="-j$NJOBS"


# Stupid netgen!! Needs to have it's dir set for it
export NETGENDIR="/usr/share/netgen/"


# Key binds 
# ============================================================

# Don't use the flow control bindings for C-q and C-s (stop/resume output,
# for use with ancient typewriter style outputs).
setopt noflowcontrol

# movement
bindkey "\C-n" emacs-backward-word
bindkey "\C-o" emacs-forward-word
bindkey "\en" backward-char
bindkey "\eo" forward-char
bindkey "\C-b" beginning-of-line
bindkey "\C-l" end-of-line

# bind vi-find-next-char vi-first-non-blank ??ds

# history
bindkey "\ei" up-line-or-history
bindkey "\ee" down-line-or-history
bindkey "\e." insert-last-word # use numeric arguments (e.g. M-2) to get
                                # 2nd to last etc.


# delete words and characters
bindkey "\C-y" backward-kill-word
bindkey "\C-d" kill-word
bindkey "\ed" delete-char
bindkey "\ey" backward-delete-char


# # shellword movement/deletes
# my_shell_wordchars=""
# backward-kill-shellword ()
# {
#   echo "hi"
#   local WORDCHARS="$my_shell_wordchars"
#   zle backward-kill-word
# }
# zle -N backward-kill-shellword
# bindkey "\C-\ey" backward-kill-shellword

# delete lines
bindkey "\C-x" kill-line
bindkey "\C-X" backward-kill-line
bindkey "\ex" kill-whole-line

# paste
bindkey "\C-v" yank
bindkey "\ev" yank-pop

# misc emacs-like things
bindkey "\C-q" quoted-insert
bindkey "\C-u" universal-argument
bindkey "\et" transpose-words

# misc shell things
bindkey "\C-\"" quote-line
bindkey "\C-j" accept-line
bindkey "\e[11~" run-help # f1 key

# change case
bindkey "\e'" capitalize-word
bindkey "\el" down-case-word
bindkey "\eu" up-case-word


# Fancy prompt with git stuff
# ============================================================

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# # Set the prompt (notice the space between use name and location, for
# # easy cut+paste). Mostly from Ubuntu defaults..
# PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]: \[\033[01;34m\]\w\[\033[00m\]'

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


# # Append git branch followed by newline and $ to prompt. Note that we HAVE to
# # use single quotes for the __git_ps1 part. Stuff in \[ \] is colour commands.
# PS1="$PS1"'\[\033[1;36m\]$(gitbranch " (%s)")\[\033[0m\] \$\n'

PROMPT='%B%F{green}%n@%M%f: %F{blue}%~%f %F{} $(gitbranch) %f %F{red}%? %F{default}$%b
'


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



# Use better top with colours and stuff:
alias top='htop'

# Use hub for better github integration if we have it and it's executable.
if [[ -x "/usr/local/bin/hub" ]]; then
    alias git='hub'
fi

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
alias ai='sudo apt-get install'
alias ar="sudo apt-get remove"
alias update='sudo apt-get update && sudo apt-get upgrade --assume-yes --quiet'
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

# Get a sorted list of disk usage (take from http://www.commandlinefu.com/commands/view/4786/nice-disk-usage-sorted-by-size-see-description-for-full-command )
sdu()
{
du -sk $1 | sort -nr | awk 'BEGIN{ pref[1]="K"; pref[2]="M"; pref[3]="G";} { total = total + $1; x = $1; y = 1; while( x > 1024 ) { x = (x + 1023)/1024; y++; } printf("%g%s\t%s\n",int(x*10)/10,pref[y],$2); } END { y = 1; while( total > 1024 ) { total = (total + 1023)/1024; y++; } printf("Total: %g%s\n",int(total*10)/10,pref[y]); }'
}

# Find all "code" files recursively
fcode()
{
    # Could probably do this with a fancy regex but this is easier
    cat <(find '$@' -name '*.cc' ) <(find '$@' -name '*.h') \
        <(find '$@' -name '*.cpp') <(find '$@' -name '*.c') \
        <(find '$@' -name '*.py') <(find '$@' -name '*.sh') \
        <(find '$@' -name '*.el')
}


# Fancy grep: with line num, with filename, exclude source control, binaries and make junk
mygrep ()
{
    grep  -n -H -I --exclude-dir=.git --exclude-dir=.svn \
        --exclude-dir='*.deps' --exclude='*.lo' --exclude='*.la' --exclude='*.lai' \
        --exclude=Makefile --exclude=Makefile.in --exclude=TAGS --color=auto $@
}

# grep source code files only
gcode ()
{
    # -u prevents grouping of output, so that --color=auto works correctly
    fcode | parallel -u mygrep $@
}


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

# Build thesis tex file
alias tb='cd ~/Dropbox/phd/reports/ongoing-writeup/ && ./build.sh'

# Set up pedals
alias pedals='sudo /lib/udev/keymap -i input/event2 /lib/udev/keymaps/microdia'

# Quickly cd to useful directorys:
alias om='cd $OOMPHMM'
alias hs='cd ~/Dropbox/programming/helperscripts'
alias wr='cd ~/Dropbox/phd/reports/ongoing-writeup'
# alias sr='cd ~/Dropbox/phd/talks/second_year_progression'
# alias rs='cd ~/Dropbox/phd/results'
# alias sicp='cd ~/programming/sicp/exercises4'
alias rc='cd ~/Dropbox/linux_setup/rcfiles'
alias wb='cd ~/Dropbox/web/blog'
alias mmm='cd ~/Dropbox/phd/posters/2013_MMM_Denver/poster'
alias css='cd ~/Dropbox/phd/talks/2013_cs_symposium'
alias sp='cd ~/programming/simpleode/'
alias spe='cd ~/programming/simpleode/experiments'
alias demon='cd ~/Dropbox/phd/demonstrations/maths_for_cs'
alias illg="cd ~/oomph-lib/user_drivers/micromagnetics/control_scripts/llg_driver"
alias sllg="cd ~/oomph-lib/user_drivers/micromagnetics/control_scripts/semi_implicit_mm_driver"
alias oode="cd ~/oomph-lib/user_drivers/micromagnetics/control_scripts/ode_driver"
alias mmr='cd ~/Dropbox/phd/talks/RGM-21-11-2013_mmm_review'


# Cd to currently used dirs
function now ()
{
    cd ~/oomph-lib/user_drivers/micromagnetics/
}

# Aliases for cds upwards
alias ....='cd ../../..'
alias ...='cd ../..'
alias ..='cd ../'


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


pdfman()
{
    man -t "$@" | ps2pdf - - | pipe-evince
}

# git diff of pdfs
git-pdf-diff()
{
    cat "$1" | pipe-evince &
    git show "HEAD:$1"  | pipe-evince
}

# Quickly cd to useful directorys:
alias om='cd $OOMPHMM'
alias hs='cd ~/Dropbox/programming/helperscripts'
alias wr='cd ~/Dropbox/phd/reports/ongoing-writeup'
# alias sr='cd ~/Dropbox/phd/talks/second_year_progression'
# alias rs='cd ~/Dropbox/phd/results'
# alias sicp='cd ~/programming/sicp/exercises4'
alias rc='cd ~/Dropbox/linux_setup/rcfiles'
alias wb='cd ~/Dropbox/web/blog'
alias mmm='cd ~/Dropbox/phd/posters/2013_MMM_Denver/poster'
alias css='cd ~/Dropbox/phd/talks/2013_cs_symposium'
alias sp='cd ~/programming/simpleode/'
alias spe='cd ~/programming/simpleode/experiments'
alias demon='cd ~/Dropbox/phd/demonstrations/maths_for_cs'
alias illg="cd ~/oomph-lib/user_drivers/micromagnetics/control_scripts/llg_driver"
alias sllg="cd ~/oomph-lib/user_drivers/micromagnetics/control_scripts/semi_implicit_mm_driver"
alias oode="cd ~/oomph-lib/user_drivers/micromagnetics/control_scripts/ode_driver"
alias mmr='cd ~/Dropbox/phd/talks/RGM-21-11-2013_mmm_review'


# Prepend a line to a file using ed
prepend ()
{
    echo "0a
$1
.
w" | ed "$2"
}

# oomph-lib 
# ============================================================

OOMPH="$HOME/oomph-lib"
OOMPHMM="$OOMPH/user_drivers/micromagnetics"
OOMPHMMDRIVER="$OOMPHMM/control_scripts/driver"
OPTOOMPH="$HOME/optoomph"
OPTOOMPHMM="$OPTOOMPH/user_drivers/micromagnetics"

alias oomphctags="ctags -e --extra=+q --recurse $OOMPH/src/generic $OOMPH/src/meshes $OOMPH/user_drivers"

alias optctags="ctags -e --extra=+q --recurse $OPTOOMPH/src/generic $OPTOOMPH/src/meshes $OPTOOMPH/user_drivers"


alias quickautogen="$OOMPH/autogen.sh -d $OOMPH -r -s -c ${HOME}/Dropbox/phd/oomph-lib/oomph-lib-compile-options-debug && oomphctags"
alias optquickautogen="$OPTOOMPH/autogen.sh -d $OPTOOMPH -r -s -c ${HOME}/Dropbox/phd/oomph-lib/oomph-lib-compile-options-optimise && optctags"

alias quickcheck="python3 $OOMPH/bin/parallel_self_test.py -C $OOMPH"
alias optquickcheck="python3 $OPTOOMPH/bin/parallel_self_test.py -C $OPTOOMPH"

alias micromagcheck="m -C $OOMPHMM && m -C $OOMPHMM install \
&&  m -C $OOMPHMM/control_scripts/driver \
&& python3 $HOME/oomph-lib/bin/parallel_self_test.py -C $OOMPHMM"
alias optmicromagcheck="m -C $OPTOOMPHMM && m -C $OPTOOMPHMM install \
&&  m -C $OPTOOMPHMM/control_scripts/driver \
&& python3 $HOME/oomph-lib/bin/parallel_self_test.py -C $OPTOOMPHMM"

alias oopt="echo -e \"$OOMPH/config/configure_options/current contains:\n\n\"; cat $OOMPH/config/configure_options/current"

alias mm="m LIBTOOLFLAGS=--silent -C $OOMPHMM && m LIBTOOLFLAGS=--silent -C $OOMPHMM install && m LIBTOOLFLAGS=--silent -C $OOMPHMM/control_scripts/driver"

alias mdr="cd $OOMPHMM/control_scripts/driver"
alias optmdr="cd $OPTOOMPHMM/control_scripts/driver"


alias optmicromagcheck="m -C $OPTOOMPHMM && m -C $OPTOOMPHMM install &&  m -C $OPTOOMPHMM/control_scripts/driver && python3 $HOME/oomph-lib/bin/parallel_self_test.py -C $OPTOOMPHMM"

optmm()
{
    m -C $OPTOOMPHMM $@ \
        && m -C $OPTOOMPHMM install $@ \
        && m -C $OPTOOMPHMM/control_scripts/driver $@
}

optnow()
{
    cd $OPTOOMPHMM/control_scripts
}

optsyncoomph()
{
    cd $OOMPH \
        && git push --force \
        && cd $OPTOOMPH \
        && git fetch --all \
        && git reset --hard origin/working \
        && optquickautogen
}

optsync()
{
    cd $OOMPHMM \
        && git push --force \
        && optnow \
        && git fetch --all \
        && git reset --hard origin/master \
        && optmicromagcheck
}

mccat ()
{
    cat $1/Validation/make_check_output
    cat $1/Validation/validation.log
    cat $1/Validation/run_script
    cat $1/Validation/stdout
}

full_test()
{
    # Arguments are passed to quickautogen and used to label files

    mkdir test_results

    oomphfile="test_results/oomph_tests_$@"
    oomphfile2=$(echo $oomphfile | sed 's/ /_/g')

    mmfile="test_results/mm_tests_$@"
    mmfile2=$(echo $mmfile | sed 's/ /_/g')

    buildfile="test_results/build_trace_$@"
    buildfile2=$(echo $buildfile | sed 's/ /_/g')

    quickautogen $@ 2>&1 | tee $buildfile2 && \
        quickcheck --no-colour | tee $oomphfile2 && \
        micromagcheck | tee $mmfile2
}

# Run all tests with debug, mpi and mpi+opt settings (assuming we are
# starting in a debug build).
alias oomphtestall="cd ~/oomph-lib && touch test_results && mv test_results test_results.old && full_test -d && full_test -am && full_test -an"

pvdat ()
{
    filename=$1
    shift
    oomph-convert $filename
    paraview ${filename%.dat}.vtu "$@"
}
export pvdat

pv-most-recent ()
{
    dirname=$1
    shift
    maxsoln=$(find $dirname -name 'soln*.dat' | sort -V | tail -n1)
    pvdat ${maxsoln} "$@"
}

alias parse="parse.py"

# Run oomph-lib micromagnetics parse over ssh and view pdfs locally
ssh-parse ()
{
    # run and save on simulations machine
    temp_sims=$(ssh david-simulations 'mktemp "ssh-parse-$USER.XXXXXXX" --tmpdir -d')
    ssh david-simulations "parse.py $@ --ssh-mode --save-to-dir \"$temp_sims\""

    # bring to local machine
    temp_local=$(mktemp "ssh-parse-local-$USER.XXXXXXX" --tmpdir -d)
    scp "david-simulations:$temp_sims/*" "$temp_local"

    # view
    evince "$temp_local/*"

    # temp files are cleared by machines automatically (on boot in
    # Ubuntu).
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

alias emacstest='\emacs --debug-init --batch -u $USER'


# PATH additions
# ============================================================
# Add my scripts to PATH
export PATH="$PATH:$HOME/Dropbox/programming/helperscripts/gnuplot:$HOME/Dropbox/programming/helperscripts/oomph-lib:$HOME/Dropbox/programming/helperscripts:$HOME/Dropbox/programming/helperscripts/python"
PATH="$PATH:$HOME/Dropbox/programming/oomph-scripts"
PATH="$PATH:$HOME/bin"
PATH="$PATH:$HOME/Dropbox/programming/pipe-plot"

# Add oomph-lib bin to path
export PATH="$PATH:$HOME/oomph-lib/bin"

# my oomph scripts
export PATH="$PATH:$HOME/oomph-lib/user_drivers/micromagnetics/control_scripts"

# Paraview
export PATH="$PATH:$HOME/code/paraview/bin"

# Matlab
export PATH="$PATH:$HOME/code/matlab/bin:/usr/local/MATLAB/R2013a/bin"

# If we have an alternative doxygen build
if [[ -d "$HOME/code/doxygen" ]]; then
    export PATH="$PATH:$HOME/code/doxygen/bin"
fi

# arch linux stuff
export PATH="$PATH:$HOME/Dropbox/arch"


# Other micromagnetics packages:
# Add nsim to PATH
if [[ -d $HOME/code/nmag* ]]; then
    export PATH="$PATH:$HOME/code/nmag-0.2.1/bin"
fi

# add magnum.fe to path if we have it
if [[ -d $HOME/code/dorsal_code ]]; then
    # Add FEniCS environment variables
    source $HOME/code/dorsal_code/FEniCS/share/fenics/fenics.conf
    export PYTHONPATH=$PYTHONPATH:$HOME/code/magnum.fe/site-packages
fi

# Add oommf to path
if [[ -d $HOME/code/oommf* ]]; then
    export PATH="$PATH:$HOME/code/oommf"
fi


# python's path as well:
export PYTHONPATH="$PYTHONPATH:$HOME/programming/:$HOME/programming/helperscripts/python/"
export PYTHONPATH="$PYTHONPATH:$HOME/Dropbox/programming"
export PYTHONPATH="$PYTHONPATH:$HOME/oomph-lib/bin/"
export PYTHONPATH="$PYTHONPATH:$HOME/oomph-lib/user_drivers/micromagnetics/etc/"
