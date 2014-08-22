# Fix C-i in urxvt?
# Replace cd aliases with directory vars and cd ~ see: http://zsh.sourceforge.net/Intro/intro_5.html


# How many cores do we have? Find out from /proc/cpuinfo (using regexp matching
# the start of info for a new processor).
NCORES=$(grep --count '^processor[[:space:]]*:' /proc/cpuinfo)

# In `make` set default number of jobs to run. Apparently make runs fastest
# with one more job than there are cores.
NJOBS=$(($NCORES + 1))
export MAKEFLAGS="-j$NJOBS"


# Stupid netgen!! Needs to have it's dir set for it
export NETGENDIR="/usr/share/netgen/"

# Directory for zsh rc files:
rcdir="$HOME/.zsh"


# Load key binds 
source ${rcdir}/zsh-bindings.sh

# Load aliases and simple functions
source ${rcdir}/zsh-aliases.sh

# Load oomph-lib related aliases/functions
source ${rcdir}/zsh-oomph-aliases.sh

# Completion
# ============================================================

zstyle ':completion:*' completer _expand _complete _ignored _match _correct _approximate _prefix
zstyle ':completion:*' match-original both
zstyle ':completion:*' max-errors 2
zstyle :compinstall filename '/home/david/.zshrc'

fpath=(~/.zsh/completion $fpath)
autoload -U ~/.zsh/completion/*(:t)

# Initialise
autoload -Uz compinit
compinit


# General options
# ============================================================

# shut the hell up and rm *
setopt rmstarsilent


# History 
# ============================================================

# Enable stored history, and make it large.
HISTSIZE=100000
SAVEHIST=$HISTSIZE
HISTFILE=~/.zsh_history

# Don't store repeated lines
setopt histignoredups

# Immediately add lines to history file, instead of waiting till end
setopt incappendhistory

# Store additional data about the history: times/dates, runtimes
setopt extendedhistory

# View history data
alias hist='history -f -D -E'

# Remove redundant whitespace from history lines
setopt histreduceblanks



# Fancy prompt with git stuff
# ============================================================

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# Allow for functions in the prompt.
setopt PROMPT_SUBST

# # Set the prompt (notice the space between use name and location, for
# # easy cut+paste). Mostly from Ubuntu defaults..
# PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]: \[\033[01;34m\]\w\[\033[00m\]'

# In git line in prompt: show symbols for non-clean state, untracked files
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES="on"

# Add git prompt function to path:
source "/usr/lib/git-core/git-sh-prompt"

# Create prompt
PROMPT='%B%F{green}%n@%M%f: %F{blue}%~%f$(__git_ps1 " (%s)")%(?/%f/%F{red})$%f%b
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
