# Fix C-i in urxvt?
# Replace cd aliases with directory vars and cd ~ see: http://zsh.sourceforge.net/Intro/intro_5.html



# Directory for zsh rc files:
zshdir="$HOME/.zsh"


# Load api keys
source "$HOME/.keys/api_keys.sh"


# Completion
# ============================================================

zstyle ':completion:*' completer _expand _complete _ignored _match _correct _approximate _prefix
zstyle ':completion:*' match-original both
zstyle ':completion:*' max-errors 2
zstyle :compinstall filename '/home/david/.zshrc'

# Case-insensitive (all), underscore vs dash insensitive, partial-word, and
# then substring completion. (stolen from the internet, not sure what it
# all does)
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# completion for kill and killall
zstyle ':completion:*:processes-names' command 'ps -e -o comm='
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;32'

# Add custom completions
fpath=(~/.zsh/completion $fpath)


# Add zsh-completions repo
fpath=(~/.zsh/completion/zsh-completions/src $fpath)
autoload ~/.zsh/completion/zsh-completions/src/*(:t)

# Can't get this working with skip_global_compinit=1
# # fix crazy oracle caps for commands in zsh-completions
# compdef _VBoxManage vboxmanage
# compdef _VBoxHeadless vboxheadless

# Add generated python completions
fpath=(~/.zsh/completion/generated $fpath)

autoload -U ~/.zsh/completion/*(:t)


# For some reason pip completion needs to go in here
function _pip3_completion {
    local words cword
    read -Ac words
    read -cn cword
    reply=( $( COMP_WORDS="$words[*]" \
                         COMP_CWORD=$(( cword-1 )) \
                         PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip3_completion pip3


# Initialise
autoload -Uz compinit
compinit


# General options
# ============================================================

# shut the hell up and rm *
setopt rmstarsilent

# Keep a stack of directories
setopt autopushd

unset MANPATH
export MANPATH="$(manpath)"

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
# setopt histreduceblanks

# Don't use: it garbles history!


# Fancy move command
# ============================================================

autoload -U zmv
alias mmv='noglob zmv -W'


# Fancy prompt with git stuff
# ============================================================

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

if [[ -f "/usr/lib/git-core/git-sh-prompt" ]]; then
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
else
    function __git_ps1 () {
        echo ""
    }
fi

if [[ -f "/usr/lib/git-core/git-sh-prompt" ]]; then

    # Create prompt
    PROMPT='%B%F{green}%n@%M%f: %F{blue}%~%f $(__git_ps1 " (%s)")%(?/%f/%F{red})$%f%b
'
else
    PROMPT='%B%F{green}%n@%M%f: %F{blue}%~%f %(?/%f/%F{red})$%f%b
'
fi

# Function to export functions to bash (for use with xargs or parallel)
# ============================================================

# From http://stackoverflow.com/questions/22738425/export-function-from-zsh-to-bash-for-use-in-gnu-parallel
function exportf ()
{
    export $1="$(whence -f $1 | sed -e "s/$1 //")"
    # any variable starting with () is a function in bash. sed strips the
    # leading function name from the function definition given by whence.
}

# For testing:
# function exportf2 ()
# {
#     export $1="$(whence -f $1 | sed -e "s/$1 //")"
#     bash -c "echo from bash; type $1; echo end bash"
# }


# Colour in man pages
# ============================================================
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# Pager
# ============================================================

export PAGER="less"
export LESS="-F -R -X -S"

# ssh
# ============================================================

# Smart ssh which runs ssh-add when needed, doesn't work with git though :(
ssh-add -l >/dev/null || \
    alias ssh='ssh-add -l >/dev/null || ssh-add && unalias ssh; ssh'



# Emacs
# ============================================================
# For some reason this runs an emacs daemon if emacs is not already running and
# you try to run an emacsclient
export EDITOR="emacsclient -c -n"
export ALTERNATE_EDITOR=''

# Use E for sudo edit
alias E="SUDO_EDITOR=\"emacsclient -c\" sudo -e"


# Compiling and testing
# ============================================================

# How many cores do we have? Find out from /proc/cpuinfo (using regexp matching
# the start of info for a new processor).
NCORES=$(grep --count '^processor[[:space:]]*:' /proc/cpuinfo)

# In `make` set default number of jobs to run. Apparently make runs fastest
# with one more job than there are cores.
NJOBS=$(($NCORES + 1))
export MAKEFLAGS="-j$NJOBS"

# and use it for ctest too
export CTEST_PARALLEL_LEVEL="$NJOBS"
export CTEST_OUTPUT_ON_FAILURE="yes"

# # Use GNU gold instead of ld for linking
# export PATH="/usr/lib/gold-ld:${PATH}"


# PATH additions
# ============================================================
# Add my scripts to PATH
PATH="$PATH:$HOME/bin"
export PATH="$PATH:$HOME/Dropbox/bin"

# # Add emacs if it's there
# emacsdir="$HOME/code/emacs/mybin"
# if [[ -d $emacsdir ]]; then
#     export PATH="$emacsdir:$PATH"
# fi

# Add general binaries
export PATH="$HOME/code/bin:$PATH"


# classifier scripts
export PATH="$PATH:$HOME/Dropbox/jobs/awe/r-classifier/bin"


# cask (for emacs)
caskdir="$HOME/.cask/bin"
if [[ -d $caskdir ]]; then
    export PATH="$PATH:$caskdir"
fi

# homebrew
brewdir="$HOME/code/linuxbrew/bin"
if [[ -d "$brewdir" ]]; then
    export PATH="$brewdir:$PATH"
fi
brewdir="/local/david-builds/linuxbrew/bin"
if [[ -d "$brewdir" ]]; then
    export PATH="$brewdir:$PATH"
fi

export PATH="$PATH:$HOME/code/google_appengine"

# Ruby gems
gemdir="$HOME/.gem/ruby/2.1.0/bin"
export PATH="$PATH:$gemdir"


# Cool compiler (for course)
export PATH="$PATH:$HOME/Dropbox/education/compilers/cool/bin"


# Python
# ============================================================

# python's paths
export PYTHONPATH="$PYTHONPATH:$HOME/programming/:$HOME/programming/helperscripts/python/"
export PYTHONPATH="$PYTHONPATH:$HOME/Dropbox/programming"
export PYTHONPATH="$PYTHONPATH:$HOME/workflows/cloudworkflowsimulator/scripts/"
export PYTHONPATH="$PYTHONPATH:$HOME/Dropbox/education"

export PYTHONPATH="$PYTHONPATH:$HOME/code/google_appengine/lib/webapp2-2.5.1"


# Java
# ============================================================

export ANT_ARGS='-emacs -logger org.apache.tools.ant.listener.AnsiColorLogger'

# Tell Java what type of window manager XMonad is so that GUI apps work.
export _JAVA_AWT_WM_NONREPARENTING=1

# Go
# ============================================================

export GOPATH="$HOME/code/go"
export PATH="$PATH:$HOME/code/go/bin"


# npm
# ============================================================

export PATH="$PATH:$HOME/.npm-global/bin"
export MANPATH="$MANPATH:$HOME/.npm-global/share/man"


# Imports
# ============================================================

# Load key binds
source ${zshdir}/zsh-bindings.sh

# Load aliases and simple functions
source ${zshdir}/zsh-aliases.sh

# Load work specific aliases/functions
# source ${zshdir}/zsh-oomph-aliases.sh
source ${zshdir}/zsh-classifier-aliases.sh

# e function
source ${zshdir}/emacs-read-stdin/emacs-read-stdin.sh

source ${zshdir}/biosite.sh
source ${zshdir}/biosite-vms.sh
source ${zshdir}/xenon.sh

if [ -e ~/code/pa-config-files/biosite-vms.bash ]; then
    autoload bashcompinit
    bashcompinit
    source ~/code/pa-config-files/biosite-vms.bash
fi

# pip completion helpers
source ${zshdir}/completion/pip.plugin.zsh


# plugins
source "${zshdir}/plugins/safe-paste.sh"
source "${zshdir}/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"


# ccache config
# ============================================================
export CCACHE_SLOPPINESS="time_macros"


# Postgres
# ============================================================

# Don't use $EDITOR because we always want to wait
export PSQL_EDITOR="emacsclient -c"


# Cows are fun, but not very readable
export ANSIBLE_NOCOWS=1


# Node
# ============================================================
function ndb() {
    export PATH="$(npm bin):$PATH"
    if [ $# -gt 0 ]; then
        target="$1"
        shift
    else
        target="$(npm prefix)/dist/index.js"
    fi

    if ! ps -aux | grep '[c]hromium-browser' -q; then
        nohup chromium-browser &
        echo "browse to chrome://inspect and click open node debugger"
    fi

    node --inspect "$target" "$@"
}
