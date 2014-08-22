#! /bin/zsh

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
alias arm="sudo apt-get remove"
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

alias gitlsuntracked='git ls-files --exclude-standard -o'


# svn aliases
alias sst='svn status -q'
alias sd='svn diff'

# A make alias
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

# Edit command scripts. ??ds fix completion?
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



# Aliases for using emacs with a daemon, ec just starts a client, emacs starts a new window.
alias ec='emacsclient -n'
alias emacs='emacsclient -c -n'
alias e='emacsclient -c -n'

# test that emacs works with this config
alias emacstest='\emacs --debug-init --batch -u $USER'

pdfpages ()
{
    pdftk A=$1 cat A$2 output "${1:r}_pp${2}.pdf"
}


# New feh wallpaper
alias wallpaper='feh --bg-scale $HOME/Dropbox/other/wallpapers -zr'
