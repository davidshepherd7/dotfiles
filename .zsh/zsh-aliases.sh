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

# tar
alias untar='tar -xzf'

# pdf viewer, disconnect from shell entirely and write stdout/stderr to a
# temp file (temp files are cleaned on reboot).
v ()
{
    viewer="evince"
    tempfile=$(mktemp viewer-$USER.XXXXXXX --tmpdir)
    setsid "$viewer" $@ >"$tempfile" 2>&1 </dev/null &
}
compdef v=evince


# New terminal in this folder
t ()
{
    urxvt &
}

# ??ds emacs with pipe in support


# ??ds better diff
gdiff()
{
    git diff --color=always --no-index $@ | perl /usr/share/doc/git/contrib/diff-highlight/diff-highlight | less -R
}
compdef gdiff=diff

# ??ds ssh with auto ssh-add


# package manager
# ============================================================

alias ai='sudo apt-get install -yq'
alias arm="sudo apt-get remove"
alias acs="apt-cache search"

install_packages ()
{
    package_list="$HOME/Dropbox/linux_setup/rcfiles/package_list"
    cat $package_list | xargs sudo apt-get install -y -q
}

alias update='sudo apt-get update && install_packages && sudo apt-get upgrade --assume-yes --quiet'
alias pm='sudo pacmatic -S'

# Open location in gnome
alias go='nautilus .'


# Git aliases
# ============================================================

alias g='git'

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
alias wr='cd ~/Dropbox/phd/reports/2014_aimr_paper'
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


alias whitespaceclean="sed -i 's/[ \t]*$//'"

# Password generator
alias genpassword='apg -a 1 -m 12 -M SNCL -k'


# Volume controls
alias vol='amixer -D pulse sset Master'
# Use eg. 50% to set to 50% of max, 5%+ or 5%- to increase/decrease


# Get a sorted list of disk usage (take from http://www.commandlinefu.com/commands/view/4786/nice-disk-usage-sorted-by-size-see-description-for-full-command )
sdu()
{
    du -sk $1 | sort -nr | awk 'BEGIN{ pref[1]="K"; pref[2]="M"; pref[3]="G";} { total = total + $1; x = $1; y = 1; while( x > 1024 ) { x = (x + 1023)/1024; y++; } printf("%g%s\t%s\n",int(x*10)/10,pref[y],$2); } END { y = 1; while( total > 1024 ) { total = (total + 1023)/1024; y++; } printf("Total: %g%s\n",int(total*10)/10,pref[y]); }'
}

# Find all "code" files recursively
fcode()
{
    # Could probably do this with a fancy regex but this is easier
    cat <(find "$@" -name '*.cc' ) <(find "$@" -name '*.h') \
        <(find "$@" -name '*.cpp') <(find "$@" -name '*.c') \
        <(find "$@" -name '*.py') <(find "$@" -name '*.sh') \
        <(find "$@" -name '*.el') <(find "$@" -name '*.tex')
}


# Fancy grep: with line num, with filename, exclude source control, binaries and make junk
mygrep ()
{
    grep  -n -H -I --exclude-dir=.git --exclude-dir=.svn \
        --exclude-dir='*.deps' --exclude='*.lo' --exclude='*.la' --exclude='*.lai' \
        --exclude=Makefile --exclude=Makefile.in --exclude=TAGS --color=auto $@
}
export mygrep
exportf mygrep

# grep source code files only
gcode ()
{
    # -u prevents grouping of output, so that --color=auto works correctly
    fcode | parallel -u grep -n -H -I --color=auto $@ {}
}

scode ()
{
    fcode | xargs sed $@
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
