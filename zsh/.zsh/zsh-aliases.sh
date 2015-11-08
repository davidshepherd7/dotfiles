#! /bin/zsh

# Use better top with colours and stuff:
alias top='htop || top'

# Tail -F isn't really tail anymore...c all it rcat(refresh cat)
alias rcat='tail -F -n 100000'

# We can also use tail to get cat with file names
alias fcat='tail -n +1'

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

# archives
alias ex='atool -x -e' # atool can extract pretty much anything, 7zip can
                       # almost do this but screws up permissions and takes
                       # two runs to extract tar.gz.

# pdf viewer, disconnect from shell entirely and write stdout/stderr to a
# temp file (temp files are cleaned on reboot).
v ()
{
    viewer="evince"
    tempfile=$(mktemp viewer-$USER.XXXXXXX --tmpdir)
    setsid "$viewer" $@ >"$tempfile" 2>&1 </dev/null &
}
compdef v=evince

# some default options for feh
alias feh="feh -B black --scale-down -d"

# New terminal in this folder
t ()
{
    urxvt &
}

# xargs with default for -I
alias x="xargs -I %"

# ??ds emacs with pipe in support

ismyinternetdown ()
{
    ping 'www.google.com'
}


# ??ds better diff
gdiff()
{
    git diff --color=always --no-index $@ | perl /usr/share/doc/git/contrib/diff-highlight/diff-highlight | less -R
}
compdef gdiff=diff


# package manager
# ============================================================

alias ai='sudo apt-get install -yq'
alias arm="sudo apt-get remove -yq"
alias acs="apt-cache search"

rcdir="$HOME/Dropbox/linux_setup/rcfiles"

install_packages ()
{
    sudo apt-get update
    package_list="$rcdir/package_list"
    < $package_list | xargs sudo apt-get install -y -q
}

install_pip_packages ()
{
    package_list="$rcdir/pip_package_list"
    < $package_list | xargs sudo pip3 install --upgrade
}

install_brew_packages () {
    brew update
    < "$rcdir/brew_package_list" x brew install %
}

install_gem_packages () {
    <  "$rcdir/gem_package_list" x sudo gem install %
}

update () {
    sudo apt-get update
    sudo apt-get upgrade --assume-yes --quiet

    install_packages
    install_pip_packages
    install_gem_packages
    install_brew_packages
}

alias pm='sudo pacmatic -S'

# Open location in gnome
alias go='nautilus .'


# Git aliases
# ============================================================


# Use hub for better github integration, if it exists
if [ command -v hub > /dev/null 2>&1 ]; then
    alias git='hub'
fi

alias g='git'

alias gs='git status -s -b'
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
alias gau='git add -u :/'

alias gss='git stash --keep-index && git stash'
alias gsk='git stash --keep-index'
alias gsp='git stash pop'
alias gri='git rebase --interactive HEAD~20'
alias grc='git rebase --continue'
alias glp='git log -p'

alias gitlsuntracked='git ls-files --exclude-standard -o'

git-ignore-rest () {
    touch .gitignore
    git add .gitignore
    git ls-files --exclude-standard -o >> .gitignore
    git add .gitignore
}


# svn aliases
alias sst='svn status -q'
alias sd='svn diff'

# A make alias
m()
{
    make --keep-going --silent LIBTOOLFLAGS=--silent $@
}
compdef m=make

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

alias pyprofile='python3 -m cProfile -o pyprofile.temp'
pyprofilestats()
{
    if [ $# -ge 1 ]; then
        nstats="$1"
    else
        nstats="10"
    fi

    if [ $# -ge 2 ]; then
        sort="$2"
    else
        sort="cumtime"
    fi

    if [ $# -ge 3 ]; then
        filename="$3"
    else
        filename="pyprofile.temp"
    fi

    python3 -c "import pstats; p=pstats.Stats('$filename'); p.strip_dirs().sort_stats('$sort').print_stats($nstats)"
}

# Edit command scripts. ??ds fix completion?
we()
{
    $EDITOR $(type -p "$1")
}

# Build thesis tex file
alias tb='cd ~/Dropbox/phd/reports/ongoing-writeup/ && ./build.sh'

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
        <(find "$@" -name '*.el') <(find "$@" -name '*.tex') \
        <(find "$@" -name '*.java') <(find "$@" -name '*.rb')
}


# Fancy recursive grep: with line num, filename, ignore files ignored by
# git.
s()
{
    if ! git rev-parse > /dev/null 2>&1; then
        additional_opt="--no-index --exclude-standard"
    fi
    git grep --color=auto -n -H -I $(echo "$additional_opt") -E $@
}
export s
exportf s

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

take-a-break ()
{
    if [ $# -lt 1 ]; then
        length="60"
    elif [ $# -gt 1 ]; then
        echo "usage mtimer [time to wait in minutes]"
    else
        length="$1"
    fi

    echo "Take a break in $length minutes at $(date --date="+$length minutes" +%T)."

    sleep "${length}m"
    gnome-screensaver-command -l
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

# Repeatedly run an experiment
run() {
    number=$1
    shift
    for i in `seq $number`; do
        $@
    done
}


# Completion generation
# ============================================================

gen_comp_dir="$HOME/.zsh/completion/generated"

# Generate a completion file for a python script which uses argparse
generate_completion ()
{
    set -o nounset
    echo "generating completion for $1"
    "$1" --help | genzshcomp > "${gen_comp_dir}/_$(basename $1)"
}

# Generate completions for all my python scripts
regenerate_all_completions ()
{
    set -o nounset

    touch "${gen_comp_dir}/.temp"
    rm "${gen_comp_dir}/"*
    # # oomph-lib
    # generate_completion "$OOMPHMM/control_scripts/parse.py"
    # generate_completion "$OOMPHMM/control_scripts/parameter-sweep.py"

    # R classifier
    generate_completion "split.py"
    generate_completion "run-classifier.py"
    generate_completion "parameter-sweep.py"
    generate_completion "accuracy.py"
    generate_completion "process-data.py"

    # splot
    generate_completion "splot"
}


# Games
# ============================================================
alias hlands="gargoyle-free $HOME/Dropbox/other/HadeanLands-generic/HadeanLands.gblorb"
