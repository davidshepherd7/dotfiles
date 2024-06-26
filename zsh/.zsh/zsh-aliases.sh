#! /bin/zsh

# Use better top with colours and stuff:
# alias top='htop || top'

# Tail -F isn't really tail anymore...c all it rcat(refresh cat)
alias rcat='tail -F -n 100000'

# We can also use tail to get cat with file names
alias fcat='tail -n +1'

# gdb with autorun and --args
alias gdbr='gdb -ex "run" --args'
alias cgdbr='cgdb -ex "run" --args'

# gdb with catch-throw, autorun and --args
alias gdbcr='gdb -ex "catch throw" -ex "run" --args'
alias cgdbcr='cgdb -ex "catch throw" -ex "run" --args'

# valgrind with debugger
# alias cvalgrind='valgrind --track-origins=yes --db-attach=yes --db-command='\''cgdb -nw %f %p'

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
alias ex='atool -x -e' # atool can extract pretty much anything, 7zip can almost
# do this but screws up permissions and takes two runs to
# extract tar.gz.

alias jc='sudo journalctl'
alias sc='sudo systemctl'

alias sl='sl -e'

# Many machines don't know what urxvt is, which makes htop fall over
alias ssh="TERM=xterm ssh"

# pdf viewer, disconnect from shell entirely and write stdout/stderr to a
# temp file (temp files are cleaned on reboot).
v ()
{
    viewer="evince"
    tempfile=$(mktemp viewer-$USER.XXXXXXX --tmpdir)
    setsid "$viewer" $@ >"$tempfile" 2>&1 </dev/null &
}
if [[ -x evince ]]; then
    compdef v=evince
fi

# some default options for feh
alias feh="feh -B black --scale-down -d"

# New terminal in this folder
t ()
{
    urxvt &
}

# xargs with default for -I
# alias x="xargs -I %"
x ()
{
    parallel --no-notice -I % -u "$@"
}

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

dif() {
    diff -u "$@" | cdiff
} 

# zmv magic
alias mmv='noglob zmv -W'

# hg + zmv magic harder
_private-hg-mmv()
{
    zmv -W "$@"
    hg addremove --similarity 60
}
alias hg-mmv='noglob _private-hg-mmv'

# Rename files with .bak appended
bak()
{
    for arg in "$@"; do
        cp -r "${arg%/}" "${arg%/}.bak"
    done
}

unbak()
{
    for arg in "$@"; do
        mv "$arg" "${arg%%.bak}"
    done
}

generate-mac ()  {
    local hexchars="0123456789abcdef"
    echo -n "ff:ff:ff"
    for i in {1..3}; do
        echo -n ":${hexchars:$[RANDOM % 16]:1}${hexchars:$[RANDOM % 16]:1}"
    done
    echo
}

# package manager
# ============================================================

alias ai='sudo apt install -yq'
alias arm="sudo apt remove -yq"
alias acs="apt-cache search"

rcdir="$HOME/Dropbox/linux_setup/rcfiles"

install_packages ()
{
    sudo apt update
    package_list="$rcdir/package_list"
    < $package_list | xargs sudo apt install -y -q --auto-remove
}

install_pipx_packages ()
{
    package_list="$rcdir/pipx_package_list"
    < "$package_list" xargs -n 1 pipx install 
    pipx upgrade-all

    # We need this exact black version for Wave compat, but pipx doesn't allow
    # specifying version constraints
    pipx install black==22.3.0 --force
}


install_gem_packages () {
    < "$rcdir/gem_package_list" x sudo gem install %
}

install_npm_packages () {
    # Update packages
    if ! grep -q 'prefix' ~/.npmrc; then
        echo 'prefix=~/.npm-global' >> ~/.npmrc
    fi
    mkdir -p ~/.npm-global
    < "$rcdir/npm_package_list" x npm install -g %

    sudo ~/.npm-global/bin/n lts
}

install_r_packages() {
    # TODO: make it set up an R directory
    R < "$rcdir/r_packages" --no-save --no-restore
}

update () {
    sudo apt update && sudo apt upgrade --assume-yes --quiet &&
        (
            # Allow failures in subshell
            install_packages
            install_pipx_packages
            install_gem_packages
            install_npm_packages
            # install_r_packages
            recompile_elisp
        )
}


recompile_elisp() {
    emacs -Q --batch -f batch-byte-compile "$HOME/.emacs.d/init.el" "$HOME/.emacs.d/lisp/"*.el
}

alias pm='sudo pacmatic -S'



# Version control aliases
# ============================================================

# Pick correct version control system
function h() {
    if hg root 1>/dev/null 2>&1; then
        hg "$@"
    elif git rev-parse --is-inside-work-tree 1>/dev/null 2>&1; then
        git "$@"
    else
        echo "No VC found" 1>&2
        return 1
    fi
}

alias hd="hg"
alias hglg="hg lg"

alias hs="h status"

alias hc="h commit"
alias hca="h commit --amend"

alias hra="hg revert --all"
alias hgplain="HGPLAIN=1 hg"

# Use hub for better github integration, if it exists
if [ command -v git-hub > /dev/null 2>&1 ]; then
    alias hub='git-hub'

    # Use completions for hub tool
    compdef _hub git-hub
fi


alias gs='git status -s -b'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl1='git log -n1 -p'
alias gc='git cherry-pick'
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


# Store/apply/revert testing patches for hg (e.g. $timeout stuff)
HG_TEST_DIFF="$HOME/scratch/hg-test-diff.diff"
reverse-patch() {
    local temp="$(mktemp reverse-patch-$USER.XXXXXXX --tmpdir)"
    cat - > "$temp"
    interdiff "$temp" /dev/null
}

hg-apply-test() {
    (
        cd "$(hg root)"
        patch -p1 <"$HG_TEST_DIFF"
    )
}

hg-unapply-test() {
    (
        cd "$(hg root)"
        <"$HG_TEST_DIFF" reverse-patch | patch -p1
    )
}

hg-set-test() {
    mkdir -p "$(dirname "$HG_TEST_DIFF")"
    hg diff > "$HG_TEST_DIFF"
}

hg-rename-function() {
    local old="$1"
    local new="$2"

    # Replace everywhere
    ag -l "${old}" | xargs sed -i "s/${old}/${new}/g" --follow-symlinks

    # And uppercase too (for include guards)
    local old_upper="$(echo $old | awk '{print toupper($0)}')"
    local new_upper="$(echo $new | awk '{print toupper($0)}')"
    ag -l "${old_upper}" | xargs sed -i "s/${old_upper}/${new_upper}/g" --follow-symlinks

    # Move the files
    hg-mmv "$old".* "$new".*
}


# ===============================

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
alias fcmake="find \( -name 'CMakeLists.txt' -o -name '*.cmake' \) -type f"

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
alias .....='cd ../../../../'
alias ....='cd ../../..'
alias ...='cd ../..'
alias ..='cd ../'

# Alias: cd then ls
c() {
    cd "$1" && ls "${@:2}"
}


# test that emacs works with this config
alias emacstest='\emacs --debug-init --batch -u $USER'


alias gdbemacs="cd ~/code/emacs/src && sudo gdb emacs $(ps aux | grep -i 'emac[s]' | awk '{print $2}')"

alias magit="emacsclient -n -e '(magit-status)'"

pdfpages ()
{
    pdftk A=$1 cat A$2 output "${1:r}_pp${2}.pdf"
    echo "${1:r}_pp${2}.pdf"
}


# New feh wallpaper


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


alias urldecode='python3 -c "import sys, urllib.parse as ul; print(ul.unquote(sys.argv[1]))"'
alias urlencode='python3 -c "import sys, urllib.parse as ul; print(ul.quote(sys.argv[1]))"'

alias fix_headset='pulseaudio -k'

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


# Profiling
# ============================================================

perf_flame()
{
    # Using pl scripts from https://github.com/brendangregg/FlameGraph

    set -o nounset

    tempfile="$(mktemp evince-$USER.XXXXXXX --tmpdir)"
    perf script | stackcollapse-perf.pl - | flamegraph.pl - > "$tempfile"
    sensible-browser "$tempfile"
}

# Numberwang
# ============================================================

# Node
# ============================================================

# Run locally installed node modules
alias npm-exec='PATH=$(npm bin):$PATH'
alias lodash-test='PATH=$(npm bin):$PATH node -e "_ = require(\"lodash\");" -i'

alias racket="rlwrap racket"


# Proxying
# ============================================================

compsoc_proxied_firefox()
{
    # Open a socks proxy with ssh
    ssh -D 9999 compsoc -N &

    # Kill it when we are done
    ssh_pid="$!"
    trap "kill $ssh_pid" EXIT

    # Write a config file to connect to that proxy
    tempfile="$(mktemp tsocks-conf-$USER.XXXXXX --tmpdir)"
    cat <<EOF > "$tempfile"
server = 127.0.0.1
server_port = 9999
EOF

    # Run a new firefox with "transparent socks" wrapping all its network calls
    TSOCKS_CONF_FILE="$tempfile" tsocks firefox -no-remote www.whatismyip.com
}



alias whatismyip='curl "icanhazip.com"'
# This seems to be fast
# Alt:
# curl ifconfig.me
# curl ipecho.net/plain
# curl ifconfig.co



alias ansibleinit='mkdir -p tasks/ handlers/ vars/ templates/ && touch tasks/main.yml handlers/main.yml vars/main.yml'


edit-deb()
{
    deb_path="$(readlink -f $1)"

    tempfile="$(mktemp -d editable-dpkg-$USER.XXXXXX --tmpdir)"
    (
        cd "$tempfile"
        ar p "$deb_path" control.tar.gz | tar -xz

        echo "Hit ctrl-D once you are done editing the deb package"
        "$SHELL"

        tar czf control.tar.gz *[!z]
        ar r "$deb_path" control.tar.gz
    )

    # Delete the temp directory because these things can be very big
    rm -rf "$tempfile"
}

alias gpr="gh pr create --base dev --repo wavemm/monorepo --web"

git-clean-branches() {
    git branch --merged| egrep -v "(^\*|prod|dev)" | xargs -r git branch -d 
}

# Clean fetch
#
# Fetch dev but don't do anything to the current branch (even if the current
# branch is dev).
#
# Delete any merged branches
#
# Delete any branch which has been removed from origin
gcf() {
    if [ "$(git branch --show-current)" = "dev" ]; then
        if ! git diff --exit-code --name-only || ! git diff --exit-code --name-only --cached; then
            echo "Unsaved changes, please stash them."
        fi
        git checkout -b david-temp-branch-gcf
    fi
    
    git branch -f dev origin/dev
    git fetch origin dev:dev -u
    git remote prune origin
    git-clean-branches

    if [ "$(git branch --show-current)" = "david-temp-branch-gcf" ]; then
        git checkout dev
        git branch -D -f david-temp-branch-gcf
    fi
}

alias gcfr="gcf && git rebase dev"

# push.default is simple in my config so this is always something sensible
alias gpf="git push -v --force-with-lease origin" 

gn() {
    gcf && git checkout dev -b "david/$1"
}

lingq()
{
    (
        cd ~/code/LingQ-to-Anki
        poetry run ./main.py import --username davidshepherd7 --password "$LINGQ_PASSWORD" --deck French --model 'Basic (and reversed card)' --language fr --mark-known
    )
}

alias zero-pad="rename -v -e 's/\d+/sprintf(\"%05d\",$&)/e'"


# If this doesn't work run ecryptfs-manager and exit first
alias mount-old-fs="sudo ecryptfs-recover-private /media/david/82ff322c-8d44-49bc-9eae-b3f601f598ed/home/.ecryptfs/david/.Private"

agmacs()
{
    ag --line-number --column "$@" | e - --eval "(cd \"$PWD\")" --eval "(ag-mode)"
}

alias history_off="unset HISTFILE"


alias libcst-venv="source ~/.local/libcst-env/bin/activate"
alias libcst-check="bash -c 'source ~/.local/libcst-env/bin/activate && pyre check"

