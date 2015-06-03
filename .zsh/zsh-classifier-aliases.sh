#! /bin/zsh

# alias sigplot='cut -d, -f 3- | splot -d, -t'

sigplot () {
    cut -d, -f 3- | splot -d, -t "$@"
}
exportf sigplot


# Some ssh + useful command aliases
alias pitop="ssh pi-maths -t 'htop'"
alias pitmux="ssh pi-maths -t 'tmux attach'"

toreport() {
  cp "$@" "$HOME/Dropbox/jobs/awe/reports/final/plots/"
}
