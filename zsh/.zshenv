eval $(keychain --eval --agents ssh -Q --quiet)

# This seems to make zsh startup ~4x faster on my laptop!
skip_global_compinit=1

# Set up path etc here so that it's ready sooner

export PATH="$PATH:/opt/bin"
export PATH="$PATH:$HOME/bin"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"

export PATH="$HOME/.npm-global/bin:$PATH"
export MANPATH="$HOME/.npm-global/share/man:$MANPATH"

export HOMEBREW_PREFIX="/home/linuxbrew/.linuxbrew";
export HOMEBREW_CELLAR="/home/linuxbrew/.linuxbrew/Cellar";
export HOMEBREW_REPOSITORY="/home/linuxbrew/.linuxbrew/Homebrew";
fpath[1,0]="/home/linuxbrew/.linuxbrew/share/zsh/site-functions";
export PATH="/home/linuxbrew/.linuxbrew/bin:/home/linuxbrew/.linuxbrew/sbin:${PATH}";
export INFOPATH="/home/linuxbrew/.linuxbrew/share/info:${INFOPATH:-}";

# For some reason this runs an emacs daemon if emacs is not already running and
# you try to run an emacsclient
export EDITOR="emacsclient -c"
export ALTERNATE_EDITOR=''
