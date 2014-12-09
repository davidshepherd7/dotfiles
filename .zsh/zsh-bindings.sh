#!/bin/zsh

# Add binding for insert && and go to start?

# Don't use the flow control bindings for C-q and C-s (stop/resume output,
# for use with ancient typewriter style outputs).
setopt noflowcontrol

# Bashword operations
# ============================================================

# shellword movement/deletes
autoload -U select-word-style

backward-kill-bashword () {select-word-style bash; zle backward-kill-word}
zle -N backward-kill-bashword

kill-bashword () {select-word-style bash; zle kill-word}
zle -N kill-bashword

backward-bashword () {select-word-style bash; zle backward-word}
zle -N backward-bashword

forward-bashword () {select-word-style bash; zle forward-word}
zle -N forward-bashword



# Kill to/paste from x selection
# ============================================================

send-kill-to-xclip ()
{
    print -rn "$CUTBUFFER" | xclip -i
}
zle -N send-kill-to-xclip

get-from-xclip ()
{
    CUTBUFFER=$(xclip -o)
}

yank-xclip ()
{
    CUTBUFFER=$(xclip -o)
    zle yank
}
zle -N yank-xclip

# These are buggy, not using it now :(

backward-kill-word-xclip ()
{
    zle backward-kill-word
    send-kill-to-xclip
}
zle -N backward-kill-word-xclip

kill-word-xclip ()
{
    zle kill-word
    send-kill-to-xclip
}
zle -N kill-word-xclip

backward-kill-bashword-xclip ()
{
    backward-kill-bashword
    send-kill-to-xclip
}
zle -N backward-kill-bashword-xclip

kill-bashword-xclip ()
{
    kill-bashword
    send-kill-to-xclip
}
zle -N kill-bashword-xclip

kill-line-xclip ()
{
    zle kill-line
    send-kill-to-xclip
}
zle -N kill-line-xclip

backward-kill-line-xclip ()
{
    zle backward-kill-line
    send-kill-to-xclip
}
zle -N backward-kill-line-xclip

kill-whole-line-xclip ()
{
    zle kill-whole-line
    send-kill-to-xclip
}
zle -N kill-whole-line-xclip


# Bindings 
# ============================================================

forward="e"
back="n"
up="i"
down="h"

# movement
bindkey "\C-${back}" emacs-backward-word
bindkey "\C-${forward}" emacs-forward-word
bindkey "\e^${back}" backward-bashword
bindkey "\e^${forward}" forward-bashword

bindkey "\e${back}" backward-char
bindkey "\e${forward}" forward-char

bindkey "\C-b" beginning-of-line
bindkey "\C-l" end-of-line

# bind vi-find-next-char vi-first-non-blank ??ds

# undo on its normal key!
bindkey "\C-z" undo

# history
bindkey "\e${up}" up-line-or-history
bindkey "\e${down}" down-line-or-history
bindkey "\e." insert-last-word # use numeric arguments (e.g. M-2) to get
                                # 2nd to last etc.


# delete words and characters
bindkey "\C-y" backward-kill-word
bindkey "\C-d" kill-word

bindkey "\ed" delete-char
bindkey "\ey" backward-delete-char

bindkey "\e^y" backward-kill-bashword
bindkey "\e^d" kill-bashword


# delete lines
bindkey "\C-x" kill-line
# bindkey "\C-X" backward-kill-line # doesn't work, overwrites \C-x
bindkey "\ex" kill-whole-line

# paste
bindkey "\C-v" yank
bindkey "\ev" yank-pop
bindkey "\ec" send-kill-to-xclip

# misc emacs-like things
bindkey "\C-q" quoted-insert
bindkey "\C-u" universal-argument
bindkey "\et" transpose-words

# misc shell things
bindkey "\C-p" quote-line
bindkey "\C-j" accept-line
bindkey "\e[11~" run-help # f1 key

# change case
bindkey "\e'" capitalize-word
bindkey "\el" down-case-word
bindkey "\eu" up-case-word

# Alt-q inserts "sudo " at the start of line
function prepend-sudo {
  if [[ $BUFFER != "sudo "* ]]; then
    BUFFER="sudo $BUFFER"; CURSOR+=5
  fi
}
zle -N prepend-sudo
bindkey "\eq" prepend-sudo
