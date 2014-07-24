#!/bin/zsh

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

# undo on its normal key!
bindkey "\C-z" undo

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


# shellword movement/deletes
autoload -U select-word-style

backward-kill-bashword () {select-word-style bash; zle backward-kill-word}
zle -N backward-kill-bashword
bindkey "\e^y" backward-kill-bashword

backward-bashword () {select-word-style bash; zle backward-word}
zle -N backward-bashword
bindkey "\e^n" backward-bashword

forward-bashword () {select-word-style bash; zle forward-word}
zle -N forward-bashword
bindkey "\e^o" forward-bashword


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
zle -N prepend-sudo
bindkey "\eq" prepend-sudo
