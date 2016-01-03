#! /bin/zsh

bgulp()
{
    # subshell to avoid changing directory
    (
        cd ~/code/boron-unstable/boron/web_applications/;
        gulp --silent "$@"
    )
}

alias lbiosite='/home/david/code/boron-unstable/scripts/launch-biosite-single-terminal.sh'
