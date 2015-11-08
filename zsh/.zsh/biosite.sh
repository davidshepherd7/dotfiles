#! /bin/zsh

bgulp()
{
    # subshell to avoid changing directory
    (
        cd ~/code/boron-unstable/boron/web_applications/;
        gulp --silent
    )
}
