#!/bin/zsh

# load aliases, locations etc.
source ./zsh-oomph-aliases.sh


# Add custom command completion ??ds this probably shouldn't go here, not sure how fast it is
generate_completion ()
{
    $1 --help | genzshcomp > ~/.zsh/completion/_$(basename $1)
} 

# oomph-lib commands
generate_completion "$OOMPHMM/control_scripts/parse.py"
generate_completion "$OOMPHMM/control_scripts/parameter-sweep.py"


