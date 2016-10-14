#!/bin/zsh

OOMPH="$HOME/oomph-lib"
OOMPHMM="$OOMPH/user_drivers/micromagnetics"
OOMPHMMDRIVER="$OOMPHMM/control_scripts/driver"
OPTOOMPH="$HOME/optoomph"
OPTOOMPHMM="$OPTOOMPH/user_drivers/micromagnetics"


export PATH="$PATH:$OPTOOMPH/bin"
export PYTHONPATH="$PYTHONPATH:$OPTOOMPH/bin/"
export PYTHONPATH="$PYTHONPATH:$OPTOOMPH/user_drivers/micromagnetics/etc/"

so()
{
    if [[ "$(pwd)" == "$OOMPH"* ]]; then
        cd $OOMPH $OPTOOMPH
    elif [[ "$(pwd)" == "$OPTOOMPH"* ]]; then
        cd $OPTOOMPH $OOMPH
    else
        echo "Not in an oomph dir" 1>&2
    fi
}


alias oomphctags="ctags -e --extra=+q --recurse $OOMPH/src/generic $OOMPH/src/meshes $OOMPH/user_drivers"


alias quickautogen="$OOMPH/autogen.sh -s -k -c ${HOME}/Dropbox/phd/oomph-lib/oomph-lib-compile-options-debug && oomphctags"
alias optquickautogen="$OPTOOMPH/autogen.sh -s -k -c ${HOME}/Dropbox/phd/oomph-lib/oomph-lib-compile-options-optimise"

alias quickcheck="python3 $OOMPH/bin/parallel_self_test.py -C $OOMPH"
alias optquickcheck="python3 $OPTOOMPH/bin/parallel_self_test.py -C $OPTOOMPH"

alias buildcheck="make check -C $OOMPH/demo_drivers/poisson/one_d_poisson"

alias micromagcheck="m -C $OOMPHMM && m -C $OOMPHMM install \
&&  m -C $OOMPHMM/control_scripts/driver \
&& python3 $HOME/oomph-lib/bin/parallel_self_test.py -C $OOMPHMM"
alias optmicromagcheck="m -C $OPTOOMPHMM && m -C $OPTOOMPHMM install \
&&  m -C $OPTOOMPHMM/control_scripts/driver \
&& python3 $HOME/oomph-lib/bin/parallel_self_test.py -C $OPTOOMPHMM"

alias oopt="echo -e \"$OOMPH/config/configure_options/current contains:\n\n\"; cat $OOMPH/config/configure_options/current"

alias mm="m LIBTOOLFLAGS=--silent -C $OOMPHMM && m LIBTOOLFLAGS=--silent -C $OOMPHMM install && m LIBTOOLFLAGS=--silent -C $OOMPHMM/control_scripts/driver"

alias mdr="cd $OOMPHMM/control_scripts/driver"
alias optmdr="cd $OPTOOMPHMM/control_scripts/driver"


alias optmicromagcheck="m -C $OPTOOMPHMM && m -C $OPTOOMPHMM install &&  m -C $OPTOOMPHMM/control_scripts/driver && python3 $HOME/oomph-lib/bin/parallel_self_test.py -C $OPTOOMPHMM"

optmm()
{
    m -C $OPTOOMPHMM $@ \
        && m -C $OPTOOMPHMM install $@ \
        && m -C $OPTOOMPHMM/control_scripts/driver $@
}

optnow()
{
    cd $OPTOOMPHMM/control_scripts
}

optsyncoomph()
{
    cd $OOMPH \
        && git push --force \
        && cd $OPTOOMPH \
        && git fetch --all \
        && git reset --hard origin/working \
        && optquickautogen
}

optsync()
{
    cd $OOMPHMM \
        && git push --force \
        && optnow \
        && git fetch --all \
        && git reset --hard origin/master \
        && optmicromagcheck
}

mccat ()
{
    cat $1/Validation/make_check_output
    cat $1/Validation/validation.log
    cat $1/Validation/run_script
    cat $1/Validation/stdout
}

full_test()
{
    # Arguments are passed to quickautogen and used to label files

    mkdir test_results

    oomphfile="test_results/oomph_tests_$@"
    oomphfile2=$(echo $oomphfile | sed 's/ /_/g')

    mmfile="test_results/mm_tests_$@"
    mmfile2=$(echo $mmfile | sed 's/ /_/g')

    buildfile="test_results/build_trace_$@"
    buildfile2=$(echo $buildfile | sed 's/ /_/g')

    quickautogen $@ 2>&1 | tee $buildfile2 && \
        quickcheck --no-colour | tee $oomphfile2 && \
        micromagcheck | tee $mmfile2
}

# Run all tests with debug, mpi and mpi+opt settings (assuming we are
# starting in a debug build).
alias oomphtestall="cd ~/oomph-lib && touch test_results && mv test_results test_results.old && full_test -d && full_test -am && full_test -an"



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


# Paraview
export PATH="$PATH:$HOME/code/paraview/bin"

# Matlab
export PATH="$PATH:$HOME/code/matlab/bin:/usr/local/MATLAB/R2013a/bin"

# If we have an alternative doxygen build
if [[ -d "$HOME/code/doxygen" ]]; then
    export PATH="$PATH:$HOME/code/doxygen/bin"
fi

# arch linux stuff
export PATH="$PATH:$HOME/Dropbox/arch"


# Other micromagnetics packages:
# Add nsim to PATH
if test "-e $HOME/code/nmag*"; then
    export PATH="$PATH:$HOME/code/nmag-0.2.1/nsim/bin"
fi

# Stupid netgen!! Needs to have it's dir set for it
export NETGENDIR="/usr/share/netgen/"

# add magnum.fe to path if we have it
if [[ -d $HOME/code/dorsal_code ]]; then
    # Add FEniCS environment variables
    source $HOME/code/dorsal_code/FEniCS/share/fenics/fenics.conf
    export PYTHONPATH=$PYTHONPATH:$HOME/code/magnum.fe/site-packages
fi

# Add oommf to path
if [[ -d $HOME/code/oommf* ]]; then
    export PATH="$PATH:$HOME/code/oommf"
fi
