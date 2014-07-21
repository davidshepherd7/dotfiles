#!/bin/zsh

OOMPH="$HOME/oomph-lib"
OOMPHMM="$OOMPH/user_drivers/micromagnetics"
OOMPHMMDRIVER="$OOMPHMM/control_scripts/driver"
OPTOOMPH="$HOME/optoomph"
OPTOOMPHMM="$OPTOOMPH/user_drivers/micromagnetics"

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

alias optctags="ctags -e --extra=+q --recurse $OPTOOMPH/src/generic $OPTOOMPH/src/meshes $OPTOOMPH/user_drivers"


alias quickautogen="$OOMPH/autogen.sh -d $OOMPH -r -s -c ${HOME}/Dropbox/phd/oomph-lib/oomph-lib-compile-options-debug && oomphctags"
alias optquickautogen="$OPTOOMPH/autogen.sh -d $OPTOOMPH -r -s -c ${HOME}/Dropbox/phd/oomph-lib/oomph-lib-compile-options-optimise && optctags"

alias quickcheck="python3 $OOMPH/bin/parallel_self_test.py -C $OOMPH"
alias optquickcheck="python3 $OPTOOMPH/bin/parallel_self_test.py -C $OPTOOMPH"

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
