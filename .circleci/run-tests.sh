#!/bin/bash

# run the different tests

# turn echo on and error on earliest command
set -ex

# get remote exec variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

run_bmark () {
    export VERILATOR_ROOT=$LOCAL_VERILATOR_DIR/install/share/verilator
    make run-bmark-tests-fast -C $LOCAL_SIM_DIR VERILATOR_INSTALL_DIR=$LOCAL_VERILATOR_DIR $@
}

run_asm () {
    export VERILATOR_ROOT=$LOCAL_VERILATOR_DIR/install/share/verilator
    make run-asm-tests-fast -C $LOCAL_SIM_DIR VERILATOR_INSTALL_DIR=$LOCAL_VERILATOR_DIR $@
}

run_both () {
    run_bmark $@
    run_asm $@
}

case $1 in
    SmallBoomConfig)
        run_both SUB_PROJECT=boom CONFIG=SmallBoomConfig
        ;;
    MediumBoomConfig)
        run_both SUB_PROJECT=boom CONFIG=MediumBoomConfig
        ;;
    LargeBoomConfig)
        run_bmark SUB_PROJECT=boom CONFIG=LargeBoomConfig
        ;;
    MegaBoomConfig)
        run_bmark SUB_PROJECT=boom CONFIG=MegaBoomConfig
        ;;
    SmallBoomAndRocketConfig)
        run_both SUB_PROJECT=boom CONFIG=SmallBoomAndRocketConfig
        ;;
    SmallRV32UnifiedBoomConfig)
        run_asm SUB_PROJECT=boom CONFIG=SmallRV32UnifiedBoomConfig
        ;;
    *)
        echo "No set of tests for $1. Did you spell it right?"
        exit 1
        ;;
esac
