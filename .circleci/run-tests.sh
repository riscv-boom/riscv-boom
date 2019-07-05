#!/bin/bash

# run the different tests

# turn echo on and error on earliest command
set -ex

# get remote exec variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

run_bmark () {
    make run-bmark-tests-fast -C $LOCAL_SIM_DIR VERILATOR_INSTALL_DIR=$LOCAL_VERILATOR_DIR SUB_PROJECT=boom CONFIG=$1
}

run_asm () {
    make run-asm-tests-fast -C $LOCAL_SIM_DIR VERILATOR_INSTALL_DIR=$LOCAL_VERILATOR_DIR SUB_PROJECT=boom CONFIG=$1
}

run_both () {
    run_bmark $1
    run_asm $1
}

case $1 in
    SmallBoomConfig)
        run_both $1
        ;;
    MediumBoomConfig)
        run_both $1
        ;;
    LargeBoomConfig)
        run_bmark $1
        ;;
    MegaBoomConfig)
        run_bmark $1
        ;;
    SmallBoomAndRocketConfig)
        run_both $1
        ;;
    SmallRV32UnifiedBoomConfig)
        run_asm $1
        ;;
    *)
        echo "No set of tests for $1. Did you spell it right?"
        exit 1
        ;;
esac
