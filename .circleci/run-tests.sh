#!/bin/bash

# run the different tests

# turn echo on and error on earliest command
set -ex

# get remote exec variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

export VERILATOR_ROOT=$LOCAL_VERILATOR_DIR/install/share/verilator

run_bmark () {
    make -j$NPROC run-bmark-tests-fast -C $LOCAL_SIM_DIR VERILATOR_INSTALL_DIR=$LOCAL_VERILATOR_DIR $@
}

run_asm () {
    make -j$NPROC run-asm-tests-fast -C $LOCAL_SIM_DIR VERILATOR_INSTALL_DIR=$LOCAL_VERILATOR_DIR $@
}

run_both () {
    run_bmark $@
    run_asm $@
}

case $1 in
    smallboom)
        run_both ${mapping[$1]}
        ;;
    mediumboom)
        run_both ${mapping[$1]}
        ;;
    largeboom)
        run_bmark ${mapping[$1]}
        ;;
    megaboom)
        run_bmark ${mapping[$1]}
        ;;
    boomandrocket)
        run_both ${mapping[$1]}
        ;;
    rv32unifiedboom)
        run_asm ${mapping[$1]}
        ;;
    *)
        echo "No set of tests for $1. Did you spell it right?"
        exit 1
        ;;
esac
