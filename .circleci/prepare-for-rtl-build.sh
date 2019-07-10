#!/bin/bash

# build verilator and init submodules with chipyard hash given by riscv-boom

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# call clean on exit
trap clean EXIT

run_script $LOCAL_CHIPYARD_DIR/.circleci/clean-old-files.sh $CI_DIR

# check to see if both dirs exist
if [ ! -d "$LOCAL_VERILATOR_DIR" ] && [ ! -d "$LOCAL_CHIPYARD_DIR" ]; then
    cd $HOME

    git clone --progress --verbose https://github.com/ucb-bar/chipyard.git chipyard
    cd $LOCAL_CHIPYARD_DIR

    echo "Checking out Chipyard version: $(cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash)"
    git fetch
    git checkout $(cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash)

    # init all submodules (according to what boom-template wants)
    ./scripts/init-submodules-no-riscv-tools.sh

    # move the pull request riscv-boom repo into boom-template
    rm -rf $LOCAL_CHIPYARD_DIR/generators/boom
    cp -r $LOCAL_CHECKOUT_DIR $LOCAL_CHIPYARD_DIR/generators/boom/

    # set stricthostkeychecking to no (must happen before rsync)
    run "echo \"Ping $SERVER\""

    clean

    run "mkdir -p $REMOTE_CHIPYARD_DIR"
    copy $LOCAL_CHIPYARD_DIR/ $SERVER:$REMOTE_CHIPYARD_DIR

    run "make -C $REMOTE_SIM_DIR VERILATOR_INSTALL_DIR=$REMOTE_VERILATOR_DIR verilator_install"

    # copy so that circleci can cache
    mkdir -p $LOCAL_CHIPYARD_DIR
    mkdir -p $LOCAL_VERILATOR_DIR
    copy $SERVER:$REMOTE_CHIPYARD_DIR/  $LOCAL_CHIPYARD_DIR
    copy $SERVER:$REMOTE_VERILATOR_DIR/ $LOCAL_VERILATOR_DIR

    cp -r $LOCAL_VERILATOR_DIR/install/bin/* $LOCAL_VERILATOR_DIR/install/share/verilator/bin/.
fi
