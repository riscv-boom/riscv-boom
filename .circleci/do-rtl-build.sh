#!/bin/bash

# create the different verilator builds of BOOM based on arg

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# call finish on exit
finish () {
    # remove remote work dir
    run "rm -rf $REMOTE_WORK_DIR"
}
trap finish EXIT

# set stricthostkeychecking to no (must happen before rsync)
run "echo \"Ping $SERVER\""

# copy over riscv-tools, verilator, and chipyard to remote
run "mkdir -p $REMOTE_RISCV_DIR"
run "mkdir -p $REMOTE_CHIPYARD_DIR"
run "mkdir -p $REMOTE_VERILATOR_DIR"
copy $LOCAL_RISCV_DIR/ $SERVER:$REMOTE_RISCV_DIR
copy $LOCAL_CHIPYARD_DIR/ $SERVER:$REMOTE_CHIPYARD_DIR
copy $LOCAL_VERILATOR_DIR/ $SERVER:$REMOTE_VERILATOR_DIR

# enter the verisim directory and build the specific config on remote server
run "make -C $REMOTE_SIM_DIR clean"
run "export RISCV=\"$REMOTE_RISCV_DIR\"; make -C $REMOTE_SIM_DIR VERILATOR_INSTALL_DIR=$REMOTE_VERILATOR_DIR JAVA_ARGS=\"-Xmx8G -Xss8M\" SUB_PROJECT=boom CONFIG=$1 TOP=BoomRocketSystem"

# copy back the final build
mkdir -p $LOCAL_CHIPYARD_DIR
copy $SERVER:$REMOTE_CHIPYARD_DIR/ $LOCAL_CHIPYARD_DIR
