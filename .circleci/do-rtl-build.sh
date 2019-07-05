#!/bin/bash

# create the different verilator builds of BOOM based on arg

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

REMOTE_JOB_DIR=$REMOTE_WORK_DIR/chipyard-$1
REMOTE_SIM_DIR=$REMOTE_JOB_DIR/sims/verisim

# set stricthostkeychecking to no (must happen before rsync)
run "echo \"Ping $SERVER\""

run "mkdir -p $REMOTE_JOB_DIR"
run "cp -R $REMOTE_CHIPYARD_DIR/. $REMOTE_JOB_DIR"

# enter the verisim directory and build the specific config on remote server
run "make -C $REMOTE_SIM_DIR clean"
run "export RISCV=\"$REMOTE_RISCV_DIR\"; make -C $REMOTE_SIM_DIR VERILATOR_INSTALL_DIR=$REMOTE_VERILATOR_DIR JAVA_ARGS=\"-Xmx8G -Xss8M\" SUB_PROJECT=boom CONFIG=$1 TOP=BoomRocketSystem"

# copy back the final build
mkdir -p $LOCAL_CHIPYARD_DIR
copy $SERVER:$REMOTE_JOB_DIR/ $LOCAL_CHIPYARD_DIR
