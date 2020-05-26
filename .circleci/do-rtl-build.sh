#!/bin/bash

#-------------------------------------------------------------
# create the different verilator builds of BOOM
# the command is the make command string
#
# run location: circle ci docker image
# usage:
#   $1 - config string
#-------------------------------------------------------------

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

rm -rf $LOCAL_CHIPYARD_DIR/generators/boom/*
mv -f $LOCAL_CHECKOUT_DIR/* $LOCAL_CHIPYARD_DIR/generators/boom/

# call clean on exit
trap clean EXIT

# set stricthostkeychecking to no (must happen before rsync)
run "echo \"Ping $SERVER\""

clean

# copy over riscv/esp-tools, and chipyard to remote
run "mkdir -p $REMOTE_CHIPYARD_DIR"
copy $LOCAL_CHIPYARD_DIR/ $SERVER:$REMOTE_CHIPYARD_DIR

run "cp -r ~/.ivy2 $REMOTE_WORK_DIR"
run "cp -r ~/.sbt  $REMOTE_WORK_DIR"

TOOLS_DIR=$REMOTE_RISCV_DIR
LD_LIB_DIR=$REMOTE_RISCV_DIR/lib
if [ $1 = "hwachaboom" ]; then
    TOOLS_DIR=$REMOTE_ESP_DIR
    LD_LIB_DIR=$REMOTE_ESP_DIR/lib
    run "mkdir -p $REMOTE_ESP_DIR"
    copy $LOCAL_ESP_DIR/ $SERVER:$REMOTE_ESP_DIR
else
    run "mkdir -p $REMOTE_RISCV_DIR"
    copy $LOCAL_RISCV_DIR/ $SERVER:$REMOTE_RISCV_DIR
fi

# enter the verilator directory and build the specific config on remote server
run "make -C $REMOTE_SIM_DIR clean"
run "export RISCV=\"$TOOLS_DIR\"; \
     export LD_LIBRARY_PATH=\"$LD_LIB_DIR\"; \
     export PATH=\"$REMOTE_VERILATOR_DIR/bin:\$PATH\"; \
     make -C $REMOTE_SIM_DIR -j$REMOTE_MAKE_NPROC JAVA_ARGS=\"-Xmx16G -Xss8M\" ${mapping[$1]}"
run "rm -rf $REMOTE_CHIPYARD_DIR/project"

# copy back the final build
mkdir -p $LOCAL_CHIPYARD_DIR
copy $SERVER:$REMOTE_CHIPYARD_DIR/ $LOCAL_CHIPYARD_DIR
