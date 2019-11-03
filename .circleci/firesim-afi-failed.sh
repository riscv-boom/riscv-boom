#!/bin/bash

# -------------------------------------------------------------
# retrieve and cat the output log from the failed build
# -------------------------------------------------------------

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# set stricthostkeychecking to no (must happen before rsync)
run_aws "echo \"Ping $AWS_SERVER\""

# copy over the logs
copy $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/logs/ $HOME/

# cannot distinguish between which afi run failed so just dump all
cat $HOME/*buildafi*

# this is just to fail
exit 1
