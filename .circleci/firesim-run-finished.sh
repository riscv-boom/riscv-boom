#!/bin/bash

#-------------------------------------------------------------
# get the results of the workload run
#   cat the output log from the run
#   use the variables *_PASSED given by the post that spawned this job to determine status
#
# usage:
#   $1 - config string (translates to afi folder inside firesim-configs/*)
#   $2 - workload name (folder inside firesim-configs/afi-longname/*)
#-------------------------------------------------------------

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# setup arguments
AFI_NAME=${afis[$1]}
WORKLOAD_NAME=$2

# set stricthostkeychecking to no (must happen before rsync)
run_aws "echo \"Ping $AWS_SERVER\""

copy $AWS_SERVER:$REMOTE_AWS_RESULTS_DIR/ $HOME/$WORKLOAD_NAME-resultsdir
copy $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/logs/ $HOME/$WORKLOAD_NAME-logdir

if $LAUNCHRUNFARM_PASSED; then
    echo "launchrunfarm passed"
else
    echo "launchrunfarm failed printing logs"
    cat $HOME/$WORKLOAD_NAME-logdir/*launchrunfarm*
    exit 1
fi

if $INFRASETUP_PASSED; then
    echo "infrasetup passed"
else
    echo "infrasetup failed printing logs"
    cat $HOME/$WORKLOAD_NAME-logdir/*infrasetup*
    exit 1
fi

# cannot distinguish between which command run failed so just dump all
# print the results
FMRSHL_CFG=$LOCAL_FSIM_CFGS_DIR/$AFI_NAME/$WORKLOAD_NAME/firemarshal_config
FMRSHL_NAME=$(sed -n '2p' $FMRSHL_CFG)

if $RUNWORKLOAD_PASSED; then
    echo "runworkload passed"

    #print uartlog
    cat $HOME/$WORKLOAD_NAME-resultsdir/*$FMRSHL_NAME*/*/uartlog
    exit 0
else
    echo "runworkload failed"

    #print runworkload log
    cat $HOME/$WORKLOAD_NAME-logdir/*runworkload*
    #print uartlog
    cat $HOME/$WORKLOAD_NAME-resultsdir/*$FMRSHL_NAME*/*/uartlog
    exit 1
fi

