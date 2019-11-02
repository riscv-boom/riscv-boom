#!/bin/bash

# get the results of the workload run
#   cat the output log from the run
#   use the variables *_PASSED given by the post that spawned this job to determine status
# $1 - the name of the workload (e.g. buildroot, fedora)

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# set stricthostkeychecking to no (must happen before rsync)
run_aws "echo \"Ping $AWS_SERVER\""

copy $AWS_SERVER:$REMOTE_AWS_RESULTS_DIR/ $HOME/$1-resultsdir
copy $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/logs/ $HOME/$1-logdir

if $LAUNCHRUNFARM_PASSED; then
    echo "launchrunfarm passed"
else
    echo "launchrunfarm failed printing logs"
    cat $HOME/$1-logdir/*launchrunfarm*
    exit 1
fi

if $INFRASETUP_PASSED; then
    echo "infrasetup passed"
else
    echo "infrasetup failed printing logs"
    cat $HOME/$1-logdir/*infrasetup*
    exit 1
fi

# cannot distinguish between which command run failed so just dump all
# print the results
FMRSHL_CFG=$LOCAL_CHECKOUT_DIR/.circleci/firesim-configs/$1/firemarshal_config
WORKLOAD_NAME=$(sed -n '2p' $FMRSHL_CFG)

if $RUNWORKLOAD_PASSED; then
    echo "runworkload passed"

    #print uartlog
    cat $HOME/$1-resultsdir/*$WORKLOAD_NAME*/*/uartlog
    exit 0
else
    echo "runworkload failed"

    #print runworkload log
    cat $HOME/$1-logdir/*runworkload*
    #print uartlog
    cat $HOME/$1-resultsdir/*$WORKLOAD_NAME*/*/uartlog
    exit 1
fi

