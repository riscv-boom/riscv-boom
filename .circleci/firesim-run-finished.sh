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

# setup AWS_SERVER variable
AWS_SERVER=centos@$(sed -n '2p' /tmp/FSIM_MANAGER_INSTANCE_DATA.txt)

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# setup arguments
CONFIG_KEY=$1
AFI_NAME=${afis[$1]}
WORKLOAD_NAME=$2

# install rsync
sudo apt-get install -y rsync

# see if the instance should be stopped
# TODO: if can't use context aka doesn't work, just stop on afi fail
stop_instance_check () {
    # search inside "workloads_running" file for the AFI+WORKLOAD name
    if grep "$AFI_NAME-$WORKLOAD_NAME" $REMOTE_AWS_WORK_DIR/workloads_running; then
        # delete line and check if file is empty
        sed -i "/$AFI_NAME-$WORKLOAD_NAME/d" $REMOTE_AWS_WORK_DIR/workloads_running
        if [ ! -s $REMOTE_AWS_WORK_DIR/workloads_running ]; then
            # if all workloads are done... just stop the manager instance
            MANAGER_ID=$(sed -n '1p' /tmp/FSIM_MANAGER_INSTANCE_DATA.txt)
            aws ec2 stop-instances --instance-ids $MANAGER_ID
        fi
    else
        # error... this script should not enter twice on the same AFI and same workload
        echo "[UNKNOWN] unknown error found... workload finished twice on same AFI"
        exit 1
    fi
}

# set stricthostkeychecking to no (must happen before rsync)
run_aws "echo \"Ping $AWS_SERVER\""

RESULT_DIR=$CONFIG_KEY-$WORKLOAD_NAME-resultdir
LOG_DIR=$CONFIG_KEY-$WORKLOAD_NAME-logdir

copy $AWS_SERVER:$REMOTE_AWS_RESULTS_DIR/ $HOME/$RESULT_DIR
copy $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/logs/ $HOME/$LOG_DIR

set +e

WORKLOAD_LOG=$(grep -irl "$FMRSHL_NAME" $HOME/$LOG_DIR/*launchrunfarm*)
UNIQUE_LOG=$(grep -irl "$AFI_NAME" $WORKLOAD_LOG)
if $LAUNCHRUNFARM_PASSED; then
    echo "[LAUNCHRUNFARM] passed"
else
    echo "[LAUNCHRUNFARM] failed... printing log"
    cat $UNIQUE_LOG

    stop_instance_check
    exit 1
fi

WORKLOAD_LOG=$(grep -irl "$FMRSHL_NAME" $HOME/$LOG_DIR/*infrasetup*)
UNIQUE_LOG=$(grep -irl "$AFI_NAME" $WORKLOAD_LOG)
if $INFRASETUP_PASSED; then
    echo "[INFRASETUP] passed"
else
    echo "[INFRASETUP] failed... printing log"
    cat $UNIQUE_LOG

    stop_instance_check
    exit 1
fi

# cannot distinguish between which command run failed so just dump all
# print the results
FMRSHL_CFG=$LOCAL_FSIM_CFGS_DIR/$AFI_NAME/$WORKLOAD_NAME/firemarshal_config
FMRSHL_NAME=$(sed -n '2p' $FMRSHL_CFG)

WORKLOAD_LOG=$(grep -irl "$FMRSHL_NAME" $HOME/$LOG_DIR/*runworkload*)
UNIQUE_LOG=$(grep -irl "$AFI_NAME" $WORKLOAD_LOG)
if $RUNWORKLOAD_PASSED; then
    echo "[RUNWORKLOAD] passed"

    echo "[RUNWORKLOAD] printing uart log"
    cat $HOME/$RESULT_DIR/*$FMRSHL_NAME*/*/uartlog

    stop_instance_check
    exit 0
else
    echo "[RUNWORKLOAD] failed"

    echo "[RUNWORKLOAD] printing log"
    cat $UNIQUE_LOG

    echo "[RUNWORKLOAD] printing uart log"
    echo "[RUNWORKLOAD]   note: cannot distinguish between multiple same workload names with different afis"
    echo "[RUNWORKLOAD]         printing all uart logs"
    cat $HOME/$RESULT_DIR/*$FMRSHL_NAME*/*/uartlog

    stop_instance_check
    exit 1
fi
