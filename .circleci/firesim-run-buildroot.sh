#!/bin/bash

# turn echo on and error on earliest command
set -ex

copy $LOCAL_CHECKOUT_DIR/.circleci/firesim-configs $SERVER_AWS:$REMOTE_AWS_FSIM_DEPLOY_DIR
copy $HOME/largefireboom_hwdb.ini $SERVER_AWS:$REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs

run_manager
    # setup firesim
    cd $REMOTE_AWS_FSIM_DIR
    source sourceme_f1_manager.sh

    # start up the run
    cd deploy
    firesim launchrunfarm -c $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/config_runtime_buildroot.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/largefireboom_hwdb.ini
    firesim infrasetup -c $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/config_runtime_buildroot.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/largefireboom_hwdb.ini

    # timeout after 30m or 33m
    timeout -k 3m 30m firesim runworkload -c $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/config_runtime_buildroot.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/largefireboom_hwdb.ini

# copy over results
copy $SERVER_AWS:$REMOTE_AWS_RESULTS_DIR/ $HOME/

# print the results
cat results_workload/*br*/*/uartlog
