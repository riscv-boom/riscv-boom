#!/bin/bash

# init manager with chipyard, setup firesim

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# TODO: CLEAR OLD FIRESIM DIR
#run_script $LOCAL_CHECKOUT_DIR/.circleci/clean-old-files.sh $CI_DIR

run_manager
    git clone --progress --verbose https://github.com/ucb-bar/chipyard.git $REMOTE_AWS_CHIPYARD_DIR
    cd $REMOTE_AWS_CHIPYARD_DIR

    echo "Checking out Chipyard version: $(cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash)"
    git fetch
    git checkout $(cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash)

    # init all submodules and firesim
    ./scripts/init-submodules-no-riscv-tools.sh
    ./scripts/firesim-setup.sh --fast

    # setup firesim
    firesim managerinit <<EOD




    EOD

    # move the pull request riscv-boom repo into chipyard
    rm -rf $REMOTE_CHIPYARD_DIR/generators/boom
    git checkout -C $REMOTE_AWS_MARSHAL_DIR MY_HASH_WITH_THE_CAT_PROC_STUFF

copy $LOCAL_CHECKOUT_DIR $SERVER_AWS:$REMOTE_CHIPYARD_DIR/generators/boom/


