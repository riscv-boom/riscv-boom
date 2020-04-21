#!/bin/bash

# init submodules with chipyard hash given by riscv-boom

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# call clean on exit
trap clean EXIT

run_script $LOCAL_CHECKOUT_DIR/.circleci/clean-old-files.sh $CI_DIR

# check to see if both dirs exist
if [ ! -d "$LOCAL_CHIPYARD_DIR" ]; then
    cd $HOME

    git clone --progress --verbose https://github.com/ucb-bar/chipyard.git chipyard
    cd $LOCAL_CHIPYARD_DIR

    echo "Checking out Chipyard version: $(cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash)"
    git fetch
    git checkout $(cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash)

    # init all submodules (according to what chipyard wants)
    ./scripts/init-submodules-no-riscv-tools.sh

    # move the pull request riscv-boom repo into chipyard
    rm -rf $LOCAL_CHIPYARD_DIR/generators/boom
    cp -r $LOCAL_CHECKOUT_DIR $LOCAL_CHIPYARD_DIR/generators/boom/

    clean
fi
