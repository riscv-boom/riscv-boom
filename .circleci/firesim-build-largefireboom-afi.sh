#!/bin/bash

# take the hash located in chipyard-boom.hash
# and see if the /home/riscvuser/largefireboom_hwdb.ini entry exists

# if so then it no need to regen afi

# turn echo on and error on earliest command
set -ex

if [ ! -d "$HOME/largefireboom_hwdb.ini" ]; then
    copy $LOCAL_CHECKOUT_DIR/.circleci/firesim-configs $SERVER_AWS:$REMOTE_AWS_FSIM_DEPLOY_DIR

    run_manager
        # setup firesim
        cd $REMOTE_AWS_FSIM_DIR
        source sourceme_f1_manager.sh

        # start a afi build
        cd deploy
        firesim buildafi -b $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/config_build.ini -r $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/config_build_recipes.ini 

    # copy over hwdb entry
    copy $SERVER_AWS:$REMOTE_AWS_FSIM_DEPLOY_DIR/built-hwdb-entries/ $HOME/

    # TODO: TRANSFER FROM THE hwdb folder to the largefireboom_hwdb.ini file
fi
