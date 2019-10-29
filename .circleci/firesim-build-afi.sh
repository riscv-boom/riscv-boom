#!/bin/bash

# take the hash located in chipyard-boom.hash
# and see if the /home/riscvuser/largefireboom_hwdb.ini entry exists

# if so then it no need to regen afi

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

if [ ! -d "$HOME/$1" ]; then
    copy $LOCAL_CHECKOUT_DIR/.circleci/firesim-configs/$1 $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR

    # create a script to run
    cat <<EOF >> $LOCAL_CHECKOUT_DIR/firesim-build-$1-afi.sh
#!/bin/bash

set -ex

# setup firesim
cd $REMOTE_AWS_FSIM_DIR
source sourceme-f1-manager.sh

# build afi
firesim buildafi -b $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/$1/config_build.ini -r $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/$1/config_build_recipes.ini
EOF

    # execute the script
    chmod +x $LOCAL_CHECKOUT_DIR/firesim-build-$1-afi.sh
    run_script_aws $LOCAL_CHECKOUT_DIR/firesim-build-$1-afi.sh

    # copy over hwdb entries to cache them
    copy $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/built-hwdb-entries/$1 $HOME/
fi
