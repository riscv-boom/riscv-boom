#!/bin/bash

# make an afi and detach from server
# spawn all jobs related to it
# requires workloads to be done before this afi is built
# usage
#   $1 - the name of the afi to build

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# set stricthostkeychecking to no (must happen before rsync)
run_aws "echo \"Ping $AWS_SERVER\""

copy $LOCAL_CHECKOUT_DIR/.circleci/firesim-configs/$1 $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/

# setup workload variables
BUILDROOT_CFG=$LOCAL_CHECKOUT_DIR/.circleci/firesim-configs/buildroot/firemarshal_config
FEDORA_CFG=$LOCAL_CHECKOUT_DIR/.circleci/firesim-configs/fedora/firemarshal_config

# create a script to run
cat <<EOF >> $LOCAL_CHECKOUT_DIR/firesim-build-$1-afi.sh
#!/bin/bash

set -ex

# setup firesim
cd $REMOTE_AWS_FSIM_DIR
source sourceme-f1-manager.sh

set +e

# build afi
if firesim buildafi -b $REMOTE_AWS_FSIM_DEPLOY_DIR/$1/config_build.ini -r $REMOTE_AWS_FSIM_DEPLOY_DIR/$1/config_build_recipes.ini; then
    echo "AFI successfully built"
else
    # spawn fail job
    echo "AFI failed... spawning failure job"
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=afi-failed \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
    exit 1
fi

# run workload 1 - buildroot
WORKLOAD_NAME=$(sed -n '2p' $BUILDROOT_CFG)
WORKLOAD_DIR_NAME=$(sed -n '1p' $BUILDROOT_CFG)
if [ -f $REMOTE_AWS_WORK_DIR/\$WORKLOAD_DIR_NAME-\$WORKLOAD_NAME-FINISHED ]; then
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=launch-buildroot-run \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
fi

# run workload 2 - fedora
WORKLOAD_NAME=$(sed -n '2p' $FEDORA_CFG)
WORKLOAD_DIR_NAME=$(sed -n '1p' $FEDORA_CFG)
if [ -f $REMOTE_AWS_WORK_DIR/\$WORKLOAD_DIR_NAME-\$WORKLOAD_NAME-FINISHED ]; then
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=launch-fedora-run \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
fi

EOF

echo "script created"
cat $LOCAL_CHECKOUT_DIR/firesim-build-$1-afi.sh

# execute the script and detach
chmod +x $LOCAL_CHECKOUT_DIR/firesim-build-$1-afi.sh
run_detach_script_aws $1 $LOCAL_CHECKOUT_DIR firesim-build-$1-afi.sh
