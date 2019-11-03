#!/bin/bash

#-------------------------------------------------------------
# run the afi's workload (run, detach)
#
# usage:
#   $1 - firesim afi longname (folder inside firesim-configs/*)
#   $2 - workload name (folder inside firesim-configs/afi-longname/*)
#-------------------------------------------------------------

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# setup arguments
AFI_NAME=$1
WORKLOAD_NAME=$2

# set stricthostkeychecking to no (must happen before rsync)
run_aws "echo \"Ping $AWS_SERVER\""

# copy collateral needed
copy $LOCAL_FSIM_CFGS_DIR/$AFI_NAME $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/

REMOTE_CFG_DIR=$REMOTE_AWS_FSIM_DEPLOY_DIR/$AFI_NAME
# TODO TODO TODO TODO TODO TODO Renable
#BUILD_ARGS="-c $REMOTE_CFG_DIR/$WORKLOAD_NAME/config_runtime.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/built-hwdb-entries/$AFI_NAME -r $REMOTE_CFG_DIR/config_build_recipes.ini"
BUILD_ARGS="-c $REMOTE_CFG_DIR/$WORKLOAD_NAME/config_runtime.ini -a /home/centos/riscv-boom-firesim-ci-2e15ba72f8bc7c70ed976af4c5efa32d701ff273/chipyard/sims/firesim/deploy/built-hwdb-entries/$AFI_NAME -r $REMOTE_CFG_DIR/config_build_recipes.ini"
SCRIPT_NAME=firesim-run-$AFI_NAME-$WORKLOAD_NAME.sh

cat <<EOF >> $LOCAL_CHECKOUT_DIR/$SCRIPT_NAME
#!/bin/bash

set -ex

# setup firesim
cd $REMOTE_AWS_FSIM_DIR
source sourceme-f1-manager.sh

set +e

if firesim launchrunfarm $BUILD_ARGS; then
    echo "launchrunfarm passed"
else
    echo "launchrunfarm failed"
    firesim terminaterunfarm -q $BUILD_ARGS
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=$WORKLOAD_NAME-run-finished \
        -d build_parameters[LAUNCHRUNFARM_PASSED]=false \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
    exit 1
fi

if firesim infrasetup $BUILD_ARGS; then
    echo "infrasetup passed"
else
    echo "infrasetup failed"
    firesim terminaterunfarm -q $BUILD_ARGS
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=$WORKLOAD_NAME-run-finished \
        -d build_parameters[LAUNCHRUNFARM_PASSED]=true \
        -d build_parameters[INFRASETUP_PASSED]=false \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
    exit 1
fi

if timeout -k 3m 30m firesim runworkload $BUILD_ARGS; then
    echo "runworkload passed"
    firesim terminaterunfarm -q $BUILD_ARGS
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=$WORKLOAD_NAME-run-finished \
        -d build_parameters[LAUNCHRUNFARM_PASSED]=true \
        -d build_parameters[INFRASETUP_PASSED]=true \
        -d build_parameters[RUNWORKLOAD_PASSED]=true \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
    exit 0
else
    echo "runworkload failed"
    firesim terminaterunfarm -q $BUILD_ARGS
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=$WORKLOAD_NAME-run-finished \
        -d build_parameters[LAUNCHRUNFARM_PASSED]=true \
        -d build_parameters[INFRASETUP_PASSED]=true \
        -d build_parameters[RUNWORKLOAD_PASSED]=false \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
    exit 1
fi
EOF

echo "script created"
cat $LOCAL_CHECKOUT_DIR/$SCRIPT_NAME

# execute the script and detach
chmod +x $LOCAL_CHECKOUT_DIR/$SCRIPT_NAME
run_detach_script_aws $WORKLOAD_NAME $LOCAL_CHECKOUT_DIR $SCRIPT_NAME
