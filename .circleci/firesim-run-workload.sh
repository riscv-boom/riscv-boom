#!/bin/bash

#-------------------------------------------------------------
# run the afi's workload (run, detach)
#
# usage:
#   $1 - config string (translates to afi folder inside firesim-configs/*)
#   $2 - workload name (folder inside firesim-configs/afi-longname/*)
#   $3 - timeout amount (amount of time to run the workload before killing it)
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
TIMEOUT=$3

# set stricthostkeychecking to no (must happen before rsync)
run_aws "echo \"Ping $AWS_SERVER\""

# copy collateral needed
copy $LOCAL_FSIM_CFGS_DIR/$AFI_NAME $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/

REMOTE_CFG_DIR=$REMOTE_AWS_FSIM_DEPLOY_DIR/$AFI_NAME
BUILD_ARGS="-c $REMOTE_CFG_DIR/$WORKLOAD_NAME/config_runtime.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/built-hwdb-entries/$AFI_NAME -r $REMOTE_CFG_DIR/config_build_recipes.ini"
#BUILD_ARGS="-c $REMOTE_CFG_DIR/$WORKLOAD_NAME/config_runtime.ini -a $CI_AWS_DIR/riscv-boom-firesim-ci-57f15deed62bd778cdd9a03dcc8135dbc1526501/chipyard/sims/firesim/deploy/built-hwdb-entries/fireboom-singlecore-no-nic-l2-llc4mb-ddr3 -r $REMOTE_CFG_DIR/config_build_recipes.ini"
SCRIPT_NAME=firesim-run-$AFI_NAME-$WORKLOAD_NAME.sh

cat <<EOF >> $LOCAL_CHECKOUT_DIR/$SCRIPT_NAME
#!/bin/bash

set -ex

# setup firesim
cd $REMOTE_AWS_FSIM_DIR
source sourceme-f1-manager.sh

set +e

# acquire lock on the afi name (race condition in the firesim make during infrasetup)
exec {lock_fd}>$REMOTE_AWS_FSIM_DIR/$AFI_NAME.lock || exit 1
flock "\$lock_fd" || { echo "ERROR: flock() failed." >&2; exit 1; }

if firesim launchrunfarm $BUILD_ARGS; then
    echo "launchrunfarm passed"
else
    echo "launchrunfarm failed"
    firesim terminaterunfarm -q $BUILD_ARGS
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=$CONFIG_KEY-$WORKLOAD_NAME-run-finished \
        -d build_parameters[LAUNCHRUNFARM_PASSED]=false \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH

    # release the lock
    flock -u "\$lock_fd"

    exit 1
fi

if firesim infrasetup $BUILD_ARGS; then
    echo "infrasetup passed"
else
    echo "infrasetup failed"
    firesim terminaterunfarm -q $BUILD_ARGS
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=$CONFIG_KEY-$WORKLOAD_NAME-run-finished \
        -d build_parameters[LAUNCHRUNFARM_PASSED]=true \
        -d build_parameters[INFRASETUP_PASSED]=false \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH

    # release the lock
    flock -u "\$lock_fd"

    exit 1
fi

# release the lock
flock -u "\$lock_fd"

if timeout -k 3m $TIMEOUT firesim runworkload $BUILD_ARGS; then
    echo "runworkload passed"
    firesim terminaterunfarm -q $BUILD_ARGS
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=$CONFIG_KEY-$WORKLOAD_NAME-run-finished \
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
        -d build_parameters[CIRCLE_JOB]=$CONFIG_KEY-$WORKLOAD_NAME-run-finished \
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
