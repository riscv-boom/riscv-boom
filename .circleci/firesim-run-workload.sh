#!/bin/bash

# run the workload and detach
# usage
#  $1 - the name of the afi used
#  $2 - the name of the workload dir in firesim-configs

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# set stricthostkeychecking to no (must happen before rsync)
run_aws "echo \"Ping $AWS_SERVER\""

copy $LOCAL_CHECKOUT_DIR/.circleci/firesim-configs/$1/ $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/
copy $LOCAL_CHECKOUT_DIR/.circleci/firesim-configs/$2/ $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/

BUILD_ARGS=-c $REMOTE_AWS_FSIM_DEPLOY_DIR/config_runtime.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/built-hwdb-entries/$1 -r $REMOTE_AWS_FSIM_DEPLOY_DIR/config_build_recipes.ini

cat <<EOF >> $LOCAL_CHECKOUT_DIR/firesim-run-$1-$2.sh
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
        -d build_parameters[CIRCLE_JOB]=$1-run-finished \
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
        -d build_parameters[CIRCLE_JOB]=$1-run-finished \
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
        -d build_parameters[CIRCLE_JOB]=$1-run-finished \
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
        -d build_parameters[CIRCLE_JOB]=$1-run-finished \
        -d build_parameters[LAUNCHRUNFARM_PASSED]=true \
        -d build_parameters[INFRASETUP_PASSED]=true \
        -d build_parameters[RUNWORKLOAD_PASSED]=false \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
    exit 1
fi
EOF

echo "script created"
cat $LOCAL_CHECKOUT_DIR/firesim-run-$1-$2.sh

# execute the script and detach
chmod +x $LOCAL_CHECKOUT_DIR/firesim-run-$1-$2.sh
run_detach_script_aws $1 $LOCAL_CHECKOUT_DIR firesim-run-$1-$2.sh
