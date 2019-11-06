#!/bin/bash

# -------------------------------------------------------------
# make an afi (run, detach so it runs in the background)
# spawn all jobs related to it (specific to this script)
# **runs all workloads that finished building**
#   - otherwise it just skips that workload
#
# usage:
#   $1 - config string (translates to afi folder inside firesim-configs/*)
#-------------------------------------------------------------

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# setup arguments
CONFIG_KEY=$1
AFI_NAME=${afis[$1]}

# set stricthostkeychecking to no (must happen before rsync)
run_aws "echo \"Ping $AWS_SERVER\""

# copy over the configuration collateral
copy $LOCAL_FSIM_CFGS_DIR/$AFI_NAME $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/

# setup workload variables
BUILDROOT_CFG=$LOCAL_FSIM_CFGS_DIR/$AFI_NAME/buildroot/firemarshal_config
FEDORA_CFG=$LOCAL_FSIM_CFGS_DIR/$AFI_NAME/fedora/firemarshal_config

SCRIPT_NAME=firesim-build-$AFI_NAME-afi.sh

# create a script to run
cat <<EOF >> $LOCAL_CHECKOUT_DIR/$SCRIPT_NAME
#!/bin/bash

set -ex

# setup firesim
cd $REMOTE_AWS_FSIM_DIR
source sourceme-f1-manager.sh

set +e

# build afi
if firesim buildafi -b $REMOTE_AWS_FSIM_DEPLOY_DIR/$AFI_NAME/config_build.ini -r $REMOTE_AWS_FSIM_DEPLOY_DIR/$AFI_NAME/config_build_recipes.ini; then
    echo "AFI successfully built"
else
    # spawn fail job
    echo "AFI failed... spawning failure job"
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=$CONFIG_KEY-afi-failed \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
    exit 1
fi

# launch workloads related to this afi
cd $REMOTE_AWS_FSIM_DEPLOY_DIR
./$AFI_NAME/launch-workloads.sh $CONFIG_KEY $AFI_NAME $API_TOKEN $CIRCLE_SHA1 $API_URL $CIRCLE_PROJECT_USERNAME $CIRCLE_PROJECT_REPONAME $CIRCLE_BRANCH

EOF

echo "script created"
cat $LOCAL_CHECKOUT_DIR/$SCRIPT_NAME

# execute the script and detach
chmod +x $LOCAL_CHECKOUT_DIR/$SCRIPT_NAME
run_detach_script_aws $AFI_NAME $LOCAL_CHECKOUT_DIR $SCRIPT_NAME
