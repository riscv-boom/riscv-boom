#!/bin/bash

############################################################################
# launch workloads related to this afi
# note: expects that this is called from the firesim deploy directory
#
# usage:
#   $1 - config string (translates to afi folder inside firesim-configs/*)
#   $2 - afi name
#   $3 - circle ci api token
#   $4 - circle ci sha1 hash
#   $5 - circle ci api url
#   $6 - circle ci project username
#   $7 - circle ci project reponame
#   $8 - circle ci branch
############################################################################

# setup input variables
DEPLOY_DIR=$PWD
CONFIG_KEY=$1
AFI_NAME=$2
API_TOKEN=$3
CIRCLE_SHA1=$4
API_URL=$5
CIRCLE_PROJECT_USERNAME=$6
CIRCLE_PROJECT_REPONAME=$7
CIRCLE_BRANCH=$8
REMOTE_AWS_WORK_DIR=$DEPLOY_DIR/../../../..

# run workload 1 - buildroot
WORKLOAD_NAME=buildroot
BUILDROOT_CFG=$DEPLOY_DIR/$AFI_NAME/$WORKLOAD_NAME/firemarshal_config
FMRSHL_NAME=$(sed -n '2p' $BUILDROOT_CFG)
FMRSHL_DIR_NAME=$(sed -n '1p' $BUILDROOT_CFG)
if [ -f $REMOTE_AWS_WORK_DIR/$AFI_NAME-$WORKLOAD_NAME-FINISHED ]; then
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=launch-$CONFIG_KEY-$WORKLOAD_NAME-run \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
fi

# run workload 2 - fedora
WORKLOAD_NAME=fedora
FEDORA_CFG=$DEPLOY_DIR/$AFI_NAME/$WORKLOAD_NAME/firemarshal_config
FMRSHL_NAME=$(sed -n '2p' $FEDORA_CFG)
FMRSHL_DIR_NAME=$(sed -n '1p' $FEDORA_CFG)
if [ -f $REMOTE_AWS_WORK_DIR/$AFI_NAME-$WORKLOAD_NAME-FINISHED ]; then
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=launch-$CONFIG_KEY-$WORKLOAD_NAME-run \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
fi

# run workload 3 - coremark
WORKLOAD_NAME=coremark
COREMARK_CFG=$DEPLOY_DIR/$AFI_NAME/$WORKLOAD_NAME/firemarshal_config
FMRSHL_NAME=$(sed -n '2p' $COREMARK_CFG)
FMRSHL_DIR_NAME=$(sed -n '1p' $COREMARK_CFG)
if [ -f $REMOTE_AWS_WORK_DIR/$AFI_NAME-$WORKLOAD_NAME-FINISHED ]; then
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=launch-$CONFIG_KEY-$WORKLOAD_NAME-run \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
fi

# run workload 4 - spec17-intspeed
WORKLOAD_NAME=spec17-intspeed
INTSPEED_CFG=$DEPLOY_DIR/$AFI_NAME/$WORKLOAD_NAME/firemarshal_config
FMRSHL_NAME=$(sed -n '2p' $INTSPEED_CFG)
FMRSHL_DIR_NAME=$(sed -n '1p' $INTSPEED_CFG)
if [ -f $REMOTE_AWS_WORK_DIR/$AFI_NAME-$WORKLOAD_NAME-FINISHED ]; then
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=launch-$CONFIG_KEY-$WORKLOAD_NAME-run \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
fi

# run workload 5 - spec17-intrate
WORKLOAD_NAME=spec17-intrate
INTRATE_CFG=$DEPLOY_DIR/$AFI_NAME/$WORKLOAD_NAME/firemarshal_config
FMRSHL_NAME=$(sed -n '2p' $INTRATE_CFG)
FMRSHL_DIR_NAME=$(sed -n '1p' $INTRATE_CFG)
if [ -f $REMOTE_AWS_WORK_DIR/$AFI_NAME-$WORKLOAD_NAME-FINISHED ]; then
    curl -u $API_TOKEN: \
        -d build_parameters[CIRCLE_JOB]=launch-$CONFIG_KEY-$WORKLOAD_NAME-run \
        -d revision=$CIRCLE_SHA1 \
        $API_URL/project/github/$CIRCLE_PROJECT_USERNAME/$CIRCLE_PROJECT_REPONAME/tree/$CIRCLE_BRANCH
fi
