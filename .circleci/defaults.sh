#!/bin/bash

# shared variables between the different services
#
# CircleCI set values:
#   $SERVER - points to the millennium build server
#   $AWS_SERVER - points to the aws manager instance
#   $CI_DIR - home directory on build server
#   $CI_AWS_DIR - home directory on aws

##################
# SHARED FUNCTIONS
##################

# shared across build server and aws

copy () {
    rsync -avzp -e 'ssh' $1 $2
}

run_impl () {
    ssh -o "StrictHostKeyChecking no" -t $1 $2
}

run_script_impl () {
    ssh -o "StrictHostKeyChecking no" -t $1 'bash -s' < $2 "$3"
}

# build server calls

run () {
    run_impl $SERVER $@
}

run_script () {
    run_script_impl $SERVER $1 $2
}

clean () {
    # remove remote work dir
    run "rm -rf $REMOTE_WORK_DIR"
}

# aws calls

run_aws () {
    run_impl $AWS_SERVER $@
}

run_script_aws () {
    run_script_impl $AWS_SERVER $1 $2
}

#############
# SHARED VARS
#############

# make parallelism
NPROC=8

# remote variables
REMOTE_WORK_DIR=$CI_DIR/$CIRCLE_PROJECT_REPONAME-$CIRCLE_BRANCH-$CIRCLE_SHA1-$CIRCLE_JOB
REMOTE_RISCV_DIR=$REMOTE_WORK_DIR/riscv-tools-install
REMOTE_ESP_DIR=$REMOTE_WORK_DIR/esp-tools-install
REMOTE_CHIPYARD_DIR=$REMOTE_WORK_DIR/chipyard
REMOTE_VERILATOR_DIR=$REMOTE_WORK_DIR/verilator
REMOTE_SIM_DIR=$REMOTE_CHIPYARD_DIR/sims/verilator
REMOTE_JAVA_ARGS="-Xmx8G -Xss8M -Dsbt.ivy.home=$REMOTE_WORK_DIR/.ivy2 -Dsbt.global.base=$REMOTE_WORK_DIR/.sbt -Dsbt.boot.directory=$REMOTE_WORK_DIR/.sbt/boot"

REMOTE_AWS_WORK_DIR=$CI_AWS_DIR/$CIRCLE_PROJECT_REPONAME-$CIRCLE_BRANCH-$CIRCLE_SHA1
REMOTE_AWS_CHIPYARD_DIR=$REMOTE_AWS_WORK_DIR/chipyard
REMOTE_AWS_FSIM_DIR=$REMOTE_AWS_CHIPYARD_DIR/sims/firesim
REMOTE_AWS_MARSHAL_DIR=$REMOTE_AWS_FSIM_DIR/sw/firesim-software
REMOTE_AWS_RESULTS_DIR=$REMOTE_AWS_FSIM_DEPLOY_DIR/results_workload
REMOTE_AWS_FSIM_DEPLOY_DIR=$REMOTE_AWS_FSIM_DIR/deploy

# local variables (aka within the docker container)
LOCAL_CHECKOUT_DIR=$HOME/project
LOCAL_RISCV_DIR=$HOME/riscv-tools-install
LOCAL_ESP_DIR=$HOME/esp-tools-install
LOCAL_CHIPYARD_DIR=$HOME/chipyard
LOCAL_VERILATOR_DIR=$HOME/verilator
LOCAL_SIM_DIR=$LOCAL_CHIPYARD_DIR/sims/verilator

# key value store to get the build strings
declare -A mapping
mapping["smallboom"]="CONFIG=SmallBoomConfig"
mapping["mediumboom"]="CONFIG=MediumBoomConfig"
mapping["largeboom"]="CONFIG=LargeBoomConfig"
mapping["megaboom"]="CONFIG=MegaBoomConfig"
mapping["boomandrocket"]="CONFIG=SmallBoomAndRocketConfig"
mapping["rv32boom"]="CONFIG=SmallRV32BoomConfig"
mapping["hwachaboom"]="CONFIG=HwachaLargeBoomConfig"
