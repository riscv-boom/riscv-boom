#!/bin/bash

# turn echo on and error on earliest command
set -ex

run_manager
    cd $REMOTE_AWS_MARSHAL_DIR
    git checkout MY_HASH_WITH_THE_CAT_PROC_STUFF

    ./marshal build workloads/br-base-test.json
    ./marshal install workloads/br-base-test.json
