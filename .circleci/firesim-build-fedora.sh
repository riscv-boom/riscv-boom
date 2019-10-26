#!/bin/bash

# turn echo on and error on earliest command
set -ex

run_manager
    cd $REMOTE_AWS_MARSHAL_DIR

    ./marshal build workloads/fedora-test.json
    ./marshal install workloads/fedora-test.json
