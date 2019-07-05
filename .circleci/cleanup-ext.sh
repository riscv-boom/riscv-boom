#!/bin/bash

set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# cleanup all folders of the same repo/branch (expects that the ci cancels the prior run of the same branch)
run "rm -rf $REMOTE_WORK_PREFIX-*"
