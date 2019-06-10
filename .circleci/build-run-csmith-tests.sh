#!/bin/bash

# test the verilator simulation using csmith random testing
# NOTE: uses example project within 'project-template'

# turn echo on and error on earliest command
set -ex

SIM_BASE=simulator-example-
CONFIG=$1
SIM=${SIM_BASE}${CONFIG}
AMT_RUNS=$2

# run csmith utility
cd $HOME/project/util/csmith
./install-csmith.sh
./run-csmith.sh --sim $HOME/bhd/sims/verisim/$SIM --run $AMT_RUNS --nodebug
