#!/bin/bash

# test the verilator simulation using csmith random testing 

# turn echo on and error on earliest command
set -x
set -e

SIM_BASE=simulator-boom.system-
CONFIG=$1
SIM=${SIM_BASE}${CONFIG}
AMT_RUNS=$2

# run csmith utility
cd ci/csmith
./install-csmith.sh
./run-csmith.sh --sim $HOME/boom-template/verisim/$SIM --run $AMT_RUNS 
