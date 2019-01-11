#!/bin/bash

# create the different verilator builds of BOOM based on arg

# turn echo on and error on earliest command
set -x
set -e

# enter the verisim directory and build the specific config
cd ../boom-template/verisim
make CONFIG=$1

# remove generated sources to make cache smaller
cd ..
rm -rf project
