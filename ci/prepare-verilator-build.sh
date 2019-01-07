#!/bin/bash

# create the different verilator builds of BOOM based on arg

# turn echo on and error on earliest command
set -x
set -e

# move to top level dir and remove boom-template if present
cd ..
cd boom-template

# enter the verisim directory and build the specific config
cd verisim
make CONFIG=$1

# remove generated sources to make cache smaller
cd ..
rm -rf project
