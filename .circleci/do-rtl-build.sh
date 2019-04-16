#!/bin/bash

# create the different verilator builds of BOOM based on arg

# turn echo on and error on earliest command
set -ex

# this file assumes cache is updated correctly

# TODO: this is redundant but safe
# move the pull request riscv-boom repo into boom-template
rm -rf $HOME/boom-template/boom
cp -r $HOME/project $HOME/boom-template/boom/

# enter the verisim directory and build the specific config
cd $HOME/boom-template/verisim
make clean
make CONFIG=$1 JAVA_ARGS="-Xmx2G -Xss8M"

# remove generated sources to make cache smaller
cd ..
rm -rf project
