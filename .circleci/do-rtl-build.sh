#!/bin/bash

# create the different verilator builds of BOOM based on arg

# turn echo on and error on earliest command
set -x
set -e

# move the pull request riscv-boom repo into boom-template
rm -rf $HOME/boom-template/boom
cp -r $HOME/project $HOME/boom-template/boom/

# enter the verisim directory and build the specific config
cd $HOME/boom-template/verisim
make CONFIG=$1

# remove generated sources to make cache smaller
cd ..
rm -rf project
