#!/bin/bash

# create the different verilator builds of BOOM based on arg

# turn echo on and error on earliest command
set -ex

# this file assumes cache is updated correctly
# copy project to subdir (just in case)
rm -rf $HOME/bhd/generators/boom
cp -r $HOME/project $HOME/bhd/generators/boom

# enter the verisim directory and build the specific config
cd $HOME/bhd/sims/verisim
make clean
make SUB_PROJECT=boom CONFIG=$1 TOP=BoomRocketSystem JAVA_ARGS="-Xmx2500M -Xss8M"
