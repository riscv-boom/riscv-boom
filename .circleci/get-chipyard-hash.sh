#!/bin/bash

# get the hash of chipyard and store in file

# turn echo on and error on earliest command
set -ex

# clone chipyard and get its last commit
git clone --progress --verbose https://github.com/ucb-bar/project-template.git chipyard
cd chipyard
git checkout rebar-dev

git rev-parse HEAD > $HOME/chipyard-old-rocket.hash

# use rocket-hash that boom uses in ci (to make bumps easier)
git submodule update --init $HOME/chipyard/generators/boom
cat $HOME/chipyard-old-rocket.hash $HOME/chipyard/generators/boom/ROCKETCHIP_VERSION >> chipyard.hash

rm -rf $HOME/chipyard
