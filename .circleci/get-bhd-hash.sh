#!/bin/bash

# get the hash of bhd and store in file

# turn echo on and error on earliest command
set -ex

# clone bhd and get its last commit
git clone --progress --verbose https://github.com/ucb-bar/project-template.git bhd
cd bhd
git checkout rebar-dev
git rev-parse HEAD > $HOME/bhd.hash
rm -rf $HOME/bhd
