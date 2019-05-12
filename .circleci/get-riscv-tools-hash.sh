#!/bin/bash

# get the hash of riscv-tools and store in file

# turn echo on and error on earliest command
set -ex

# clone riscv-tools and get its last commit
cd $HOME
git clone --progress --verbose https://github.com/freechipsproject/rocket-chip.git
cd rocket-chip
git checkout $(cat $HOME/project/ROCKETCHIP_VERSION)
cp riscv-tools.hash $HOME/riscv-tools.hash
rm -rf $HOME/rocket-chip
