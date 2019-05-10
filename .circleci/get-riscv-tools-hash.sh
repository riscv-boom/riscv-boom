#!/bin/bash

# get the hash of riscv-tools and store in file

# turn echo on and error on earliest command
set -ex

# clone riscv-tools and get its last commit
git clone --progress --verbose https://github.com/freechipsproject/rocket-tools.git riscv-tools
cd riscv-tools
git rev-parse HEAD > $HOME/riscv-tools.hash
rm -rf $HOME/riscv-tools
