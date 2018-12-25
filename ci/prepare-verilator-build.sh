#!/bin/bash

# create the different verilator builds of BOOM based on arg

# turn echo on and error on earliest command
set -x
set -e

# move to top level dir and remove boom-template if present
cd ..
rm -rf boom-template

# clone boom-template and init submodules
git clone --progress --verbose https://github.com/riscv-boom/boom-template.git
cd boom-template
./scripts/init-submodules-no-riscv-tools.sh

# move the pull request riscv-boom repo into boom-template
rm -rf boom
mv /home/circleci/project/ boom/

# enter the verisim directory and build the specific config
cd verisim
make CONFIG=$1

# remove generated sources to make cache smaller
cd ..
rm -rf project
