#!/bin/bash

# create the different verilator builds of BOOM based on arg

# turn echo on and error on earliest command
set -x
set -e

# move to top level dir and remove boom-template if present
cd ..
cd boom-template
./scripts/init-submodules-no-riscv-tools.sh

# move the pull request riscv-boom repo into boom-template
rm -rf boom
mv /home/circleci/project/ boom/

# enter the verisim directory and build verilator
cd verisim
make verilator_install
