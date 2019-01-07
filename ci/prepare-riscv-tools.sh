#!/bin/bash

# create the riscv tools binaries from riscv-boom/boom-template

# turn echo on and error on earliest command
set -x
set -e

if [ ! -d "/home/circleci/riscv-tools-install" ]; then
    # move to top level dir and remove boom-template if present
    cd ..
    rm -rf boom-template

    # clone boom-template and create the riscv-tools
    git clone --progress --verbose https://github.com/riscv-boom/boom-template.git
    cd boom-template
    ./scripts/build-tools.sh
fi
