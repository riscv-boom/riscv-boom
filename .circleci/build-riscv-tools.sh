#!/bin/bash

# create the riscv tools binaries from riscv-boom/boom-template with rocket-chip hash given by riscv-boom

# turn echo on and error on earliest command
set -ex

if [ ! -d "$HOME/riscv-tools-install" ]; then

    cd $HOME/riscv-tools
    git submodule update --init --recursive
    ./build-tools.sh
fi
