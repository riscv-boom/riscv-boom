#!/bin/bash

# create the riscv tools binaries from riscv-boom/boom-template with rocket-chip hash given by riscv-boom

# turn echo on and error on earliest command
set -ex

if [ ! -d "$HOME/riscv-tools-install" ]; then

    # clone the boom-template repo
    cd $HOME

    # clone boom-template and create the riscv-tools
    git clone --progress --verbose https://github.com/riscv-boom/boom-template.git
    cd boom-template

    # init all submodules including the tools
    ./scripts/init-submodules.sh

    # build the tools
    ./scripts/build-tools.sh
fi
