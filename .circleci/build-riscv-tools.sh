#!/bin/bash

# create the riscv tools binaries from riscv-boom/boom-template with rocket-chip hash given by riscv-boom

# turn echo on and error on earliest command
set -ex
cd $HOME
if [ ! -d "$HOME/riscv-tools-install" ]; then
    git clone --progress --verbose https://github.com/ucb-bar/project-template.git chipyard
    cd $HOME/chipyard

    echo "Checking out Chipyard version: $(cat $HOME/project/CHIPYARD.hash)"
    git fetch
    git checkout $(cat $HOME/project/CHIPYARD.hash)

    git submodule update --init --recursive toolchains/riscv-tools
    cd toolchains/riscv-tools
    ./build.sh
fi
