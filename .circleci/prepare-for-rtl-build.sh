#!/bin/bash

# build verilator and init submodules with rocket-chip hash given by riscv-boom

# turn echo on and error on earliest command
set -ex

if [ ! -d "$HOME/boom-template" ]; then
    # clone the boom-template repo
    cd $HOME

    # clone boom-template and create the riscv-tools
    git clone --progress --verbose https://github.com/riscv-boom/boom-template.git
    cd boom-template

    # init all submodules (according to what boom-template wants)
    ./scripts/init-submodules-no-riscv-tools.sh

    # move the pull request riscv-boom repo into boom-template
    rm -rf $HOME/boom-template/boom
    cp -r $HOME/project $HOME/boom-template/boom/

    # get boom specific rocket-chip version
    echo "Checking out rocket-chip with hash: $(cat boom/ROCKETCHIP_VERSION)"
    cd rocket-chip
    git fetch
    git checkout $(cat ../boom/ROCKETCHIP_VERSION)

    echo "Initialize final submodules"
    git submodule update --init --recursive

    # extra patches
    # TODO: Remove FIRRTL patch in next rocket-bump (fixes const prop issue)
    git -C firrtl checkout 9535e03020c6e654dae3ce7e95f4d8649405ce3d

    # make boom-template verilator version
    cd ../verisim
    make verilator_install
fi
