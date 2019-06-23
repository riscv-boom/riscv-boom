#!/bin/bash

# build verilator and init submodules with rocket-chip hash given by riscv-boom

# turn echo on and error on earliest command
set -ex

if [ ! -d "$HOME/chipyard" ]; then
    cd $HOME

    git clone --progress --verbose https://github.com/ucb-bar/project-template.git chipyard
    cd chipyard
    git checkout rebar-dev

    # init all submodules (according to what boom-template wants)
    ./scripts/init-submodules-no-riscv-tools.sh

    # move the pull request riscv-boom repo into boom-template
    rm -rf $HOME/chipyard/generators/boom
    cp -r $HOME/project $HOME/chipyard/generators/boom/

    # get boom specific rocket-chip version
    echo "Checking out rocket-chip with hash: $(cat boom/ROCKETCHIP_VERSION)"
    cd generators/rocket-chip
    git fetch
    git checkout $(cat $HOME/chipyard/generators/boom/ROCKETCHIP_VERSION)

    echo "Initialize final submodules"
    git submodule update --init --recursive

    # TODO: remove this
    # Copy chisel/firrtl of rocketchip into rebar base
    rm -rf $HOME/chipyard/tools/chisel3
    rm -rf $HOME/chipyard/tools/firrtl
    cp -r chisel3 $HOME/chipyard/tools
    cp -r firrtl $HOME/chipyard/tools


    # make boom-template verilator version
    cd $HOME/chipyard/sims/verisim
    make verilator_install
fi

