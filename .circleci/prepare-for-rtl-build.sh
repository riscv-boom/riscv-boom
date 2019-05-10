#!/bin/bash

# build verilator and init submodules with rocket-chip hash given by riscv-boom

# turn echo on and error on earliest command
set -ex

if [ ! -d "$HOME/bhd" ]; then
    cd $HOME

    git clone --progress --verbose https://github.com/ucb-bar/project-template.git bhd
    cd bhd
    git checkout rebar-dev

    # init all submodules (according to what boom-template wants)
    ./scripts/init-submodules-no-riscv-tools.sh

    # move the pull request riscv-boom repo into boom-template
    rm -rf $HOME/bhd/generators/boom
    cp -r $HOME/project $HOME/bhd/generators/boom/

    # get boom specific rocket-chip version
    echo "Checking out rocket-chip with hash: $(cat boom/ROCKETCHIP_VERSION)"
    cd generators/rocket-chip
    git fetch
    git checkout $(cat $HOME/bhd/generators/boom/ROCKETCHIP_VERSION)

    echo "Initialize final submodules"
    git submodule update --init --recursive

    # make boom-template verilator version
    cd $HOME/bhd/sims/verisim
    make verilator_install
fi

