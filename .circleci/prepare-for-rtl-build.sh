#!/bin/bash

# build verilator and init submodules with rocket-chip hash given by riscv-boom

# turn echo on and error on earliest command
set -x
set -e

cd $HOME/boom-template

# Same steps as ./scripts/init-submodules-no-riscv-tools.sh
# Main difference is that you checkout the rocket-chip version
echo "Initialize top-level submodules"
git submodule update --init

# move the pull request riscv-boom repo into boom-template
rm -rf $HOME/boom-template/boom
cp -r $HOME/project $HOME/boom-template/boom/

echo "Checking out rocket-chip with hash: $(cat boom/ROCKETCHIP_VERSION)"
cd rocket-chip
git fetch
git checkout $(cat ../boom/ROCKETCHIP_VERSION)

echo "Initialize final submodules"
git submodule update --init --recursive
cd ../torture
git submodule update --init --recursive

# make boom-template verilator version
cd ../verisim
make verilator_install
