#!/bin/bash

# build verilator and init submodules with rocket-chip hash given by riscv-boom

# turn echo on and error on earliest command
set -ex

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

# TODO: Remove FIRRTL patch in next rocket-bump (fixes const prop issue)
git -C firrtl checkout 9535e03020c6e654dae3ce7e95f4d8649405ce3d

cd ../torture
git submodule update --init --recursive

# make boom-template verilator version
cd ../verisim
make verilator_install
