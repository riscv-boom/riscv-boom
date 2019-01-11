#!/bin/bash

# build verilator and init submodules with rocket-chip hash given by riscv-boom

# turn echo on and error on earliest command
set -x
set -e

cd ../boom-template

echo "Initialize top-level submodules"
git submodule update --init

echo "Checking out rocket-chip with hash: $(cat boom/ROCKETCHIP_VERSION)"
cd rocket-chip
git fetch
git checkout $(cat ../boom/ROCKETCHIP_VERSION)

echo "Initialize final submodules"
git submodule update --init --recursive hardfloat chisel3 firrtl
cd ../torture
git submodule update --init --recursive

# make boom-template verilator version
cd ../verisim
make verilator_install
