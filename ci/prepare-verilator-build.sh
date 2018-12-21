#!/bin/bash

# create the verilator build of BoomConfig

set -x #echo on

printenv
cd ..
rm -rf boom-template
git clone --progress --verbose https://github.com/riscv-boom/boom-template.git
cd boom-template
./scripts/init-submodules-no-riscv-tools.sh
cp -ir ../riscv-boom boom/
cd verisim
make CONFIG=BoomConfig
cd .. && rm -rf project
