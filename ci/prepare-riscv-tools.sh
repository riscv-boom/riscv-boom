#!/bin/bash

# create the riscv tools from the boom-template clone

set -x #echo on

printenv
cd ..
rm -rf boom-template
git clone --progress --verbose https://github.com/riscv-boom/boom-template.git
cd boom-template
./scripts/build-tools.sh
cd ..
rm -rf boom-template
