#!/bin/bash

set -x #echo on

printenv
cd ..
rm -rf boom-template
git clone --progress --verbose https://github.com/abejgonzalez/boom-template.git #TODO: Change
cd boom-template
./scripts/init-submodules-no-riscv-tools.sh
cp -ir ../riscv-boom boom/
cd verisim
make CONFIG=BoomConfig
cd .. && rm -rf project
