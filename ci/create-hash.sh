#!/bin/bash

# get the hash of repo and store in file

# turn echo on and error on earliest command
set -x
set -e

# move to top level dir
cd ..
if [ ! -d "/home/circleci/boom-template" ]; then
    # clone boom-template and create the riscv-tools
    git clone --progress --verbose https://github.com/riscv-boom/boom-template.git
fi
cd boom-template

if [ -z $1 ]; then
    git rev-parse HEAD >> ../boom-template.hash
    echo "Hashfile for boom-template created in ..$PWD"
else
    git rev-parse HEAD:$1 >> ../$1.hash
    echo "Hashfile for $1 created in ..$PWD"
fi

