#!/bin/bash

# get the hash of riscv-tools or boom-template and store in file

# turn echo on and error on earliest command
set -ex

# clone the boom-template repo
cd $HOME

# clone boom-template and create the riscv-tools
git clone --progress --verbose https://github.com/riscv-boom/boom-template.git
cd boom-template

cd $HOME/boom-template
if [ $1 == "boom-template" ]; then
    git rev-parse HEAD >> $HOME/$1.hash
    cat $HOME/project/ROCKETCHIP_VERSION $HOME/$1.hash > $HOME/$1.hash
    echo "Hashfile for $1 created in $HOME (note: combined with rocket-chip hash)"
elif [ $1 == "riscv-tools" ]; then
    # Use riscv-boom rocket-chip hash to specify version of rocket-chip to use
    git submodule update --init rocket-chip
    echo "Checking out rocket-chip with hash: $(cat $HOME/project/ROCKETCHIP_VERSION)"
    cd rocket-chip
    git fetch
    git checkout $(cat $HOME/project/ROCKETCHIP_VERSION)
    git submodule update --recursive

    # rocket-chip now provides their own hash for the tools
    cp riscv-tools.hash $HOME/$1.hash
    echo "Hashfile for $1 created in $HOME"
fi

# delete boom-template
rm -rf $HOME/boom-template
