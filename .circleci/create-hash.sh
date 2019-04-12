#!/bin/bash

# get the hash of riscv-tools or boom-template and store in file

# turn echo on and error on earliest command
set -ex

# only execute if boom-template is not present (if it is present then it should have run this command before already)
if [ ! -d "$HOME/boom-template" ]; then
    cd $HOME

    # clone boom-template and create the riscv-tools
    git clone --progress --verbose https://github.com/riscv-boom/boom-template.git
    cd boom-template
fi

# move the pull request riscv-boom repo into boom-template
rm -rf $HOME/boom-template/boom
cp -r $HOME/project $HOME/boom-template/boom/


cd $HOME/boom-template
if [ $1 == "boom-template" ]; then
    git rev-parse HEAD >> $HOME/$1.hash
    echo "Hashfile for $1 created in $HOME"
elif [ $1 == "riscv-tools" ]; then
    # Use riscv-boom rocket-chip hash to specify version of rocket-chip to use
    git submodule update --init rocket-chip
    echo "Checking out rocket-chip with hash: $(cat boom/ROCKETCHIP_VERSION)"
    cd rocket-chip
    git fetch
    git checkout $(cat ../boom/ROCKETCHIP_VERSION)
    git submodule update --recursive

    # rocket-chip now provides their own hash for the tools
    cp riscv-tools.hash $HOME/$1.hash
    echo "Hashfile for $1 created in $HOME"
fi

