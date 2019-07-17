#!/bin/bash

# get the hash of riscv-tools

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# enter bhd repo
cd $HOME

git clone --progress --verbose https://github.com/ucb-bar/chipyard.git chipyard
cd $LOCAL_CHIPYARD_DIR

echo "Checking out Chipyard version: $(cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash)"
git fetch
git checkout $(cat $LOCAL_CHIPYARD_DIR/CHIPYARD.hash)

# get the version of riscv-tools from the git submodule hash
git submodule status | grep "riscv-tools" | awk '{print$1}' | grep -o "[[:alnum:]]*" >> $HOME/riscv-tools.hash
git submodule status | grep "esp-tools" | awk '{print$1}' | grep -o "[[:alnum:]]*" >> $HOME/esp-tools.hash

echo "Hashfile for riscv-tools and esp-tools created in $HOME"
echo "Contents: riscv-tools:$(cat $HOME/riscv-tools.hash)"
echo "Contents: esp-tools:$(cat $HOME/esp-tools.hash)"

rm -rf $LOCAL_CHIPYARD_DIR
