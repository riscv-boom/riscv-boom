#!/bin/bash

# Get riscv-tools prebuilt from firesim

# turn echo on and error on earliest command
set -x
set -e

# clone and install
git clone --progress --verbose https://github.com/firesim/firesim-riscv-tools-prebuilt.git
cd firesim-riscv-tools-prebuilt
./installrelease.sh
cp -r distrib $RISCV
