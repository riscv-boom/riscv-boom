#!/bin/bash

# create the riscv tools binaries from riscv-boom/boom-template with rocket-chip hash given by riscv-boom

# turn echo on and error on earliest command
set -x
set -e

if [ ! -d "$HOME/riscv-tools-install" ]; then

    cd $HOME/boom-template

    echo "Initialize top-level submodules"
    git submodule update --init

    echo "Checking out rocket-chip with hash: $(cat boom/ROCKETCHIP_VERSION)"
    cd rocket-chip
    git fetch
    git checkout $(cat ../boom/ROCKETCHIP_VERSION)

    echo "Initializing riscv-tools"
    git submodule update --init riscv-tools
    cd riscv-tools
    git submodule update --init --recursive riscv-isa-sim riscv-fesvr riscv-pk riscv-opcodes riscv-tests riscv-gnu-toolchain riscv-openocd

    # We need to build a RV64G toolchain (not RVC which is the current riscv-tools default).
    # Therefore, let's make our own build script and then invoke it.
    f=build-rv64g.sh

    echo "Build RV tools"
    if [[ ! -e "$f" ]]
    then
        # If file doesn't exist, generate a new build script file.
        echo "Generating script: rocket-chip/riscv-tools/$f"
        echo "#! /bin/bash" >> $f
        echo "#" >> $f
        echo "# Script to build RISC-V ISA simulator, proxy kernel, and GNU toolchain." >> $f
        echo "# Tools will be installed to \$RISCV." >> $f
        echo "" >> $f
        echo ". build.common" >> $f
        echo "" >> $f
        echo "echo "Starting RISC-V Toolchain build process"" >> $f
        echo "echo "Tools will be installed to \$RISCV."" >> $f
        echo "" >> $f
        echo "build_project riscv-fesvr --prefix=\$RISCV" >> $f
        echo "build_project riscv-isa-sim --prefix=\$RISCV --with-fesvr=\$RISCV --with-isa=rv64imafd" >> $f
        echo "build_project riscv-gnu-toolchain --prefix=\$RISCV --with-arch=rv64imafd" >> $f
        echo "CC= CXX= build_project riscv-pk --prefix=\$RISCV --host=riscv64-unknown-elf" >> $f
        echo "build_project riscv-openocd --prefix=\$RISCV --enable-remote-bitbang --enable-jtag_vpi --disable-werror" >> $f
        echo "build_project riscv-tests --prefix=\$RISCV/riscv64-unknown-elf" >> $f
        echo "" >> $f
        echo "echo \"RISC-V Toolchain installation completed!\"" >> $f
    else
        echo "Using existing $f script."
    fi

    chmod a+x $f && echo "./$f" && ./$f
    retVal=$?
    if [ ! $retVal -eq 0 ]; then
        echo "A error has been encountered while building the toolchain."
    fi
    exit $retVal
fi
