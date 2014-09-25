About The Berkeley Out-of-Order RISC-V Processor
================================================

Source repository for the Berkeley Out-of-Order RISC-V processor (BOOM).

This repository only covers the source code to a "BOOM Tile" - the uncore and
surrounding infrastructure will need to be provided.

BOOM depends on the Chisel project. It also depends on Rocket and uncore source
codes.

The Software Story
==================

Currently, BOOM does not support atomics or floating point instructions.
Therefore, when running programs on the RISC-V proxy-kernel (pk) you MUST
compile riscv-pk using ``--disable-atomics".  You can modify the build.sh file
in riscv-tools as follows:


   CC=riscv-gcc build_project riscv-pk --prefix=$RISCV/riscv-elf --host=riscv --disable-atomics

FP Instructions can be trapped and emulated by the proxy kernel, however, when
attempting to compare executions of spike and BOOM, you will want to compile
spike with FPU disabled so that their behaviors are indentical:

   build_project riscv-isa-sim --prefix=$RISCV --with-fesvr=$RISCV --disable-fpu


