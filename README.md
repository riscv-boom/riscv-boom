About Berkeley Out-of-Order RISC-V Processor
============================================

Source repository for the Berkeley Out-of-Order RISC-V processor (BOOM).

To use this processor, include this repo as a git submodule and add it as to
your chip's build.scala as a Project, e.g.  lazy val boom = Project("boom",
file("boom"), settings = buildSettings)

BOOM depends on the Chisel project, make sure this library's jars are
installed. It also depends on Rocket source code, so make sure Rocket is in 
the path too.

The Software Story
==================

Currently, BOOM does not support atomics or floating point instructions. Therefore, when running programs on the RISC-V proxy-kernel (pk) you MUST compile riscv-pk using ``--disable-atomics".  You can modify the build.sh file in riscv-tools as follows:


   CC=riscv-gcc build_project riscv-pk --prefix=$RISCV/riscv-elf --host=riscv --disable-atomics

FP Instructions can be trapped and emulated by the proxy kernel, however, when attempting to compare executions of spike and BOOM, you will want to compile spike with FPU disabled so that their behaviors are indentical:

   build_project riscv-isa-sim --prefix=$RISCV --with-fesvr=$RISCV --disable-fpu


