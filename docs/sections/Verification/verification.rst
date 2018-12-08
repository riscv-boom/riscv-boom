Verification
============

This chapter covers the current recommended techniques for verifying
BOOM. Although not provided as part of the BOOM or rocket-chip
repositories, it is also recommended that BOOM be tested on “hello-world
+ riscv-pk" and the RISC-V port of Linux to properly stress the
processor.

RISC-V Tests
------------

A basic set of functional tests and micro-benchmarks can be found at
(https://github.com/riscv/riscv-tests). These are invoked by the “make
run" targets in the emulator, fsim, and vsim directories.

RISC-V Torture Tester
---------------------

Berkeley’s riscv-torture tool is used to stress the BOOM pipeline, find
bugs, and provide small code snippets that can be used to debug the
processor. Torture can be found at
(https://github.com/ucb-bar/riscv-torture).
