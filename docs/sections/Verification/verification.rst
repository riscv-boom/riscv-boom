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

Continuous Integration (CI)
---------------------------

The CircleCI Continuous Integration (CI) tool is used to check pull requests and
the master branch of riscv-boom. All files associated with it can be found in
two directories. Firstly, the configuration file used to run CI is located at
riscv-boom/.circleci/config.yml. This specifies the current tests and builds that
are run using which BOOM configurations. Additionally, the DockerFile used to 
build the CI docker images resides in riscv-boom/.circleci/images. Finally, all
scripts that are used during the CI run are located at riscv-boom/ci. Note that even
though boom-template is cloned during the CI process, the riscv-boom repository specifies
which version of rocket-chip to use (which in turn determines the proper version of
riscv-tools).

Current Configurations Tested:

* BoomConfig
* SmallBoomConfig
* MediumBoomConfig

Current Tests Used:

* riscv-tools assembly tests
* riscv-tools benchmarks
* scala style checks 
