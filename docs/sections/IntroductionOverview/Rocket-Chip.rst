The Rocket-chip Repository Layout
====================================

.. _boom-chip:
.. figure:: /figures/chip.png
    :alt: BOOM Chip

    A single-core "BOOM-chip", with no L2 last-level cache

As BOOM is just a core, an entire SoC infrastructure must be provided.
BOOM was developed to use the open-source Rocket-chip SoC generator
https://github.com/freechipsproject/rocket-chip. The Rocket-chip generator
can instantiate a wide range of SoC designs, including cache-coherent
multi-tile designs, cores with and without accelerators, and chips with
or without a last-level shared cache.

To manage the wide array of actively developed projects that encompass
Rocket-chip, the Rocket-chip repository makes heavy use of git
submodules. The directory structure of the Rocket-chip repository is
shown below.

* rocket-chip/

  * boom/ **Git submodule of the  source code for the BOOM core.**
  * chisel **The source code to the Chisel language itself.**
  * firrtl **The source code to the FIRRTL project.**
  * csrc/ **Utility C/C++ source code.**
  * emulator/ **Verilator simulation tools and support.**
  * generated-src/ **Auto-generated Verilog code.**
  * Makefile **Makefile for Verilator simulation.**
  * output/ **Output files from Verilator simulation runs.**
  * riscv-tools/ **Git submodule that points to the RISC-V toolchain.**
  * riscv-tests/ **Source code for benchmarks and tests.**
  * riscv-bmarks/ **Benchmarks written in C.**
  * riscv-tests/ **Tests written in assembly.**
  * Makefrag **The high-level Makefile fragment.**
  * src/ **source code for rocket-chip.**
  * rocket/ **Git submodule of the  source code for the Rocket core (used as a library of processor components).**
  * junctions/ **Git submodule of the  source code for the uncore and off-chip network.**
  * uncore/ **Git submodule of the  source code for the uncore components (including LLC).**
  * sbt/ **Scala voodoo.**
  * vsim/ **The ASIC Verilog simulation and build directories.**

The Rocket Core - a Library of Processor Components!
-------------------------------------------------------------------

Rocket is a 5-stage in-order core that implements the RV64G ISA and
page-based virtual memory. The original design purpose of the Rocket
core was to enable architectural research into vector co-processors by
serving as the scalar **Control Processor**. Some of that work can be
found at http://hwacha.org.

Rocket has been taped out at least thirteen times in three different
commercial processes, and has been successfully demonstrated to reach
over 1.65 GHz in IBM 45 nm SOI. As its namesake suggests,
Rocket is the baseline core for the Rocket-chip SoC generator. As
discussed earlier, BOOM is instantiated by replacing a Rocket tile with
a BOOM tile.

However, from BOOM’s point of view, Rocket can also be thought of as a
“Library of Processor Components." There are a number of modules created
for Rocket that are also used by BOOM - the functional units, the
caches, the translation look-aside buffers, the page table walker, and
more. Thus, throughout this document you will find references to these
Rocket components and descriptions on how they fit into BOOM.

The source code to Rocket can be found at https://github.com/freechipsproject/rocket-chip.


