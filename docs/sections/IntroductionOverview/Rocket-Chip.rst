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
submodules. Selected directories of the Rocket-chip repository is
shown below.

* :code:`rocket-chip/`

  * :code:`bootrom/` **Files used to create a boomrom.**
  * :code:`chisel3/` **Git submodule pointing to the source code to the Chisel3 language itself.**
  * :code:`emulator/` **The Verilator simulation tools and support directories.**
  * :code:`firrtl/` **Git submodule pointing to the source code to the FIRRTL project.**
  * :code:`hardfloat/` **Git submodule pointing to the Berkeley Hardware FPUs.** 
  * :code:`project/` **SBT voodoo.** 
  * :code:`regression/` **Used for TravisCI regression testing.**
  * :code:`riscv-tools/` **Git submodule that points to the RISC-V toolchain.**
  * :code:`src/` **Source code for rocket-chip.**
  * :code:`torture/` **Git submodule that points to the RISC-V Torture Test Generator.**
  * :code:`vsim/` **The VCS ASIC Verilog simulation and build directories.**

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


