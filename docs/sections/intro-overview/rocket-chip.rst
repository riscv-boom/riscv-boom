Rocket-Chip SoC Generator
=========================

.. _boom-chip:
.. figure:: /figures/chip.png
    :alt: BOOM Chip

    A single-core "BOOM-chip", with no L2 last-level cache

As BOOM is just a core, an entire SoC infrastructure must be provided.
BOOM was developed to use the open-source `Rocket-Chip SoC generator <https://github.com/chipsalliance/rocket-chip>`__
The Rocket-Chip generator can instantiate a wide range of SoC designs, including cache-coherent
multi-tile designs, cores with and without accelerators, and chips with or without a last-level shared cache.
BOOM uses the Rocket Chip infrastructure to instantiate it's core/tile complex (tile is a core, L1D/I$, and PTW) instead of a
Rocket tile.

To manage the wide array of actively developed projects that encompass Rocket-Chip, the Rocket-Chip repository
makes heavy use of git submodules. Selected directories of the Rocket-Chip repository are shown below.

* ``rocket-chip/``

  * ``bootrom/`` **Files used to create a boomrom.**
  * ``chisel3/`` **Git submodule pointing to the source code to the Chisel3 language itself.**
  * ``firrtl/`` **Git submodule pointing to the source code to the FIRRTL project.**
  * ``hardfloat/`` **Git submodule pointing to the Berkeley Hardware FPUs.**
  * ``project/`` **SBT voodoo.**
  * ``src/`` **Source code for rocket-chip.**
  * ``torture/`` **Git submodule that points to the RISC-V Torture Test Generator.**

The Rocket Core - a Library of Processor Components!
----------------------------------------------------

Rocket is a 5-stage in-order core that implements the RV64GC ISA and
page-based virtual memory. The original design purpose of the Rocket
core was to enable architectural research into vector co-processors by
serving as the scalar **Control Processor**. Some of that work can be
found at http://hwacha.org.

Rocket has been taped out at least thirteen times in three different
commercial processes, and has been successfully demonstrated to reach
over 1.65 GHz in IBM 45nm SOI. As its namesake suggests,
Rocket is the baseline core for the Rocket-Chip SoC generator. As
discussed earlier, BOOM is instantiated by replacing a Rocket tile with
a BOOM tile.

However, from BOOM’s point of view, Rocket can also be thought of as a
“Library of Processor Components." There are a number of modules created
for Rocket that are also used by BOOM - the functional units, the
caches, the translation look-aside buffers, the page table walker, and
more. Throughout this document you will find references to these
Rocket components and descriptions on how they fit into BOOM.

More information about the Rocket Chip SoC generator and it's source code can be found at http://github.com/chipsalliance/rocket-chip.
