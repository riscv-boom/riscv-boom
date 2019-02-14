Introduction and Overview
=========================

The goal of this document is to describe the design and implementation
of the Berkeley Out–of–Order Machine (BOOM).

BOOM is heavily inspired by the MIPS R10k and the Alpha 21264
out–of–order processors. Like the R10k and the
21264, BOOM is a unified physical register file design (also known as
“explicit register renaming").

The source code to BOOM can be found at https://github.com/riscv-boom/riscv-boom.

.. toctree::
    :maxdepth: 2
    :caption: Introduction and Overview:

    boom-pipeline
    riscv-isa
    chisel
    quick-start
    boom-repository
    rocket-chip
    quick-build-overview
