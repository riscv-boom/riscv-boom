Instruction Fetch
=================

.. _fetch-unit:
.. figure:: /figures/frontend.png
    :alt: The Fetch Unit

    The Fetch Unit. The grey box is the front-end instantiated from the Rocket code base.


BOOM instantiates the Rocket core’s *Front-end* (highlighted in grey
in :numref:`fetch-unit` , which fetches instructions and predicts every
cycle where to fetch the next instructions using a “next-line predictor"
(NLP). If a misprediction is detected in BOOM’s backend, or BOOM’s own
predictor wants to redirect the pipeline in a different direction, a
request is sent to the Front-End and it begins fetching along a new
instruction path. See :ref:`Branch Prediction` for more information on
how branch prediction fits into the Fetch Unit’s pipeline.

Since superscalar fetch is supported, the *Front-end* returns a
*fetch packet* of instructions. The *fetch packet* also contains
meta-data, which includes a *valid mask* (which instructions in the
packet are valid?) and some branch prediction information that is used
later in the pipeline.

The Rocket I-Cache
------------------

BOOM instantiates the i-cache found in the Rocket processor source code.
The i-cache is a virtually indexed, physically tagged set-associative
cache.

To save power, the i-cache reads out a fixed number of bytes (aligned)
and stores the instruction bits into a register. Further instruction
fetches can be managed by this register. The i-cache is only fired up
again once the fetch register has been exhausted (or a branch prediction
directs the PC elsewhere).

The i-cache does not (currently) support fetching across cache-lines,
nor does it support fetching unaligned relative to the superscalar fetch
address. [1]_

The i-cache does not (currently) support hit-under-miss. If an icache
miss occurs, the icache will not accept any further requests until the
miss has been handled. This is less than ideal for scenarios in which
the pipeline discovers a branch mispredict and would like to redirect
the icache to start fetching along the correct path.

Fetching Compressed Instructions
--------------------------------
This section describes how the RISC-V Compressed ISA extension
was implemented in BOOM. The Compressed ISA Extension, or RVC
(http://riscv.org/download.html#spec_compressed_isa) enables smaller, 16
bit encodings of common instructions to decrease the static and dynamic
code size. “RVC" comes with a number of features that are of particular
interest to micro-architects:

-  32b instructions have no alignment requirement, and may start on a
   half-word boundary.

-  All 16b instructions map directly into a longer 32b instruction.

BOOM re-uses the front-end design from Rocket, a 5-stage in-order core.
BOOM then takes instructions returning (the *fetch packet*) from the
Rocket front-end, quickly decodes the instructions for branch
prediction, and pushes the *fetch packet* into the *Fetch Buffer*.

-  Increased decoding complexity (e.g., operands can now move around).

-  Finding *where* the instruction begins.

-  Removing :math:`+4` assumptions throughout the code base,
   particularly with branch handling.

-  Unaligned instructions, in particular, running off cache lines and
   virtual pages.

The last point requires some additional “statefulness" in the Fetch
Unit, as fetching all of the pieces of an instruction may take multiple
cycles.

The following describes the implementation of RVC in BOOM by describing
the lifetime of a instruction.

-  The front-end returns fetchpackets of fetchWidth*16 bits wide. This
   was supported inherently in the Rocket front-end

-  Maintain statefulness in F3, in the cycle where fetchbundles are
   dequeued from the ICache response queue and enqueued onto the
   fetch buffer

-  F3 tracks the trailing 16b, PC, and instruction boundaries of the
   last fetchbundle. These bits are combined with the current
   fetchbundle and expanded to fetchWidth*32 bits for enqueuing onto the
   fetch buffer. Predecode determines the start address of every
   instruction in this fetch bundle and masks the fetch packet for the
   fetch buffer

-  The fetch buffer now compacts away invalid, or misaligned
   when storing to its memory.

The following section describes miscellaneous implementation details.

-  RVC decode is performed by expanding RVC instructions using Rocket's
   RVCExpander in the normal Decode stage

-  A challenging problem is dealing with instructions that cross a
   fetch boundary. We track these instructions as belonging to the
   fetch packet that contains their higher-order 16 bits. We have to
   be careful when determining the PC of these instructions, by tracking
   all instructions which were initially misaligned across a fetch
   boundary.

-  The pipeline must also track whether an instruction was originally
   16b or 32b, for calculating PC+4 or PC+2.

The Fetch Buffer
----------------

*Fetch packets* coming from the i-cache are placed into a *Fetch
Buffer*. The *Fetch Buffer* helps to decouple the instruction
fetch front-end from the execution pipeline in the back-end.

The *Fetch Buffer* is parameterizable. The number of entries can be
changed and whether the buffer is implemented as a “flow-through"
queue [2]_ or not can be toggled.

.. [1] This constraint is due to the fact that a cache-line is not stored
    in a single row of the memory bank, but rather is striped across a
    single bank to match the refill size coming from the uncore.
    Fetching unaligned would require modification of the underlying
    implementation, such as banking the i-cache such that consecutive
    chunks of a cache-line could be accessed simultaneously.

.. [2] A flow-through queue allows entries being enqueued to be
    immediately dequeued if the queue is empty and the consumer is
    requesting (the packet “flows through" instantly).
