Future Work
===========

This chapter lays out some of the potential future directions that BOOM
can be taken. To help facilitate such work, the preliminary design
sketches are described below.

The Rocket Custom Co-processor Interface (ROCC)
-----------------------------------------------

The Rocket in-order processor comes with a ROCC interface that
facilitates communication with co-processor/accelerators. Such
accelerators include crypto units (e.g., SHA3) and vector processing
units (e.g., the open-source Hwacha vector-thread unit[@hwacha]).

The ROCC interface accepts co-processor commands emitted by
[*committed*]{} instructions run on the “Control Processor" (e.g., a
scalar Rocket core). Any ROCC commands [*will*]{} be executed by the
co-processor (barring exceptions thrown by the co-processor); nothing
speculative can be issued over ROCC.

Some ROCC instructions will write back data to the Control Processor’s
scalar register file.

### The Demands of the ROCC Interface

The ROCC interface accepts a ROCC command and up to two register inputs
from the Control Processor’s scalar register file. The ROCC command is
actually the entire RISC-V instruction fetched by the Control Processor
(a “ROCC instruction"). Thus, each ROCC queue entry is at least
2\*XPRLEN + 32 bits in size (additional ROCC instructions may use the
longer instruction formats to encode additional behaviors).

As BOOM does not store the instruction bits in the ROB, a separate data
structure (A “ROCC Reservation Station") will have to hold the
instructions until the ROCC instruction can be committed and the ROCC
command sent to the co-processor.

The source operands will also require access to BOOM’s register file.
Two possibilities are proposed:

-   ROCC instructions are dispatched to the Issue Window, and scheduled
    so that they may access the read ports of the register file once the
    operands are available. The operands are then written into the ROCC
    Reservation Station, which stores the operands and the instruction
    bits until they can be sent to the co-processor. This may require
    significant state.

-   ROCC instructions, when they are committed and sent to the ROCC
    command queue, must somehow access the register file to read out its
    operands. If the register file has dynamically scheduled read ports,
    this may be trivial. Otherwise, some technique to either inject a
    ROCC micro-op into the issue window or a way to stall the issue
    window while ROCC accesses the register file will be needed.

### A Simple One-at-a-Time ROCC Implementation

The simplest way to add ROCC support to BOOM would be to stall
[*Decode*]{} on every ROCC instruction and wait for the ROB to empty.
Once the ROB is empty, the ROCC instruction can proceed down the BOOM
pipeline non-speculatively, and get sent to the ROCC command queue. BOOM
remains stalled until the ROCC accelerator acknowledges the completion
of the ROCC instruction and sends back any data to BOOM’s register file.
Only then can BOOM proceed with its own instructions.

### A High-performance ROCC Implementation Using Two-Phase Commit

While some of the above constraints can be relaxed, the performance of a
decoupled co-processor depends on being able to queue up multiple
commands while the Control Processor runs ahead (prefetching data and
queueing up as many commands as possible). However, this requirement
runs counter to the idea of only sending committed ROCC instructions to
the co-processor.

BOOM’s ROB can be augmented to track [*commit*]{} and
[*non-speculative*]{} pointers. The [*commit*]{} head pointer tracks the
next instruction that BOOM will [*commit*]{}, i.e., the instruction that
will be removed from the ROB and the resources allocated for that
instruction will be de-allocated for use by incoming instructions. The
[*non-speculative*]{} head will track which instructions can no longer
throw an exception and are no longer speculated under a branch (or other
speculative event), i.e., which instructions absolutely will execute and
will not throw a pipeline-retry exception.

This augmentation will allow ROCC instructions to be sent to the ROCC
command queue once they are deemed “non-speculative", but the resources
they allocate will not be freed until the ROCC instruction returns an
acknowledgement. This prevents a ROCC instruction that writes a scalar
register in BOOM’s register file from overwriting a newer instruction’s
writeback value, a scenario that can occur if the ROCC instruction
commits too early, followed by another instruction committing that uses
the same ISA register as its writeback destination.

### The BOOM Custom Co-processor Interface (BOCC)

Some accelerators may wish to take advantage of speculative instructions
(or even out-of-order issue) to begin executing instructions earlier to
maximize de-coupling. Speculation can be handled by either by epoch tags
(if in-order issue is maintained to the co-processor) or by allocating
mask bits (to allow for fine-grain killing of instructions).

The Vector (“V") ISA Extension
------------------------------

Implementing the Vector Extension in BOOM would open up the ability to
leverage performance (or energy-efficiency) improvements in running
data-level parallel codes (DLP). While it would be relatively easy to
add vector arithmetic operations to BOOM, the significant challenges lie
in the vector load/store unit.

Perhaps unexpectedly, a simple but very efficient implementation could
be very small. The smallest possible vector register file (four 64-bit
elements per vector) weighs in at 1024 bytes. A reasonable out-of-order
implementation could support 8 elements per vector and 16 inflight
vector registers (for a total of 48 physical vector registers) which
would only be 3 kilobytes. Following the temporal vector design of the
Cray I, the vector unit can re-use the expensive scalar functional units
by trading off space for time. This also opens up the vector register
file to being implemented using 1 read/1 write ports, fitting it in very
area-efficient SRAMs. As a point of comparison, one of the most
expensive parts of a synthesizable BOOM is its flip-flop based scalar
register file. While a 128-register scalar register file comes in at
1024 bytes, it must be highly ported to fully exploit scalar
instruction-level parallelism (a three-issue BOOM with one FMA unit is 7
read ports and 3 write ports).

The Compressed (“C") ISA Extension
----------------------------------

This section describes how to approach adding the Compressed ISA
Extension to BOOM. The Compressed ISA Extension, or RVC
(<http://riscv.org/download.html#spec_compressed_isa>) enables smaller,
16 bit encodings of common instructions to decrease the static and
dynamic code size. “RVC" comes with a number of features that are of
particular interest to micro-architects:

-   All 16b instructions map directly into a longer 32b instruction.

-   32b instructions have no alignment requirement, and may start on a
    half-word boundary.

BOOM re-uses the front-end design from Rocket, a 5-stage in-order core.
BOOM then takes instructions returning (the [*fetch packet*]{}) from the
Rocket front-end, quickly decodes the instructions for branch
prediction, and pushes the [*fetch packet*]{} into the [*Fetch
Buffer*]{}.

The C Extension provides the following challenges to micro-architects, a
few include:

-   Increased decoding complexity (e.g., operands can now move around).

-   Finding [*where*]{} the instruction begins.

-   Tracking down $+4$ assumptions throughout the code base,
    particularly with branch handling.

-   Unaligned instructions, in particular, running off cache lines and
    virtual pages.

The last point requires some additional “statefulness" in the Fetch
Unit, as fetching all of the pieces of an instruction may take multiple
cycles.

The following describes the proposed implementation strategy of RVC in
BOOM:

-   Implement RVC in the Rocket in-order core. Done properly, BOOM may
    then gain RVC support almost entirely for free (modulo any $+4$
    assumptions in the code base).

-   Move BOOM’s [*Fetch Buffer*]{} into Rocket’s front-end. Rocket will
    need the statefulness to handle wrap-around issues with fetching
    unaligned 32 bit instructions. A non-RVC Rocket core can optionally
    remove this buffer.

-   Expand 16-bit instructions as they enter (or possibly exit) the
    [*Fetch Buffer*]{}.

-   Minimize latency by placing 16b$\rightarrow$32b expanders at every
    half-word start.

### Challenging Implementation Details

There are many challenging corner cases to consider with adding RVC
support to BOOM. First, although all 16 bit encodings map to a 32b
version, [**the behavior of some 16b instructions are different from
their 32b counterparts**]{}! A JAL instruction writes the address of the
following instruction to [rd]{} - but whether that is $PC+2$ or $PC+4$
depends on whether it’s the 16b JAL or a 32b JAL! Likewise, a
mispredicted not-taken branch redirects the fetch unit to $PC+2$ or
$PC+4$ depending on whether the branch was the compressed version or
not. [**Thus, the pipeline must track whether any given instruction was
originally a compressed 16b instruction or not.**]{}

The branch prediction units will also require a careful rethink. The BTB
tracks which instructions are [*predicted-taken*]{} branches and
redirects the PC as desired. For a superscalar [*fetch packet*]{}, the
BTB must help denote which instruction is to be blamed for the taken
prediction to help mask off any invalid instructions that come afterward
within the [*fetch packet*]{}. RVC makes this much more difficult, as
some [*predicted-taken*]{} branches can wrap around fetch
groupings/cache lines/virtual page boundaries. Thus, the “taken"
prediction must be attached to a tag-hit on the [*end*]{} of the branch
instruction. This handles fetching the first part of the branch (and
predicting “not-taken"), then fetching the second part (which hits in
the BTB and predicts “taken"), and only then redirecting the front-end
to the predicted-taken PC target.
