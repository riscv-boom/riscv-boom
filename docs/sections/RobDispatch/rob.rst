The Reorder Buffer (ROB) and the Dispatch Stage
===============================================

The ROB tracks the state of all inflight instructions in the pipeline.
The role of the ROB is to provide the illusion to the programmer that
his program executes in-order. After instructions are decoded and
renamed, they are then dispatched to the ROB and the issue window and
marked as *busy*. As instructions finish execution, they inform the ROB
and are marked *not busy*. Once the “head" of the ROB is no longer busy,
the instruction is *committed*, and it’s architectural state now
visible. If an exception occurs and the excepting instruction is at the
head of the ROB, the pipeline is flushed and no architectural changes
that occurred after the excepting instruction are made visible. The ROB
then redirects the PC to the appropriate exception handler.

The ROB Organization
--------------------

The ROB is, conceptually, a circular buffer that tracks all inflight
instructions in-order. The oldest instruction is pointed to by the
*commit head*, and the newest instruction will be added at the *rob
tail*.

To facilitate superscalar *Dispatch* and *Commit*, the ROB is
implemented as a circular buffer with :math:`W` banks (where :math:`W`
is the *dispatch* and *commit* width of the machine [1]_). This
organization is shown in Figure [fig:rob].

At *dispatch*, up to :math:`W` instructions are written from the *fetch
packet* into an ROB row, where each instruction is written to a
different bank across the row. As the instructions within a *fetch
packet* are all consecutive (and aligned) in memory, this allows a
single PC to be associated with the entire *fetch packet* (and the
instruction’s position within the *fetch packet* provides the low-order
bits to its own PC). While this means that branching code will leave
bubbles in the ROB, it makes adding more instructions to the ROB very
cheap as the expensive costs are amortized across each ROB row.

ROB State
---------

Each ROB entry contains relatively little state:

-  is entry valid?

-  is entry busy?

-  is entry an exception?

-  branch mask (which branches is this entry still speculated under?

-  rename state (what is the logical destination and the stale physical
   destination?)

-  floating-point status updates

-  other miscellaneous data (e.g., helpful for statistic tracking)

The PC and the branch prediction information is stored on a per-row
basis (see Sec [sec:pcstorage]). The Exception State only tracks the
oldest known excepting instruction (see Sec [sec:rob\_xcpt]).

Exception State
~~~~~~~~~~~~~~~

The ROB tracks the oldest excepting instruction. If this instruction
reaches the head of the ROB, then an exception is thrown.

Each ROB entry is marked with a single-bit to signify whether or not the
instruction has encountered exceptional behavior, but the additional
exception state (e.g., the bad virtual address and the exception cause)
is only tracked for the oldest known excepting instruction. This saves
considerable state by not storing this on a per entry basis.

PC Storage
~~~~~~~~~~

The ROB must know the PC of every inflight instruction. This information
is used in the following situations:

-  Any instruction could cause an exception, in which the “exception pc"
   (epc) must be known.

-  Branch and jump instructions need to know their own PC for for target
   calculation.

-  Jump-register instructions must know both their own PC **and the PC
   of the following instruction** in the program to verify if the
   front-end predicted the correct JR target.

This information is incredibly expensive to store. Instead of passing
PCs down the pipeline, branch and jump instructions access the ROB’s “PC
File" during the *Register-read* stage for use in the Branch Unit. Two
optimizations are used:

-  only a single PC is stored per ROB row. [2]_

-  the PC File is stored in two banks, allowing a single read-port to
   read two consecutive entries simultaneously (for use with JR
   instructions).

The Commit Stage
----------------

When the instruction at the *commit head* is no longer busy (and it is
not excepting), it may be *committed*, i.e., its changes to the
architectural state of the machine are made visible. For superscalar
commit, the entire ROB row is analyzed for *not busy* instructions (and
thus, up to the entire ROB row may be committed in a single cycle). The
ROB will greedily commit as many instructions as it can per row to
release resource as soon as possible. However, the ROB does not
(currently) look across multiple rows to find commit-able instructions.

Only once a store has been committed may it be sent to memory. For
superscalar committing of stores, the LSU is told “how many stores" may
be marked as committed. The LSU will then drain the committed stores to
memory as it sees fit.

When an instruction (that writes to a register) commits, it then frees
the *stale physical destination register*. The *stale pdst* is then free
to be re-allocated to a new instruction.

Exceptions and Flushes
----------------------

Exceptions are handled when the instruction at the *commit head* is
excepting. The pipeline is then flushed and the ROB emptied. The rename
map tables must be reset to represent the true, non-speculative
*committed* state. The front-end is then directed to the appropriate PC.
If it is an architectural exception, the excepting instruction’s PC
(referred to as the *exception vector*) is sent to the Control/Status
Register file. If it is a micro-architectural exception (e.g., a
load/store ordering misspeculation) the failing instruction is refetched
and execution can begin anew.

Parameterization - Rollback versus Single-cycle Reset
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The behavior of resetting the map tables is parameterizable. The first
option is to rollback the ROB one row per cycle to unwind the rename
state (this is the behavior of the MIPS
R10k:raw-latex:`\cite{mipsr10k}`). For each instruction, the *stale
physical destination* register is written back into the map table for
its *logical destination* specifier.

A faster single-cycle reset is available. This is accomplished by using
another rename snapshot that tracks the *committed* state of the rename
tables. This *committed map table* is updated as instructions
commit. [3]_

Causes
~~~~~~

The RV64G ISA provides relatively few exception sources:

    Load/Store Unit
        - page faults

    Branch Unit
        - misaligned fetches

    Decode Stage
        - all other exceptions and interrupts can be handled before the
        instruction is dispatched to the ROB

Note that memory ordering speculation errors also originate from the
Load/Store Unit, and are treated as exceptions in the BOOM pipeline
(actually they only cause a pipeline “retry").

.. [1]
   This design sets up the *Dispatch* and *Commit* widths of BOOM to be
   the same. However, that is not necessarily a fundamental constraint,
   and it would be possible to orthogonalize the *Dispatch* and *Commit*
   widths, just with more added control complexity.

.. [2]
   Because instructions within an ROB row are consecutive in the
   program, the instruction’s ROB bank implicitly provides the lower PC
   bits.

.. [3]
   The tradeoff here is between longer latencies on exceptions versus an
   increase in area and wiring.
