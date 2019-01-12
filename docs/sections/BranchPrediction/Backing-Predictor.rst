The Backing Predictor (BPD)
===========================

When the next-line predictor (NLP) is predicting well, the processor’s
backend is provided an unbroken stream of instructions to execute. The
NLP is able to provide fast, single-cycle predictions by being expensive
(in terms of both area and power), very small (only a few dozen branches
can be remembered), and very simple (the *gshare* hysterisis bits
are not able to learn very complicated or long history patterns).

To capture more branches and more complicated branching behaviors, BOOM
provides support for a “Backing Predictor", or BPD (see 
:numref:`backing-predictor-unit`).


The BPD’s goal is to provide very high accuracy in a (hopefully) dense
area. To make this possible, the BPD will not make a prediction until
the *fetch packet* has been decoded and the branch targets computed
directly from the instructions themselves. This saves on needing to
store the *PC tags* and *branch targets* within the BPD [7]_.

The BPD is accessed in parallel with the instruction cache access (see
:numref:`Fetch-Unit`). This allows the BPD to be stored in sequential
memory (i.e., SRAM instead of flip-flops). With some clever
architecting, the BPD can be stored in single-ported SRAM to achieve the
density desired.

Making Predictions
------------------

When making a prediction, the backing predictor must provide the
following:

-   is a prediction being made?

-   a bit-vector of taken/not-taken predictions

As per the first bullet-point, the BPD may decide to not make a
prediction. This may be because the predictor uses tags to inform
whether its prediction is valid or there may be a structural hazard that
prevented a prediction from being made.

The BPD provides a bit-vector of taken/not-taken predictions, the size
of the bit-vector matching the *fetch width* of the pipeline (one
bit for each instruction in the *fetch packet*). The *Branch
Prediction* stage will decode the instructions in the *fetch
packet*, compute the branch targets, and decide in conjunction with
the BPD’s prediction bit-vector if a front-end redirect should be made.

.. _backing-predictor-unit:
.. figure:: /figures/bpd.png
    :alt: The Backing Branch Predictor 

    The Backing Branch Predictor (BPD) Unit. The front-end sends the “next PC” (npc) to
    the BPD (BP0 stage). A hash of the npc and the global history is used to index the predictor tables. The
    predictor tables (ideally stored in SRAM) are accessed in parallel with the instruction cache (BP1 stage).
    The BPD then returns a prediction for each instruction in the fetch packet. The instructions returning from
    the instruction cache are quickly decoded; any branches that are predicted as taken will redirect the front-
    end from the BP2 stage. Prediction snapshots and metadata are stored in the branch rename snapshots (for
    fixing the predictor after mispredictions) and the Branch Re-order Buffer (for updating the predictor in the
    Commit stage).

Jump and Jump-Register Instructions
-----------------------------------

The BPD makes predictions only on the direction (taken versus not-taken)
of conditional branches. Non-conditional “jumps" and “jump-register"
instructions are handled separately from the BPD. [8]_

The NLP learns any “taken" instruction’s *PC* and *target PC* -
thus, the NLP is able to predict jumps and jump-register instructions.

If the NLP does not make a prediction on a  instruction, the pipeline
will redirect the Fetch Unit in the *Fetch2 Stage* (see
:numref:`Fetch-Unit`). [9]_

Jump-register instructions that were not predicted by the NLP will be
sent down the pipeline with no prediction made. As  instructions require
reading the register file to deduce the jump target, there’s nothing
that can be done if the NLP does not make a prediction.

Updating the Backing Predictor
------------------------------

Generally speaking, the BPD is updated during the *Commit* stage.
This prevents the BPD from being polluted by wrong-path
information. [10]_ However, as the BPD makes use of global history, this
history must be reset whenever the Fetch Unit is redirected. Thus, the
BPD must also be (partially) updated during *Execute* when a
misprediction occurs to reset any speculative updates that had occurred
during the *Fetch* stages.

When making a prediction, the BPD passes to the pipeline a “response
info packet". This “info packet" is stored in a “branch re-order buffer"
(BROB) until commit time. [11]_ Once all of the instructions
corresponding to the “info packet" is committed, the “info packet" is
set to the BPD (along with the eventual outcome of the branches) and the
BPD is updated. :ref:`The Branch Reorder Buffer (BROB)` covers the BROB, which handles the
snapshot information needed for update the predictor during
*Commit*. :ref:`Rename Snapshot State` covers the BPD Rename
Snapshots, which handles the snapshot information needed to update the
predictor during a misspeculation in the *Execute* stage.

Managing the Global History Register
------------------------------------

The *global history register* is an important piece of a branch
predictor. It contains the outcomes of the previous :math:`N` branches (where
:math:`N` is the size of the global history register). [12]_

When fetching branch :math:`i`, it is important that the direction of the
previous :math:`i-N` branches is available so an accurate prediction can be
made. Waiting till the *Commit* stage to update the global history
register would be too late (dozens of branches would be inflight and not
reflected!). Therefore, the global history register must be updated
*speculatively*, once the branch is fetched and predicted in the
*BP2* stage.

If a misprediction occurs, the global history register must be reset and
updated to reflect the actual history. This means that each branch (more
accurately, each *fetch packet*) must snapshot the global history
register in case of a misprediction. [13]_

There is one final wrinkle - exceptional pipeline behavior. While each
branch contains a snapshot of the global history register, any
instruction can potential throw an exception that will cause a front-end
redirect. Such an event will cause the global history register to become
corrupted. For exceptions, this may seem acceptable - exceptions should
be rare and the trap handlers will cause a pollution of the global
history register anyways (from the point of view of the user code).
However, some exceptional events include “pipeline replays" - events
where an instruction causes a pipeline flush and the instruction is
refetched and re-executed. [14]_ For this reason, a *commit copy* of
the global history register is also maintained by the BPD and reset on
any sort of pipeline flush event.

The Branch Reorder Buffer (BROB)
--------------------------------

The Reorder Buffer (see :ref:`The Reorder Buffer (ROB) and the Dispatch Stage`) maintains a record of
all inflight instructions. Likewise, the Branch Reorder Buffer (BROB)
maintains a record of all inflight branch predictions. These two
structure are decoupled as BROB entries are *incredibly* expensive
and not all ROB entries will contain a branch instruction. As only
roughly one in every six instructions is a branch, the BROB can be made
to have fewer entries than the ROB to leverage additional savings.

Each BROB entry corresponds to a single superscalar branch prediction.
Said another way, there is a 1:1 correspondence between a single fetch
cycle’s prediction and a BROB entry. For each prediction made, the
branch predictor packs up data that it will need later to perform an
update. For example, a branch predictor will want to remember what
*index* a prediction came from so it can update the counters at that
index later. This data is stored in the BROB.

When the last instruction in a fetch group is committed, the BROB entry
is deallocated and returned to the branch predictor. Using the data
stored in the BROB entry, the branch predictor can perform any desired
updates to its prediction state.

There are a number of reasons to update the branch predictor after
*Commit*. It is crucial that the predictor only learns *correct*
information. In a data cache, memory fetched from a wrong path execution
may eventually become useful when later executions go to a different
path. But for a branch predictor, wrong path updates encode information
that is pure pollution – it takes up useful entries by storing
information that is not useful and will never be useful. Even if later
iterations do take a different path, the history that got it there will
be different. And finally, while caches are fully tagged, branch
predictors use partial tags (if any) and thus suffer from deconstructive
aliasing.

Of course, the latency between *Fetch* and *Commit* is
inconvenient and can cause extra branch mispredictions to occur if
multiple loop iterations are inflight. However, the BROB could be used
to bypass branch predictions to mitigate this issue. Currently, this
bypass behavior is not supported in BOOM.

The BROB is broken up into two parts: the prediction *data* and the
branch execution *metadata*. The metadata tracks which instructions
within the fetch packet where branches, which direction they took, and
which branches were mispredicted (this requires random access). The
prediction data is written once into the BROB upon instruction
*Dispatch* and read out (and deallocated) during *Commit*.

Rename Snapshot State
---------------------

The BROB holds branch predictor data that will be needed to update the
branch predictor during *Commit* (for both correct and incorrect
predictions). However, there is additional state needed for when the
branch predictor makes an incorrect prediction *and must be updated
immediately*. For example, if a misprediction occurs, the
speculatively-updated global history must be reset to the correct value
before the processor can begin fetching (and predicting) again.

This state can be very expensive but it can be deallocated once the
branch is resolved in the *Execute* stage. Therefore, the state is
stored in parallel with the *Rename Snapshots*. During *Decode*
and *Rename*, a branch tag is allocated to each branch and a
snapshot of the rename tables are made to facilitate single-cycle
rollback if a misprediction occurs. Like the branch tag and rename
maptable snapshots, the corresponding branch predictor “rename” snapshot
can be deallocated once the branch is resolved by the Branch Unit in
*Execute*.

The Abstract Branch Predictor Class
-----------------------------------

To facilitate exploring different global history-based BPD designs, an
abstract “BrPredictor" class is provided. It provides a standard
interface into the BPD, the control logic for managing the global
history register, and contains the *branch reorder buffer (BROB)*
(which handles the inflight branch prediction checkpoints). This
abstract class can be found in :numref:`backing-predictor-unit` labeled “predictor
(base)”.

Global History
^^^^^^^^^^^^^^

As discussed in :ref:`Managing the Global History Register`, global history is a vital
piece of any branch predictor. As such, it is handled by the abstract
BranchPredictor class. Any branch predictor extending the abstract
BranchPredictor class gets access to global history without having to
handle snapshotting, updating, and bypassing.

Very Long Global History (VLHR)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Some branch predictors (see :ref:`The TAGE Predictor`) require access to
incredibly long histories – over a thousand bits. Global history is
speculatively updated after each prediction and must be snapshotted and
reset if a misprediction was made. Snapshotting a thousand bits is
untenable. Instead, VLHR is implemented as a circular buffer with a
speculative head pointer and a commit head pointer. As a prediction is
made, the prediction is written down at :math:`VLHR[spec\_head]` and the
speculative head pointer is incremented and snapshotted. When a branch
mispredicts, the head pointer is reset to :math:`snapshot+1` and the correct
direction is written to :math:`VLHR[snapshot]`. In this manner, each snapshot
is on the order of 10 bits, not 1000 bits.

Operating System-aware Global Histories
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Although the data on its benefits are preliminary, BOOM does support
OS-aware global histories. The normal global history tracks all
instructions from all privilege levels. A second *user-only global
history* tracks only user-level instructions.

The Two-bit Counter Tables
--------------------------

The basic building block of most branch predictors is the “Two-bit
Counter Table” (2BC). As a particular branch is repeatedly taken, the
counter saturates upwards to the max value 3 (*0b11*) or *strongly
taken*. Likewise, repeatedly not-taken branches saturate towards zero
(*0b00*). The high-order bit specifies the *prediction* and the
low-order bit specifies the *hysteresis* (how “strong” the
prediction is).

.. _gshare-predictor:
.. figure:: /figures/2bc-prediction.png
    :scale: 30 %
    :align: center
    :alt: The GShare Predictor 

    A gshare predictor uses the global history hashed with the PC to index into a table of 2-bit
    counters. The high-order bit makes the prediction.

These two-bit counters are aggregated into a table. Ideally, a good
branch predictor knows which counter to index to make the best
prediction. However, to fit these two-bit counters into dense SRAM, a
change is made to the 2bc finite state machine – mispredictions made in
the *weakly not-taken* state move the 2bc into the *strongly
taken* state (and vice versa for *weakly taken* being
mispredicted). The FSM behavior is shown in :numref:`two-bit-fsm`.

Although it’s no longer strictly a “counter", this change allows us to
separate out the read and write requirements on the *prediction* and
*hystersis* bits and place them in separate sequential memory
tables. In hardware, the 2bc table can be implemented as follows:

The P-bit:

* **read** - every cycle to make a prediction

* **write** - only when a misprediction occurred (the value of
  the h-bit).

The H-bit:

* **read** - only when a misprediction occurred.

* **write** - when a branch is resolved (write the direction the
  branch took).

.. _two-bit-fsm:
.. figure:: /figures/2bc-fsm.png
    :scale: 30 %
    :align: center
    :alt: The Two-bit counter state machine 
    
    The Two-bit counter state machine 

By breaking the high-order p-bit and the low-order h-bit apart, we can
place each in 1 read/1 write SRAM. A few more assumptions can help us do
even better. Mispredictions are rare and branch resolutions are not
necessarily occurring on every cycle. Also, writes can be delayed or
even dropped altogether. Therefore, the *h-table* can be implemented
using a single 1rw-ported SRAM by queueing writes up and draining them
when a read is not being performed. Likewise, the *p-table* can be
implemented in 1rw-ported SRAM by banking it – buffer writes and drain
when there is not a read conflict.

A final note: SRAMs are not happy with a “tall and skinny” aspect ratio
that the 2bc tables require. However, the solution is simple – tall and
skinny can be trivially transformed into a rectangular memory structure.
The high-order bits of the index can correspond to the SRAM row and the
low-order bits can be used to mux out the specific bits from within the
row.

The GShare Predictor
--------------------

*Gshare* is a simple but very effective branch predictor.
Predictions are made by hashing the instruction address and the global
history (typically a simple XOR) and then indexing into a table of
two-bit counters. :numref:`Gshare-Predictor` shows the logical
architecture and :numref:`gshare-predictor-pipeline` shows the physical implementation
and structure of the *gshare* predictor. Note that the prediction
begins in the BP0 stage when the requesting address is sent to the
predictor but that the prediction is made later in the BP2 stage once
the instructions have returned from the instruction cache and the
prediction state has been read out of the *gshare*’s p-table.

.. _gshare-predictor-pipeline:
.. figure:: /figures/gshare.png
    :alt: The GShare predictor pipeline 

    The GShare predictor pipeline 

The TAGE Predictor
------------------

.. _tage-predictor:
.. figure:: /figures/tage.png
    :alt: The TAGE Predictor 

    The TAGE predictor. The requesting address (PC) and the global history are fed into each
    table’s index hash and tag hash. Each table provides its own prediction (or no prediction) and the table
    with the longest history wins.

BOOM also implements the TAGE conditional branch predictor. TAGE is a
highly-parameterizable, state-of-the-art global history
predictor. The design is able to
maintain a high degree of accuracy while scaling from very small
predictor sizes to very large predictor sizes. It is fast to learn short
histories while also able to learn very, very long histories (over a
thousand branches of history).

TAGE (TAgged GEometric) is implemented as a collection of predictor
tables. Each table entry contains a *prediction counter*, a
*usefulness counter*, and a ***tag***. The *prediction counter*
provides the prediction (and maintains some hysteresis as to how
strongly biased the prediction is towards taken or not-taken). The
*usefulness counter* tracks how useful the particular entry has been
in the past for providing correct predictions. The *tag* allows the
table to only make a prediction if there is a tag match for the
particular requesting instruction address and global history.

Each table has a different (and geometrically increasing) amount of
history associated with it. Each table’s history is used to hash with
the requesting instruction address to produce an index hash and a tag
hash. Each table will make its own prediction (or no prediction, if
there is no tag match). The table with the longest history making a
prediction wins.

On a misprediction, TAGE attempts to allocate a new entry. It will only
overwrite an entry that is “not useful” (`ubits == 0`).

TAGE Global History and the Circular Shift Registers (CSRs) [15]_
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Each TAGE table has associated with it its own global history (and each
table has geometrically more history than the last table). As the
histories become incredibly long (and thus too expensive to snapshot
directly), TAGE uses the Very Long Global History Register (VLHR) as
described in :ref:`Very Long Global History (VLHR)`. The histories contain many more bits
of history that can be used to index a TAGE table; therefore, the
history must be “folded” to fit. A table with 1024 entries uses 10 bits
to index the table. Therefore, if the table uses 20 bits of global
history, the top 10 bits of history are XOR’ed against the bottom 10
bits of history.

Instead of attempting to dynamically fold a very long history register
every cycle, the VLHR can be stored in a circular shift register (CSR).
The history is stored already folded and only the new history bit and
the oldest history bit need to be provided to perform an update. 
:numref:`tage-csr` shows an example of how a CSR works.

.. _tage-csr:
.. code-block:: none
    :caption: The circular shift register. When a new branch outcome is added, the register is shifted (and wrapped around). The new outcome is added and the oldest bit in the history is “evicted”.

    Example:   
      A 12 bit value (0b_0111_1001_1111) folded onto a 5 bit CSR becomes 
      (0b_0_0010), which can be found by:                                       
                                                                                 
                                                                                 
                   /-- history[12] (evict bit)                                   
                   |                                                             
     c[4], c[3], c[2], c[1], c[0]                                                
      |                        ^                                                 
      |                        |                                                 
      \_______________________/ \---history[0] (newly taken bit)                 
                                                                                 
                                                                                 
    (c[4] ^ h[ 0] generates the new c[0]).                                        
    (c[1] ^ h[12] generates the new c[2]).       

Each table must maintain *three* CSRs. The first CSR is used for
computing the index hash and has a size :math:`n=log(num\_table\_entries)`. As
a CSR contains the folded history, any periodic history pattern matching
the length of the CSR will XOR to all zeroes (potentially quite common).
For this reason, there are two CSRs for computing the tag hash, one of
width :math:`n` and the other of width :math:`n-1`.

For every prediction, all three CSRs (for every table) must be
snapshotted and reset if a branch misprediction occurs. Another three
*commit copies* of these CSRs must be maintained to handle pipeline
flushes.

Usefulness counters (u-bits)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The “usefulness” of an entry is stored in the *u-bit* counters.
Roughly speaking, if an entry provides a correct prediction, the u-bit
counter is incremented. If an entry provides an incorrect prediction,
the u-bit counter is decremented. When a misprediction occurs, TAGE
attempts to allocate a new entry. To prevent overwriting a useful entry,
it will only allocate an entry if the existing entry has a usefulness of
zero. However, if an entry allocation fails because all of the potential
entries are useful, then all of the potential entries are decremented to
potentially make room for an allocation in the future.

To prevent TAGE from filling up with only useful but rarely-used
entries, TAGE must provide a scheme for “degrading” the u-bits over
time. A number of schemes are available. One option is a timer that
periodically degrades the u-bit counters. Another option is to track the
number of failed allocations (incrementing on a failed allocation and
decremented on a successful allocation). Once the counter has saturated,
all u-bits are degraded.

TAGE Snapshot State
^^^^^^^^^^^^^^^^^^^

For every prediction, all three CSRs (for every table) must be
snapshotted and reset if a branch misprediction occurs. TAGE must also
remember the index of each table that was checked for a prediction (so
the correct entry for each table can be updated later). Finally, TAGE
must remember the tag computed for each table – the tags will be needed
later if a new entry is to be allocated. [16]_

Other Predictors
----------------

BOOM provides a number of other predictors that may provide useful.

The Null Predictor
^^^^^^^^^^^^^^^^^^

The Null Predictor is used when no BPD predictor is desired. It will
always predict “not taken".

The Random Predictor
^^^^^^^^^^^^^^^^^^^^

The Random Predictor uses an LFSR to randomize both “was a prediction
made?" and “which direction each branch in the *fetch packet* should
take?". This is very useful for both torturing-testing BOOM and for
providing a worse-case performance baseline for comparing branch
predictors.

.. [7] It’s the *PC tag* storage and *branch target* storage that
    makes the BTB within the NLP so expensive.

.. [8]  instructions jump to a :math:`PC+Immediate` location, whereas
     instructions jump to a :math:`PC+Register[rs1]+Immediate` location.

.. [9] Redirecting the Fetch Unit in the *Fetch2 Stage* for
     instructions is trivial, as the instruction can be decoded and its
    target can be known.

.. [10] In the data-cache, it can be useful to fetch data from the wrong
    path- it is possible that future code executions may want to access
    the data. Worst case, the cache’s effective capacity is reduced. But
    it can be quite dangerous to add wrong-path information to the BPD -
    it truly represents a code-path that is never exercised, so the
    information will *never* be useful in later code executions.
    Worst, aliasing is a problem in branch predictors (at most partial
    tag checks are used) and wrong-path information can create
    deconstructive aliasing problems that worsens prediction accuracy.
    Finally, bypassing of the inflight prediction information can occur,
    eliminating any penalty of not updating the predictor until the
    *Commit* stage.

.. [11] These *info packets* are not stored in the ROB for two
    reasons - first, they correspond to *fetch packets*, not
    instructions. Second, they are very expensive and so it is
    reasonable to size the BROB to be smaller than the ROB.

.. [12] Actually, the direction of all conditional branches within a
    *fetch packet* are compressed (via an OR-reduction) into a
    single bit, but for this section, it is easier to describe the
    history register in slightly inaccurate terms.

.. [13] Notice that there is a delay between beginning to make a
    prediction in the *BP0* stage (when the global history is read)
    and redirecting the front-end in the *BP2* stage (when the
    global history is updated). This results in a “shadow” in which a
    branch beginning to make a prediction in *BP0* will not see the
    branches (or their outcomes) that came a cycle (or two) earlier in
    the program (that are currently in *BP1* or *BP2* stages).
    It is vitally important though that these “shadow branches” be
    reflected in the global history snapshot.

.. [14] An example of a pipeline replay is a *memory ordering
    failure* in which a load executed before an older store it
    depends on and got the wrong data. The only recovery requires
    flushing the entire pipeline and re-executing the load.

.. [15] No relation to the Control/Status Registers.

.. [16] There are ways to mitigate some of these costs, but this margin
    is too narrow to contain them.
