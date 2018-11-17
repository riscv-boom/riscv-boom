The Rocket Next-line Predictor (NLP)
====================================

BOOM instantiates the Rocket core’s Front-End, which fetches
instructions and predicts every cycle where to fetch the next
instructions. If a misprediction is detected in BOOM’s backend, or
BOOM’s own backing predictor wants to redirect the pipeline in a
different direction, a request is sent to the Front-End and it begins
fetching along a new instruction path.

The next-line predictor (NLP) takes in the current PC being used to
fetch instructions (the *Fetch PC*) and predicts combinationally
where the next instructions should be fetched for the next cycle. If
predicted correctly, there are no pipeline bubbles.

The next-line predictor is an amalgamation of a fully-associative branch
target buffer (BTB), a *gshare* branch history table (BHT), and a
return address stack (RAS) which work together to make a fast, but
reasonably accurate prediction.

NLP Predictions
---------------

The *Fetch PC* first performs a tag match to find a uniquely
matching BTB entry. If a hit occurs, the BTB entry will make a
prediction in concert with the BHT and RAS as to whether there is a
branch, jump, or return found in the *fetch packet* and which
instruction in the *fetch packet* is to blame. The BTB entry also
contains a predicted PC target, which is used as the *Fetch PC* on
the next cycle.

.. _NLP-Predictor-Unit:
.. figure:: /figures/boom_stages.png
    :alt: The Next-line Predictor 

    The Next-line Predictor (NLP) Unit. The Fetch PC scans the BTB’s “PC tags” for a match.
    If a match is found (and the entry is valid), the BHT and RAS are consulted for the final verdict. If the entry
    is a “ret” (return instruction), then the target comes from the RAS. If the entry is a unconditional “jmp”
    (jump instruction), then the BHT is not consulted. The “bidx”, or branch index, marks which instruction
    in a superscalar fetch packet is the cause of the control flow prediction. This is necessary to mask off the
    other instructions in the fetch packet that come over the taken branch

The hysteresis bits (governed by a *gshare* predictor) are only used
on a BTB entry *hit* and if the predicting instruction is a branch.

If the BTB entry contains a *return* instruction, the RAS stack is
used to provide the predicted return PC as the next *Fetch PC*. The
actual RAS management (of when to or the stack) is governed externally.

For area-efficiency, the high-order bits of the PC tags and PC targets
are stored in a compressed file.

NLP Updates
-----------

Each branch passed down the pipeline remembers not only its own PC, but
also its *Fetch PC* (the PC of the head instruction of its *fetch
packet*. [2]_

BTB Updates
^^^^^^^^^^^

The BTB is updated **only** when the Fetch Unit is redirected to
**take** a branch or jump by either the Branch Unit (in the
*Execute* stage) or the Backing Predictor (in the *Branch
Predict* stage). [3]_

If there is no BTB entry corresponding to the taken branch or jump, an
new entry is allocated for it.

BHT Updates
^^^^^^^^^^^

The BHT is composed of two parts that require updates - a *global
history (ghistory)* register and a table of *history counters*.

The  register tracks the outcomes of the last $N$ branches that have
been fetched. It must be updated:

-   in the *Branch Predict* stage - once we have decoded the
    instruction *fetch bundle*, know if any branches are present,
    and which direction the branch predictors have chosen to direct the
    Fetch Unit.

-   in the *Execute* stage - if and only if a *misprediction*
    occurs, the  register must be reset with the correct outcome of the
    branch history.

The *history counter* table is updated when the  register is
updated. Because the counters are read out and passed down the pipeline
with the branch instruction, there is not a problem with having updated
the counters incorrectly in the earlier *Branch Predict* stage. If a
misprediction occurs, the counters will be reset and incremented to the
proper value.

Notice that by updating the history counters in the *Branch Predict*
stage, the updates are being performed in-order! However, it is possible
for a branch to update the *history counters* before later being
found to have been misspeculated under a previous branch. We suspect
that this is a fairly benign scenario. [4]_

RAS Updates
^^^^^^^^^^^

The RAS is updated during the *Branch Predict* stage once the
instructions in the *fetch packet* have been decoded. If the taken
instruction is a call [5]_ , the *Return Address* is onto the RAS. If
the taken instruction is a , then the RAS is .

Superscalar Predictions
^^^^^^^^^^^^^^^^^^^^^^^

When the NLP makes a prediction, it is actually using the BTB to tag
match against the predicted branch’s *Fetch PC*, and not the PC of
the branch itself. The NLP must predict across the entire *fetch
packet* which of the many possible branches will be the dominating
branch that redirects the PC. For this reason, we use a given branch’s
*Fetch PC* rather than its own PC in the BTB tag match. [6]_

.. [1] Unfortunately, the terminology in the literature gets a bit
    muddled here in what to call different types and levels of branch
    predictor. I have seen “micro-BTB" versus “BTB", “NLP" versus “BHT",
    and “cache-line predictor" versus “overriding predictor". Although
    the Rocket code calls its own predictor the “BTB", I have chosen to
    refer to it in documentation as the “next-line predictor", to denote
    that it is a combinational predictor that provides single-cycle
    predictions for fetching “the next line", and the Rocket BTB
    encompasses far more complexity than just a “branch target buffer"
    structure. Likewise, I have chosen the name “backing predictor" as I
    believe it is the most accurate name, while simultaneously avoiding
    being overly descriptive of the internal design (is it a simple BHT?
    Is it tagged? Does it override the NLP?). But in short, I am open
    to better names!

.. [2] In reality, only the very lowest bits must be saved, as the
    higher-order bits will be the same.

.. [3] Rocket’s BTB relies on a little cleverness - when redirecting the
    PC on a misprediction, this new *Fetch PC*  is the same as the
    *Update PC* that needs to be written into a new BTB entry’s
    *Target PC* field. This “coincidence" allows the PC compression
    table to use a single search port - it is simultaneously reading the
    table for the next prediction while also seeing if the new *Update
    PC* already has the proper high-order bits allocated for it.

.. [4] Likewise, the BHT does not keep track of a *commit copy* of
    the  register. This means that any sort of exceptions or pipeline
    replays will leave the  register in an incoherent state. However,
    experiments showed that this had no noticeable effect on performance
    on real benchmarks. This is probably because the BHT’s  register is
    fairly small and can quickly re-learn the history in only a few
    cycles.

.. [5] While RISC-V does not have a dedicated instruction, it can be
    inferred by checking for a or instruction with a writeback
    destination to (aka, the ).

.. [6] Each BTB entry corresponds to a single *Fetch PC*, but it is
    helping to predict across an entire *fetch packet*. However, the
    BTB entry can only store meta-data and target-data on a single
    control-flow instruction. While there are certainly pathological
    cases that can harm performance with this design, the assumption is
    that there is a correlation between which branch in a *fetch
    packet* is the dominating branch relative to the *Fetch PC*,
    and - at least for narrow fetch designs - evaluations of this design
    has shown it is very complexity-friendly with no noticeable loss in
    performance. Some other designs instead choose to provide a whole
    bank of BTBs for each possible instruction in the *fetch
    packet*.


