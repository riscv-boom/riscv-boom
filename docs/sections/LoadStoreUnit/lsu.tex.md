The Load/Store Unit (LSU) {#sec:lsu}
=========================

The Load/Store Unit is responsible for deciding when to fire memory
operations to the memory system. There are three queues: the Load
Address Queue (LAQ), the Store Address Queue (SAQ), and the Store Data
Queue (SDQ). Load instructions generate a “uopLD" micro-op. When issued,
“uopLD" calculates the load address and places its result in the LAQ.
Store instructions (may) generate [*two*]{} micro-ops, “uopSTA" (Store
Address Generation) and “uopSTD" (Store Data Generation). The STA
micro-op calculates the store address and places its result in the SAQ
queue. The STD micro-op moves the store data from the register file to
the SDQ. Each of these micro-ops will issue out of the [*Issue
Window*]{} as soon their operands are ready. See Section
\[sec:storeuops\] for more details on the store micro-op specifics.

Store Instructions
------------------

Entries in the Store Queue[^1] are allocated in the [*Decode*]{} stage
(the appropriate bit in the [stq\_entry\_val]{} vector is set). A
“valid" bit denotes when an entry in the SAQ or SDQ holds a valid
address or data ([saq\_val]{} and [sdq\_val]{} respectively). Once a
store instruction is committed, the corresponding entry in the Store
Queue is marked as committed. The store is then free to be fired to the
memory system at its convenience. Stores are fired to the memory in
program order.

### Store Micro-ops {#sec:storeuops}

Stores are inserted into the issue window as a single instruction (as
opposed to being broken up into separate [addr-gen]{} and [data-gen]{}
micro-ops). This prevents wasteful usage of the expensive issue window
entries and extra contention on the issue port to the LSU. A store in
which both operands are ready can be issued to the LSU as a single
micro-op which provides both the address and the data to the LSU. While
this requires store instructions to have access to two register file
read ports, this is motivated by a desire to not cut performance in half
on store-heavy code. Sequences involving stores to the stack should
operate at IPC=1!

However, it is common for store addresses to be known well in advance of
the store data. Store addresses should be moved to the SAQ as soon as
possible to allow later loads to avoid any memory ordering failures.
Thus, the issue window will emit uopSTA or uopSTD micro-ops as required,
but retain the remaining half of the store until the second operand is
ready.

Load Instructions
-----------------

Entries in the Load Queue (LAQ) are allocated in the [*Decode*]{} stage
([laq\_entry\_val]{}). In [*Decode*]{}, each load entry is also given a
[*store mask*]{} ([laq\_st\_mask]{}), which marks which stores in the
Store Queue the given load depends on. When a store is fired to memory
and leaves the Store Queue, the appropriate bit in the [*store mask*]{}
is cleared.

Once a load address has been computed and placed in the LAQ, the
corresponding [*valid*]{} bit is set ([laq\_val]{}).

Loads are optimistically fired to memory on arrival to the LSU (getting
loads fired early is a huge benefit of out–of–order pipelines).
Simultaneously, the load instruction compares its address with all of
the store addresses that it depends on. If there is a match, the memory
request is killed. If the corresponding store data is present, then the
store data is [*forwarded*]{} to the load and the load marks itself as
having [*succeeded*]{}. If the store data is not present, then the load
goes to [*sleep*]{}. Loads that have been put to sleep are retried at a
later time.[^2]

The BOOM Memory Model
---------------------

Currently, as of October 2016, the RISC-V memory model is underspecified
and will take some time to settle on an exact specification. However,
the current RISC-V specification describes as [*relaxed consistency*]{}
model [@Gharachorloo:1990:MCE:325096.325102] in which stores and loads
may be freely re-ordered.

BOOM currently exhibits the following behavior:

1.  Write-&gt;Read constraint is relaxed (newer loads may execute before
    older stores).

2.  Read-&gt;Read constraint is currently relaxed (loads to the same
    address may be reordered).

3.  A thread can read its own writes early.

### Ordering Loads to the Same Address

The RISC-V memory model is expected to strengthen the requirement that
loads to the same address be ordered.[^3] This requires loads to search
against other loads for potential address conflicts. If a younger load
executes before an older load with a matching address, the younger load
must be replayed and the instructions after it in the pipeline flushed.
However, this scenario is only required if a cache coherence probe event
snooped the core’s memory, exposing the reordering to the other threads.
If no probe events occurred, the load re-ordering may safely occur.

Memory Ordering Failures
------------------------

The Load/Store Unit has to be careful regarding
[store$\rightarrow$load]{} dependences. For the best performance, loads
need to be fired to memory as soon as possible.

> `s`w x1 $\rightarrow$ 0(x2)
>
> ld x3 $\leftarrow$ 0(x4)

However, if [x2]{} and [x4]{} reference the same memory address, then
the load in our example [*depends*]{} on the earlier store. If the load
issues to memory before the store has been issued, the load will read
the wrong value from memory, and a [*memory ordering failure*]{} has
occurred. On an ordering failure, the pipeline must be flushed and the
rename map tables reset. This is an incredibly expensive operation.

To discover ordering failures, when a store commits, it checks the
entire LAQ for any address matches. If there is a match, the store
checks to see if the load has [*executed*]{}, and if it got its data
from memory or if the data was forwarded from an older store. In either
case, a memory ordering failure has occurred.

See Figure \[fig:lsu\] for more information about the Load/Store Unit.

[^1]: When I refer to the [*Store Queue*]{}, I really mean both the SAQ
    and SDQ.

[^2]: Higher-performance processors will track [*why*]{} a load was put
    to sleep and wake it up once the blocking cause has been alleviated.

[^3]: Technically, a [*fence.r.r*]{} could be used to provide the
    correct execution of software on machines that reorder dependent
    loads. However, there are two reasons for an ISA to disallow
    re-ordering of dependent loads: 1) no other popular ISA allows this
    relaxation, and thus porting software to RISC-V could face extra
    challenges, and 2) cautious software may be too liberal with the
    appropriate [*fence*]{} instructions causing a slow-down in
    software. Thankfully, enforcing ordered dependent loads may not
    actually be very expensive. For one, load addresses are likely to be
    known early - and are probably likely to execute in-order anyways.
    Second, misordered loads are only a problem in the cache of a cache
    coherence probe, so performance penalty is likely to be negligible.
    The hardware cost is also negligible - loads can use the same CAM
    search port on the LAQ that stores must already use. While this may
    become an issue when supporting one load and one store address
    calculation per cycle, the extra CAM search port can either be
    mitigated via banking or will be small compared to the other
    hardware costs required to support more cache bandwidth.
