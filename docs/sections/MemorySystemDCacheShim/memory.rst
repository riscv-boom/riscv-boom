The Memory System and the Data-cache Shim
=========================================

BOOM uses the Rocket non-blocking cache (“Hellacache"). Designed for use
in in-order vector processors, a “shim” is used to connect BOOM to the
data cache. The source code for the cache can be found in
``n``\ bdcache.scala in the Rocket repository
(`github.com/ucb-bar/rocket <github.com/ucb-bar/rocket>`__).

The contract with the cache is that it may execute all memory operations
sent to it (barring structural hazards). As BOOM will send speculative
load instructions to the cache, the shim (``d``\ cacheshim.scala) must
track all “inflight load requests" and their status. If an inflight load
is discovered to be misspeculated, it is marked as such in the shim.
Upon return from the data cache, the load’s response to the pipeline is
suppressed and it is removed from the inflight load queue.

The Hellacache does not ack store requests; the absence of a nack is
used to signal a success.

All memory requests to the Hellacache may be killed the cycle after
issuing the request (while the request is accessing the data arrays).

The current data cache design accesses the SRAMs in a single-cycle.
