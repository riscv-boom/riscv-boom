Micro-architectural Event Tracking
==================================

Version 1.9.1 of the RISC-V Privileged Architecture adds support for
Hardware Performance Monitor (HPM) counters.[^1] The HPM support allows
a nearly infinite number of micro-architectural events to be multiplexed
onto up to 29 physical counters. The privilege access levels can also be
set on a per-counter basis.

   Number                Event
  -------- ---------------------------------
     1            Committed Branches
     2      Committed Branch Mispredictions
    ...     and many more (see core.scala)

  : Uarch Events

\[table:uarchcounters\]

The available events can be modified in [core.scala]{} as desired.[^2]
It is then up to machine-level software to set the privilege access
level and the [*event selectors*]{} for each counter as desired.

    static void mstatus_init()
    {
       // Enable user/supervisor use of perf counters
       write_csr(mucounteren, -1);
       write_csr(mscounteren, -1);

       // set events 1 and 2 to counters 3 and 4.
       write_csr(mhpmevent3, 1);
       write_csr(mhpmevent4, 2);

\[ref:code\_hpm\_init\]

Reading HPM Counters in Software
--------------------------------

The Code Example \[ref:code\_uarch\] demonstrates how to read the value
of any CSR register from software.

    #define read_csr_safe(reg) ({ register long __tmp asm("a0"); \   
      asm volatile ("csrr %0, " #reg : "=r"(__tmp)); \               
      __tmp; })             
      
      long csr_cycle  = read_csr_safe(cycle);
      long csr_instr  = read_csr_safe(instret);
      long csr_hpmc3  = read_csr_safe(hpmcounter3);
      ...
      long csr_hpmc31 = read_csr_safe(hpmcounter31);
      

\[ref:code\_uarch\]

[^1]: Future efforts may add some counters into a memory-mapped access
    region. This will open up the ability to track events that, for
    example, may not be tied to any particular core (like last-level
    cache misses).

[^2]: Note: [io.events(0)]{} will get mapped to being [Event \#1]{}
    (etc.) from the point of view of the RISC-V software.
