The BOOM Repository
====================================

The BOOM repository holds the source code to the BOOM core; it is not a
full processor and thus is **NOT A SELF-RUNNING** repository. To
instantiate a BOOM core, the Rocket chip generator found in the
rocket-chip git repository must be used
(<https://github.com/ucb-bar/rocket-chip>), which provides the caches,
uncore, and other needed infrastructure to support a full processor.

The BOOM source code can be found in `boom/src/main/scala`.

The code structure is shown below:

-   `boom/src/main/scala`/

    -   bpd\_pipeline.scala [ branch prediction stage.]{}

    -   brpredictor.scala [ abstract branch predictor.]{}

    -   configs.scala [ BOOM configurations. ]{}

    -   consts.scala [ constant definitions. ]{}

    -   core.scala [ the top-level of the processor core.]{}

    -   dcacheshim.scala [ the shim between the the core and the
        dcache.]{}

    -   decode.scala [ decode stage.]{}

    -   execute.scala [ high-level execution units (made up of
        FUs).]{}

    -   fpu.scala [ floating point unit.]{}

    -   functional\_unit.scala [ low-level functional units.]{}

    -   gshare.scala [ gshare branch predictor.]{}

    -   imul.scala [ integer multiplier.]{}

    -   issue\_ageordered.scala [ age-ordered (collasping-queue) issue
        window implementation.]{}

    -   issue.scala [ abstract issue window.]{}

    -   issue\_slot.scala [ An issue window slot.]{}

    -   issue\_unordered.scala [ un-ordered issue window
        implementation.]{}

    -   lsu.scala [ load/store unit.]{}

    -   package.scala [ ]{}

    -   parameters.scala [ knobs/parameters.]{}

    -   prefetcher.scala [ data prefetcher.]{}

    -   regfile.scala [ register file.]{}

    -   registerread.scala [ registerRead stage and bypassing.]{}

    -   rename.scala [ register renaming logic.]{}

    -   rob.scala [ re-order buffer.]{}

    -   tile.scala [ top-level tile.]{}

    -   util.scala [ utility code.]{}

