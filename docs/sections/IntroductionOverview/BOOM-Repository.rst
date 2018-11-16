The BOOM Repository
====================================

The BOOM repository holds the source code to the BOOM core; it is not a
full processor and thus is **NOT A SELF-RUNNING** repository. To
instantiate a BOOM core, the Rocket chip generator found in the
rocket-chip git repository must be used
https://github.com/ucb-bar/rocket-chip, which provides the caches,
uncore, and other needed infrastructure to support a full processor.

The BOOM source code can be found in boom/src/main/scala.

The code structure is shown below:

* boom/src/main/scala/

  * bpu/

    * 2bc-table.scala
    * base-only.scala
    * bim.scala
    * bpd-pipeline.scala
    * brpredictor.scala
    * btb-sa.scala
    * btb.scala
    * dense-btb.scala
    * gshare.scala
    * tage.scala
    * tage-table.scala

  * common/

    * configs
    * consts
    * microop
    * package
    * parameters
    * tile
    * types

  * exu/

    * core.scala
    * decode.scala
    * execute.scala
    * execution_units.scala
    * fdiv.scala
    * fppipeline.scala
    * fpu.scala
    * fudecode.scala
    * functional_unit.scala
    * imul.scala
    * issue_ageordered.scala
    * issue.scala
    * issue_slot.scala
    * issue_unordered.scala
    * regfile-custom.scala
    * regfile.scala
    * registerread.scala
    * rename-busytable.scala
    * rename-freelist.scala
    * rename-maptable.scala
    * rename.scala
    * rob.scala

  * ifu/

    * branchchecker.scala
    * fetchbuffer.scala
    * fetchmonitor.scala
    * fetch.scala
    * fetchtargetqueue.scala
    * frontend.scala
    * icache.scala
  
  * lsu/

    * dcacheshim.scala
    * lsu.scala
    * types.scala

  * system/

    * BoomSubsystem.scala
    * BoomTestSuites.scala
    * Configs.scala
    * ExampleBoomSystem.scala
    * Generator.scala
    * TestHarness.scala

  * util/

    * elastic-reg.scala
    * elastic-sram.scala
    * seqmem-transformable.scala
    * util.scala
