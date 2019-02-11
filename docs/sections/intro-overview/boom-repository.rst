The BOOM Repository
====================================

The BOOM repository holds the source code to the BOOM core; it is not a
full processor and thus is **NOT A SELF-RUNNING** repository. To
instantiate a BOOM core, the Rocket-Chip generator found in the
rocket-chip git repository must be used
https://github.com/freechipsproject/rocket-chip, which provides the caches,
uncore, and other needed infrastructure to support a full processor.

The BOOM source code can be found in ``boom/src/main/scala``.

The code structure is shown below:

* ``boom/src/main/scala/``

  * ``bpu/``

    * ``bpd-pipeline.scala``
    * ``bpd/``
      
        * ``br-predictor.scala``
        * ``gshare/``

            * ``gshare.scala``

        * ``simple-predictors/``

            * ``base-only.scala``
            * ``simple-predictors.scala``

        * ``tage/``

            * ``tage.scala``
            * ``tage-table.scala``

    * ``btb/``

        * ``bim.scala``
        * ``btb-sa.scala``
        * ``btb.scala``
        * ``dense-btb.scala``

    * ``misc/``

        * ``2bc-table.scala``

  * ``common/``

    * ``configs.scala``
    * ``consts.scala``
    * ``micro-op.scala``
    * ``package.scala``
    * ``parameters.scala``
    * ``tile.scala``
    * ``types.scala``

  * ``exu/``

    * ``core.scala``
    * ``decode.scala``
    * ``fp-pipeline.scala``
    * ``rob.scala``
    * ``execution-units/``

        * ``execution-units.scala``
        * ``execution-unit.scala``
        * ``functional-unit.scala``
        * ``fpu.scala``
        * ``fdiv.scala``

    * ``issue-units/``

        * ``issue-slot.scala``
        * ``issue-unit-ageordered.scala``
        * ``issue-unit-unordered.scala``
        * ``issue-unit.scala``
        * ``issue-units.scala``

    * ``register-read/``

        * ``func-unit-decode.scala``
        * ``regfile-custom.scala``
        * ``regfile.scala``
        * ``register-read.scala``

    * ``rename/``

        * ``rename-busytable.scala``
        * ``rename-freelist.scala``
        * ``rename-maptable.scala``
        * ``rename-stage.scala``

  * ``ifu/``

    * ``branch-checker.scala``
    * ``fetch-buffer.scala``
    * ``fetch-monitor.scala``
    * ``fetch-control-unit.scala``
    * ``fetch-target-queue.scala``
    * ``frontend.scala``
    * ``icache.scala``
  
  * ``lsu/``

    * ``dcache-shim.scala``
    * ``dcache.scala``
    * ``lsu.scala``
    * ``types.scala``

  * ``system/``

    * ``BoomSubsystem.scala``
    * ``BoomTestSuites.scala``
    * ``Configs.scala``
    * ``ExampleBoomSystem.scala``
    * ``Generator.scala``
    * ``TestHarness.scala``

  * ``util/``

    * ``elastic-reg.scala``
    * ``elastic-sram.scala``
    * ``seqmem-transformable.scala``
    * ``util.scala``
