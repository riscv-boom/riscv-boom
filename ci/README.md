These are the scripts that Circle CI uses to run the tests during a PR.

Note: This uses the most up to date version of riscv-boom/boom-template to run the tests

Things to look into:
--------------------
- This does not test changes across multiple sub-modules of boom-template. For ex, if rocket-chip
also needs to be updated, then this will not reflect those changes unless boom-template has those
changes already put in

- How to build and test MegaBoomConfig (seems to error out saying "Killed/Error 137" which indicates OOM 
since there is only 2GB of RAM per docker instance). So far as I can tell, the only way to fix this is to
get a paid account with better docker instances (or to use machine for now and stop using once machine is a
paid service which might happen soon). (Note 1/5/19 BoomConfig seems to suffer from same issue)

