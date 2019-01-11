These are the scripts that Circle CI uses to run the tests during a PR.

Note: This uses the most up to date version of riscv-boom/boom-template to run the tests using the
hash of rocket-chip in riscv-boom to specify the version of rocket-chip

Things to look into:
--------------------
- How to build and test MegaBoomConfig (seems to error out saying "Killed/Error 137" which indicates OOM 
since there is only 2GB of RAM per docker instance). So far as I can tell, the only way to fix this is to
get a paid account with better docker instances (or to use machine for now and stop using once machine is a
paid service which might happen soon).

