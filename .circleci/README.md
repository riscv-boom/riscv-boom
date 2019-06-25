These are the scripts that Circle CI uses to run the tests during a PR.

Note: This uses the most up to date version of ucb-bar/project-template (**boom-ci**) to run the tests.
Thus this requires the **boom-ci** branch to have all the changes needed for boom-ci to work.

WARNING: IF **boom-ci** BRANCH IN ucb-bar/project-template IS GONE. CONTACT THE BOOM DEVELOPERS!!!!

Things to look into:
--------------------
- How to build and test MegaBoomConfig (seems to error out saying "Killed/Error 137" which indicates OOM
since there is only 2GB of RAM per docker instance). So far as I can tell, the only way to fix this is to
get a paid account with better docker instances (or to use machine for now and stop using once machine is a
paid service which might happen soon). (Note 1/5/19 BoomConfig seems to suffer from same issue)

* How to get more coverage of pipeline using something like csmith and/or riscv-torture.
