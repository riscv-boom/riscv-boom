These are the scripts that Circle CI uses to run the tests during a PR.

Note: This uses the most up to date version of ucb-bar/project-template (**boom-ci**) to run the tests.
Thus this requires the **boom-ci** branch to have all the changes needed for boom-ci to work.

WARNING: IF **boom-ci** BRANCH IN ucb-bar/project-template IS GONE. CONTACT THE BOOM DEVELOPERS!!!!

Note: This uses $SERVER and $CI_DIR which is given in the CircleCI env var setup to specify a server to build on.
To change these variables you must change the project settings of CircleCI.

Note: You also need to add the private key of the build server to CircleCI and match the fingerprint it gives in the config.yml

Things to look into:
--------------------
* How to get more coverage of pipeline using something like csmith and/or riscv-torture.
