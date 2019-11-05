CI Notes
----------------

These are the scripts that Circle CI uses to run the tests during a PR.

Note: This uses the Chipyard `ucb-bar/chipyard:dev` branch to run CI.

Note: This uses `$SERVER` and `$CI_DIR` which is given in the CircleCI env var setup to specify a server to build on.
To change these variables you must change the project settings of CircleCI.

Note: You also need to add the private key of the build server to CircleCI and match the fingerprint it gives in the config.yml

Things to look into:
--------------------
* How to get more coverage of pipeline using something like csmith and/or riscv-torture.

FireSim CI Notes
----------------

Requirements:
- Need to create a manager instance and in the CircleCI UI add AWS_SERVER with "centos@IP_ADDR"
- Need to add to CircleCI UI the CI_AWS_DIR to point to the "~"
- Need to install `expect` on the manager instance
- Add SSH key ("firesim.pem") to the CircleCI UI and use the key in the `config.yml`
