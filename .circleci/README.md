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

Currently only supports 1 AFI to build and test so that the manager stop procedure works.

Stop procedure:
- When an AFI fails, it will go ahead and stop the instance then stop the CI.
- When all workloads for an AFI have finished, the AFI will stop.

Requirements:
- Add to CircleCI env. var. `$CI_AWS_DIR` point to the "~" of manager instance
- Add SSH key ("firesim.pem") to the CircleCI SSH keys and use the key in the `config.yml` jobs
- Add `$API_TOKEN` to the env. vars in CircleCI - make sure it is the user API token (found in user settings, not in project settings)
- Fill out the following CircleCI env. var.s to access AWS:
    - `$AWS_ACCESS_KEY_ID`
    - `$AWS_SECRET_ACCESS_KEY`
    - `$AWS_DEFAULT_REGION`
- Add your `firesim.pem` into the `$FIRESIM_PEM` CircleCI env. var.
    - Note: This is a single line .pem where all \n's are ,'s


