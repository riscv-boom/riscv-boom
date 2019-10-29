#!/bin/bash

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

copy $LOCAL_CHECKOUT_DIR/.circleci/firesim-configs $SERVER_AWS:$REMOTE_AWS_FSIM_DEPLOY_DIR
copy $HOME/largefireboom_hwdb.ini $SERVER_AWS:$REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs

cat <<EOF >> $LOCAL_CHECKOUT_DIR/firesim-run-fedora.sh
#!/bin/bash

set -ex

# setup firesim
cd $REMOTE_AWS_FSIM_DIR
source sourceme-f1-manager.sh

# run test
firesim launchrunfarm -c $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/config_runtime_fedora.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/largefireboom_hwdb.ini
firesim infrasetup -c $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/config_runtime_fedora.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/largefireboom_hwdb.ini
timeout -k 4m 40m firesim runworkload -c $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/config_runtime_fedora.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/largefireboom_hwdb.ini
EOF

# execute the script
chmod +x $LOCAL_CHECKOUT_DIR/firesim-run-fedora.sh
run_script_aws $LOCAL_CHECKOUT_DIR/firesim-run-fedora.sh

# copy over results
copy $SERVER_AWS:$REMOTE_AWS_RESULTS_DIR/ $HOME/

# print the results
cat results_workload/*fed-run*/*/uartlog

