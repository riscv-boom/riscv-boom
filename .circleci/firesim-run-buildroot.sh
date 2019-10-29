#!/bin/bash

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

copy $LOCAL_CHECKOUT_DIR/.circleci/firesim-configs $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR
copy $HOME/largefireboom_hwdb.ini $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs

cat <<EOF >> $LOCAL_CHECKOUT_DIR/firesim-run-br.sh
#!/bin/bash

set -ex

# setup firesim
cd $REMOTE_AWS_FSIM_DIR
source sourceme-f1-manager.sh

# run test
firesim launchrunfarm -c $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/config_runtime_buildroot.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/largefireboom_hwdb.ini
firesim infrasetup -c $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/config_runtime_buildroot.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/largefireboom_hwdb.ini
timeout -k 3m 30m firesim runworkload -c $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/config_runtime_buildroot.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/firesim-configs/largefireboom_hwdb.ini
EOF

# execute the script
chmod +x $LOCAL_CHECKOUT_DIR/firesim-run-br.sh
run_script_aws $LOCAL_CHECKOUT_DIR/firesim-run-br.sh

# copy over results
copy $AWS_SERVER:$REMOTE_AWS_RESULTS_DIR/ $HOME/

# print the results
cd $HOME
cat results_workload/*command*/*/uartlog
