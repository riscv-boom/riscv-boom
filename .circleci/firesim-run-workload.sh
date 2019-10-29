#!/bin/bash

# usage
#  $1 - the name of the afi used
#  $2 - the name of the workload dir in firesim-configs

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

copy $LOCAL_CHECKOUT_DIR/.circleci/firesim-configs/$1/ $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/
copy $LOCAL_CHECKOUT_DIR/.circleci/firesim-configs/$2/ $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/
copy $HOME/$1 $AWS_SERVER:$REMOTE_AWS_FSIM_DEPLOY_DIR/$1

cat <<EOF >> $LOCAL_CHECKOUT_DIR/firesim-run-$1-$2.sh
#!/bin/bash

set -ex

# setup firesim
cd $REMOTE_AWS_FSIM_DIR
source sourceme-f1-manager.sh

# run test                               runtime                                           hwdb                              buildrecipe
firesim launchrunfarm                 -c $REMOTE_AWS_FSIM_DEPLOY_DIR/config_runtime.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/$1 -r $REMOTE_AWS_FSIM_DEPLOY_DIR/config_build_recipes.ini
firesim infrasetup                    -c $REMOTE_AWS_FSIM_DEPLOY_DIR/config_runtime.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/$1 -r $REMOTE_AWS_FSIM_DEPLOY_DIR/config_build_recipes.ini
timeout -k 3m 30m firesim runworkload -c $REMOTE_AWS_FSIM_DEPLOY_DIR/config_runtime.ini -a $REMOTE_AWS_FSIM_DEPLOY_DIR/$1 -r $REMOTE_AWS_FSIM_DEPLOY_DIR/config_build_recipes.ini
EOF

# execute the script
chmod +x $LOCAL_CHECKOUT_DIR/firesim-run-$1-$2.sh
run_script_aws $LOCAL_CHECKOUT_DIR/firesim-run-$1-$2.sh

# copy over results
copy $AWS_SERVER:$REMOTE_AWS_RESULTS_DIR $HOME/$1-$2/

# print the results
cd $HOME
cat $1-$2/results_workload/*/*/uartlog
