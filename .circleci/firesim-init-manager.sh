#!/bin/bash

# init manager with chipyard, setup firesim

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# clear folders older than 30 days
run_script_aws $LOCAL_CHECKOUT_DIR/.circleci/clean-old-files.sh $CI_AWS_DIR

# call clean on exit
trap clean_aws EXIT

clean_aws

# create a script to run
cat <<EOF >> $LOCAL_CHECKOUT_DIR/firesim-manager-setup.sh
#!/bin/bash

set -ex

# get chipyard
mkdir -p $REMOTE_AWS_WORK_DIR
cd $REMOTE_AWS_WORK_DIR
git clone --progress --verbose https://github.com/ucb-bar/chipyard.git $REMOTE_AWS_CHIPYARD_DIR
cd $REMOTE_AWS_CHIPYARD_DIR
echo "Checking out Chipyard version: $(cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash)"
git fetch
git checkout $(cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash)

# setup repo
./scripts/init-submodules-no-riscv-tools.sh
./scripts/firesim-setup.sh --fast

# setup firesim
cd $REMOTE_AWS_FSIM_DIR
source sourceme-f1-manager.sh

# use expect to send newlines to managerinit (for some reason heredoc errors on email input)
/bin/expect << EXP
set timeout -1
spawn firesim managerinit
send -- "\r"
send -- "\r"
send -- "\r"
send -- "\r"
send -- "\r"
expect eof
EXP

# remove boom so it can get added properly
rm -rf $REMOTE_AWS_CHIPYARD_DIR/generators/boom
EOF

# TODO: MAKE SURE YOU GET RIGHT FIREMARSHAL HASH
# git checkout -C $REMOTE_AWS_MARSHAL_DIR MY_HASH_WITH_THE_CAT_PROC_STUFF"

# execute the script
chmod +x $LOCAL_CHECKOUT_DIR/firesim-manager-setup.sh
run_script_aws $LOCAL_CHECKOUT_DIR/firesim-manager-setup.sh

# add checkout boom to repo
copy $LOCAL_CHECKOUT_DIR $AWS_SERVER:$REMOTE_AWS_CHIPYARD_DIR/generators/boom/
