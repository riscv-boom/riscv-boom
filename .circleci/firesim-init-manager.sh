#!/bin/bash

# -------------------------------------------------------------
# init manager with chipyard and firesim as a library
# do firesim managerinit (don't need to do it in future setups)
# add version of boom (override older version)
# -------------------------------------------------------------

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

# clear folders older than 30 days
run_script_aws $SCRIPT_DIR/clean-old-files.sh $CI_AWS_DIR

SCRIPT_NAME=firesim-manager-setup.sh

# create a script to run
cat <<EOF >> $LOCAL_CHECKOUT_DIR/$SCRIPT_NAME
#!/bin/bash

set -ex

# get chipyard
mkdir -p $REMOTE_AWS_WORK_DIR
cd $REMOTE_AWS_WORK_DIR
rm -rf $REMOTE_AWS_CHIPYARD_DIR
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

# TODO: get the right firemarshal hash
# git checkout -C $REMOTE_AWS_MARSHAL_DIR $HASH_WITH_SPEC_COREMARK"

# execute the script
chmod +x $LOCAL_CHECKOUT_DIR/$SCRIPT_NAME
run_script_aws $LOCAL_CHECKOUT_DIR/$SCRIPT_NAME

# add checkout boom to repo
copy $LOCAL_CHECKOUT_DIR/ $AWS_SERVER:$REMOTE_AWS_CHIPYARD_DIR/generators/boom/
