#!/bin/bash

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

cat <<EOF >> $LOCAL_CHECKOUT_DIR/firesim-br-build.sh
#!/bin/bash

set -ex

cd $REMOTE_AWS_MARSHAL_DIR
./marshal build test/command.json
./marshal install test/command.json
EOF

# execute the script
chmod +x $LOCAL_CHECKOUT_DIR/firesim-br-build.sh
run_script_aws $LOCAL_CHECKOUT_DIR/firesim-br-build.sh

