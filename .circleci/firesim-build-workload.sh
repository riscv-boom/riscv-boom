#!/bin/bash

# usage get the workload from
#  $1 - firemarshall folder
#  $2 - name of json workload

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

cat <<EOF >> $LOCAL_CHECKOUT_DIR/firesim-$2-build.sh
#!/bin/bash

set -ex

cd $REMOTE_AWS_MARSHAL_DIR
./marshal build $1/$2.json
./marshal install $1/$2.json
EOF

# execute the script
chmod +x $LOCAL_CHECKOUT_DIR/firesim-$1-build.sh
run_script_aws $LOCAL_CHECKOUT_DIR/firesim-$1-build.sh

