#!/bin/bash

# take chipyard hash and combine with the checkout hash of boom

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

cp $LOCAL_CHECKOUT_DIR/CHIPYARD.hash $HOME/chipyard-boom.hash
echo $CIRCLE_SHA1 >> $HOME/chipyard-boom.hash
