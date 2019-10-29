#!/bin/bash

# take chipyard hash and combine with the checkout hash of boom

# turn echo on and error on earliest command
set -ex

# get shared variables
SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source $SCRIPT_DIR/defaults.sh

cat $LOCAL_CHECKOUT_DIR/CHIPYARD.hash $CIRCLE_SHA1 &> $HOME/chipyard-boom.hash
