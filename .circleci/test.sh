#!/bin/bash

#var=something

if [ -v var ]; then
    echo "Using $var"
    AWS_SERVER=$var
else
    AWS_SERVER=old_value
fi

echo "Server val: $AWS_SERVER"
