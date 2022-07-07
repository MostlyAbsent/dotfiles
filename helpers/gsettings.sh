#!/bin/bash

# Usage: helpers/gsettings.sh "schema" "key" "value"

schema=$1
key=$2
value=$3

startvalue=$(gsettings get $schema $key)

if [ "$startvalue" != "$value" ]; then
  gsettings set $schema $key $value
fi

