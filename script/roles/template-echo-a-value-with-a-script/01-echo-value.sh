#!/bin/bash

# Scripts run by the script jig are actually individually sourced
# by the script runner.  This happens in a subshell, so the individual
# scripts do not share a single running environment.

# read_attribute gets the value of an attribute.  When a noderole
# is scheduled to run by the annealer, it calculates what all the
# attrib values should be based on the state of the noderole graph
# at the time.  
VALUE=$(read_attribute 'template/value_1')

echo "Value of template-value-1: $VALUE"

sleep 1

NEXTVAL=$RANDOM
echo "Setting value of template/value_2 to $NEXTVAL"

# write_attribute takes an attrib value as a second parameter.
# Attributes set this way do not go through schema validation.
# They will be converted to the appropriate JSON values.
write_attribute 'template/value_2' "$NEXTVAL"
