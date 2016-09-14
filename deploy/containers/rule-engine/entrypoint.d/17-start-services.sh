#!/bin/bash

/usr/local/bin/rule-engine --backing=consul \
                           --dataloc=digitalrebar/rule-engine/database && exit 0
exit 1
