#!/usr/bin/env bash

SIG=$1

echo "Signature: $SIG"

aws s3 cp --recursive --acl public-read /root/.cache/digitalrebar/tftpboot/sledgehammer/$SIG s3://rackn-sledgehammer/sledgehammer/$SIG

