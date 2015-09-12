#!/bin/bash
# Copyright 2015, RackN Inc

./workload.sh digitalrebar core

while [[ $1 ]]; do
  case "$1" in
    kubernetes)
      ./workload.sh rackn $1
      shift;;
    hardware)
      ./workload.sh rackn $1
      shift;;
    ceph)
      ./workload.sh rackn $1
      shift;;
    *)
      echo "$1 is not known, use `./workload [organization] [repo]` to install"
      break;;
  esac
done