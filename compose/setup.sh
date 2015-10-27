#!/bin/bash
# Copyright 2015, RackN Inc

if [[ ! -e workload.sh ]]; then
  cd compose
fi

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
    packstack)
      ./workload.sh rackn $1
      shift;;
    docker)
      ./workload.sh rackn $1
      shift;;
    docker-swarm)
      ./workload.sh rackn $1
      shift;;
    enterprise)
      ./workload.sh rackn $1
      shift;;
    mesosphere)
      ./workload.sh rackn $1
      shift;;
    burnin)
      ./workload.sh rackn $1
      shift;;
    stackengine)
      ./workload.sh rackn $1
      shift;;
    rackn)
      for wl in kubernetes hardware ceph; do
        ./workload.sh rackn ${wl}
      done
      shift;;
    all)
      for wl in kubernetes hardware ceph packstack docker docker-swarm enterprise mesosphere burnin stackengine; do
        ./workload.sh rackn ${wl}
      done
      shift;;

    *)
      echo "$1 is not known, use `./workload [organization] [repo]` to install"
      break;;
  esac
done
