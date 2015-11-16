#!/bin/bash
# Copyright 2015, RackN Inc

set -e

if [[ ! -e workload.sh ]]; then
  cd compose
fi

./workload.sh digitalrebar core

while [[ $1 ]]; do
  case "$1" in
    kubernetes)
      ./workload.sh rackn docker
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
      ./workload.sh rackn docker
      ./workload.sh rackn $1
      shift;;
    enterprise)
      ./workload.sh rackn $1
      shift;;
    mesosphere)
      ./workload.sh rackn docker
      ./workload.sh rackn $1
      shift;;
    burnin)
      ./workload.sh rackn $1
      shift;;
    k8s-contrail)
      ./workload.sh rackn $1
      shift;;
    stackengine)
      ./workload.sh rackn $1
      shift;;
    rackn)
      for wl in docker kubernetes hardware ceph; do
        ./workload.sh rackn ${wl}
      done
      shift;;
    all)
      for wl in docker kubernetes hardware ceph packstack docker-swarm enterprise mesosphere burnin stackengine k8s-contrail; do
        ./workload.sh rackn ${wl}
      done
      shift;;

    *)
      echo "$1 is not known, use `./workload [organization] [repo]` to install"
      exit -1;;
  esac
done
