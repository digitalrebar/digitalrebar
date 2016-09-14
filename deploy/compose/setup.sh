#!/usr/bin/env bash
# Copyright 2015, RackN Inc

set -e

if [[ ! -e workload.sh ]]; then
  cd compose
fi

DR_TAG=latest
if [[ $1 == "--tag" ]] ; then
    shift
    DR_TAG=$1
    shift
fi

./workload.sh digitalrebar core

while [[ $1 ]]; do
  case "$1" in
    ux)
      ./workload.sh rackn digitalrebar-ux
      shift;;
    kubernetes)
      ./workload.sh digitalrebar kompos8
      shift;;
    kompos8)
      ./workload.sh digitalrebar $1
      shift;;
    kargo)
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
    rackn)
      for wl in docker kubernetes hardware ceph; do
        ./workload.sh rackn ${wl}
      done
      shift;;
    all)
      for wl in docker kompos8 hardware ceph packstack docker-swarm enterprise mesosphere burnin digitalrebar-ux; do
        ./workload.sh rackn ${wl}
      done
      shift;;

    *)
      echo "$1 is not known, use `./workload [organization] [repo]` to install"
      exit -1;;
  esac
done
