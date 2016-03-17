#!/bin/bash

que_worker() {
    queue="$1"
    shift
    workers="$1"
    shift
    LOG_LEVEL=info
    if [[ -f /opt/digitalrebar/dev.mode ]] ; then
      LOG_LEVEL=debug
    fi
    for ((i=0; i < workers; i++)) ; do
        cmd="bundle exec que -q $queue -w 1 -l ${LOG_LEVEL} ./config/environment.rb"
        as_rebar "$cmd" </dev/zero 2>&1 &>> "/var/log/rebar/$queue.$i.log" &
    done
}

start_workers() {
    que_worker NodeRoleRunner "${NODE_ROLE_RUNNERS:-10}"
    que_worker HighPriorityRunner 2
    disown -a
}

mkdir -p /var/run/rebar && chown rebar:rebar /var/run/rebar
start_workers
as_rebar bundle exec puma -d -C $PUMA_CFG
