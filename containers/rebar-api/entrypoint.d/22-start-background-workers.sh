#!/bin/bash

que_worker() {
    queue="$1"
    shift
    workers="$1"
    shift
    for ((i=0; i < workers; i++)) ; do
        cmd="bundle exec que -q $queue -w 1 -l debug ./config/environment.rb"
        as_rebar "$cmd" </dev/zero 2>&1 &>> "/var/log/rebar/$queue.$i.log" &
    done
}

start_workers() {
    que_worker NodeRoleRunner 10
    que_worker HighPriorityRunner 2
    disown -a
}

mkdir -p /var/run/rebar && chown rebar:rebar /var/run/rebar
start_workers
as_rebar bundle exec puma -d -C $PUMA_CFG
