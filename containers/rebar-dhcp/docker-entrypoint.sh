#!/bin/bash
set -e

answer=""
# We need consul to converge on a leader.
# This can take a little time, so we ask for
# leader status.  The leader status returns
# nothing, a string that says no leader, or
# the leader IP:port pair.  The default port
# is 8300 for server communication.
count=0
while [[ $answer != *:8300* ]]; do
  if [ $((count % 60)) -eq 0 ] ; then
      echo "Waiting for consul leader: $answer"
  fi
  sleep 1
  answer=`curl http://localhost:8500/v1/status/leader`
  count=$((count+1))
done

# Now that consul is up, run whatever commands we want the system to run.
for cmd in /usr/local/entrypoint.d/*.sh; do
    [[ -x $cmd ]] || continue
    . "$cmd"
done
