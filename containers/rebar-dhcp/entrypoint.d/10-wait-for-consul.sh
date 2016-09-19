#!/bin/bash

# This overrides the wait-for-consul.sh from service-wrapper.

# We do this because dhcp always runs in either the host's networking
# or the forwarder containers, so there will be a running consul for us to
# look at.

# We need consul to converge on a leader.
# This can take a little time, so we ask for
# leader status.  The leader status returns
# nothing, a string that says no leader, or
# the leader IP:port pair.  The default port
# is 8300 for server communication.
while [[ $(curl http://localhost:8500/v1/status/leader) != *:8300* ]]; do
  sleep 1
done
