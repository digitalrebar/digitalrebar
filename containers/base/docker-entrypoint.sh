#!/bin/bash
export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
set -e
set -x

declare -a join
if grep -q consul /etc/hosts; then
    # If there is a consul host entry, join it.
    join=(--retry-join consul)
elif [[ $CONSUL_JOIN ]]; then
    # Otherwise, try $CONSUL_JOIN
    join=()
    for j in $CONSUL_JOIN; do
        join+=(--retry-join "$j")
    done
else
    # Finally, see if there is a consul running on our gateway and try to join it
    join=(--retry-join $(ip route show scope global |awk '/default/ {print $3}'))
fi
consul agent --config-dir /etc/consul.d --data-dir /data "${join[@]}" &
unset join
unset j
answer=""
# We need consul to converge on a leader.
# This can take a little time, so we ask for
# leader status.  The leader status returns
# nothing, a string that says no leader, or
# the leader IP:port pair.  The default port
# is 8300 for server communication.
set -o pipefail
count=0
while [[ $answer != *:8300* ]]; do
  sleep 1
  answer=$(curl http://localhost:8500/v1/status/leader)
  if [ $((count % 30)) -eq 0 ] ; then
      echo "Waiting for consul leader: $answer"
  fi
  count=$((count + 1))
done

kv_get() {
    # $1 = path into the KV store of the thing to get
    curl -sfg "http://localhost:8500/v1/kv/${1}?token=${CONSUL_M_ACL}" | \
        jq -r -c '.[0].Value' | \
        base64 -d
}

kv_put() {
    # $1 = path into the KV store to put things
    # Stdin should contain the data to store
    curl -sfg "http://localhost:8500/v1/kv/${1}?token=${CONSUL_M_ACL}" \
         -X PUT \
         --data-binary @-
}

unset answer count
# Just about everything wants an IP address.
IP=$(ip -o -4 addr show eth0 |awk '{ print $4 }' | awk -F/ '{ print $1 }')
forwarder=$(awk '/forwarder/ { print $1}' </etc/hosts|head -1)

# Now that consul is up, run whatever commands we want the system to run.
for cmd in /usr/local/entrypoint.d/*.sh; do
    [[ -x $cmd ]] || continue
    echo "Calling cmd: $cmd"
    . "$cmd"
done
