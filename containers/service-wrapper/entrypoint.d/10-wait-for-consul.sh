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
consul_addr=$(ip -4 -o addr show scope global |fgrep -v $EXTERNAL_IP |head -1 |awk '{print $4}')
consul agent --config-dir /etc/consul.d --data-dir /data --advertise ${consul_addr%%/*} "${join[@]}" &
unset join
unset j
# We need consul to converge on a leader.
# This can take a little time, so we ask for
# leader status.  The leader status returns
# nothing, a string that says no leader, or
# the leader IP:port pair.  The default port
# is 8300 for server communication.
while [[ $(curl http://localhost:8500/v1/status/leader) != *:8300* ]]; do
  sleep 1
done
