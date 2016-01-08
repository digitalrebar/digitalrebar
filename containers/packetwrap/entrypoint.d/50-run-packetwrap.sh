#!/bin/bash
if [[ $forwarder ]] ; then
    ip route del default
    ip route add default via $forwarder
fi

cat >> /etc/consul.d/packetwrap.json <<EOF
{
  "service": {
    "name": "packetwrap",
    "tags": [ "deployment:system" ],
    "port": 3031,
    "check": {
      "http": "http://localhost:3031",
      "interval": "10s"
    }
  }
}
EOF

run_forever() (
    while true; do
        "$@"
        sleep 5
    done
)

consul reload
cd /opt/packetwrap

touch api.log waiter.log
run_forever bundle exec ./api.rb >>api.log &
run_forever bundle exec ./waiter.rb >>waiter.log &
tail -f api.log waiter.log
