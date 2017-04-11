#!/bin/bash
curl -X PUT http://localhost:8500/v1/agent/service/register --data-binary @- <<EOF
{
    "name": "cloudwrap",
    "tags": [ "deployment:$SERVICE_DEPLOYMENT" ],
    "port": 3030,
    "check": {
      "http": "http://localhost:3030",
      "interval": "10s"
    }
}
EOF

run_forever() (
    while true; do
        "$@"
        sleep 5
    done
)

cd /opt/cloudwrap

touch api.log waiter.log
run_forever bundle exec ./api.rb >>api.log &
run_forever bundle exec ./waiter.rb >>waiter.log &
tail -f api.log waiter.log
