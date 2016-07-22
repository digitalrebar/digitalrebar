#!/bin/bash

/usr/local/bin/rule-engine --listen=:${APIPORT} \
                           --endpoint ${EXTERNAL_REBAR_ENDPOINT} \
                           --backing=consul \
			   --host="rule-engine,rule-engine-servce,$IP,${EXTERNAL_IP%%/*},${HOSTNAME},127.0.0.1,localhost" \
                           --dataloc=digitalrebar/rule-engine/database

echo '^rule-engine/(api/.*)' |kv_put digitalrebar/public/revproxy/rule-engine/matcher

make_service rule-engine ${APIPORT} '{"script": "pidof rule-engine", "interval": "10s"}'
