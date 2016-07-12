#!/bin/bash

/usr/local/bin/rule-engine --listen=:${APIPORT} \
                           --endpoint ${EXTERNAL_REBAR_ENDPOINT} \
                           --backing=consul \
                           --dataloc=digitalrebar/rule-engine/database

echo '^rule-engine/(api/.*)' |kv_put digitalrebar/public/revproxy/rule-engine/matcher

check_line="{\"script\": \"curl --cacert /etc/rule-engine/cacert.prm --cert /etc/rule-engine/cert.pem https://${IP%/*}:${APIPORT}/api/v0/rulesets >/dev/null 2>&1\", \"interval\": \"10s\", \"timeout\": \"2s\"}"

make_service rule-engine ${APIPORT} "${check_line}"
