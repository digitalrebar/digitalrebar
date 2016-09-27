#!/bin/bash

cd ..
tar -zcf dr-master-code-`date +"%Y%m%d"`.tgz digitalrebar

for i in gliderlabs/consul digitalrebar/dr_trust_me:master digitalrebar/dr_postgres:master digitalrebar/dr_webproxy:master digitalrebar/dr_goiardi:master digitalrebar/dr_rebar_api:master digitalrebar/dr_ntp:master digitalrebar/cloudwrap:master digitalrebar/dr_dns:master digitalrebar/dr_rebar_dhcp:master digitalrebar/dr_provisioner:master digitalrebar/logging:master digitalrebar/dr_forwarder:master digitalrebar/dr_rev_proxy:master digitalrebar/dr_node:master digitalrebar/rule-engine:master
do
    docker pull $i
done

docker save -o dr-master-containers-`date +"%Y%m%d"`.tgz gliderlabs/consul digitalrebar/dr_trust_me:master digitalrebar/dr_postgres:master digitalrebar/dr_webproxy:master digitalrebar/dr_goiardi:master digitalrebar/dr_rebar_api:master digitalrebar/dr_ntp:master digitalrebar/cloudwrap:master digitalrebar/dr_dns:master digitalrebar/dr_rebar_dhcp:master digitalrebar/dr_provisioner:master digitalrebar/logging:master digitalrebar/dr_forwarder:master digitalrebar/dr_rev_proxy:master digitalrebar/dr_node:master digitalrebar/rule-engine:master

cd -
