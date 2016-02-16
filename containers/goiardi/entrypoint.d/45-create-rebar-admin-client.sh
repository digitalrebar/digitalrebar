mkdir -p /etc/chef

cd /tmp
if [[ $first_start ]]; then
    KEYFILE="/tmp/rebar.pem"
    EDITOR=/bin/true knife client create rebar \
          -s https://localhost:4646 \
          -a --file "$KEYFILE" -u admin \
          -k /etc/goiardi/admin.pem

    # Add chef-service after initial converge.
    bind_service "chef-service"
    set_service_attrib chef-service chef-servers-admin-key "{\"value\": [$(jq -R -s '@text' <"$KEYFILE")]}"
    set_service_attrib chef-service chef-servers-admin-name "{\"value\": [\"rebar\"]}"
    rm -f $KEYFILE
fi


chef_ip="${FORWARDER_IP:-${EXTERNAL_IP}}"
set_service_attrib chef-service chef-servers "{\"value\": [\"https://${chef_ip%%/*}:4646\"]}"
make_service "chef" "4646" '{"script": "pidof goiardi", "interval": "10s"}'
consul reload
