mkdir -p /etc/chef

if ! kv_get digitalrebar/private/goiardi/rebar_key &>/dev/null; then
    EDITOR=/bin/true knife client create rebar \
          -s https://localhost:4646 \
          -a --file /tmp/rebar.pem -u admin \
          -k /etc/goiardi/admin.pem
    # Add chef-service after initial converge.
    bind_service "chef-service"
    set_service_attrib chef-service chef-servers-admin-key "{\"value\": [$(jq -R -s '@text' </tmp/rebar.pem)]}"
    set_service_attrib chef-service chef-servers-admin-name "{\"value\": [\"rebar\"]}"
    kv_put digitalrebar/private/goiardi/rebar_key </tmp/rebar.pem
    rm -f /tmp/rebar.pem
fi


chef_ip="${FORWARDER_IP:-${EXTERNAL_IP}}"
set_service_attrib chef-service chef-servers "{\"value\": [\"https://${chef_ip%%/*}:4646\"]}"
make_service "chef" "4646" '{"script": "pidof goiardi", "interval": "10s"}'
