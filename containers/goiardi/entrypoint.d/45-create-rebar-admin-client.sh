mkdir -p /etc/chef

cd /tmp
if [[ $first_start ]]; then
    KEYFILE="/tmp/rebar.pem"
    EDITOR=/bin/true knife client create rebar \
          -s https://localhost:4646 \
          -a --file "$KEYFILE" -u admin \
          -k /etc/goiardi/admin.pem

    # Add chef-service after initial converge.
    count=0
    while ! rebar nodes bind "system-phantom.internal.local" to "chef-service"; do
        if ((count % 12 == 0)); then
            echo "Waiting for system-phantom.internal.local to show up."
        fi
        sleep 5
        count=$((count+1))
    done
    rebar deployments set system attrib chef-servers-admin-key to "{\"value\": [$(jq -R -s '@text' <"$KEYFILE")]}"
    rebar deployments set system attrib chef-servers-admin-name to "{\"value\": [\"rebar\"]}"
    rm -f $KEYFILE
fi


chef_ip="${FORWARDER_IP:-${EXTERNAL_IP}}"
rebar deployments set system attrib chef-servers to "{\"value\": [\"https://${chef_ip%%/*}:4646\"]}"

rebar deployments commit system

if [[ ! $FORWARDER_IP ]] ; then
    sed -e "s/FILLMEIN/${EXTERNAL_IP%%/*}/" /root/goiardi-external.json > /etc/consul.d/goiardi.json
else
    cp /root/goiardi-internal.json /etc/consul.d/goiardi.json
fi
consul reload
