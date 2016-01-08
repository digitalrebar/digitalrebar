mkdir -p /etc/chef

cd /tmp
KEYFILE="/tmp/rebar.pem"
EDITOR=/bin/true knife client create rebar \
   -s https://localhost:4646 \
   -a --file "$KEYFILE" -u admin \
   -k /etc/goiardi/admin.pem

# Store rebar pem file.
baseurl="http://127.0.0.1:8500/v1/kv/digitalrebar/private/chef/system"
token="?token=$CONSUL_M_ACL"

curl --data-binary "$(cat $KEYFILE)" -X PUT ${baseurl}/pem${token}
curl --data-binary "rebar" -X PUT ${baseurl}/account${token}
curl --data-binary "https" -X PUT ${baseurl}/proto${token}

# Add chef-service after initial converge.
count=0
while ! rebar nodes bind "system-phantom.internal.local" to "chef-service"; do
  if ((count % 12 == 0)); then
    echo "Waiting for system-phantom.internal.local to show up."
  fi
  sleep 5
  count=$((count+1))
done

chef_ip="${FORWARDER_IP:-${EXTERNAL_IP}}"
rebar deployments set system attrib chef-servers-admin-key to "{\"value\": [$(jq -R -s '@text' <"$KEYFILE")]}"
rebar deployments set system attrib chef-servers-admin-name to "{\"value\": [\"rebar\"]}"
rebar deployments set system attrib chef-servers to "{\"value\": [\"https://${chef_ip%%/*}:4646\"]}"

rebar deployments commit system


rm -f $KEYFILE
