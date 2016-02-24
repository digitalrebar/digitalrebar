#!/bin/bash

mkdir -p /home/rebar/.chef
admin_account=null
admin_key=null
chef_server=null
while [[ $admin_account = null || $admin_key = null || $chef_server = null ]]; do
    admin_account=$(rebar deployments get $SERVICE_DEPLOYMENT attrib chef-servers-admin-name |jq -r '.value | .[0]')
    chef_server=$(rebar deployments get $SERVICE_DEPLOYMENT attrib chef-servers |jq -r '.value | .[0]')
    admin_key=$(rebar deployments get $SERVICE_DEPLOYMENT attrib chef-servers-admin-key |jq -r '.value | .[0]')
    [[ $admin_account && $chef_server && $admin_key ]] || sleep 1
done
printf '%s' "$admin_key" >"/home/rebar/.chef/$admin_account.pem"
cat > /home/rebar/.chef/knife.rb <<EOF
log_level                :info
log_location             STDOUT
node_name                '${admin_account}'
client_key               '/home/rebar/.chef/${admin_account}.pem'
chef_server_url          '${chef_server}'
syntax_check_cache_path  '/home/rebar/.chef/syntax_check_cache'
ssl_verify_mode          :verify_none
EOF
chown -R rebar:rebar /home/rebar/.chef
chmod 0600 /home/rebar/.chef/*

/opt/digitalrebar/core/bin/chef-cookbook-upload >/tmp/chef-upload.out 2>&1

