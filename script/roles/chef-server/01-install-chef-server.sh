#!/bin/bash

server_fqdn=$(read_attribute "chefjig/server/fqdn")
server_address=$(read_attribute "chefjig/server/address")
server_url=$(read_attribute "chefjig/server/url")
server_deploy=$(read_attribute "chefjig/server/deploy")
admin_client=$(read_attribute "chefjig/server/client-name")
admin_clientkey=$(read_attribute "chefjig/server/client-key")
KEYFILE="/home/crowbar/.chef/$admin_client.pem"

mkdir -p "/home/crowbar/.chef"
if [[ $admin_clientkey ]]; then
    printf "%s" "$admin_clientkey" > "$KEYFILE"
fi

[[ -f $KEYFILE ]] || {
    echo "Cannot find admin client key @ $KEYFILE!"
    exit 1
}

cat > /home/crowbar/.chef/knife.rb <<EOF
log_level                :info
log_location             STDOUT
node_name                '$admin_client'
client_key               '$KEYFILE'
chef_server_url          '$server_url'
syntax_check_cache_path  '/home/crowbar/.chef/syntax_check_cache'
EOF
chown -R crowbar:crowbar /home/crowbar/.chef/

# Once we have a working client, upload all the cookbooks we care about.
/opt/opencrowbar/core/bin/chef-cookbook-upload
