#!/bin/bash

if ! which chef-client; then
    if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
        yum -y makecache
        yum install -y chef
    elif [[ -d /etc/apt ]]; then
        apt-get -y update
        # Our chef package does not need ruby, but it does need the cstruct gem.
        apt-get -y --force-yes install chef
        service chef-client stop
    elif [[ -f /etc/SuSE-release ]]; then
        zypper install -y -l chef
    else
        die "Staged on to unknown OS media!"
    fi
fi

mkdir -p "/etc/chef"
mkdir -p "/var/chef"
clientname=$(read_attribute "rebar/chef-solo/name")
cat > "/etc/chef/solo.rb" <<EOF
log_level       :info
log_location    STDOUT
node_name       '$clientname'
solo            true
cookbook_path   "/var/chef/cookbooks"
data_bag_path   "/var/chef/data_bags"
role_path       "/var/chef/roles"
EOF
