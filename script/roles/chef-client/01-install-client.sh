#!/bin/bash
chef_url="$(read_attribute "chefjig/server/url")"
chef_client="$(read_attribute "chefjig/client/name")"
chef_key="$(read_attribute "chefjig/client/key")"

if ! [[ $chef_url && $chef_client && $chef_key ]]; then
    echo "Missing required attribs!"
    exit 1
fi

if [[ -f /etc/os-release ]]; then
  . /etc/os-release
fi

if ! which chef-client; then
    if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
        yum install -y glibc.i686
        curl -fgLO https://opscode-omnibus-packages.s3.amazonaws.com/el/6/i686/chef-11.18.12-1.el6.i686.rpm
        yum install -y chef-11.18.12-1.el6.i686.rpm

        # Fix ohai to work on centos7.1
        cd /opt/chef/embedded/lib/ruby/gems/1.9.1/gems/ohai-7.4.1/lib/ohai/plugins/linux/
        yum install -y patch
        patch <<EOF
--- platform.rb
+++ platform.rb
@@ -38,6 +38,9 @@
       contents = File.read("/etc/enterprise-release").chomp
       platform "oracle"
       platform_version get_redhatish_version(contents)
+    elsif File.exists?('/etc/centos-release')
+      platform "centos"
+      platform_version File.read("/etc/centos-release").scan(/(\d+|\.+)/).join
     elsif File.exists?("/etc/debian_version")
     # Ubuntu and Debian both have /etc/debian_version
     # Ubuntu should always have a working lsb, debian does not by default
EOF
        cd -

    elif [[ -d /etc/apt ]]; then
        cd /tmp
        # Stick with 11.x for now.
        curl -fgLO http://opscode-omnibus-packages.s3.amazonaws.com/ubuntu/10.04/x86_64/chef_11.18.12-1_amd64.deb
        dpkg -i chef_11.18.12-1_amd64.deb
    elif [[ -f /etc/SuSE-release ]]; then
        zypper install -y -l chef
    elif [[ "x$NAME" == "xCoreOS" ]]; then
        webserver=$(read_attribute "rebar/provisioner/server/webservers/0/url")
        if [[ ! $webserver ]]; then
            echo "Cannot figure out the URL to poll to see if we are ready to reboot!"
            exit 1
        fi
        mkdir -p /opt
        cd /opt
        wget --quiet "http://$webserver/files/coreos-chef.tgz"
        tar -zxf coreos-chef.tgz
        cd -
        mkdir -p /etc/profile.d
        echo 'export PATH="$PATH:/opt/chef/bin"' > /etc/profile.d/chef_path.sh
    else
        die "Staged on to unknown OS media!"
    fi
fi

mkdir -p "/etc/chef"
cat > "/etc/chef/client.rb" <<EOF
log_level       :info
log_location    STDOUT
node_name       '$chef_client'
chef_server_url '$chef_url'
client_key      '/etc/chef/client.pem'
EOF

printf '%s' "$chef_key" > "/etc/chef/client.pem"
