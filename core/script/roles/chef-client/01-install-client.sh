#!/bin/bash
chef_url="$(read_attribute "chefjig/server/url")"
chef_client="$(read_attribute chefjig/client/name)"
chef_key="$(read_attribute chefjig/client/key)"
sources="$(get_attr chef-client-sources)"
provisioner="$(get_attr provisioner-webservers |jq -r -c '.[0] | .url')"

if ! [[ $chef_url && $chef_client && $chef_key ]]; then
    echo "Missing required attribs!"
    exit 1
fi

client_src=''
if ! which chef-client; then
    case $OS_TYPE in
        centos|redhat|fedora|rhel|scientificlinux)
            client_src='.centos';;
        *) client_src=".${OS_TYPE}";;
    esac
    if [[ $client_src == null || $client_src = '' ]]; then
        echo "Cannot determine where to install Chef from"
        exit 1
    fi
    client_upstream="$(jq -r -c "$client_src | .source" <<< "$sources")"
    client_sha256="$(jq -r -c "$client_src | .sha256" <<< "$sources")"
    client_pkg="${client_upstream##*/}"
    # See if we can get the package from the provisioner first
    if curl -fglO "$provisioner/files/chef/$client_pkg"; then
        echo "Chef client cached on the provisioner, will use it."
    elif curl -fgLO "$client_upstream"; then
        echo "Chef client $client_pkg downloaded from upstream."
        echo "Please consider caching it on the provisioner by:"
        echo "   curl -fgLO $client_upstream"
        echo "   rebar provisioner files upload $client_pkg to chef/$client_pkg"
    else
        echo "Failed to download Chef client, cannot continue"
        echo "Please consider caching it on the provisioner by:"
        echo "   curl -fgLO $client_upstream"
        echo "   rebar provisioner files upload $client_pkg to chef/$client_pkg"
        echo
        echo "This will allow the provisioner to serve the Chef client package directly"
        exit 1
    fi
    if ! sha256sum -c <(printf '%s  %s' "$client_sha256" "$client_pkg"); then
        echo "Invalid checksum for $client_pkg"
        exit 1
    fi
    case $OS_TYPE in
        centos|redhat|fedora|rhel|scientificlinux)
            yum -y install "$client_pkg";;
        ubuntu|debian)
            dpkg -i "$client_pkg";;
        suse|sles|opensuse)
            zypper install -y -l "$client_pkg";;
        *)
            echo "No idea how to install on $OS_TYPE";;
    esac
fi

mkdir -p "/etc/chef"
cat > "/etc/chef/client.rb" <<EOF
log_level       :info
log_location    STDOUT
node_name       '$chef_client'
chef_server_url '$chef_url'
client_key      '/etc/chef/client.pem'
# Yes, this is insecure for now.
ssl_verify_mode :verify_none
EOF

printf '%s' "$chef_key" > "/etc/chef/client.pem"
