#!/bin/bash
# Wipe out stuff we want to re-fetch from scratch.
shopt -s nullglob extglob globstar
rm -rf /var/cache/rebar \
    /usr/share/ruby/gems/* \
    /usr/lib/ruby/gems/* \
    /var/lib/gems/*/gems \
    /opt/digitalrebar/core/rails/Gemfile.lock

touch /tmp/install_pkgs
cd /opt/digitalrebar/core
./bootstrap.sh
./rebar-boot.sh
    chef-solo -c /opt/digitalrebar/core/bootstrap/chef-solo.rb -o 'recipe[rebar-bootstrap::cleanup]' || {
    echo "Failed to create a cleaned Docker image"
    exit 1
}
exit 0
