#!/bin/bash
# Wipe out stuff we want to re-fetch from scratch.
shopt -s nullglob extglob globstar
rm -rf /var/cache/crowbar \
    /usr/share/ruby/gems/* \
    /usr/lib/ruby/gems/* \
    /var/lib/gems/*/gems \
    /opt/opencrowbar/core/rails/Gemfile.lock
touch /tmp/install_pkgs
cd /opt/opencrowbar/core
./bootstrap.sh
./crowbar-boot.sh
    chef-solo -c /opt/opencrowbar/core/bootstrap/chef-solo.rb -o 'recipe[crowbar-bootstrap::cleanup]' || {
    echo "Failed to create a cleaned Docker image"
    exit 1
}
exit 0
