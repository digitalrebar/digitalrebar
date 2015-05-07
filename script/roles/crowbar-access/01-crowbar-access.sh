#!/bin/bash

# Clean out key file
rm -f /root/.ssh/authorized_keys

# Make file key file
mkdir -p /root/.ssh
echo "# Crowbar built file" >> /root/.ssh/authorized_keys
chmod 600 /root/.ssh/authorized_keys

# Put keys in place
key_names=$(get_keys "crowbar/access_keys")
if [[ "${key_names}" != "" ]]; then
    for name in $key_names
    do
      read_attribute "crowbar/access_keys/${name}" >> /root/.ssh/authorized_keys
      echo >> /root/.ssh/authorized_keys
    done
fi

# Put machine key in place
rm -f /etc/crowbar.install.key
read_attribute_file_content "crowbar/machine_key" "/etc/crowbar.install.key"
chmod 644 /etc/crowbar.install.key

