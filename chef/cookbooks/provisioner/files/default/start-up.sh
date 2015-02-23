#!/bin/bash
# Figure out where we PXE booted from.
if ! [[ $(cat /proc/cmdline) =~ $host_re ]]; then
    export HOSTNAME="d${MAC//:/-}.${DOMAIN}"

    # does the node exist?
    exists=$(curl -s -o /dev/null -w "%{http_code}" --digest -u "$CROWBAR_KEY" \
      -X GET "$CROWBAR_WEB/api/v2/nodes/$HOSTNAME")
    if [[ $exists == 404 ]]; then
        # Get IP for create suggestion
        IP=""
        bootdev_ip_re='inet ([0-9.]+)/([0-9]+)'
        if [[ $(ip -4 -o addr show dev $BOOTDEV) =~ $bootdev_ip_re ]]; then
          IP="${BASH_REMATCH[1]}/${BASH_REMATCH[2]}"
        fi

        # Create a new node for us,
        # Add the default noderoles we will need, and
        # Let the annealer do its thing.
        curl -f -g --digest -u "$CROWBAR_KEY" -X POST \
          -d "name=$HOSTNAME" \
          -d "mac=$MAC" \
          -d "ip=$IP" \
          "$CROWBAR_WEB/api/v2/nodes/" || {
            echo "We could not create a node for ourself!"
            exit 1
        }
    else
        echo "Node already created, moving on"
    fi

    # does the crowbar-managed-role exist?
    managed=$(curl -s -o /dev/null -w "%{http_code}" --digest -u "$CROWBAR_KEY" \
      -X GET "$CROWBAR_WEB/api/v2/nodes/$HOSTNAME/node_roles/crowbar-managed-node")
    if [[ $managed == 404 ]]; then
        curl -f -g --digest -u "$CROWBAR_KEY" -X POST \
          -d "node=$HOSTNAME" \
          -d "role=crowbar-managed-node" \
          "$CROWBAR_WEB/api/v2/node_roles/" && \
        curl -f -g --digest -u "$CROWBAR_KEY" -X PUT \
          "$CROWBAR_WEB/api/v2/nodes/$HOSTNAME/commit" || {
            echo "We could not commit the node!"
            exit 1
        }
    else
        echo "Node already committed, moving on"
    fi
else
    # Let Crowbar know that we are back, and booted into Sledgehammer.
    export HOSTNAME="${BASH_REMATCH[1]}"
    echo "Node is back."
fi

# Always make sure we are marking the node not alive. It will comeback later.
curl -f -g --digest -u "$CROWBAR_KEY" \
    -X PUT "$CROWBAR_WEB/api/v2/nodes/$HOSTNAME" \
    -d 'alive=false' \
    -d 'bootenv=sledgehammer'
echo "Set node not alive - will be set in control.sh!"

# Figure out what IP addresses we should have.
netline=$(curl -f -g --digest -u "$CROWBAR_KEY" \
    -X GET "$CROWBAR_WEB/api/v2/networks/admin/allocations" \
    -d "node=$HOSTNAME")

routerline=$(curl -f -g --digest -u "$CROWBAR_KEY" \
    -X GET "$CROWBAR_WEB/api/v2/networks/admin/network_routers/1" \
    -d "node=$HOSTNAME")

# Bye bye to DHCP.
killall dhclient || :
ip addr flush "$BOOTDEV"

# Add our new IP addresses.
nets=(${netline//,/ })
for net in "${nets[@]}"; do
    [[ $net =~ $ip_re ]] || continue
    net=${BASH_REMATCH[1]}
    # Make this more complicated and exact later.
    ip addr add "$net" dev "$BOOTDEV" || :
done

routers=(${routerline//,/ })
iponly_re='([0-9a-f.:]+)/[0-9]+'
for router in "${routers[@]}"; do
    [[ $router =~ $iponly_re ]] || continue
    router=${BASH_REMATCH[1]}
    ip route add default via "$router" || :
done

# Set our hostname for everything else.
if is_suse; then
    echo "$HOSTNAME" > /etc/HOSTNAME
else
    if [ -f /etc/sysconfig/network ] ; then
      sed -i -e "s/HOSTNAME=.*/HOSTNAME=${HOSTNAME}/" /etc/sysconfig/network
    fi
    echo "${HOSTNAME#*.}" >/etc/domainname
fi
hostname "$HOSTNAME"

# Update our /etc/resolv.conf with the IP address of our DNS servers,
# which were passed to us via kernel param.
chattr -i /etc/resolv.conf || :
echo "domain $DOMAIN" >/etc/resolv.conf.new

for server in ${DNS_SERVERS//,/ }; do
    echo "nameserver ${server}" >> /etc/resolv.conf.new
done

mv -f /etc/resolv.conf.new /etc/resolv.conf

# Force reliance on DNS
echo '127.0.0.1 localhost' >/etc/hosts
echo '::1 localhost6' >>/etc/hosts

# Wait until the provisioner has noticed our state change
while true; do
    curl -s -f -L -o /tmp/bootstate "$PROVISIONER_WEB/nodes/$HOSTNAME/bootstate" && \
        [[ -f /tmp/bootstate && $(cat /tmp/bootstate) = sledgehammer ]] && break
    sleep 1
done

curl -s -f -L -o /tmp/control.sh "$PROVISIONER_WEB/nodes/$HOSTNAME/control.sh" && \
    grep -q '^exit 0$' /tmp/control.sh && \
    head -1 /tmp/control.sh | grep -q '^#!/bin/bash' || {
    echo "Could not load our control.sh!"
    exit 1
}
chmod 755 /tmp/control.sh

export CROWBAR_KEY PROVISIONER_WEB CROWBAR_WEB
export MAC BOOTDEV DOMAIN HOSTNAME

echo "transfer from start-up to control script"

[[ -x /tmp/control.sh ]] && exec /tmp/control.sh

echo "Did not get control.sh from $PROVISIONER_WEB/nodes/$HOSTNAME/control.sh"
exit 1
