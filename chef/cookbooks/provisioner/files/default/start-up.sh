#!/bin/bash

export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
set -x
set -e
shopt -s extglob

get_param() {
    [[ $(cat /proc/cmdline) =~ $1 ]] && echo "${BASH_REMATCH[1]}"
}

# Stuff from sledgehammer file that makes this command debuggable
# Some useful boot parameter matches
ip_re='([0-9a-f.:]+/[0-9]+)'
bootif_re='BOOTIF=([^ ]+)'
host_re='rebar\.fqdn=([^ ]+)'
install_key_re='rebar\.install\.key=([^ ]+)'
provisioner_re='provisioner\.web=([^ ]+)'
rebar_re='rebar\.web=([^ ]+)'
domain_re='rebar\.dns\.domain=([^ ]+)'
dns_server_re='rebar\.dns\.servers=([^ ]+)'
netname_re='"network":"([^ ]+)"'

# Grab the boot parameters we should always be passed

# install key first
export REBAR_KEY="$(get_param "$install_key_re")"

# Provisioner and Rebar web endpoints next
export PROVISIONER_WEB="$(get_param "$provisioner_re")"
export REBAR_WEB="$(get_param "$rebar_re")"
export DOMAIN="$(get_param "$domain_re")"
export DNS_SERVERS="$(get_param "$dns_server_re")"

if [[ $(cat /proc/cmdline) =~ $bootif_re ]]; then
    MAC="${BASH_REMATCH[1]//-/:}"
    MAC="${MAC#*:}"
elif [[ -d /sys/firmware/efi ]]; then
    declare -A boot_entries
    bootent_re='^Boot([0-9]{4})'
    efimac_re='MAC\(([0-9a-f]+)'
    while read line; do
        k="${line%% *}" 
        v="${line#* }"
        if [[ $k = BootCurrent:* ]]; then
            current_bootent="${line##BootCurrent: }"
        elif [[ $k =~ $bootent_re ]]; then
            boot_entries["${BASH_REMATCH[1]}"]="$v"
        fi
    done < <(efibootmgr -v) 

    if [[ ${boot_entries["$current_bootent"]} =~ $efimac_re ]]; then
        MAC=''
        for o in 0 2 4 6 8 10; do
            MAC+="${BASH_REMATCH[1]:$o:2}:"
        done
        MAC=${MAC%:}
    fi  
fi
for nic in /sys/class/net/*; do
    [[ -f $nic/address && -f $nic/type && \
        $(cat "$nic/type") = 1 && \
        $(cat "$nic/address") = $MAC ]] || continue
    BOOTDEV="${nic##*/}"
    break
done

if [[ ! $BOOTDEV ]]; then
    echo "We don't know what the MAC address of our boot NIC was!"
    exit 1
fi

# Figure out where we PXE booted from.
if ! [[ $(cat /proc/cmdline) =~ $host_re ]]; then
    export HOSTNAME="d${MAC//:/-}.${DOMAIN}"

    # does the node exist?
    exists=$(curl -s -o /dev/null -w "%{http_code}" --digest -u "$REBAR_KEY" \
      -X GET "$REBAR_WEB/api/v2/nodes/$HOSTNAME")
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
        curl -f -g --digest -u "$REBAR_KEY" -X POST \
          -d "name=$HOSTNAME" \
          -d "mac=$MAC" \
          -d "ip=$IP" \
          "$REBAR_WEB/api/v2/nodes/" || {
            echo "We could not create a node for ourself!"
            exit 1
        }
    else
        echo "Node already created, moving on"
    fi

    # does the rebar-managed-role exist?
    managed=$(curl -s -o /dev/null -w "%{http_code}" --digest -u "$REBAR_KEY" \
      -X GET "$REBAR_WEB/api/v2/nodes/$HOSTNAME/node_roles/rebar-managed-node")
    if [[ $managed == 404 ]]; then
        curl -f -g --digest -u "$REBAR_KEY" -X POST \
          -d "node=$HOSTNAME" \
          -d "role=rebar-managed-node" \
          "$REBAR_WEB/api/v2/node_roles/" && \
        curl -f -g --digest -u "$REBAR_KEY" -X PUT \
          "$REBAR_WEB/api/v2/nodes/$HOSTNAME/commit" || {
            echo "We could not commit the node!"
            exit 1
        }
    else
        echo "Node already committed, moving on"
    fi
else
    # Let Rebar know that we are back, and booted into Sledgehammer.
    export HOSTNAME="${BASH_REMATCH[1]}"
    echo "Node is back."
fi

# Always make sure we are marking the node not alive. It will comeback later.
curl -f -g --digest -u "$REBAR_KEY" \
    -X PUT "$REBAR_WEB/api/v2/nodes/$HOSTNAME" \
    -d 'alive=false' \
    -d 'bootenv=sledgehammer'
echo "Set node not alive - will be set in control.sh!"


# Figure out the admin network name
the_netname=""
netnameline=$(curl -f -g --digest -u "$REBAR_KEY" \
    -X GET "$REBAR_WEB/api/v2/nodes/$HOSTNAME/addresses?category=admin")
netnames=(${netnameline//,/ })
for netname in "${netnames[@]}"; do
    [[ $netname =~ $netname_re ]] || continue
    the_netname=${BASH_REMATCH[1]}
    break
done
echo "Using network name: $the_netname"

# Figure out what IP addresses we should have.
netline=$(curl -f -g --digest -u "$REBAR_KEY" \
    -X GET "$REBAR_WEB/api/v2/networks/${the_netname}/allocations" \
    -d "node=$HOSTNAME")

routerline=$(curl -f -g --digest -u "$REBAR_KEY" \
    -X GET "$REBAR_WEB/api/v2/networks/${the_netname}/network_routers/1" \
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

# We gotta have jq
if ! which jq; then
    yum -y install jq
fi

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

export REBAR_KEY PROVISIONER_WEB REBAR_WEB
export MAC BOOTDEV DOMAIN HOSTNAME

echo "transfer from start-up to control script"

[[ -x /tmp/control.sh ]] && exec /tmp/control.sh

echo "Did not get control.sh from $PROVISIONER_WEB/nodes/$HOSTNAME/control.sh"
exit 1
