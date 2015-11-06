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
export REBAR_ENDPOINT="$(get_param "$rebar_re")"
# Provisioner and Rebar web endpoints next
export PROVISIONER_WEB="$(get_param "$provisioner_re")"
export DOMAIN="$(get_param "$domain_re")"
export DNS_SERVERS="$(get_param "$dns_server_re")"

# Download the Rebar CLI
(cd /usr/local/bin; curl -s -f -L -O  "$PROVISIONER_WEB/files/rebar"; chmod 755 rebar)
export PATH=$PATH:/usr/local/bin

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
    if ! rebar nodes show $HOSTNAME &>/dev/null; then
        # Get IP for create suggestion
        IP=""
        bootdev_ip_re='inet ([0-9.]+)/([0-9]+)'
        if [[ $(ip -4 -o addr show dev $BOOTDEV) =~ $bootdev_ip_re ]]; then
          IP="${BASH_REMATCH[1]}/${BASH_REMATCH[2]}"
        fi

        # Create a new node for us,
        # Add the default noderoles we will need, and
        # Let the annealer do its thing.
        rebar nodes create "{\"name\": \"$HOSTNAME\",
                             \"mac\": \"$MAC\",
                             \"ip\": \"$IP\",
                             \"variant\": \"metal\",
                             \"os_family\": \"linux\",
                             \"arch\": \"$(uname -m)\"}" || {
            echo "We could not create a node for ourself!"
            exit 1
        }
    else
        echo "Node already created, moving on"
    fi

    # does the rebar-managed-role exist?
    if ! grep -q rebar-managed-node < <(rebar nodes roles $HOSTNAME); then
        rebar nodes bind $HOSTNAME to rebar-managed-node && \
            rebar nodes commit $HOSTNAME || {
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
rebar nodes update $HOSTNAME '{"alive": false}'
echo "Set node not alive - will be set in control.sh!"

# Figure out what our current addresses should be
addrs="$(rebar nodes networkallocations $HOSTNAME |jq -r '.[] | .address')"
if [[ ! $addrs ]]; then
    echo "Could not find local network address allocations"
    exit 1
fi

network_id="$(rebar nodes networkallocations $HOSTNAME |jq -r '.[0].network_id')"
network_router="$(rebar networkrouters match "{\"network_id\": $network_id}" |jq -r '.[0].address')"
killall dhclient || :
ip addr flush scope global dev "$BOOTDEV"
for addr in $addrs; do
    ip addr add "$addr" dev "$BOOTDEV"
done

if [[ $network_router ]]; then
    ip route add default via "${network_router%%/*}"
fi

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

# Wait until the provisioner has noticed our state change
while [[ $(rebar nodes get "$HOSTNAME" attrib provisioner-active-bootstate |jq -r '.value') != sledgehammer ]]; do
    sleep 1
done

curl -s -f -L -o /tmp/control.sh "$PROVISIONER_WEB/nodes/$HOSTNAME/control.sh" && \
    grep -q '^exit 0$' /tmp/control.sh && \
    head -1 /tmp/control.sh | grep -q '^#!/bin/bash' || {
    echo "Could not load our control.sh!"
    exit 1
}
chmod 755 /tmp/control.sh

export REBAR_KEY PROVISIONER_WEB REBAR_ENDPOINT
export MAC BOOTDEV DOMAIN HOSTNAME

echo "transfer from start-up to control script"

[[ -x /tmp/control.sh ]] && exec /tmp/control.sh

echo "Did not get control.sh from $PROVISIONER_WEB/nodes/$HOSTNAME/control.sh"
exit 1
