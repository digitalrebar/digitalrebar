#!/bin/bash

export PS4='${BASH_SOURCE}@${LINENO}(${FUNCNAME[0]}): '
set -x
set -e
shopt -s extglob

get_param() {
    [[ $(cat /proc/cmdline) =~ $1 ]] && echo "${BASH_REMATCH[1]}"
}

dhcp_param() {
    [[ $(cat /var/lib/dhclient/dhclient.leases) =~ $1 ]] && echo "${BASH_REMATCH[1]}"
}


DHCPDIR=/var/lib/dhclient
RSYSLOGSERVICE=rsyslog

# Some useful boot parameter matches
ip_re='([0-9a-f.:]+/[0-9]+)'
bootif_re='BOOTIF=([^ ]+)'
provisioner_re='provisioner\.web=([^ ]+)'
domain_re='option domain-name "([^"]+)'
dns_servers_re='option domain-name-servers ([^;]+)'
PROVISIONER_WEB="$(get_param "$provisioner_re")"

# Test to see if we got everything we must have.
# Die horribly otherwise.
if ! [[ $PROVISIONER_WEB ]]; then
    echo "Do not know where to find the Provisioner."
    echo "This cannot happen"
    exit 1
fi

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

killall dhclient && sleep 5 || :
# Make sure our PXE interface is up, then fire up DHCP on it.
ip link set "$BOOTDEV" up || :
dhclient "$BOOTDEV" || :

bootdev_ip_re='inet ([0-9.]+)/([0-9]+)'
if ! [[ $(ip -4 -o addr show dev $BOOTDEV) =~ $bootdev_ip_re ]]; then
    echo "We did not get an address on $BOOTDEV"
    echo "Things will end badly."
    exit 1
fi

while ! [[ -x /tmp/start-up.sh ]]; do
    curl -sfL -o /tmp/start-up.sh "$PROVISIONER_WEB/nodes/start-up.sh" || :
    if grep -q '/tmp/control.sh' /tmp/start-up.sh && \
        head -1 /tmp/start-up.sh | grep -q '^#!/bin/bash'; then
        chmod 755 /tmp/start-up.sh
        break
    fi
    sleep 1
done
DOMAIN=$(dhcp_param "$domain_re")
DNS_SERVERS=$(dhcp_param "$dns_servers_re")
if [[ ! ($DOMAIN && $DNS_SERVERS) ]]; then
    echo "Cannot find domain from the DHCP information"
    exit 1
fi

export BOOTDEV PROVISIONER_WEB MAC DOMAIN DNS_SERVERS

. /tmp/start-up.sh
