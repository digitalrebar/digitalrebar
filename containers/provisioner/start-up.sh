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
host_re='rebar\.uuid=([^ ]+)'
uuid_re='^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[1-5][0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$'
install_key_re='rebar\.install\.key=([^ ]+)'
rebar_re='rebar\.web=([^ ]+)'
netname_re='"network":"([^ ]+)"'

# Grab the boot parameters we should always be passed

# install key first
export REBAR_KEY="$(get_param "$install_key_re")"
export REBAR_ENDPOINT="$(get_param "$rebar_re")"

echo "export REBAR_KEY=\"$REBAR_KEY\"" >/etc/profile.d/rebar-key.sh
echo "export REBAR_ENDPOINT=\"$REBAR_ENDPOINT\"" >> /etc/profile.d/rebar-key.sh
# Provisioner and Rebar web endpoints next

# Download the Rebar CLI
(cd /usr/local/bin; curl -s -f -L -O  "$PROVISIONER_WEB/files/rebar"; chmod 755 rebar)
export PATH=$PATH:/usr/local/bin

# See if we have already been created.
if [[ $(cat /proc/cmdline) =~ $host_re ]]; then
    export REBAR_UUID="${BASH_REMATCH[1]}"
    if [[ $REBAR_UUID =~ $uuid_re ]]; then
        # Our rebar.uuid is really a UUID, get the hostname elsewhere.
        export HOSTNAME="$(rebar nodes get "$REBAR_UUID" |jq -r '.name')"
    else
        # The 'uuid' we recieved is really the hostname.
        export HOSTNAME="$REBAR_UUID"
        export REBAR_UUID="$(rebar nodes show "$HOSTNAME" |jq -r '.uuid')"
    fi
else
    # We have not been created.
    export HOSTNAME="d${MAC//:/-}.${DOMAIN}"
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
                         \"provider\": \"metal\",
                         \"os_family\": \"linux\",
                         \"arch\": \"$(uname -m)\"}" || {
        echo "We could not create a node for ourself!"
        exit 1
    }
    export REBAR_UUID="$(rebar nodes show "$HOSTNAME" |jq -r '.uuid')"
    # does the rebar-managed-role exist?
    if ! grep -q rebar-managed-node < <(rebar nodes roles $REBAR_UUID); then
        rebar nodes bind $REBAR_UUID to rebar-managed-node && \
            rebar nodes commit $REBAR_UUID || {
            echo "We could not commit the node!"
            exit 1
        }
    else
        echo "Node already committed, moving on"
    fi
    dhclient -r
    dhclient "$BOOTDEV"
fi
echo "${REBAR_UUID}" > /etc/rebar-uuid
# Set our hostname for everything else.
if [ -f /etc/sysconfig/network ] ; then
    sed -i -e "s/HOSTNAME=.*/HOSTNAME=${HOSTNAME}/" /etc/sysconfig/network
fi
echo "${HOSTNAME#*.}" >/etc/domainname
hostname "$HOSTNAME"

# Force reliance on DNS
echo '127.0.0.1 localhost' >/etc/hosts

control_ip=$(ip -o -4 addr show scope global dev "$BOOTDEV" |awk '{print $4}')
rebar nodes set $REBAR_UUID attrib node-control-address to "{\"value\": \"${control_ip}\"}"

# Always make sure we are marking the node not alive. It will comeback later.
rebar nodes update $REBAR_UUID '{"alive": false, "bootenv": "sledgehammer"}'
echo "Set node not alive - will be set in control.sh!"

# Wait until the provisioner has noticed our state change
while [[ $(rebar nodes get "$REBAR_UUID" attrib provisioner-active-bootstate |jq -r '.value') != sledgehammer ]]; do
    sleep 1
done

control_sh_found=''
for p in "$REBAR_UUID" "$HOSTNAME"; do
    curl -s -f -L -o /tmp/control.sh "$PROVISIONER_WEB/machines/$p/control.sh" && \
    grep -q '^exit 0$' /tmp/control.sh && \
    head -1 /tmp/control.sh | grep -q '^#!/bin/bash' || continue
    control_sh_found=true
    break
done

if [[ ! $control_sh_found ]]; then
    echo "Could not load our control.sh!"
    exit 1
fi
chmod 755 /tmp/control.sh

export REBAR_KEY REBAR_ENDPOINT REBAR_UUID BOOTDEV PROVISIONER_WEB MAC DOMAIN DNS_SERVERS HOSTNAME

echo "transfer from start-up to control script"

[[ -x /tmp/control.sh ]] && exec /tmp/control.sh

echo "Did not get control.sh from $PROVISIONER_WEB/machines/$REBAR_UUID/control.sh"
exit 1
