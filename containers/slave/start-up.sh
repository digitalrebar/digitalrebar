#!/bin/bash
if [[ ! $REBAR_KEY ]]; then
    echo "REBAR_KEY not set, will not be able to communicate with Rebar"
    exit 1
fi
if [[ ! $REBAR_ENDPOINT ]]; then
    echo "REBAR_ENDPOINT not set, will not be able to communicate with Rebar"
    exit 1
fi
if [[ ! $PROVISIONER_WEB ]]; then
    echo "PROVISIONER_WEB not found, will not be able to fake being Sledgehammer"
    exit 1
fi


export HOSTNAME="$HOSTNAME.docker.local"
bootdev_ip_re='inet ([0-9.]+)/([0-9]+)'
bootdev_mac_re='link/ether ([0-9a-f:]+)'
if [[ $(ip -4 -o addr show dev eth0) =~ $bootdev_ip_re ]]; then
    IP="${BASH_REMATCH[1]}/${BASH_REMATCH[2]}"
else
    echo "Cannot find IP address of eth0, bailing"
    exit 1
fi
if [[ $(ip -o link show dev eth0) =~ $bootdev_mac_re ]]; then
    MAC="${BASH_REMATCH[1]}"
else
    echo "Cannot find MAC address of eth0, bailing"
    exit 1
fi

# Start up dropbear for SSH handling
mkdir -p /var/empty
ssh-keygen -A
$(which sshd)

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
fi
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

echo '#!/bin/true' >/usr/local/bin/ntpdate
chmod 755 /usr/local/bin/ntpdate

/tmp/control.sh

while [[ 1 = 1 ]]; do sleep 600; done
