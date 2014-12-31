#!/bin/bash
export LANG=en_US.UTF-8
if grep -q crowbar /etc/passwd; then
    find /var /home  -xdev -user crowbar -exec chown "$OUTER_UID" '{}' ';'
    usermod -o -u "$OUTER_UID" crowbar
else
    useradd -o -U -u "$OUTER_UID" \
        -d /home/crowbar -m \
        -s /bin/bash \
        crowbar
fi
if grep -q crowbar /etc/group; then
    find /var /home -xdev -group crowbar -exec chown "$OUTER_UID:$OUTER_GID" '{}' ';'
    groupmod -o -g "$OUTER_GID" crowbar
    usermod -g "$OUTER_GID" crowbar
fi

if [[ $http_proxy ]] && ! pidof squid; then
    export upstream_proxy=$http_proxy
fi

if [[ $1 = --no-shell ]]; then
    shift
    NO_SHELL=true
fi
mkdir -p /root/.ssh
printf "%s\n" "$SSH_PUBKEY" >> /root/.ssh/authorized_keys

if [[ $1 ]]; then
    "$@"
    res=$?
fi
[[ $NO_SHELL = true ]] && exit ${res:=0}
. /etc/profile
export PATH=$PATH:/opt/opencrowbar/core/bin
/bin/bash -i
rm -rf /tftpboot/nodes
