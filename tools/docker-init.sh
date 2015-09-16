#!/bin/bash
export LANG=en_US.UTF-8
if grep -q rebar /etc/passwd; then
    find /var /home  -xdev -user rebar -exec chown "$OUTER_UID" '{}' ';'
    usermod -o -u "$OUTER_UID" rebar
else
    useradd -o -U -u "$OUTER_UID" \
        -d /home/rebar -m \
        -s /bin/bash \
        rebar
fi
if grep -q rebar /etc/group; then
    find /var /home -xdev -group rebar -exec chown "$OUTER_UID:$OUTER_GID" '{}' ';'
    groupmod -o -g "$OUTER_GID" rebar
    usermod -g "$OUTER_GID" rebar
fi

if [[ $http_proxy ]] && ! pidof squid; then
    export upstream_proxy=$http_proxy
else
    rm /etc/profile.d/proxy.sh
    unset http_proxy https_proxy no_proxy
fi

if [[ $1 = --no-shell ]]; then
    shift
    NO_SHELL=true
fi
[[ -d /tftpboot/nodes ]] && rm -rf /tftpboot/nodes || :
mkdir -p /root/.ssh
printf "%s\n" "$SSH_PUBKEY" >> /root/.ssh/authorized_keys

if [[ $1 ]]; then
    "$@"
    res=$?
fi
[[ $NO_SHELL = true ]] && exit ${res:=0}
. /etc/profile
export PATH=$PATH:/opt/digitalrebar/core/bin
/bin/bash -i
