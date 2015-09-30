#!/bin/bash

# If something else has taken control of resolv.conf, stop right now.
if [[ -f /etc/.resolv.conf.rebar ]]; then
   if ! fgrep -q 'Managed by Rebar' /etc/resolv.conf; then
       echo "Something else has taken over managing resolv.conf.  We are done here."
       exit 0
   else
       cp /etc/resolv.conf /etc/.resolv.conf.orig
   fi
fi

# Disable resolvconf if it is installed.
if which resolvconf &>/dev/null && resolvconf --updates-are-enabled; then
    resolvconf --disable-updates
fi
    
nameservers="$(read_attribute 'rebar/dns/nameservers' |jq -r '.[] |.address')"
if [[ ! $nameservers ]]; then
    nameservers="$(read_attribute 'rebar/dns/server/forwarders' |jq -r '.[]')"
fi
domain="$(read_attribute 'rebar/dns/domain')"

echo "# Managed by Rebar.  Do not edit" >/etc/.resolv.conf.rebar
printf 'search %s\n' "$domain" >> /etc/.resolv.conf.rebar
printf 'nameserver %s\n' $nameservers >> /etc/.resolv.conf.rebar


if [[ -x /.dockerinit ]]; then
    if ! cat /etc/.resolv.conf.rebar >/etc/resolv.conf; then
        echo "/etc/resolv.conf cannot be managed for this container.  You may get unexpected results"
    fi
else
    chattr -i /etc/resolv.conf || :
    mv /etc/.resolv.conf.rebar /etc/resolv.conf
    chattr +i /etc/resolv.conf
fi
