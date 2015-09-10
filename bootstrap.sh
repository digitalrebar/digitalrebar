#!/bin/bash
set -e
# If we have an http_proxy variable, make sure we have a semi-cromulent
# no_proxy variable as well.

if [[ ! $OCB_CLEANED ]]; then

    if which yum &>/dev/null; then
        yum clean all
    fi

    if which rpm &>/dev/null; then
        rpm --rebuilddb
    fi
    export OCB_CLEANED=true
fi

. /etc/profile
if [[ $http_proxy && !$no_proxy ]] ; then
    export no_proxy="127.0.0.1,localhost,::1"
fi

hostname_re='([[:alnum:]]+\.){2,}[[:alnum:]]+'

check_hostname() {
    [[ $(hostname) =~ $hostname_re ]] && return
    echo "The hostname for the system must already be set to its proper name!"
    exit 1
}

prefix_r=('recipe[barclamp]'
          'recipe[ohai]'
          'recipe[utils]')

boot_r=('recipe[rebar-bootstrap::boot]'
        'recipe[rebar-bootstrap::ssh]'
        'recipe[rebar-bootstrap::rebar-user]'
        'recipe[rebar-bootstrap::wsman]'
        'recipe[rebar-bootstrap::sledgehammer]'
        'recipe[rebar-bootstrap::gemstuff]'
        'recipe[rebar-bootstrap::go]'
        'recipe[rebar-bootstrap::goiardi-build]'
        'recipe[rebar-bootstrap::sws-build]'
        'recipe[rebar-bootstrap::dns-mgmt-build]'
        'recipe[rebar-bootstrap::consul]'
        'recipe[consul::install]'
        'recipe[consul::ui]')

node_r=('recipe[rebar-bootstrap::ssh]'
        'recipe[rebar-bootstrap::rebar-user]')

database_r=('recipe[rebar-bootstrap::consul]'
            'recipe[rebar-bootstrap::postgresql]')
chef_server_r=('recipe[rebar-bootstrap::consul]'
               'recipe[rebar-bootstrap::goiardi]')
proxy_r=("${prefix_r[@]}"
         'recipe[rebar-squid::client]')
consul_r=('recipe[rebar-bootstrap::consul]'
          'recipe[consul::start-service]'
          'recipe[rebar-bootstrap::consul-post]')

make_recipes() {
    local res="$(printf "%s," "$@")"
    printf "${res%,}"
}

prefix_recipes="$(make_recipes "${prefix_r[@]}")"
boot_recipes="$(make_recipes "${boot_r[@]}")"
database_recipes="$(make_recipes "${database_r[@]}")"
chef_server_recipes="$(make_recipes "${chef_server_r[@]}")"
proxy_recipes="$(make_recipes "${proxy_r[@]}")"
consul_recipes="$(make_recipes "${consul_r[@]}")"
node_recipes="$(make_recipes "${node_r[@]}")"

cd /opt/digitalrebar/core
# Figure out what we are running on.
if [[ -f /etc/system-release ]]; then
    read DISTRIB_ID _t DISTRIB_RELEASE rest < /etc/system-release
elif [[ -f /etc/os-release ]]; then
    . /etc/os-release
    DISTRIB_ID="$ID"
    DISTRIB_RELEASE="$VERSION_ID"
elif [[ -f /etc/lsb-release ]]; then
    . /etc/lsb-release
else
    echo "Cannot figure out what we are running on!"
fi
DISTRIB_ID="${DISTRIB_ID,,}"
OS_TOKEN="$DISTRIB_ID-$DISTRIB_RELEASE"
export OS_TOKEN DISTRIB_ID DISTRIB_RELEASE

if [[ -f bootstrap/${OS_TOKEN}.lib.sh ]]; then
    . "bootstrap/${OS_TOKEN}.lib.sh"
elif [[ -f bootstrap/${DISTRIB_ID}.lib.sh ]]; then
    . "bootstrap/${DISTRIB_ID}.lib.sh"
else
    echo "Cannot source a bootstrap library for $OS_TOKEN!"
    exit 1
fi

which chef-solo &>/dev/null || install_prereqs
