#!/bin/bash
set -e
# If we have an http_proxy variable, make sure we have a semi-cromulent
# no_proxy variable as well.
. /etc/profile
if [[ $http_proxy && !$no_proxy ]] ; then
    export no_proxy="127.0.0.1,localhost,::1"
fi

prefix_r=(recipe[barclamp]
          recipe[ohai]
          recipe[utils])
boot_r=('recipe[crowbar-bootstrap::boot]'
        'recipe[crowbar-bootstrap::wsman]'
        'recipe[crowbar-bootstrap::grub]'
        'recipe[crowbar-bootstrap::sledgehammer]'
        'recipe[crowbar-bootstrap::gemstuff]'
        'recipe[crowbar-bootstrap::go]'
        'recipe[crowbar-bootstrap::goiardi-build]'
        'recipe[crowbar-bootstrap::sws-build]'
        'recipe[crowbar-bootstrap::consul]'
        'recipe[consul::install]'
        'recipe[consul::ui]')
database_r=('recipe[crowbar-bootstrap::postgresql]'
            'recipe[crowbar-bootstrap::goiardi]')
proxy_r=("${prefix_r[@]}"
         'recipe[crowbar-squid]')
consul_r=('recipe[consul::start-service]')

make_recipes() {
    local res="$(printf "%s," "$@")"
    printf "${res%,}"
}

prefix_recipes="$(make_recipes "${prefix_r[@]}")"
boot_recipes="$(make_recipes "${boot_r[@]}")"
database_recipes="$(make_recipes "${database_r[@]}")"
proxy_recipes="$(make_recipes "${proxy_r[@]}")"
consul_recipes="$(make_recipes "${consul_r[@]}")"

cd /opt/opencrowbar/core
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
