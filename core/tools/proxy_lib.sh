#!/bin/bash

# This mangles proxies into shape for docker and KVM admin nodes.
declare -A mangled_proxies
mangled_proxies['http_proxy']=''
mangled_proxies['https_proxy']=''
mangled_proxies['no_proxy']=''

# Rewrite any proxies that refer to localhost to
# refer to $1 instead.
mangle_proxies() {
    local bridge_ip="$1"
    # If we are not using an HTTP proxy, then leave things alone.
    [[ $http_proxy ]] || return 0
    local raw_proxy="${http_proxy#*://}"
    raw_proxy="${raw_proxy%/}"
    local proxy_re='^(.+):([0-9]+)$'
    local hostsplit_re='(.*)@(.*)'
    local userpass_re='(.*):(.*)'
    if [[ $raw_proxy =~ $proxy_re ]]; then
        raw_proxy="${BASH_REMATCH[1]}"
        local proxy_port="${BASH_REMATCH[2]}"
    fi
    if [[ $raw_proxy =~ $hostsplit_re ]]; then
        raw_proxy="${BASH_REMATCH[2]}"
        local proxy_user="${BASH_REMATCH[1]}"
        if [[ ${BASH_REMATCH[1]} =~ $userpass_re ]]; then
            local proxy_password="${BASH_REMATCH[2]}"
            proxy_user="${BASH_REMATCH[1]}"
        fi
    fi
    # If we are running a local proxy, have Docker use it.
    # This will fail badly if the proxy does not accept
    # requests for addresses in Docker's range.'
    case $raw_proxy in
        127.0.0.1|[::]|localhost) raw_proxy="$bridge_ip";;
    esac
    local base_proxy="http://"
    if [[ $proxy_user ]]; then
        if [[ $proxy_password ]]; then
            base_proxy+="$proxy_user:$proxy_password@"
        else
            base_proxy+="$proxy_user@"
        fi
    fi
    base_proxy+="$raw_proxy"
    [[ $proxy_port ]] && base_proxy+=":${proxy_port}"
    mangled_proxies["http_proxy"]="$base_proxy"
    [[ $https_proxy ]] && mangled_proxies["https_proxy"]="$base_proxy"
    [[ $no_proxy ]] || return 0
    local -A wanted_no_proxy
    wanted_no_proxy['localhost']=true
    wanted_no_proxy['127.0.0.0/8']=true
    wanted_no_proxy['::1']=true
    wanted_no_proxy['172.16.0.0/12']=true
    wanted_no_proxy['192.168.0.0/16']=true
    wanted_no_proxy['127.0.0.1']=true
    if [[ $no_proxy ]]; then
        local -a no_proxy_parts no_proxy_part
        IFS="," read -a no_proxy_parts <<<"$no_proxy"
        for no_proxy_part in "${no_proxy_parts[@]}"; do
            [[ ${wanted_no_proxy["$no_proxy_part"]} ]] && continue
            wanted_no_proxy["$no_proxy_part"]=true
        done
    fi
    local no_proxy="$(printf '%s,' "${!wanted_no_proxy[@]}")"
    mangled_proxies["no_proxy"]="${no_proxy%,}"
}
