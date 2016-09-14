#!/usr/bin/env bash

rebar() {
    local rebar_cmd

    rebar_cmd=$(which rebar)
    if [[ $rebar_cmd == "" ]] ; then
        export PATH=$PATH:.
        rebar_cmd=$(which rebar)
        if [[ $rebar_cmd == "" ]] ; then
            if [[ $(uname -s) == Darwin ]] ; then
                curl -so rebar https://s3-us-west-2.amazonaws.com/rebar-cli/rebar-darwin-amd64
            else
                curl -so rebar https://s3-us-west-2.amazonaws.com/rebar-cli/rebar-linux-amd64
            fi
            chmod +x ./rebar
        fi
    fi

    command rebar "$@"
}

converge() {
    rebar converge && return 0
    failed_ids=($(rebar noderoles match '{"state": -1}' |jq -r '.[] |.id'))
    if [[ ! $failed_ids ]]; then
        echo "Converge failed, but no noderoles errored!"
        return 1
    fi
    for id in "${failed_ids[@]}"; do
        failed_noderole="$(rebar noderoles show "$id")"
        nodename=$(rebar nodes show $(printf '%s' "$failed_noderole" |jq -r '.node_id') |jq -r -c '.name')
        rolename=$(rebar roles show $(printf '%s' "$failed_noderole" |jq -r '.role_id') |jq -r -c '.name')
        echo "*** FAILED NODEROLE $nodename: $rolename ***"
        printf '%s' "$failed_noderole" |jq -r '.runlog'
        echo
        echo "*** END FAILED NODEROLE $nodename: $rolename ***"
    done
    echo "Rebar failed to converge."
    return 1
}

retry_until() {
    # $1 = seconds to wait
    # $2 = message to print on timeout
    # rest = command to retry
    local count="$1" msg="$2"
    shift 2
    while ! "$@" &>/dev/null; do
        count=$((count - 1))
        if ((count == 0)); then
            echo "$msg" >&2
            return 1
        fi
        sleep 1
    done
}

test_phantom() {
    ! rebar nodes show system-phantom.internal.local | jq '.alive, .available' |grep false
}

export REBAR_ACCESS=${REBAR_USER:-rebar}:${REBAR_PASSWORD:-rebar1}
export REBAR_ENDPOINT=${REBAR_ENDPOINT:-https://127.0.0.1:3000}
export REBAR_KEY=${REBAR_KEY:-$REBAR_ACCESS}

# Make sure jq is installed
if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
    sudo yum install -y epel-release
    sudo yum -y makecache
    sudo yum install -y jq curl
elif [[ -d /etc/apt ]]; then
    sudo apt-get -y --force-yes install jq curl
elif [[ -f /etc/SuSE-release ]]; then
    sudo zypper install -y -l jq curl
else
    echo "Staged on to unknown OS media!"
    exit 1
fi

echo "Waiting on system deployment"
retry_until 240 \
            "Took too long for system deployment to appear" \
            rebar deployments show system || exit 1
echo "Waiting on system-phantom.internal.local"
retry_until 240 \
            "Took too long for system-phantom.internal.local to be runnable" \
            test_phantom || exit 1
echo "Waiting for rebar to converge (up to 10 minutes)"
converge || exit 1

