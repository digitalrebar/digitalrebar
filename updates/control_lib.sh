#!/bin/bash

[[ $MAXTRIES ]] || export MAXTRIES=5

try_to() {
    # $1 = max times to try a command.
    # $2 = times to wait in between tries
    # $@ function and args to try
    local tries=1 maxtries="$1" sleeptime="$2"
    shift 2
    until "$@"; do
        ((tries >= maxtries)) && {
            echo "$* failed ${tries} times.  Rebooting..."
            reboot_system
        }
        echo "$* failed ${tries} times.  Retrying..."
        sleep "$sleeptime"
        tries=$((${tries}+1))
    done
}

__post_state() {
  local curlargs=(--connect-timeout 60 -s -L -X PUT -d "{ \"state\": \"$1\" }" \
      -H "Accept: application/json" -H "Content-Type: application/json")
  [[ $REBAR_KEY ]] && curlargs+=(-u "$REBAR_KEY" --digest --anyauth)
  (unset http_proxy; curl "${curlargs[@]}" \
      "http://$ADMIN_IP:3000/api/v2/nodes/$HOSTNAME/transition")
}

__get_state() {
    local curlargs=(--connect-timeout 60 -s -L -H "Accept: application/json" \
        -H "Content-Type: application/json")
  [[ $REBAR_KEY ]] && curlargs+=(-u "$REBAR_KEY" --digest)
  curl "${curlargs[@]}" \
      "http://$ADMIN_IP:3000/api/v2/nodes/$HOSTNAME"
}

post_state() { try_to "$MAXTRIES" 15 __post_state "$@"; }
get_state() { try_to "$MAXTRIES" 15 __get_state "$@"; }

reboot_system() {
  sync
  sleep 30
  umount -l /updates /install-logs
  reboot
}

wait_for_allocated() {
    # $1 = hostname
    while [[ $ALLOCATED = false ]]; do
        get_state "$1"
        [[ $ALLOCATED = true ]] && return
        sleep 15
    done
}

hook_has_run() {
    local statefile="/install-logs/$HOSTNAME-$HOOKNAME-$HOOKSTATE"
    if [[ -f $statefile ]]; then
        return 0
    else
        touch "$statefile"
        return 1
    fi
}

wait_for_rebar_state() {
    # $1 = hostname
    # $2 = rebar state to wait for.  If empty, wait for a state change
    [[ $2 && $2 = $REBAR_STATE ]] && return
    local current_state=$REBAR_STATE
    while [[ 1 = 1 ]]; do
        get_state "$1"
        if [[ $2 ]]; then
            [[ $2 = $REBAR_STATE ]] && return
        elif [[ $current_state != $REBAR_STATE ]]; then
            return
        fi
        sleep 15
    done
}

report_state () {
    if [ -a /var/log/chef/hw-problem.log ]; then
	"cp /var/log/chef/hw-problem.log /install-logs/$HOSTNAME-hw-problem.log"
        post_state problem
    else
        post_state "$2"
    fi
}
