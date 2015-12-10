
#
# workloads/wl-init.sh - Provides common functions and help extenders
#

# Prevent recursion
if [[ $WL_INIT_LOADED == true ]] ; then
    return
fi
export WL_INIT_LOADED=true

# Load default vars
if [ -f ~/.dr_info ] ; then
    . ~/.dr_info
fi

die() {
    echo "$@"
    exit 1
}

#
# Functions that help validate or start things.
# 

validate_tools() {
    error=0
    if [ "${BASH_VERSINFO}" -lt 4 ] ; then
        echo "Must have a bash version of 4 or higher"
        error=1
    fi

    if ! which ansible &>/dev/null; then
        echo "Please install Ansible!"
        if [[ $(uname -s) == Darwin ]] ; then
            echo "Something like: brew install ansible or pip install ansible"
        fi
        error=1
    fi

    if ! which curl &>/dev/null; then
        echo "Please install curl!"
        if [[ $(uname -s) == Darwin ]] ; then
            echo "Something like: brew install curl"
        fi
        error=1
    fi

    if ! which jq &>/dev/null; then
        echo "Please install jq!"
        if [[ $(uname -s) == Darwin ]] ; then
            echo "Something like: brew install jq"
        fi
        error=1
    fi

    if [[ $error == 1 ]] ; then
        exit 1
    fi

    if ! which rebar &>/dev/null ; then
        if [[ $(uname -s) == Darwin ]] ; then
            curl -O rebar https://s3-us-west-2.amazonaws.com/rebar-cli/rebar-darwin-amd64
        else
            curl -O rebar https://s3-us-west-2.amazonaws.com/rebar-cli/rebar-linux-amd64
        fi
        chmod +x ./rebar
        export REBAR=./rebar
    else
        export REBAR=$(which rebar)
    fi

    REBAR_USER=${REBAR_USER:-rebar}
    REBAR_PASSWORD=${REBAR_PASSWORD:-rebar1}
    export REBAR_KEY=${REBAR_KEY:-$REBAR_USER:$REBAR_PASSWORD}
}

usage() {
  echo "Usage $0:"
  echo
  echo "The equals (=) is required for valued options"
  echo
  for i in "${!help_options[@]}"; do
      printf "  %-30s: %s\n"  "$i"  "${help_options[$i]}"
  done | sort 
  echo
  echo "Allowed container services are:"
  for i in "${known_containers[@]}"; do
      printf "  %s\n" $i
  done
  echo "Used by: --con-provisioner or --con-no-provisioner"
  echo
  echo "Allowed workloads are:"
  for i in "${known_workloads[@]}"; do
      printf "  %s\n" $i
  done
  echo "Used by: --wl-kubernetes or --wl-no-kubernetes"
  echo
}

validate_tools

declare -A help_options

#
# Help commands
#

help_options["--admin-ip=<CIDRIP>"]="Admin IP to use - some providers don't need this"
help_options["--deploy-admin=<environment>"]="Environment to run the admin node in (packet|system|local)"
help_options["--device-id=<String>"]="Device ID of the admin node to use (allows command running for packet)"
help_options["--access=<HOST|FORWARDER>"]="Mode to run the admin node in"
help_options["--rebar-user=<String>"]="Username to access the rebar system"
help_options["--rebar-password=<String>"]="Password to access the rebar system"

help_options["--con-*"]="Container services to enable"
help_options["--con-no-*"]="Container services to disable"
help_options["--wl-*"]="Container services to enable"
help_options["--wl-no-*"]="Container services to disable"

help_options["--clean-ids"]="Clean the ssh keys authorized_keys file when touching it"
help_options["--id-file=<file>"]="Identity file to use to log into the node to ensure keys are in place"
help_options["--account=<string>"]="User to when initialy logging into systems"
help_options["--init-id-file=<file>"]="File that defines the initial identity"

# Provider config
#
#   --provider=<provider>
#     provider:
#       packet - needs KEY and PROJECT_ID
#       aws - Needs info - needs AWS config
#       system - nothing - needs IP list
#
# Can get from .dr_info or .dr_packet_info instead
#   --provider-packet-key=<key>
#   --provider-packet-project=<project>
#
# Can get from .dr_aws_info instead
#   --provider-aws-??
#   --provider-aws-??
#   --provider-aws-??
#   --provider-aws-??
#
# Can get from .dr_system_info
#   --provider-system-ip-list=<hostnames or ips comma separated>
#

