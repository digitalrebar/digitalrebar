
#
# workloads/wl-lib.sh - Provides a library of functions for all workloads
#

# Initialize if it hasn't already
. workloads/wl-init.sh

# Prevent recursion
if [[ $WL_LIB_LOADED == true ]] ; then
    return
fi
export WL_LIB_LOADED=true

validate_provider() {
    error=0
    prov=$1

    if [ "$prov" == "" ] ; then
        return 0
    fi

    case $prov in
    packet)
        if [ "$PROVIDER_PACKET_KEY" == "" ] ; then
            echo "You must define PROVIDER_PACKET_KEY (can be added to ~/.dr_info)"
            error=1
        fi
        if [ "$PROVIDER_PACKET_PROJECT_ID" == "" ] ; then
            PROVIDER_PACKET_PROJECT_ID=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects | jq -r ".projects[].id"`
            if [ "$PROVIDER_PACKET_PROJECT_ID" == "" ] ; then
                echo "You must define PROVIDER_PACKET_PROJECT_ID (can be added to ~/.dr_info as PROVIDER_PACKET_PROJECT_ID)"
                error=1
            fi
        fi
        ;;
    aws)
        if [ "$PROVIDER_AWS_ACCESS_KEY_ID" == "" ] ; then
            echo "You must define PROVIDER_AWS_ACCESS_KEY_ID (can be added to ~/.dr_info)"
            error=1
        fi
        if [ "$PROVIDER_AWS_SECRET_ACCESS_KEY" == "" ] ; then
            echo "You must define PROVIDER_AWS_SECRET_ACCESS_KEY (can be added to ~/.dr_info)"
            error=1
        fi
        ;;
    google)
        if [ "$PROVIDER_GOOGLE_PROJECT" == "" ] ; then
            echo "You must define PROVIDER_GOOGLE_PROJECT (can be added to ~/.dr_info)"
            error=1
        fi
        if [ "$PROVIDER_GOOGLE_JSON_KEY" == "" ] ; then
            echo "You must define PROVIDER_GOOGLE_JSON_KEY (can be added to ~/.dr_info)"
            error=1
        fi
        ;;
    system|local)
        ;;
    *)
        echo "Unknown Provider or Unset Provider: $prov"
        error=1
        ;;
    esac

    if [[ $error == 1 ]] ; then
        exit 1
    fi
}

bring_up_admin() {

    echo "Bring up admin: provider $DEPLOY_ADMIN"

    case $DEPLOY_ADMIN in
        packet)
            if [ "$DEVICE_ID" != "" ] ; then
                # Get Public IP - HACK - should look it up
                IP=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID | jq -r .ip_addresses[0].address`
                CIDR=`curl -s -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID | jq -r .ip_addresses[0].cidr`
                export ADMIN_IP="$IP/$CIDR"
            fi
            ;;
        local)
            if [[ $(uname -s) = Darwin ]] ; then
                IP=${DOCKER_HOST%:*}
                IP=${IP##*/}

                SUBNETISH=${IP%.*}
                netmask=$(ifconfig -a | grep $SUBNETISH | awk '{ print $4}')
                CIDR=??
                if [ "$netmask" == "0xffffff00" ] ; then
                  CIDR=24
                fi
                if [ "$netmask" == "0xffff0000" ] ; then
                  CIDR=16
                fi
                export ADMIN_IP="$IP/$CIDR"
            fi
            ;;
    esac

    export REBAR_ENDPOINT=https://${ADMIN_IP%/*}:3000

    if rebar ping 2>/dev/null >/dev/null ; then
        echo "Admin node at $ADMIN_IP already running."
        return 0
    fi

    # Must set ADMIN_IP if it isn't set.
    case $DEPLOY_ADMIN in
        packet)
            # Inherits all our vars!!
            . ./run-in-packet.sh "$1"
            ;;
        local|system)
            # Inherits all our vars!!
            . ./run-in-system.sh
            ;;
        *)
            die "bring_up_admin not implemented: $DEPLOY_ADMIN"
            ;;
    esac

    export REBAR_ENDPOINT=https://${ADMIN_IP%/*}:3000
}

tear_down_admin() {
    case $DEPLOY_ADMIN in
        packet)
            curl -s -X DELETE -H "X-Auth-Token: $PROVIDER_PACKET_KEY" https://api.packet.net/projects/$PROVIDER_PACKET_PROJECT_ID/devices/$DEVICE_ID
            ;;

        system|local)
            # Inherits all our vars!!
            sleep 60 # Let the deletes drain.  This is lame
            . ./stop-in-system.sh
            ;;
        *)
            die "tear_down_admin not implemented: $DEPLOY_ADMIN"
            ;;
    esac
}

add_provider() {
    case $PROVIDER in
        packet)
            export PROVIDER_NAME="packet-provider"
            provider="{
  \"name\": \"$PROVIDER_NAME\",
  \"description\": \"Packet Provider\",
  \"type\": \"PacketProvider\",
  \"auth_details\": {
    \"project_token\": \"$PROVIDER_PACKET_KEY\",
    \"project_id\": \"$PROVIDER_PACKET_PROJECT_ID\"
  }
}"
            rebar providers create "$provider"
            ;;
        aws)
            export PROVIDER_NAME="aws-provider"
            PROVIDER_AWS_REGION=${PROVIDER_AWS_REGION:-us-west-2}
            provider="{
  \"name\": \"$PROVIDER_NAME\",
  \"description\": \"AWS Provider\",
  \"type\": \"AwsProvider\",
  \"auth_details\": {
    \"provider\": \"AWS\",
    \"aws_access_key_id\": \"$PROVIDER_AWS_ACCESS_KEY_ID\",
    \"aws_secret_access_key\": \"$PROVIDER_AWS_SECRET_ACCESS_KEY\",
    \"region\": \"$PROVIDER_AWS_REGION\"
  }
}"
            rebar providers create "$provider"
            ;;
        google)
            export PROVIDER_NAME="google-provider"
            provider="{
  \"name\": \"$PROVIDER_NAME\",
  \"description\": \"Google Provider\",
  \"type\": \"GoogleProvider\",
  \"auth_details\": {
    \"provider\": \"Google\",
    \"google_project\": \"$PROVIDER_GOOGLE_PROJECT\",
    \"google_json_key\": $PROVIDER_GOOGLE_JSON_KEY
  }
}"
            rebar providers create "$provider"
            ;;
        *)
            die "add_provider not implemented: $PROVIDER"
            ;;
    esac
}

#
# Arg1 is name of node
# Arg2 is the OS to install (defaults to centos7 if not specified)
#
# OS options are currently: centos7, ubuntu1404, debian8, debian7
#
start_machine() {
    OS=${2:-centos7}

    case $PROVIDER in
        packet)
            case $OS in 
                centos7) OS=centos_7;;
                ubuntu1404) OS=ubuntu_14_04;;
                debian7) OS=debian_7;;
                debian8) OS=debian_8;;
                *) OS=centos_7;;
            esac

            node="{
  \"name\": \"$1\",
  \"provider\": \"$PROVIDER_NAME\",
  \"hints\": {
    \"use-proxy\": false,
    \"use-ntp\": false,
    \"use-dns\": false,
    \"use-logging\": false,
    \"provider-create-hint\": {
      \"os\": \"$OS\",
      \"hostname\": \"$1\"
    }
  }
}"

            rebar nodes create "$node"
            ;;
        aws)
            # GREG: Choose ami?? by OS and Region

            node="{
  \"name\": \"$1\",
  \"provider\": \"$PROVIDER_NAME\",
  \"hints\": {
    \"use-proxy\": false,
    \"use-ntp\": false,
    \"use-dns\": false,
    \"use-logging\": false,
    \"provider-create-hint\": {}
  }
}"

            rebar nodes create "$node"
            ;;

        google)
            # GREG: Choose ami?? by OS and Region

            node="{
  \"name\": \"$1\",
  \"provider\": \"$PROVIDER_NAME\",
  \"hints\": {
    \"use-proxy\": false,
    \"use-ntp\": false,
    \"use-dns\": false,
    \"use-logging\": false,
    \"provider-create-hint\": {}
  }
}"

            rebar nodes create "$node"
            ;;

        system)
            echo "Should call run-in-system.sh"
            die "add_provider not implemented: $PROVIDER"
            ;;
        *)
            die "add_provider not implemented: $PROVIDER"
            ;;
    esac
}

known_containers=(provisioner logging debug node access)
known_workloads=(all docker kubernetes hardware ceph packstack docker-swarm enterprise mesosphere burnin k8s-contrail stackengine rackn)

declare -A containers
declare -A workloads

use_container() {
    ! [[ ! ${containers[$1]} || ${containers[$1]} == false ]]
}
use_workload() {
    ! [[ ! ${workloads[$1]} || ${workloads[$1]} == false ]]
}

DEFAULT_ACCESS=${DEFAULT_ACCESS:-FORWARDER}
ACCESS=${ACCESS:-$DEFAULT_ACCESS}
DEPLOY_ADMIN=${DEPLOY_ADMIN:-system}

# preseed the containers from the environment variables
for cval in "${known_containers[@]}"; do
    varname=${cval^^}
    varname=${varname/-/_}
    novarname=CON_NO_${varname}
    varname=CON_${varname}

    if [[ ${!varname} ]] ; then
        containers["$cval"]=true
    fi
    if [[ ${!novarname} ]] ; then
        containers["$cval"]=false
    fi
done

# preseed the workloads from the environment variables
for cval in "${known_workloads[@]}"; do
    varname=${cval^^}
    varname=${varname/-/_}
    novarname=WL_NO_${varname}
    varname=WL_${varname}

    if [[ ${!varname} ]] ; then
        workloads["$cval"]=true
    fi
    if [[ ${!novarname} ]] ; then
        workloads["$cval"]=false
    fi
done

args=()
while (( $# > 0 )); do
    arg="$1"
    arg_key="${arg%%=*}"
    arg_data="${arg#*=}"
    case $arg_key in
        # This used to process init-files.sh and workload.sh args
        --con-*)
            a="${arg_key#--con-}"
            is_set=false
            for cval in "${known_containers[@]}"; do
                if [[ $a = $cval ]]; then
                    containers["$cval"]=true
                    is_set=true
                elif [[ $a = no-$cval ]]; then
                    containers["$cval"]=false
                    is_set=true
                fi
                [[ $is_set = true ]] && break
            done
            if [[ $is_set = false ]]; then
                die "Unknown container arg: ${arg}"
            fi;;

        --wl-*)
            a="${arg_key#--wl-}"
            is_set=false
            for cval in "${known_workloads[@]}"; do
                if [[ $a = $cval ]]; then
                    workloads["$cval"]=true
                    is_set=true
                elif [[ $a = no-$cval ]]; then
                    workloads["$cval"]=false
                    is_set=true
                fi
                [[ $is_set = true ]] && break
            done
            if [[ $is_set = false ]]; then
                die "Unknown workload arg: ${arg}"
            fi;;
        --help|-h)
            usage
            exit 0
            ;;
        --*)
            arg_key="${arg_key#--}"
            arg_key="${arg_key/-/_}"
            arg_key="${arg_key^^}"
            echo "Overriding $arg_key with $arg_data"
            export $arg_key="$arg_data"
            ;;
        *)
            args+=("$arg");;
    esac
    shift
done
set -- "${args[@]}"

if [[ $DEBUG == true ]] ; then
    set -x
fi

validate_provider $DEPLOY_ADMIN
validate_provider $PROVIDER

