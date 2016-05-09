
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
        if [ "$PROVIDER_AWS_REGION" == "" ] ; then
            echo "You must define PROVIDER_AWS_REGION (can be added to ~/.dr_info)"
            error=1
        fi

        if [[ $error = 0 ]] ; then
            aws configure set aws_access_key_id $PROVIDER_AWS_ACCESS_KEY_ID
            aws configure set aws_secret_access_key $PROVIDER_AWS_SECRET_ACCESS_KEY
            aws configure set default.region $PROVIDER_AWS_REGION
        fi

        ;;
    debug)
        ;;
    docean)
        # ADD PROVIDER INIT HERE!
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
    openstack)
        if [ "$PROVIDER_OS_AUTH_URL" == "" ] ; then
            echo "You must define PROVIDER_OS_AUTH_URL (can be added to ~/.dr_info)"
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
        aws)
            if [ "$DEVICE_ID" != "" ] ; then
                # Get Public IP - HACK - should look it up
                IP=`aws ec2 describe-instances --instance-id $DEVICE_ID | jq -r .Reservations[0].Instances[0].PublicIpAddress`
                CIDR=32
            fi
            ;;
        google)
            if [ "$DEVICE_ID" != "" ] ; then
                # Get Public IP - HACK - should look it up
                if [[ $PROVIDER_GOOGLE_ZONE ]] ; then
                    ZONE_NAME="--zone $PROVIDER_GOOGLE_ZONE"
                fi
                if [[ $PROVIDER_GOOGLE_PROJECT ]] ; then
                    PROJECT_ID="--project $PROVIDER_GOOGLE_PROJECT"
                fi
                IP=`gcloud ${PROJECT_ID} compute instances describe $ZONE_NAME $DEVICE_ID --format=json | jq -r .networkInterfaces[0].accessConfigs[0].natIP`
                CIDR=32
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

    if [[ $ADMIN_IP ]] ; then
        export REBAR_ENDPOINT=https://${ADMIN_IP%/*}:3000
        if rebar ping 2>/dev/null >/dev/null ; then
            echo "Admin node at $ADMIN_IP already running."
            ADMIN_ALREADY_UP=true
            return 0
        fi
    fi

    # Must set ADMIN_IP if it isn't set.
    case $DEPLOY_ADMIN in
        packet)
            # Inherits all our vars!!
            . ./run-in-packet.sh "$1"
            ;;
        aws)
            # Inherits all our vars!!
            . ./run-in-aws.sh "$1"
            ;;
        google)
            # Inherits all our vars!!
            . ./run-in-google.sh "$1"
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

        google)
            if [[ $PROVIDER_GOOGLE_ZONE ]] ; then
                ZONE_NAME="--zone $PROVIDER_GOOGLE_ZONE"
            fi
            if [[ $PROVIDER_GOOGLE_PROJECT ]] ; then
                PROJECT_ID="--project $PROVIDER_GOOGLE_PROJECT"
            fi
            gcloud ${PROJECT_ID} compute instances delete $DEVICE_ID --delete-disks all $ZONE_NAME
            ;;

        aws)
            aws ec2 terminate-instances --instance-ids $DEVICE_ID
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
        debug)
            export PROVIDER_NAME="debug-provider"
            PROVIDER_AWS_REGION=${PROVIDER_AWS_REGION:-us-west-2}
            provider="{
  \"name\": \"$PROVIDER_NAME\",
  \"description\": \"Debug Provider\",
  \"type\": \"AwsProvider\",
  \"auth_details\": {
    \"provider\": \"AWS\",
    \"aws_access_key_id\": \"dummyiTqwW5qpsDQbn1Z38Ma94gWh99c\",
    \"aws_secret_access_key\": \"dummyca9-28d4-4cee-8280-8af2f5464f37\",
    \"region\": \"us-west-2\",
    \"debug\":{
      \"host_ip\":\"192.17.0.17\",
      \"boot_delay_time\": 1,
      \"ssh_delay_time\": 1
    }
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
        openstack)
            export PROVIDER_NAME="openstack-provider"
            provider="{
  \"name\": \"$PROVIDER_NAME\",
  \"description\": \"OpenStack Provider\",
  \"type\": \"OpenStackProvider\",
  \"auth_details\": {
    \"os-username\": \"$PROVIDER_OS_USERNAME\",
    \"os-password\": \"$PROVIDER_OS_PASSWORD\",
    \"os-project-name\": \"$PROVIDER_OS_PROJECT_NAME\",
    \"os-auth-url\": \"$PROVIDER_OS_AUTH_URL\",
    \"os-region-name\": \"$PROVIDER_OS_REGION_NAME\",
    \"os-ssh-user\": \"$PROVIDER_OS_SSH_USER\",
    \"os-debug\": \"$PROVIDER_OS_DEBUG\"
  }
}"
            rebar providers create "$provider"
            ;;
        *)
            die "add_provider not implemented: $PROVIDER"
            ;;
    esac
}

lookup_image_id() {
  PROV=$1
  REGION=$2
  OS=$3

  answer=$(egrep "$PROV.*$REGION.*$OS" workloads/os.map | awk '{ print $4 }')
  if [[ ! $answer ]] ; then
    answer=$(egrep "$PROV.*all.*$OS" workloads/os.map | awk '{ print $4 }')
  fi

  echo $answer
}

#
# Arg1 is name of node
# Arg2 is the OS to install (defaults to centos7 if not specified)
# Arg2 is the IP to use for system provider
#
# OS options are currently: centos7, ubuntu1404, debian8, debian7
#
start_machine() {
    OS=${2:-centos7}
    os_name=$(lookup_image_id $PROVIDER $PROVIDER_AWS_REGION $OS)

    case $PROVIDER in
        packet)
            export PROVIDER_NAME="packet-provider"
            if [[ $os_name ]] ; then
                OS_FIELD="\"os\": \"${os_name}\","
            fi

            node="{
  \"name\": \"$1\",
  \"provider\": \"$PROVIDER_NAME\",
  \"hints\": {
    \"use-proxy\": false,
    \"use-ntp\": false,
    \"use-dns\": false,
    \"use-logging\": false,
    \"provider-create-hint\": {
      $OS_FIELD
      \"hostname\": \"$1\"
    }
  }
}"
            rebar nodes create "$node"
            ;;
        aws)
            export PROVIDER_NAME="aws-provider"
            if [[ $os_name ]] ; then
                IMAGE_ID="\"image_id\": \"${os_name}\","
            fi
            if [[ $PROVIDER_AWS_INSTANCE_TYPE ]] ; then
                INST_DATA="\"flavor_id\": \"$PROVIDER_AWS_INSTANCE_TYPE\","
            fi

            node="{
  \"name\": \"$1\",
  \"provider\": \"$PROVIDER_NAME\",
  \"hints\": {
    \"use-proxy\": false,
    \"use-ntp\": false,
    \"use-dns\": false,
    \"use-logging\": false,
    \"provider-create-hint\": {
      $IMAGE_ID
      $INST_DATA
      \"hostname\": \"$1\"
    }
  }
}"

            rebar nodes create "$node"
            ;;

        google)
            export PROVIDER_NAME="google-provider"
            if [[ $os_name ]] ; then
                DISKS="\"disks\": [{
                  \"autoDelete\": true,
                  \"boot\": true,
                  \"type\": \"PERSISTENT\",
                  \"initializeParams\": {
                    \"sourceImage\": \"$os_name\"
                  }
                }],"
            fi

            if [[ $PROVIDER_GOOGLE_INSTANCE_TYPE ]] ; then
                INST_DATA="\"machine_type\": \"$PROVIDER_GOOGLE_INSTANCE_TYPE\","
            fi

            if [[ $PROVIDER_GOOGLE_ZONE ]] ; then
                ZONE_NAME="\"zone_name\": \"$PROVIDER_GOOGLE_ZONE\","
            fi

            node="{
  \"name\": \"$1\",
  \"provider\": \"$PROVIDER_NAME\",
  \"hints\": {
    \"use-proxy\": false,
    \"use-ntp\": false,
    \"use-dns\": false,
    \"use-logging\": false,
    \"provider-create-hint\": {
      $DISKS
      $INST_DATA
      $ZONE_NAME
      \"hostname\": \"$1\"
    }
  }
}"

            rebar nodes create "$node"
            ;;
        openstack)
            node="{
  \"name\": \"$1\",
  \"provider\": \"$PROVIDER_NAME\",
  \"hints\": {
    \"use-proxy\": false,
    \"use-ntp\": false,
    \"use-dns\": false,
    \"use-logging\": false,
    \"provider-create-hint\": {
      \"os-flavor\":\"4096\",
      \"hostname\": \"$1\"
    }
  }
}"

            rebar nodes create "$node"
            ;;
        system)
            # Assumes that ADMIN_IP is set
            # Assumes that OS==IP and it can ssh.
            #
            # Get the ssh keys and update authorized_keys
            KEY_FILE2="/tmp/keys2.$$"
            rebar deployments get system rebar-access_keys | jq -r '.value|to_entries[].value' > $KEY_FILE2
            scp $KEY_FILE2 root@$IP:keys
            rm -rf $KEY_FILE2

            scp scripts/join_rebar.sh root@$OS:
            ssh root@$OS /root/join_rebar.sh $ADMIN_IP
            ;;
        *)
            die "start_machine not implemented: $PROVIDER"
            ;;
    esac
}

known_containers=(provisioner dhcp dns dns-mgmt ntp chef revproxy webproxy logging debug node access ux)
known_workloads=(all docker kubernetes hardware ceph packstack docker-swarm enterprise mesosphere burnin k8s-contrail stackengine rackn ux)

declare -A containers
declare -A workloads

classifiers=()

# Default the normal containers on, but they can be turned off.
[[ ${containers["dns"]} ]] || containers["dns"]=true
[[ ${containers["dns-mgmt"]} ]] || containers["dns-mgmt"]=true
[[ ${containers["ntp"]} ]] || containers["ntp"]=true
[[ ${containers["chef"]} ]] || containers["chef"]=true
[[ ${containers["webproxy"]} ]] || containers["webproxy"]=true

use_container() {
    ! [[ ! ${containers[$1]} || ${containers[$1]} == false ]]
}
use_workload() {
    ! [[ ! ${workloads[$1]} || ${workloads[$1]} == false ]]
}

DEFAULT_ACCESS=${DEFAULT_ACCESS:-FORWARDER}
ACCESS=${ACCESS:-$DEFAULT_ACCESS}
DEPLOY_ADMIN=${DEPLOY_ADMIN:-system}
DR_TAG=${DR_TAG:-latest}

# preseed the containers from the environment variables
for cval in "${known_containers[@]}"; do
    varname=${cval^^}
    varname=${varname//-/_}
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
    varname=${varname//-/_}
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
        --classifier)
            classifiers+=("$arg_data")
	    ;;
        --help|-h)
            usage
            exit 0
            ;;
        --*)
            arg_key="${arg_key#--}"
            arg_key="${arg_key//-/_}"
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

OLD_DR_TAG=$(cat compose/tag)
if [[ $OLD_DR_TAG != $DR_TAG ]] ; then
	echo "WARNING: ******************************"
	echo "WARNING: Changing tags can be dangerous"
	echo "WARNING: ******************************"
	echo "$DR_TAG" > compose/tag
fi

validate_tools

# Gets overridden when bringing up the admin node
export REBAR_ENDPOINT=https://${ADMIN_IP%/*}:3000

validate_provider $DEPLOY_ADMIN
validate_provider $PROVIDER

