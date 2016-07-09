#!/usr/bin/env bash

if which sudo 2>/dev/null >/dev/null ; then
    SUDO=sudo
fi

SERVICES="network-server rebar-api_service logging-service"

function usage {
    echo "Usage: $0 <flags> [options docker-compose flags/commands]"
    echo "  -h or --help - help (this)"
    echo "  --clean - cleans up directory and exits"
    echo "  --access <HOST|FORWARDER> # Defines how the admin containers should be accessed"
    echo "  --dhcp # Adds the dhcp component"
    echo "  --provisioner # Adds the provisioner component"
    echo "  --dns # Adds the dns component"
    echo "  --ntp # Adds the ntp component"
    echo "  --chef # Adds the chef component"
    echo "  --webproxy # Adds the webproxy component"
    echo "  --revproxy # Adds the revproxy component"
    echo "  --logging # Adds the logging (kibana,elasticsearch+) components"
    echo "  --debug # Adds the cadviser components"
    echo "  --node # Adds the node component"
    echo "  --tag <TAG> # Uses that tag for builds and trees. default: latest"
    echo "  --classifier name:rulepath # adds a classifier to the docker-compose file with that name and rule file"
    echo
    echo "  --external_ip <CIDR Address, default: 192.168.124.11/24> "
    echo "  --forwarder_ip <CIDR Address, default: 192.168.124.11/24> "
    echo "       forwarder_ip is ignored if HOST access mode is used."
    echo
    echo " If additional arguments are provided, they are passed to docker-compose"
    echo " Otherwise nothing is run and just files are setup."
}

#
# Sets a value for a variable.
# The variable must exist.
#
function set_var_in_common_env {
  local var=$1
  local value=$2

  sed -i -e "s/^${var}=.*/${var}=${value}/" common.env
}

function add_classifier {
    local clname=$1
    local clpath=$2

    echo "Adding classifier: $clname $clpath"
    cat > /tmp/${clname}.yml <<EOF
cl_${clname}:
  extends:
    file: docker-compose-common.yml
    service: classifier
  volumes:
    - ${clpath}:/etc/classifier/rules.yml
EOF
    FILES="$FILES /tmp/${clname}.yml"
    REMOVE_FILES="$REMOVE_FILES /tmp/${clname}.yml"
}

FILES="base.yml trust-me.yml"
REMOVE_FILES=""
ACCESS_MODE="FORWARDER"
PROVISION_IT="NO"
if [[ -f tag ]]; then
    DR_TAG="$(cat tag)"
elif [[ ! $DR_TAG ]]; then
    DR_TAG="latest"
fi
ADD_DNS=false
RUN_NTP="NO"

while [[ $1 == -* ]] ; do
  arg=$1
  shift

  case $arg in
    --help)
      usage
      exit 0
      ;;
    -h)
      usage
      exit 0
      ;;
    --clean)
        rm -f access.env services.env dc docker-compose.yml config-dir/api/config/networks/the_admin.json config-dir/api/config/networks/the_bmc.json tag
        $SUDO rm -rf data-dir
      exit 0
      ;;
    --access)
      ACCESS_MODE=$1
      shift
      ;;
    --tag)
      DR_TAG=$1
      shift
      ;;
    --external_ip)
      EXTERNAL_IP=$1
      shift
      ;;
    --forwarder_ip)
      FORWARDER_IP=$1
      shift
      ;;
    --provisioner)
      FILES="$FILES provisioner.yml"
      PROVISION_IT="YES"
      SERVICES+=" provisioner-service"
      ;;
    --ntp)
      FILES="$FILES ntp.yml"
      SERVICES+=" ntp-service"
      RUN_NTP="YES"
      ;;
    --chef)
      FILES="$FILES chef.yml"
      SERVICES+=" chef-service"
      ;;
    --dhcp)
      FILES="$FILES dhcp.yml"
      SERVICES+=" dhcp-mgmt_service dhcp-service"
      ;;
    --dns)
      if [[ $ADD_DNS != true ]] ; then
          FILES="$FILES dns.yml"
      fi
      SERVICES+=" dns-service"
      ADD_DNS=true
      ;;
    --dns-mgmt)
      if [[ $ADD_DNS != true ]] ; then
          FILES="$FILES dns.yml"
      fi
      SERVICES+=" dns-mgmt_service"
      ADD_DNS=true
      ;;
    --webproxy)
      FILES="$FILES webproxy.yml"
      SERVICES+=" proxy-service"
      ;;
    --revproxy)
      FILES="$FILES revproxy.yml"
      ;;
    --debug)
      FILES="$FILES debug.yml"
      ;;
    --logging)
      FILES="$FILES logging.yml"
      ;;
    --node)
      FILES="$FILES node.yml"
      ;;
    --ux)
      FILES="$FILES ux.yml"
      ;;
    --classifier)
      clinfo=$1
      shift

      clname=${clinfo%:*}
      clpath=${clinfo##*:}
      add_classifier "$clname" "$clpath"
      ;;
  esac

done

if [ "$ACCESS_MODE" == "FORWARDER" ] ; then
    EXTERNAL_IP=${EXTERNAL_IP:-192.168.124.11/24}
    FORWARDER_IP=${FORWARDER_IP:-192.168.124.11/24}
    ACCESS_MODE_SED_DELETE="HOST"
    MYPWD=`pwd`
    cd config-dir/api/config/networks
    rm -f the_admin.json
    rm -f the_bmc.json
    if [ "$PROVISION_IT" == "YES" ] ; then
        ln -s the_admin.json.forwarder the_admin.json
        ln -s the_bmc.json.forwarder the_bmc.json
    fi
    cd $MYPWD
elif [ "$ACCESS_MODE" == "HOST" ] ; then
    EXTERNAL_IP=${EXTERNAL_IP:-192.168.99.100/24}
    FORWARDER_IP=
    ACCESS_MODE_SED_DELETE="FORWARDER"
    MYPWD=`pwd`
    cd config-dir/api/config/networks
    rm -f the_admin.json
    rm -f the_bmc.json
    if [ "$PROVISION_IT" == "YES" ] ; then
        ln -s the_admin.json.mac the_admin.json
    fi
    cd $MYPWD
else
    echo "ACCESS MODE: $ACCESS_MODE is not HOST or FORWARDER"
    exit 1
fi

# Find classifier files in repos
# This is assumed to be run from in the compose directory.
echo "Trying to find classifiers"
while read clpath ; do
    clname=$(basename $(dirname $clpath))
    add_classifier "$clname" "$clpath"
done < <(find ../.. -type f -name classifier.yml 2>/dev/null)

# Process templates and build one big yml file for now.
rm -f docker-compose.yml
for i in $FILES
do
    fname=$i
    if [[ $i != /* ]] ; then
	    fname=yaml_templates/$i
    fi
    # Fix Access Mode
    sed "/START ACCESS_MODE==${ACCESS_MODE_SED_DELETE}/,/END ACCESS_MODE==${ACCESS_MODE_SED_DELETE}/d" $fname >> docker-compose.yml
done
sed "/ACCESS_MODE==/d" docker-compose.yml > dc.yml
mv dc.yml docker-compose.yml

if [[ $REMOVE_FILES ]] ; then
	rm -f $REMOVE_FILES
fi

# Find the IP address we should have Consul advertise on
if [[ $(uname -s) == Darwin ]]; then
    CONSUL_ADVERTISE=${DOCKER_HOST%:*}
    CONSUL_ADVERTISE=${CONSUL_ADVERTISE##*/}
elif [[ $(uname -s) == "MINGW64_NT-10.0" ]]; then
    CONSUL_ADVERTISE=${DOCKER_HOST%:*}
    CONSUL_ADVERTISE=${CONSUL_ADVERTISE##*/}
else
    gwdev=$(/sbin/ip -o -4 route show default |head -1 |awk '{print $5}')
    if [[ $gwdev ]]; then
        # First, advertise the address of the device with the default gateway
        CONSUL_ADVERTISE=$(/sbin/ip -o -4 addr show scope global dev "$gwdev" |head -1 |awk '{print $4}')
        CONSUL_ADVERTISE="${CONSUL_ADVERTISE%/*}"
    else
        # Hmmm... we have no access to the Internet.  Pick an address with
        # global scope and hope for the best.
        CONSUL_ADVERTISE=$(/sbin/ip -o -4 addr show scope global |head -1 |awk '{print $4}')
        CONSUL_ADVERTISE="${CONSUL_ADVERTISE%/*}"
    fi
fi
FORWARDER_OTHER_IP=$(/sbin/ip -o -4 addr show scope global |awk '{print $4}')
FORWARDER_OTHER_IP="$(echo $FORWARDER_OTHER_IP | tr " " ,),127.0.0.1/8"
# If we did not get and address to listen on, we are pretty much boned anyways
if [[ ! $CONSUL_ADVERTISE ]]; then
    echo "Could not find an address for Consul to listen on!"
    exit 1
fi
# CONSUL_JOIN is separate from CONSUL_ADVERTISE as futureproofing
CONSUL_JOIN="$CONSUL_ADVERTISE"
# Make access.env for Variables.
cat >access.env <<EOF
EXTERNAL_IP=$EXTERNAL_IP
FORWARDER_IP=$FORWARDER_IP
FORWARDER_OTHER_IP=$FORWARDER_OTHER_IP
CONSUL_JOIN=$CONSUL_JOIN
DR_START_TIME=$(date +%s)
RUN_NTP=$RUN_NTP
EOF

cat >services.env <<EOF
SERVICES=$SERVICES
EOF

cat >config-dir/consul/server-advertise.json <<EOF
{"advertise_addr": "${CONSUL_ADVERTISE}"}
EOF

echo "$DR_TAG" >tag

# With remaining arguments
if [ "$#" -gt 0 ] ; then
    docker-compose $@
fi
