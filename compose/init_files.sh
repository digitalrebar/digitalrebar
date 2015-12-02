#!/bin/bash

function usage {
    echo "Usage: $0 <flags> [options docker-compose flags/commands]"
    echo "  -h or --help - help (this)"
    echo "  --clean - cleans up directory and exits"
    echo "  --access <HOST|FORWARDER> # Defines how the admin containers should be accessed"
    echo "  --provisioner # Adds the provisioner components"
    echo "  --logging # Adds the logging (kibana,elasticsearch+) components"
    echo "  --debug # Adds the cadviser components"
    echo "  --node # Adds the node component"
    echo
    echo "  --external_ip <CIDR Address, default: 192.168.124.11/24> "
    echo "  --forwarder_ip <CIDR Address, default: 192.168.124.11/24> "
    echo "       forwarder_ip is ignored if HOST access mode is used."
    echo
    echo " If additional arguments are provided, they are passed to docker-compose"
    echo " Otherwise nothing is run and just files are setup."
}

ACCESS_MODE="FORWARDER"
FILES="base.yml"
PROVISION_IT="NO"

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
        rm -f access.env dc docker-compose.yml config-dir/api/config/networks/the_admin.json config-dir/api/config/networks/the_bmc.json
        sudo rm -rf data-dir
      exit 0
      ;;
    --access) 
      ACCESS_MODE=$1
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

# Process templates and build one big yml file for now.
rm -f docker-compose.yml
for i in $FILES
do
    # Fix Access Mode
    sed "/START ACCESS_MODE==${ACCESS_MODE_SED_DELETE}/,/END ACCESS_MODE==${ACCESS_MODE_SED_DELETE}/d" yaml_templates/$i >> docker-compose.yml
done
sed "/ACCESS_MODE==/d" docker-compose.yml > dc.yml
mv dc.yml docker-compose.yml

# Make access.env for Variables.
echo "EXTERNAL_IP=$EXTERNAL_IP" > access.env
echo "FORWARDER_IP=$FORWARDER_IP" >> access.env

# With remaining arguments
if [ "$#" -gt 0 ] ; then
    docker-compose $@
fi

