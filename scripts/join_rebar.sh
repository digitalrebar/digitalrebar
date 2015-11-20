#!/bin/bash

set -e
shopt -s extglob

# Make sure that curl and jq are installed.

if [[ -f /etc/redhat-release || -f /etc/centos-release ]]; then
    yum install -y epel-release
    yum -y makecache
    yum install -y jq curl
elif [[ -d /etc/apt ]]; then
    apt-get -y --force-yes install jq curl
elif [[ -f /etc/SuSE-release ]]; then
    zypper install -y -l jq curl
else
    echo "Staged on to unknown OS media!"
    exit 1
fi

ADMIN_IP=$1
PASSED_IN_IP=$2
export REBAR_USER=rebar
export REBAR_PASSWORD=rebar1
export REBAR_KEY="$REBAR_USER:$REBAR_PASSWORD"
export REBAR_WEB="https://$ADMIN_IP:3000"

HOSTNAME=`hostname`
if [[ $HOSTNAME == ${HOSTNAME%%.*} ]] ; then
    HOSTNAME=`hostname -f`
fi
if [[ $HOSTNAME == ${HOSTNAME%%.*} ]] ; then
    echo "Hostname need to be fully qualified"
    HOSTNAME=`hostname`.rebar.local
fi

# Get the ssh keys and update authorized_keys
success=$(curl -k -s -o /tmp/keys -w "%{http_code}" --digest -u "$REBAR_KEY" \
      -X GET "${REBAR_WEB}/api/v2/deployments/1/attribs/rebar-access_keys")
if [[ $success != 200 ]] ; then
    echo "Failed to get keys"
    exit -1
fi
jq -r '.value|to_entries[].value' /tmp/keys > /tmp/keys2
echo >> /root/.ssh/authorized_keys
cat /tmp/keys2 >> /root/.ssh/authorized_keys
rm -rf /tmp/keys /tmp/keys2

# does the node exist?
exists=$(curl -k -s -o /dev/null -w "%{http_code}" --digest -u "$REBAR_KEY" \
      -X GET "$REBAR_WEB/api/v2/nodes/$HOSTNAME")
if [[ $exists == 404 ]]; then
    # Get IP for create suggestion
    IP="$PASSED_IN_IP"
    if [ "$IP" == "" ] ; then
        ip_re='([0-9a-f.:]+/[0-9]+)'
        if ! [[ $(ip -4 -o addr show |grep 'scope global' |grep -v ' lo' |grep -v ' dynamic') =~ $ip_re ]]; then
            echo "Cannot find IP address for the admin node!"
            exit 1
        fi
        IP="${BASH_REMATCH[1]}"
    fi

    # Create a new node for us,
    # Add the default noderoles we will need, and
    # Let the annealer do its thing.
    curl -k -f -g --digest -u "$REBAR_KEY" -X POST \
      -d "name=$HOSTNAME" \
      -d "ip=$IP" \
      -d "variant=metal" \
      -d "os_family=linux" \
      -d "arch=$(uname -m)" \
      "$REBAR_WEB/api/v2/nodes/" || {
        echo "We could not create a node for ourself!"
        exit 1
    }
else
    echo "Node already created, moving on"
fi

# does the rebar-joined-role exist?
joined=$(curl -k -s -o /dev/null -w "%{http_code}" --digest -u "$REBAR_KEY" \
      -X GET "$REBAR_WEB/api/v2/nodes/$HOSTNAME/node_roles/rebar-joined-node")
if [[ $joined == 404 ]]; then
    curl -k -f -g --digest -u "$REBAR_KEY" -X POST \
      -d "node=$HOSTNAME" \
      -d "role=rebar-joined-node" \
      "$REBAR_WEB/api/v2/node_roles/" && \
    curl -k -f -g --digest -u "$REBAR_KEY" -X PUT \
      "$REBAR_WEB/api/v2/nodes/$HOSTNAME/commit" || {
        echo "We could not commit the node!"
        exit 1
    }
else
    echo "Node already committed, moving on"
fi

# Always make sure we are marking the node not alive. It will comeback later.
curl -k -f -g --digest -u "$REBAR_KEY" \
    -X PUT "$REBAR_WEB/api/v2/nodes/$HOSTNAME" \
    -d 'alive=false' \
    -d 'bootenv=local' \
    -d "variant=metal" \
    -d "os_family=linux" \
    -d "arch=$(uname -m)"
echo "Set node not alive"

# And then alive
curl -k -f -g --digest -u "$REBAR_KEY" \
    -X PUT "$REBAR_WEB/api/v2/nodes/$HOSTNAME" \
    -d 'alive=true' \
    -d 'bootenv=local' \
    -d "variant=metal" \
    -d "os_family=linux" \
    -d "arch=$(uname -m)"
echo "Set node alive"

