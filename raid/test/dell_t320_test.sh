#!/bin/bash


export REBAR_KEY=rebar:rebar1
export REBAR_ENDPOINT=https://127.0.0.1:3000

NODE_ID=$1

if ! rebar nodes show $NODE_ID 2>/dev/null >/dev/null; then
  echo "Missing valid node id"
  exit 1
fi

ROLE_ID=$(rebar roles show raid-configure | jq -r .id)
NR_ID=$(rebar noderoles match "{ \"node_id\": $NODE_ID, \"role_id\": $ROLE_ID }" | jq -r .[].id)

# Make the deployments to hold things around
rebar deployments create default 2>/dev/null >/dev/null
rebar nodes move $NODE_ID to default 2>/dev/null >/dev/null

# Make the node stay on
rebar nodes set $NODE_ID attrib stay_on to '{ "value": true }' 2>/dev/null >/dev/null
rebar nodes commit $NODE_ID attrib stay_on to '{ "value": true }' 2>/dev/null >/dev/null

echo "n=$NODE_ID r=$ROLE_ID nr=$NR_ID"

v="
[
  {
    \"boot\": true,
    \"name\": \"os\",
    \"size\": \"min\",
    \"disks\": 2,
    \"raid_level\": \"raid1\"
  },
  {
    \"size\": \"min\",
    \"name\": \"j1\",
    \"disks\": 1,
    \"raid_level\": \"jbod\"
  },
  {
    \"size\": \"min\",
    \"name\": \"j2\",
    \"disks\": 1,
    \"raid_level\": \"jbod\"
  }
]
"

# Reset the run_count
rebar noderoles update $NR_ID '{ "run_count": 0 }'

rebar nodes propose $NODE_ID
rebar nodes set $NODE_ID attrib raid-wanted-volumes to "{ \"value\": $v }"
rebar nodes commit $NODE_ID
rebar deployments commit system
rebar deployments commit default


