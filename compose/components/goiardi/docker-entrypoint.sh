#!/bin/bash
set -e
set -x

consul agent --join consul --config-dir /etc/consul.d --data-dir /data &

answer=""
# We need consul to converge on a leader.
# This can take a little time, so we ask for
# leader status.  The leader status returns
# nothing, a string that says no leader, or
# the leader IP:port pair.  The default port
# is 8300 for server communication.
while [[ $answer != *:8300* ]]; do
  sleep 1
  answer=`curl http://localhost:8500/v1/status/leader`
  echo "Waiting for consul leader: $answer"
done

baseurl="http://127.0.0.1:8500/v1/kv/opencrowbar/private/database/goiardi"
token="?token=$CONSUL_M_ACL"
pass=`curl ${baseurl}/password${token}`
while [ "$pass" == "" ] ; do
  sleep 5
  pass=`curl ${baseurl}/password${token}`
done
echo $pass | jq -r .[0].Value | base64 -d > ~/.pgpass
chmod 600 ~/.pgpass
rm -f ~/.pgpass

tmpdir=$(mktemp -d /tmp/sqitch-XXXXXX)
cd "$tmpdir"
cp -a "/go/src/github.com/ctdk/goiardi/sql-files/postgres-bundle/"* .
cd $tmpdir

sqitch target add goiardi db:pg://$POSTGRES_USER:$POSTGRES_PASSWORD@database/goiardi
sqitch deploy goiardi

export PGPASSWORD=$POSTGRES_PASSWORD
psql -U $POSTGRES_USER -h database goiardi -c "grant all on database goiardi to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on schema public to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all tables in schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all tables in schema public to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all sequences in schema public to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all sequences in schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all functions in schema goiardi to goiardi;"
psql -U $POSTGRES_USER -h database goiardi -c "grant all on all functions in schema public to goiardi;"
cd -
rm -rf "$tmpdir"

# GREG: Get the database parameters and put them in the file.

# Use my IP as my hostname
IP=`ip addr show eth0 | grep inet | grep -v inet6 | awk '{ print $2 }' | awk -F/ '{ print $1 }'`
echo "hostname=\"$IP\"" > /tmp/goiardi.conf
cat /etc/goiardi/goiardi.conf >> /tmp/goiardi.conf
cp /tmp/goiardi.conf /etc/goiardi/goiardi.conf
rm -f /tmp/goiardi.conf

/go/bin/goiardi -c /etc/goiardi/goiardi.conf &

while [ ! -e /etc/goiardi/admin.pem ] ; do
  echo "Waiting for goiardi to start"
  sleep 5
done

mkdir -p /etc/chef

cd /tmp
KEYFILE="/tmp/rebar.pem"
EDITOR=/bin/true knife client create crowbar \
   -s http://localhost:4646 \
   -a --file "$KEYFILE" -u admin \
   -k /etc/goiardi/admin.pem

# Store crowbar pem file.
baseurl="http://127.0.0.1:8500/v1/kv/opencrowbar/private/chef/system"
token="?token=$CONSUL_M_ACL"
curl --data-binary "$(cat $KEYFILE)" -X PUT ${baseurl}/pem${token}
curl --data-binary "crowbar" -X PUT ${baseurl}/account${token}
curl --data-binary "http" -X PUT ${baseurl}/proto${token}

rm -f $KEYFILE

while [ true ] ; do
  sleep 30
done
