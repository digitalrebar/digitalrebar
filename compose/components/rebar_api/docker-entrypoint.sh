#!/bin/bash
set -e
set -x

[[ -d /opt/digitalrebar/core ]] || exit 1
uid="$(stat -c '%u' /opt/digitalrebar/core)"
gid="$(stat -d '%g' /opt/digitalrebar/core)"

find /var /home -xdev -user rebar -exec chown "$uid:$gid" '{}' ';'
find /var /home -xdev -group rebar -exec chown "$uid:$gid" '{}' ';'
usermod -o -u "$uid" rebar
groupmod -o -g "$gid" rebar

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

baseurl="http://127.0.0.1:8500/v1/kv/digitalrebar/private/database/digitalrebar"
token="?token=$CONSUL_M_ACL"
pass=`curl ${baseurl}/password${token}`
while [ "$pass" == "" ] ; do
  sleep 5
  pass=`curl ${baseurl}/password${token}`
done
echo $pass | jq -r .[0].Value | base64 -d > ~/.pgpass
chmod 600 ~/.pgpass
rm -f ~/.pgpass

cat > /etc/consul_m_acl.json <<EOF
{
  "acl_master_token": "${CONSUL_M_ACL}"
}
EOF
chown rebar:rebar /etc/consul_m_acl.json

# Make sure rebar user and ssh are in place
su -l -c 'ssh-keygen -q -b 2048 -P "" -f /home/rebar/.ssh/id_rsa' rebar
mkdir -p /root/.ssh
cd /root/.ssh
touch authorized_keys
cat authorized_keys /home/rebar/.ssh/id_rsa.pub >> authorized_keys.new
sort -u <authorized_keys.new >authorized_keys
rm authorized_keys.new
chmod 600 /root/.ssh/authorized_keys
cd /opt/digitalrebar/core

if [[ $DR_DEV ]]; then
	export RAILS_ENV=development
	export PUMA_CFG=puma-dev.cfg
	touch /tmp/development.txt
else
	export RAILS_ENV=production
	export PUMA_CFG=puma.cfg
fi

rm -f /opt/digitalrebar/core/rails/Gemfile.lock
su -l -c 'cd /opt/digitalrebar/core/rails; bundle install --path /var/cache/rebar/gems --standalone --binstubs /var/cache/rebar/bin' rebar

# Setup database tasks
/opt/digitalrebar/core/setup/00-rebar-rake-tasks.install

# Start up the code
/opt/digitalrebar/core/setup/01-rebar-start.install

# Build initial access keys
/opt/digitalrebar/core/setup/02-make-machine-key.install

./rebar-docker-install.sh

# Add provisioner-service after initial converge.
. /etc/profile.d/rebar*
rebar nodes bind "system-phantom.internal.local" to "provisioner-service"
rebar nodes commit "system-phantom.internal.local"

# Copy out stuff to data dir
cp /etc/profile.d/rebar* /node-data
IP=`ip addr show eth0 | grep inet | grep -v inet6 | awk '{ print $2 }' | awk -F/ '{ print $1 }'`
echo "export REBAR_ENDPOINT=http://$IP:3000" >> /node-data/rebar*

tail -f /var/log/rebar/*.log
