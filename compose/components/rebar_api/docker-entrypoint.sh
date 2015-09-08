#!/bin/bash
set -e
set -x

chown -R crowbar:crowbar /opt/opencrowbar

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

baseurl="http://127.0.0.1:8500/v1/kv/opencrowbar/private/database/opencrowbar"
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
chown crowbar:crowbar /etc/consul_m_acl.json

# Make sure crowbar user and ssh are in place
su -l -c 'ssh-keygen -q -b 2048 -P "" -f /home/crowbar/.ssh/id_rsa' crowbar
mkdir -p /root/.ssh
cd /root/.ssh
touch authorized_keys
cat authorized_keys /home/crowbar/.ssh/id_rsa.pub >> authorized_keys.new
sort -u <authorized_keys.new >authorized_keys
rm authorized_keys.new
chmod 600 /root/.ssh/authorized_keys
cd /opt/opencrowbar/core

if [[ $DR_DEV ]]; then
	export RAILS_ENV=development
	export PUMA_CFG=puma-dev.cfg
	touch /tmp/development.txt
else
	export RAILS_ENV=production
	export PUMA_CFG=puma.cfg
fi

rm -f /opt/opencrowbar/core/rails/Gemfile.lock
su -l -c 'cd /opt/opencrowbar/core/rails; bundle install --path /var/cache/crowbar/gems --standalone --binstubs /var/cache/crowbar/bin' crowbar

# Setup database tasks
/opt/opencrowbar/core/setup/00-crowbar-rake-tasks.install

# Start up the code
/opt/opencrowbar/core/setup/01-crowbar-start.install

# Build initial access keys
/opt/opencrowbar/core/setup/02-make-machine-key.install

./crowbar-docker-install.sh

# Add provisioner-service after initial converge.
. /etc/profile.d/crowbar*
crowbar nodes bind "system-phantom.internal.local" to "provisioner-service"
crowbar nodes commit "system-phantom.internal.local"

# Copy out stuff to data dir
cp /etc/profile.d/crowbar* /node-data
IP=`ip addr show eth0 | grep inet | grep -v inet6 | awk '{ print $2 }' | awk -F/ '{ print $1 }'`
echo "export CROWBAR_ENDPOINT=http://$IP:3000" >> /node-data/crowbar*

tail -f /var/log/crowbar/*.log
