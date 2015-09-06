#
# Cookbook Name:: crowbar-bootstrap
# Recipe:: goiardi
#
# Copyright (C) 2014 Victor Lowther
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

goiardi_repo=node["bootstrap"]["goiardi"]["repo"]
goiardi_port=node["bootstrap"]["goiardi"]["port"]
goiardi_protocol=node["bootstrap"]["goiardi"]["protocol"]
ENV["GOPATH"]=node["bootstrap"]["gopath"]
goiardi_src="#{ENV["GOPATH"]}/src/#{goiardi_repo}"

template "/etc/goiardi/goiardi.conf" do
  source "goiardi.conf.erb"
  variables address: '::',
            port: goiardi_port,
            protocol: goiardi_protocol,
            conf_root: "/etc/goiardi",
            use_auth: true,
            hostname: node[:machinename],
            local_filestore: "/var/cache/goiardi",
            index_file: "/var/cache/goiardi/index.bin"
end

bash "Create self-signed certificates for goiardi" do
  cwd "/etc/goiardi"
  code <<EOC
openssl req -nodes -sha256 -x509 -newkey rsa:2048 \
    -keyout https-key.pem -out https-cert.pem -days 1001 \
    -subj "/C=US/ST=Denial/L=Anytown/O=Dis/CN=admin"
chmod 600 *.pem
EOC
  not_if { goiardi_protocol != "https" || File.exists?("/etc/goiardi/key.pem") }
end

bash "Create database for goiardi" do
  code <<EOC
set -e
su -c 'createuser goiardi' postgres
su -c 'createdb goiardi -O goiardi' postgres
EOC
  not_if "su -c 'psql goiardi -c \"select 1;\"' postgres"
end

bash "Populate goiardi database" do
  code <<EOC
set -e
tmpdir=$(mktemp -d /tmp/sqitch-XXXXXX)
cd "$tmpdir"
cp -a "#{goiardi_src}/sql-files/postgres-bundle/"* .
chown -R postgres .
su -c "cd $tmpdir; sqitch deploy db:pg:goiardi" postgres
su -c 'psql goiardi -c "grant all on database goiardi to goiardi;"' postgres
su -c 'psql goiardi -c "grant all on schema goiardi to goiardi;"' postgres
su -c 'psql goiardi -c "grant all on schema public to goiardi;"' postgres
su -c 'psql goiardi -c "grant all on all tables in schema goiardi to goiardi;"' postgres
su -c 'psql goiardi -c "grant all on all tables in schema public to goiardi;"' postgres
su -c 'psql goiardi -c "grant all on all sequences in schema public to goiardi;"' postgres
su -c 'psql goiardi -c "grant all on all sequences in schema goiardi to goiardi;"' postgres
su -c 'psql goiardi -c "grant all on all functions in schema goiardi to goiardi;"' postgres
su -c 'psql goiardi -c "grant all on all functions in schema public to goiardi;"' postgres
cd ..
rm -rf "$tmpdir"
EOC
end

template "/etc/init.d/goiardi" do
  mode "0755"
  source "goiardi.init.erb"
end

service "goiardi" do
  action [:enable, :start]
end

ruby_block "Wait for goiardi to start" do
  block do
    s = 1
    while (s < 32) && !File.exists?("/etc/goiardi/admin.pem")
      sleep s
      s <<= 1
    end
    raise "Goiardi never created access keys!" unless File.exists?("/etc/goiardi/admin.pem")
  end
end

directory "/etc/chef" do
  action :create
end

account="crowbar"
keyfile="/root/#{account}.pem"
token = nil
if node[:consul][:acl_master_token] && !node[:consul][:acl_master_token].empty?
  token = "?token=#{node[:consul][:acl_master_token]}"
end
baseurl="http://127.0.0.1:8500/v1/kv/opencrowbar/private/chef/system"

bash "Insert into consul" do
  code <<EOC
curl --data-binary "$(cat #{keyfile})" -X PUT #{baseurl}/pem#{token}
curl --data-binary "#{account}" -X PUT #{baseurl}/account#{token}
curl --data-binary "#{goiardi_protocol}" -X PUT #{baseurl}/proto#{token}
EOC
  action :nothing
end

bash "Create admin client for Crowbar user" do
  code <<EOC
mkdir /home/crowbar/.chef
admin_client="#{account}"
KEYFILE="#{keyfile}"
EDITOR=/bin/true knife client create "$admin_client" \
    -s #{goiardi_protocol}://localhost:#{goiardi_port} \
    -a --file "$KEYFILE" -u admin \
    -k /etc/goiardi/admin.pem
chown -R crowbar:crowbar /home/crowbar/.chef
EOC
  not_if { File.exists?(keyfile) }
  notifies :run, "bash[Insert into consul]", :immediately
end

bash "consul reload" do
  code "consul reload"
  action :nothing
end

template "/etc/consul.d/crowbar-chef.json" do
  source "crowbar-chef.json.erb"
  notifies :run, "bash[consul reload]", :immediately
end

