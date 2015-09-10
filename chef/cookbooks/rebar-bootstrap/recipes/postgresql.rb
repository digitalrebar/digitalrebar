#
# Cookbook Name:: rebar-bootstrap
# Recipe:: postgresql
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
#

pg_conf_dir = "/var/lib/pgsql/data"
case node["platform"]
when "ubuntu","debian"
  pg_conf_dir = "/etc/postgresql/9.3/main"
  service "postgresql" do
    action [:enable, :start]
  end
  pg_database_dir = "/var/lib/postgresql/9.3/main/base"
  directory "#{pg_database_dir}" do
    owner "postgres"
  end
when "centos","redhat"
  pg_conf_dir = "/var/lib/pgsql/9.3/data"
  bash "Init the postgresql database" do
    code <<EOC
su -l -c '/usr/pgsql-9.3/bin/initdb --locale=en_US.UTF-8 -D #{pg_conf_dir}' postgres
EOC
    not_if do File.exists?("#{pg_conf_dir}/pg_hba.conf") end
  end

  if File.exists?("/usr/lib/systemd/system/postgresql-9.3.service") &&
     File.exists?("/.dockerenv")
    bash "Disable OOM disablement for Postgresql" do
      code "sed 's/^OOM/#OOM/' </usr/lib/systemd/system/postgresql-9.3.service >/etc/systemd/system/postgresql-9.3.service"
    end
  end
  service "postgresql" do
    service_name "postgresql-9.3"
    action [:enable, :start]
  end
when "opensuse", "suse"
  bash "Init the postgresql database" do
    code <<EOC
su -l -c 'initdb --locale=en_US.UTF-8 -D #{pg_conf_dir}' postgres
sed -i -e '/POSTGRES_DATADIR/ s@=.*$@="#{pg_conf_dir}"@' /etc/sysconfig/postgresql
EOC
    not_if do File.exists?("#{pg_conf_dir}/pg_hba.conf") end
  end
  service "postgresql" do
    action [:enable, :start]
  end
end

# This will configure us to only listen on a local UNIX socket
template "#{pg_conf_dir}/postgresql.conf" do
  source "postgresql.conf.erb"
  notifies :restart, "service[postgresql]", :immediately
end

template  "#{pg_conf_dir}/pg_hba.conf" do
  source "pg_hba.conf.erb"
  notifies :restart, "service[postgresql]",:immediately
end

# Don't allow = in the password
rebar_password=SecureRandom.base64.gsub('=','3')
rebar_user='rebar'
rebar_database='digitalrebar'

if node[:consul][:acl_master_token] && !node[:consul][:acl_master_token].empty?

  # We do not actually use the database token for anything, as the
  # web app and the runners just use the master ACL.  If we change that, then this
  # code should be reenabled.
  if false
    bash "Create database ACL" do
      code <<EOC
echo '{
  "Name": "DigitalRebar Database ACL",
  "Type": "client",
  "Rules": "key \\"digitalrebar/private\\" { policy = \\"deny\\" }\\nkey \\"digitalrebar/private/database/digitalrebar\\" { policy = \\"write\\" }"
}' > /tmp/tmp_cred.json
curl -X PUT -d @/tmp/tmp_cred.json http://localhost:8500/v1/acl/create?token=#{node[:consul][:acl_master_token]}
rm -f /tmp/tmp_cred.json
EOC
    end

    ruby_block "record the database acl" do
      block do
        listStr = %x{curl http://localhost:8500/v1/acl/list?token=#{node[:consul][:acl_master_token]}}
        list = JSON.parse(listStr)
        list.each do |elem|
          if elem['Name'] == 'DigitalRebar Database ACL'
            IO.open("/etc/rebar.database.acl",'w') do |f|
              f.write(elem['ID'])
            end
            break
          end
        end
      end
    end
  end

end

ruby_block "Add keys to consul" do
  block do
    baseurl = "http://127.0.0.1:8500/v1/kv/digitalrebar/private/database/digitalrebar"
    token = nil
    if node[:consul][:acl_master_token] && !node[:consul][:acl_master_token].empty?
      token = "?token=#{node[:consul][:acl_master_token]}"
    end
    http = Net::HTTP.new("127.0.0.1",8500)
    { "database" => rebar_database,
      "username" => rebar_user,
      "password" => rebar_password }.each do |k,v|
      Chef::Log.info("Adding #{k} to Consul for the DigitalRebar database")
      url="#{baseurl}/#{k}"
      url << token if token
      Chef::Log.info("PUTing to #{url}")
      req = Net::HTTP::Put.new(url)
      req.body = v
      response = http.request(req)
      Chef::Log.info("Response code: #{response.code}: #{response.message}")
      Chef::Log.info("Response body: #{response.body}")
      response.value
    end
  end
  not_if "sudo -H -u postgres -- psql postgres -tAc \"SELECT 1 FROM pg_roles WHERE rolname='#{rebar_user}'\" |grep -q 1"
end

bash "create rebar user for postgres" do
  code <<EOC
sudo -H -u postgres createuser -d -E -S -R -w #{rebar_user}
sudo -H -u postgres psql -c "ALTER USER #{rebar_user} WITH ENCRYPTED PASSWORD '#{rebar_password}';"
EOC
  not_if "sudo -H -u postgres -- psql postgres -tAc \"SELECT 1 FROM pg_roles WHERE rolname='#{rebar_user}'\" |grep -q 1"
end

bash "consul reload" do
  code "consul reload"
  action :nothing
end

template "/etc/consul.d/rebar-database.json" do
  source "rebar-database.json.erb"
  notifies :run, "bash[consul reload]", :immediately
end
