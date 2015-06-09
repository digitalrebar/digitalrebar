#
# Cookbook Name:: crowbar-bootstrap
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
crowbar_password=SecureRandom.base64.gsub('=','3')
crowbar_user='crowbar'
crowbar_database='opencrowbar'

if node[:consul][:acl_master_token] && !node[:consul][:acl_master_token].empty?

  # This is needed to let the Crowbar API and UI read the default.json to get the
  # master ACL token.
  execute "Set access rights on default.json" do
    command "setfacl -m crowbar:r /etc/consul.d/default.json"
  end

  # Create an opencrowbar private keyspace under opencrowbar/private
  # One day we may want to require tokens for all, but ..
  bash "Update anonymous ACL" do
    code <<EOC
echo '{
  "ID": "anonymous",
  "Type": "client",
  "Rules": "key \\"opencrowbar/private\\" { policy = \\"deny\\" }"
}' > /tmp/tmp_cred.json
EOF
curl -X PUT -d @/tmp/tmp_cred.json http://localhost:8500/v1/acl/update?token=#{node[:consul][:acl_master_token]}
rm -f /tmp/tmp_cred.json
EOC
    not_if { node[:consul][:acl_master_token].nil? || node[:consul][:acl_master_token].empty? }
  end


  # We do not actually use the database token for anything, as the
  # web app and the runners just use the master ACL.  If we change that, then this
  # code should be reenabled.
  if false
    bash "Create database ACL" do
      code <<EOC
echo '{
  "Name": "OpenCrowbar Database ACL",
  "Type": "client",
  "Rules": "key \\"opencrowbar/private\\" { policy = \\"deny\\" }\\nkey \\"opencrowbar/private/database/opencrowbar\\" { policy = \\"write\\" }"
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
          if elem['Name'] == 'OpenCrowbar Database ACL'
            IO.open("/etc/crowbar.database.acl",'w') do |f|
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
    baseurl = "http://127.0.0.1:8500/v1/kv/opencrowbar/private/database/opencrowbar"
    token = nil
    if node[:consul][:acl_master_token] && !node[:consul][:acl_master_token].empty?
      token = "?token=#{node[:consul][:acl_master_token]}"
    end
    http = Net::HTTP.new("127.0.0.1",8500)
    { "database" => crowbar_database,
      "username" => crowbar_user,
      "password" => crowbar_password }.each do |k,v|
      Chef::Log.info("Adding #{k} to Consul for the OpenCrowbar database")
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
  not_if "sudo -H -u postgres -- psql postgres -tAc \"SELECT 1 FROM pg_roles WHERE rolname='#{crowbar_user}'\" |grep -q 1"
end

bash "create crowbar user for postgres" do
  code <<EOC
sudo -H -u postgres createuser -d -E -S -R -w #{crowbar_user}
sudo -H -u postgres psql -c "ALTER USER #{crowbar_user} WITH ENCRYPTED PASSWORD '#{crowbar_password}';"
EOC
  not_if "sudo -H -u postgres -- psql postgres -tAc \"SELECT 1 FROM pg_roles WHERE rolname='#{crowbar_user}'\" |grep -q 1"
end

bash "consul reload" do
  code "consul reload"
  action :nothing
end

template "/etc/consul.d/crowbar-database.json" do
  source "crowbar-database.json.erb"
  notifies :run, "bash[consul reload]", :immediately
end
