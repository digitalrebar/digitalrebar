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

bash "create crowbar user for postgres" do
  code "sudo -H -u postgres createuser -d -S -R -w crowbar"
  not_if "sudo -H -u postgres -- psql postgres -tAc \"SELECT 1 FROM pg_roles WHERE rolname='crowbar'\" |grep -q 1"
end

# XXX: One day change this to listen on a non-unix domain socket.
# XXX: One day store the username/password into the consul key/value store.
# XXX: One day use encrypted keys to store the username and password

bash "consul reload" do
  code "consul reload"
  action :nothing
end

template "/etc/consul.d/crowbar-database.json" do
  source "crowbar-database.json.erb"
  notifies :run, "bash[consul reload]", :immediately
end

