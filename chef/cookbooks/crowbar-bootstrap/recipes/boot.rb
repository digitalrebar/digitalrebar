#
# Cookbook Name:: crowbar-bootstrap
# Recipe:: default
#
# Copyright (C) 2014 Dell, Inc.
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

tftproot = node["bootstrap"]["tftproot"]
tracedir = node["bootstrap"]["tracedir"]
directory tracedir do
  recursive true
end

crowbar_yml = node["bootstrap"]["crowbar_yml"]
 
unless File.exists?(crowbar_yml)
  raise "No crowbar checkout to bootstrap!"
end

prereqs = YAML.load(File.open(crowbar_yml))

os_token="#{node["platform"]}-#{node["platform_version"]}"

os_pkg_type = case node["platform"]
              when "debian","ubuntu" then "debs"
              when "centos","redhat","opensuse","suse","fedora" then "rpms"
              else
                raise "Cannot figure out what package type we should use!"
              end

unless prereqs["os_support"].member?(os_token)
  raise "Cannot install crowbar on #{os_token}!  Can only install on one of #{prereqs["os_support"].join(" ")}"
end

repos = []
pkgs = []
raw_pkgs = []
gems = []
extra_files = []

# Find all the upstream repos and packages we will need.

Dir.glob("/opt/opencrowbar/**/crowbar.yml").each do |prereq_file|
  prereqs = YAML.load(File.open(prereq_file))
  if prereqs[os_pkg_type]
    if prereqs[os_pkg_type][os_token]
      repos << prereqs[os_pkg_type][os_token]["repos"]
      pkgs << prereqs[os_pkg_type][os_token]["build_pkgs"]
      pkgs << prereqs[os_pkg_type][os_token]["required_pkgs"]
      raw_pkgs << prereqs[os_pkg_type][os_token]["raw_pkgs"]
    end
    repos << prereqs[os_pkg_type]["repos"]
    pkgs << prereqs[os_pkg_type]["build_pkgs"]
    pkgs << prereqs[os_pkg_type]["required_pkgs"]
    raw_pkgs << prereqs[os_pkg_type]["raw_pkgs"]
  end
  if prereqs["gems"] && prereqs["gems"]["required_pkgs"]
    gems << prereqs["gems"]["required_pkgs"]
  end

  extra_files << prereqs["extra_files"]
end

Chef::Log.debug(repos)
Chef::Log.debug(pkgs)

repos.flatten!
repos.compact!
repos.uniq!
pkgs.flatten!
pkgs.compact!
pkgs.uniq!
pkgs.sort!
raw_pkgs.flatten!
raw_pkgs.compact!
raw_pkgs.uniq!
raw_pkgs.sort!
gems.flatten!
gems.compact!
gems.uniq!
extra_files.flatten!
extra_files.compact!
extra_files.uniq!

Chef::Log.debug(repos)

node.normal[:bootstrap][:gems] = gems

proxies = Hash.new
["http_proxy","https_proxy","no_proxy"].each do |p|
  next unless ENV[p] && !ENV[p].strip.empty?
  Chef::Log.info("Using #{p}='#{ENV[p]}'")
  proxies[p]=ENV[p].strip
end
unless proxies.empty?
  # Hack up /etc/environment to hold our proxy environment info
  template "/etc/environment" do
    source "environment.erb"
    variables(:values => proxies)
  end

  template "/etc/profile.d/proxy.sh" do
    source "proxy.sh.erb"
    variables(:values => proxies)
  end

  case node["platform"]
  when "redhat","centos"
    template "/etc/yum.conf" do
      source "yum.conf.erb"
      variables(
                :distro => node["platform"],
                :proxy => proxies["http_proxy"]
                )
    end
    bash "Disable fastestmirrors plugin" do
      code "sed -i '/enabled/ s/1/0/' /etc/yum/pluginconf.d/fastestmirror.conf"
    end
  end
end

file "#{tracedir}/install_pkgs" do
  action :nothing
end

template "/etc/gemrc" do
  source "gemrc.erb"
  variables(:proxy => ENV["http_proxy"])
end

template "#{tracedir}/required_pkgs" do
  source "required_pkgs.erb"
  variables( :pkgs => pkgs )
  notifies :create_if_missing, "file[#{tracedir}/install_pkgs]",:immediately
end

repofile_path = case node["platform"]
                when "centos","redhat" then "/etc/yum.repos.d"
                when "suse","opensuse" then "/etc/zypp/repos.d"
                else raise "Don't know where to put repo files for #{node["platform"]}'"
                end

case node["platform"]
when "debian","ubuntu"
  template "/etc/apt/sources.list.d/crowbar.list" do
    source "crowbar.list.erb"
    variables( :repos => repos )
    notifies :create_if_missing, "file[#{tracedir}/install_pkgs]",:immediately
  end
when "centos","redhat","suse","opensuse","fedora"
  # Docker images do not have this, but the postgresql init script insists on it being present.
  file "/etc/sysconfig/network" do
    action :create
    content "NETWORKING=yes"
  end if !File.file?("/etc/sysconfig/network")
  # We want a better Ruby on the admin nodes, but should not muck up
  # all the other nodes.
  if /^(centos|redhat)-6/ =~ os_token
    repos << "bare ruby 20 http://opencrowbar.s3-website-us-east-1.amazonaws.com/ruby"
  end
  repos.each do |repo|
    rtype,rdest = repo.split(" ",2)
    case rtype
    when "rpm"
      rpm_file = rdest.split("/")[-1]
      bash "Install #{rpm_file}" do
        code "rpm -Uvh #{tracedir}/#{rpm_file}"
        action :nothing
        ignore_failure true
        notifies :create_if_missing, "file[#{tracedir}/install_pkgs]",:immediately
      end

      bash "Fetch #{rpm_file}" do
        code "curl -fgL -o '#{tracedir}/#{rpm_file}' '#{rdest}'"
        not_if "test -f '#{tracedir}/#{rpm_file}'"
        notifies :run, "bash[Install #{rpm_file}]",:immediately
      end

    when "bare"
      rname, rprio, rurl = rdest.split(" ",3)
      template "#{repofile_path}/crowbar-#{rname}.repo" do
        source "crowbar.repo.erb"
        variables(
                  :repo_name => rname,
                  :repo_prio => rprio,
                  :repo_url => rurl
                  )
        notifies :create_if_missing, "file[#{tracedir}/install_pkgs]",:immediately
      end
    when "repo"
      rurl,rname = rdest.split(" ",2)
      template "#{repofile_path}/crowbar-#{rname}.repo" do
        source "crowbar.repo.erb"
        variables(
                  :repo_name => rname,
                  :repo_prio => 20,
                  :repo_url => rurl
                  )
        notifies :create_if_missing, "file[#{tracedir}/install_pkgs]",:immediately
      end
    else
      raise "#{node["platform"]}: Unknown repo type #{rtype}"
    end
  end
else
  raise "Don't know how to update repositories for #{node["platform"]}"
end

unless raw_pkgs.empty?
  dest = "/tftpboot/#{os_token}/crowbar-extra/raw_pkgs"
  FileUtils.mkdir_p(dest)
  bash "Trigger raw_pkg metadata update" do
    code "touch #{dest}/canary"
    action :nothing
  end
  raw_pkgs.each do |pkg|
    next if File.file?("#{dest}/#{pkg.split('/')[-1]}")
    bash "Fetch #{pkg}" do
      code "curl -fgL -o '#{dest}/#{pkg.split('/')[-1]}' '#{pkg}'"
      notifies :run, "bash[Trigger raw_pkg metadata update]", :immediately
    end
  end
  bash "Update package metadata in #{dest}" do
    cwd dest
    code <<EOC
[[ -f "#{dest}/canary" ]] || exit 0
case #{os_pkg_type} in
    debs) apt-get -y install dpkg-dev; dpkg-scanpackages . |gzip -9 >Packages.gz;;
    rpms) yum -y install createrepo; createrepo .;;
    *) echo "Cannot create package metadata for #{os_pkg_type}"
       exit 1;;
esac
rm "#{dest}/canary"
EOC
  end
  case node["platform"]
  when "centos","redhat","suse","opensuse","fedora"
    template "#{repofile_path}/crowbar-raw_pkgs.repo" do
      source "crowbar.repo.erb"
      variables(
                :repo_name => "raw_pkgs",
                :repo_prio => 20,
                :repo_url => "file:///tftpboot/#{os_token}/crowbar-extra/raw_pkgs"
                )
    end
  when "debian","ubuntu"
    template "/etc/apt/sources.list.d/crowbar.list" do
      source "crowbar.list.erb"
      variables( :repos => repos )
      notifies :create_if_missing, "file[#{tracedir}/install_pkgs]",:immediately
    end
  else
    raise "Don't know how to create raw_pkgs repo on #{node["platform"]}"
  end
end

bash "Install required files" do
  code case node["platform"]
       when "ubuntu","debian" then "apt-get -y update && apt-get -y --force-yes install #{pkgs.join(" ")} && rm #{tracedir}/install_pkgs"
       when "centos","redhat","fedora" then "yum -y install #{pkgs.join(" ")} && rm #{tracedir}/install_pkgs"
       when "suse","opensuse" then "zypper -n install --no-recommends #{pkgs.join(" ")} && rm #{tracedir}/install_pkgs"
       else raise "Don't know how to install required files for #{node["platform"]}'"
       end
  only_if do ::File.exists?("#{tracedir}/install_pkgs") end
end

extra_files.each do |f|
  src, dest = f.strip.split(" ",2)
  target_dir = "#{tftproot}/files/#{dest}"
  target = "#{target_dir}/#{src.split("/")[-1]}"
  next if File.exists?(target)
  Chef::Log.info("Installing extra file '#{src}' into '#{target}'")
  directory target_dir do
    action :create
    recursive true
  end
  bash "#{target}: Fetch #{src}" do
    code "curl -fgL -o '#{target}' '#{src}'"
  end
end

directory "/var/run/sshd" do
  mode 0755
  owner "root"
  recursive true
end

bash "Regenerate Host SSH keys" do
  code "ssh-keygen -q -b 2048 -P '' -f /etc/ssh/ssh_host_rsa_key"
  not_if "test -f /etc/ssh/ssh_host_rsa_key"
end

bash "Unlock the root account" do
  code "while grep -q '^root:!' /etc/shadow; do usermod -U root; done"
end

# We need Special Hackery to run sshd in docker.
if File.file?("/.dockerenv")
  service "ssh" do
    service_name "sshd" if node["platform"] == "centos"
    start_command "/usr/sbin/sshd"
    stop_command "pkill -9 sshd"
    status_command "pgrep sshd"
    restart_command "pkill -9 sshd && /usr/sbin/sshd"
    action [:start]
  end
else
  service "ssh" do
    service_name "sshd" if node["platform"] == "centos"
    action [:enable, :start]
  end
end

directory "/root/.ssh" do
  action :create
  recursive true
  owner "root"
  mode 0755
end

bash "Enable root access" do
  cwd "/root/.ssh"
  code <<EOC
cat authorized_keys /home/crowbar/.ssh/id_rsa.pub >> authorized_keys.new
sort -u <authorized_keys.new >authorized_keys
rm authorized_keys.new
EOC
end

template "/etc/ssh/sshd_config" do
  source "sshd_config.erb"
  action :create
  notifies :restart, 'service[ssh]', :immediately
end

# Why does opensuse consider ping to be a security risk?
bash "Allow everyone to ping" do 
  code "chmod u+s $(which ping) $(which ping6)"
end

user "crowbar" do
  home "/home/crowbar"
  action :create
  password '$6$afAL.34B$T2WR6zycEe2q3DktVtbH2orOroblhR6uCdo5n3jxLsm47PBm9lwygTbv3AjcmGDnvlh0y83u2yprET8g9/mve.'
  shell "/bin/bash"
  supports :manage_home => true
end

group "crowbar" do
  action :create
  append true
  members "crowbar"
end

directory "/home/crowbar/.ssh" do
  action :create
  owner "crowbar"
  group "crowbar"
  mode 0755
end

bash "Regenerate Crowbar SSH keys" do
  code "su -l -c 'ssh-keygen -q -b 2048 -P \"\" -f /home/crowbar/.ssh/id_rsa' crowbar"
  not_if "test -f /home/crowbar/.ssh/id_rsa"
end

bash "Enable root access" do
  cwd "/root/.ssh"
  code <<EOC
cat authorized_keys /home/crowbar/.ssh/id_rsa.pub >> authorized_keys.new
sort -u <authorized_keys.new >authorized_keys
rm authorized_keys.new
EOC
end

template "/home/crowbar/.ssh/config" do
  source "ssh_config.erb"
  owner "crowbar"
  group "crowbar"
  mode 0644
end

template "/etc/ssh/sshd_config" do
  source "sshd_config.erb"
  action :create
  notifies :restart, 'service[ssh]', :immediately
end

template "/etc/sudoers.d/crowbar" do
  source "crowbar_sudoer.erb"
  mode 0440
end

# Sigh, we need this so that the pg gem will install correctly
bash "Make sure pg_config is in the PATH" do
  code "ln -sf /usr/pgsql-9.3/bin/pg_config /usr/local/bin/pg_config"
  not_if "which pg_config"
end
