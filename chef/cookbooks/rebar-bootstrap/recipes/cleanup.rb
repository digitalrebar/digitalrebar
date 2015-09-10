bash "Upgrade all the things" do
  code case node["platform"]
       when "ubuntu","debian" then "apt-get -y --force-yes dist-upgrade"
       when "centos","redhat","fedora" then "yum -y upgrade"
       when "suse","opensuse" then "zypper -n upgrade"
       else raise "Don't know how to install required files for #{node["platform"]}'"
       end
  only_if do ::File.exists?("/tmp/install_pkgs") end
end

[
  "/root/.ssh",
  "/root/openwsman",
  "/home/rebar/.ssh",
  "/var/cache/rebar/rails-cache",
  "/var/log/rebar",
  "/var/cache/yum",
  "/var/cache/apt/archives",
  "/rpmbuild"
].each do |target|
  directory target do
    action :delete
    recursive true
  end
end

# Clear out /tmp and /var/tmp
["/tmp", "/var/tmp"].each do |target|
  bash "Clean out #{target}" do
    code "rm -rf *"
    cwd target
  end
end

service "postgresql" do
  action :stop
end

["/etc/environment","/etc/yum.conf"].each do |f|
  next unless File.file?(f)
  bash "Clean proxies from #{f}" do
    code "grep -v proxy '#{f}' > '#{f}.cleaned'; rm '#{f}'; mv '#{f}.cleaned' '#{f}'"
  end
end

file "/etc/profile.d/proxy.sh" do
  action :delete
end

bash "Clean up history files" do
  code "find / -type f -name '.*history' -delete"
end
