# Copyright 2011, Dell
# Copyright 2012, SUSE Linux Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied
# See the License for the specific language governing permissions and
# limitations under the License
#
# This recipe sets up the general environmnet needed to PXE boot
# other servers.

admin_ip = node.address.addr
domain_name = node["dns"].nil? ? node["domain"] : (node["dns"]["domain"] || node["domain"])
Chef::Log.info("Provisioner: raw server data #{ node["crowbar"]["provisioner"]["server"] }")

provisioner_web = node["crowbar"]["provisioner"]["server"]["webservers"].first["url"]
api_server=node['crowbar']['api']['servers'].first["url"]

machine_key = node["crowbar"]["machine_key"]

use_local_security = node["crowbar"]["provisioner"]["server"]["use_local_security"]
os_token="#{node["platform"]}-#{node["platform_version"]}"
tftproot =  node["crowbar"]["provisioner"]["server"]["root"]
discover_dir="#{tftproot}/discovery"
pxecfg_dir="#{discover_dir}/pxelinux.cfg"
uefi_dir=discover_dir
pxecfg_default="#{pxecfg_dir}/default"

# Build base sledgehammer kernel args
sledge_args = Array.new
sledge_args << "rootflags=loop"
sledge_args << "initrd=initrd0.img"
sledge_args << "root=live:/sledgehammer.iso"
sledge_args << "rootfstype=auto"
sledge_args << "ro"
sledge_args << "liveimg"
sledge_args << "rd_NO_LUKS"
sledge_args << "rd_NO_MD"
sledge_args << "rd_NO_DM"
if node["crowbar"]["provisioner"]["server"]["use_serial_console"]
  sledge_args << "console=tty0 console=ttyS1,115200n8"
end
sledge_args << "provisioner.web=#{provisioner_web}"
sledge_args << "crowbar.web=#{api_server}"
sledge_args << "crowbar.dns.domain=#{node["crowbar"]["dns"]["domain"]}"
sledge_args << "crowbar.dns.servers=#{node["crowbar"]["dns"]["nameservers"].join(',')}"

node.normal["crowbar"]["provisioner"]["server"]["sledgehammer_kernel_params"] = sledge_args.join(" ")
append_line = node["crowbar"]["provisioner"]["server"]["sledgehammer_kernel_params"]

# By default, install the same OS that the admin node is running
# If the comitted proposal has a defualt, try it.
# Otherwise use the OS the provisioner node is using.

bash "Set up selinux contexts for #{tftproot}" do
  code <<EOC
semanage fcontext -a -f '' -t public_content_t "#{tftproot}"
semanage fcontext -a -f '' -t public_content_t "#{tftproot}(/.*)?"
EOC
  only_if "which selinuxenabled && selinuxenabled"
  not_if "ls -adZ #{tftproot} |grep -q public_content_t"
end

unless default = node["crowbar"]["provisioner"]["server"]["default_os"]
  node.normal["crowbar"]["provisioner"]["server"]["default_os"] = default = os_token
end

unless node.normal["crowbar"]["provisioner"]["server"]["repositories"]
  node.normal["crowbar"]["provisioner"]["server"]["repositories"] = Mash.new
end
node.normal["crowbar"]["provisioner"]["server"]["available_oses"] = Mash.new

directory "#{pxecfg_dir}" do
  action :create
  recursive true
end

directory "#{tftproot}/nodes" do
  action :create
  recursive true
end

cookbook_file "#{tftproot}/nodes/start-up.sh" do
  source "start-up.sh"
  action :create
end

template "#{pxecfg_dir}/default" do
  mode 0644
  owner "root"
  group "root"
  source "default.erb"
  variables(:append_line => "#{append_line} crowbar.state=discovery crowbar.install.key=#{machine_key}",
            :install_name => "discovery",
            :initrd => "initrd0.img",
            :machine_key => machine_key,
            :kernel => "vmlinuz0")
end

# Do uefi as well.
template "#{uefi_dir}/elilo.conf" do
  mode 0644
  owner "root"
  group "root"
  source "default.elilo.erb"
  variables(:append_line => "#{append_line} crowbar.state=discovery",
            :install_name => "discovery",
            :initrd => "initrd0.img",
            :machine_key => machine_key,
            :kernel => "vmlinuz0")
end

node["crowbar"]["provisioner"]["server"]["supported_oses"].each do |os,params|
  web_path = "#{provisioner_web}/#{os}"
  admin_web = os_install_site = "#{web_path}/install"
  crowbar_repo_web="#{web_path}/crowbar-extra"
  os_dir="#{tftproot}/#{os}"
  os_install_dir = "#{os_dir}/install"
  iso_dir="#{tftproot}/isos"
  os_codename=node["lsb"]["codename"]
  role="#{os}_install"
  initrd = params["initrd"]
  kernel = params["kernel"]

  # Don't bother for OSes that are not actaully present on the provisioner node.
  next unless File.file?("#{iso_dir}/#{params["iso_file"]}") or
    File.directory?(os_install_dir)
  node.normal["crowbar"]["provisioner"]["server"]["available_oses"][os] = true
  node.normal["crowbar"]["provisioner"]["server"]["repositories"][os] = Mash.new

  if os =~ /^(esxi)/
    # Extract esxi iso through rsync - bsdtar messes up the filenames
    tmpesxi="/tmp/esxi_mnt_pt/"
    bash "Extract rsync #{params["iso_file"]}" do
      code <<EOC
set -e
[[ -d "#{os_install_dir}.extracting" ]] && rm -rf "#{os_install_dir}.extracting"
mkdir -p "#{os_install_dir}.extracting"

mkdir -v #{tmpesxi}
mount -o loop "#{iso_dir}/#{params["iso_file"]}" #{tmpesxi}
rsync -av #{tmpesxi} "#{os_install_dir}.extracting"
sync && umount #{tmpesxi} && rmdir -v #{tmpesxi}
losetup -j "#{iso_dir}/#{params["iso_file"]}" | awk -F: '{ print $1 }' | xargs losetup -d

chmod +w "#{os_install_dir}.extracting"/*
sed -e "s:/::g" -e "3s:^:prefix=/../#{os}/install/\\n:" -i.bak "#{os_install_dir}.extracting"/boot.cfg

touch "#{os_install_dir}.extracting/.#{params["iso_file"]}.crowbar_canary"
[[ -d "#{os_install_dir}" ]] && rm -rf "#{os_install_dir}"
mv "#{os_install_dir}.extracting" "#{os_install_dir}"
EOC
      only_if do File.file?("#{iso_dir}/#{params["iso_file"]}") &&
          !File.file?("#{os_install_dir}/.#{params["iso_file"]}.crowbar_canary") end
    end
  else
    # Extract the ISO install image.
    # Do so in such a way the we avoid using loopback mounts and get
    # proper filenames in the end.
    bash "Extract #{params["iso_file"]}" do
      code <<EOC
set -e
[[ -d "#{os_install_dir}.extracting" ]] && rm -rf "#{os_install_dir}.extracting"
mkdir -p "#{os_install_dir}.extracting"
(cd "#{os_install_dir}.extracting"; bsdtar -x -f "#{iso_dir}/#{params["iso_file"]}")
touch "#{os_install_dir}.extracting/.#{params["iso_file"]}.crowbar_canary"
[[ -d "#{os_install_dir}" ]] && rm -rf "#{os_install_dir}"
mv "#{os_install_dir}.extracting" "#{os_install_dir}"
EOC
      only_if do File.file?("#{iso_dir}/#{params["iso_file"]}") &&
          !File.file?("#{os_install_dir}/.#{params["iso_file"]}.crowbar_canary") end
    end
  end

  #
  # TODO:Make generic NFS one day
  # Make sure we setup an nfs server and export the fuel directory
  #
  if os =~ /^(fuel)/
    package "nfs-utils"

    service "rpcbind" do
      action [ :enable, :start ]
    end

    service "nfs" do
      action [ :enable, :start ]
    end

    utils_line "#{os_install_dir} *(ro,async,no_subtree_check,no_root_squash,crossmnt)" do
      action :add
      file '/etc/exports'
      notifies :restart, "service[nfs]", :delayed
    end
  end

  # For CentOS and RHEL, we need to rewrite the package metadata
  # to make sure it does not refer to packages that do not exist on the first DVD.
  bash "Rewrite package repo metadata for #{params["iso_file"]}" do
    cwd os_install_dir
    code <<EOC
set -e

mv repodata/*comps*.xml ./comps.xml
createrepo -g ./comps.xml .
touch "repodata/.#{params["iso_file"]}.crowbar_canary"
EOC
    not_if do File.file?("#{os_install_dir}/repodata/.#{params["iso_file"]}.crowbar_canary") end
    only_if do os =~ /^(redhat|centos|fedora)/ end
  end
  
  # Figure out what package type the OS takes.  This is relatively hardcoded.
  pkgtype = case
            when os =~ /^(ubuntu|debian)/ then "debs"
            when os =~ /^(redhat|centos|suse|fedora)/ then "rpms"
            when os =~ /^(coreos|fuel|esxi|xenserver)/ then "custom"
            else raise "Unknown OS type #{os}"
            end
  # If we are running in online mode, we need to do a few extra tasks.
  if node["crowbar"]["provisioner"]["server"]["online"]
    # This information needs to be saved much earlier.
     node["crowbar"]["provisioner"]["barclamps"].each do |bc|
      # Populate our known online repos.
      if bc[pkgtype]
        bc[pkgtype]["repos"].each do |repo|
          node.normal["crowbar"]["provisioner"]["server"]["repositories"][os]["online"] ||= Mash.new
          node.normal["crowbar"]["provisioner"]["server"]["repositories"][os]["online"][repo] = true
        end if bc[pkgtype]["repos"]
        bc[pkgtype][os]["repos"].each do |repo|
          node.normal["crowbar"]["provisioner"]["server"]["repositories"][os]["online"] ||= Mash.new
          node.normal["crowbar"]["provisioner"]["server"]["repositories"][os]["online"][repo] = true
        end if (bc[pkgtype][os]["repos"] rescue nil)
      end
      # Download and create local packages repositories for any raw_pkgs for this OS.
      if (bc[pkgtype][os]["raw_pkgs"] rescue nil)
        destdir = "#{os_dir}/crowbar-extra/raw_pkgs"

        directory destdir do
          action :create
          recursive true
        end

        bash "Delete #{destdir}/gen_meta" do
          code "rm -f #{destdir}/gen_meta"
          action :nothing
        end

        bash "Update package metadata in #{destdir}" do
          cwd destdir
          action :nothing
          notifies :run, "bash[Delete #{destdir}/gen_meta]", :immediately
          code case pkgtype
               when "debs" then "dpkg-scanpackages . |gzip -9 >Packages.gz"
               when "rpms" then "createrepo ."
               else raise "Cannot create package metadata for #{pkgtype}"
               end
        end

        file "#{destdir}/gen_meta" do
          action :nothing
          notifies :run, "bash[Update package metadata in #{destdir}]", :immediately
        end

        bc[pkgtype][os]["raw_pkgs"].each do |src|
          dest = "#{destdir}/#{src.split('/')[-1]}"
          bash "#{destdir}: Fetch #{src}" do
            code "curl -fgL -o '#{dest}' '#{src}'"
            notifies :create, "file[#{destdir}/gen_meta]", :immediately
            not_if "test -f '#{dest}'"
          end
        end
      end
    end
  end

  # Index known barclamp repositories for this OS
  ruby_block "Index the current local package repositories for #{os}" do
    block do
      if File.exists? "#{os_dir}/crowbar-extra" and File.directory? "#{os_dir}/crowbar-extra"
        Dir.glob("#{os_dir}/crowbar-extra/*") do |f|
          reponame = f.split("/")[-1]
          node.normal["crowbar"]["provisioner"]["server"]["repositories"][os][reponame] = Mash.new
          case
          when os =~ /(ubuntu|debian)/
            bin="deb #{provisioner_web}/#{os}/crowbar-extra/#{reponame} /"
            src="deb-src #{provisioner_web}/#{os}/crowbar-extra/#{reponame} /"
            node.normal["crowbar"]["provisioner"]["server"]["repositories"][os][reponame][bin] = true if
              File.exists? "#{os_dir}/crowbar-extra/#{reponame}/Packages.gz"
            node.normal["crowbar"]["provisioner"]["server"]["repositories"][os][reponame][src] = true if
              File.exists? "#{os_dir}/crowbar-extra/#{reponame}/Sources.gz"
          when os =~ /(redhat|centos|suse|fedora)/
            bin="baseurl=#{provisioner_web}/#{os}/crowbar-extra/#{reponame}"
            node.normal["crowbar"]["provisioner"]["server"]["repositories"][os][reponame][bin] = true
          else
            raise ::RangeError.new("Cannot handle repos for #{os}")
          end
        end
      end
    end
  end

  replaces={
    '%os_site%'         => web_path,
    '%os_install_site%' => os_install_site
  }
  append = params["append"] || ""

  # Sigh.  There has to be a more elegant way.
  replaces.each { |k,v|
    append.gsub!(k,v)
  }

  # If we were asked to use a serial console, arrange for it.
  if  node["crowbar"]["provisioner"]["server"]["use_serial_console"]
    append << " console=tty0 console=ttyS1,115200n8"
  end

  # If we were asked to use a serial console, arrange for it.
  if  node["crowbar"]["provisioner"]["server"]["use_serial_console"]
    append << " console=tty0 console=ttyS1,115200n8"
  end

  # Add per-OS base repos that may not have been added above.

  unless node["crowbar"]["provisioner"]["server"]["boot_specs"]
    node.normal["crowbar"]["provisioner"]["server"]["boot_specs"] = Mash.new
  end
  unless node["crowbar"]["provisioner"]["server"]["boot_specs"][os]
    node.normal["crowbar"]["provisioner"]["server"]["boot_specs"][os] = Mash.new
  end
  node.normal["crowbar"]["provisioner"]["server"]["boot_specs"][os]["kernel"] = "../#{os}/install/#{kernel}"
  node.normal["crowbar"]["provisioner"]["server"]["boot_specs"][os]["initrd"] = "../#{os}/install/#{initrd}"
  node.normal["crowbar"]["provisioner"]["server"]["boot_specs"][os]["os_install_site"] = os_install_site
  node.normal["crowbar"]["provisioner"]["server"]["boot_specs"][os]["kernel_params"] = append

  ruby_block "Set up local base OS install repos for #{os}" do
    block do
      case
      when (/^ubuntu/ =~ os and File.exists?("#{tftproot}/#{os}/install/dists"))
        node.normal["crowbar"]["provisioner"]["server"]["repositories"][os]["base"] = { "#{provisioner_web}/#{os}/install" => true }
      when /^(suse)/ =~ os
        node.normal["crowbar"]["provisioner"]["server"]["repositories"][os]["base"] = { "baseurl=#{provisioner_web}/#{os}/install" => true }
      when /^(redhat|centos|fedora)/ =~ os
        # Add base OS install repo for redhat/centos
        if ::File.exists? "#{tftproot}/#{os}/install/repodata"
          node.normal["crowbar"]["provisioner"]["server"]["repositories"][os]["base"] = { "baseurl=#{provisioner_web}/#{os}/install" => true }
        elsif ::File.exists? "#{tftproot}/#{os}/install/Server/repodata"
          node.normal["crowbar"]["provisioner"]["server"]["repositories"][os]["base"] = { "baseurl=#{provisioner_web}/#{os}/install/Server" => true }
        end
      end
    end
  end
end

# Generate the appropriate pxe and uefi config files for discovery
# These will only be used if we have not already discovered the system.

package "syslinux"

ruby_block "Install pxelinux.0" do
  block do
    ["share","lib"].each do |d|
      next unless ::File.exists?("/usr/#{d}/syslinux/pxelinux.0")
      ::Kernel.system("cp /usr/#{d}/syslinux/pxelinux.0 #{discover_dir}")
    end
  end
  not_if do ::File.exists?("#{discover_dir}/pxelinux.0") end
end

bash "Install elilo as UEFI netboot loader" do
  code <<EOC
cd #{uefi_dir}
tar xzf '#{tftproot}/files/elilo-3.16-all.tar.gz'
mv elilo-3.16-x86_64.efi bootx64.efi
mv elilo-3.16-ia32.efi bootia32.efi
mv elilo-3.16-ia64.efi bootia64.efi
rm elilo*.efi elilo*.tar.gz || :
EOC
  not_if "test -f '#{uefi_dir}/bootx64.efi'"
end

# Build coreos chef code tgz - fix ip issue for ohai and dmidecode
bash "Build CoreOS chef code" do
  code <<EOC
set -e -x
cp -r /opt/chef /tmp
cd /tmp/chef
while read file; do
  sed -i "s:/sbin/ip:/bin/ip:g" "$file"
done < <(find . -type f | xargs grep -l "/sbin/ip")
while read file; do
  sed -i 's:"dmidecode":"/opt/chef/dmidecode/usr/sbin/dmidecode":g' "$file"
done < <(find . -type f | xargs grep -l 'shell_out("dmidecode")' | grep -v spec)
mkdir -p /tmp/chef/dmidecode
cd /tmp/chef/dmidecode
bzip2 -d -c #{tftproot}/files/dmidecode-2.10.tbz2 | tar xf -
cd /tmp
tar -zcf #{tftproot}/files/coreos-chef.tgz chef
cd
rm -rf /tmp/chef
EOC
  not_if do File.file?("#{tftproot}/files/coreos-chef.tgz") end
end

bash "Restore selinux contexts for #{tftproot}" do
  code "restorecon -R -F #{tftproot}"
  only_if "which selinuxenabled && selinuxenabled"
end

