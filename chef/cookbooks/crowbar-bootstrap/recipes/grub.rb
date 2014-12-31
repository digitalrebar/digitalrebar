#
# Cookbook Name:: crowbar-bootstrap
# Recipe:: grub
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
# We are going to use grub for the provisioner bootloader.  We build it as
# part of the bootstrap to make sure everyone using the docker admin
# has the same bits.

tracedir=node["bootstrap"]["tracedir"]
tftproot=node["bootstrap"]["tftproot"]
directory tracedir do
  recursive true
end

grub_dest = "#{tracedir}/grub2-#{node["bootstrap"]["grub2"]["version"]}"
unless File.directory?(grub_dest)

  bash "Fetch grub2 source" do
    cwd "/root"
    code "git clone #{node["bootstrap"]["grub2"]["repo"]}"
    not_if "test -d /root/grub/.git"
  end

  bash "Check out grub commit #{node["bootstrap"]["grub2"]["version"]}" do
    cwd "/root/grub"
    code <<EOC
set -e
git checkout -f #{node["bootstrap"]["grub2"]["version"]} 
rm #{tracedir}/grub2 || :
EOC
    not_if "cd /root/grub; git status |fgrep -q ' #{node["bootstrap"]["grub2"]["version"]}'"
  end

  bash "Configure grub2 sources" do
    cwd "/root/grub"
    code "git clean -f -x -d; ./autogen.sh"
    not_if "test -x /root/grub/configure"
  end

  node["bootstrap"]["grub2"]["platforms"].each do |platform,imgmaker|
    bash "Build grub2 binaries for platform #{platform}" do
      cwd "/root/grub"
      code <<EOC
set -e
mkdir -p /tmp/grub/install /tmp/grub/staged
./configure --with-platform=#{platform} --prefix=/tmp/grub/install
make -j4 install
./grub-mkimage --output=/tmp/grub/staged/#{imgmaker[:file]} #{imgmaker[:cmd]}
make clean
EOC
      not_if "test -f /tmp/grub/staged/#{imgmaker[:file]}"
    end
  end

  bash "Make grub modules available for tftp" do
    code "/tmp/grub/install/bin/grub-mknetdir --net-directory=/tmp/grub/staged"
  end

  bash "download unifont.pf2 for Grub" do
    code "curl -fgl -o /tmp/grub/staged/boot/grub/fonts/unifont.pf2 #{node["bootstrap"]["grub2"]["bootfont"]}"
  end

  bash "Copy over grub boot files" do
    code <<EOC
rm -rf #{tracedir}/grub2-* || :
mv /tmp/grub/staged '#{grub_dest}'
EOC
  end
end

bash "Clean up grub build" do
  code "rm -rf /root/grub /tmp/grub || :"
end

bash "Copy over ancillary grub boot files" do
  cwd grub_dest
  code "cp -a --remove-destination -t '#{tftproot}' *"
end
