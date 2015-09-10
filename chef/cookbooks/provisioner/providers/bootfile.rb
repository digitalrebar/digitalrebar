# Copyright 2013, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#


action :add do
  nodeaddr = sprintf("%08X",new_resource.address.address)
  tftproot = node["rebar"]["provisioner"]["server"]["root"]
  discover_dir="#{tftproot}/discovery"
  uefi_dir=discover_dir
  pxecfg_dir="#{discover_dir}/pxelinux.cfg"
  pxefile = "#{pxecfg_dir}/#{nodeaddr}"
  uefifile = "#{uefi_dir}/#{nodeaddr}.conf"
  extra = (new_resource.bootenv == "local" ? "local." : "")
  template pxefile do
    mode 0644
    owner "root"
    group "root"
    source "default.#{extra}erb"
    variables(:append_line => new_resource.kernel_params,
              :install_name => new_resource.bootenv,
              :initrd => new_resource.initrd,
              :kernel => new_resource.kernel)
  end
  template uefifile do
    mode 0644
    owner "root"
    group "root"
    source "default.elilo.erb"
    variables(:append_line => new_resource.kernel_params,
              :install_name => new_resource.bootenv,
              :initrd => new_resource.initrd,
              :kernel => new_resource.kernel)
  end if new_resource.bootenv != "local"
end

action :remove do
  nodeaddr = sprintf("%08X",new_resource.address.address)
  tftproot = node["rebar"]["provisioner"]["server"]["root"]
  discover_dir="#{tftproot}/discovery"
  uefi_dir=discover_dir
  pxecfg_dir="#{discover_dir}/pxelinux.cfg"
  pxefile = "#{pxecfg_dir}/#{nodeaddr}"
  uefifile = "#{uefi_dir}/#{nodeaddr}.conf"
  file pxefile do
    action :delete
  end
  file uefifile do
    action :delete
  end
end
