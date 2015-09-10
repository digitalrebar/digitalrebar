# Copyright 2013-4, Dell
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

# if docker, everything is cool.  don't worry
return if node[:rebar_ohai][:in_docker]

# Tell the kernel to reboot after 10 seconds on panic, if we want it to.
if node["panic"] and node["panic"]["reboot"] == true
  if node["panic"]["timeout"]
    timeout = node["panic"]["timeout"]
  else
    timeout = 15
  end
  if ::File.exists?("/etc/sysctl.d") and ::File.directory?("/etc/sysctl.d")
    bash "Save reboot on panic" do
      code "echo 'kernel.panic = #{timeout}' >/etc/sysctl.d/80-reboot-on-panic.conf"
    end
  else
    bash "Save reboot on panic" do
      code <<-__EOC__
if grep -q 'kernel.panic' /etc/sysctl.conf; then
sed -i '/^kernel.panic/ s/(kernel.panic = ).*/\1#{timeout}/' /etc/sysctl.conf
else
echo 'kernel.panic = #{timeout}' >>/etc/sysctl.conf
fi
__EOC__
    end
  end 
  bash "Reboot on panic" do
    code "echo #{timeout} >/proc/sys/kernel/panic"
  end
else
  bash "Stop rebooting on panic" do
    code <<-__EOC__
if [[ -f /etc/sysctl.d/80-reboot-on-panic.conf ]]; then
rm -f /etc/sysctl.d/80-reboot-on-panic.conf
fi
sed -i '/kernel.panic/ s/.*//' /etc/sysctl.conf
echo 0 >/proc/sys/kernel/panic
__EOC__
  end
end
