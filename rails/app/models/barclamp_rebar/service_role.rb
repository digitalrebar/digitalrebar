# Copyright 2016, RackN
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

class BarclampRebar::ServiceRole < Role

  def sync_on_destroy(nr, *args)
    collect_addresses(nr, args)
  end

  def on_active(nr, *args)
    collect_addresses(nr, args)
  end

  private

  def collect_addresses(nr, *args)

    aname = "#{nr.role.name}-addresses"
    addresses = []

    d = nr.deployment
    d.node_roles.where(role_id: nr.role_id).each do |nrs|
      if nrs.active?
        n = nrs.node
        str_addr = n.get_attrib('node-control-address')
        str_addr ||= n.addresses.first.addr.to_s 
        addresses << str_addr
      end
    end

    Rails.logger.info("Updating #{nr.role.name} #{aname}: #{addresses.inspect}")
    Attrib.set(aname,nr.deployment_role,addresses)

    true

  end

end
