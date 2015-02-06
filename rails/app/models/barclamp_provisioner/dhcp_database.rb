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

class BarclampProvisioner::DhcpDatabase < Role

  def on_node_create(node)
    rerun_my_noderoles
  end

  def on_node_change(node)
    rerun_my_noderoles
  end

  def on_node_delete(node)
    rerun_my_noderoles node.name
  end

  def rerun_my_noderoles(skip_node=nil)
    hosts = {}
    to_enqueue = []
    ActiveRecord::Base.connection.execute("select * from dhcp_database").each do |row|
      name,v4addr,bootenv = row["name"],row["address"],row["bootenv"]
      next if name.eql? skip_node
      ints = JSON.parse(row["discovered_macs"]) if row["discovered_macs"]
      mac_list = row["hinted_macs"] ? JSON.parse(row["hinted_macs"]) : []
      unless ints.nil?
        ints.each do |net, net_data|
          net_data.each do |field, field_data|
            next if field != "addresses"
            field_data.each do |addr, addr_data|
              next if addr_data["family"] != "lladdr"
              mac_list << addr unless mac_list.include? addr
            end
          end
        end
      end
      next unless mac_list.length > 0
      hosts[name] ||= Hash.new
      hosts[name]["mac_addresses"] = mac_list.map{|m|m.upcase}.sort.uniq
      hosts[name]["v4addr"] = v4addr
      hosts[name]["bootenv"] = bootenv
    end
    node_roles.each do |nr|
      nr.with_lock('FOR NO KEY UPDATE') do
        old_hosts = (nr.sysdata["crowbar"]["dhcp"]["clients"] rescue {})
        next if hosts == old_hosts
        nr.update_column("sysdata",{"crowbar" => {"dhcp" => {"clients" => hosts}}})
        to_enqueue << nr
      end
    end
    to_enqueue.each { |nr| Run.enqueue(nr) }
  end
end
