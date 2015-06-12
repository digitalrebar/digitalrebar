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

class BarclampProvisioner::Database < Role

  def on_node_create(node)
    rerun_my_noderoles
  end

  def on_node_change(node)
    rerun_my_noderoles
  end

  def on_node_delete(node)
    rerun_my_noderoles
  end

  def rerun_my_noderoles
    hosts = {}
    to_enqueue = []
    ActiveRecord::Base.connection.execute("select * from provisioner_database").each do |row|
      name,v4addr,bootenv = row["name"],row["address"],row["bootenv"]
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
      if /-install/.match(bootenv)
        # If we have an OS bootenv, we might need to reserve a disk for the OS instal.
        n = Node.find_by!(name: name)
        n.with_lock('FOR NO KEY UPDATE') do
          disks = Attrib.get('disks',n) || []
          claims = Attrib.get('claimed-disks',n) || {}
          target_id = Attrib.get('operating-system-disk',n)
          unless target_id &&
                 claims[target_id] == 'operating system' &&
                 disks.any?{|d|d['unique_name'] == target_id}
            target_id = nil
            claims.delete_if{|k,v|k == 'operating system'}
            # Grab the first unclaimed nonremovable disk for the OS.
            target = disks.detect{|d|!d["removable"] && !claims[d['unique_name']]}
            Attrib.set('operating-system-disk',n,target["unique_name"])
            claims[target["unique_name"]] = "operating system"
            Attrib.set('claimed-disks',n,claims)
            target_id = target["unique_name"]
          end
          hosts[name]["rootdev"] = target_id
        end
      end
    end
    node_roles.each do |nr|
      nr.with_lock('FOR NO KEY UPDATE') do
        old_hosts = (nr.sysdata["crowbar"]["provisioner"]["clients"] rescue {})
        next if hosts == old_hosts
        nr.update_column("sysdata",{"crowbar" => {"provisioner" => {"clients" => hosts}}})
        to_enqueue << nr
      end
    end
    to_enqueue.each { |nr| Run.enqueue(nr) }
  end
end
