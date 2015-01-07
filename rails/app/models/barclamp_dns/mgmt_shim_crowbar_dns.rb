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

class BarclampDns::MgmtShimCrowbarDns < Role


  def on_node_create(n)
    Rails.logger.info("dns-database: Updating for new node #{n.name}")
    rerun_my_noderoles
  end

  def on_node_change(n)
    rerun_my_noderoles
  end

  def on_node_delete(n)
    rerun_my_noderoles
  end

  private

  def rerun_my_noderoles
    hosts = {}
    to_enqueue = []
    ActiveRecord::Base.connection.execute("select name, cname, address
                                           from dns_database
                                           where network = 'admin'").each do |row|
      name, addr, cname = row["name"] + ".", IP.coerce(row["address"]), row["cname"]
      hosts[name] ||= Hash.new
      hosts[name][addr.v4? ? "ip4addr" : "ip6addr"] ||= addr.addr
      hosts[name][cname] ||= cname if cname && !name.index(cname)
    end
    node_roles.each do |nr|
      nr.with_lock('FOR NO KEY UPDATE') do
        old_hosts = (nr.sysdata["crowbar"]["dns"]["hosts"] rescue {})
        if hosts == old_hosts
          Rails.logger.info("dns-database: DNS information unchanged.")
          next
        end
        Rails.logger.info("dns-database: Updating #{nr.name}")
        nr.update_column("sysdata", {"crowbar" => {"dns" => {"hosts" => hosts}}})
        to_enqueue << nr
      end
    end
    to_enqueue.each {|nr| Run.enqueue(nr)}
  end
end
