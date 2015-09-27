# Copyright 2015, RackN
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

class BarclampProxy::Client < Role

  def sysdata(nr)
    admin_addrs = Node.admin.collect {|a| a.network_allocations}.flatten.collect { |na| na.address.addr }
    { :proxy => { :admin_addrs => admin_addrs } }
  end

  # If the nodes networking changes, then we need to rerun the role
  def on_network_allocation_create(na)
    nr = NodeRoles.peers_by_node_and_role(na.node, self).first rescue nil
    if nr
      Run.enqueue(nr)
    end
  end

  def on_network_allocation_delete(na)
    nr = NodeRoles.peers_by_node_and_role(na.node, self).first rescue nil
    if nr
      Run.enqueue(nr)
    end
  end
end
