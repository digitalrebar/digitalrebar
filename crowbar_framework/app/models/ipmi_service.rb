# Copyright 2012, Dell 
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

class IpmiService < ServiceObject
  
  def transition(inst, name, state)
    @logger.debug("IPMI transition: make sure that network role is on all nodes: #{name} for #{state}")
    
    #
    # If we are discovering the node, make sure that we add the ipmi role to the node
    #
    if state == "discovering"
      @logger.debug("IPMI transition: discovering state for #{name} for #{state}")
      result = add_role_to_instance_and_node(name, inst, "ipmi-discover")
      @logger.debug("ipmi transition: leaving from installed state for #{name} for #{state}")
      a = [200, "" ] if result
      a = [400, "Failed to add role to node"] unless result
      return a
    end
    
    #
    # If we are discovering the node, make sure that we add the ipmi role to the node
    #
    if state == "discovered"
      @logger.debug("IPMI transition: installed state for #{name} for #{state}")
      result = add_role_to_instance_and_node(name, inst, "ipmi-configure")
      @logger.debug("ipmi transition: leaving from installed state for #{name} for #{state}")
      a = [200, "" ] if result
      a = [400, "Failed to add role to node"] unless result
      return a
    end
    
    @logger.debug("ipmi transition: leaving for #{name} for #{state}")
    [200, ""]
  end
  
end
