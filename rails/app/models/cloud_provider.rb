# Copyright 2015 RackN
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
require 'diplomat'
require 'jsonrpc-client'


class CloudProvider < Provider

  after_commit :register_endpoint, on: [:create, :update]

  def can_create_nodes
    true
  end

  def create_node(obj)
    obj.with_lock do
      if Attrib.get('provider-node-id',obj) != nil
        Rails.logger.fatal("Trying to recreate #{obj.name} in provider #{self.class.name}")
        Rails.logger.fatal(caller.join("\n"))
        raise "Trying to recreate #{obj.name} in provider #{self.class.name}"
      end
      ep = endpoint
      params = Attrib.get('provider-create-hint',obj) || {}
      begin
        server = ep.invoke('servers.create',[self.auth_details,obj.id,params], { timeout: 120 })
        Rails.logger.info("Created server #{server.inspect}")
        Attrib.set('provider-node-id',obj, server["id"], :hint)
      rescue Exception => e
        Rails.logger.fatal("invoke create failed: #{e.inspect}")
      end
    end

    # Nodes should always have rebar-joined-node.
    r = Role.find_by_name('rebar-joined-node')
    r.add_to_node(obj)
    obj.commit!
  end

  def reboot_node(obj)
    ep = endpoint
    begin
      ep.invoke('servers.reboot',[self.auth_details,Attrib.get('provider-node-id',obj)], { timeout: 120 })
    rescue Exception => e
      Rails.logger.fatal("invoke reboot failed: #{e.inspect}")
      false
    end
  end

  def delete_node(obj)
    ep = endpoint
    begin
      ep.invoke('servers.delete',[self.auth_details,Attrib.get('provider-node-id',obj)], { timeout: 120 })
    rescue Exception => e
      Rails.logger.fatal("invoke delete failed: #{e.inspect}")
      false
    end
  end

  def endpoint
    service = Diplomat::Service.get('cloudwrap')
    JSONRPC::Client.new("http://#{service.Address}:#{service.ServicePort}")
  end

  private
  def register_endpoint
    ep = endpoint
    begin
      ep.invoke('servers.register',
                [self.auth_details, 'rebar', Attrib.get('rebar-access_keys',Deployment.system)],
                {timeout: 120})
    rescue Exception => e
      Rails.logger.fatal("invoke register failed: #{e.inspect}")
      false
    end
  end

end
