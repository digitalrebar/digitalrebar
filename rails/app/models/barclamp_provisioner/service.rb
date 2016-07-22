# Copyright 2015, Greg Althaus
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

require 'rest-client'
require 'uri'

class BarclampProvisioner::Service < Service

  def on_node_delete(node)
    provisioner_delete(node.uuid)
  end

  def on_node_change(node)
    return unless node.previous_changes['bootenv']

    if node.bootenv == 'unknown'
      Rails.logger.info("Node: #{name} bootenv set to unknown, forgetting about it")
      provisioner_delete(node.uuid)
      return
    end
    provisioner_create(node)

    return unless node.actions[:boot] and node.previous_changes['bootenv']
    if node.bootenv == 'local'
      node.actions[:boot].disk
    else
      node.actions[:boot].pxe
    end
  end

  def do_transition(nr, data)
    wait_for_service(nr, data, 'provisioner-service')
    wait_for_service(nr, data, 'provisioner-mgmt-service')
    deployment_role = nr.deployment_role
    until Attrib.get('provisioner-webservers', deployment_role) &&
          Attrib.get('provisioner-management-servers', deployment_role) do
      sleep 1
      deployment_role.reload
    end
  end

private

  def get_rest_resource(url)
    # Validation Cert
    store = OpenSSL::X509::Store.new
    store.add_cert(OpenSSL::X509::Certificate.new(File.read('/var/run/rebar/ca.pem')))

    # get client key and cert
    client_cert = OpenSSL::X509::Certificate.new(File.read('/var/run/rebar/server.crt'))
    client_key  = OpenSSL::PKey.read(File.read('/var/run/rebar/server.key'))

    RestClient::Resource.new(
      url,
      :ssl_cert_store  =>  store,
      :ssl_client_cert =>  client_cert,
      :ssl_client_key  =>  client_key,
      :verify_ssl      =>  OpenSSL::SSL::VERIFY_PEER
    )
  end

  def provisioner_create(node)
    sysdepl = Deployment.system
    provisioner_mgmt = Attrib.get('provisioner-management-servers',sysdepl)
    url = provisioner_mgmt[0]['url']
    payload = {'Name' => node.name,
               'Uuid' => node.uuid,
               'TenantId' => node.tenant_id,
               'Address' => node.addresses(:v4_only)[0].addr,
               'BootEnv' => node.bootenv,
               'Params' => {}
              }
    begin
      user = User.find_by(username: 'system')
      response = get_rest_resource("#{url}/bootenvs/#{node.bootenv}").get :'X-Authenticated-Username' => 'system', :'X-Authenticated-Capability' => user.cap_map.to_json
    rescue => e
      Rails.logger.error("Node: provisioner manager #{url} does not know about bootenv #{node.bootenv}")
      raise "Provisioner management does not know about bootenv #{node.bootenv}"
    end
    bootenv_options = JSON.parse(response.body)
    bootenv_options['RequiredParams'].each do |param|
      # Try a few ways to get the params
      val = (Attrib.get(param,node) rescue nil)
      if val.nil?
        depl = node.deployment
        while val.nil?
          dnode = Node.find_by!(system: true, deployment_id: depl)
          val = (Attrib.get(param, dnode) rescue nil)
          val = (Attrib.get(param, depl) rescue nil) if val.nil?
          break if depl.parent_id.nil?
          depl = depl.parent
        end
      end
      if val.nil?
        Rails.logger.error("Node: bootenv #{node.bootenv} requires parameter #{param}, but we cannot find it!")
        raise "Cannot find required parameter #{param}"
      end
      payload['Params'][param] = val
    end if bootenv_options['RequiredParams'] && !bootenv_options['RequiredParams'].empty?
    begin
      user = User.find_by(username: 'system')
      response = get_rest_resource("#{url}/machines").post payload.to_json, content_type: :json, :'X-Authenticated-Username' => 'system', :'X-Authenticated-Capability' => user.cap_map.to_json
    rescue => e
      Rails.logger.error("Node: failed to switch #{node.name} to #{node.bootenv}\n#{e.response}")
      raise "Unable to change bootenv to #{node.bootenv}"
    end
    Attrib.set('provisioner-active-bootstate',node,node.bootenv)
    Rails.logger.info("Node: #{node.name} transitioned to #{node.bootenv}")
  end

  def provisioner_delete(uuid)
    sysdepl = Deployment.system
    provisioner_mgmt = Attrib.get('provisioner-management-servers',sysdepl)
    url = provisioner_mgmt[0]['url']
    user = User.find_by(username: 'system')
    get_rest_resource("#{url}/machines/#{uuid}").delete :'X-Authenticated-Username' => 'system', :'X-Authenticated-Capability' => user.cap_map.to_json
  end

end
