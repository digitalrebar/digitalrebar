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

class BarclampChef::Service < Service

  def do_transition(nr, data)
    Rails.logger.info("Waiting on chef-service to appear in Consul")
    wait_for_service(nr, data, "chef-service")
    deployment_role = nr.deployment_role
    admin_accounts=nil
    chef_servers = nil
    until admin_accounts && chef_servers do
      admin_accounts = Attrib.get('chef-servers-admin-name',deployment_role)
      chef_servers = Attrib.get('chef-servers',deployment_role)
      Rails.logger.info("chef-service: Testing required attribs")
      unless admin_accounts && chef_servers
        deployment_role.reload
        sleep 1
      end
    end
    account = admin_accounts.first
    url = chef_servers.first
    # Update knife file and key file
    until File.exist?("/home/rebar/.chef/knife.rb") &&
          File.exist?("/home/rebar/.chef/#{account}.pem")
      sleep 1
    end

    j = BarclampChef::Jig.where(:name => "chef").first
    j.server = url
    j.client_name = account
    j.active = (Rails.env.development? ? false : true )
    j.key = "/home/rebar/.chef/#{account}.pem"
    Rails.logger.info("chef-service: Saving info for the Chef jig")
    j.save!

    url
  end
end
