# Copyright 2015, Greg Althaus
#
# Licensed under the Apache License, Version 2.0 (the "License");.
# you may not use this file except in compliance with the License..
# You may obtain a copy of the License at.
#
#  http://www.apache.org/licenses/LICENSE-2.0.
#
# Unless required by applicable law or agreed to in writing, software.
# distributed under the License is distributed on an "AS IS" BASIS,.
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied..
# See the License for the specific language governing permissions and.
# limitations under the License..
#
#

class ConsulAccess

  # TODO: One day, we should update Diplomat to take token as an option.
  # This will allow for running as non-master token and do lookups per token.

  # Wrap the Diplomat access to set common config/values
  def self.getService(service_name, scope = :first, options = {}, meta = {})
    if Diplomat.configuration.acl_token.nil? and File.exists?('/etc/crowbar.master.acl')
      Diplomat.configuration.acl_token = File.read('/etc/crowbar.master.acl').chomp
    end
    Diplomat::Service.get(service_name, scope, options, meta)
  end

  # Wrap the Diplomat access to set common config/values
  def self.getKey(key)
    if Diplomat.configuration.acl_token.nil? and File.exists?('/etc/crowbar.master.acl')
      Diplomat.configuration.acl_token = File.read('/etc/crowbar.master.acl').chomp
    end
    Diplomat::Kv.get(key)
  end

end

