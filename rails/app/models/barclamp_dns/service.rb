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

class BarclampDns::Service < Service

  def do_transition(nr, data)
    internal_do_transition(nr, data, "dns-service", "dns_servers") do |s|
      Rails.logger.debug("DnsServer: #{s.inspect} #{s.ServiceAddress}")
      addr = IP.coerce(s.ServiceAddress)
      Rails.logger.debug("DnsServer: #{addr.inspect}")

      server_name = s.ServiceTags.first
      server_type = ConsulAccess.getKey("opencrowbar/private/dns/#{server_name}/type")

      res = { "address" => s.ServiceAddress,
              "port" => "#{s.ServicePort}",
              "name" => server_name,
              "type" => server_type }

      if server_type == 'POWERDNS'
        res['mgmt_port'] = ConsulAccess.getKey("opencrowbar/private/dns/#{server_name}/mgmt_port")
        res['mgmt_token'] = ConsulAccess.getKey("opencrowbar/private/dns/#{server_name}/mgmt_token")
        res['mgmt_name'] = (ConsulAccess.getKey("opencrowbar/private/dns/#{server_name}/mgmt_name") rescue 'localhost')
      end

      res
    end
  end

end
