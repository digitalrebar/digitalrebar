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

class BarclampProvisioner::Service < Service

  def do_transition(nr, data)
    internal_do_transition(nr, data, "provisioner-service", "provisioner-webservers") do |s|
      str_addr = s.ServiceAddress
      str_addr = s.Address if str_addr.nil? or str_addr.empty?
      Rails.logger.debug("ProvisionerService: #{s.inspect} #{str_addr}")
      addr = IP.coerce(str_addr)
      Rails.logger.debug("ProvisionerService: #{addr.inspect}")
      url = "http://"
      if addr.v6?
        url << "[#{addr.addr}]"
      else
        url << addr.addr
      end

      # Make sure that the provisioner is setup.
      system("/opt/opencrowbar/core/bin/update-provisioner #{addr.addr}")

      url << ":#{s.ServicePort}"
      { "address" => str_addr,
        "port" => "#{s.ServicePort}",
        "url" => url}
    end
  end

end
