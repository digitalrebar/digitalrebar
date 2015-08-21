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

class BarclampProxy::Service < Service

  def do_transition(nr, data)
    internal_do_transition(nr, data, "proxy-service", "proxy-servers") do |s|
      str_addr = s.ServiceAddress
      str_addr = s.Address if str_addr.nil? or str_addr.empty?
      Rails.logger.debug("ProxyService: #{s.inspect} #{str_addr}")
      addr = IP.coerce(str_addr)
      Rails.logger.debug("ProxyService: #{addr.inspect}")
      url = "http://"
      if addr.v6?
        url << "[#{addr.addr}]"
      else
        url << addr.addr
      end
      url << ":#{s.ServicePort}"
      { "address" => str_addr,
        "port" => "#{s.ServicePort}",
        "url" => url}
    end
  end

end
