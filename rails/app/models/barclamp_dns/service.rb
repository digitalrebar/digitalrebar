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

class BarclampDns::Service < Role

  def do_transition(nr,data)
    runlog = []
    addr_arr = []
    runlog << "Getting dns-service information from consul"
    pieces = nil
    options = {}
    meta = {}
    while pieces == nil
      begin
        pieces = Diplomat::Service.get("dns-service", :all, options, meta)
        if pieces and pieces.empty?
          runlog << "dns-service not availabe ... wait 10m or next update"
          if meta[:index]
            options[:wait] = "10m"
            options[:index] = meta[:index]
          else
            sleep 1
          end
          pieces = nil
        end
      rescue Exception => e
        runlog << "Failed to talk to consul: #{e.message}"
        nr.runlog = runlog.join("\n")
        sleep 1
      end
    end

    runlog << "Processing pieces"
    pieces.each do |p|
      addr_arr << p.Address
    end

    runlog << "Setting dns-service attribute"
    Attrib.set("dns_servers", nr, addr_arr, :system)
    nr.runlog = runlog.join("\n")
    raise "dns-service not available" if pieces.empty?
  end

end
