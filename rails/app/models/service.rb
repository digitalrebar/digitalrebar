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

class Service < Role

  def internal_do_transition(nr, data, service_name, service_attribute)
    runlog = []
    addr_arr = []
    runlog << "Getting #{service_name} information from consul"
    pieces = nil
    options = {}
    meta = {}
    count=0
    while pieces == nil
      begin
        count += 1
        break if count > 20
        pieces = Diplomat::Service.get(service_name, :all, options, meta)
        if pieces and pieces.empty?
          Rails.logger.info("#{service_name} not available ... wait 10m or next update")
          runlog << "#{service_name} not available ... wait 10m or next update"
          if meta[:index]
            options[:wait] = "10m"
            options[:index] = meta[:index]
          else
            sleep 10
          end
          pieces = nil
        elsif pieces == nil
          Rails.logger.info("#{service_name} not found ... wait 10s")
          runlog << "#{service_name} not found ... wait 10s"
          sleep 10
        end
      rescue Exception => e
        runlog << "Failed to talk to consul: #{e.message}"
        Rails.logger.info("Failed to talk to consul: #{e.message}")
        sleep 10
      end
    end

    runlog << "Processing pieces for #{service_name}"
    if pieces
      pieces.each do |p|
        addr_arr << p.Address
      end
      runlog << "Setting #{service_name} attribute"
      Attrib.set(service_attribute, nr, addr_arr, :system)
    end

    Rails.logger.info("Finished waiting for #{service_name}: #{addr_arr.length} found")
    nr.runlog = runlog.join("\n")
    nr.save!
    raise "#{service_name} not available" if pieces == nil or pieces.empty?
  end

end
