# Copyright 2014 Victor Lowther
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

class BarclampRaid::Jig < Jig

  # run is a bit specialized for the LsiRaidJig
  # If the noderole that it is being called on is a raid-discover noderole,
  # it scans for and grabs the current raid configuration for the node in question.
  # If it is a raid-configure noderole, it will try to converge the desired raid
  # configuration and the current raid configuration.
  def run(nr,data)
    begin
      case nr.role.name
      when "raid-discover"
        config = nr.node.actions[:raid].detect(nr).map{|c|c.to_hash}
        Attrib.set('raid-detected-controllers',nr,config,:wall)
      when "raid-configure"
        enabled = Attrib.get('raid-enable',nr)
        unless enabled
          Rails.logger.info("raid-configure not enabled, skipping raid config")
          return
        end
        config = nr.node.actions[:raid].converge(nr).map{|c|c.to_hash}
        Attrib.set('raid-configured-volumes',nr,config,:wall)
      else
        raise "Raid jig does not know how to handle #{nr.role.name}"
      end
    end
  rescue Exception => e
    Rails.logger.fatal("Raid Jig: Exception: #{e.inspect}")
    Rails.logger.fatal("Backtrace: #{e.backtrace.join("\n\t")}")
    raise e
  end

end
