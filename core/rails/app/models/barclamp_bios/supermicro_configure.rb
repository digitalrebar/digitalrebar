#!/usr/bin/ruby
# Copyright (c) 2016 Rackn
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# This file contains the logic needed to do BIOS configuration and
# updates for Dell Poweredge R-series gear.
# It assumes that we can talk to the IPMI controller embedded on the system,
# and that the system has WSMAN enabled.

# Implement everything needed to manage the attributes we care about on
# a Supermicro box

class BarclampBios::SupermicroConfigure < BarclampBios::Configure

  def on_proposed(nr)
    sum_archive = Attrib.get('bios-sum-archive',nr)
    archive_path = "/opt/digitalrebar/#{sum_archive}"
    unless File.exists?(archive_path)
      raise "Missing Supermicro Update Manager utility (#{archive_path})"
    end
    bin_path = "/usr/local/bin/sum"
    unless File.exists?(bin_path) && File.executable?(bin_path)
      Dir.mktmpdir do |dir|
        Dir.chdir(dir) do
          unless system "tar xzf #{archive_path}"
            raise "Failed to extract #{archive_path}"
          end
          ents = Dir.glob("*/sum")
          if ents.length != 1
            raise "Failed to extract a unique sum binary( #{ents} )"
          end
          system("cp #{ents[0]} /usr/local/bin")
        end
        system("rm -rf #{dir}/*")
      end
    end
  end

  def do_transition(nr, data)
    @driver = BarclampBios::Supermicro.new(nr.node, nr)
    super(nr,data)
  end
  
end
