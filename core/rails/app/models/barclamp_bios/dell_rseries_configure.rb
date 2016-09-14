#!/usr/bin/ruby
# Copyright (c) 2014 Victor Lowther
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
# a Dell R series box.


class BarclampBios::DellRseriesConfigure < BarclampBios::Configure

  # Configure the BIOS settings we care about for Dell R-series gear.
  # @param nr [NodeRole] The noderole that we are acting on behalf of.
  # @param data [Hash] The merged attrib data that we should use for configuration.
  def do_transition(nr,data)
    @driver = BarclampBios::Dellrseries.new(nr.node, nr)
    super(nr,data)
  end

end
