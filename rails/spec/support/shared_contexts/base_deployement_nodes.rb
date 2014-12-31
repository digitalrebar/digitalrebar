# Copyright 2013, Dell 
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



# This context establishes a base Crowbar deployment.
# it should be included in all tests that manipulate node attributes
shared_context "crowbar test deployment" do
  
  before(:all) do
    # we need this to ensure that we have the crowbar barclamp
    bc = Barclamp.find_by_name('crowbar') || Barclamp.import('crowbar')

    sd = Deployment.find_by_name 'system'

    admin_net= {
      "name" => "admin",
      "deployment" => sd,
      "conduit" => "1g0"
    }
    admin_ranges = [
        {
          "name" => "admin",
          "first" => "192.168.124.10/24",
          "last" => "192.168.124.11/24"
        },
        {
          "name" => "host",
          "first" => "192.168.124.81/24",
          "last" => "192.168.124.254/24"
        },
        {
          "name" => "dhcp",
          "first" => "192.168.124.21/24",
          "last" => "192.168.124.80/24"
        }
    ]

    network = Network.find_or_create_by! admin_net
    admin_ranges.each do |r|
      r[:network_id] = network.id
      NetworkRange.find_or_create_by! r
    end

    bmc_net = {
      "name" => "bmc",
      "deployment" => sd,
      "conduit" => "1g0"
    }
    bmc_ranges = [
      {
        "name" => "admin",
        "first" => "192.168.128.10/24",
        "last" => "192.168.128.20/24"
      },
      {
        "name" => "host",
        "first" => "192.168.128.21/24",
        "last" => "192.168.128.254/24"
      }
    ]

    network = Network.find_or_create_by! bmc_net
    bmc_ranges.each do |r|
      r[:network_id] = network.id
      NetworkRange.find_or_create_by! r
    end

  end

  let(:deployment) { Deployment.find_by_name 'system' }
  let(:active)     { deployment.head }

  describe Deployment do

    it "is a system deployment" do
      expect(deployment.system?).to be true
    end

    it "is committed" do
      expect(deployment.committed?).to be true
    end

  end

end

# Just 2 dummy nodes
shared_context "2 dummy nodes" do
  let(:node1) { Node.create! :name=>"unit1.test.com", :alias=>"unit1", :admin=>true }
  let(:node2) { Node.create! :name=>"unit2.test.com", :alias=>"unit2" }
end
