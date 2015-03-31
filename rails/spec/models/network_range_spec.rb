# Copyright 2015, RackN
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
require 'spec_helper'

describe NetworkRange do

  it "has a valid factory" do
    n = FactoryGirl.create(:network)
    expect(FactoryGirl.create(:network_range, network_id: n.id)).to be_valid
  end

  it "is invalid without a network" do
    d = FactoryGirl.build(:network_range, network_id: nil)
    expect(d).not_to be_valid
    expect(d.errors.messages[:network]).to eq(["NetworkRange does not have an associated network!"])
  end

  it "is invalid without a name" do
    n = FactoryGirl.create(:network)
    d = FactoryGirl.build(:network_range, network_id: n.id, name: nil)
    expect(d).not_to be_valid
    expect(d.errors.messages[:name]).to eq(["NetworkRange squeaky.: No name"])
  end

  it "is invalid with overlap set, but parent network configured" do
    n = FactoryGirl.create(:network)
    d = FactoryGirl.build(:network_range, network_id: n.id, overlap: true)
    expect(d).not_to be_valid
    expect(d.errors.messages[:network]).to eq(["NetworkRange squeaky.rangy: must have a non-configure parent network with specifying overlap"])
  end

  it "is invalid with the type not the same for first and last" do
    n = FactoryGirl.create(:network)
    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.10/24", last: "::3/24")
    expect(d).not_to be_valid
    expect(d.errors.messages[:first]).to eq(["NetworkRange squeaky.rangy: 192.168.130.10/24 and ::3/24 must be of the same type","NetworkRange squeaky.rangy: 192.168.130.10/24 and ::3/24 must be in the same subnet"])
  end

  it "is invalid with the netmask not the same for first and last" do
    n = FactoryGirl.create(:network)
    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.0.4/24", last: "192.168.0.10/28")
    expect(d).not_to be_valid
    expect(d.errors.messages[:first]).to eq(["NetworkRange squeaky.rangy: 192.168.0.4/24 and 192.168.0.10/28 must be of the same netmask"])
  end

  it "is invalid with the network not the same for first and last" do
    n = FactoryGirl.create(:network)
    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.10/24", last: "192.168.138.20/24")
    expect(d).not_to be_valid
    expect(d.errors.messages[:first]).to eq(["NetworkRange squeaky.rangy: 192.168.130.10/24 and 192.168.138.20/24 must be in the same subnet"])
  end

  it "is invalid with the first as a subnet address" do
    n = FactoryGirl.create(:network)
    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.0/24", last: "192.168.130.20/24")
    expect(d).not_to be_valid
    expect(d.errors.messages[:first]).to eq(["NetworkRange squeaky.rangy: 192.168.130.0/24 cannot be a subnet address"])
  end

  it "is invalid with the first as a broadcast address" do
    n = FactoryGirl.create(:network)
    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.255/24", last: "192.168.130.20/24")
    expect(d).not_to be_valid
    expect(d.errors.messages[:first]).to eq(["NetworkRange squeaky.rangy: 192.168.130.255/24 cannot be a broadcast address"])
  end

  it "is invalid with the last as a subnet address" do
    n = FactoryGirl.create(:network)
    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.1/24", last: "192.168.130.0/24")
    expect(d).not_to be_valid
    expect(d.errors.messages[:last]).to eq(["NetworkRange squeaky.rangy: 192.168.130.0/24 cannot be a subnet address"])
  end

  it "is invalid with the last as a broadcast address" do
    n = FactoryGirl.create(:network)
    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.1/24", last: "192.168.130.255/24")
    expect(d).not_to be_valid
    expect(d.errors.messages[:last]).to eq(["NetworkRange squeaky.rangy: 192.168.130.255/24 cannot be a broadcast address"])
  end

  it "is invalid with the last as a broadcast address" do
    n = FactoryGirl.create(:network)
    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.1/24", last: "192.168.130.255/24")
    expect(d).not_to be_valid
    expect(d.errors.messages[:last]).to eq(["NetworkRange squeaky.rangy: 192.168.130.255/24 cannot be a broadcast address"])
  end

  it "is invalid with overlapping ranges in the first" do
    n = FactoryGirl.create(:network)
    FactoryGirl.create(:network_range, network_id: n.id, first: "192.168.130.1/24", last: "192.168.130.100/24")

    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.50/24", last: "192.168.130.200/24")
    expect(d).not_to be_valid
    expect(d.errors.messages[:first]).to eq(["NetworkRange squeaky.rangy: first address 192.168.130.50/24 overlaps with range squeaky.rangy"])
  end

  it "is invalid with overlapping ranges in the last" do
    n = FactoryGirl.create(:network)
    FactoryGirl.create(:network_range, network_id: n.id, first: "192.168.130.51/24", last: "192.168.130.100/24")

    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.50/24", last: "192.168.130.90/24")
    expect(d).not_to be_valid
    expect(d.errors.messages[:last]).to eq(["NetworkRange squeaky.rangy: last address 192.168.130.90/24 overlaps with range squeaky.rangy"])
  end

  it "is invalid with overlapping conduits" do
    n = FactoryGirl.create(:network)
    FactoryGirl.create(:network_range,
                       network_id: n.id,
                       first: "192.168.130.1/24",
                       last: "192.168.130.40/24",
                       conduit: "1g2")
    d = FactoryGirl.build(:network_range, network_id: n.id,
                          first: "192.168.130.50/24",
                          last: "192.168.130.90/24",
                          use_team: true,
                          conduit: "1g2,1g3")
    expect(d).not_to be_valid
    expect(d.errors.messages[:conduit]).to eq(["NetworkRange squeaky.rangy: Conduit mapping overlaps with network range squeaky.rangy"])
  end

  it "is valid with overlapping ranges if it is set to overlap and network not configure" do
    n = FactoryGirl.create(:network, configure: false)
    FactoryGirl.create(:network_range, network_id: n.id, first: "192.168.130.51/24", last: "192.168.130.100/24")

    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.1/24", last: "192.168.130.254/24", overlap: true)
    expect(d).to be_valid
  end

  it "is valid with overlapping ranges if old is set to overlap" do
    n = FactoryGirl.create(:network, configure: false)
    FactoryGirl.create(:network_range, network_id: n.id, first: "192.168.130.1/24", last: "192.168.130.254/24", overlap: true)

    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.51/24", last: "192.168.130.100/24")
    expect(d).to be_valid
  end

  it "is invalid with overlapping ranges if old is not set to overlap" do
    n = FactoryGirl.create(:network, configure: false)
    FactoryGirl.create(:network_range, network_id: n.id, first: "192.168.130.1/24", last: "192.168.130.254/24")

    d = FactoryGirl.build(:network_range, network_id: n.id, first: "192.168.130.51/24", last: "192.168.130.100/24")
    expect(d).not_to be_valid
    expect(d.errors.messages[:first]).to eq(["NetworkRange squeaky.rangy: first address 192.168.130.51/24 overlaps with range squeaky.rangy"])
    expect(d.errors.messages[:last]).to eq(["NetworkRange squeaky.rangy: last address 192.168.130.100/24 overlaps with range squeaky.rangy"])
  end

end

