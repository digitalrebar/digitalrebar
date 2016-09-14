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

describe Network do

  it "has a valid factory" do
    expect(FactoryGirl.create(:network)).to be_valid
  end

  it "is invalid without a name" do
    d = FactoryGirl.build(:network, name: nil)
    expect(d).not_to be_valid
    expect(d.errors.messages[:name]).to eq(["Network : No name"])
  end

  it "should have a category of general by default" do
    d = FactoryGirl.create(:network)
    expect(d).to be_valid
    expect(d.category).to eq('general')
  end

  it "is invalid with an empty name" do
    d = FactoryGirl.build(:network, name: "")
    expect(d).not_to be_valid
    expect(d.errors.messages[:name]).to eq(["Network : No name"])
  end

  it "is invalid without a deployment" do
    d = FactoryGirl.build(:network, deployment_id: nil)
    expect(d).not_to be_valid
    expect(d.errors.messages[:deployment_id]).to eq([ "Network squeaky: Cannot create a network without binding it to a deployment"])
  end

  it "is invalid without a conduit" do
    d = FactoryGirl.build(:network, conduit: nil)
    expect(d).not_to be_valid
    expect(d.errors.messages[:conduit]).to eq(["Network squeaky: Conduit definition cannot be empty"])
  end

  it "is invalid without a valid conduit" do
    ["cow", "?1c3", "?1gd", "x1g3"]. each do |c|
      d = FactoryGirl.build(:network, conduit: c)
      expect(d).not_to be_valid
      expect(d.errors.messages[:conduit]).to eq(["Network squeaky: Invalid abstract interface names in conduit: #{c}"])
    end
  end

  it "is invalid with multiple conduits without use_team" do
    d = FactoryGirl.build(:network, conduit: "1g4,1g5")
    expect(d).not_to be_valid
    expect(d.errors.messages[:conduit]).to eq(["Network squeaky: Do not want bonding, but requested conduit 1g4,1g5 has multiple members"])
  end

  it "is invalid with multiple conduits with use_team, but one is bad" do
    d = FactoryGirl.build(:network, use_team: true, conduit: "1g2,1k0")
    expect(d).not_to be_valid
    expect(d.errors.messages[:conduit]).to eq(["Network squeaky: Invalid abstract interface names in conduit: 1k0"])
  end

  it "is invalid with multiple conduits with use_team, but speeds are different" do
    d = FactoryGirl.build(:network, use_team: true, conduit: "1g3,1m0")
    expect(d).not_to be_valid
    expect(d.errors.messages[:conduit]).to eq(["Network squeaky: Not all abstract interface names have the same speed and flags: 1g3,1m0"])
  end

  it "is invalid with multiple conduits with use_team, that overlap with other nets (not exactly)" do
    FactoryGirl.create(:network, name: "fred", conduit: "1g2")
    FactoryGirl.create(:network, name: "john", conduit: "1g3")
    d = FactoryGirl.build(:network, use_team: true, conduit: "1g2,1g3")
    expect(d).not_to be_valid
    expect(d.errors.messages[:conduit]).to eq(["Network squeaky: Conduit mapping overlaps with network fred", "Network squeaky: Conduit mapping overlaps with network john"])
  end

  it "is invalid with more than 8 conduits with use_team" do
    d = FactoryGirl.build(:network, use_team: true, conduit: "1g10,1g11,1g12,1g13,1g14,1g15,1g16,1g17,1g18")
    expect(d).not_to be_valid
    expect(d.errors.messages[:conduit]).to eq(["Network squeaky: Want bonding, but requested conduit 1g10,1g11,1g12,1g13,1g14,1g15,1g16,1g17,1g18 has too many members"])
  end

  it "is invalid with a single conduit with use_team" do
    d = FactoryGirl.build(:network, use_team: true, conduit: "1g0")
    expect(d).not_to be_valid
    expect(d.errors.messages[:conduit]).to eq(["Network squeaky: Want bonding, but requested conduit 1g0 has too few members"])
  end

  it "is invalid with an empty conduit with use_team" do
    d = FactoryGirl.build(:network, use_team: true, conduit: "")
    expect(d).not_to be_valid
    expect(d.errors.messages[:conduit]).to eq(["Network squeaky: Conduit definition cannot be empty", "Network squeaky: Want bonding, but requested conduit  has too few members"])
  end

  it "is valid with multiple conduits with use_team" do
    d = FactoryGirl.build(:network, use_team: true, conduit: "1g5,1g6")
    expect(d).to be_valid
  end

  it "is valid with use_vlan and a valid vlan" do
    d = FactoryGirl.build(:network, use_vlan: true, vlan: 3)
    expect(d).to be_valid
  end

  it "is invalid with use_vlan and an invalid vlan" do
    [-12, 0, 4095, 4096, 50000].each do |v|
      d = FactoryGirl.build(:network, use_vlan: true, vlan: v)
      expect(d).not_to be_valid
      expect(d.errors.messages[:vlan]).to eq(["Network squeaky: VLAN #{v} not sane"])
    end
  end

  it "is valid without use_vlan and an invalid vlan" do
    [-12, 0, 4095, 4096, 50000].each do |v|
      d = FactoryGirl.build(:network, use_vlan: false, vlan: v)
      expect(d).to be_valid
    end
  end

  it "is valid without use_team and an invalid team_mode" do
    [-12, 7, -1, 100].each do |t|
      d = FactoryGirl.build(:network, use_team: false, team_mode: t)
      expect(d).to be_valid
    end
  end

  it "is invalid with team_mode and an invalid team_mode" do
    [-12, 7, -1, 100].each do |t|
      d = FactoryGirl.build(:network, use_team: true, team_mode: t, conduit: "1g4,1g5")
      expect(d).not_to be_valid
      expect(d.errors.messages[:team_mode]).to eq(["Network squeaky: Invalid bonding mode"])
    end
  end

end

