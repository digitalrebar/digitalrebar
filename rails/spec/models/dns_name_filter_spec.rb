# Copyright 2015, Greg Althaus

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

#
# By doing this way, we skip the network allocation code
#
def make_network_allocation(cat_name, net_name, range_name)
  r = FactoryGirl.create(:role)
  FactoryGirl.create(:attrib, name: 'rack-position', role_id: r)
  n = FactoryGirl.create(:node)
  r.add_to_node(n)
  net = FactoryGirl.create(:network, category: cat_name, name: net_name)
  net_range = FactoryGirl.create(:network_range, network: net, name: range_name)
  FactoryGirl.create(:network_allocation, node: n, network: net, network_range: net_range)
end

describe 'dns_name_filter' do

  it 'has a valid factory' do
    expect(FactoryGirl.build_stubbed(:dns_name_filter)).to be_valid
  end

  it 'must have a valid matcher' do

  end

  it 'must have a valid template' do

  end

  it 'must have a service' do

  end

  it 'must match by category' do
    dnf = FactoryGirl.create(:dns_name_filter)
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_truthy
  end

  it 'must not match by category' do
    dnf = FactoryGirl.create(:dns_name_filter)
    na = make_network_allocation('public', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_falsey
  end

  it 'must match by net.name' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'net.name == "squeaky"')
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_truthy
  end

  it 'must not match by net.name' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'net.name == "fred"')
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_falsey
  end

  it 'must match by range.name' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'range.name == "rangy"')
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_truthy
  end

  it 'must not match by range.name' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'range.name == "fred"')
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_falsey
  end

  it 'must match by deployment.name' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"')
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_truthy
  end

  it 'must not match by deployment.name' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "fred"')
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_falsey
  end

  it 'must match by node has role' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'node.role has "roley"')
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_truthy
  end

  it 'must not match by node has role' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'node.role has "fred"')
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_falsey
  end

  it 'must not match by node attribute: unknown attribute' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'node.attr.unrack-position == "fred"')
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_falsey
  end

  it 'must match by node attribute: known attribute with good value' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'node.attr.rack-position == "fred"')
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_truthy
  end

  it 'must not match by node attribute: known attribute with bad value' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'node.attr.rack-position == "333"')
    na = make_network_allocation('admin', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_falsey
  end

  it 'must match multiple successful conditions' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'node.role has "roley",net.category == "public"')
    na = make_network_allocation('public', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_truthy
  end

  it 'must not match multiple first unsuccessful conditions' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'node.role has "fred",net.category == "public"')
    na = make_network_allocation('public', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_falsey
  end

  it 'must not match multiple second unsuccessful conditions' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'node.role has "roley",net.category == "admin"')
    na = make_network_allocation('public', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_falsey
  end

  it 'must not match unknown condition' do
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'fred')
    na = make_network_allocation('public', 'squeaky', 'rangy')
    expect(dnf.claims(na)).to be_falsey
  end

  it 'must handle make_name returning a string' do
    dnf = FactoryGirl.create(:dns_name_filter, template: '{{node.name}}..{{node.id}}..{{node.mac}}..{{node.cow}}')
    n = FactoryGirl.create(:node, name: 'fred.drolling.pig')
    expect(dnf.make_name(n)).to eq("fred..#{n.id}....{{node.cow}}")
  end

  it 'must return false if can not claim_and_update' do
    allow(BarclampDns::MgmtService).to receive(:get_service).and_return({something: 'testing'})

    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "fred"')
    na = make_network_allocation('public', 'squeaky', 'rangy')
    expect(dnf.claim_and_update(na)).to be_falsey
  end

  it 'must return true and create a new DNE when one does not exist' do
    allow(BarclampDns::MgmtService).to receive(:get_service).and_return({something: 'testing'})

    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"')
    na = make_network_allocation('public', 'squeaky', 'rangy')

    expect(dnf.claim_and_update(na)).to be_truthy
    dnes = DnsNameEntry.all
    expect(dnes.length).to be(1)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[0].dns_name_filter).to eq(dnf)
    expect(dnes[0].rr_type).to eq('A')
  end

  it 'must claim a network_allocation when a matching item is created' do
    allow(BarclampDns::MgmtService).to receive(:get_service).and_return({something: 'testing'})

    na = make_network_allocation('public', 'squeaky', 'rangy')
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"')

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(1)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[0].dns_name_filter).to eq(dnf)
  end

  it 'must return false if can not claim_and_update' do
    allow(BarclampDns::MgmtService).to receive(:get_service).and_return({something: 'testing'})

    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "fred"')
    na = make_network_allocation('public', 'squeaky', 'rangy')
    expect(DnsNameFilter.claim_by_any(na)).to be_falsey
  end

  it 'must return true and create a new DNE when one does not exist' do
    allow(BarclampDns::MgmtService).to receive(:get_service).and_return({something: 'testing'})

    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"')
    na = make_network_allocation('public', 'squeaky', 'rangy')

    expect(DnsNameFilter.claim_by_any(na)).to be_truthy
    dnes = DnsNameEntry.all
    expect(dnes.length).to be(1)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[0].dns_name_filter).to eq(dnf)
    expect(dnes[0].rr_type).to eq('A')
  end


  it 'must release na when changed and no longer matches' do
    allow(BarclampDns::MgmtService).to receive(:get_service).and_return({something: 'testing'})

    na = make_network_allocation('public', 'squeaky', 'rangy')
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"')

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(1)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[0].dns_name_filter).to eq(dnf)

    dnf.matcher = 'deployment.name == "fred"'
    dnf.save!

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(0)
  end

  it 'must change name when changing template' do
    allow(BarclampDns::MgmtService).to receive(:get_service).and_return({something: 'testing'})
    allow(BarclampDns::MgmtService).to receive(:remove_ip_address).and_return(true)
    allow(BarclampDns::MgmtService).to receive(:add_ip_address).and_return(true)

    na = make_network_allocation('public', 'squeaky', 'rangy')
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"')

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(1)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[0].dns_name_filter).to eq(dnf)

    dnf.template = 'cow.test.com'
    dnf.save!

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(1)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[0].dns_name_filter).to eq(dnf)
    expect(dnes[0].rr_type).to eq('A')
    expect(dnes[0].name).to eq('cow.test.com')
  end

  it 'must release na when deleted' do
    allow(BarclampDns::MgmtService).to receive(:get_service).and_return({something: 'testing'})

    na = make_network_allocation('public', 'squeaky', 'rangy')
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"')

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(1)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[0].dns_name_filter).to eq(dnf)

    dnf.destroy!

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(0)
  end

  it 'must release na when deleted and get picked up' do
    allow(BarclampDns::MgmtService).to receive(:get_service).and_return({something: 'testing'})
    allow(BarclampDns::MgmtService).to receive(:remove_ip_address).and_return(true)
    allow(BarclampDns::MgmtService).to receive(:add_ip_address).and_return(true)

    na = make_network_allocation('public', 'squeaky', 'rangy')
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"', priority: 1)

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(1)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[0].dns_name_filter).to eq(dnf)

    dnf2 = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"', priority: 2)

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(2)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[1].network_allocation).to eq(na)

    dnf.destroy

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(1)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[0].dns_name_filter).to eq(dnf2)
  end


  it 'must release na when deleted and get picked up (order)' do
    allow(BarclampDns::MgmtService).to receive(:get_service).and_return({something: 'testing'})
    allow(BarclampDns::MgmtService).to receive(:remove_ip_address).and_return(true)
    allow(BarclampDns::MgmtService).to receive(:add_ip_address).and_return(true)

    na = make_network_allocation('public', 'squeaky', 'rangy')
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"', priority: 2)

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(1)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[0].dns_name_filter).to eq(dnf)

    dnf2 = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"', priority: 1)

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(2)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[1].network_allocation).to eq(na)

    dnf.destroy

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(1)
    expect(dnes[0].network_allocation).to eq(na)
    expect(dnes[0].dns_name_filter).to eq(dnf2)
  end

  it 'must not claim if service is not found' do
    na = make_network_allocation('public', 'squeaky', 'rangy')
    dnf = FactoryGirl.create(:dns_name_filter, matcher: 'deployment.name == "system"', priority: 2)

    dnes = DnsNameEntry.all
    expect(dnes.length).to be(0)
  end

end