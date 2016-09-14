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
#
require 'spec_helper'

describe "admin create" do

  subject { Node.create! :name=>'rspec-admin.rebar.com', :admin => true, :deployment => Deployment.system }

  it {should be_is_admin} 

  it "must have implicit roles" do
    rr = FactoryGirl.create(:role, implicit: true, name: 'child')
    RoleRequire.find_or_create_by!(:role_id => rr.id, :requires => 'parent')
    rr2 = FactoryGirl.create(:role, implicit: true, name: 'parent')

    rr.add_to_node(subject)
    subject.commit!
    expect( subject.node_roles.count{ |e| e.role.implicit } ).to be(2)
  end

  it "must be added to the deployment" do
    subject.commit!
    expect( Deployment.system.nodes.any?{ |e| e.id == subject.id } ).to be_truthy
  end
end

describe "node create" do

  subject { Node.create! :name=>'rspec-node.rebar.com', :admin => false, :deployment => Deployment.system }

  it "must have implicit roles" do
    rr = FactoryGirl.create(:role, implicit: true, name: 'child')
    RoleRequire.find_or_create_by!(:role_id => rr.id, :requires => 'parent')
    rr2 = FactoryGirl.create(:role, implicit: true, discovery: true, name: 'parent')

    rr.add_to_node(subject)
    subject.commit!
    expect( subject.node_roles.any?{ |e| e.role.implicit } ).to be_truthy
    expect( subject.node_roles.any?{ |e| e.role.discovery } ).to be_truthy
  end

  it "must be added to the deployment" do
    subject.commit!
    expect( Deployment.system.nodes.any?{ |e| e.id == subject.id } ).to be_truthy
  end

end

