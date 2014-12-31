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

describe "jig proposal manipulation" do
  # make sure that the there's a crowbar deploment (named 'test')
  include_context "crowbar test deployment"
  # just 2 nodes.
  include_context "2 dummy nodes"


  context "test deployment with 2 nodes" do

    let(:active)   { deployment.head}

    before(:all) {            
      # add node
    }

    it "we can commit the deployment" do
      #deployment.commit
    end

  end
end

