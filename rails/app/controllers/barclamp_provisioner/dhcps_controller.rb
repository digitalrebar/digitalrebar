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
class BarclampProvisioner::DhcpsController < ::ApplicationController

  respond_to :json

  def index
    # we need to get the database
    entries = {}
    begin
      nr = Node.admin.first.node_roles.where(:name => 'provisioner-database').first
      if params.key? :id
        node = Node.find_key params[:id]
        entries = nr.sysdata['crowbar']['provisioner']['clients'][node.name]
      else
        entries = nr.sysdata['crowbar']['provisioner']
      end
      render api_array entries
    rescue
      render :text=>I18n.t('api.not_found', :type=>'provisioner_client', :id=>(params[:id] || ':all')), :status => :not_found
    end
  end

end
