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
#
class AttribsController < ApplicationController

  def sample
    render api_sample(Attrib)
  end

  def match
    attrs = Attrib.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = Attrib.where(ok_params) if !ok_params.empty?
    respond_to do |format|
      format.html {}
      format.json { render api_index :attrib, objs.as_json }
    end
  end

  def index
    target = find_target
    @list = target.nil? ? Attrib.all : target.attribs
    @list = @list.map do |i|
      e = i.as_json
      e["value"] = i.get(target)
      e
    end if target
    respond_to do |format|
      format.html { }
      format.json { render api_index :attrib, @list }
    end
  end

  def show
    target = find_target
    bucket = params[:bucket] ? params[:bucket].to_sym : :all
    @attrib = Attrib.find_key params[:id]
    if target.nil?
      respond_to do |format|
        format.html {  }
        format.json { render api_show @attrib }
      end
    else
      ret = @attrib.as_json
      ret["value"] = @attrib.get(target,bucket)
      # added node_id so what we can get backwards references if type is node
      ret["node_id"] = target.is_a?(Node) ? target.id : nil
      respond_to do |format|
        format.html { }
        format.json { render json: ret, content_type: cb_content_type(@attrib, "obj") }
      end
    end
  end

  def create
    params[:barclamp_id] = Barclamp.find_key(params[:barclamp]).id if params.has_key? :barclamp
    params[:role_id] =  Role.find_key(params[:role]).id if params.has_key? :role
    params.require(:name)
    params.require(:barclamp_id)
    @attrib = Attrib.create!(params.permit(:name,
                                       :barclamp_id,
                                       :role_id,
                                       :type,
                                       :description,
                                       :writable,
                                       :schema,
                                       :order,
                                       :map))
    render api_show @attrib
  end

  def update
    ret = Hash.new
    attrib = nil
    Attrib.transaction do
      attrib = Attrib.find_key(params[:id])
      # unpack form updates
      bucket = params[:bucket] ? params[:bucket].to_sym : :user
      if bucket != :user && bucket != :note
        render api_not_supported 'put', 'attribs/:id'
        return
      end
      target = find_target
      if target.nil?
        # We do not allow updating attribs outside the context of
        # some other object.
        render api_not_supported 'put', 'attribs/:id'
        return
      end
      target.lock!
      val = nil
      if request.patch?
        current_attrib = attrib.as_json
        current_attrib["value"]=attrib.get(target)
        Rails.logger.debug("patch_attrib: starting with #{current_attrib["value"].inspect}")
        Rails.logger.debug("patch_attrib: patch: #{request.raw_post}")
        val = JSON::Patch.new(current_attrib,JSON.parse(request.raw_post)).call["value"]
        Rails.logger.debug("patch attrib: patched to #{val}")
      else
        params[:value] = params[:attrib][:value] if params[:attrib]
        params.require(:value)
        params[:value] = params[:value].to_i if attrib.schema['type'] == 'int'
        val = params["value"]
      end
      Rails.logger.debug("update_attrib: saving #{val} to #{target.class.name}:#{target.uuid}")
      attrib.set(target,val, bucket)
      flash[:notice] = I18n.t('commit_required', :role => target.name)
      ret = attrib.as_json
      ret["value"] = val
    end
    render json: ret, content_type: cb_content_type(attrib, "obj")
  end

  def destroy
    @attrib = Attrib.find_key(params[:id] || params[:name])
    @attrib.destroy
    render api_delete @attrib
  end

  private

  def find_target
    case
    when params.has_key?(:node_id) then Node.find_key(params[:node_id])
    when params.has_key?(:role_id) then Role.find_key(params[:role_id])
    when params.has_key?(:node_role_id) then NodeRole.find_key(params[:node_role_id])
    when params.has_key?(:deployment_id) then Deployment.find_key(params[:deployment_id])
    when params.has_key?(:deployment_role_id) then DeploymentRole.find_key(params[:deployment_role_id])
    else nil
    end
  end

end
