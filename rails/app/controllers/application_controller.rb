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

require 'uri'
require 'digest/md5'
require 'active_support/core_ext/string'
require 'json'

# Filters added to this controller apply to all controllers in the application.
# Likewise, all the methods added will be available for all controllers.
class ApplicationController < ActionController::Base

  before_filter :rebar_auth
  after_filter  :filter_json

  # Helpers for the capability system.
  # Allows each controller to express which model and capability
  # set it should interact with by default
  class_attribute :model, :cap_base

  def model
    self.class.model
  end

  def cap_base
    self.class.cap_base
  end
  
  # Construct a capability by adding cap_base to cap_action
  def cap(cap_action, base = self.cap_base)
    "#{base}_#{cap_action}"
  end

  #helper :all # include all helpers, all the time

  protect_from_forgery # See ActionController::RequestForgeryProtection for details
  skip_before_action :verify_authenticity_token, if: :digest_request?

  def self.set_layout(template = "application")
    layout proc { |controller|
      if controller.is_ajax?
        return nil
      end
      template
    }
  end

  # needed by Devise auth
  def is_ajax?
    request.xhr?
  end

  # Common sample method.  It is basically identical for every controller.
  def sample
    if self.model
      j = self.model.column_defaults.reject{|k,v| /(^id)|_(id|at)$/ =~ k}
      render api_show(j,self.class.model)
    else
      render api_not_implemented(self.model, "sample", "")
    end
  end

  # Common match method.  It is basically identical for every controller
  def match
    attrs = self.model.attribute_names.map{|a|a.to_sym}
    ok_params = params.permit(attrs)
    objs = ok_params.empty? ? model.where('false') : visible(self.model,cap("READ")).where(ok_params)
    respond_to do |format|
      format.html { }
      format.json { render api_index(self.model, objs) }
    end
  end
  
  # Given an object and list of permitted object attributes to update, extract the
  # JSON patch that should be in the request body, apply it to the object cast as a
  # JSON blob, and update the permitted attribs of the actual object.
  # If the patch fails to apply, or if the patch tries to update an attribute not in
  # the permitted list, fail.
  #
  # This method MUST be called within a transaction, and obj MUST be locked in order
  # to guarantee that the requested changes happen atomically.
  def patch(obj, permitted_attribs)
    patch = JSON.parse(request.raw_post)
    Rails.logger.debug(obj.as_json)
    Rails.logger.debug(request.raw_post)
    raise "Patch not formatted properly" unless patch.is_a?(Array)
    res = JSON::Patch.new(obj.as_json,patch).call
    # Now, sanity-check res to make sure it only updated the keys we wanted it to
    res.keys.each do |k|
      # Ignore synthetic keys
      next unless obj.has_attribute?(k)
      # Nothing changed, OK.
      next if obj[k] == res[k]
      if permitted_attribs.member?(k)
        obj[k] = res[k]
      else
        raise "Cannot update attribute #{k} for #{obj.class.name}"
      end
    end
    obj.save!
  end

  # creates the content type for a consistent API
  def cb_content_type(type, form="list", type_override=nil)
    type = type_override || type2name(type)
    "application/vnd.rebar.#{type}.#{form}+json; version=2.0"
  end

  def api_not_found(e)
    json = {}
    json[:status] = 404
    if e.rebar_key
      json[:message]=I18n.t('api.not_found', :id=>e.rebar_key, :type=>type2name(e.rebar_model))
    else
      json[:message]=e.message
      json[:backtrace]=e.backtrace
    end
    { :json => json,
      :status => :not_found
    }
  end

  def api_conflict_error(e)
    json = {}
    json[:status] = 409
    if (e.rebar_key rescue false)
      json[:message]=I18n.t('api.not_found', :id=>e.rebar_key, :type=>type2name(e.rebar_model))
    else
      json[:message]=e.message
      json[:backtrace]=e.backtrace
    end
    { :json => json,
      :status => 409
    }
  end

  def api_forbidden(e)
    json = {}
    json[:status] = 403
    if e.rebar_key
      json[:message]=I18n.t('api.not_found', :id=>e.rebar_key, :type=>type2name(e.rebar_model))
    else
      json[:message]=e.message
      json[:backtrace]=e.backtrace
    end
    { :json => json,
      :status => :forbidden
    }
  end

  def api_conflict(object)
    return {:json => {
        :message => I18n.t('api.conflict', :item=>object.id),
        :status => 409
      },
      :status => 409,
      :content_type=>cb_content_type(object, "error")
    }
  end

  def api_not_supported(verb, object)
    return {:json => {
        :message => I18n.t('api.not_supported', :verb=>verb.upcase, :obj=>object),
        :status => 405
      },
      :status => 405,
      :content_type=>cb_content_type(object, "error")
    }
  end

  # 501 for actions that are not supported
  def api_not_implemented(object, attempted, suggestion)
    return {:json => {
        :message => I18n.t('api.not_implemented', :requested=>attempted, :suggestion=>suggestion),
        :status => 501
      },
      :status => 501,
      :content_type=>cb_content_type(object, "error")
    }
  end

  def ui_not_supported(verb, object)
    return { :text=>I18n.t('ui.not_supported', :verb=>verb.upcase, :obj=>object),
      :status => 405,
      :content_type=>cb_content_type(object, "error")
    }
  end

  # formats API json output
  # using this makes it easier to update the API format for all models
  def api_index(type, list)
    return {:json=>list, :content_type=>cb_content_type(type, "list") }
  end

  # formats API json for output
  # using this makes it easier to update the API format for all models
  def api_show(o, type_override=nil)
    return {:json=>o, :content_type=>cb_content_type(o, "obj", type_override) }
  end

  # formats API for delete
  # using this makes it easier to update the API format for all models
  def api_delete(o)
    return {:json => {
        :message => I18n.t('api.deleted', :id=>o.id, :obj=>type2name(o))
      },
      :content_type=>cb_content_type(o, "empty")
    }
  end

  # formats API json output
  # used for json output that is not mapped to a Rebar model
  def api_result(json)
    return {json: json, content_type: cb_content_type("json", "result") }
  end

  # formats API json output
  # used for json results output that is not mapped to a Rebar model
  def api_array(json)
    return {:json=>json, :content_type=>cb_content_type("json", "array") }
  end

  set_layout

  unless Rails.application.config.consider_all_requests_local
    rescue_from StandardError, :with => :render_error
  end

  private

  def filter_one_entry(ent,attrs)
    res = {}
    attrs.each do |k|
      next unless ent.key?(k)
      res[k] = ent[k]
    end
    res
  end

  def filter_json
    unless (response.status == 200) &&
        request.headers["x-return-attributes"] &&
        (response.content_type =~ /json/)
      return
    end
    body = JSON.parse(response.body)
    filter_attributes = JSON.parse(request.headers["x-return-attributes"])
    if body.is_a?(Array)
      filtered_body = body.map{|ent| filter_one_entry(ent,filter_attributes)}
    else
      filtered_body = filter_one_entry(body,filter_attributes)
    end
    response.body = JSON.generate(filtered_body)
  end

  def type2name(type)
    case
    when type.kind_of?(ActiveRecord::Base) then type.class.name.underscore
    when type.respond_to?(:descends_from_active_record?) &&
        type.descends_from_active_record? then type.name.underscore
    when type.kind_of?(String) then type.underscore
    when type.kind_of?(Symbol) then type.to_s.underscore
    when type.nil? then "unknown"
    else raise "type2name cannot handle #{type.inspect}"
    end
  end

  def render_error(exception)
    @error = exception
    Rails.logger.debug("Failed: #{@error.message}")
    Rails.logger.debug(@error.backtrace)
    case
    when @error.is_a?(ActiveRecord::RecordNotFound), @error.is_a?(RebarNotFoundError)
      respond_to do |format|
        format.html { render :status => 404 }
        format.json { render api_not_found(@error) }
      end
    when @error.is_a?(ActiveRecord::RecordNotUnique), @error.is_a?(PG::UniqueViolation)
      respond_to do |format|
        format.html { render :status => 409 }
        format.json { render api_conflict_error(@error) }
      end
    when @error.is_a?(RebarForbiddenError)
      respond_to do |format|
        format.html { render :status => 403 }
        format.json { render api_forbidden(@error) }
      end
    when @error.is_a?(ActiveRecord::RecordInvalid)
      Rails.logger.error("Failed: #{@error.message}")
      Rails.logger.error(@error.backtrace)
      respond_to do |format|
        format.html { render :status => 409 }
        format.json { render :json => {
            :message => @error.message,
            :backtrace => @error.backtrace,
            :status => 409
          },
          :status => 409,
          :content_type=>cb_content_type("record", "error")
        }
      end
    else
      Rails.logger.error("EXCEPTION: #{@error.message}")
      Rails.logger.error("BACKTRACE:\n#{@error.backtrace.join("\n")}")
      respond_to do |format|
        format.html { render :template => "/errors/500.html.haml", :status => 500 }
        format.json { render :json => {
            :message => @error.message,
            :backtrace => @error.backtrace,
            :status => 500
          },
          :status => 500,
          :content_type=>cb_content_type("framework", "error")
        }
      end
    end
  end

  protected

  def cors_headers
    access_control = {
      'Access-Control-Allow-Origin' => request.headers["HTTP_ORIGIN"],
      'Access-Control-Allow-Headers' => 'X-Requested-With,Content-Type,Cookie,Authorization,WWW-Authenticate', # If-Modified-Since,If-None-Match,
      'Access-Control-Allow-Credentials' => true,
      'Access-Control-Expose-Headers' => 'WWW-Authenticate, Set-Cookie, Access-Control-Allow-Headers, Access-Control-Allow-Credentials, Access-Control-Allow-Origin'
    }
    access_control.each{ |k, v| response.headers[k] = v } if request.headers["HTTP_ORIGIN"]
  end

  def digest_request?
    request.headers["HTTP_AUTHORIZATION"] && request.headers["HTTP_AUTHORIZATION"].starts_with?('Digest username=')
  end

  def digest_auth!
    u = nil
    cors_headers
    authed = authenticate_or_request_with_http_digest(User::DIGEST_REALM) do |username|
      u = User.find_by!(username: username)
      session[:digest_user] = u.username
      u.encrypted_password
    end
    Rails.logger.info("digest auth for #{u ? u.username : "unknown"}: #{authed}")
    @current_user = u if authed
    authed
  end

  def do_auth!
    Rails.logger.info("Fail through auth: do_auth!")
    session[:marker] = "login"
    session[:start] = Time.now
    respond_to do |format|
      format.html { authenticate_user! }
      format.json { digest_auth! unless signed_in? :user }
    end
  end

  #return true if we digest signed in
  def rebar_auth
    Rails.logger.debug("peercert: #{request.headers["puma.socket"].peercert}")
    Rails.logger.info("username header: #{request.headers["HTTP_X_AUTHENTICATED_USERNAME"]}")
    Rails.logger.info("capability header: #{request.headers["HTTP_X_AUTHENTICATED_CAPABILITY"]}")
    case
    when request.headers["puma.socket"].peercert && !request.headers["HTTP_X_AUTHENTICATED_USERNAME"].nil?
      # This assumes that the rev-proxy is handling cors
      username = request.headers["HTTP_X_AUTHENTICATED_USERNAME"]
      capability = request.headers["HTTP_X_AUTHENTICATED_CAPABILITY"]
      wants_admin = capability == "ADMIN"
      Rails.logger.info("Auth by key: #{username}")
      Rails.logger.info("headers['HTTP_ORIGIN'] = #{request.headers["HTTP_ORIGIN"]}")

      email = "#{username}@internal.local"
      if username =~ /@/
        email = username
        username = username.split("@")[0]
      end

      @current_user = User.create_with(email: email).find_or_create_by(username: username)
      # Update the capabiilties
      if @current_user.is_admin != wants_admin
        @current_user.is_admin = wants_admin
        @current_user.save
        @current_user = User.find_by(username: username)
      end
      session[:digest_user] = username
      true
    when current_user
      val = authenticate_user!
      @current_user = current_user if val
      val
    when digest_request?
      digest_auth!
    when request.path == '/api/license'
      true  # specialized path for license & URL validation
    when (request.local? ||
          (/^::ffff:127\.0\.0\.1$/ =~ request.remote_ip)) &&
        File.exists?("/tmp/.rebar_in_bootstrap") &&
        (File.stat("/tmp/.rebar_in_bootstrap").uid == 0)
      @current_user = User.find_by(username: "rebar")
      true
    else
      do_auth!
    end
  end

  # See if the current user has this capability in the given tenant id
  def capable(t_id, cap)
    @current_user.capable(cap, t_id)
  end

  # A little visibility helper for lists
  def visible(klass, cap)
    klass.visible(cap, @current_user.id)
  end

  # Try to find an object filtered by capability
  def find_key_cap(klass, key, cap)
    klass.find_key_cap(key, cap, @current_user.id)
  end

  #
  # Given a capabilitiy, create a list of tenant ids that can be operated on.
  # This is mostly for read capabilities
  #
  def build_tenant_list(cap, cap_map = @current_user.cap_map, t_id = @current_user.current_tenant_id)
    self.class.build_tenant_list(cap, cap_map, t_id)
  end

  def self.build_tenant_list(cap, cap_map, t_id)
    found = false
    while cap_map[t_id] do
      if cap_map[t_id]["capabilities"].include? cap
        found = true
        break
      end
      t_id = cap_map[t_id]["parent"]
    end

    if found
      t_ids = [ t_id ]
      t_ids << Tenant.where(["id IN (select child_id from all_tenant_parents where parent_id = :ten)", {ten: t_id}]).map { |x| x.id }
      return t_ids.flatten
    end

    t = Tenant.find_by(id: t_id)
    return [] unless t

    t_ids = []
    t.children.map {|x| x.id }.each do |nt|
      t_ids << self.build_tenant_list(cap, cap_map, nt)
    end

    return t_ids.flatten
  end

  # Validation helpers
  def validate_action(t_id, cap_base, klass, key, action)
    return true if capable(t_id, "#{cap_base}_#{action}")
    raise RebarForbiddenError.new(key, klass) if capable(t_id, "#{cap_base}_READ")
    raise RebarNotFoundError.new(key, klass)
  end

  def validate_update(t_id, cap_base, klass, key)
    validate_action(t_id, cap_base, klass, key, "UPDATE")
  end

  def validate_read(t_id, cap_base, klass, key)
    raise RebarNotFoundError.new(key, klass) unless capable(t_id, "#{cap_base}_READ")
  end

  def validate_create(t_id, cap_base, klass)
    raise RebarForbiddenError.new("new", klass) unless capable(t_id, "#{cap_base}_CREATE")
  end

  def validate_destroy(t_id, cap_base, klass, key)
    validate_action(t_id, cap_base, klass, key, "DESTROY")
  end

  def validate_match(ok_params, t_key, cap_base, klass)
    return klass.where("false") if ok_params.empty?
    visible(klass, cap_base+ "_READ").where(ok_params)
  end

end
