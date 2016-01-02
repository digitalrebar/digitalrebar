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

  # Basis for the reflection/help system.

  # First, a place to stash the help contents.
  # Using a class_inheritable_accessor ensures that
  # these contents are inherited by children, but can be
  # overridden or appended to by child classes without messing up
  # the contents we are building here.
  class_attribute :help_contents
  self.help_contents = []

  # Class method for adding method-specific help/API information
  # for each method we are going to expose to the CLI.
  # Since it is a class method, it will not be bothered by the Rails
  # trying to expose it to everything else, and we can call it to build
  # up our help contents at class creation time instead of instance  creation
  # time, so there is minimal overhead.
  # Since we are just storing an arrray of singleton hashes, adding more
  # user-oriented stuff (descriptions, exmaples, etc.) should not be a problem.
  def self.add_help(method,args=[],http_method=[:get])
    # if we were passed multiple http_methods, build an entry for each.
    # This assumes that they all take the same parameters, if they do not
    # you should call add_help for each different set of parameters that the
    # method/http_method combo can take.
    http_method.each { |m|
      self.help_contents = self.help_contents.push({
        method => {
                                             "args" => args,
                                             "http_method" => m
        }
      })
    }
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

  def api_sample(model)
    j = model.column_defaults.reject{|k,v| /(^id)|_(id|at)$/ =~ k}
    api_show(j,model)
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
      :content_type=>cb_content_type(e.rebar_key, "error"),
      :status => :not_found
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

  add_help(:help)
  def help
    render :json => { self.controller_name => self.help_contents.collect { |m|
        res = {}
        m.each { |k,v|
          # sigh, we cannot resolve url_for at clqass definition time.
          # I suppose we have to do it at runtime.
          url=URI::unescape(url_for({ :action => k,
                        :controller => self.controller_name,

          }.merge(v["args"].inject({}) {|acc,x|
            acc.merge({x.to_s => "(#{x.to_s})"})
          }
          )
          ))
          res.merge!({ k.to_s => v.merge({"url" => url})})
        }
        res
      }
    }
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
    case
    when @error.is_a?(ActiveRecord::RecordNotFound)
      respond_to do |format|
        format.html { render :status => 404 }
        format.json { render api_not_found(@error) }
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
      'Access-Control-Allow-Headers' => 'X-Requested-With,Content-Type,Cookie,Authorization', # If-Modified-Since,If-None-Match,
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
    @current_user = u if authed
    authed
  end

  def do_auth!
    session[:marker] = "login"
    session[:start] = Time.now
    respond_to do |format|
      format.html { authenticate_user! }
      format.json { digest_auth! unless signed_in? :user }
    end
  end

  #return true if we digest signed in
  def rebar_auth
    Rails.logger.error("ZEHICLE: #{request.headers['HTTP_AUTHORIZATION']}")
    case
    when current_user then authenticate_user!
    when digest_request? then digest_auth!
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
end
