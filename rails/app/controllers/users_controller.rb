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

require 'base64'

class UsersController < ApplicationController
  self.model = User
  self.cap_base = "USER"
  
  helper_method :is_edit_mode?

  skip_before_filter :rebar_auth, :only => [:options]
  skip_before_filter :authenticate_user!, :only => [:options]

  def cors_headers
    access_control = {
      'Access-Control-Allow-Origin' => request.headers["HTTP_ORIGIN"],
      'Access-Control-Allow-Headers' => 'X-Requested-With,Content-Type,Cookie,Authorization,WWW-Authenticate', # If-Modified-Since,If-None-Match,
      'Access-Control-Allow-Credentials' => true,
      'Access-Control-Expose-Headers' => 'WWW-Authenticate, Set-Cookie, Access-Control-Allow-Headers, Access-Control-Allow-Credentials, Access-Control-Allow-Origin'
    }
    access_control.each{ |k, v| response.headers[k] = v } if request.headers["HTTP_ORIGIN"]
  end

  def digest
    if request.get? or request.post? or request.head?
      if request.headers["HTTP_ORIGIN"]
        user = User.find_key session[:digest_user]
        if user
          render api_show(user), :status => :accepted
        else
          render :text => "digest", :status => :unauthorized
        end
      else
        if session[:digest_user]
          render :text => t('user.digest_success', :default=>'success'), :status => :ok
        else
          render :text => "digest", :status => :unauthorized
        end
      end
    elsif request.delete?
      session_reset
      render :nothing => true, :status => :ok
    end
  end

  # CORS header method - not used for rev_proxy
  def options
    cors_headers
    response.headers['Access-Control-Allow-Methods'] = 'GET,POST,PUT,DELETE,OPTIONS,PATCH,HEAD'
    render :nothing => true, :status => :no_content
  end

  def index
    @users = visible(model,cap("READ"))
    respond_to do |format|
      format.html { }
      format.json { render api_index User, @users }
    end
  end

 # RESTful DELETE of the node resource
  def destroy
    model.transaction do
      @user = find_key_cap(model, params[:id], cap("DESTROY"))
      @user.destroy
    end
    render api_delete @user
  end

  def create
    # Maybe UPDATE here instead?
    params.require(:username)
    params.require(:email)
    params[:tenant_id] ||= @current_user.current_tenant_id
    params[:current_tenant_id] ||= params[:tenant_id]
    model.transaction do
      t = find_key_cap(Tenant, params[:tenant_id],cap("READ","TENANT"))
      # Sanity-check that current_tenant_id is not being naughty.
      find_key_cap(Tenant, params[:current_tenant_id],cap("READ","TENANT"))
      validate_create(t.id)
      @user = User.create!(params.permit(user_params.map{|i|i.to_sym}))
      if params[:digest]
        @user.digest_password(params[:password])
        @user.save!
      end
    end
    respond_to do |format|
      format.html { } # show.html.erb
      format.json { render api_show @user }
    end
  end

  def update
    User.transaction do
      @user = find_key_cap(model,params[:id],cap("UPDATE")).lock!
      simple_update(@user,user_params())
      if params[:digest]
        @user.digest_password(params[:password])
        @user.save!
      end
    end
    respond_to do |format|
      format.html { } # show.html.erb
      format.json { render api_show @user }
    end
  end

  def digest_password
    @user = find_key_cap(model, params[:id], cap("READ_DIGEST"))
    render api_show @user.encrypted_password
  end

  def capabilities
    data = {}
    model.transaction do
      @user = find_key_cap(model, params[:id], cap("READ_CAPABILITIES"))
      data = @user.cap_map
    end
    render json: data
  end

  def show
    @user = find_key_cap(model, params[:id], cap("READ"))
    respond_to do |format|
      format.html { } # show.html.erb
      format.json { render api_show @user }
    end
  end

  def start_password_reset
    User.transaction do
      # Probably need a PASSWORD_CHANGE cap or something.
      @user = find_key_cap(model, params[:id],cap("UPDATE"))
      unless current_user.is_admin || current_user == @user
        sleep 2
        raise "Cannot start password change"
      end
      PasswordChangeToken.where(user_id: @user.id).delete_all
      render api_show PasswordChangeToken.create!(user: @user)
    end
  end

  def complete_password_reset
    # Same as the last one.
    @user = find_key_cap(model,params[:id],cap("UPDATE"))
    User.transaction do 
      token = PasswordChangeToken.find_by!(token: params[:token])
      unless @user.id == token.user_id && (current_user == @user || current_user.is_admin)
        sleep 2
        raise "Password change failed"
      end
      payload = token.decode(params[:decoder],params[:nonce],params[:payload])
      @user.digest_password(payload["password"])
      @user.save!
    end
    render api_show @user
  end

  # Ditto
  def is_edit_mode?
    current_user.is_admin? && Rails.env.development?
  end

  # Ditto
  def edit
    fetch_user
    edit_common
  end

  # Ditto
  def edit_password
    code, exception = fetch_user
    @user.admin_reset_password = true if code == 200
    edit_common
  end

  private

  def user_params
    fields = %w(username email password password_confirmation remember_me tenant_id current_tenant_id)
    fields << "is_admin" if current_user.is_admin
    fields
  end

  def edit_common
    setup_users
    render :action => :index
  end

  def check_password
    if params[:user][:password].blank?
      params[:user].delete(:password)
      params[:user].delete(:password_confirmation)
    end
  end

  def setup_users
    if (current_user.is_admin)
      @users = User.all if @users.nil?
      @user ||= User.new
    else
      @users = [current_user]
    end
    
  end

  def update_admin(onOff=false)
    begin
      @user.is_admin = onOff;
      @user.save
      [200, ""]
    rescue ActiveRecord::RecordNotFound => ex
      Rails.logger.warn(ex.message)
      [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      [500, ex.message]
    end
  end
  
  def fetch_user
    ret = nil
    begin
      @user = User.find_key((params[:user].nil? or params[:user][:id].nil?) ? \
      ((params[:user_id].nil?) ? params[:id] : params[:user_id]) : \
      params[:user][:id])
      ret = [200, ""]
    rescue ActiveRecord::RecordNotFound => ex
      puts "ActiveRecord::RecordNotFound #{ex}"
      Rails.logger.warn(ex.message)
      ret = [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      puts "RuntimeError #{ex}"
      ret = [500, ex.message]
    end
    ret
  end
  
  def fetch_users
    ret = nil
    begin
      @users = User.all
      ret = [200,  ""]
    rescue ActiveRecord::RecordNotFound => ex
      puts "ActiveRecord::RecordNotFound #{ex}"
      Rails.logger.warn(ex.message)
      ret = [404, ex.message]
    rescue RuntimeError => ex
      Rails.logger.error(ex.message)
      puts "RuntimeError #{ex}"
      ret = [500, ex.message]
    end
    ret
  end
  
  def populate_user
    return User.new(params[:user]) unless params[:user].nil?
    User.new(:username => params[:username], :email => params[:email], :password => params[:password], \
     :password_confirmation => params[:password_confirmation], :remember_me => params[:remember_me], \
     :is_admin => params[:is_admin])
  end
end
