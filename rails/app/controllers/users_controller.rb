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
  respond_to :html, :json
  
  helper_method :is_edit_mode?

  add_help(:index,[],[:get])

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

  def sample
    render api_show({},User)
  end

  def match
    attrs = User.attribute_names.map{|a|a.to_sym}
    objs = []
    ok_params = params.permit(attrs)
    objs = validate_match(ok_params, :tenant_id, "USER", User)
    respond_to do |format|
      format.html {}
      format.json { render api_index User, objs }
    end
  end
  
  def index
    t_ids = build_tenant_list("USER_READ")
    @users = User.where(tenant_id: t_ids)
    respond_to do |format|
      format.html { }
      format.json { render api_index User, @users }
    end
  end

 # RESTful DELETE of the node resource
  def destroy
    @user = User.find_key params[:id]
    validate_destroy(@user.tenant_id, "USER", User, params[:id])
    @user.destroy
    render api_delete @user
  end

  add_help(:create,[:username, :email, :password, :password_confirmation, :remember_me, :is_admin, :digest],[:post])
  def create
    params.require(:username)
    params.require(:email)
    unless params[:tenant_id]
      params[:tenant_id] = @current_user.current_tenant_id
    end
    params[:tenant_id] = params[:tenant_id].to_i
    unless params[:current_tenant_id]
      params[:current_tenant_id] = params[:tenant_id]
    end
    params[:current_tenant_id] = params[:current_tenant_id].to_i
    validate_create(params[:tenant_id], "USER", User)
    @user = User.create! user_params
    if params[:digest]
      @user.digest_password(params[:password])
      @user.save!
    end
    respond_to do |format|
      format.html { } # show.html.erb
      format.json { render api_show @user }
    end
  end

  def update
    User.transaction do
      @user = User.find_key(params[:id]).lock!
      validate_update(@user.tenant_id, "USER", User, params[:id])
      if request.patch?
        fields = %w{username email}
        fields << "is_admin" if current_user.is_admin && current_user.id != @user.id
        patch(@user,fields)
      else
        @user.update_attributes!(user_params)
        @user.save!
      end
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
      @user = User.find_key(params[:id])
      validate_action(@user.tenant_id, "USER", User, params[:id], "READ_DIGEST")
      render api_show @user.encrypted_password
  end

  def capabilities
      @user = User.find_key(params[:id])
      validate_action(@user.tenant_id, "USER", User, params[:id], "READ_CAPABILITIES")
      data = @user.cap_map
      render json: data
  end

  add_help(:show,[:id],[:get])
  def show
    @user = User.find_key params[:id]
    validate_read(@user.tenant_id, "USER", User, params[:id])
    respond_to do |format|
      format.html { } # show.html.erb
      format.json { render api_show @user }
    end
  end

  add_help(:unlock,[:id],[:delete]) 
  def unlock
    # TODO REFACTOR!
    respond_with(@user)  do |format|
      @user.unlock_access! if (!@user.nil? and @user.access_locked?)
      format.html do
        redirect_to users_path, :notice => t("users.index.unlocked")
      end
      format.json do
        render api_show @user
      end
    end
  end

  add_help(:lock,[:id],[:post])
  def lock
    # TODO REFACTOR!
    respond_with(@user)  do |format|
      @user.lock_access! if (!@user.nil? and !@user.access_locked?)
      format.html do
        redirect_to users_path, :notice => t("users.index.locked")
      end
      format.json do
        render api_show @user
      end
    end
  end

  def start_password_reset
    User.transaction do
      @user = User.find_key(params[:id])
      unless current_user.is_admin || current_user == @user
        sleep 2
        raise "Cannot start password change"
      end
      PasswordChangeToken.where(user_id: @user.id).delete_all
      render api_show PasswordChangeToken.create!(user: @user)
    end
  end

  def complete_password_reset
    @user = User.find_key(params[:id])
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

 add_help(:reset_password,[:id, :password, :password_confirmation],[:put])
 def reset_password
    #  TODO REFACTOR!
   ret = fetch_user
   respond_with(@user)  do |format|
    Rails.logger.debug("Reset password for user #{@user}")
    format.html do
      if !params[:cancel].nil?
        @user = nil
        setup_users
        return render :action => :index
      end
      check_password
      @user.admin_reset_password = true
      if @user.reset_password!(params[:user][:password],params[:user][:password_confirmation])
        redirect_to users_path, :notice => t("users.index.reset_password_success")
      else
        setup_users
        render :action => :index
      end
    end
    format.json do
      password = params[:password]
      password_confirmation = params[:password_confirmation]
      begin
         @user.admin_reset_password = true
         reset_success = @user.reset_password!(password, password_confirmation)
         raise ActiveRecord::RecordInvalid.new(@user) unless reset_success
      rescue ActiveRecord::RecordInvalid, ArgumentError => ex
          Rails.logger.error(ex.message)
          ret = [500, ex.message]
      end  if ret[0]==200
      return render :text => ret[1], :status => ret[0] unless ret[0] == 200
      render api_show @user
    end
   end
 end

  def is_edit_mode?
    current_user.is_admin? && Rails.env.development?
  end
  
  add_help(:make_admin,[:id],[:post])
  def make_admin
    ret = fetch_user
    respond_with(@user)  do |format|
      Rails.logger.debug("Making user #{@user.id} admin") unless @user.nil?
      format.html do
        @user.is_admin = true;
        @user.save
        render
      end
      format.json do
        ret = update_admin(true) if ret[0] == 200
        return render :text => ret[1], :status => ret[0] unless ret[0] == 200
        render api_show @user
      end
    end
  end

  add_help(:remove_admin,[:id],[:delete])
  def remove_admin
    ret = fetch_user
    respond_with(@user) do |format|
      format.html do
        @user.is_admin = false;
        @user.save
        render
      end
      format.json do
        ret = update_admin(false) if ret[0] == 200
        return render :text => ret[1], :status => ret[0] unless ret[0] == 200
        render :json => @user.to_json
      end
    end
  end
  
  def edit
    fetch_user
    edit_common
  end

  def edit_password
    code, exception = fetch_user
    @user.admin_reset_password = true if code == 200
    edit_common
  end

  private

  def user_params
    fields = [:username, :email, :password, :password_confirmation, :remember_me, :tenant_id, :current_tenant_id ]
    fields << :is_admin if current_user.is_admin
    params.permit(*fields)
  end

  def required_user_params
    [:username, :email].each do |k|
      params.require(k)
    end
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
