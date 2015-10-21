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


class SupportController < ApplicationController

  skip_before_filter :rebar_auth, :only => :digest
  before_filter :digest_auth!, :only => :digest

  def eula
  end
  
  # used to pass a string into the debug logger to help find specificall calls  
  def marker
    Rails.logger.info "\nMARK >>>>> #{params[:id]} <<<<< KRAM\n"
    render :text=>params[:id]
  end

  def digest
    if session[:digest_user]
      render :text => t('user.digest_success', :default=>'success')
    else
      render :text => "digest", :status => :unauthorized
    end
  end

  def fail
    raise I18n.t('chuck_norris')
  end

  # used to lookup localization values
  def i18n
    begin
      render :text=>I18n.translate(params[:id], :raise => I18n::MissingTranslationData)
    rescue I18n::MissingTranslationData
      render :text=>"No translation for #{params[:id]}", :status => 404
    end
  end

  # Legacy Support (UI version moved to loggin barclamp)
  def logs
    @file = "rebar-logs-#{ctime}.tar.bz2"
    system("sudo -i /opt/dell/bin/gather_logs.sh #{@file}")
    redirect_to "/export/#{@file}"
  end
  
  def get_cli
    system("sudo -i /opt/dell/bin/gather_cli.sh #{request.env['SERVER_ADDR']} #{request.env['SERVER_PORT']}")
    redirect_to "/rebar-cli.tar.gz"
  end

  def import
    @barclamps = Barclamp.all
  end

  def index
    @waiting = params['waiting'] == 'true'
    remove_file 'export', params[:id] if params[:id]
    @exports = { :count=>0, :logs=>[], :cli=>[], :chef=>[], :other=>[], :bc_import=>[] }
    Dir.entries(export_dir).each do |f|
      if f =~ /^\./
        next # ignore rest of loop
      elsif f =~ /^KEEP_THIS.*/
        next # ignore rest of loop
      elsif f =~ /^rebar-logs-.*/
        @exports[:logs] << f 
      elsif f =~ /^rebar-cli-.*/
        @exports[:cli] << f
      elsif f =~ /^rebar-chef-.*/
        @exports[:chef] << f 
      elsif f =~ /(.*).import.log$/
        @exports[:bc_import] << f 
      else
        @exports[:other] << f
      end
      @exports[:count] += 1
      @file = params['file'] if params['file']
      @waiting = false if @file == f
    end
    respond_to do |format|
      format.html # index.html.haml
      format.json { render :json => @exports }
    end
  end

  # used by BDD to create Admin node
  def bootstrap_post
    # only create if no other netwroks
    if Network.where(:name=>'admin').count == 0
      deployment = Deployment.system
      Network.transaction do
        # admin network
        net = Network.create :name=>'admin', :description=>I18n.t('support.bootstrap.admin_net', :default=>""),  :deployment_id=>deployment.id, :conduit=>Network::DEFAULTCONDUIT, :v6prefix => Network::V6AUTO, :category => "admin"
        NetworkRange.create :name=>'admin', :network_id=>net.id, :first=>"192.168.124.10/24", :last=>"192.168.124.11/24"
        NetworkRange.create :name=>'dhcp', :network_id=>net.id, :first=>"192.168.124.21/24", :last=>"192.168.124.80/24"
        NetworkRange.create :name=>'host', :network_id=>net.id, :first=>"192.168.124.81/24", :last=>"192.168.124.254/24"
        # bmc network
        bmc = Network.create :name=>'bmc', :description=>I18n.t('support.bootstrap.bmc_net', :default=>""),  :deployment_id=>deployment.id, :conduit=>Network::BMCCONDUIT, :v6prefix => Network::V6AUTO, :category => "bmc"
        NetworkRange.create :name=>'admin', :network_id=>bmc.id, :first=>"192.168.128.10/24", :last=>"192.168.128.20/24"
        NetworkRange.create :name=>'host', :network_id=>bmc.id, :first=>"192.168.128.21/24", :last=>"192.168.128.254/24"

      end
    end
  end

  def restart
    @init = false
    if params[:id].nil?
      render
    elsif params[:id].eql? "request" or params[:id].eql? "import"
      @init = true
      render
    elsif params[:id].eql? "in_process"
      %x[sudo bluepill rebar-webserver restart] unless Rails.env == 'development'
      render :json=>false
    elsif params[:id].eql? SERVER_PID
      render :json=>false
    elsif !params[:id].eql? SERVER_PID
      render :json=>true
    else
      render
    end
  end

  # allows user to change UI behaviors  
  def settings_put
  
    # expected to set ALL values in one put using checkboxes
    # for this reason, missing values are assumed FALSE
    current_user.settings(:ui).refresh = params[:refresh].to_i rescue current_user.settings(:ui).refresh
    current_user.settings(:ui).fast_refresh = params[:fast_refresh].to_i rescue current_user.settings(:ui).fast_refresh
    current_user.settings(:ui).edge = params[:edge].eql?('true') rescue false
    current_user.settings(:ui).test = params[:test].eql?('true') rescue false
    current_user.settings(:ui).debug = params[:debug].eql?('true') rescue false
    current_user.settings(:ui).milestone_roles = params[:milestone_roles].eql?('true') rescue false
    current_user.settings(:errors).expand = params[:expand].eql?('true') rescue false
    current_user.save!
    #render :json=>true
    redirect_to :action => :settings

  end

  
  def settings
    respond_to do |format|
      format.html { }
    end
  end

  # return the queue status
  def queue
    j = { }
    render :json=>j
  end

  # supplies UI heartbeat information
  def heartbeat
    if session[:marker] != params[:marker]
      session[:marker] = params[:marker]
      session[:start] = Time.now
    end
    elapsed = (Time.now - session[:start]) rescue 0
    total = NodeRole.count
    error = NodeRole.where(state: NodeRole::ERROR).count
    active = NodeRole.where(state: NodeRole::ACTIVE).count
    render :json=>{ :active=>active, :todo=>(total-error-active), :error=>error, :elapsed=>elapsed.to_i } 
  end

  private 

  def bootstrap_update(yaml, key, config, params)  
    # scan the keys we have
    config[key].each_index do |i|
      r = params["#{key}|#{i}"]
      config[key][i] = (yaml ? YAML::load(r) : JSON.parse(r))
    end
    # add new keys
    unless ["{}","---"].include? params["#{key}|new"]
      r = params["#{key}|new"]
      config[key] << (yaml ? YAML::load(r) : JSON.parse(r))
    end
  end


  def ctime
    Time.now.strftime("%Y%m%d-%H%M%S")
  end
  
  def import_dir
    check_dir 'import'
  end
  
  def export_dir
    check_dir 'export'
  end

  def check_dir(type)
    d = File.join 'public', type
    unless File.directory? d
      Dir.mkdir d
    end
    return d    
  end
  
  def remove_file(type, name)
    begin
      f = File.join(check_dir(type), name)
      File.delete f
      flash[:notice] = t('support.index.delete_succeeded') + ": " + f
    rescue
      flash[:notice] = t('support.index.delete_failed') + ": " + f
    end
  end
  
  def do_auth!
    case
    when request.fullpath.index("/get_cli") || request.fullpath.index("/logs") then digest_auth!
    else
      super
    end
  end
end 
