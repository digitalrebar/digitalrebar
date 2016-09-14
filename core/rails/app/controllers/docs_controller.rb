# Copyright 2014, Dell
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

class DocsController < ApplicationController

  # no login required for docs!
  skip_before_filter :rebar_auth

  # render licenses details for login
  def eula
    access_control = {
      'Access-Control-Allow-Origin' => request.headers["HTTP_ORIGIN"],
      'Access-Control-Allow-Headers' => 'X-Requested-With,Content-Type,Cookie,Authorization,WWW-Authenticate', # If-Modified-Since,If-None-Match,
      'Access-Control-Allow-Credentials' => true,
      'Access-Control-Expose-Headers' => 'WWW-Authenticate, Set-Cookie, Access-Control-Allow-Headers, Access-Control-Allow-Credentials, Access-Control-Allow-Origin'
    }
    access_control.each{ |k, v| response.headers[k] = v } if request.headers["HTTP_ORIGIN"]
    if params.has_key? :alive            
      render json: {}, status => :no_content
    else
      @file = Rails.configuration.rebar.eula_base || '../LICENSE.md'
      if File.exist? @file
        @raw = IO.read(@file)
        fix_encoding! unless @raw.valid_encoding?
        @raw.encode!('UTF-8', :invalid=>:replace)
        @text = Maruku.new(@raw).to_html
      else
        @text = "#{I18n.t('.topic_missing', :scope=>'docs.topic')}: #{@file}"
      end
      respond_to do |format|
        format.html { render :show }
        format.json { render json: { :eula => @raw } }
      end
    end
  end

end
