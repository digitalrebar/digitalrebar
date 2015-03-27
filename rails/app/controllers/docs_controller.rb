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
  skip_before_filter :crowbar_auth

  # render licenses details for login
  def eula
    @file = Rails.configuration.crowbar.eula_base || '../doc/licenses/README.md'
    if File.exist? @file
      @raw = IO.read(@file)
      fix_encoding! unless @raw.valid_encoding?
      @raw.encode!('UTF-8', :invalid=>:replace)
      @text = Maruku.new(@raw).to_html
    else
      @text = "#{I18n.t('.topic_missing', :scope=>'docs.topic')}: #{@file}"
    end
    render :show
  end

  def index
    if params.has_key?(:rebuild)
        Doc.delete_all 
        Doc.gen_doc_index
    end
    @top_index = []
    # change order
    @top_index << Doc.find_key('/user-guide/README.md')
    @top_index << Doc.find_key('/deployment-guide/README.md')
    @top_index << Doc.find_key('/faq/README.md')
    @top_index << Doc.find_key('/principles/README.md')
    # make sure we have them all
    Doc.roots.sort.each { |d| @top_index << d unless @top_index.include? d }
    respond_to do |format|
      format.html # index.html.haml
      format.json { render :json => Doc.all }
    end
  end

  def fix_encoding!
    ['UTF-8', 'Windows-1252', 'ASCII'].each do |encoding|
      s = @raw.force_encoding encoding
      if s.valid_encoding?
        @raw = s
        return
      end
    end
    if @raw.encoding == Encoding::UTF_8
      (0...@raw.length).each do |i|
        @raw[c] = "\ufffd" unless @raw[c].valid_encoding?
      end
    end
  end

  def show
    id = params[:id]
    @doc = Doc.find_key id rescue nil
    @doc ||= Doc.find_key "/#{id}" rescue nil
    @doc ||= Doc.find_key "/#{id}/README.md" rescue nil
    # we may be looking for a relative link
    if @doc.nil? and session['last_doc']
      children = Doc.where(:parent_id=>session['last_doc'])
      children.each do |c|
        if c.name.ends_with? id
          @doc = c 
        elsif c.name.ends_with? "#{id}/README.md"
          @doc = c 
        end
      end
    end
    @text = ""
    begin
      if @doc
        session['last_doc'] = @doc.id
        @nav_up = @doc.parent
        brothers = Doc.where(:parent_id=>@doc.parent_id).sort
        @nav_prev = nil
        @nav_next = nil
        reached = false
        brothers.each do |x|
          if reached
            @nav_next = x
            break
          elsif x.id == @doc.id
            reached = true
          else
            @nav_prev = x
          end
        end
        if not reached
          @nav_prev = nil
        end
        @file = @doc.file_name
      else
        raise "doc not found: #{id}"
        # @file = File.join Doc.root_directory, id
      end
      html = !params.has_key?(:source)
      image = false
      # navigation items
      if File.exist? @file
        if @file =~ /\.md$/
          @raw = IO.read(@file)
          fix_encoding! unless @raw.valid_encoding?
          @raw.encode!('UTF-8', :invalid=>:replace)
          @text = (html ? Maruku.new(@raw).to_html : @raw)
          # @text += Doc.topic_expand(@doc.name, html) if params.has_key? :expand
        elsif @file =~ /\.(jpg|png)$/
          html = false
          image = true
        end
      else
        @text = "#{I18n.t('.topic_missing', :scope=>'docs.topic')}: #{@file}"
      end
    rescue
      @text = "#{I18n.t('.topic_error', :scope=>'docs.topic')}: #{@file}"
      flash[:notice] = @text
    end
    if image
      render :text=>open(@file, "rb").read, :content_type => :image, :content_disposition => "inline"
    elsif params.has_key? :expand
      if html
        render :layout => 'doc_export'
      else
        render :text=>@text, :content_type => :text
      end
    end
  end

  def file_name
    File.join barclamp.source_path, name
  end
  def export
    @docs = params[:q].split(/,/).map do |id|
      params[:id] = id
      show
      @text
    end
  end

end
