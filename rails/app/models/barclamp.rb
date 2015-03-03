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

class Barclamp < ActiveRecord::Base

  #
  # Validate the name should unique
  # and that it starts with an alph and only contains alpha,digits,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_exclusion_of :name, :in => %w(framework api barclamp docs machines jigs roles groups users support application), :message => I18n.t("db.barclamp_excludes", :default=>"Illegal barclamp name")

  validates_format_of :name, :with=>/\A[a-zA-Z][_a-zA-Z0-9]*\z/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  # Deployment
  has_many          :roles,     :dependent => :destroy
  belongs_to        :barclamp,  :dependent => :destroy
  alias_attribute   :parent,    :barclamp
  serialize         :cfg_data

  scope :roots,     -> { where(["barclamp_id = id"]) }

  #
  # Human printable random password generator
  #
  def self.random_password(size = 12)
    chars = (('a'..'z').to_a + ('0'..'9').to_a) - %w(o 0 O i 1 l)
    (1..size).collect{|a| chars[rand(chars.size)] }.join
  end

  def self.import(bc_name="core", bc=nil, source_path=nil, parent_id=nil)
    Barclamp.transaction do
      source_path ||= File.expand_path(File.join(Rails.root, '..'))
      bc_file = File.expand_path(File.join(source_path, bc_name)) + '.yml'

      # load JSON
      if bc.nil?
        raise "Barclamp metadata #{bc_file} for #{bc_name} not found" unless File.exists?(bc_file)
        bc = YAML.load_file bc_file
      end
      barclamp = Barclamp.find_by(name: bc_name) || Barclamp.create!(name: bc_name, cfg_data: bc)
      bc_namespace = "Barclamp#{bc_name.camelize}"

      Rails.logger.info "Importing Barclamp #{bc_name} from #{source_path}"

      # verson tracking (use Git if we don't get it from the RPM)
      gitinfo = %x[cd '#{source_path}' && git log -n 1].split("\n") rescue ['unknown']*3
      gitcommit = gitinfo[0] || "unknown" rescue "unknown"
      gitdate = gitinfo[2] || "unknown" rescue "unknown"
      if bc['git']
        gitcommit = bc['git']['commit'] || gitcommit
        gitdate = bc['git']['date'] || gitdate
      end
      version = bc["version"] || '0.0'
      build_version = bc["build_version"] || '0.0'

      source_url = bc["source_url"]
      barclamp.update_attributes!(:description   => bc['description'] || bc_name.humanize,
                                  :version       => version,
                                  :build_version => build_version,
                                  :source_path   => source_path,
                                  :source_url    => source_url,
                                  :build_on      => gitdate,
                                  :barclamp_id   => parent_id || barclamp.id,
                                  :commit        => gitcommit,
                                  :cfg_data      => bc)

      # Load node maanger information, if any.
      bc['hammers'].each do |nm|
        AvailableHammer.find_or_create_by!(name: nm['name'],
                                                klass: nm['type'],
                                                priority: nm['priority'])
      end if bc['hammers']

      # load the jig information.
      bc['jigs'].each do |jig|
        raise "Jigs must have a name" unless jig['name'] && !jig['name'].empty?
        raise "Jigs must have a type" unless jig['class'] && !jig["class"].empty?
        Rails.logger.info("Creating jig #{jig['name']} from #{barclamp.name}")
        jig_name = jig["name"]
        jig_desc = jig['description'] || "Imported by #{barclamp.name}"
        jig_type = jig['class']
        jig_client_role = jig["implementor"]
        jig_active = if (Rails.env == "production")
                       jig_name != "test"
                     else
                       ["noop","test","role-provided"].include? jig_name
                     end
        jig = jig_type.constantize.find_or_create_by!(:name => jig_name)
        jig.update_attributes!(:order => 100,
                               :active => jig_active,
                               :description => jig_desc,
                               :client_role_name => jig_client_role)
 
        # temporary until jigs have a barlcamp import support method
        if jig_type.start_with?("BarclampChef")
          Rails.logger.info("Import: #{jig_name} is a chef-type jig. Using Berkshelf.")
          cookbook_path = File.expand_path(File.join(source_path, 'chef/cookbooks/'))
          berksfile = cookbook_path + '/Berksfile'
          raise "Import: No Berksfile found for #{bc_name} in #{berksfile}" unless File.exists?(berksfile)
          if Rails.env.eql? "production"
            result = berks(cookbook_path,"install")
            raise "Import: Unable to berks install #{berksfile}: #{result}" unless $?.exitstatus == 0
            Rails.logger.info("Import: berks install: #{result}\n")
            if jig_type.end_with?("SoloJig")
              Rails.logger.info("Import: #{jig_name} is a chef-solo-type jig. Using Berkshelf packaging.")
              result = berks(cookbook_path,"package /var/cache/crowbar/cookbooks/package.tar.gz")
              raise "Import: Unable to berks package #{berksfile}: #{result}" unless $?.exitstatus == 0
              Rails.logger.info("Import: berks package: #{result}\n")
            end
          else
            Rails.logger.info("Skipping Berks import (only needed for production environment)")
          end
        end
      end if bc["jigs"]

      # load the barclamps submodules information.
      bc['barclamps'].each do |sub_details|
        name = sub_details['name']
        subm_file = File.join(source_path,"barclamps","#{name}.yml")
        next unless File.exists?(subm_file)
        Barclamp.import name, YAML.load_file(subm_file), source_path, barclamp.id

      end if bc["barclamps"]

      # iterate over the roles in the yml file and load them all.
      # Jigs are now late-bound, so we just load everything.
      bc['roles'].each do |role|
        Rails.logger.info("Importing role #{role['name']} for #{barclamp.name}")
        role_name = role["name"]
        role_jig = Jig.find_by!(name: role["jig"])
        role_type_candidates = []
        role_type_candidates << role['type'] if role['type']
        role_type_candidates << "#{bc_namespace}::#{role_name.sub("#{bc_name}-", '').gsub('-','_').camelize}"
        role_type_candidates << "#{bc_namespace}::Role"
        role_type_candidates << "Role"
        role_type = role_type_candidates.detect{|rt| (rt.constantize ? true : false) rescue false}.constantize
        prerequisites = role['requires'] || []
        wanted_attribs = role['wants-attribs'] || []
        flags = role['flags'] || []
        description = role['description'] || role_name.gsub("-"," ").titleize
        role_provides = role['provides'] || []
        role_conflicts = role['conflicts'] || []
        template = File.join source_path, role_jig.on_disk_name || "none", 'roles', role_name, 'role-template.json'
        template = if File.file?(template)
                     Rails.logger.info("Import: Loading role #{role_name} template from #{template}")
                     JSON.parse(IO.read(template))
                   else
                     Rails.logger.info("Import: Loading role #{role_name} using blank template")
                     {}
                   end
        # roles data import
        r = role_type.find_or_create_by!(:name=>role_name,
                                         :jig_name => role_jig.name,
                                         :barclamp_id=>barclamp.id)
        r.update_attributes!(:description=>description,
                             :barclamp_id=>barclamp.id,
                             :template=>template,
                             :provides=>role_provides,
                             :conflicts=>role_conflicts,
                             :milestone=>flags.include?('milestone'),
                             :library=>flags.include?('library'),
                             :implicit=>flags.include?('implicit'),
                             :bootstrap=>flags.include?('bootstrap'),
                             :discovery=>flags.include?('discovery'),
                             :abstract=>flags.include?('abstract'),
                             :destructive=>flags.include?('destructive'),
                             :service=>flags.include?('service'),
                             :cluster=>flags.include?('cluster'),
                             :powersave=>flags.include?('powersave'))
        RoleRequire.where(:role_id=>r.id).delete_all
        RoleRequireAttrib.where(:role_id => r.id).delete_all
        prerequisites.each do |req|
          RoleRequire.create! :role_id => r.id, :requires => req
        end
        wanted_attribs.each do  |attr|
          attr_name = attr.is_a?(Hash) ? attr["name"] : attr
          attr_at = attr.is_a?(Hash) ? attr["at"] : nil
          RoleRequireAttrib.create!(:role_id => r.id,
                                    :attrib_name => attr_name,
                                    :attrib_at => attr_at)
        end
        role['attribs'].each do |attrib|
          Rails.logger.info("Importing attrib #{attrib['name']} for role #{role['name']} in barclamp #{barclamp.name}")
          attrib_type_candidates = []
          attrib_type_candidates << "#{r.jig.type}Attrib"
          attrib_type_candidates << "#{bc_namespace}::Attrib::#{attrib["name"].camelize}"
          attrib_type_candidates << "Attrib"
          attrib_type = attrib_type_candidates.detect{|at| (at.constantize ? true : false) rescue false}.constantize
          attrib_name = attrib["name"]
          attrib_desc = attrib['description'] || ""
          attrib_ui_renderer = attrib['ui_renderer'] || Attrib::UI_RENDERER
          attrib_map = attrib['map'] || ""
          attrib_writable = !!attrib['schema']
          a = attrib_type.find_or_create_by!(name: attrib_name)
          a.update_attributes!(:description => attrib_desc,
                               :map => attrib_map,
                               :role_id => r.id,
                               :ui_renderer => attrib_ui_renderer,
                               :writable => attrib_writable,
                               :schema => attrib_writable ? attrib['schema']: nil,
                               :barclamp_id => barclamp.id)
        end if r && role['attribs']
      end if bc['roles']
      bc['attribs'].each do |attrib|
        Rails.logger.info("Importing attrib #{attrib['name']} for barclamp #{barclamp.name}")
        attrib_type_candidates = []
        attrib_type_candidates << "#{bc_namespace}::Attrib::#{attrib["name"].camelize}"
        attrib_type_candidates << "Attrib"
        attrib_type = attrib_type_candidates.detect{|at| (at.constantize ? true : false) rescue false}.constantize
        attrib_name = attrib["name"]
        attrib_desc = attrib['description'] || ""
        attrib_map = attrib['map'] || ""
        attrib_writable = !!attrib['schema']
        a = attrib_type.find_or_create_by!(:name => attrib_name)
        a.update_attributes!(:description => attrib_desc,
                             :map => attrib_map,
                             :writable => attrib_writable,
                             :schema => attrib_writable ? attrib['schema']: nil,
                             :barclamp_id => barclamp.id)
      end if bc['attribs']
      barclamp
    end
  end

  def api_version
    'v2'
  end

  def api_version_accepts
    'v2'
  end

  private
  def self.berks(dir,args)
    Dir.chdir(dir) do
      %x{unset GEM_HOME BUNDLE_BIN_PATH BUNDLE_GEMFILE RUBYOPT RUBYLIB GEM_PATH; berks #{args} 2>&1}
    end
  end

end
