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

  after_create      :load_uuid

  def load_uuid
    self.reload
  end

  private :load_uuid

  #
  # Validate the name should unique
  # and that it starts with an alph and only contains alpha,digits,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_exclusion_of :name, :in => %w(framework api barclamp docs machines jigs roles groups users support application), :message => I18n.t("db.barclamp_excludes", :default=>"Illegal barclamp name")

  validates_format_of :name, :with=>/\A[a-zA-Z][_a-zA-Z0-9]*\z/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  # Deployment
  has_many          :roles,     :dependent => :destroy
  has_many          :attribs,   :dependent => :destroy
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
    raise "Barclamp.import is deprecated!"
  end

  def self.import_or_update(bc)
    Barclamp.transaction do
      raise "Barclamp metadata malformed!" unless bc['barclamp'] &&
                                                  bc['barclamp']['name'] &&
                                                  bc['barclamp']['source_path']
      bc_name = bc['barclamp']['name']
      barclamp = Barclamp.find_by(name: bc_name)
      if barclamp
        unless bc['barclamp']['name'] == barclamp.name
          raise "Cannot change the name of a barclamp via update!"
        end
      else
        barclamp = Barclamp.create!(name: bc_name, cfg_data: bc)
      end
      if bc['barclamp']['parent']
        parent = Barclamp.find_by!(name: bc['barclamp']['parent'])
        barclamp.update!(barclamp_id: parent.id)
      else
        barclamp.update!(barclamp_id: barclamp.id)
      end
      bc_namespace = "Barclamp#{bc_name.camelize}"

      Rails.logger.info "Importing Barclamp #{bc_name}"
      if bc['git']
        gitcommit = bc['git']['commit'] || "unknown"
        gitdate = bc['git']['date'] || "unknown"
      end
      version = bc['barclamp']["version"] || '0.0'
      build_version = bc['barclamp']["build_version"] || '0.0'

      source_url = bc['barclamp']["source_url"]
      barclamp.update_attributes!(
        :description   => bc['barclamp']['description'] || bc['barclamp']['display'] || bc_name.humanize,
        :version       => version,
        :build_version => build_version,
        :source_url    => source_url,
        :source_path   => bc['barclamp']['source_path'],
        :build_on      => gitdate,
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
      end if bc["jigs"]

      bc['providers'].each do |provider|
        Rails.logger.info("Importing provider #{provider['name']} for #{barclamp.name}")
        p_obj = provider['class'].constantize.find_or_create_by!(name: provider['name'],
                                                                 description: provider['description'])
        p_obj.update_attributes!(auth_details: provider['auth_details']) if provider['auth_details']
      end if bc['providers']

      # iterate over the roles in the yml file and load them all.
      # Jigs are now late-bound, so we just load everything.
      bc['roles'].each do |role|
        Rails.logger.info("Importing role #{role['name']} for #{barclamp.name}")
        role_name = role["name"]
        role_jig = Jig.find_by!(name: role["jig"])
        if ! role['type']
          role_type_candidates = []
          rname = role_name.sub("#{bc_name}-",'').gsub('-','_').camelize
          role_type_candidates << "#{bc_namespace}::#{rname}"
          role_type_candidates << "#{bc_namespace}::Role"
          role_type_candidates << "Role"
          role_type = role_type_candidates.detect{|rt| (rt.constantize ? true : false) rescue false}.constantize
        else
          role_type = role['type'].constantize
        end
        Rails.logger.info("Role #{role_name} using class #{role_type.name}")
        prerequisites = role['requires'] || []
        if role_jig.client_role_name
          prerequisites << role_jig.client_role_name
        end
        attaches = role['attaches'] || []
        wanted_attribs = role['wants-attribs'] || []
        flags = role['flags'] || []
        description = role['description'] || role_name.gsub("-"," ").titleize
        role_provides = role['provides'] || []
        role_conflicts = role['conflicts'] || []
        role_metadata = role['metadata'] || {}
        icon = if role['icon']
          role['icon']
        else
          case role_jig.name
          when 'script'
            'sim_card'
          when 'chef-solo', 'chef'
            'restaurant_menu'
          when 'ansible-playbook', 'ansible'
            'gamepad'
          when 'role-provided'
            'developer_board'
          when 'noop'
            'beenhere'
          when "test"
            'healing'
          else
            'memory'
          end
        end
        # roles data import
        r = role_type.find_or_create_by!(:name=>role_name,
                                         :jig_name => role_jig.name,
                                         :barclamp_id=>barclamp.id)
        r.update_attributes!(:description=>description,
                             :barclamp_id=>barclamp.id,
                             :template=>{},
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
                             :powersave=>flags.include?('powersave'),
                             :leaverunlog=>flags.include?('leaverunlog'),
                             :metadata=>role_metadata,
                             :icon=>icon)
        prerequisites.each do |rr|
          Rails.logger.info("Making #{r.name} depend on #{rr}")
          RoleRequire.find_or_create_by!(:role_id => r.id, :requires => rr)
        end
        attaches.each do |ar_name|
          ar = Role.find_by!(name: ar_name)
          Rails.logger.info("Making #{r.name} depend on #{ar.name}")
          RoleRequire.find_or_create_by!(role_id: ar.id, requires: r.name)
        end
        RoleRequireAttrib.where(:role_id => r.id).delete_all
        wanted_attribs.each do  |attr|
          attr_name = attr.is_a?(Hash) ? attr["name"] : attr
          attr_at = attr.is_a?(Hash) ? attr["at"] : nil
          Rails.logger.info("Making role #{r.name} want attribute #{attr_name}")
          RoleRequireAttrib.create!(:role_id => r.id,
                                    :attrib_name => attr_name,
                                    :attrib_at => attr_at)
        end
        role['attribs'].each do |attrib|
          Rails.logger.info("Importing attrib #{attrib['name']} for role #{role['name']} in barclamp #{barclamp.name}")
          attrib_type_candidates = []
          attrib_type_candidates << attrib['type'] if attrib['type']
          attrib_type_candidates << "#{r.jig.type}Attrib"
          attrib_type_candidates << "#{bc_namespace}::Attrib::#{attrib["name"].gsub("-","_").camelize}"
          attrib_type_candidates << "Attrib"
          attrib_type = attrib_type_candidates.detect{|at| (at.constantize ? true : false) rescue false}.constantize
          attrib_name = attrib["name"]
          attrib_desc = attrib['description'] || ""
          attrib_ui_renderer = attrib['ui_renderer'] || Attrib::UI_RENDERER
          attrib_map = attrib['map'] || ""
          attrib_default = attrib['default']
          attrib_writable = !!attrib['schema']
          a = attrib_type.find_or_create_by!(name: attrib_name)
          a.update_attributes!(:description => attrib_desc,
                               :map => attrib_map,
                               :default => {"value" => attrib_default},
                               :role_id => r.id,
                               :ui_renderer => attrib_ui_renderer,
                               :writable => attrib_writable,
                               :schema => attrib_writable ? attrib['schema']: nil,
                               :barclamp_id => barclamp.id)
        end if r && role['attribs']
        role['events'].each do |event|
          evt = EventSink.find_or_create_by!(endpoint: event['endpoint'])
          evt_args={}
          evt_args[:username] ||= event['username']
          evt_args[:authenticator] ||= event['authenticator']
          evt_args[:notes] ||= event['notes']
          evt.update_attributes!(evt_args)
          evt_selectors = evt.event_selectors.all
          event['selectors'].each do |selector|
            next if evt_selectors.any?{|sel| sel.selector == selector}
            Rails.logger.info("Registering role #{role_name} to handle event #{event['endpoint']} #{selector['event']}")
            EventSelector.create!(event_sink_id: evt.id, selector: selector)
          end
        end if r && role['events']
      end if bc['roles']
      bc['attribs'].each do |attrib|
        Rails.logger.info("Importing attrib #{attrib['name']} for barclamp #{barclamp.name}")
        attrib_type_candidates = []
        attrib_type_candidates << attrib['type'] if attrib['type']
        attrib_type_candidates << "#{bc_namespace}::Attrib::#{attrib["name"].gsub("-","_").camelize}"
        attrib_type_candidates << "Attrib"
        attrib_type = attrib_type_candidates.detect{|at| (at.constantize ? true : false) rescue false}.constantize
        attrib_name = attrib["name"]
        attrib_desc = attrib['description'] || ""
        attrib_default = attrib['default']
        attrib_map = attrib['map'] || ""
        attrib_writable = !!attrib['schema']
        a = attrib_type.find_or_create_by!(:name => attrib_name)
        a.update_attributes!(:description => attrib_desc,
                             :map => attrib_map,
                             :writable => attrib_writable,
                             :default => {"value" => attrib_default},
                             :schema => attrib_writable ? attrib['schema']: nil,
                             :barclamp_id => barclamp.id)
      end if bc['attribs']
      # add menut item if wizard enabled
      if bc['wizard']
        Nav.find_or_create_by(item: "wizard_"+bc_name, parent_item: 'deploy', name: bc_name.titleize+" Wizard", description: bc['barclamp']['description'], path: "/barclamps/#{bc_name}/wizard", order: 5000)
      end
      barclamp
    end
  end

  def wizard
    cfg_data["wizard"]
  end

  def api_version
    'v2'
  end

  def api_version_accepts
    'v2'
  end

end
