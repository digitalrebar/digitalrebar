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

require 'yaml'
require 'kwalify'
class Attrib < ActiveRecord::Base

  # determines default view for editing attributes in UI
  UI_RENDERER = "attribs/default"

  validate :schema_is_valid

  serialize :schema

  # Will be thrown unless the attribute is writable.
  class AttribReadOnly < Exception
    def initialize(attr)
      @errstr = "Attrib #{attr.name} is read-only"
    end

    def to_s
      @errstr
    end

    def to_str
      to_s
    end
  end

  belongs_to      :role
  belongs_to      :barclamp
  has_many        :node_role_attrib_links, :dependent => :destroy

  def name_i18n
    I18n.t(name, :default=>name.humanize, :scope=>'common.attribs')
  end

  def wrap_schema(fragment)
    {"type" => "map",
      "required" => true,
      "mapping" => {
        name => fragment
      }
    }
  end

  # Returns an array of schema validation errors.
  # If the array is empty, the validation passed.
  # Fragment must be a string full of YAML.
  def validate_schema(fragment)
    Rails.logger.info("Attrib: Validating schema for #{name}")
    test_schema = wrap_schema(fragment)
    Rails.logger.info("Attrib: #{name}: Schema:\n#{test_schema.to_yaml}")
    validator = Kwalify::MetaValidator.instance
    schema_errors = validator.validate(test_schema)
    Rails.logger.info("Attrib: Schema validation for #{name} passed.") if schema_errors.empty?
    schema_errors
  end

  # Return a deeply nested hash table built from the map with this attribute's
  # data at the end.
  def template(value)
    keys = map.split('/')
    raise "Cannot deal with an empty map!" if keys.empty?
    res = value
    while !keys.empty? do
      res = {keys.pop => res}
    end
    res
  end

  def self.get(name, from, source=:all)
    begin
      (name.is_a?(Attrib) ? name : Attrib.find_key(name)).get(from, source)       
    rescue Exception => e
      Rails.logger.warn "Warn, did not get #{name} from #{from.name} with error #{e.message}"
      nil      
    end
  end

  # Get the attribute value from the passed object.
  # For now, we are encoding information about the objects we can use directly in to
  # the Attrib class, and failing hard if we were passed something that
  # we do not know how to handle.
  def get(from_orig,source=:all)
    from = __resolve(from_orig)
    d = case
        when from.is_a?(Hash) then from
        when from.is_a?(Node)
          case source
          when :all then from.discovery.deep_merge(from.hint)
          when :hint,:user then from.hint
          else from.discovery
          end
        when from.is_a?(DeploymentRole)
          case source
          when :all then from.wall.deep_merge(from.all_data)
          when :hint, :user then from.all_data
          when :wall then from.wall
          else from.data
          end
        when from.is_a?(NodeRole)
          case source
          when :all then from.attrib_data
          when :wall then from.wall
          when :system then from.sysdata
          when :user,:hint then from.data
          else raise("#{from} is not a valid source to read noderole data from!")
          end
        when from.is_a?(Role) then from.template
        else raise("Cannot extract attribute data from #{from.class.to_s}")
        end
    begin
      map.split('/').each{|s|d = d[s]}
      return d
    rescue
      nil
    end
  end

  # Gets the requested value from the passed data, but returns it wrapped in template()
  # unless this attribute is not in the passed blob, in which case it returns nil.
  def extract(from,hint=:all)
    template(get(from,hint))
  end

  def hint_set(to,value)
    __set(to,value,:hint)
  end

  def discovery_set(to,value)
    __set(to,value,:discovery)
  end

  def wall_set(to,value)
    __set(to,value,:wall)
  end

  def user_set(to,value)
    __set(to,value,:user)
  end

  def system_set(to,value)
    __set(to,value,:system)
  end

  def set(to,value,type=:system)
    __set(to,value,type)
  end

  def self.set(name, to, value, type)
    Attrib.find_key(name).set(to,value,type)
  end

  class AttribValidationFailed < Exception
    def initialize(attr,data,errors)
      @errstr = "Attrib #{attr.name}: New requested data #{data.to_json} failed schema validation\n"
      errors.each do |e|
        @errstr << "[#{e.path}]: #{e.message}\n"
      end
    end

    def to_s
      @errstr
    end

    def to_str
      to_s
    end
  end

  def kwalify_validate(value)
    return if schema.nil?
    test_schema = wrap_schema(schema)
    test_value = { name => value }
    validator = Kwalify::Validator.new(test_schema)
    errors = validator.validate(test_value)
    return true if errors.empty?
    raise AttribValidationFailed.new(self,value,errors)
  end

  private

  def schema_is_valid
    return if schema.nil?
    validate_schema(schema).each do |e|
      errors.add(:schema,"[#{e.path}]: #{e.message}")
    end
  end

  # If we were asked to do something with an attribute on a node,
  # but that attribute is part of a node role bound to that node,
  # use the node role instead.
  def __resolve(to)
    case
    when (to.is_a?(Node) && self.role_id) then to.node_roles.find_by!(:role_id => self.role_id)
    when to.is_a?(Deployment) then to.deployment_roles.find_by!(:role_id => self.role_id)
    when [Node,Role,DeploymentRole,NodeRole,Hash].any?{|klass|to.is_a?(klass)} then to
    else raise "#{to.class.name} is not something that we can use Attribs with!"
    end
  end

  # Set a new value for this attribute onto the passed object.
  # The last parameter is what area the new attribute should be placed on
  def __set(to_orig,value,target=:system)
    raise AttribReadOnly.new(self) unless writable || target != :user || to_orig.is_a?(Hash)
    kwalify_validate(value) if target == :user
    to_merge = template(value)
    to = __resolve(to_orig)
    Rails.logger.debug("Attrib: Attempting to update #{name} on #{to.class.name}:#{to.name} to #{value} with #{to_merge.inspect}")
    case
    when to.is_a?(Hash) then to.deep_merge(to_merge)
    when to.is_a?(Node)
      case target
      when :discovery then to.discovery_update(to_merge)
      else to.hint_update(to_merge)
      end
    when to.is_a?(Role) then to.template_update(to_merge)
    when to.is_a?(DeploymentRole)
      target == :system ? to.wall_update(to_merge) : to.data_update(to_merge)
    when to.is_a?(NodeRole)
      case target
      when :system then to.sysdata_update(to_merge)
      when :user,:hint then to.data_update(to_merge)
      when :wall then to.wall_update(to_merge)
      else raise("#{target} is not a valid target to write noderole data to!")
      end
    else raise("Cannot write attribute data to #{to.class.to_s}")
    end
  end

end
