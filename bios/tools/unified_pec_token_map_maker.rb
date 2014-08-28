#!/usr/bin/env ruby
# Copyright 2011, Dell
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

=begin
Process a csv file and produce setting maps. The expected format is:

token description set

where set can be:
- y:  include in default
- virt - include in virtualization set
- storage - include in storage set

=end

require 'csv'
require 'rubygems'
require 'json'
require 'yaml'

bios_maps = {
  "C6100" => "bios_settings.c6100",
  "PowerEdgeC2100" => "bios_settings.c2100",
  "PowerEdgeC6105" => "bios_settings.c6105",
  "PowerEdgeC6145" => "bios_settings.c6145",
  "PowerEdgeC6220" => "bios_settings.c6220",
  "PowerEdgeC8220" => "bios_settings.c8220"
}

# Some fine day this will be real metadata
supported_bioses = {
  "C6100" => ["1.69"],
  "PowerEdgeC2100" => ["C99Q3B22"],
  "PowerEdgeC6105" => ["2.1.4"],
  "PowerEdgeC6145" => ["2.8.0"],
  "PowerEdgeC6220" => ["1.0.28"],
  "PowerEdgeC8220" => ["1.0.24"]
}

set_keys={
  "default" => ["default"],
  "virt"=> ["Virtualization"],
  "storage" => ["Storage","Hadoop"]
}

bios_cfg_file = "pec-bios-options.csv"

class UnifiedPECSchema
  def add_attribute(name,values = [])
    @attributes[name] = {
      "type" => "str",
      "required" => false
    }
    if values && values.kind_of?(Array) && ! values.empty?
      @attributes[name]["enum"] = values
    end
  end

  def initialize
    @attributes = Hash.new
    @defaults = Hash.new
  end

  def attribute_exists?(n)
    @attributes.has_key?(n)
  end

  def value_ok?(n,val)
    return false unless attribute_exists?(n)
    return true unless @attributes[n]["enum"]
    @attributes[n]["enum"].member?(val)
  end

  def default(n)
    @defaults[n]
  end

  def set_default(n,val)
    return nil unless attribute_exists?(n) && value_ok?(n,val)
    @defaults[n] = val
  end

  def to_hash
    res = {
      "type" => "map",
      "required" => true,
      "mapping" => {
        "id" => {
          "type" => "str",
          "required" => true },
        "versions" => {
          "type" => "seq",
          "required" => false,
          "sequence" => [ { "type" => "str" } ] },
        "style" => {
          "type" => "str",
          "required" => true,
          "enum" => ["unified_pec"] },
        "attributes" => {
          "type" => "map",
        "required" => true,
          "mapping" => {
            "raw_tokens" => {
              "type" => "seq",
              "required" => false,
              "sequence" => [ { "type" => "str" } ] },
          }
        }
      }
    }
    res["mapping"]["attributes"]["mapping"].merge!(@attributes)
    res
  end
end

def clean_str(s)
  s.gsub(/[^a-zA-Z_0-9. ]/, "_").strip
end

unless ENV['BC_CACHE'] && ENV['BC_DIR']
  STDERR.puts("BC_CACHE and BC_DIR must be set un your environment.")
  STDERR.puts("If you are running this from outside the crowbar buils system,")
  STDERR.puts("please invoke legacy_token_map_maker.rb with:")
  STDERR.puts
  STDERR.puts("BC_CACHE=path/to/crowbar-build-cache/barclamps/bios BC_DIR=path/to/crowbar/barclamps/bios legacy_token_map_maker.rb")
  exit(1)
end

unless File.exists?("#{ENV['BC_DIR']}/crowbar.yml")
  STDERR.puts("No crowbar.yml in #{ENV['BC_DIR']}!")
  exit 1
end

# Try to find the setupbios tarball
barclamp_params=YAML.load(File.open("#{ENV['BC_DIR']}/crowbar.yml"))
setupbios = cache_path = nil
barclamp_params["extra_files"].each do |k|
  next unless k =~ /setupbios/
  setupbios,cache_path = k.split(' ',2)
  cache_path ||= ''
  setupbios = "#{ENV['BC_CACHE']}/files/#{cache_path}/#{setupbios.split('/')[-1]}"
  break
end

tmpdir = %x{mktemp -d /tmp/token-map-XXXXXXX}.strip.chomp
unless File.directory?(tmpdir)
  STDERR.puts("Cannot create temporary directory #{tmpdir}!")
  exit(1)
end

at_exit {system("rm -rf #{tmpdir}")}

unless File.exists?(setupbios)
  STDERR.puts("Cannot find the setupbios tarball at #{setupbios}")
  exit(1)
end

system("tar -x -C '#{tmpdir}' -f '#{setupbios}' #{bios_maps.values.sort.map{|f|"setupbios/#{f}"}.join(' ')}")

# Parse each bios mapping file from setupbios, saving
# all the information we will need to valdate pec-bios-options and create a schema map.
bios_schemas = {}
bios_options = {}
bios_maps.each do |sys_name,bios_map|
  unless File.exists?("#{tmpdir}/setupbios/#{bios_map}")
    STDERR.puts("Cannot find needed map file #{tmpdir}/setupbios/#{bios_map}")
    exit 1
  end
  bios_schemas[sys_name] = UnifiedPECSchema.new
  File.open("#{tmpdir}/setupbios/#{bios_map}",'r') do |bios_map_file|
    bios_map_file.each do |real_line|
      line = real_line.gsub(/[#;].*/,'').strip
      next if line.empty?
      setting_name, default, token_mapping = line.split(',',3).map{|s|s.strip}
      token_settings = token_mapping.split(',').map do |t|
        t.strip.split(':',2).map{|tv|tv.strip}[-1]
      end
      bios_schemas[sys_name].add_attribute(setting_name,token_settings)
      unless bios_schemas[sys_name].value_ok?(setting_name,default)
        STDERR.puts("Inconsistent token map file #{bios_map}!")
        STDERR.puts("#{default} is set as the default for #{setting_name}, but it is not in the token mapping section!")
        STDERR.puts("Will assume no default for #{real_line.chomp}.")
        next
      end
      bios_schemas[sys_name].set_default(setting_name,default)
      bios_options[sys_name] ||= {}
      bios_options[sys_name]["default"] ||= {}
      bios_options[sys_name]["default"][setting_name] = default
    end
  end
end

# Make sure we have a pec-bios-options.csv
unless File.exists?("#{ENV['BC_DIR']}/tools/maps/pec-bios-options.csv")
  STDERR.puts("Cannot find maps/pec-bios-options.csv!")
  STDERR.puts("Exiting.")
  exit(1)
end

# Grab the bios options we want out of pec-bios-options.csv
STDERR.puts("Processing #{ENV['BC_DIR']}/tools/maps/pec-bios-options.csv:")
reader = CSV.open("#{ENV['BC_DIR']}/tools/maps/pec-bios-options.csv",'r')
reader.shift
line = 1
reader.each do |row|
  line += 1
  sys_name,token,crowbar_config,setting = row.map{|e|clean_str(e)}
  unless bios_maps[sys_name]
    STDERR.puts("Unknown system type #{sys_name} on line #{line} of pec-bios-options.csv")
    STDERR.puts("Skipping line #{line}: #{row.join(',')}")
    next
  end
  bios_options[sys_name] ||= {}
  bios_options[sys_name][crowbar_config] ||= {}
  if token =~ /^0x[0-9a-fA-F]+$/
    STDERR.puts("Line #{line}: Using raw token values depreciated.")
    bios_options[sys_name][crowbar_config]["raw_tokens"] ||= []
    bios_options[sys_name][crowbar_config]["raw_tokens"] << token
    next
  end
  unless bios_schemas[sys_name].attribute_exists?(token)
    STDERR.puts("Line #{line}: #{token} is not a setting name that setupbios will understand!")
    exit 1
  end
  if (bios_schemas[sys_name].default(token) == setting) &&
      (crowbar_config == "default")
    STDERR.puts("Line #{line}: #{token} is being set to its default value.")
    next
  end
  bios_options[sys_name][crowbar_config][token] = setting
end
reader.close

# Save our newly created schema files
bios_schemas.each do |k,v|
  File.open("bios-set-#{k}.schema",'w') do |f|
    f.puts(JSON.pretty_generate(v.to_hash))
  end
end

bios_options.each do |sys_name,crowbar_config|
  crowbar_config.each_key do |cfg|
    next unless set_keys[cfg]
    set_keys[cfg].each do |ccfg|
      out = {
        "id" => "bios-set-#{sys_name}-#{ccfg}",
        "style" => "unified_pec",
        "versions" => supported_bioses[sys_name],
        "attributes" => crowbar_config[cfg]
      }
      File.open("#{out['id']}.json","w") do |f|
        f.puts(JSON.pretty_generate(out))
      end
    end
  end
end
