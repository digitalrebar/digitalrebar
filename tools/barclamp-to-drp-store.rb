#!/usr/bin/env ruby
# This performs a rough translation of a barclamp.yml into
# a storage layer that can be bundled up by drpcli bundle
def log(str)
  $stderr.puts(str)
end

require 'yaml'
require 'fileutils'

$import_yml = ARGV[0]
$dest = ARGV[1]

raise "No importable YAML file" unless File.exists?($import_yml)
$settings = YAML.load_file($import_yml)

# I only care about attribs, roles, and profiles
#
# Roles will be translated into tasks, and if a role uses the script jig
# its associated script will be included as a template.
# Manual fixup will be needed to make it actually a valid template.
#
# attribs will be turned into params, and the kwalify schema will be translated
# into JSON schema.  If schema translation fails, the process will stop
#
# profiles will be translated into drp profiles.

FileUtils.mkdir_p($dest)
IO.write(File.join($dest,"._Name.meta"), $settings["barclamp"]["name"])
IO.write(
  File.join($dest,"._Description.meta"),
  $settings["barclamp"]["description"] || "Automatically generated from the #{$settings["barclamp"]["name"]} barclamp"
)

def kwalify2jsonschema(input = {})
  res = {}
  return res if input.empty?
  raise "Type for kwalify schema required" unless input["type"]
  case input["type"]
  when "str","text" then res["type"] = "string"
  when "int","float","number" then res["type"] = "number"
  when "bool" then res["type"] = "boolean"
  when "date","time","timestamp"
    res["type"] = "string"
    res["format"] = "date-time"
  when "seq"
    res["type"] = "array"
    res["items"] = kwalify2jsonschema(input["sequence"][0])
  when "map"
    res["type"] = "object"
    input["mapping"].each do |k,v|
      case k
      when "=" then res["additionalProperties"] = kwalify2jsonschema(v)
      else
        res["properties"] ||= {}
        res["properties"][k] = kwalify2jsonschema(v)
      end
    end
  else
    raise "Unsupported kwalify schema fragment #{input}"
  end
  res
end

def save(dest, val)
  FileUtils.mkdir_p(File.dirname(dest))
  File.open(dest,"w") do |f|
    f.write(YAML.dump(val))
  end
end

$default_profile = {}

def handle_attrib(val)
  res = {}
  res["Name"] = val["name"]
  res["Description"] = val["description"] || ""
  res["Schema"] = kwalify2jsonschema(val["schema"]) if val["schema"]
  $default_profile[val["name"]] = val["default"] if val["default"]
  save(File.join($dest, "params", val["name"] + ".yml"), res)
end

def handle_profile(val)
  res = {}
  res["Name"] = val["name"]
  res["Params"] = val["values"]
  save(File.join($dest,"profiles",val["name"] + ".yml"), res)
end

def handle_template(name,contents)
  FileUtils.mkdir_p(File.join($dest,"templates"))
  File.open(File.join($dest,"templates",name),"w") do |f|
    f.write(contents)
  end
end

def handle_role(val)
  res = {}
  return if val["jig"] == "noop"
  res["Name"] = val["name"]
  res["Description"] = val["description"] || ""
  res["RequiredParams"] = val["wants-attribs"] if val["wants-attribs"]
  if val["jig"] == "script"
    puts("Script jig detected")
    scriptPath = File.join(File.dirname($import_yml),"script","roles",val["name"],"*")
    puts("Script path #{scriptPath}")
    Dir.glob(scriptPath).each do |fname|
      puts("Importing #{fname}")
      res["Templates"] ||= []
      tmpl = {}
      tmpl["ID"] = $settings["barclamp"]["name"]+ "-" +  File.basename(fname)
      handle_template(tmpl["ID"],IO.read(fname))
      tmpl["Name"] = File.basename(fname)
      res["Templates"] << tmpl
    end
    if val["scripts"]
      val["scripts"].each_with_index do |script,ii|
        res["Templates"] ||= []
        tmpl = {}
        tmpl["Contents"] = script
        tmpl["Name"] = "%02d-script.sh" % ii
        res["Templates"] << tmpl
      end
    end
  end
  val["attribs"].each do |attr|
    res["RequiredParams"] ||= []
    res["RequiredParams"] << attr["name"]
    handle_attrib(attr)
  end if val["attribs"]
  save(File.join($dest,"tasks",val["name"]+".yml"), res)

end

$settings["attribs"].each do |attr|
  handle_attrib(attr)
end if $settings["attribs"]

$settings["roles"].each do |role|
  handle_role(role)
end if $settings["roles"]

$settings["profiles"].each do |profile|
  handle_profile(profile)
end if $settings["profiles"]

if !$default_profile.empty?
  handle_profile(
    {
      "name" => $settings["barclamp"]["name"] + "_defaults",
      "values" => $default_profile
    })
end
