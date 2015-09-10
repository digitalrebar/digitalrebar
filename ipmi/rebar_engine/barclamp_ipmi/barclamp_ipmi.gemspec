$:.push File.expand_path("../lib", __FILE__)

# Maintain your gem's version:
require "barclamp_ipmi/version"

# Describe your gem and declare its dependencies:
Gem::Specification.new do |s|
  s.name        = "barclamp_ipmi"
  s.version     = BarclampIpmi::VERSION
  s.authors     = ["Rebar Team"]
  s.email       = ["support@rackn.com"]
  s.homepage    = ""
  s.summary     = " Summary of BarclampIpmi."
  s.description = " Description of BarclampIpmi."

  s.files = Dir["{app,config,db,lib}/**/*"] + [ "Rakefile", ]
  s.test_files = Dir["test/**/*"]

  s.add_dependency "rails"
  if !(File.exists?("/etc/redhat-release") &&
       IO.read("/etc/redhat-release").match(/ 7\.0/))
    if !(File.exist?("/etc/os-release") &&
         IO.read("/etc/os-release").match(/VERSION_ID="14\.04"/))
      s.add_dependency "openwsman", '=2.4.14'
    else
      s.add_dependency "openwsman", '=2.4.1'
    end
  end

  # s.add_dependency "jquery-rails"
end
