$:.push File.expand_path("../lib", __FILE__)

# Maintain your gem's version:
require "barclamp_ipmi/version"

# Describe your gem and declare its dependencies:
Gem::Specification.new do |s|
  s.name        = "barclamp_ipmi"
  s.version     = BarclampIpmi::VERSION
  s.authors     = ["Dell Crowbar Team"]
  s.email       = ["crowbar@dell.com"]
  s.homepage    = ""
  s.summary     = " Summary of BarclampIpmi."
  s.description = " Description of BarclampIpmi."

  s.files = Dir["{app,config,db,lib}/**/*"] + [ "Rakefile", ]
  s.test_files = Dir["test/**/*"]

  s.add_dependency "rails"
  # s.add_dependency "jquery-rails"

  s.add_development_dependency "sqlite3"
end
