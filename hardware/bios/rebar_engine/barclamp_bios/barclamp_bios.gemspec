$:.push File.expand_path("../lib", __FILE__)

# Maintain your gem's version:
require "barclamp_bios/version"

# Describe your gem and declare its dependencies:
Gem::Specification.new do |s|
  s.name        = "barclamp_bios"
  s.version     = BarclampBios::VERSION
  s.authors     = ["DigitalRebar Devs"]
  s.email       = ["digitalrebar.com"]
  s.homepage    = ""
  s.summary     = " Summary of BarclampBios."
  s.description = " Description of BarclampBios."
  s.license     = "MIT"

  s.files = Dir["{app,config,db,lib}/**/*",  "Rakefile", ]
  s.test_files = Dir["test/**/*"]

  s.add_dependency "rails"

  s.add_development_dependency "sqlite3"
end
