# Copyright 2015, OCBTemplatAuthor
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

$:.push File.expand_path("../lib", __FILE__)

# Maintain your gem's version:
require "barclamp_ocb_template/version"

# Describe your gem and declare its dependencies:
Gem::Specification.new do |s|
  s.name        = "barclamp_ocb_template"
  s.version     = BarclampOcbTemplate::VERSION
  s.authors     = ["OCBTemplateAuthor"]
  s.email       = ["unknown@sample.com"]
  s.homepage    = ""
  s.summary     = "Summary of OCBTemplate."
  s.description = "Description of OCBTemplate."

  s.files = Dir["{app,config,db,lib}/**/*"] + [ "Rakefile", ]
  s.test_files = Dir["test/**/*"]

  s.add_dependency "rails"

end
