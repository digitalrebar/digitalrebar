#!/usr/bin/ruby
# Copyright (c) 2013 Dell Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require 'rubygems'
require 'yaml'
require 'json'

class XML_UTIL
   # chef breaks if 'require' is outsite executable code for any GEMs 
   # installed during compile phase in other recipies in run list
   # Dependency checking is happening in libraries prior to compile phase. 
   # Hack is to move 'require' in initialize() which is called during exectution phase.
   def initialize()
     require 'xmlsimple' 
   end
  
  def  processResponse(xml, path, options={"ForceArray" => false})
    begin
      hash = XmlSimple.xml_in(xml, options)
      output = eval("hash#{path}")
      return output
    rescue Exception => e
      puts "Unable to find node #{path} in returned xml...Returning nil"
      return nil
    end
  end
  
  def returnValue(xml,cmd)
    path = '["Body"]["' + cmd + '_OUTPUT"]["ReturnValue"]' 
    processResponse(xml , path)
  end
  
end
