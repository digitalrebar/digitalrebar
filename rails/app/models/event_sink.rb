# Copyright 2016, RackN
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

require 'rest-client'

class EventSink < ActiveRecord::Base

  validate :check_endpoint_sanity
  has_many :event_selectors

  def self.name_column
    return :endpoint
  end

  def endpoint_method
    endpoint.partition('://')[0]
  end
  
  def run(obj, selector)
    meth, _, rest = endpoint.partition("://")
    case meth
    when 'inproc'
      objklass, _, methpart = rest.partition(':')
      case objklass
      when 'role'
        objname, _ ,methname = methpart.partition('/')
        runobj = Role.find_key(objname)
        meth_sym = methname.to_sym
        runobj.send(meth_sym, obj)
      else
        raise "EventSink.run for inproc:// only handles Role hooks for now"
      end
    when 'http'
      raise "http handler only accepts on_milestone for now" unless selector['event'] == 'on_milestone'
      data = selector.dup
      data['node'] = obj.node
      data['role'] = {
        'id' => obj.role.uuid,
        'name' => obj.role.name
      }
      begin
        RestClient::Request.execute(
          method: :post,
          url: endpoint,
          timeout: 1,
          payload: data.to_json,
          headers: {
            content_type: 'application/json',
            accept: 'application/json'
          })
      rescue => e
        Rails.logger.error("EventSink: error POSTing back to #{endpoint}: #{e.inspect}")
      end
    else
      raise "Event handling method #{method} not implemented"
    end
  end

  private

  def check_endpoint_sanity
    method, sep, rest  = endpoint.partition('://')
    errors.add("Malformed endpoint #{endpoint}") if sep != '://'
    case method
    when 'http' then true
    when 'inproc' then
      objklass, _, methpart = rest.partition(':')
      case objklass
      when 'role'
        objname, _ ,methname = methpart.partition('/')
        runobj = (Role.find_key(objname) rescue nil)
        if runobj
          meth_sym = methname.to_sym
          unless runobj.respond_to?(meth_sym)
            errors.add("Role #{objname} does not respond to #{methname}")
          end
        else
          errors.add("No such role #{objname}")
        end
      else
        errors.add("inproc:// only handles roles for now")
      end
    else
      errors.add("Event handling method #{method} not supported yet")
    end
  end


end
