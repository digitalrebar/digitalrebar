# Copyright 2014, Dell
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
#
#
# Test generic list/get on available endpoints 

from cb2_api.api import cb2_Api
from cb2_api.objects import *



class main():
    
    
    session = cb2_Api("192.168.124.10", "3000", "crowbar", "crowbar")
    
    list_methods ={'get_nodes',
                   'get_deployments',
                   'get_deployment_roles',
                   #'get_interfaces',    #NOT CURRENTLY AVAILABLE
                   'get_users',
                   'get_node_roles',
                   'get_barclamps',
                   'get_groups',
                   #'get_dhcpdatabases',  #NOT CURRENTLY AVAILABLE
                   'get_attributes',
                   'get_jigs',
                   }
    
    for method in list_methods :
        print "--- Checking list method " + method +" --- "
        ls = getattr(session, method)()
        if len(ls) > 0 :
            for each in ls :
                print "  -- checking object of type " + str(each.__class__) + " generic get"
                remote = session.get(each)
                
                # .. this is not pretty i know, but else we ll get random errors (not sure what triggers the timestamp to be updated)
                if str(each.__class__.__name__) == 'User':
                    each.updated_at = each.updated_at[:-4]
                    remote.updated_at = remote.updated_at[:-4]
                
                if remote.__dict__ == each.__dict__: 
                    remote.pretty_print()
                    print "     PASS"
                else:
                    print "     FAIL"
                    print "  local object instance :"
                    each.pretty_print()
                    print "  remote object instance :"
                    remote.pretty_print()
                    raise AssertionError
        else:
            print "empty list back, doesn't look right"
            raise AssertionError
    
    print "That's all folks"   
    
