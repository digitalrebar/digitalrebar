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

from cb2_api.endpoint import EndPoint
from apiobject import ApiObject

class Node_RoleEP(EndPoint):    
    '''
    https://github.com/digitalrebar/core/blob/master/doc/devguide/api/node_roles.md
    '''
    __endpoint   = "/api/v2/node_roles"      
    __apiObjectType = "Node_Role"
    
    def __init__(self, session):
        self.session = session
        self.endpoint = Node_RoleEP.__endpoint
        super(Node_RoleEP, self).__init__(session)
        
class Node_Role(ApiObject):
    '''
    Node_role object
    
    ''' 
    
    def __init__(self, json={}):
        self.available = None
        self.cohort = None
        self.updated_at = None
        self.created_at = None
        self.runlog = None
        self.order = None
        self.state = None
        self.node_id = None
        self.status = None
        self.run_count = None
        self.deployment_id = None
        self.role_id = None
        self.id = None
        self.__dict__ = json
        super(Node_Role, self).__init__()
   
class Enums_Role():
    '''
    TODO
    '''
 
    
        

        
