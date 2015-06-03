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
import json, requests
from apiobject import ApiObject

class NodeEP(EndPoint):    
    '''
    https://github.com/opencrowbar/core/blob/master/doc/devguide/api/node.md
    '''
    __endpoint   = "/api/v2/nodes"     
 
    __apiObjectType = "Node"
    
    def __init__(self, session):
        self.session = session
        self.endpoint = NodeEP.__endpoint
        super(NodeEP, self).__init__(session)
        
        
        
    def get_roles(self, node_ref):
        '''
        Shows all the roles that the node is using (including their status)
        '''
        #TODO
        raise NotImplementedError
        
        
class Node(ApiObject):
    '''
    Node object
    // Create required attribute
    admin           : true/false
    alive           : true/false
    allocated       : true/false
    available       : true/false
    bootenv         : local/sledgehammer/????
    description     : description for the node
    hint            : ?? 
    name            : name of the node
    order           : ??
    // Other attributes
    id              : id of the node
    deployment_id   : id of the deployment associated with the node
    discovery       : ???
    created_at      : creation date
    updated_at      : last update date
    ''' 
 
    def __init__(self, json={}):
        self.available = None
        self.deployment_id = None
        self.description = None
        self.name = None
        self.hint = None
        self.admin = None
        self.created_at = None
        self.updated_at = None
        self.alive = None
        self.id = None
        self.target_role_id = None
        self.allocated = None
        self.order = None
        self.bootenv = None
        self.discovery = None
        self.endpoint = None
        self.__dict__ = json
        super(Node, self).__init__()
        
   
class Enums_Node():
 
    class Alive ():
        alive =  "true"
        dead = "false"
        
    class BootEnv():
        local = "local"  
        sledgehammer = 'sledgehammer'  
        
    class Admin():
        true = "true"
        false = "false"
        
    class Allocation():
        true = "true"
        false = "false"
        
    class Available():
        true = "true"
        false = "false"
        

        
