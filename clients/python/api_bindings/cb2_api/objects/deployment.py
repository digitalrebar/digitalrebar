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

class DeploymentEP(EndPoint):    
    '''
    https://github.com/opencrowbar/core/blob/master/doc/devguide/api/deployment.md
    '''
    __endpoint   = "/api/v2/deployments"      
    __apiObjectType = "Deployment"
    
    def __init__(self, session):
        self.session = session
        self.endpoint = DeploymentEP.__endpoint
        super(DeploymentEP, self).__init__(session)
        
class Deployment(ApiObject):
    '''
    Deployment object
    
    ''' 
    
    def __init__(self, json={}):
        self.description = None
        self.created_at = None
        self.system = None
        self.updated_at = None
        self.parent_id = None
        self.state = None
        self.id = None
        self.name = None
        self.__dict__ = json
        super(Deployment, self).__init__()
       
   
class Enums_Deployment():
    '''
    TODO
    '''
 
    
        

        
