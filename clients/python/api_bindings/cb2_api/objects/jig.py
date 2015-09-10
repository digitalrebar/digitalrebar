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

class JigEP(EndPoint):    
    '''
    https://github.com/digitalrebar/core/blob/master/doc/devguide/api/jig.md
    '''
    __endpoint   = "/api/v2/jigs"      
    __apiObjectType = "Jig"
    
    def __init__(self, session):
        self.session = session
        self.endpoint = JigEP.__endpoint
        super(JigEP, self).__init__(session)
        
    def various(self):
        '''
        TODO
        VARIOUS     api/v2/jigs/:id/extra     Special Ops 
        '''
        
        
class Jig(ApiObject):
    '''
    Jig object
    
    ''' 
    
    def __init__(self, json={}):
        self.description = None
        self.client_role_name = None
        self.created_at = None
        self.updated_at = None
        self.server = None
        self.id = None
        self.key = None
        self.active = None
        self.order = None
        self.client_name = None
        self.name = None
        self.__dict__ = json
        super(Jig, self).__init__()
   
class Enums_Jig():
    '''
    TODO
    '''
 
    
        

        