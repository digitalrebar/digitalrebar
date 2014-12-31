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

class InterfacesEP(EndPoint):    
    '''
    https://github.com/opencrowbar/core/blob/master/doc/devguide/api/interfaces.md
    '''
    __endpoint   = "/api/v2/interfaces"      
    __apiObjectType = "Interface"
    
    def __init__(self, session):
        self.session = session
        self.endpoint = InterfacesEP.__endpoint
        super(InterfacesEP, self).__init__(session)
        
    def delete(self):
        '''
        not available
        '''
        print "Delete method not available for Interfaces"
        raise NotImplementedError
        
        
class Interfaces(ApiObject):
    '''
    Interface object
    
    ''' 
    
    def __init__(self, json={}):
        '''
        TODO : currently not available
        '''
        super(Interfaces, self).__init__()
        
   
class Enums_Interfaces():
    '''
    TODO
    '''
 
    
        

        