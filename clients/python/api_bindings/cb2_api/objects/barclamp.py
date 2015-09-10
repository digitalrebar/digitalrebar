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

class BarclampEP(EndPoint):    
    '''
    https://github.com/digitalrebar/core/blob/master/doc/devguide/api/barclamp.md
    '''
    __endpoint   = "/api/v2/barclamps"      
    __apiObjectType = "Barclamp"
    
    def __init__(self, session):
        self.session = session
        self.endpoint = BarclampEP.__endpoint
        super(BarclampEP, self).__init__(session)
        
class Barclamp(ApiObject):
    '''
    Barclamp object
    
    ''' 
    
    def __init__(self, json={}):
        self.updated_at = None
        self.id = None
        self.layout = None
        self.copyright = None
        self.version = None
        self.online_help = None
        self.api_version = None
        self.description = None
        self.proposal_schema_version = None
        self.members = None
        self.requirements = None
        self.name = None
        self.allow_multiple_deployments = None
        self.license = None
        self.created_at = None
        self.source_path = None
        self.user_managed = None
        self.mode = None
        self.api_version_accepts = None
        self.commit = None
        self.display = None
        self.__dict__ = json
        super(Barclamp, self).__init__()
   
class Enums_Barclamp():
    '''
    TODO
    '''
 
    
        

        