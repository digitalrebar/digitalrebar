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

class GroupEP(EndPoint):    
    '''
    https://github.com/digitalrebar/core/blob/master/doc/devguide/api/group.md
    '''
    __endpoint   = "/api/v2/groups"      
    __apiObjectType = "Group"
    
    def __init__(self, session):
        self.session = session
        self.endpoint = GroupEP.__endpoint
        super(GroupEP, self).__init__(session)
        
class Group(ApiObject):
    '''
    Group object
    
    ''' 
    
    def __init__(self, json={}):
        self.category = None
        self.description = None
        self.created_at = None
        self.updated_at = None
        self.id = None
        self.order = None
        self.name = None
        self.__dict__ = json
        super(Group, self).__init__()
   
class Enums_Group():
    '''
    TODO
    '''
 
    
        

        