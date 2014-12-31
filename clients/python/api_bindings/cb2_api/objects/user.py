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

class UserEP(EndPoint):    
    '''
    https://github.com/opencrowbar/core/blob/master/doc/devguide/api/users.md
    '''
    __endpoint   = "/api/v2/users"      
    __apiObjectType = "User"
    
    def __init__(self, session):
        self.session = session
        self.endpoint = UserEP.__endpoint
        super(UserEP, self).__init__(session)
        
class User(ApiObject):
    '''
    User object
    
    ''' 
    
    def __init__(self, json={}):
        self.username = None
        self.created_at = None
        self.updated_at = None
        self.id = None
        self.is_admin = None
        self.email = None
        self.__dict__ = json
        super(User, self).__init__()
        
   
class Enums_User():
    '''
    TODO
    '''
 
    
        

        