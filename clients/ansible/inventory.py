# Copyright 2015, RackN
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

# pip install requests
import requests, json
from requests.auth import HTTPDigestAuth
  
'''
https://github.com/opencrowbar/core/blob/master/doc/devguide/api.md
'''
    
# change these values to match your OpenCrowbar installation
addr = "http://127.0.0.1:3000"
user = "crowbar"
password = "crowbar"

Auth = HTTPDigestAuth(user,password)
Headers = {'content-type': 'application/json'}
URL = addr + "/api/status/inventory"
r = requests.get(URL,auth=Auth,headers=Headers)
if r.status_code == 200: 
    print r.json()
else:
    raise IOError(r.text)

