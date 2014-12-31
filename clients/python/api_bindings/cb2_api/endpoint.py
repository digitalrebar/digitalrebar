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

import requests, json, inspect
from requests.auth import HTTPDigestAuth, HTTPBasicAuth
  

class EndPoint(object):
    '''
    https://github.com/opencrowbar/core/blob/master/doc/devguide/api.md
    '''
    
    def __init__(self, session):
        self.session = session
        self.Auth = HTTPBasicAuth(self.session.user,self.session.pwd)
        #TODOWHENFIXED ^ Note that we should be using HTTPDigest, not Basic (bug)
        self.Headers = {'content-type': 'application/json'}
        
        

    def list(self):
        '''
        List
        ''' 
        self.URL = self.session.url + self.endpoint
        r = requests.get(self.URL,auth=self.Auth,headers=self.Headers)
        self.check_response(r)        
        resp = r.json()
        return resp


    
    def get(self, ref_id):
        '''
        '''
        self.URL = self.session.url + self.endpoint + "/" + ref_id
        r = requests.get(self.URL,auth=self.Auth,headers=self.Headers)
        self.check_response(r)
        resp = r.json()
        return resp


       
    def update(self, ref_id, json):
        '''
        '''
        self.URL = self.session.url + self.endpoint + "/" + ref_id
        r = requests.put(self.URL,data=json, auth=self.Auth,headers=self.Headers)
        self.check_response(r)
        resp = r.json()
        return resp

    def create(self, json):
        '''
        '''
        self.URL = self.session.url + self.endpoint
        r = requests.post(self.URL,data=json, auth=self.Auth,headers=self.Headers)
        self.check_response(r)
        resp = r.json()
        return resp

    def delete(self, ref_id):
        '''
        '''
        self.URL = self.session.url + self.endpoint + "/" + ref_id      
        r = requests.delete(self.URL,auth=self.Auth,headers=self.Headers)
        self.check_response(r)
        resp = r.json()
        return resp   

    @DeprecationWarning
    def jsonToObject(self, json):
        '''
        Turn the json back into what it should be aka for eg. a node Object list.. here or in api class ??
        is this used anywhere ?? ::TODELETE
        '''
        classRef =  self.__apiObjectType
        ob = json # .. change that back 
        return ob
      
    def check_response(self, request):
            if request.status_code == 401 :
                print "ERROR : Unauthorized - check your session user/password"
                raise IOError(request.text)
            if request.status_code == 404 :
                print "ERROR : Not Found - check endpoint " + self.URL
                raise IOError(request.text)
            if request.status_code == 500 :
                print "ERROR : Internal Server Error, something went bad ... " + self.URL
                print request.text
                raise IOError(request.text)
            elif request.status_code == 200: 
                return request
            else:
                print "todo handle http responses.."
                print request.text
                print request.status_code
                raise NotImplementedError