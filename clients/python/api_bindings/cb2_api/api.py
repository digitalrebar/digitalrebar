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

from cb2_api import *
from objects import *
import json , os, sys, inspect

class cb2_Api():
    
    def __init__(self, host, port, username, password):
        self.host = host
        self.port = port
        self.user = username
        self.pwd = password
        
        self.url = "http://" + self.host + ":" + self.port
             
      
    def get_nodes(self):
        endpoint = NodeEP(self)
        ls = []
        alls = endpoint.list()
        for each in alls :
            snap = Node(each)
            ls.append(snap)
        return ls
    
    def get_deployments(self):
        endpoint = DeploymentEP(self)
        ls = []
        alls = endpoint.list()
        for each in alls :
            snap = Deployment(each)
            ls.append(snap)
        return ls
    
    def get_deployment_roles(self):
        endpoint = Deployment_RoleEP(self)
        ls = []
        alls = endpoint.list()
        for each in alls :
            snap = Deployment_Role(each)
            ls.append(snap)
        return ls
    
        
    def get_node(self, node_ref):
        endpoint = NodeEP(self)
        return Node(endpoint.get(node_ref))
    
    def get_interfaces(self):
        endpoint = InterfacesEP(self)
        ls = []
        alls = endpoint.list()
        for each in alls :
            snap = Interfaces(each)
            ls.append(snap)
        return ls
        
    def get_users(self):
        endpoint = UserEP(self)
        ls = []
        alls = endpoint.list()
        for each in alls :
            snap = User(each)
            ls.append(snap)
        return ls
        
    def get_node_roles(self):
        endpoint = Node_RoleEP(self)
        ls = []
        alls = endpoint.list()
        for each in alls :
            snap = Node_Role(each)
            ls.append(snap)
        return ls
        
    def get_barclamps(self):
        endpoint = BarclampEP(self)
        ls = []
        alls = endpoint.list()
        for each in alls :
            snap = Barclamp(each)
            ls.append(snap)
        return ls
    
    
    def get_groups(self):
        endpoint = GroupEP(self)
        ls = []
        alls = endpoint.list()
        for each in alls :
            snap = Group(each)
            ls.append(snap)
        return ls
    
    def get_dhcpdatabases(self):
        endpoint = DHCPdbEP(self)
        ls = []
        alls = endpoint.list()
        for each in alls :
            snap = DHCPdb(each)
            ls.append(snap)
        return ls
        
    def get_group(self, node_ref):
        endpoint = GroupEP(self)
        return Group(endpoint.get(node_ref))
        
    def get_attributes(self):
        endpoint = AttributeEP(self)
        ls = []
        alls = endpoint.list()
        for each in alls :
            snap = Attribute(each)
            ls.append(snap)
        return ls
        
    def get_attribute(self, node_ref):
        endpoint = AttributeEP(self)
        return Attribute(endpoint.get(node_ref))
        
    def get_jigs(self):
        endpoint = JigEP(self)
        ls = []
        alls = endpoint.list()
        for each in alls :
            snap = Jig(each)
            ls.append(snap)
        return ls
        
    def get_jig(self, node_ref):
        endpoint = JigEP(self)
        return Node(endpoint.get(node_ref))
        
    def get(self, obj):
        '''
        Generic get 
        @param : local object
        @return: Remote Object
        '''
        endPoint = cb2_Api.get_endpoint_(self, obj)
        ref = cb2_Api.get_object_hint(obj)
        return globals()[str(obj.__class__.__name__)](endPoint.get( ref)) 
        
    def update(self, obj):
        '''
        Generic Update 
        @param : local object 
        @return: Remote Object
        '''
        endPoint = cb2_Api.get_endpoint_(self, obj)
        ref = cb2_Api.get_object_hint(obj)
        return globals()[str(obj.__class__.__name__)](endPoint.update( ref, json.dumps(obj.__dict__)))
                
    def delete(self, obj):
        '''
        Generic delete
        @param : local object
        '''
        endPoint = cb2_Api.get_endpoint_(self, obj)
        ref = cb2_Api.get_object_hint(obj)
        endPoint.delete( ref)
        
    def create(self, obj):
        '''
        Generic create
        @param : local object
        @return: remote Object
        '''
        endPoint = cb2_Api.get_endpoint_(self, obj)
        raw =  endPoint.create(json.dumps(obj.__dict__))        
        return globals()[str(obj.__class__.__name__)](raw) 
        
    def get_endpoint_(self, obj):
        '''
        Map Objects to their respective endPoint instances
        @param : object
        '''
        cl = obj.__class__.__name__
        if str(cl) == "Node":
            endpoint = NodeEP(self)
        elif str(cl) == 'Attribute':
            endpoint = AttributeEP(self)
        elif str(cl) == 'Barclamp':
            endpoint = BarclampEP(self)
        elif str(cl) == 'Deployment_Role':
            endpoint = Deployment_RoleEP(self)
        elif str(cl) == 'Deployment':
            endpoint = DeploymentEP(self)
        elif str(cl) == 'DHCPdb':
            endpoint = DHCPdbEP(self)
        elif str(cl) == 'Group':
            endpoint = GroupEP(self)
        elif str(cl) == 'Interfaces':
            endpoint = InterfacesEP(self)
        elif str(cl) == 'Jig':
            endpoint = JigEP(self)
        elif str(cl) == 'Group':
            endpoint = GroupEP(self)
        elif str(cl) == 'Node_Role':
            endpoint = Node_RoleEP(self)
        elif str(cl) == 'User':
            endpoint = UserEP(self)
        else:
            raise NotImplementedError
        return endpoint
    
    @staticmethod
    def get_object_hint(obj):
        '''
        What hint to use for a given object ? usually name or id.
        '''
        cl = obj.__class__.__name__
        if str(cl) == "Node":
            id_ref = obj.name 
        elif str(cl) == 'Attribute':
            id_ref = obj.name
        elif str(cl) == 'Barclamp':
            id_ref = obj.name
        elif str(cl) == 'Deployment_Role':
            id_ref = obj.id
        elif str(cl) == 'Deployment':
            id_ref = obj.name
        elif str(cl) == 'DHCPdb':
            raise NotImplementedError 
        elif str(cl) == 'Group':
            id_ref = obj.name
        elif str(cl) == 'Interfaces':
            raise NotImplementedError 
        elif str(cl) == 'Jig':
            id_ref = obj.name
        elif str(cl) == 'Node_Role':
            id_ref = obj.id  
        elif str(cl) == 'User':
            id_ref = obj.username
        else:
            raise NotImplementedError 
        return str(id_ref)
    

    def commit_deployment(self, deploymentID):
        endpoint = DeploymentEP(self)
        endpoint.commit(deploymentID)
        
