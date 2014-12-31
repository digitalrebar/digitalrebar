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
#
#
#
#Example workflow : Pushing new capabilites to a node

from cb2_api.api import cb2_Api
from cb2_api.objects import Deployment, Node, Node_Role


class main():
    
        #variables
        admin_ip = '192.168.124.10'
        admin_port = '3000'
        user = 'crowbar'
        password = 'crowbar'
        target_node = "d08-00-27-20-36-b0.test.org"
        deployment_name = 'newTestDeployment'

        #create a session 
        session = cb2_Api(admin_ip, admin_port, user, password)

        #create a new deployment    
        deploy = Deployment()
        deploy.name = deployment_name
        deploy = session.create(deploy)
    
        #get/set the target node    
        targetNode = session.get(session.get(Node({"name":target_node})))   
        targetNode.deployment_id = deploy.id
        node = session.update(targetNode)
        node.pretty_print()
    
        #create a node role & assign it to the node created above    
        nodeRole = Node_Role()
        nodeRole.node_id = targetNode.id
        nodeRole.role_id = '6'
        nodeRole = session.update(nodeRole)
        nodeRole.pretty_print()
    
        #commit the deployment
        session.commit_deployment(deploy.id)
    
    
