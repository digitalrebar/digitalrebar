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

Opencrowbar :
        https://github.com/opencrowbar

Installation :
        python setup.py install


Session management.

        cb2_api manages the authentication, you need to create a session instance for your crowbar admin node
                session = cb2_Api("192.168.124.10", "3000", "crowbar", "crowbar")

        You can update/get/delete.. objects in a number of ways. The
        following illustrate the methods available, we're using a
        'Node' object in the examples, but similar methods are
        available for all object types.
        

        To create local instance of objects, you can either :

                create an 'empty' instance of the object and fill in its properties
                        node = Node()
                        node.name = 'nodeName'
                construct the object passing in a json dict (the bindings serialize / deserialise from dict to json)
                        node = Node({"name":"nodeName"})


The following are the generic session methods available to get/list/create/update/delete objects

- Getting objects

        getnode = Node()
        getnode.name = 'nodeName'
        node = session.get(getnode)
        node = session.get(Node({"name":"newnode3.test.org'}))

- Listing objects

        nodes = session.get_nodes()

- Creating objects

        node = Node()
        node.name = 'newShinyNode'
        node.alias = 'thisIsMe'
        node.alive = Enums_Node.Alive.alive
        node.bootenv = Enums_Node.BootEnv.local
        node = session.create(node)
        session.create_node({"name":"newnode3.test.org"})

- Deleting objects

        node = Node
        node.name = 'nodeName'
        session.delete(node)
        .
        node = session.get_node(Node({"name":"newnode3.test.org"}))
        session.delete(node)

- Updating objects.

        node = Node()
        node.name = 'myName'
        node.alias = 'MyNewAlias'
        session.update(node)
        .
        node = session.get(Node({"name":"newnode3.test.org"}))
        node.alias = “newAlias”
        session.update(node)
        .
The above methods are available to most object, with some exceptions
(see API doc), for e.g. : the deployment endpoint doesn't allow delete.
The bindings will throw a NotImplementedError exception if trying to use such methods where not available.

Some object have 'special' methods, those are available either through the session or on the object's instance

        session.commit_deployment(deploymentID)
        Node.get_roles()
        etc.


Wokflow example

   #create an api session
    session = cb2_Api("192.168.124.10", "3000", "crowbar", "crowbar")

    #create a new deployment
    deploy = Deployment()
    deploy.name = 'ApiDeployment'
    deploy = session.create(deploy)

    #get/set the target node
    targetNode = session.get(session.create(Node({"name":"nodename.domain.org"})))
    targetNode.deployment_id = deploy.id
    node = session.update(targetNode)

    #create a node role & assign it to the node created above
    nodeRole = Node_Role()
    nodeRole.node_id = targetNode.id
    nodeRole.role_id = '6'
    nodeRole = session.update(nodeRole)

    #commit the deployment
    snap = session.commit_deployment(deploy.id)

    #that's all folks
