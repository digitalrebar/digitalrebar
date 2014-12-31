## Node Model

### Boot Environment ("bootenv")

The node's Boot Environment determines which operating system will be
installed on the node after configuration and inventory by Sledgehammer.

There are several built in bootenv values:

* local - Tells the provisioner that this node should not be PXE booted to
anything.
* sledgehammer - Tells the provisioner that the node should PXE boot into
  Sledgehammer the next time it reboots.
* [os-name]-install - Tells the provisioner that the node shold boot
  into the OS install environment for [os-name].

### Hint

Hints are settable on a Node to make node-specific user preferences
available to Roles before a noderole that would otherwise convey that
information has been bound to the node.  For example, a user may wish
a node to have a specific IP address.  This advice is communicated
to the network role(s) by giving the hint-admin-v4addr attrib on the
role the preferred IP address.

#### Hint Shortcut

Some hints are so common that there are parameter short-cuts during
node creation.  This makes it easier to set these special values.

* =ip= maps to =hint-admin-v4addr=
* =mac= maps to =hint-admin-macs=

### Aliveness and availability:

Nodes in the OpenCrowbar framework have two related flags that control
whether the annealer can operate on them.

Aliveness is under the control of the OpenCrowbar framework and
encapsulates the framework's idea of whether any given node is
manageable or not.  If a node is pingable and can be SSH'ed into as
root without a password using the credentials of the root user on
the admin node, then the node is alive, otherwise it is dead.
Aliveness is tested everytime a jig tries to do something on a node
-- if a node cannot be pinged and SSH'ed into from at least one of
its addresses on the admin network, it will be marked as
dead.  When a node is marked as dead, all of the noderoles on that
node will be set to either blocked or todo (depending on the state of
their parent noderoles), and those changes will ripple down the
noderole dependency graph to any child noderoles.

Nodes will also mark themselves as alive and dead in the course of
their startup and shutdown routines.

Availability is under the control of the OpenCrowbar cluster
administrators, and should be used by them to tell OpenCrowbar that it
should stop managing noderoles on the node.  When a node is not
available, the annealer will not try to perform any jig runs on a
node, but it will leave the state of the noderoles alone.

A node must be both alive and available for the annealer to perform
operations on it.

> Defaults: Alive defaults to false and Available default to false.

### Node Creation

In order for a node to be useable by the Crowbar process, it has to be
created and populated with its initial noderole bindings and
(optional) hints.  Here is an example that demonstrates the node
creation process:

* CLI: `crowbar nodes create '{"name": "newtest.cr0wbar.com", "bootenv": "local"}`
* API: `curl --digest -u $(cat /etc/crowbar.install.key) \
    -X POST \
    -d "name=newtest.cr0wbar.com" \
    -d "bootenv=local" \
    -H "Content-Type:application/json" \
    --url http://127.0.0.1:3000/api/v2/nodes`

This will return:
    {
    "admin":false,
    "alias":"newtest",
    "alive":false,
    "allocated":false,
    "available":false,
    "bootenv":"local",
    "created_at":"2013-12-21T05:49:00Z",
    "deployment_id":1,
    "description":"",
    "discovery":{},
    "hint":{},
    "id":41,
    "name":"newtest.cr0wbar.com",
    "order":100,
    "target_role_id":null,
    "updated_at":"2013-12-21T05:49:00Z"
    }

After creating the node, we still need to set the hint for the Admin
IP to have Crowbar try and use the one it already has:

* CLI: `crowbar nodes set newtest.cr0wbar.com attrib hint-admin-v4addr
to '{"value": "192.168.124.99/24"}`
* API: `curl --digest -u $(cat /etc/crowbar.install.key)
    -X PUT
    -H "Content-Type:application/json"
    --url http://127.0.0.1:3000/api/v2/nodes/newtest.cr0wbar.com/attribs/hint-admin-v4addr
    -d '{"value": "192.168.124.99/24"}'`

We then need to bind a useful set of default noderoles to the node:

* CLI: `crowbar roles bind crowbar-managed-node to newtest.cr0wbar.com`
* API: `curl --digest -u $(cat /etc/crowbar.install.key)
    -X POST
    -H "Content-Type:application/json"
    --url http://127.0.0.1:3000/api/v2/node_roles
    -d '{"node": "newtest.cr0wbar.com", "role": "crowbar-managed-node"}'`

Commit the node, which will move the newly-created noderoles from
proposed to todo or blocked, and mark the node as available:

* CLI: `crowbar nodes commit newtest.cr0wbar.com`
* API: `curl --digest -u $(cat /etc/crowbar.install.key)
    -X PUT
    -H "Content-Type:application/json"
    --url http://127.0.0.1:3000/api/v2/nodes/newtest.cr0wbar.com/commit`

Mark the node as alive, which will allow the annealer to do its thing:

* CLI: `crowbar nodes update newtest.cr0wbar.com '{"alive": true}'`
* API: `curl --digest -u $(cat /etc/crowbar.install.key)
    -X PUT
    -H "Content-Type:application/json"
    --url http://127.0.0.1:3000/api/v2/nodes/newtest.cr0wbar.com
    -d 'alive=true'`
