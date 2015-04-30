## OpenCrowbar API

This document is the reference guide for the OpenCrowbar API.  [Additional information is provided in the API](./) directory.

### Using the API

The OpenCrowbar API is RESTful and accepts/returns JSON.  XML output is not supported.

The OpenCrowbar API is versioned.  API urls include the OpenCrowbar version of the API (e.g.: 1.0 or v2).  Please use the most recent version available!

> Legacy Note: routes with 1.0 are deprecated!

### API Index

_This is a reference index - API is documented in subpages_

    * /api/v2
      * /anneal (check the annealer status)
      * /nodes
        * /[:id]/node_roles
        * /[:id]/attribs
      * /jigs
      * /barclamps
      * /deployments
        * /[:id]/roles
        * /[:id]/node_roles
        * /graph (GET only)
        * /propose (PUT only)
        * /commit (PUT only)
        * /recall (PUT only)
      * /deployment_roles
      * /jigs
      * /roles
      * /attribs
      * /groups
        * /[:id]/nodes
    * /:barclamp/v2
      * see docs per barclamp

### OpenCrowbar API Pattern

The OpenCrowbar API follows the following behavior pattern.

#### Expectations:

* Core objects can be referenced equally by name or ID.
  This means that objects with natural key names are NOT allowed to
  start with a number (similar to FQDN restrictions)
* JSON is the API serialization model

#### Digest Authentication
API callers use digest authentication for all requests. User accounts
need to be specifically configured for API only access.  A user
account with API access will still be able to log in normally.

To get the digest, make a HEAD or GET request to /api/v2/digest

#### Common API URL Patterns:

OpenCrowbar uses a versioned URL pattern. By convention, resources
names are pluralized in the API.  For example, the API will use
=nodes= instead of =node= in paths.

* Base Form: `[workload | api]/[version]/[resources]/[id]`
  * version - version of OpenCrowbar framework being used (v2 for this guide)
  * workload - workload (aka barclamp) that owns the requested activity.  Framework uses 'api'
  * bc_version - the version of the barclamp being used. 
  * key_word - groups the API into different categories
     * reserved words such as status and crowbar
     * resource types like node, group, network, etc
  * id - (optional) name or DB id of the barclamp configuration
  * Result codes
     * 200 = OK, success
     * 500 = Error in processing.
     * 404 = item not found in database (may return 500 in some cases)

* List: 
  * HTTP get
  * Returns a json array of objects

* CRUD Operations: 
  * id - name or database ID of the item.  Items that do not have
  natural keys are not expected to honor use of name instead of
  database ID.  When possible, either will be allowed.
  * RESTful Verbs for CRUD:
     * POST / Create - ID is ignored if provided
     * GET / Read - Objects will be shallow
     * PUT / Update - returns the updated object serialized
     * DELETE/ Delete - no return data except 200
  * Special Cases
     * PUT - used to start an action on existing data (commit a node
       or deployment)
     * DELETE - Unlink/Deactivate/Dequeue

In general, OpenCrowbar REST pattern uses the 4 HTTP verbs as follows:

   * GET to retrieve information
   * PUT to transform or change existing data
   * POST to create new data or relationships
   * DELETE to remove data or relationships

### Expected Fields

By convention, most OpenCrowbar models have the same fields.

* id - database assigned role, number
* name - resource name, often a natural key with enforced uniqueness
* description - user definable content
* created_at - when object was created
* updated_at - when object was last updated
* object_id - cross reference id to an object.  In most cases, you can
  use the name of the object instead of the API

> Some of the information stored in objects is maintained as json and will appear as nested data.

### API Headers & Response Patterns

The OpenCrowbar REST API uses HTTP `content-type` metadata header tags
to help clients quickly identify the information being returned by the API.

The API adds ="application/vnd.crowbar.[type].[form]+json;version=2.0"= to the content-type tag.

If you only care about certian attributes being returned for an API
call, you can set the `x-return-attributes` header to a JSON array of
the attributes you want to return.

* [type] is the object type being returned.  E.g.: node, deployment, jig, etc
* [form] describes how the objects are formed
   * obj = single obj
   * list = list of objects
   * empty = nothing
   * error = error.

REST results should be returned with the appropriate standard HTTP response code, such as:

* 200 = ok
* 404 = object not found
* 500 = application error
* [complete list](http://en.wikipedia.org/wiki/List_of_HTTP_status_codes)

### Example Documentation

The following table should be populated for all API calls:

#### API Actions

| Verb | URL | Comments |
|:----------|:------------------------------|:---------|
| GET | api/v2/resources | List |
| GET | api/v2/resources/:id | Specific Item |
| PUT | api/v2/resources/:id  | Update Item |
| POST | api/v2/resources | Create Item |
| DELETE | api/v2/resources/:id | Delete Item |
| VARIOUS | api/v2/resources/:id/extra | Special Ops |

### JSON Output Example:

    {
      "id":41,
      "name":"sim.cr0wbar.com",
      "description":"example",
      "order":100,
      "admin":true,
      "alias":"sim",
      "alive":true,
      "allocated":false,
      "available":true,
      "bootenv":"sledgehammer",
      "deployment_id":1,
      "discovery":{
         {"foo":"this is json"}
      },
      "created_at":"2013-11-01T03:23:27Z",
      "updated_at":"2013-11-01T03:23:27Z"
    }


### Some workflow examples (using the Crowbar CLI)

#### Creating a Node for a system that already has an OS:

This example will show how to create a new node in Crowbar for an
already-installed system that we want to bring under Crowbar
management.  This example assumes that it has a non-conflicting IP
address that is already in the nodes range of the admin network, that
the public key of the Crowbar user will let the Script jig run things
as root on the node, and that there is already a Crowbar-compatible
operating system installed.

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



#### Installing an operating system on a node

##### Get the names of the nodes you want to install:

* CLI: `crowbar nodes list --attributes name`
* API: `curl --digest -u $(cat /etc/crowbar.install.key)
    -X GET
    -H "Content-Type:application/json"
    -H 'x-return-attributes:["name"]'
    --url http://127.0.0.1:3000/api/v2/nodes`

Returns:

    [
      {
        "name": "78e3be198029.smoke.test"
      },
      {
        "name": "d52-54-05-3f-00-00.smoke.test"
      }
    ]

##### Create a deployment to deploy the nodes into:

* CLI: `crowbar deployments create '{"name": "test1"}'`
* API: `curl --digest -u $(cat /etc/crowbar.install.key)
    -X POST
    -H "Content-Type:application/json"
    --url http://127.0.0.1:3000/api/v2/deployments
    -d '{"name": "test1"}'`

Returns:

    {
      "system": false,
      "created_at": "2014-03-03T04:40:07.351Z",
      "state": 0
      "parent_id": 1,
      "description": null,
      "updated_at": "2014-03-03T04:40:07.351Z",
      "id": 2,
      "name": "test1"
    }

##### Update the target node with the new deployment that you just created:

* CLI: `crowbar nodes move d52-54-05-3f-00-00.smoke.test to test1`
* API: `curl --digest -u $(cat /etc/crowbar.install.key)
    -X PUT
    -H "Content-Type:application/json"
    --url http://127.0.0.1:3000/api/v2/nodes/d52-54-05-3f-00-00.smoke.test
    -d '{"deployment": "test1"}'`

Returns:

    {
      "alias": "d52-54-05-3f-00-00",
      "description": null,
      "target_role_id": null,
      "deployment_id": 2,
      "alive": true,
      "hint": {
        "admin_macs": [
          "52:54:05:3f:00:00"
        ]
      },
      "bootenv": "sledgehammer",
      "admin": false,
      "created_at": "2014-03-03T04:35:19.642Z",
      "name": "d52-54-05-3f-00-00.smoke.test",
      "id": 2,
      "order": 10000,
      "discovery": {},
      "available": true,
      "allocated": false,
      "updated_at": "2014-03-03T04:41:13.342Z"
    }

##### Create a node-role to bind the role to the node:

* CLI: `crowbar roles bind crowbar-installed-node to d52-54-05-3f-00-00.smoke.test`
* API: `curl --digest -u $(cat /etc/crowbar.install.key)
    -X POST
    -H "Content-Type:application/json"
    --url http://127.0.0.1:3000/api/v2/node_roles
    -d '{"node": "d52-54-05-3f-00-00.smoke.test", "role": "crowbar-installed-node"}'`

Returns:

    {
      "id": 25,
      "role_id": 3,
      "state": 4,
      "run_count": 0,
      "node_id": 2,
      "deployment_id": 2,
      "available": true,
      "runlog": "",
      "order": 10000,
      "created_at": "2014-03-03T04:47:43.856Z",
      "updated_at": "2014-03-03T04:47:43.860Z",
      "cohort": 10,
      "status": null
    }

##### (Optional) Change the operating system to deploy onto the node:

* CLI: `crowbar nodes set d52-54-05-3f-00-00.smoke.test attrib
  provisioner-target_os to '{"value": "ubuntu-12.04"}'`
* API: `curl --digest -u $(cat /etc/crowbar.install.key)
    -X PUT
    -H "Content-Type:application/json"
    --url http://127.0.0.1:3000/api/v2/nodes/d52-54-05-3f-00-00.smoke.test/attribs/provisioner-target_os
    -d '{"value": "ubuntu-12.04"}'`

Returns:

    {
      "updated_at": "2014-03-03T16:37:43.478Z",
      "description": "The operating system to install on a node",
      "writable": true,
      "barclamp_id": 7,
      "value": "ubuntu-12.04",
      "order": 10000,
      "name": "provisioner-target_os",
      "id": 37,
      "role_id": 24,
      "created_at": "2014-03-03T16:37:43.466Z",
      "schema": {
        "required": true,
        "enum": [
          "ubuntu-12.04",
          "redhat-6.5",
          "centos-6.6"
        ],
        "type": "str"
      },
      "map": "crowbar/target_os"
    }

##### Commit the deployment:
* CLI: `crowbar deployments commit test1`
* API: `curl --digest -u $(cat /etc/crowbar.install.key)
    -X PUT
    -H "Content-Type:application/json"
    --url http://127.0.0.1:3000/api/v2/deployments/test1/commit`

Returns:

    {
      "name": "test1",
      "system": false,
      "parent_id": 1,
      "id": 2,
      "created_at": "2014-03-03T04:40:07.351Z",
      "updated_at": "2014-03-03T04:40:07.351Z",
      "description": null
    }
