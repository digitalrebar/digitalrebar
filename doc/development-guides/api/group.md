### Group APIs

Group APIs are used to manage groups.  Groups are used to organized things

#### Group CRUD

Create, Read, Update, Delete actions for Groups

##### List

Returns list of group id:names in the system

> Note: Category is not included in this list

**Input:**

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET  | /2.0/crowbar/2.0/group | - | - | All groups |
| GET  | /2.0/crowbar/2.0/group/:id | - | - | Single Group |
| POST  | /2.0/crowbar/2.0/group | - | - | Creats Group |
| PUT  | /2.0/crowbar/2.0/group/:id | - | - | Updates Group values |
| DELETE  | /2.0/crowbar/2.0/group/:id | - | - | Remove Group |
| GET  | /2.0/crowbar/2.0/group/:id/nodes | - | - | Lists nodes in group |
| POST  | /2.0/crowbar/2.0/group/:id/nodes | - | - | Adds Node based on JSON {node_id:#} |
| DELETE  | /2.0/crowbar/2.0/group/:id/nodes/:id | - | - | Removes Node from Group |


Details:

* id - Node id
* name - Node name
* category - limited to ui, rack, class or tag

##### JSON Format 

    { 
      "id":1
      "name":"fqdn.example.com",
      "description":"description",
      "category":"ui"
      "order":10000,
    }

Details:

* name - group name (must be letters - numbers and start with a letter)
* description - optional (default null)
* category - (default = ui) determines the collection of groups.  Allowed categories are
  * ui
  * rack
  * tag
  * class
* order - optional (default 10000) 
