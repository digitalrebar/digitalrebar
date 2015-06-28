### Node Role APIs

Node Roles are the core of OpenCrowbar deployment and orchestration engine

There are four types of data that OpenCrowbar tracks, three of them
are maintained on related NodeRoleDatum mode.

1. user data (node_role.data) is set by users during the proposed
state (also known as "out bound data")
2. system data (node_role.sysdata) is set by crowbar before annealing
(also known as "out bound data")
3. wall data (node_role.wall) is set by the jig after transistion
(also known as "in bound data")
4. discovery data (node.wall) is stored on the node instead of node
   role because it reflects node information aggregated from all the
   jigs.  This information is available using the node.attrib_[name]
   and Attrib model.  Please see the node API docs for more about this
   type of data

NodeRole does not have a natural key, so you must reference them them
by ID or under the relevant Nodes, Roles, or Deployment.

#### API Actions

| Verb | URL | Comments |
|:------|:-----------------------|:----------------|
| GET  |api/v2/node_roles | List |
| GET  |api/v2/node_roles/:id | Specific Item |
| PUT  |api/v2/node_roles/:id | Update Item |
| PUT  |api/v2/node_roles/:id/retry | Retry (sets stack back to TODO) |
| PUT  |api/v2/node_roles/:ignored/retry?list=:id|:id|:id | Retry List of IDs (pipe deliminated) |
| POST  |api/v2/node_roles | Create Item |
| GET  | /api/v2/node_roles/[:node_role_id]/attribs  |  List Attribs for a specific node_role |
| GET  | /api/v2/node_roles/[:node_role_id]/attribs/[:id]  | Show Attrib (including value) for a specific Node_Role |
| PUT  | /api/v2/node_roles/[:node_role_id]/attribs/[:id]  | Update Attrib |
| DELETE  | - |NOT SUPPORTED |

Helpers have been added to NodeRole create so that you do not need to
provide IDs when creating a new NodeRole.  You can pass:

* Deployment Name instead of Deployment ID
* Node Name instead of Node ID
* Role Name instead of Role ID

## JSON fields

|Attribute|Type|Settable|Note|
|---------|----|--------|----|
|Available|Boolean|Yes||
|Cohort|Integer|??||
|Created_at|String|No|Unicode - date format|
|Updated_at|String|No|Unicode - date format|
|Runlog|String|??||
|Order|Integer|??||
|State|Integer|??||
|Node_Error|Boolean|No|Calculated|
|Node_Id|Integer|Yes||
|Status|??|??||
|Run_count|Integer|No||
|Deployment_Id|Integer|??||
|Role_Id|Integer|Yes||
|Id|Internal Ref|??|Actually an Int|

## Field Notes

### Node_Error

_Calculated_

True if any of the NodeRoles on the associated Node are in an error state.  This allows API users to monitor the status of a target role and know if there was an error that will block progress without having to inspect other NodeRoles.  Instead of looking at all parents (which could span nodes), Node provides a more limited scope