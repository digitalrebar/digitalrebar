# Node API

Node APIs are used to manage nodes (servers) within the Crowbar system

When Nodes are created, updated or deleted, roles and jigs are notified so they can tale appropriate actions.

## API Actions
|Verb |	URL |	Comments|
|-----|-----|-----------|
|GET |	api/v2/nodes |	List|
|GET |	api/v2/nodes/:id |	Specific Item|
|PUT 	|api/v2/nodes/:id | Update Item, notifies all jigs and roles |
|POST |	api/v2/nodes |	Create Item, notifies all jigs and roles |
|DELETE |	api/v2/nodes/:id |	Delete Item + notifies all jigs and roles |
|GET |	api/v2/nodes/:id/node_roles |	Shows all the roles that the node is using (including their status) |
| GET  | /api/v2/nodes/[:node_id]/attribs  | List Attribs for a specific node | 
| GET  | /api/v2/nodes/[:node_id]/attribs/[:id]  | Show Attrib (including value) for a specific Node |
| PUT  | /api/v2/nodes/[:node_id]/commit  | Commit all the noderoles in proposed on a specific node |
| PUT  | /api/v2/nodes/[:node_id]/attribs/[:id]  | Update Attrib |

Details:

-     name - must be FQDN

Hints:

Uesrs can provide shortcuts to the hint data. The following hints have been defined as optional parameters for the Node API

-    ip - requests a specific network-admin IP
-    mac - setup up the DHCP resolution for the node using the given MAC address

## Examples

Using CURL to create a minimally configured node from the Admin node

curl --digest -u 'developer:Cr0wbar!' -H "Content-Type:application/json" --url http://127.0.0.1:3000/api/v2/nodes -X POST --data @ns.json

Where the data file is `ns.json` and contains

{ "alive": "true", "bootenv": "local",
"name": "test.cr0wbar.com" } 
## JSON Fields

| Attribute | Type |Settable | Note
|:--------|:--------|:--------------|:----------------|
| Admin       |  Boolean      |Yes||
|Alias|String|Yes||
|Alive|Boolean|Yes||
|Allocated|Boolean|Yes||
|Available|Boolean|Yes||
|Bootenv|String|Yes||
|Created_at|String|No|Unicode - date format|
|Deployment_id|Internal Ref|No|Actually an Int|
|Description|String|Yes||
|Discovery|String|Yes|All the details of the hardware - very large|
|Hint|String|Yes||
|Id|Integer|No||
|Name|String|Yes||
|Order|Integer|??||
|Target\_role_id|Internal Ref|No||
|Updated_at|String|No|Unicode - date format|


## Minimum fields needed for create

Alive, Bootenv, Name
