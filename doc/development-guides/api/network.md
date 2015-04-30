
### Network API

The network API is used to manage networks.

#### Network CRUD

Lists the current networks.

**Input:**

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET   | api/v2/networks | N/A | JSON array of network IDs | |
| GET   | \api/v2/networks/[network] | id is the network ID or name. | Details of the network in JSON format | -|
| POST   | api/v2/networks |  json definition (see Node Show)  |  must be a legal object | |
| PUT   | api/v2/networks/[network] |  |  | |
| DELETE   | api/v2/networks/[network] |  Database ID or name  | HTTP error code 200 on success | |


> There are helpers on the POST method that allow you to create ranges and routers when you create the network. 

Sample:

    {
      "name":       "networkname",
      "deployment": "deploymentname",
      "vlan":       your_vlan,
      "use_vlan":   true or false,
      "team_mode":  teaming mode,
      "use_team":   true or false,
      "use_bridge": true or false,
      "category":   "String to indicate type: admin,bmc,general,...",
      "group":      "String to indicate groups of networks",
      "conduit":    "1g0,1g1", // or whatever you want to use as a conduit for this network
      "ranges": [
         { "name": "name", "first": "192.168.124.10/24", "last": "192.168.124.245/24" }
      ],
      "router": {
         "pref": 255, // or whatever pref you want.  Lowest on a host will win.
         "address": "192.168.124.1/24"
      }
    }

#### Network Actions: IP Allocate

Allocates a free IP address in a network.

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
|POST | api/v2/networks/[id]/allocate_ip |  Database ID or name of the network barclamp  | HTTP error code 200 on success | |


#### Network Actions: IP Deallocate

Deallocates a used IP address in a network.

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
|DELETE | api/v2/networks/deallocate_ip/[network_id]/[node_id] | id: Database ID or name of proposal<br>network_id: Database ID or name of network<br>node_id: Database ID or name of node | HTTP error code 200 on success | |

