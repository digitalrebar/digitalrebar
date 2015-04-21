### Router API

The Router API is used to manage networks.  It must be a child of a specific network.  There can be only one router per network.

#### Router CRUD

Lists the current ranges for a network.

**Input:**

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET   | network/api/v2/networks/[network]/network_routers | N/A | JSON array of router for the network | |
| GET   | network/api/v2/network_routers | N/A | JSON array of routers | |
| GET   | network/api/v2/networks/[network]/network_routers/[any] |  | Details of the router in JSON format | |
| GET   | network/api/v2/network_routers/[id] | no natural key, requires Database ID | Details of the router in JSON format | |
| POST  | network/api/v2/networks/[network]/network_routers |  json definition  |  network id is infered from path | | 
| POST  | network/api/v2/network_routers |  json definition  |  must include network id | | 
| PUT   | network/api/v2/networks/[network]/network_routers/[range] |  | network id can be infered from path | |
| PUT   | network/api/v2/network_routers/[range] |  |  | |
| DELETE| network/api/v2/networks/[network]/network_routers/[range] |  |  | |
| DELETE| network/api/v2/network_routers/[range] |  |  | |


#### Notes 

* Because only one router is allowed per network, Crowbar ignores the ID of the router when used with the networks path.
* Router does not conform to other Crowbar object maps.  It does not have a name, description or order.

