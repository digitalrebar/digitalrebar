### Range API

The Range API is used to manage networks.  It must be a child of a specific network.

#### Range CRUD

Lists the current ranges for a network.

**Input:**

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET   | network/api/v2/networks/[network]/ranges | N/A | JSON array of ranges | |
| GET   | network/api/v2/networks/[network]/ranges/[range] |  | Details of the network in JSON format | |
| POST   | network/api/v2/networks/[network]/ranges |  json definition (see Node Show)  |  must be a legal object | | 
| PUT   | network/api/v2/networks/[network]/ranges/[range] |  |  | |

You cannot delete a range at this time.  You must delete the entire network.
