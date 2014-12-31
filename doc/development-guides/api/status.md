### Status APIs

Status APIs are used to provide lists of objects in optimized formats.  They do not have a release contract and should not be used for external APIs that are not tightly integrated to the code base

The general pattern for the Status API calls is:

> `api/status/object/[:id]`

#### Queue Status 

Returns JSON for Annealer worker queue

**Input:**

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET  |/api/status/queue  |none | Number of Workers and List of Jobs | Used by BDD |

**Output**

  { workers:10, jobs:[] }


#### Node Status 

Returns JSON for node status for AJAX calls.  Includes hash of all nodes to help detect changes.

**Input:**

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET  | api/status/node | none | All nodes | Used by Dashboard |
| GET  | api/status/node/[id] | none | id is the node ID or name. | Used by Node details |

