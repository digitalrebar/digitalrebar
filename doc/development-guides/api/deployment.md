### Deployment APIs

#### Deployment Routes

| Verb | URL | Options | Returns | Comments |
|:------|:-----------------------|--------|--------|:----------------|
| GET  | /api/v2/deployments  | none   | Deployment List | - | 
| POST | /api/v2/deployments  | none   | New Deployment | - | 
| GET  | /api/v2/deployments/[:id]  | none   | Existing Deployment Detail | - | 
| PUT  | /api/v2/deployments/[:id]  | none   | Update Deployment Detail | - | 
| GET  | /api/v2/deployments/[:id]  | none   | Existing Deployment Detail | - |
| PUT  | /api/v2/deployments/[:id]/commit  | none   | Commit Proposed | - | 
| PUT  | /api/v2/deployments/[:id]/propose | none   | Create an new Proposal based on Active| - | 
| PUT  | /api/v2/deployments/[:id]/transition | none   | Send Transistion Data into the system| - | 
| GET  | /api/v2/deployments/[:deployment_id]/attribs  | none   | List Attribs for a specific deployment| - | 
| GET  | /api/v2/deployments/[:deployment_id]/attribs/[:id]  | none   | Show Attrib (including value) for a specific Deployment| - | 
| PUT  | /api/v2/deployments/[:deployment_id]/attribs/[:id]  | none   | Update Attrib | - |


## JSON fields

|Attribute|Type|Settable|Note|
|---------|----|--------|----|
|System|Boolean|No||
|Parent_id|Internal Ref|??|Actually an Int|
|Description|String|Yes||
|Name|String|Yes|Limited to Alpha + Numbers - no spaces or special chars|
|Created_at|String|No|Unicode - date format|
|Updated_at|String|No|Unicode - date format|

Minimum fields needed for create - name

